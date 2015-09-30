{-# LANGUAGE RecordWildCards #-}
module Main where

import Graphics.VR.OpenVR
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Data.Time
import Control.Monad.Trans
import Control.Lens.Extra
import Data.Maybe
import Control.Monad
import CubeUniforms

data EyeInfo = EyeInfo
  { eiProjection :: M44 GLfloat
  , eiEyeHeadTrans  :: M44 GLfloat
  , eiViewport   :: (GLint, GLint, GLsizei, GLsizei)
  }


data OpenVR = OpenVR
  { ovrSystem :: IVRSystem
  , ovrCompositor :: IVRCompositor
  , ovrFramebuffer :: GLuint
  , ovrFramebufferTexture :: GLuint
  , ovrRenderTargetSize :: V2 GLint
  , ovrEyes :: [EyeInfo]
  }

createOpenVR = do
  putStrLn "Starting OpenVR"
  mSystem <- initOpenVR

  case mSystem of
    Nothing -> putStrLn "Couldn't create OpenVR system :*(" >> return Nothing
    Just system -> do
      putStrLn $ "Got system: " ++ show system
      (w,h) <- getRenderTargetSize system
      print (w,h)

      let halfWidth = fromIntegral $ w `div` 2
      eyes <- forM (zip [0..] [LeftEye, RightEye]) $ \(i, eye) -> do
        eyeProj  <- getEyeProjectionMatrix system eye 0.01 100
        eyeTrans <- safeInv44 <$> getEyeToHeadTransform system eye


        let x = fromIntegral $ i * halfWidth
        return EyeInfo
          { eiProjection = eyeProj
          , eiEyeHeadTrans = eyeTrans
          , eiViewport = (x, 0, x + halfWidth, fromIntegral h)
          }

      mCompositor <- getCompositor
      case mCompositor of
        Nothing -> putStrLn "Couldn't create OpenVR compositor :*(" >> return Nothing
        Just compositor -> do

          (framebuffer, framebufferTexture) <- createFramebuffer (fromIntegral w) (fromIntegral h)

          return . Just $ OpenVR
            { ovrSystem = system
            , ovrCompositor = compositor
            , ovrRenderTargetSize = V2 w h
            , ovrFramebuffer = framebuffer
            , ovrFramebufferTexture = framebufferTexture
            , ovrEyes = eyes
            }
          

main :: IO ()
main = do

  (window, events) <- createWindow "OpenVR" 1024 768

  cubeProg   <- createShaderProgram "app/cube.vert" "app/cube.frag"
  cubeGeo    <- cubeGeometry (0.1 :: V3 GLfloat) (V3 1 1 1)
  cubeShape  <- makeShape cubeGeo cubeProg
  let _ = cubeShape :: Shape Uniforms

  glEnable GL_DEPTH_TEST
  useProgram (sProgram cubeShape)

  mOpenVR <- createOpenVR
  --let mOpenVR = Nothing
  
  case mOpenVR of 
    Just openVR -> openVRLoop window events cubeShape openVR
    Nothing -> flatLoop window events cubeShape
  
      
  putStrLn "Done!"

openVRLoop window events cubeShape OpenVR{..} = do
  let zoom = 3 :: GLfloat
      view = lookAt (V3 0 2 0) (V3 0 0 zoom) (V3 0 1 0)

  whileWindow window $ do
    

    now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> liftIO getCurrentTime
    glClearColor (now * 0.4) 1.0 0.3 1
    withFramebuffer ovrFramebuffer $ do

      headPose <- waitGetPoses ovrCompositor ovrSystem
      let _ = headPose :: M44 GLfloat

      glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

      info <- forM ovrEyes $ \EyeInfo{..} -> do
        let (x, y, w, h) = eiViewport
            finalView    = eiEyeHeadTrans !*! headPose
        glViewport x y w h

        let info = eiViewport
        forM_ [(x,y,z) | x <- [-2..2], y <- [-2..2], z <- [-2..2] ] $ \(x,y,z) -> do
          let model = mkTransformation (axisAngle (V3 0 1 0) now) (V3 x y z)

          render cubeShape eiProjection finalView model
        return info

      processEvents events $ \e -> do
        onKeyDown Key'Space e (print info)
        (closeOnEscape window e)

      submitFrame ovrCompositor ovrFramebufferTexture (ovrRenderTargetSize ^. _x) (ovrRenderTargetSize ^. _y)

    swapBuffers window


flatLoop window events cubeShape = do
  let zoom = 3
      view = lookAt (V3 0 2 0) (V3 0 0 zoom) (V3 0 1 0)
      projection = perspective 45 (1024/768) 0.1 1000
  whileWindow window $ do
    processEvents events (closeOnEscape window)

    now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> liftIO getCurrentTime
    glClearColor now 0.2 0.5 1

    let model = mkTransformation (axisAngle (V3 0 1 0) now) (V3 0 0 zoom)

    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    glViewport 0 0 1024 768

    render cubeShape projection view model

    swapBuffers window


render :: (MonadIO m) 
       => Shape Uniforms
       -> M44 GLfloat
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render cubeShape projection viewMat model = do
  let Uniforms{..} = sUniforms cubeShape
      projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = safeInv44 viewMat ^. translation

  uniformV3 uCamera eyePos

  withVAO (sVAO cubeShape) $ do
    uniformV4 uDiffuse (V4 1 0.1 0.1 1)

    drawShape model projectionView cubeShape

drawShape :: MonadIO m => M44 GLfloat -> M44 GLfloat -> Shape Uniforms -> m ()
drawShape model projectionView shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uModelViewProjection (projectionView !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model

  let vc = vertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr










safeInv44 matrix = fromMaybe matrix (inv44 matrix)

withFramebuffer :: MonadIO m => GLuint -> m a -> m ()
withFramebuffer framebuffer action = do
  glBindFramebuffer GL_FRAMEBUFFER framebuffer
  _ <- action
  glBindFramebuffer GL_FRAMEBUFFER 0


-- | Create and configure the texture to use for our framebuffer
createFramebufferTexture :: GLsizei -> GLsizei -> IO GLuint
createFramebufferTexture sizeX sizeY = do
  texID <- overPtr (glGenTextures 1)
  
  glBindTexture   GL_TEXTURE_2D texID
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER
  glTexStorage2D  GL_TEXTURE_2D 1 GL_RGBA8 sizeX sizeY
  glBindTexture   GL_TEXTURE_2D 0
  
  return texID

-- | Create the framebuffer we'll render into and pass to the Oculus SDK
createFramebuffer :: GLsizei -> GLsizei -> IO (GLuint, GLuint)
createFramebuffer sizeX sizeY = do
  framebufferTexture <- createFramebufferTexture sizeX sizeY

  framebuffer <- overPtr (glGenFramebuffers 1)

  -- Attach the eye texture as the color buffer
  glBindFramebuffer GL_FRAMEBUFFER framebuffer
  glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D framebufferTexture 0

  -- Generate a render buffer for depth
  renderbuffer <- overPtr (glGenRenderbuffers 1)

  -- Configure the depth buffer dimensions to match the eye texture
  glBindRenderbuffer GL_RENDERBUFFER renderbuffer
  glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT16 sizeX sizeY
  glBindRenderbuffer GL_RENDERBUFFER 0

  -- Attach the render buffer as the depth target
  glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER renderbuffer

  -- Unbind the framebuffer
  glBindFramebuffer GL_FRAMEBUFFER 0

  return (framebuffer, framebufferTexture)


