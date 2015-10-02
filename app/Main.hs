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
import Halive.Utils

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
  , ovrEyes :: [EyeInfo]
  }

main :: IO ()
main = do

  (window, events) <- reacquire 0 $ createWindow "OpenVR" 1024 768

  cubeProg   <- createShaderProgram "app/cube.vert" "app/cube.frag"
  cubeGeo    <- cubeGeometry (0.1 :: V3 GLfloat) (V3 1 1 1)
  cubeShape  <- makeShape cubeGeo cubeProg
  let _ = cubeShape :: Shape Uniforms

  glEnable GL_DEPTH_TEST
  useProgram (sProgram cubeShape)

  mOpenVR <- createOpenVR
  -- let mOpenVR = Nothing
  
  case mOpenVR of 
    Just openVR -> openVRLoop window events cubeShape openVR
    Nothing -> flatLoop window events cubeShape
  
      
  putStrLn "Done!"

createOpenVR = do
  putStrLn "Starting OpenVR"
  mSystem <- reacquire 1 $ initOpenVR

  case mSystem of
    Nothing -> putStrLn "Couldn't create OpenVR system :*(" >> return Nothing
    Just system -> do
      putStrLn $ "Got system: " ++ show system
      (w,h) <- getRenderTargetSize system
      print (w,h)
      eyes <- forM (zip [0..] [LeftEye, RightEye]) $ \(i, eye) -> do
        eyeProj  <- getEyeProjectionMatrix system eye 0.1 100
        eyeTrans <- safeInv44 <$> getEyeToHeadTransform system eye
        print eyeTrans
        print (view translation . safeInv44 $ eyeTrans)
        -- (x,y,w,h) <- getEyeViewport system eye
        -- print eyeProj


        let x = fromIntegral $ i * w
        return EyeInfo
          { eiProjection = eyeProj
          , eiEyeHeadTrans = eyeTrans
          , eiViewport = (x, 0, x + w, fromIntegral h)
          -- , eiViewport = (x,y,w,h)
          }

      mCompositor <- getCompositor
      case mCompositor of
        Nothing -> putStrLn "Couldn't create OpenVR compositor :*(" >> return Nothing
        Just compositor -> do

          (framebuffer, framebufferTexture) <- createFramebuffer (fromIntegral w * 2) (fromIntegral h)

          return . Just $ OpenVR
            { ovrSystem = system
            , ovrCompositor = compositor
            , ovrFramebuffer = framebuffer
            , ovrFramebufferTexture = framebufferTexture
            , ovrEyes = eyes
            }


openVRLoop window events cubeShape OpenVR{..} = do

  whileWindow window $ do
    

    now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> liftIO getCurrentTime
    glClearColor (now * 0.4) 1.0 0 1
    withFramebuffer ovrFramebuffer $ do

      headPose <- safeInv44 <$> waitGetPoses ovrCompositor ovrSystem
      let _ = headPose :: M44 GLfloat
      -- putStrLn $ "Head pose: " ++ show (headPose ^. translation)

      glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

      forM_ ovrEyes $ \EyeInfo{..} -> do
        let (x, y, w, h) = eiViewport
            finalView    = eiEyeHeadTrans !*! headPose
            -- finalView    = headPose !*! eiEyeHeadTrans
        glViewport x y w h

        forM_ [(x,y,z) | x <- [-2..2], y <- [-2..2], z <- [-2..2] ] $ \(x,y,z) -> do
          let model = mkTransformation (axisAngle (V3 0 1 0) 0) (V3 x y z)
                        -- !*! scaleMatrix (V3 (y + 2.1) 1 1)
              color = V4 ((y + 2) / 4) 0.4 ((x+2)/4) 1 -- increase redness as y goes up, blueness as x goes up


          render cubeShape eiProjection finalView model color

      processEvents events $ closeOnEscape window

      submitFrame ovrCompositor ovrFramebufferTexture

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

    render cubeShape projection view model (V4 0.1 0.2 0.8 1)

    swapBuffers window


render :: (MonadIO m) 
       => Shape Uniforms
       -> M44 GLfloat
       -> M44 GLfloat
       -> M44 GLfloat
       -> V4 GLfloat
       -> m ()
render cubeShape projection viewMat model color = do
  let Uniforms{..} = sUniforms cubeShape
      projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = safeInv44 viewMat ^. translation

  uniformV3 uCamera eyePos

  withVAO (sVAO cubeShape) $ do
    uniformV4 uDiffuse color

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
  putStrLn $"Creating framebuffer of size" ++ show (sizeX, sizeY)
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


