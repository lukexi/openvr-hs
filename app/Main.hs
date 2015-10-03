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
import Halive.Utils
import Framebuffer
import CubeUniforms
import Cube

cubes = [cubeAt x y z | x <- [-2..2], y <- [-2..2], z <- [-2..2] ]
  where
    cubeAt x y z = Cube 
        { _cubPose = newPose {_posPosition = V3 x y z}
        , _cubColor = color
        , _cubScale = 1
        }
      where color = V4 ((y + 2) / 4) 0.4 ((x+2)/4) 1 -- increase redness as y goes up, blueness as x goes up

data EyeInfo = EyeInfo
  { eiEye                :: HmdEye
  , eiProjection         :: M44 GLfloat
  , eiEyeHeadTrans       :: M44 GLfloat
  , eiViewport           :: (GLint, GLint, GLsizei, GLsizei)
  , eiFramebuffer        :: GLuint
  , eiFramebufferTexture :: GLuint
  }


data OpenVR = OpenVR
  { ovrSystem     :: IVRSystem
  , ovrCompositor :: IVRCompositor
  , ovrEyes       :: [EyeInfo]
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

        (framebuffer, framebufferTexture) <- createFramebuffer (fromIntegral w) (fromIntegral h)

        return EyeInfo
          { eiEye = eye
          , eiProjection = eyeProj
          , eiEyeHeadTrans = eyeTrans
          , eiViewport = (0, 0, w, h)
          , eiFramebuffer = framebuffer
          , eiFramebufferTexture = framebufferTexture
          }

      mCompositor <- getCompositor
      case mCompositor of
        Nothing -> putStrLn "Couldn't create OpenVR compositor :*(" >> return Nothing
        Just compositor -> do
          return . Just $ OpenVR
            { ovrSystem = system
            , ovrCompositor = compositor
            , ovrEyes = eyes
            }



openVRLoop window events cubeShape OpenVR{..} = whileWindow window $ do
  
  now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> liftIO getCurrentTime
  glClearColor (now * 0.4) 1.0 0 1

  poses <- waitGetPoses ovrCompositor ovrSystem
  
  let (headPose, leftHandPose, rightHandPose) = case poses of
        (head:controller1:controller2:xs) -> (safeInv44 head, controller1, controller2)
        _                                 -> (identity, identity, identity)
  -- let _ = headPose :: M44 GLfloat

  forM_ ovrEyes $ \EyeInfo{..} -> do

    withFramebuffer eiFramebuffer $ do
      glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
      let (x, y, w, h) = eiViewport
          finalView    = eiEyeHeadTrans !*! headPose
      glViewport x y w h

      render cubeShape eiProjection finalView cubes

      submitFrameForEye ovrCompositor eiEye eiFramebufferTexture

  processEvents events $ closeOnEscape window

  swapBuffers window


flatLoop window events cubeShape = do
  let zoom = 3
      view = lookAt (V3 0 2 0) (V3 0 0 zoom) (V3 0 1 0)
      projection = perspective 45 (1024/768) 0.1 1000
  whileWindow window $ do
    processEvents events (closeOnEscape window)

    now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> liftIO getCurrentTime
    glClearColor now 0.2 0.5 1

    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    glViewport 0 0 1024 768

    render cubeShape projection view cubes

    swapBuffers window


render :: (MonadIO m) 
       => Shape Uniforms
       -> M44 GLfloat
       -> M44 GLfloat
       -> [Cube]
       -> m ()
render cubeShape projection viewMat cubes = do
  let Uniforms{..} = sUniforms cubeShape
      projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = safeInv44 viewMat ^. translation

  uniformV3 uCamera eyePos

  withVAO (sVAO cubeShape) $ forM_ cubes $ \cube -> do
    uniformV4 uDiffuse (cube ^. cubColor)

    drawShape (transformationFromPose (cube ^. cubPose)) projectionView cubeShape

drawShape :: MonadIO m => M44 GLfloat -> M44 GLfloat -> Shape Uniforms -> m ()
drawShape model projectionView shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uModelViewProjection (projectionView !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model

  let vc = vertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr



