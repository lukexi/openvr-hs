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
import CubeUniforms
import Cube

worldCubes :: [Cube]
worldCubes = [cubeAt x y z | x <- [-2..2], y <- [-2..2], z <- [-2..2] ]
  where
    cubeAt x y z = Cube 
        { _cubMatrix = transformationFromPose $ newPose { _posPosition = V3 x y z }
        , _cubColor = color
        }
      where color = V4 ((y + 2) / 4) 0.4 ((x+2)/4) 1 -- increase redness as y goes up, blueness as x goes up

main :: IO ()
main = do

  (window, events) <- reacquire 0 $ createWindow "OpenVR" 1024 768

  cubeProg   <- createShaderProgram "app/cube.vert" "app/cube.frag"
  cubeGeo    <- cubeGeometry (0.1 :: V3 GLfloat) (V3 1 1 1)
  cubeShape  <- makeShape cubeGeo cubeProg
  let _ = cubeShape :: Shape Uniforms

  glEnable GL_DEPTH_TEST
  useProgram (sProgram cubeShape)

  mOpenVR <- reacquire 1 $ createOpenVR
  -- let mOpenVR = Nothing
  
  case mOpenVR of 
    Just openVR -> do
      triggerHapticPulse (ovrSystem openVR) 0 0 1000
      triggerHapticPulse (ovrSystem openVR) 1 0 1000
      openVRLoop window events cubeShape openVR
    Nothing -> flatLoop window events cubeShape
  
      
  putStrLn "Done!"

data Hand = Hand 
  { hndGrip :: Bool
  , hndStart :: Bool
  , hndTrigger :: GLfloat
  , hndXY :: V2 GLfloat
  , hndMatrix :: M44 GLfloat
  } deriving Show

openVRLoop :: Window -> Events -> Shape Uniforms -> OpenVR -> IO ()
openVRLoop window events cubeShape OpenVR{..} = whileWindow window $ do
  pollNextEvent ovrSystem
  poses <- getDevicePosesOfClass ovrSystem TrackedDeviceClassController

  hands <- forM (zip [0..] poses) $ \(i, pose) -> do
    (x, y, trigger, grip, start) <- getControllerState ovrSystem i
    let hand = Hand
          { hndMatrix = pose
          , hndXY = realToFrac <$> V2 x y
          , hndTrigger = realToFrac trigger
          , hndGrip = grip
          , hndStart = start
          }
    print hand
    return hand

  let handCubes = flip map hands $ \hand -> Cube
        { _cubMatrix = hndMatrix hand
        , _cubColor = V4 1 0 0 1
        }
  
  now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> liftIO getCurrentTime
  glClearColor 0.2 0.1 (now * 0.3) 1

  headPose <- safeInv44 <$> waitGetPoses ovrCompositor

  forM_ ovrEyes $ \EyeInfo{..} -> do

    withFramebuffer eiFramebuffer $ do
      glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
      let (x, y, w, h) = eiViewport
          finalView    = eiEyeHeadTrans !*! headPose
      glViewport x y w h

      render cubeShape eiProjection finalView (handCubes ++ worldCubes)

      submitFrameForEye ovrCompositor eiEye eiFramebufferTexture

  processEvents events $ closeOnEscape window

  swapBuffers window

flatLoop :: MonadIO m => Window -> Events -> Shape Uniforms -> m ()
flatLoop window events cubeShape = do
  let viewMat = lookAt (V3 0 2 0) (V3 0 0 3) (V3 0 1 0)
      projectionMat = perspective 45 (1024/768) 0.1 1000
  whileWindow window $ do
    processEvents events (closeOnEscape window)

    now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> liftIO getCurrentTime
    glClearColor now 0.2 0.5 1

    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    glViewport 0 0 1024 768

    render cubeShape projectionMat viewMat worldCubes

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

    drawShape (cube ^. cubMatrix) projectionView cubeShape

drawShape :: MonadIO m => M44 GLfloat -> M44 GLfloat -> Shape Uniforms -> m ()
drawShape model projectionView shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uModelViewProjection (projectionView !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model

  let vc = vertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr



