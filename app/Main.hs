{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.VR.OpenVR
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Data.Time
import Control.Monad.State
import Control.Lens.Extra
import Halive.Utils
import CubeUniforms
import Cube

data World = World
  { wldKeyboardShowing :: Bool
  }

newWorld :: World
newWorld = World 
  { wldKeyboardShowing = False
  }

data Hand = Hand 
  { hndGrip :: Bool
  , hndStart :: Bool
  , hndTrigger :: GLfloat
  , hndXY :: V2 GLfloat
  , hndMatrix :: M44 GLfloat
  } deriving Show

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

  mOpenVR <- reacquire 1 $ do
    hmdPresent <- isHMDPresent
    if hmdPresent then createOpenVR else return Nothing
  -- let mOpenVR = Nothing
  
  _ <- flip runStateT newWorld $ case mOpenVR of 
    Just openVR -> do

      forM_ (ovrEyes openVR) $ \eye -> case eiEye eye of
        LeftEye -> do
          let (_, _, w, h) = eiViewport eye
          setWindowSize window (fromIntegral w `div` 2) (fromIntegral h `div` 2)
        _ -> return ()

      openVRLoop window events cubeShape openVR
    Nothing -> flatLoop window events cubeShape
  
      
  putStrLn "Done!"



openVRLoop :: (MonadState World m, MonadIO m) => Window -> Events -> Shape Uniforms -> OpenVR -> m ()
openVRLoop window events cubeShape OpenVR{..} = whileWindow window $ do
  pollNextEvent ovrSystem
  poses <- waitGetPoses ovrCompositor ovrSystem
  let (headPose, handPoses) = case poses of
        [headPose] -> (headPose, [])
        [headPose, onePose] -> (headPose, [onePose])
        [headPose, leftPose, rightPose] -> (headPose, [leftPose, rightPose])
        _ -> (identity, [])

  hands <- forM (zip [0..] handPoses) $ \(i, pose) -> do
    (x, y, trigger, grip, start) <- getControllerState ovrSystem i
    let hand = Hand
          { hndMatrix = pose
          , hndXY = realToFrac <$> V2 x y
          , hndTrigger = realToFrac trigger
          , hndGrip = grip
          , hndStart = start
          }
    when (trigger > 0.5) $
      triggerHapticPulse ovrSystem i 0 100

    when (i == 0) $ do
      keyboardShowing <- gets wldKeyboardShowing
      when (grip && not keyboardShowing) $ do
        showKeyboard
        modify (\world -> world { wldKeyboardShowing = True })
      
      when (not grip) $
        modify (\world -> world { wldKeyboardShowing = False })
    return hand

  let handCubes = flip concatMap hands $ \Hand{..} -> 
        [ Cube
          { _cubMatrix = hndMatrix !*! translateMatrix (V3 0 0 0.05) !*! scaleMatrix (V3 0.4 0.4 1.6)
          , _cubColor = V4 1 0 0 1
          }
        , Cube
          { _cubMatrix = hndMatrix !*! scaleMatrix (V3 0.1 0.1 0.1) !*! translateMatrix (V3 (hndXY ^. _x * 0.5) 0.4 (-hndXY ^. _y) * 0.5)
          , _cubColor = V4 0 1 0 1
          }
        , Cube
          { _cubMatrix = hndMatrix !*! scaleMatrix (V3 0.1 0.1 0.1) !*! translateMatrix (V3 0 (-hndTrigger - 0.2) 0)
          , _cubColor = V4 0 0 1 1
          }
        , Cube
          { _cubMatrix = hndMatrix !*! scaleMatrix (V3 0.1 0.1 0.1) !*! translateMatrix (V3 0 0.2 0.4)
          , _cubColor = if hndStart then V4 1 1 1 1 else V4 0 1 1 1
          }
        , Cube
          { _cubMatrix = hndMatrix !*! scaleMatrix (V3 0.5 0.1 0.1) !*! translateMatrix (V3 0 0 0.5)
          , _cubColor = if hndGrip then V4 1 1 1 1 else V4 1 1 0 1
          }
        ]
  
  now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> liftIO getCurrentTime
  glClearColor 0.2 0.1 (now * 0.3) 1

  let view44 = inv44 headPose
  forM_ ovrEyes $ \eye@EyeInfo{..} -> do

    withFramebuffer eiFramebuffer $ do
      glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
      let (x, y, w, h) = eiViewport
          finalView    = eiEyeHeadTrans !*! view44
      glViewport x y w h

      render cubeShape eiProjection finalView (handCubes ++ worldCubes)

      submitFrameForEye ovrCompositor eiEye eiFramebufferTexture

      mirrorOpenVREyeToWindow eye

  processEvents events $ closeOnEscape window

  swapBuffers window



flatLoop :: (MonadState World m, MonadIO m) => Window -> Events -> Shape Uniforms -> m ()
flatLoop window events cubeShape = do
  let viewMat = lookAt (V3 0 2 0) (V3 0 0 3) (V3 0 1 0)
  projectionMat <- getWindowProjection window 45 0.1 1000
  (x,y,w,h)     <- getWindowViewport window
  whileWindow window $ do
    processEvents events (closeOnEscape window)

    now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> liftIO getCurrentTime
    glClearColor now 0.2 0.5 1

    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    glViewport x y w h

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
      -- We extract eyePos from the view matrix to get eye-to-head offsets baked in
      eyePos = inv44 viewMat ^. translation

  uniformV3 uCamera eyePos

  withVAO (sVAO cubeShape) $ forM_ cubes $ \cube -> do
    uniformV4 uDiffuse (cube ^. cubColor)

    draw (cube ^. cubMatrix) projectionView cubeShape

draw :: MonadIO m => M44 GLfloat -> M44 GLfloat -> Shape Uniforms -> m ()
draw model projectionView shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uModelViewProjection (projectionView !*! model)
  uniformM44 uInverseModel        (inv44 model)
  uniformM44 uModel               model

  let vc = geoVertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr



