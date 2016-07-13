{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
import Graphics.VR.OpenVR
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Data.Time
import Control.Monad
import Data.Maybe
main :: IO ()
main = do

    (window, _, eventsChan) <- createWindow "OpenVR" 1024 768

    mOpenVR <- createOpenVR
    forM_ mOpenVR $ \openVR@OpenVR{..} -> do

        -- Define render function
        let renderScene _projM44 _viewM44 = do
                now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> getCurrentTime
                glClearColor 0.2 0.1 (now * 0.3) 1
                glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        whileWindow window $ do
            -- Handle window events
            events <- gatherEvents eventsChan
            forM_ events $ closeOnEscape window

            -- Gather VR events
            _ <- pollNextEvent ovrSystem

            -- Get head and hand poses
            (headPose, handPosesByRole) <- waitGetPoses openVR

            -- Pair hand poses with button/trackpad/trigger states
            _hands <- forM handPosesByRole $ \(controllerRole, pose) -> do
                (x, y, trigger, grip, start) <- getControllerState ovrSystem controllerRole
                return (pose, x, y, trigger, grip, start)

            let viewM44 = inv44 headPose
            -- Render each eye, with multisampling
            forM_ ovrEyes $ \EyeInfo{..} -> withMultisamplingFramebuffer eiMultisampleFramebuffer $ do

                glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
                let (x, y, w, h) = eiViewport
                    finalView    = eiEyeHeadTrans !*! viewM44
                glViewport x y w h

                -- Render the scene
                renderScene eiProjection finalView

            -- Submit rendered frames to OpenVR
            forM_ ovrEyes $ \EyeInfo{..} -> do
                let MultisampleFramebuffer{..} = eiMultisampleFramebuffer
                submitFrameForEye ovrCompositor eiEye (unTextureID mfbResolveTextureID)

            -- Finally, mirror.
            forM_ (listToMaybe ovrEyes) $ \eye -> do
                (winW, winH) <- getWindowSize window
                mirrorOpenVREyeToWindow eye (fromIntegral winW) (fromIntegral winH)
            swapBuffers window
