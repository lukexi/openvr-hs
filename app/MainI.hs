{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal hiding (profile)
import Halive.Utils
import Data.Time
import Graphics.VR.OpenVR
import Control.Lens.Extra
import Data.Maybe
import System.Mem
import System.Clock
import Control.Concurrent
import System.IO
import Foreign
import Control.Monad.Reader
import Data.IORef

type ProfilerChan = Chan (Char, TimeSpec, TimeSpec)

startProfiler :: IO ProfilerChan
startProfiler = liftIO $ do
    eventsChan <- newChan
    perfLog <- openFile "maini-perf.log" WriteMode
    let writeChar = hPutChar perfLog
    _ <- forkOS . forever $ do
        (name, before, after) <- readChan eventsChan
        let nanoSecDiff = timeSpecAsNanoSecs $ after `diffTimeSpec` before
            timeMS = nsToMS nanoSecDiff * 10
        replicateM_ (floor timeMS) (writeChar name) >> writeChar '\n'


    
    return eventsChan

profileToChan :: MonadIO m => ProfilerChan -> Char -> m a -> m a
profileToChan eventsChan name action = do
    let clockType = Monotonic
    --let clockType = Realtime
    before <- liftIO (getTime clockType)
    x <- action
    after <- liftIO (getTime clockType)
    liftIO $ writeChan eventsChan (name, before, after)
    return x

handsFromPoses :: (MonadIO m) => OpenVR -> [(TrackedControllerRole, M44 GLfloat)] -> m [Hand]
handsFromPoses OpenVR{..} handPosesByRole = forM handPosesByRole $ \(controllerRole, pose) -> do
        (x, y, trigger, grip, start) <- getControllerState ovrSystem controllerRole
        let hand = Hand
              { hndMatrix = pose
              , hndXY = realToFrac <$> V2 x y
              , hndTrigger = realToFrac trigger
              , hndGrip = grip
              , hndStart = start
              }
        
        return hand

data Uniforms = Uniforms 
    { uProjectionView :: UniformLocation (M44 GLfloat)
    , uCamera :: UniformLocation (V3 GLfloat)
    } deriving Data


data Hand = Hand 
    { hndGrip :: Bool
    , hndStart :: Bool
    , hndTrigger :: GLfloat
    , hndXY :: V2 GLfloat
    , hndMatrix :: M44 GLfloat
    } deriving Show

side,side2,side3,halfSide :: Int
side     = 8
side2    = side ^ (2::Int)
side3    = side ^ (3::Int)
halfSide = side `div` 2

numInstances :: Num a => a
numInstances = fromIntegral side3

streamingBufferCapacity :: Int
streamingBufferCapacity = numInstances * 800

generateTransforms :: GLfloat -> Int -> M44 GLfloat
generateTransforms t i = 
    let x = fromIntegral $ (i `mod` side)             - halfSide :: GLfloat
        y = fromIntegral $ (i `mod` side2 `div` side) - halfSide :: GLfloat
        z = fromIntegral $ (i `div` side2)            - halfSide :: GLfloat
        m44 = mkTransformation 
                (axisAngle (V3 0 1 0) t) 
                (V3 x y z)
    in m44

--generateColors :: GLfloat -> Int -> V4 GLfloat
--generateColors t i = hslColor hue 0.9 0.6
--    where hue = fromIntegral i / numInstances + t

generateColors :: GLfloat -> Int -> V4 GLfloat
generateColors t i = V4 0.3 0.9 0.6 1

--logIO :: MonadIO m => String -> m ()
--logIO = liftIO . putStrLn



main :: IO ()
main = do
    --profileChan <- startProfiler
    --let profile = profileToChan profileChan
    let profile _ a = a
  
    (window, events) <- reacquire 0 $ createWindow "OpenVR" 1024 768
    
    shader     <- createShaderProgram "app/cubeI.vert" "app/cubeI.frag"
    cubeGeo    <- cubeGeometry (0.1 :: V3 GLfloat) (V3 1 1 1)
    cubeShape  <- makeShape cubeGeo shader
    let _ = cubeShape :: Shape Uniforms


    sab              <- makeSAB streamingBufferCapacity
    transformsBuffer <- bufferDataEmpty GL_STREAM_DRAW streamingBufferCapacity (Proxy :: Proxy (M44 GLfloat))
    colorsBuffer     <- bufferDataEmpty GL_STREAM_DRAW streamingBufferCapacity (Proxy :: Proxy (V4  GLfloat))
    
    let resetShapeInstanceBuffers = withShape cubeShape $ do

            withArrayBuffer transformsBuffer $ do
                resetSABBuffer sab transformsBuffer
                assignMatrixAttributeInstanced shader "aInstanceTransform" GL_FLOAT

            withArrayBuffer colorsBuffer $ do
                resetSABBuffer sab colorsBuffer
                assignFloatAttributeInstanced  shader "aInstanceColor" GL_FLOAT 4
    resetShapeInstanceBuffers
    
    glEnable GL_DEPTH_TEST
    useProgram (sProgram cubeShape)
    mOpenVR <- reacquire 1 $ do
        hmdPresent <- isHMDPresent
        if hmdPresent then createOpenVR else return Nothing
    
    forM_ mOpenVR $ \openVR -> do  
        forM_ (listToMaybe $ ovrEyes openVR) $ \eye -> do
            let (_, _, w, h) = eiViewport eye
            setWindowSize window (fromIntegral w `div` 2) (fromIntegral h `div` 2)
        
        openVRLoop window events cubeShape openVR sab transformsBuffer colorsBuffer resetShapeInstanceBuffers
    
        
    putStrLn "Done!"


openVRLoop :: MonadIO m 
           => Window
           -> Events
           -> Shape Uniforms
           -> OpenVR
           -> StreamingArrayBuffer
           -> ArrayBuffer (M44 GLfloat)
           -> ArrayBuffer (V4 GLfloat)
           -> m a
           -> m ()
openVRLoop window events cubeShape openVR@OpenVR{..} sab transformsBuffer colorsBuffer resetShapeInstanceBuffers = whileWindow window $ do

    --profile 'g' $ liftIO performMinorGC
    processEvents events $ closeOnEscape window
    _ <- pollNextEvent ovrSystem
    (headM44, handPosesByRole) <- waitGetPoses openVR
    
    _hands <- handsFromPoses openVR handPosesByRole

    t <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> liftIO getCurrentTime
    glClearColor 0.2 0.1 (t * 0.3) 1
  
    let viewM44 = inv44 headM44 

    writeSAB sab numInstances resetShapeInstanceBuffers $ do
        fillSABBuffer transformsBuffer  (generateTransforms t)
        fillSABBuffer colorsBuffer      (generateColors t)

    -- Render each eye, with multisampling
    forM_ ovrEyes $ \EyeInfo{..} -> withMultisamplingFramebuffer eiMultisampleFramebuffer $ do 

        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        let (x, y, w, h) = eiViewport
            finalView    = eiEyeHeadTrans !*! viewM44
        glViewport x y w h
        
        -- Render the scene
        render cubeShape sab eiProjection finalView headM44

    -- Submit frames after rendering both
    forM_ ovrEyes $ \EyeInfo{..} -> do
        let MultisampleFramebuffer{..} = eiMultisampleFramebuffer
        submitFrameForEye ovrCompositor eiEye (unTextureID mfbResolveTextureID)

    -- Mirror to window
    forM_ (listToMaybe ovrEyes) $ mirrorOpenVREyeToWindow
    
    --swapBuffers window

render :: (MonadIO m) 
       => Shape Uniforms
       -> StreamingArrayBuffer
       -> M44 GLfloat
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render cubeShape sab projM44 viewM44 headM44 = do
    let Uniforms{..} = sUniforms cubeShape
        projViewM44  = projM44 !*! viewM44
    
    withShape cubeShape $ do
        uniformV3 uCamera (headM44 ^. translation)        
        uniformM44 uProjectionView projViewM44
        drawSAB sab numInstances






