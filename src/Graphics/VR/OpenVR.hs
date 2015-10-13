{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Graphics.VR.OpenVR where

import Foreign
-- import Foreign.Ptr
import Foreign.C
import qualified Language.C.Inline.Cpp as C
import Control.Monad.Trans
import Data.Monoid
import Linear.Extra
import Text.RawString.QQ (r)
import Graphics.GL.Pal
import Control.Monad

-- Set up inline-c to gain Cpp and Function Pointer abilities
C.context (C.cppCtx <> C.funCtx)

-- Import OpenVR
C.include "openvr_capi.h"
C.include "stdio.h"

-- Add the VREvent_t type and ButtonMaskFromId method
-- from openvr.h, which are missing from openvr_capi.h
C.verbatim [r|
/** An event posted by the server to all running applications */
struct VREvent_t
{
  EVREventType eventType;
  TrackedDeviceIndex_t trackedDeviceIndex;
  VREvent_Data_t data;
  float eventAgeSeconds;
};

inline uint64_t ButtonMaskFromId( EVRButtonId id ) { return 1ull << id; }
|]

newtype IVRSystem     = IVRSystem     { unIVRSystem     :: CIntPtr } deriving Show
newtype IVRCompositor = IVRCompositor { unIVRCompositor :: CIntPtr } deriving Show

data HmdEye = LeftEye | RightEye deriving (Enum)

data TrackedDeviceClass = TrackedDeviceClassInvalid
                        | TrackedDeviceClassHMD
                        | TrackedDeviceClassController
                        | TrackedDeviceClassTrackingReference
                        | TrackedDeviceClassOther

maxTrackedDeviceCount :: Num b => b
maxTrackedDeviceCount = fromIntegral [C.pure|int{k_unMaxTrackedDeviceCount}|]

trackedDeviceClassToC :: TrackedDeviceClass -> CInt
trackedDeviceClassToC TrackedDeviceClassInvalid           = [C.pure|int{TrackedDeviceClass_Invalid}|]
trackedDeviceClassToC TrackedDeviceClassHMD               = [C.pure|int{TrackedDeviceClass_HMD}|]
trackedDeviceClassToC TrackedDeviceClassController        = [C.pure|int{TrackedDeviceClass_Controller}|]
trackedDeviceClassToC TrackedDeviceClassTrackingReference = [C.pure|int{TrackedDeviceClass_TrackingReference}|]
trackedDeviceClassToC TrackedDeviceClassOther             = [C.pure|int{TrackedDeviceClass_Other}|]

-- | Temporarily allocate an array of the given size, 
-- pass it to a foreign function, then peek it before it is discarded
withArray_ :: (Storable a) => Int -> (Ptr a -> IO ()) -> IO [a]
withArray_ size action = allocaArray size $ \ptr -> do
  _ <- action ptr
  peekArray size ptr


-- | OpenVR matrices are transposed from Linear's
m44FromOpenVRList :: (Fractional a, Real a1) => [a1] -> M44 a
m44FromOpenVRList = transpose . m44FromList . map realToFrac

-- buildM44WithPtr action = m44FromOpenVRList <$> withArray_ 16 action
buildM44WithPtr :: (Ptr b -> IO a) -> IO (M44 GLfloat)
buildM44WithPtr action = fmap transpose . alloca $ \ptr -> do
  let _ = ptr :: Ptr (M44 GLfloat)
  action (castPtr ptr)
  peek ptr

buildM44sWithPtr :: (Fractional a, Real a1, Storable a1) => Int -> (Ptr a1 -> IO ()) -> IO [M44 a]
buildM44sWithPtr count action = splitMatrices <$> withArray_ (16 * count) action
  where splitMatrices [] = []
        splitMatrices ms = 
          let (next, remain) = splitAt 16 ms
          in m44FromOpenVRList next : splitMatrices remain

C.verbatim [r|

void fillFromMatrix44(HmdMatrix44_t matrix, float* out) {
  
  out[0]  = matrix.m[0][0];
  out[1]  = matrix.m[1][0];
  out[2]  = matrix.m[2][0];
  out[3]  = matrix.m[3][0];
  out[4]  = matrix.m[0][1];
  out[5]  = matrix.m[1][1];
  out[6]  = matrix.m[2][1];
  out[7]  = matrix.m[3][1];
  out[8]  = matrix.m[0][2];
  out[9]  = matrix.m[1][2];
  out[10] = matrix.m[2][2];
  out[11] = matrix.m[3][2];
  out[12] = matrix.m[0][3];
  out[13] = matrix.m[1][3];
  out[14] = matrix.m[2][3];
  out[15] = matrix.m[3][3];
}

void fillFromMatrix34(HmdMatrix34_t matrix, float* out) {
  
  out[0]  = matrix.m[0][0];
  out[1]  = matrix.m[1][0];
  out[2]  = matrix.m[2][0];
  out[3]  = 0;
  out[4]  = matrix.m[0][1];
  out[5]  = matrix.m[1][1];
  out[6]  = matrix.m[2][1];
  out[7]  = 0;
  out[8]  = matrix.m[0][2];
  out[9]  = matrix.m[1][2];
  out[10] = matrix.m[2][2];
  out[11] = 0;
  out[12] = matrix.m[0][3];
  out[13] = matrix.m[1][3];
  out[14] = matrix.m[2][3];
  out[15] = 1;
}

// Via https://github.com/ValveSoftware/openvr/wiki/IVRSystem::GetDeviceToAbsoluteTrackingPose

float getSecondsToPhotons(intptr_t system) {
  float fSecondsSinceLastVsync; 
  VR_IVRSystem_GetTimeSinceLastVsync(system, &fSecondsSinceLastVsync, NULL );
  float fDisplayFrequency = VR_IVRSystem_GetFloatTrackedDeviceProperty(
    system,
    k_unTrackedDeviceIndex_Hmd, 
    TrackedDeviceProperty_Prop_DisplayFrequency_Float, 
    NULL);
  
  float fFrameDuration = 1.f / fDisplayFrequency;

  float fVsyncToPhotons = VR_IVRSystem_GetFloatTrackedDeviceProperty(
    system,
    k_unTrackedDeviceIndex_Hmd, 
    TrackedDeviceProperty_Prop_SecondsFromVsyncToPhotons_Float,
    NULL);

  float fPredictedSecondsFromNow = fFrameDuration - fSecondsSinceLastVsync + fVsyncToPhotons;
  return fPredictedSecondsFromNow;
}
|]

-- | Creates the OpenVR System object, which is the main point of interface with OpenVR.
-- Will return Nothing if no headset can be found, or if some other error occurs during initialization.
initOpenVR :: MonadIO m => m (Maybe IVRSystem)
initOpenVR = liftIO $ do
  systemPtr <- [C.block| intptr_t {
    HmdError error = HmdError_None;
    intptr_t system = VR_Init(&error, EVRApplicationType_VRApplication_Scene);

    if (system == 0) {
      char buf[1024];
      sprintf_s(buf, sizeof(buf), 
        "Unable to init VR runtime: %s", 
        VR_GetStringForHmdError(error));
      printf("initOpenVR error: %s\n", buf);
    }

    return system;
    } |]

  return $ if systemPtr == 0 then Nothing else Just (IVRSystem systemPtr)

-- | Gets a reference to the OpenVR Compositor, which is used to submit frames to the headset.
getCompositor :: MonadIO m => m (Maybe IVRCompositor) 
getCompositor = liftIO $ do
  compositorPtr <- [C.block| intptr_t {
    HmdError error = HmdError_None;

    intptr_t compositor = VR_GetGenericInterface(IVRCompositor_Version, &error);

    if (error != HmdError_None) {
      compositor = 0;

      printf("Compositor initialization failed with error: %s\n", VR_GetStringForHmdError(error));
      return 0;
    }

    uint32_t unSize = VR_IVRCompositor_GetLastError(compositor, NULL, 0);
    if (unSize > 1) {
      char buffer[unSize];
      VR_IVRCompositor_GetLastError(compositor, buffer, unSize);
      printf( "Compositor - %s\n", buffer );
      return 0;
    }

    return compositor;

    }|]

  return $ if compositorPtr == 0 then Nothing else Just (IVRCompositor compositorPtr)


-- | Returns the size of the framebuffer you should render to for one eye.
-- Double the width if using a single framebuffer for both eyes.
getRenderTargetSize :: Integral a => MonadIO m => IVRSystem -> m (a, a)
getRenderTargetSize (IVRSystem systemPtr) = liftIO $ do
  (w, h) <- C.withPtrs_ $ \(wPtr, hPtr) -> 
    [C.block| void {
      intptr_t system = $(intptr_t systemPtr);
      VR_IVRSystem_GetRecommendedRenderTargetSize(system, $(uint32_t* wPtr), $(uint32_t* hPtr));
    }|]
  return (fromIntegral w, fromIntegral h)



-- | Returns the viewport to give to glViewport when rendering the given eye.
-- NOTE this doesn't seem to return the correct values for the Oculus 
-- (it returns 1080p rather than the upscaled values for the render buffer)
getEyeViewport :: Integral a => MonadIO m => IVRSystem -> HmdEye -> m (a, a, a, a)
getEyeViewport (IVRSystem systemPtr) eye = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  (x, y, w, h) <- C.withPtrs_ $ \(xPtr, yPtr, wPtr, hPtr) -> 
    [C.block| void {
      intptr_t system = $(intptr_t systemPtr);
      Hmd_Eye eye = $(int eyeNum) == 0 ? Hmd_Eye_Eye_Left : Hmd_Eye_Eye_Right;

      VR_IVRSystem_GetEyeOutputViewport(system, eye, 
        $(uint32_t* xPtr), $(uint32_t* yPtr), $(uint32_t* wPtr), $(uint32_t* hPtr));
    }|]
  return (fromIntegral x, fromIntegral y, fromIntegral w, fromIntegral h)


-- | Returns the projection matrix for the given eye for the given near and far clipping planes.
getEyeProjectionMatrix :: (MonadIO m) => IVRSystem -> HmdEye -> Float -> Float -> m (M44 GLfloat)
getEyeProjectionMatrix (IVRSystem systemPtr) eye (realToFrac -> zNear) (realToFrac -> zFar) = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  buildM44WithPtr $ \ptr ->
    [C.block|void {
      intptr_t system = $(intptr_t systemPtr);

      Hmd_Eye eye = $(int eyeNum) == 0 ? Hmd_Eye_Eye_Left : Hmd_Eye_Eye_Right;

      HmdMatrix44_t projection = VR_IVRSystem_GetProjectionMatrix(system, eye, 
        $(float zNear), $(float zFar), GraphicsAPIConvention_API_OpenGL);

      fillFromMatrix44(projection, $(float* ptr));
    }|]


-- | Returns the offset of each eye from the head pose.
getEyeToHeadTransform :: (MonadIO m) => IVRSystem -> HmdEye -> m (M44 GLfloat)
getEyeToHeadTransform (IVRSystem systemPtr) eye = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  buildM44WithPtr $ \ptr ->
    [C.block|void {
      intptr_t system = $(intptr_t systemPtr);

      Hmd_Eye eye = $(int eyeNum) == 0 ? Hmd_Eye_Eye_Left : Hmd_Eye_Eye_Right;

      HmdMatrix34_t transform = VR_IVRSystem_GetEyeToHeadTransform(system, eye);

      fillFromMatrix34(transform, $(float* ptr));
    }|]

showMirrorWindow :: MonadIO m => IVRCompositor -> m ()
showMirrorWindow (IVRCompositor compositorPtr) = liftIO $ do
  [C.block|void{
    intptr_t compositor = $(intptr_t compositorPtr);
    VR_IVRCompositor_ShowMirrorWindow(compositor);
  }|]

hideMirrorWindow :: MonadIO m => IVRCompositor -> m ()
hideMirrorWindow (IVRCompositor compositorPtr) = liftIO $ do
  [C.block|void{
    intptr_t compositor = $(intptr_t compositorPtr);
    VR_IVRCompositor_HideMirrorWindow(compositor);
  }|]

triggerHapticPulse :: MonadIO m => IVRSystem -> CInt -> CInt -> CUShort -> m ()
triggerHapticPulse system@(IVRSystem systemPtr) controllerNumber axis duration = liftIO $ do
  deviceIndex <- getDeviceIndexOfController system controllerNumber
  [C.block|void {
    intptr_t system = $(intptr_t systemPtr);
    int nDevice = $(int deviceIndex);
    int32_t unAxisId = $(int axis);
    unsigned short usDurationMicroSec = $(unsigned short duration);
    VR_IVRSystem_TriggerHapticPulse(system, nDevice, unAxisId, usDurationMicroSec);
  }|]

-- | Currently just prints out the event
pollNextEvent :: MonadIO m => IVRSystem -> m ()
pollNextEvent (IVRSystem systemPtr) = liftIO $ do
  [C.block|void {
    intptr_t system = $(intptr_t systemPtr);

    VREvent_t event;

    while (VR_IVRSystem_PollNextEvent(system, &event)) {
      char *eventName = VR_IVRSystem_GetEventTypeNameFromEnum(system, event.eventType);
      printf("Got event type: %s\n", eventName);
    }

    
  }|]
  
-- | The controller number here refers to an index into the number of controllers there are,
-- not the TrackedDevice index. The first controller is always controllerNumber 0,
-- the second is controllerNumber 1, and so on, regardless of their TrackedDevice indices.
getControllerState :: MonadIO m => IVRSystem -> CInt -> m (CFloat, CFloat, CFloat, Bool, Bool)
getControllerState system@(IVRSystem systemPtr) controllerNumber = liftIO $ do

  deviceIndex <- getDeviceIndexOfController system controllerNumber
  (x, y, trigger, grip, start) <- C.withPtrs_ $ \(xPtr, yPtr, triggerPtr, gripPtr, startPtr) -> 
    [C.block|void {
      intptr_t system = $(intptr_t systemPtr);
      int nDevice = $(int deviceIndex);

      VRControllerState_t state;
      VR_IVRSystem_GetControllerState(system, nDevice, &state);     
      

      /*
      for (int nAxis; nAxis < k_unControllerStateAxisCount; nAxis++) {
        printf("%i Axis %i: %f \t%f\n", 
          nDevice,
          nAxis, 
          state.rAxis[nAxis].x, 
          state.rAxis[nAxis].y);
      }

      printf("%i Touched: %i\n", nDevice, state.ulButtonTouched);
      printf("%i Pressed: %i\n", nDevice, state.ulButtonPressed);
      */


      // Yes, this is intentional - the values seem to be offset
      // such that x is on rAxis[1].y and y is on rAxis[0].x;
      *$(float* xPtr) = state.rAxis[1].y;
      *$(float* yPtr) = state.rAxis[2].x;

      *$(float* triggerPtr) = state.rAxis[2].y;

      int gripMask = ButtonMaskFromId(EVRButtonId_k_EButton_Grip);
      int menuMask = ButtonMaskFromId(EVRButtonId_k_EButton_ApplicationMenu);

      *$(int* gripPtr)    = (state.ulButtonTouched & gripMask) 
                            == gripMask;
      *$(int* startPtr)   = (state.ulButtonTouched & menuMask) 
                            == menuMask;
      
    }|]
  return (x, y, trigger, grip /= 0, start /= 0)

getDeviceIndexOfController :: MonadIO m => IVRSystem -> CInt -> m CInt
getDeviceIndexOfController (IVRSystem systemPtr) controllerNumber = liftIO $ do
  [C.block|int {
    intptr_t system = $(intptr_t systemPtr);
    int controllerNumber = $(int controllerNumber);

    TrackedDeviceIndex_t deviceIndices[2];
    uint32_t numIndices = VR_IVRSystem_GetSortedTrackedDeviceIndicesOfClass(system, TrackedDeviceClass_Controller, deviceIndices, 2, 0);
    if (numIndices > controllerNumber) {
      return deviceIndices[controllerNumber];
    } else {
      return 0;
    }
  }|]

waitGetPoses :: (MonadIO m) => IVRCompositor -> m (M44 GLfloat)
waitGetPoses (IVRCompositor compositorPtr) = liftIO $ do
  buildM44WithPtr $ \ptr ->
    [C.block|void {
      intptr_t compositor = $(intptr_t compositorPtr);
      TrackedDevicePose_t hmdPose;
      VR_IVRCompositor_WaitGetPoses(compositor, &hmdPose, 1, NULL, 0);

      HmdMatrix34_t transform = hmdPose.mDeviceToAbsoluteTracking;
      fillFromMatrix34(transform, $(float* ptr));
    }|]


-- | Returns the predicted poses of devices of the given class at the next display time
getDevicePosesOfClass :: (Fractional a, MonadIO m) 
                      => IVRSystem -> TrackedDeviceClass -> m [M44 a]
getDevicePosesOfClass system@(IVRSystem systemPtr) trackedDeviceClass = liftIO $ do
  let trackedDeviceClassInt = trackedDeviceClassToC trackedDeviceClass
  count <- getNumDevicesOfClass system trackedDeviceClass

  buildM44sWithPtr count $ \ptr ->
    [C.block| void {
      intptr_t system = $(intptr_t systemPtr);

      TrackedDevicePose_t trackedDevicePoses[k_unMaxTrackedDeviceCount];
      float secondsToPhotons = getSecondsToPhotons(system);

      VR_IVRSystem_GetDeviceToAbsoluteTrackingPose(system, 
        TrackingUniverseOrigin_TrackingUniverseStanding,
        secondsToPhotons,
        trackedDevicePoses,
        k_unMaxTrackedDeviceCount
        );

      int offset = 0;
      for (int nDevice = 0; nDevice < k_unMaxTrackedDeviceCount; nDevice++) {
        TrackedDeviceClass deviceClass = VR_IVRSystem_GetTrackedDeviceClass(system, nDevice);

        if (deviceClass == $(int trackedDeviceClassInt)) {
          TrackedDevicePose_t pose = trackedDevicePoses[nDevice];
          HmdMatrix34_t transform = pose.mDeviceToAbsoluteTracking;
          fillFromMatrix34(transform, $(float* ptr) + 16 * offset);
          offset++;
        }
        
      }
    }|]

getNumDevicesOfClass :: (Num a, MonadIO m) 
                     => IVRSystem -> TrackedDeviceClass -> m a
getNumDevicesOfClass (IVRSystem systemPtr) trackedDeviceClass = liftIO $ do
  let trackedDeviceClassInt = trackedDeviceClassToC trackedDeviceClass
  fromIntegral <$> 
    [C.block| int {
      intptr_t system = $(intptr_t systemPtr);
      int numDevicesOfClass = 0;

      for (int nDevice = 0; nDevice < k_unMaxTrackedDeviceCount; nDevice++) {
        TrackedDeviceClass deviceClass = VR_IVRSystem_GetTrackedDeviceClass(system, nDevice);

        if (deviceClass == $(int trackedDeviceClassInt)) {
          numDevicesOfClass++;
        }
      }

      return numDevicesOfClass;
    }|]

-- | Submits a frame for each eye using the given textureID as a source,
-- where the texture is expected to be double the width of getRenderTargetSize 
-- and contain the images for both eyes side by size
-- NOTE: I haven't been able to get this method to work!!
-- Use submitFrameForEye and two framebuffers.
submitFrame :: (Integral a, MonadIO m) => IVRCompositor -> a -> m ()
submitFrame (IVRCompositor compositorPtr) (fromIntegral -> framebufferTextureID) = liftIO $ do

  [C.block|void {
    intptr_t compositor = $(intptr_t compositorPtr);

    //                               xMin yMin xMax yMax
    VRTextureBounds_t leftBounds  = {0,   0,   0.5, 1};
    VRTextureBounds_t rightBounds = {0.5, 0,   1,   1};

    VR_IVRCompositor_Submit(compositor, Hmd_Eye_Eye_Left,  GraphicsAPIConvention_API_OpenGL, 
      (void*)$(unsigned int framebufferTextureID), &leftBounds, VRSubmitFlags_t_Submit_Default);
    VR_IVRCompositor_Submit(compositor, Hmd_Eye_Eye_Right, GraphicsAPIConvention_API_OpenGL, 
      (void*)$(unsigned int framebufferTextureID), &rightBounds, VRSubmitFlags_t_Submit_Default);
  }|]

-- | Submits a frame for the given eye
submitFrameForEye :: (Integral a, MonadIO m) => IVRCompositor -> HmdEye -> a -> m ()
submitFrameForEye (IVRCompositor compositorPtr) eye (fromIntegral -> framebufferTextureID) = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  [C.block|void {
    intptr_t compositor = $(intptr_t compositorPtr);
    Hmd_Eye eye = $(int eyeNum) == 0 ? Hmd_Eye_Eye_Left : Hmd_Eye_Eye_Right;

    VR_IVRCompositor_Submit(compositor, eye,  GraphicsAPIConvention_API_OpenGL, 
      (void*)$(unsigned int framebufferTextureID), NULL, VRSubmitFlags_t_Submit_Default);
  }|]


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

createOpenVR :: IO (Maybe OpenVR)
createOpenVR = do
  putStrLn "Starting OpenVR"
  mSystem <- initOpenVR

  case mSystem of
    Nothing -> putStrLn "Couldn't create OpenVR system :*(" >> return Nothing
    Just system -> do
      putStrLn $ "Got system: " ++ show system
      (w,h) <- getRenderTargetSize system
      print (w,h)
      eyes <- forM [LeftEye, RightEye] $ \eye -> do
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
          showMirrorWindow compositor
          return . Just $ OpenVR
            { ovrSystem = system
            , ovrCompositor = compositor
            , ovrEyes = eyes
            }