{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.VR.OpenVR where

import Foreign
import Foreign.C
import qualified Language.C.Inline as C
import Control.Monad.Trans
import Data.Monoid
import Linear.Extra
import Text.RawString.QQ (r)
import Graphics.GL.Pal
import Control.Monad
import Data.List (sortOn)

-- Set up inline-c to gain Cpp and Function Pointer abilities
C.context (C.baseCtx <> C.funCtx)

-- Import OpenVR
C.include "openvr_capi.h"
C.include "stdio.h"
C.include "string.h"

newtype IVRSystem     = IVRSystem     { unIVRSystem     :: CIntPtr } deriving Show
newtype IVRCompositor = IVRCompositor { unIVRCompositor :: CIntPtr } deriving Show

data HmdEye = LeftEye | RightEye deriving (Enum, Eq, Show)

data TrackedDeviceClass = TrackedDeviceClassInvalid
                        | TrackedDeviceClassHMD
                        | TrackedDeviceClassController
                        | TrackedDeviceClassTrackingReference
                        | TrackedDeviceClassOther

maxTrackedDeviceCount :: Num b => b
maxTrackedDeviceCount = fromIntegral [C.pure|int{k_unMaxTrackedDeviceCount}|]

trackedDeviceClassToC :: TrackedDeviceClass -> CInt
trackedDeviceClassToC TrackedDeviceClassInvalid           = [C.pure|int{ETrackedDeviceClass_TrackedDeviceClass_Invalid}|]
trackedDeviceClassToC TrackedDeviceClassHMD               = [C.pure|int{ETrackedDeviceClass_TrackedDeviceClass_HMD}|]
trackedDeviceClassToC TrackedDeviceClassController        = [C.pure|int{ETrackedDeviceClass_TrackedDeviceClass_Controller}|]
trackedDeviceClassToC TrackedDeviceClassTrackingReference = [C.pure|int{ETrackedDeviceClass_TrackedDeviceClass_TrackingReference}|]
trackedDeviceClassToC TrackedDeviceClassOther             = [C.pure|int{ETrackedDeviceClass_TrackedDeviceClass_Other}|]

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
buildM44WithPtr :: (Ptr b -> IO ()) -> IO (M44 GLfloat)
buildM44WithPtr action = fmap transpose . alloca $ \ptr -> do
  let _ = ptr :: Ptr (M44 GLfloat)
  action (castPtr ptr)
  peek ptr

buildM44sWithPtr :: Int -> (Ptr a -> IO ()) -> IO [M44 GLfloat]
buildM44sWithPtr count action = fmap transpose <$> withArray_ count (action . castPtr)

C.verbatim [r|

#define g_trackedDevicePosesCount 16
TrackedDevicePose_t g_trackedDevicePoses[g_trackedDevicePosesCount];

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
    ETrackedDeviceProperty_Prop_DisplayFrequency_Float, 
    NULL);
  
  float fFrameDuration = 1.f / fDisplayFrequency;

  float fVsyncToPhotons = VR_IVRSystem_GetFloatTrackedDeviceProperty(
    system,
    k_unTrackedDeviceIndex_Hmd, 
    ETrackedDeviceProperty_Prop_DisplayFrequency_Float,
    NULL);

  float fPredictedSecondsFromNow = fFrameDuration - fSecondsSinceLastVsync + fVsyncToPhotons;
  return fPredictedSecondsFromNow;
}

|]

isHMDPresent :: MonadIO m => m Bool
isHMDPresent = toEnum . fromIntegral <$> liftIO [C.block| int {

  return VR_IsHmdPresent() ? 1 : 0;

  }|]

-- | Creates the OpenVR System object, which is the main point of interface with OpenVR.
-- Will return Nothing if no headset can be found, or if some other error occurs during initialization.
initOpenVR :: MonadIO m => m (Maybe IVRSystem)
initOpenVR = liftIO $ do
  systemPtr <- [C.block| intptr_t {
    EVRInitError err = EVRInitError_VRInitError_None;
    intptr_t system = VR_Init(&err, EVRApplicationType_VRApplication_Scene);

    if (system == 0) {
      printf("initOpenVR error: %s\n", VR_GetVRInitErrorAsEnglishDescription(err));
    }

    return system;
    } |]

  return $ if systemPtr == 0 then Nothing else Just (IVRSystem systemPtr)

-- | Gets a reference to the OpenVR Compositor, which is used to submit frames to the headset.
getCompositor :: MonadIO m => m (Maybe IVRCompositor) 
getCompositor = liftIO $ do
  compositorPtr <- [C.block| intptr_t {
    EVRInitError error = EVRInitError_VRInitError_None;

    intptr_t compositor = VR_GetGenericInterface(IVRCompositor_Version, &error);

    if (error != EVRInitError_VRInitError_None) {
      compositor = 0;

      printf("Compositor initialization failed with error: %s\n", VR_GetVRInitErrorAsEnglishDescription(error));
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


-- | Returns the projection matrix for the given eye for the given near and far clipping planes.
getEyeProjectionMatrix :: (MonadIO m) => IVRSystem -> HmdEye -> Float -> Float -> m (M44 GLfloat)
getEyeProjectionMatrix (IVRSystem systemPtr) eye (realToFrac -> zNear) (realToFrac -> zFar) = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  buildM44WithPtr $ \ptr ->
    [C.block|void {
      intptr_t system = $(intptr_t systemPtr);

      EVREye eye = $(int eyeNum) == 0 ? EVREye_Eye_Left : EVREye_Eye_Right;

      HmdMatrix44_t projection = VR_IVRSystem_GetProjectionMatrix(system, eye, 
        $(float zNear), $(float zFar), EGraphicsAPIConvention_API_OpenGL);

      fillFromMatrix44(projection, $(float* ptr));
    }|]


-- | Returns the offset of each eye from the head pose.
getEyeToHeadTransform :: (MonadIO m) => IVRSystem -> HmdEye -> m (M44 GLfloat)
getEyeToHeadTransform (IVRSystem systemPtr) eye = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  buildM44WithPtr $ \ptr ->
    [C.block|void {
      intptr_t system = $(intptr_t systemPtr);

      EVREye eye = $(int eyeNum) == 0 ? EVREye_Eye_Left : EVREye_Eye_Right;

      HmdMatrix34_t transform = VR_IVRSystem_GetEyeToHeadTransform(system, eye);

      fillFromMatrix34(transform, $(float* ptr));
    }|]

isUsingLighthouse :: MonadIO m => IVRSystem -> m Bool
isUsingLighthouse (IVRSystem systemPtr) = liftIO $ do
  foundLighthouse <- [C.block|int {
    intptr_t system = $(intptr_t systemPtr);
    bool foundLighthouse = 0;
    for (int nDevice = 0; nDevice < k_unMaxTrackedDeviceCount; nDevice++) {
      char trackingSystemName[k_unTrackingStringSize];
      ETrackedPropertyError error;
      VR_IVRSystem_GetStringTrackedDeviceProperty(
        system, nDevice, 
        ETrackedDeviceProperty_Prop_TrackingSystemName_String, 
        trackingSystemName, k_unTrackingStringSize, &error);
      if (strcmp(trackingSystemName, "lighthouse") == 0) {
        foundLighthouse = 1;
      }
    }
    return foundLighthouse;
    }|]
  return (foundLighthouse == 1)
  

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

resetSeatedZeroPose :: MonadIO m => IVRSystem -> m ()
resetSeatedZeroPose (IVRSystem systemPtr) = liftIO $ do
  [C.block|void{
    intptr_t system = $(intptr_t systemPtr);
    VR_IVRSystem_ResetSeatedZeroPose(system);
  }|]

showKeyboard :: MonadIO m => m ()
showKeyboard = liftIO $ do
  [C.block|void{
    EVROverlayError err;
    VROverlayHandle_t overlayHandle;
    printf("Overlay: %lli ", VROverlay());
    err = VR_IVROverlay_CreateOverlay(VROverlay(), "myOverlayKey", "My Friendly Overlay", &overlayHandle);
    printf("Overlay error: %s\n", VR_IVROverlay_GetOverlayErrorNameFromEnum(VROverlay(), err));
    VR_IVROverlay_SetOverlayFromFile(VROverlay(), overlayHandle, "C:\\Users\\lukex_000\\Pictures\\Raptor.jpg");
    printf("Overlay error: %s\n", VR_IVROverlay_GetOverlayErrorNameFromEnum(VROverlay(), err));
    err = VR_IVROverlay_ShowOverlay(VROverlay(), overlayHandle);
    printf("Overlay error: %s\n", VR_IVROverlay_GetOverlayErrorNameFromEnum(VROverlay(), err));
    const char * pchDescription = "MyDescription";
    const char * pchExistingText = "";
    uint32_t unCharMax = 256;
    bool bUseMinimalMode = 1;
    uint64_t uUserValue = 0;
    err = VR_IVROverlay_ShowKeyboard(VROverlay(), 
    // err = VR_IVROverlay_ShowKeyboardForOverlay(VROverlay(), overlayHandle,
      EGamepadTextInputMode_k_EGamepadTextInputModeNormal, 
      EGamepadTextInputLineMode_k_EGamepadTextInputLineModeSingleLine, 
      pchDescription, 
      unCharMax, 
      pchExistingText, 
      bUseMinimalMode, 
      uUserValue);
    printf("Overlay error: %s\n", VR_IVROverlay_GetOverlayErrorNameFromEnum(VROverlay(), err));
  }|]

hideKeyboard :: MonadIO m => m ()
hideKeyboard = liftIO $ do
  [C.block|void{
    VR_IVROverlay_HideKeyboard(VROverlay());
  }|]


triggerHapticPulse :: MonadIO m => IVRSystem -> CInt -> CInt -> CUShort -> m ()
triggerHapticPulse system@(IVRSystem systemPtr) controllerNumber axis duration = liftIO $ do
  deviceIndex <- getDeviceIndexOfControllerRole system controllerNumber
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

      if (event.eventType == EVREventType_VREvent_KeyboardCharInput) {
        printf("Got keyboard character event: %s\n", eventName);
      }
      printf("Got event type: %s\n", eventName);
    }

    
  }|]
  
-- | The controller number here refers to an index into the number of controllers there are,
-- not the TrackedDevice index. The first controller is always controllerNumber 0,
-- the second is controllerNumber 1, and so on, regardless of their TrackedDevice indices.
getControllerState :: MonadIO m => IVRSystem -> CInt -> m (CFloat, CFloat, CFloat, Bool, Bool)
getControllerState system@(IVRSystem systemPtr) controllerNumber = liftIO $ do

  deviceIndex <- getDeviceIndexOfControllerRole system controllerNumber
  (x, y, trigger, grip, start) <- C.withPtrs_ $ \(xPtr, yPtr, triggerPtr, gripPtr, startPtr) -> 
    [C.block|void {
      intptr_t system = $(intptr_t systemPtr);
      int nDevice = $(int deviceIndex);

      VRControllerState_t state;
      VR_IVRSystem_GetControllerState(system, nDevice, &state);     
      
      // for (int nAxis; nAxis < k_unControllerStateAxisCount; nAxis++) {
      //   printf("%i Axis %i: %f \t%f\n", 
      //     nDevice,
      //     nAxis, 
      //     state.rAxis[nAxis].x, 
      //     state.rAxis[nAxis].y);
      // }
      // 

      // printf("%i Touched: %i\n", nDevice, state.ulButtonTouched);
      // printf("%i Pressed: %i\n", nDevice, state.ulButtonPressed);
      
      *$(float* xPtr) = state.rAxis[0].x;
      *$(float* yPtr) = state.rAxis[0].y;

      *$(float* triggerPtr) = state.rAxis[1].x;

      int gripMask = ButtonMaskFromId(EVRButtonId_k_EButton_Grip);
      int menuMask = ButtonMaskFromId(EVRButtonId_k_EButton_ApplicationMenu);

      *$(int* gripPtr)    = (state.ulButtonPressed & gripMask) 
                            == gripMask;
      *$(int* startPtr)   = (state.ulButtonPressed & menuMask) 
                            == menuMask;
    }|]
  return (x, y, trigger, grip /= 0, start /= 0)

getDeviceIndexOfControllerRole :: MonadIO m => IVRSystem -> CInt -> m CInt
getDeviceIndexOfControllerRole (IVRSystem systemPtr) controllerRole = liftIO $ do
  [C.block|int {
    intptr_t system = $(intptr_t systemPtr);
    int controllerRole = $(int controllerRole);
    return VR_IVRSystem_GetTrackedDeviceIndexForControllerRole(system, controllerRole);
  }|]


-- | Get the roles and matrices for the current frame.
-- (Nb. this function could use a few improvements : ) — we're using globals 
-- g_trackedDevicePoses and g_trackedDevicePosesCount just for storing
-- the poses across FFI calls to then pack them into M44s. We're also calling
-- the FFI 3 times. Better would be to preallocate some memory in initOpenVR, 
-- write to it with one FFI call here, return the count, then pull the data into Haskell land.
waitGetPoses :: (MonadIO m) => IVRCompositor -> IVRSystem -> m [(CInt, M44 GLfloat)]
waitGetPoses (IVRCompositor compositorPtr) (IVRSystem systemPtr) = liftIO $ do

  -- First count how many valid HMD and controller poses exist so we can allocate an array
  numPoses <- fromIntegral <$> [C.block|int {
      intptr_t compositor = $(intptr_t compositorPtr);
      intptr_t system = $(intptr_t systemPtr);
      int numPoses = 0;
      VR_IVRCompositor_WaitGetPoses(compositor, 
        g_trackedDevicePoses, g_trackedDevicePosesCount, NULL, 0);
      for (int nDevice = 0; nDevice < g_trackedDevicePosesCount; nDevice++) {
        TrackedDevicePose_t pose = g_trackedDevicePoses[nDevice];
        if (pose.bPoseIsValid) {
          TrackedDeviceClass deviceClass = VR_IVRSystem_GetTrackedDeviceClass(system, nDevice);
          if (deviceClass == ETrackedDeviceClass_TrackedDeviceClass_HMD ||
              deviceClass == ETrackedDeviceClass_TrackedDeviceClass_Controller) {
            numPoses++;
          }
        }
      }
      return numPoses;
    }|]

  -- Then fill our coffers with them
  matrices <- buildM44sWithPtr numPoses $ \ptr -> 
    [C.block|void {
      intptr_t system = $(intptr_t systemPtr);
      int offset = 0;
      for (int nDevice = 0; nDevice < g_trackedDevicePosesCount; nDevice++) {
        TrackedDevicePose_t pose = g_trackedDevicePoses[nDevice];
        if (pose.bPoseIsValid) {
          TrackedDeviceClass deviceClass = VR_IVRSystem_GetTrackedDeviceClass(system, nDevice);
          if (deviceClass == ETrackedDeviceClass_TrackedDeviceClass_HMD ||
              deviceClass == ETrackedDeviceClass_TrackedDeviceClass_Controller) {
            HmdMatrix34_t transform = pose.mDeviceToAbsoluteTracking;
            fillFromMatrix34(transform, $(float* ptr) + 16 * offset);
            offset++;
          }
        }
      }
    }|]
  -- Also nab the roles so we can match up a controller poses with their states
  roles <- withArray_ numPoses $ \ptr ->
    [C.block|void {
      intptr_t system = $(intptr_t systemPtr);
      int* roles = $(int* ptr);
      int offset = 0;
      for (int nDevice = 0; nDevice < g_trackedDevicePosesCount; nDevice++) {
        TrackedDevicePose_t pose = g_trackedDevicePoses[nDevice];
        if (pose.bPoseIsValid) {
          TrackedDeviceClass deviceClass = VR_IVRSystem_GetTrackedDeviceClass(system, nDevice);
          if (deviceClass == ETrackedDeviceClass_TrackedDeviceClass_HMD ||
              deviceClass == ETrackedDeviceClass_TrackedDeviceClass_Controller) {
            roles[offset] = VR_IVRSystem_GetControllerRoleForTrackedDeviceIndex(nDevice);
            offset++;
          }
        }
      }
    }|]
  return (sortOn fst (zip roles matrices))


-- | Returns the predicted poses of devices of the given class at the next display time
getDevicePosesOfClass :: (MonadIO m) 
                      => IVRSystem -> TrackedDeviceClass -> m [M44 GLfloat]
getDevicePosesOfClass system@(IVRSystem systemPtr) trackedDeviceClass = liftIO $ do
  let trackedDeviceClassInt = trackedDeviceClassToC trackedDeviceClass
  count <- getNumDevicesOfClass system trackedDeviceClass

  buildM44sWithPtr count $ \ptr ->
    [C.block| void {
      intptr_t system = $(intptr_t systemPtr);

      TrackedDevicePose_t trackedDevicePoses[k_unMaxTrackedDeviceCount];
      float secondsToPhotons = getSecondsToPhotons(system);

      VR_IVRSystem_GetDeviceToAbsoluteTrackingPose(system, 
        ETrackingUniverseOrigin_TrackingUniverseStanding,
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

    Texture_t texture = { 
      (void*)$(unsigned long long framebufferTextureID), 
      EGraphicsAPIConvention_API_OpenGL,
      EColorSpace_ColorSpace_Auto
    };

    VR_IVRCompositor_Submit(compositor, EVREye_Eye_Left, 
      &texture, &leftBounds, EVRSubmitFlags_Submit_Default);
    VR_IVRCompositor_Submit(compositor, EVREye_Eye_Right, 
      &texture, &rightBounds, EVRSubmitFlags_Submit_Default);
  }|]

-- | Submits a frame for the given eye
submitFrameForEye :: (Integral a, MonadIO m) => IVRCompositor -> HmdEye -> a -> m ()
submitFrameForEye (IVRCompositor compositorPtr) eye (fromIntegral -> framebufferTextureID) = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  [C.block|void {
    intptr_t compositor = $(intptr_t compositorPtr);
    EVREye eye = $(int eyeNum) == 0 ? EVREye_Eye_Left : EVREye_Eye_Right;

    Texture_t texture = { 
      (void*)$(unsigned long long framebufferTextureID), 
      EGraphicsAPIConvention_API_OpenGL,
      EColorSpace_ColorSpace_Linear
    };

    VR_IVRCompositor_Submit(compositor, eye, 
      &texture, NULL, EVRSubmitFlags_Submit_Default);
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
      -- putStrLn $ "Got system: " ++ show system
      (w,h) <- getRenderTargetSize system
      eyes <- forM [LeftEye, RightEye] $ \eye -> do
        eyeProj  <- getEyeProjectionMatrix system eye 0.1 100
        eyeTrans <- inv44 <$> getEyeToHeadTransform system eye

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
          -- showMirrorWindow compositor
          return . Just $ OpenVR
            { ovrSystem = system
            , ovrCompositor = compositor
            , ovrEyes = eyes
            }

mirrorOpenVREyeToWindow :: MonadIO m => EyeInfo -> m ()
mirrorOpenVREyeToWindow EyeInfo{..} = when (eiEye == LeftEye) $ do
  let (x, y, w, h) = eiViewport

  glBindFramebuffer GL_READ_FRAMEBUFFER eiFramebuffer
  glBindFramebuffer GL_DRAW_FRAMEBUFFER 0

  glBlitFramebuffer x y w h x y w h GL_COLOR_BUFFER_BIT GL_LINEAR
  return ()
