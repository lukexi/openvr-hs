{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Graphics.VR.OpenVR where

import Foreign
-- import Foreign.Ptr
import Foreign.C
import qualified Language.C.Inline.Cpp as C
import Control.Monad.Trans
import Data.Monoid
import Linear.Extra
import Text.RawString.QQ (r)
import Debug.Trace

-- Set up inline-c to gain Cpp and Function Pointer abilities
C.context (C.cppCtx <> C.funCtx)
-- Import OpenVR
C.include "openvr_capi.h"
C.include "stdio.h"

newtype IVRSystem     = IVRSystem     { unIVRSystem     :: CIntPtr } deriving Show
newtype IVRCompositor = IVRCompositor { unIVRCompositor :: CIntPtr } deriving Show

data HmdEye = LeftEye | RightEye deriving (Enum)

-- | Temporarily allocate an array of the given size, 
-- pass it to a foreign function, then peek it before it is discarded

withArray_ :: (Storable a) => Int -> (Ptr a -> IO ()) -> IO [a]
withArray_ size action = allocaArray size $ \ptr -> do
  _ <- action ptr
  peekArray size ptr

buildM44WithPtr action = transpose . m44FromList . map realToFrac <$> withArray_ 16 action

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
  
  printf("C++: %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f\n", 
    out[0],out[1],out[2],out[3],
    out[4],out[5],out[6],out[7],
    out[8],out[9],out[10],out[11],
    out[12],out[13],out[14],out[15]);
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

    if (error != HmdError_None)
    {
      compositor = 0;

      printf("Compositor initialization failed with error: %s\n", VR_GetStringForHmdError(error));
      return 0;
    }

    uint32_t unSize = VR_IVRCompositor_GetLastError(compositor, NULL, 0);
    if (unSize > 1)
    {
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
getEyeProjectionMatrix :: (Fractional a, MonadIO m) => IVRSystem -> HmdEye -> Float -> Float -> m (M44 a)
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
getEyeToHeadTransform :: (Fractional a, MonadIO m) => IVRSystem -> HmdEye -> m (M44 a)
getEyeToHeadTransform (IVRSystem systemPtr) eye = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  buildM44WithPtr $ \ptr ->
    [C.block|void {
      intptr_t system = $(intptr_t systemPtr);

      Hmd_Eye eye = $(int eyeNum) == 0 ? Hmd_Eye_Eye_Left : Hmd_Eye_Eye_Right;

      HmdMatrix34_t transform = VR_IVRSystem_GetEyeToHeadTransform(system, eye);

      fillFromMatrix34(transform, $(float* ptr));
    }|]
  


waitGetPoses :: (Fractional a, MonadIO m) => IVRCompositor -> IVRSystem -> m (M44 a)
waitGetPoses (IVRCompositor compositorPtr) (IVRSystem systemPtr) = liftIO $ do
  buildM44WithPtr $ \ptr ->
    [C.block|void {
      intptr_t compositor = $(intptr_t compositorPtr);
      TrackedDevicePose_t trackedDevicePoses[k_unMaxTrackedDeviceCount];
      VR_IVRCompositor_WaitGetPoses(compositor, trackedDevicePoses, k_unMaxTrackedDeviceCount, NULL, 0);

      intptr_t system = $(intptr_t systemPtr);
      for (int nDevice = 0; nDevice < k_unMaxTrackedDeviceCount; nDevice++) {
        TrackedDevicePose_t pose = trackedDevicePoses[nDevice];
        TrackedDeviceClass deviceClass = VR_IVRSystem_GetTrackedDeviceClass(system, nDevice);

        if (pose.bPoseIsValid && deviceClass == TrackedDeviceClass_HMD) {
          HmdMatrix34_t transform = pose.mDeviceToAbsoluteTracking;
          fillFromMatrix34(transform, $(float* ptr));
        }
      }
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