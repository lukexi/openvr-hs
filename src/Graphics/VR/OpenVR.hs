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

-- Set up inline-c to gain Cpp and Function Pointer abilities
C.context (C.cppCtx <> C.funCtx)
-- Import OpenVR
C.include "openvr_capi.h"
C.include "stdio.h"

newtype IVRSystem = IVRSystem { unIVRSystem :: CIntPtr } deriving Show

newtype IVRCompositor = IVRCompositor { unIVRCompositor :: CIntPtr } deriving Show

-- | Temporarily allocate an array of the given size, 
-- pass it to a foreign function, then peek it before it is discarded

withArray_ :: (Storable a) => Int -> (Ptr a -> IO ()) -> IO [a]
withArray_ size action = allocaArray size $ \ptr -> do
  _ <- action ptr
  peekArray size ptr

buildM44WithPtr action = m44FromList . map realToFrac <$> withArray_ 16 action

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
|]

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

    HmdMatrix44_t matrix = VR_IVRSystem_GetProjectionMatrix(system,
      Hmd_Eye_Eye_Left, 0.1, 10000, GraphicsAPIConvention_API_OpenGL);

    return system;
    } |]

  return $ if systemPtr == 0 then Nothing else Just (IVRSystem systemPtr)

getRenderTargetSize :: MonadIO m => IVRSystem -> m (Word32, Word32)
getRenderTargetSize (IVRSystem systemPtr) = liftIO $
  C.withPtrs_ $ \(xPtr, yPtr) -> 
    [C.block| void {
      intptr_t system = $(intptr_t systemPtr);
      VR_IVRSystem_GetRecommendedRenderTargetSize(system, $(uint32_t* xPtr), $(uint32_t* yPtr));
    }|]

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

    // uint32_t unSize = compositor->GetLastError(NULL, 0);
    // if (unSize > 1)
    // {
    //   char* buffer = new char[unSize];
    //   compositor->GetLastError(buffer, unSize);
    //   printf( "Compositor - %s\n", buffer );
    //   delete [] buffer;
    //   return 0;
    // }

    return compositor;

    }|]

  return $ if compositorPtr == 0 then Nothing else Just (IVRCompositor compositorPtr)

submitFrame (IVRCompositor compositorPtr) (fromIntegral -> framebufferTextureID) (fromIntegral -> width) (fromIntegral -> height) = do

  [C.block|void {
    intptr_t compositor = $(intptr_t compositorPtr);
    float width = $(float width);
    float height = $(float height);
    float halfWidth = width / 2;
    VRTextureBounds_t leftBounds = {0, 0, halfWidth, height};
    VRTextureBounds_t rightBounds = {halfWidth, 0, width, height};
    VR_IVRCompositor_Submit(compositor, Hmd_Eye_Eye_Left,  GraphicsAPIConvention_API_OpenGL, 
      (void*)$(unsigned int framebufferTextureID), &leftBounds, VRSubmitFlags_t_Submit_Default);
    VR_IVRCompositor_Submit(compositor, Hmd_Eye_Eye_Right, GraphicsAPIConvention_API_OpenGL, 
      (void*)$(unsigned int framebufferTextureID), &rightBounds, VRSubmitFlags_t_Submit_Default);
  }|]

waitGetPoses (IVRCompositor compositorPtr) (IVRSystem systemPtr) = do
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
          fillFromMatrix34(pose.mDeviceToAbsoluteTracking, $(float* ptr));
        }
      }
    }|]


data HmdEye = LeftEye | RightEye deriving (Enum)

getEyeToHeadTransform :: (Fractional a, MonadIO m) => IVRSystem -> HmdEye -> m (M44 a)
getEyeToHeadTransform (IVRSystem systemPtr) eye = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  buildM44WithPtr $ \ptr ->
    [C.block|void {
      intptr_t system = $(intptr_t systemPtr);

      Hmd_Eye eye = $(int eyeNum) == 0 ? Hmd_Eye_Eye_Left : Hmd_Eye_Eye_Right;

      HmdMatrix34_t transform = VR_IVRSystem_GetEyeToHeadTransform(system, eye);

      float* out = $(float* ptr);
      out[0]  = transform.m[0][0];
      out[1]  = transform.m[1][0];
      out[2]  = transform.m[2][0];
      out[3]  = 0;
      out[4]  = transform.m[0][1];
      out[5]  = transform.m[1][1];
      out[6]  = transform.m[2][1];
      out[7]  = 0;
      out[8]  = transform.m[0][2];
      out[9]  = transform.m[1][2];
      out[10] = transform.m[2][2];
      out[11] = 0;
      out[12] = transform.m[0][3];
      out[13] = transform.m[1][3];
      out[14] = transform.m[2][3];
      out[15] = 1;

    }|]
  


getEyeProjectionMatrix :: (Fractional a, MonadIO m) => IVRSystem -> HmdEye -> Float -> Float -> m (M44 a)
getEyeProjectionMatrix (IVRSystem systemPtr) eye (realToFrac -> zNear) (realToFrac -> zFar) = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  buildM44WithPtr $ \ptr ->
    [C.block|void {
      intptr_t system = $(intptr_t systemPtr);

      Hmd_Eye eye = $(int eyeNum) == 0 ? Hmd_Eye_Eye_Left : Hmd_Eye_Eye_Right;

      HmdMatrix44_t projection = VR_IVRSystem_GetProjectionMatrix(system,
        eye, $(float zNear), $(float zFar), GraphicsAPIConvention_API_OpenGL);

      float* out = $(float* ptr);
      out[0]  = projection.m[0][0];
      out[1]  = projection.m[1][0];
      out[2]  = projection.m[2][0];
      out[3]  = projection.m[3][0];
      out[4]  = projection.m[0][1];
      out[5]  = projection.m[1][1];
      out[6]  = projection.m[2][1];
      out[7]  = projection.m[3][1];
      out[8]  = projection.m[0][2];
      out[9]  = projection.m[1][2];
      out[10] = projection.m[2][2];
      out[11] = projection.m[3][2];
      out[12] = projection.m[0][3];
      out[13] = projection.m[1][3];
      out[14] = projection.m[2][3];
      out[15] = projection.m[3][3];
    }|]