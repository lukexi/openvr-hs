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

-- Set up inline-c to gain Cpp and Function Pointer abilities
C.context (C.cppCtx <> C.funCtx)
-- Import OpenVR
C.include "openvr.h"
C.include "stdio.h"

newtype IVRSystem = IVRSystem { unIVRSystem :: Ptr () } deriving Show

newtype IVRCompositor = IVRCompositor { unIVRCompositor :: Ptr () } deriving Show

-- | Temporarily allocate an array of the given size, 
-- pass it to a foreign function, then peek it before it is discarded

withArray_ :: (Storable a) => Int -> (Ptr a -> IO ()) -> IO [a]
withArray_ size action = allocaArray size $ \ptr -> do
  _ <- action ptr
  peekArray size ptr

initOpenVR :: MonadIO m => m (Maybe IVRSystem)
initOpenVR = liftIO $ do
  systemPtr <- [C.block| void* {
    vr::HmdError eError = vr::HmdError_None;
    vr::IVRSystem *system = vr::VR_Init(&eError);
    if (system == NULL) {
      char buf[1024];
      sprintf_s(buf, sizeof(buf), "Unable to init VR runtime: %s", vr::VR_GetStringForHmdError(eError));
      printf("initOpenVR error: %s\n", buf);
    }

    system->GetProjectionMatrix(
        vr::Eye_Left, 0.1, 10000, vr::API_OpenGL);

    return system;
    } |]

  return $ if systemPtr == nullPtr then Nothing else Just (IVRSystem systemPtr)

getRenderTargetSize :: MonadIO m => IVRSystem -> m (Word32, Word32)
getRenderTargetSize (IVRSystem systemPtr) = liftIO $
  C.withPtrs_ $ \(xPtr, yPtr) -> 
    [C.block| void {
      vr::IVRSystem *system = (vr::IVRSystem *)$(void* systemPtr);
      system->GetRecommendedRenderTargetSize($(uint32_t* xPtr), $(uint32_t* yPtr));
    }|]

getCompositor :: MonadIO m => m (Maybe IVRCompositor) 
getCompositor = liftIO $ do
  compositorPtr <- [C.block| void* {
    vr::HmdError error = vr::HmdError_None;

    vr::IVRCompositor* compositor = (vr::IVRCompositor*)vr::VR_GetGenericInterface(vr::IVRCompositor_Version, &error);

    if ( error != vr::HmdError_None )
    {
      compositor = NULL;

      printf( "Compositor initialization failed with error: %s\n", vr::VR_GetStringForHmdError( error ) );
      return NULL;
    }

    // uint32_t unSize = compositor->GetLastError(NULL, 0);
    // if (unSize > 1)
    // {
    //   char* buffer = new char[unSize];
    //   compositor->GetLastError(buffer, unSize);
    //   printf( "Compositor - %s\n", buffer );
    //   delete [] buffer;
    //   return NULL;
    // }

    return compositor;

    }|]

  return $ if compositorPtr == nullPtr then Nothing else Just (IVRCompositor compositorPtr)

submitFrame (IVRCompositor compositorPtr) (fromIntegral -> framebufferTextureID) (fromIntegral -> width) (fromIntegral -> height) = do

  [C.block|void {
    vr::IVRCompositor* compositor = (vr::IVRCompositor *)$(void* compositorPtr);
    compositor->Submit(vr::Eye_Left,  vr::API_OpenGL, (void*)$(unsigned int framebufferTextureID), NULL );
    compositor->Submit(vr::Eye_Right, vr::API_OpenGL, (void*)$(unsigned int framebufferTextureID), NULL );
  }|]

waitGetPoses (IVRCompositor compositorPtr) = do

  [C.block|void {

    vr::TrackedDevicePose_t trackedDevicePoses[vr::k_unMaxTrackedDeviceCount];
    vr::IVRCompositor* compositor = (vr::IVRCompositor *)$(void* compositorPtr);
    compositor->WaitGetPoses(trackedDevicePoses, vr::k_unMaxTrackedDeviceCount, NULL, 0 );
  }|]

data HmdEye = LeftEye | RightEye deriving (Enum)

getEyeToHeadTransform :: MonadIO m => IVRSystem -> HmdEye -> m [CFloat]
getEyeToHeadTransform (IVRSystem systemPtr) eye = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  -- withArray_ 16 $ \ptr ->
  [C.block|void {
    vr::IVRSystem *system = (vr::IVRSystem *)$(void* systemPtr);

    vr::Hmd_Eye eye = $(int eyeNum) == 0 ? vr::Eye_Left : vr::Eye_Right;

    system->GetEyeToHeadTransform(vr::Eye_Left);

    // printf("%i %i\n", x, y);
    //vr::HmdMatrix34_t transform = system->GetEyeToHeadTransform(eye);

    // float* out = 
    // out[0]  = transform.m[0][0];
    // out[1]  = transform.m[1][0];
    // out[2]  = transform.m[2][0];
    // out[3]  = 0;
    // out[4]  = transform.m[0][1];
    // out[5]  = transform.m[1][1];
    // out[6]  = transform.m[2][1];
    // out[7]  = 0;
    // out[8]  = transform.m[0][2];
    // out[9]  = transform.m[1][2];
    // out[10] = transform.m[2][2];
    // out[11] = 0;
    // out[12] = transform.m[0][3];
    // out[13] = transform.m[1][3];
    // out[14] = transform.m[2][3];
    // out[15] = 1;

  }|]
  return []


getEyeProjectionMatrix :: MonadIO m => IVRSystem -> HmdEye -> Float -> Float -> m [CFloat]
getEyeProjectionMatrix (IVRSystem systemPtr) eye (realToFrac -> zNear) (realToFrac -> zFar) = liftIO $ do
  let eyeNum = fromIntegral $ fromEnum eye
  withArray_ 16 $ \ptr ->
    [C.block|void {
      vr::IVRSystem *system = (vr::IVRSystem *)$(void* systemPtr);

      vr::Hmd_Eye eye = $(int eyeNum) == 0 ? vr::Eye_Left : vr::Eye_Right;

      vr::HmdMatrix44_t projection = system->GetProjectionMatrix(
        eye, $(float zNear), $(float zFar), vr::API_OpenGL);

      //float* out = $(float* ptr);
      //out[0]  = projection.m[0][0];
      //out[1]  = projection.m[1][0];
      //out[2]  = projection.m[2][0];
      //out[3]  = projection.m[3][0];
      //out[4]  = projection.m[0][1];
      //out[5]  = projection.m[1][1];
      //out[6]  = projection.m[2][1];
      //out[7]  = projection.m[3][1];
      //out[8]  = projection.m[0][2];
      //out[9]  = projection.m[1][2];
      //out[10] = projection.m[2][2];
      //out[11] = projection.m[3][2];
      //out[12] = projection.m[0][3];
      //out[13] = projection.m[1][3];
      //out[14] = projection.m[2][3];
      //out[15] = projection.m[3][3];

      float* out = $(float* ptr);
      out[0]  = 0;
      out[1]  = 1;
      out[2]  = 2;
      out[3]  = 3;
      out[4]  = 4;
      out[5]  = 5;
      out[6]  = 6;
      out[7]  = 7;
      out[8]  = 8;
      out[9]  = 9;
      out[10] = 10;
      out[11] = 11;
      out[12] = 12;
      out[13] = 13;
      out[14] = 14;
      out[15] = 15;

    }|]