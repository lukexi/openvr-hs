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

    return system;
    } |]

  return $ if systemPtr == nullPtr then Nothing else Just (IVRSystem systemPtr)

getRenderTargetSize :: IVRSystem -> IO (Word32, Word32)
getRenderTargetSize (IVRSystem systemPtr) = 
  C.withPtrs_ $ \(xPtr, yPtr) -> do
    [C.block| void {
      vr::IVRSystem *system = (vr::IVRSystem *)$(void* systemPtr);
      system->GetRecommendedRenderTargetSize($(uint32_t* xPtr), $(uint32_t* yPtr));
    }|]

getCompositor = do
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