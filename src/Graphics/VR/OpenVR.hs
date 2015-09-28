{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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