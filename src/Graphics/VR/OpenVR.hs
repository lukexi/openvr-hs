{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.VR.OpenVR where

import Foreign
import Foreign.Ptr
-- import Foreign.C
import qualified Language.C.Inline.Cpp as C
import Control.Monad.Trans
import Data.Monoid

-- Set up inline-c to 
C.context (C.cppCtx <> C.funCtx)
C.include "openvr.h"
C.include "stdio.h"

newtype IVRSystem = IVRSystem { unIVRSystem :: Ptr () } deriving Show

initOpenVR :: MonadIO m => m (Maybe IVRSystem)
initOpenVR = liftIO $ do
  systemPtr <- [C.block| void * {
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