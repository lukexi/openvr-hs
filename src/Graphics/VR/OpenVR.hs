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
import qualified Language.C.Inline.Cpp as C
import Control.Monad.Trans
import Data.Monoid
import Linear.Extra
import Text.RawString.QQ (r)
import Graphics.GL.Pal
import Control.Monad
import Control.Arrow
import Data.IORef

-- Set up inline-c to gain Cpp and Function Pointer abilities
C.context (C.cppCtx <> C.funCtx)

-- Import OpenVR
C.include "openvr.h"
C.include "stdio.h"
C.include "string.h"

C.using "namespace vr"

newtype IVRSystem     = IVRSystem     { unIVRSystem     :: Ptr () } deriving Show

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
|]

isHMDPresent :: MonadIO m => m Bool
isHMDPresent = toEnum . fromIntegral <$> liftIO [C.block| int {

  return VR_IsHmdPresent() ? 1 : 0;

  }|]

-- | Creates the OpenVR System object, which is the main point of interface with OpenVR.
-- Will return Nothing if no headset can be found, or if some other error occurs during initialization.
initOpenVR :: MonadIO m => m (Maybe IVRSystem)
initOpenVR = liftIO $ do
  systemPtr <- [C.block| void * {
    EVRInitError err = VRInitError_None;
    IVRSystem *system = VR_Init(&err, VRApplication_Scene);

    if (system == 0) {
      printf("initOpenVR error: %s\n", VR_GetVRInitErrorAsEnglishDescription(err));
    }

    return system;
    } |]

  return $ if systemPtr == nullPtr then Nothing else Just (IVRSystem systemPtr)
