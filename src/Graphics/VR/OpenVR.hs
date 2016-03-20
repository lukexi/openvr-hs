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

C.include "openvr_capi_helper.h"


newtype IVRSystem     = IVRSystem     { unIVRSystem     :: Ptr () } deriving Show
newtype IVRCompositor = IVRCompositor { unIVRCompositor :: Ptr () } deriving Show

data TrackedControllerRole = TrackedControllerRoleInvalid
                           | TrackedControllerRoleLeftHand
                           | TrackedControllerRoleRightHand
                           deriving (Eq, Show, Enum, Ord)
trackedControllerRoleToC :: TrackedControllerRole -> CInt
trackedControllerRoleToC TrackedControllerRoleInvalid     = [C.pure|int{TrackedControllerRole_Invalid}|]
trackedControllerRoleToC TrackedControllerRoleLeftHand    = [C.pure|int{TrackedControllerRole_LeftHand}|]
trackedControllerRoleToC TrackedControllerRoleRightHand   = [C.pure|int{TrackedControllerRole_RightHand}|]

data HmdEye = LeftEye | RightEye deriving (Enum, Eq, Show)

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

-- | Gets a reference to the OpenVR Compositor, which is used to submit frames to the headset.
getCompositor :: MonadIO m => m (Maybe IVRCompositor) 
getCompositor = liftIO $ do
    compositorPtr <- [C.block| void * {
        EVRInitError error = VRInitError_None;

        //IVRCompositor *compositor = VR_GetGenericInterface(IVRCompositor_Version, &error);
        IVRCompositor *compositor = VRCompositor();

        if (error != VRInitError_None) {
            compositor = 0;

            printf("Compositor initialization failed with error: %s\n", VR_GetVRInitErrorAsEnglishDescription(error));
            return 0;
        }

        return compositor;

        }|]

    return $ if compositorPtr == nullPtr then Nothing else Just (IVRCompositor compositorPtr)


-- | Returns the size of the framebuffer you should render to for one eye.
-- Double the width if using a single framebuffer for both eyes.
getRenderTargetSize :: Integral a => MonadIO m => IVRSystem -> m (a, a)
getRenderTargetSize (IVRSystem systemPtr) = liftIO $ do
    (w, h) <- C.withPtrs_ $ \(wPtr, hPtr) -> 
        [C.block| void {
            IVRSystem *system = (IVRSystem *)$(void* systemPtr);
            system->GetRecommendedRenderTargetSize($(uint32_t* wPtr), $(uint32_t* hPtr));
        }|]
    return (fromIntegral w, fromIntegral h)


-- | Returns the projection matrix for the given eye for the given near and far clipping planes.
getEyeProjectionMatrix :: (MonadIO m) => IVRSystem -> HmdEye -> Float -> Float -> m (M44 GLfloat)
getEyeProjectionMatrix (IVRSystem systemPtr) eye (realToFrac -> zNear) (realToFrac -> zFar) = liftIO $ do
    let eyeNum = fromIntegral $ fromEnum eye
    buildM44WithPtr $ \ptr ->
        [C.block|void {
            IVRSystem *system = (IVRSystem *)$(void* systemPtr);
  
            EVREye eye = $(int eyeNum) == 0 ? Eye_Left : Eye_Right;    
        
            HmdMatrix44_t projection;
            // The C++ API crashes when calling GetProjectionMatrix, so we work around by calling the
            // C API (see cbits/Why.txt)
            //HmdMatrix44_t projection = VRSystem()->GetProjectionMatrix(
            //    eye, $(float zNear), $(float zFar), API_OpenGL);
            //fillFromMatrix44(projection, $(float* ptr));

            copyProjectionMatrixForEye((int)eye, $(float zNear), $(float zFar), $(float* ptr));
        }|]


-- | Returns the offset of each eye from the head pose.
getEyeToHeadTransform :: (MonadIO m) => IVRSystem -> HmdEye -> m (M44 GLfloat)
getEyeToHeadTransform (IVRSystem systemPtr) eye = liftIO $ do
    let eyeNum = fromIntegral $ fromEnum eye
    buildM44WithPtr $ \ptr ->
        [C.block|void {
            IVRSystem *system = (IVRSystem *)$(void* systemPtr);    
  
            EVREye eye = $(int eyeNum) == 0 ? Eye_Left : Eye_Right;    
            
            // The C++ API crashes when calling GetEyeToHeadTransform, so we work around by calling the
            // C API (see cbits/Why.txt)
            //HmdMatrix34_t transform = system->GetEyeToHeadTransform(eye);
            //fillFromMatrix34(transform, $(float* ptr));
            copyEyeToHeadTransformForEye((int)eye, $(float* ptr));
        }|]

isUsingLighthouse :: MonadIO m => IVRSystem -> m Bool
isUsingLighthouse (IVRSystem systemPtr) = liftIO $ do
    foundLighthouse <- [C.block|int {
        IVRSystem *system = (IVRSystem *)$(void* systemPtr);
        bool foundLighthouse = 0;
        for (int nDevice = 0; nDevice < k_unMaxTrackedDeviceCount; nDevice++) {
            char trackingSystemName[k_unTrackingStringSize];
            ETrackedPropertyError error;
            system->GetStringTrackedDeviceProperty(
                nDevice, 
                Prop_TrackingSystemName_String, 
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
        IVRCompositor* compositor = (IVRCompositor *)$(void* compositorPtr);
        compositor->ShowMirrorWindow();
    }|]

hideMirrorWindow :: MonadIO m => IVRCompositor -> m ()
hideMirrorWindow (IVRCompositor compositorPtr) = liftIO $ do
    [C.block|void{
        IVRCompositor* compositor = (IVRCompositor *)$(void* compositorPtr);
        compositor->HideMirrorWindow();
    }|]

resetSeatedZeroPose :: MonadIO m => IVRSystem -> m ()
resetSeatedZeroPose (IVRSystem systemPtr) = liftIO $ do
    [C.block|void{
        IVRSystem *system = (IVRSystem *)$(void* systemPtr);
        system->ResetSeatedZeroPose();
    }|]

showKeyboard :: MonadIO m => m ()
showKeyboard = liftIO $ do
  [C.block|void{
    const char * pchDescription = "";
    const char * pchExistingText = "";
    uint32_t unCharMax = 256;
    bool bUseMinimalMode = 1;
    uint64_t uUserValue = 0;
    EVROverlayError err = VROverlayError_None;
    err = VROverlay()->ShowKeyboard( 
        k_EGamepadTextInputModeNormal, 
        k_EGamepadTextInputLineModeSingleLine, 
        pchDescription, 
        unCharMax, 
        pchExistingText, 
        bUseMinimalMode, 
        uUserValue);
    if (err != VROverlayError_None) {
        printf("Overlay error: %s\n", VROverlay()->GetOverlayErrorNameFromEnum(err));
    }
  }|]

hideKeyboard :: MonadIO m => m ()
hideKeyboard = liftIO $ do
  [C.block|void{
    VROverlay()->HideKeyboard();
  }|]

triggerHapticPulse :: MonadIO m => IVRSystem -> TrackedControllerRole -> CInt -> CUShort -> m ()
triggerHapticPulse (IVRSystem systemPtr) controllerRole axis duration = liftIO $ do
    let cControllerRole = trackedControllerRoleToC controllerRole
    [C.block|void {
        IVRSystem *system = (IVRSystem *)$(void *systemPtr);
        ETrackedControllerRole controllerRole = (ETrackedControllerRole)$(int cControllerRole);
        int nDevice = system->GetTrackedDeviceIndexForControllerRole(controllerRole);
        int32_t unAxisId = $(int axis);
        unsigned short usDurationMicroSec = $(unsigned short duration);
        system->TriggerHapticPulse(nDevice, unAxisId, usDurationMicroSec);
    }|]

data OpenVREvent = OpenVREventKeyboardCharInput String


-- | Currently just prints out the event
pollNextEvent :: MonadIO m => IVRSystem -> m [OpenVREvent]
pollNextEvent (IVRSystem systemPtr) = liftIO $ do

    charInputIORef <- newIORef ""
    let captureCChars charsPtr = do
          chars <- peekCString charsPtr
          modifyIORef' charInputIORef (++ chars)
  
    [C.block|void {
        IVRSystem *system = (IVRSystem *)$(void *systemPtr);
    
        VREvent_t event;
    
        while (system->PollNextEvent(&event, sizeof(event))) {
            const char *eventName = system->GetEventTypeNameFromEnum((EVREventType)event.eventType);
            // printf("Got event type: %s\n", eventName);
      
            if (event.eventType == VREvent_KeyboardCharInput) {
                printf("Got keyboard character event: %s\n", event.data.keyboard.cNewInput);
                printf("Got keyboard character event: %d\n", event.data.keyboard.cNewInput[0]);
                printf("User value: %Lu\n", event.data.keyboard.uUserValue);
        
                $fun:(void (*captureCChars)(char*))(event.data.keyboard.cNewInput);
            }
        }
    }|]
    chars <- readIORef charInputIORef
    when (not (null chars)) $ print chars
    
    return [OpenVREventKeyboardCharInput chars]
  
-- | The controller role here corresponds to the ETrackedControllerRole
getControllerState :: MonadIO m => IVRSystem -> TrackedControllerRole -> m (CFloat, CFloat, CFloat, Bool, Bool)
getControllerState (IVRSystem systemPtr) controllerRole = liftIO $ do
    let cControllerRole = trackedControllerRoleToC controllerRole
    (x, y, trigger, grip, start) <- C.withPtrs_ $ \(xPtr, yPtr, triggerPtr, gripPtr, startPtr) -> 
        [C.block|void {
            IVRSystem *system = (IVRSystem *)$(void *systemPtr);

            ETrackedControllerRole controllerRole = (ETrackedControllerRole)$(int cControllerRole);
            int nDevice = system->GetTrackedDeviceIndexForControllerRole(controllerRole);

            VRControllerState_t state;
            system->GetControllerState(nDevice, &state);
            
            // for (int nAxis; nAxis < k_unControllerStateAxisCount; nAxis++) {
            //   printf("%i Axis %i: %f \t%f\n", 
            //     nDevice,
            //     nAxis, 
            //     state.rAxis[nAxis].x, 
            //     state.rAxis[nAxis].y);
            // }
            // printf("%i Touched: %i\n", nDevice, state.ulButtonTouched);
            // printf("%i Pressed: %i\n", nDevice, state.ulButtonPressed);
            
            *$(float* xPtr) = state.rAxis[0].x;
            *$(float* yPtr) = state.rAxis[0].y;

            *$(float* triggerPtr) = state.rAxis[1].x;

            int gripMask = ButtonMaskFromId(k_EButton_Grip);
            int menuMask = ButtonMaskFromId(k_EButton_ApplicationMenu);

            *$(int* gripPtr)    = (state.ulButtonPressed & gripMask)
                                  == gripMask;
            *$(int* startPtr)   = (state.ulButtonPressed & menuMask)
                                  == menuMask;
      }|]
    return (x, y, trigger, grip /= 0, start /= 0)

-- | Get the roles and matrices for the current frame.
-- (Nb. this function could use a few improvements : ) — we're using globals 
-- g_trackedDevicePoses and g_trackedDevicePosesCount just for storing
-- the poses across FFI calls to then pack them into M44s. 
-- Better would be to preallocate some memory in initOpenVR, 
-- write to it with one FFI call here, return the count, then pull the data into Haskell land.
waitGetPoses :: (MonadIO m) => IVRCompositor -> IVRSystem -> m (M44 GLfloat, [(TrackedControllerRole, M44 GLfloat)])
waitGetPoses (IVRCompositor compositorPtr) (IVRSystem systemPtr) = liftIO $ do
  
    -- First count how many valid HMD and controller poses exist so we can allocate an array
    numPoses <- fromIntegral <$> [C.block|int {
        IVRCompositor *compositor = (IVRCompositor *)$(void *compositorPtr);
        IVRSystem *system = (IVRSystem *)$(void *systemPtr);
        int numPoses = 0;
        compositor->WaitGetPoses( 
            g_trackedDevicePoses, g_trackedDevicePosesCount, NULL, 0);
        for (int nDevice = 0; nDevice < g_trackedDevicePosesCount; nDevice++) {
            TrackedDevicePose_t pose = g_trackedDevicePoses[nDevice];
            if (pose.bPoseIsValid) {
                ETrackedDeviceClass deviceClass = system->GetTrackedDeviceClass(nDevice);
                if (deviceClass == TrackedDeviceClass_HMD ||
                    deviceClass == TrackedDeviceClass_Controller) {
                    numPoses++;
                }
            }
        }
        return numPoses;
        }|]
  
    -- Then fill our coffers with them
    -- Also nab the roles so we can match up a controller poses with their states
    (matrices, roles) <- allocaArray numPoses $ \rolesPtr -> do
        matrices <- buildM44sWithPtr numPoses $ \matricesPtr -> 
            [C.block|void {
                IVRSystem *system = (IVRSystem *)$(void *systemPtr);
                int* roles = $(int* rolesPtr);
                float* matrices = $(float* matricesPtr);
                int offset = 0;
                for (int nDevice = 0; nDevice < g_trackedDevicePosesCount; nDevice++) {
                    TrackedDevicePose_t pose = g_trackedDevicePoses[nDevice];
                    if (pose.bPoseIsValid) {
                        ETrackedDeviceClass deviceClass = system->GetTrackedDeviceClass(nDevice);
                        if (deviceClass == TrackedDeviceClass_HMD ||
                            deviceClass == TrackedDeviceClass_Controller) {
                            HmdMatrix34_t transform = pose.mDeviceToAbsoluteTracking;
                            fillFromMatrix34(transform, matrices + 16 * offset);
                            roles[offset] = system->GetControllerRoleForTrackedDeviceIndex(nDevice);
                            offset++;
                        }
                    }
                }
            }|]
        roles <- peekArray numPoses rolesPtr
        return (matrices, roles)
  
    -- Assumes head will be the first matrix, followed by any controllers.
    return $ case zip roles matrices of
        []   -> (identity, [])
        x:xs -> (snd x, map (first (toEnum . fromIntegral)) xs)


-- | Submits a frame for the given eye
submitFrameForEye :: (Integral a, MonadIO m) => IVRCompositor -> HmdEye -> a -> m ()
submitFrameForEye (IVRCompositor compositorPtr) eye (fromIntegral -> framebufferTextureID) = liftIO $ do
    let eyeNum = fromIntegral $ fromEnum eye
    [C.block|void {
        IVRCompositor *compositor = (IVRCompositor *)$(void *compositorPtr);
        EVREye eye = $(int eyeNum) == 0 ? Eye_Left : Eye_Right;
    
        Texture_t texture = { 
            (void*)$(unsigned long long framebufferTextureID), 
            API_OpenGL,
            ColorSpace_Linear
        };
    
        compositor->Submit(eye, 
            &texture, NULL, Submit_Default);
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
    mSystem <- initOpenVR
  
    case mSystem of
        Nothing -> putStrLn "Couldn't create OpenVR system :*(" >> return Nothing
        Just system -> do
            (w,h) <- getRenderTargetSize system
            eyes <- forM [LeftEye, RightEye] $ \eye -> do
                eyeProj  <- getEyeProjectionMatrix system eye 0.1 10000
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
