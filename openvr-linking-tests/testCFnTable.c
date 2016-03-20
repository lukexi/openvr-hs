#include <stdio.h>
#include <openvr_capi.h>

uint32_t VR_InitInternal( EVRInitError *peError, EVRApplicationType eApplicationType );
void VR_ShutdownInternal();
/** Returns true if there is an HMD attached. This check is as lightweight as possible and
* can be called outside of VR_Init/VR_Shutdown. It should be used when an application wants
* to know if initializing VR is a possibility but isn't ready to take that step yet.
*/
bool VR_IsHmdPresent();

/** Returns true if the OpenVR runtime is installed. */
bool VR_IsRuntimeInstalled();

/** Returns where the OpenVR runtime is installed. */
const char *VR_RuntimePath();

/** Returns the name of the enum value for an EVRInitError. This function may be called outside of VR_Init()/VR_Shutdown(). */
const char *VR_GetVRInitErrorAsSymbol( EVRInitError error );

/** Returns an english string for an EVRInitError. Applications should call VR_GetVRInitErrorAsSymbol instead and
* use that as a key to look up their own localized error message. This function may be called outside of VR_Init()/VR_Shutdown(). */
const char *VR_GetVRInitErrorAsEnglishDescription( EVRInitError error );

/** Returns the interface of the specified version. This method must be called after VR_Init. The
* pointer returned is valid until VR_Shutdown is called.
*/
void *VR_GetGenericInterface( const char *pchInterfaceVersion, EVRInitError *peError );

/** Returns whether the interface of the specified version exists.
*/
bool VR_IsInterfaceVersionValid( const char *pchInterfaceVersion );

/** Returns a token that represents whether the VR interface handles need to be reloaded */
uint32_t VR_GetInitToken();

int main(int argc, char *argv[]) {



    EVRInitError error;
    VR_InitInternal(&error, EVRApplicationType_VRApplication_Scene);
    printf("Init error: %s\n", VR_GetVRInitErrorAsEnglishDescription(error));


    char fnTableName[128];
    int result = sprintf(fnTableName, "FnTable:%s", IVRSystem_Version);

    EVRInitError initError;
    struct VR_IVRSystem_FnTable *fnTable;
    fnTable = (struct VR_IVRSystem_FnTable*)VR_GetGenericInterface(fnTableName, &initError);

    printf("%s\n", fnTable->GetEventTypeNameFromEnum(EVREventType_VREvent_TrackedDeviceActivated));
    printf("Init error: %s\n", VR_GetVRInitErrorAsEnglishDescription(initError));

    HmdMatrix44_t matrix = fnTable->GetProjectionMatrix(
        EVREye_Eye_Left, 0.1, 10000, EGraphicsAPIConvention_API_OpenGL);

    // HmdMatrix44_t matrix = VR_IVRSystem_GetProjectionMatrix(sys,
    //     Hmd_Eye_Eye_Left, 0.1, 10000, GraphicsAPIConvention_API_OpenGL);
    printf("%f\n", matrix.m[0][0]);


    return 0;
}