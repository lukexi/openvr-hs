#include <stdio.h>
#include <openvr.h>

int main(int argc, char *argv[]) {
    vr::EVRInitError eError = vr::VRInitError_None;
    vr::IVRSystem *vrSystem = vr::VR_Init( &eError, vr::VRApplication_Scene );

    if (vrSystem == NULL) {
        printf("Unable to init VR runtime: %s\n", vr::VR_GetVRInitErrorAsEnglishDescription(eError));
    } else {
        vrSystem->GetProjectionMatrix(
            vr::Eye_Left, 0.1, 10000, vr::API_OpenGL);
    }
    
    return 0;
}