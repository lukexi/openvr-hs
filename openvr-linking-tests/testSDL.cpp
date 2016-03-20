#include <stdio.h>
#include <openvr.h>
#include <SDL.h>

using namespace vr;

int main(int argc, char *argv[]) {
	
	printf("Initializing SDL\n");

	SDL_Init(0);
	
	printf("Initializing OpenVR\n");
    EVRInitError eError = VRInitError_None;
    IVRSystem *vrSystem = VR_Init( &eError, VRApplication_Scene );

    if (vrSystem == NULL) {
        printf("Unable to init VR runtime: %s\n", VR_GetVRInitErrorAsEnglishDescription(eError));
    } else {
        printf("Calling in...\n");

        VRCompositor()->FadeGrid(1, true);
        // printf("%f\n", foo);
        // HmdMatrix34_t trans = VRSystem()->GetEyeToHeadTransform(Eye_Left);
        printf("Done\n");
    }
    
    return 1;
}