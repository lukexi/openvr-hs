#include <stdio.h>
#include <openvr_capi.h>

int main(int argc, char *argv[]) {
  HmdError error = HmdError_None;
  intptr_t sys = VR_Init(&error, EVRApplicationType_VRApplication_Scene);

  if (sys == 0) {
    char buf[1024];
    sprintf_s(buf, sizeof(buf), 
      "Unable to init VR runtime: %s", 
      VR_GetStringForHmdError(error));
    printf("initOpenVR error: %s\n", buf);
  }
  
  HmdMatrix44_t matrix = VR_IVRSystem_GetProjectionMatrix(sys,
      Hmd_Eye_Eye_Left, 0.1, 10000, GraphicsAPIConvention_API_OpenGL);
  printf("%f\n", matrix.m[0][0]); 
  return 0;
}