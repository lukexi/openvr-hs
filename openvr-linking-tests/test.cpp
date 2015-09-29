#include <stdio.h>
#include <openvr.h>

int main(int argc, char *argv[]) {
  vr::HmdError eError = vr::HmdError_None;
  vr::IVRSystem *system = vr::VR_Init(&eError);
  if (system == NULL) {
    char buf[1024];
    sprintf_s(buf, sizeof(buf), "Unable to init VR runtime: %s", vr::VR_GetStringForHmdError(eError));
    printf("initOpenVR error: %s\n", buf);
  }
  
  system->GetProjectionMatrix(
      vr::Eye_Left, 0.1, 10000, vr::API_OpenGL);
  return 0;
}