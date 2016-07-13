#include "openvr_capi_missing.h"
#include "openvr_capi_helper.h"
#include <stdio.h>



struct VR_IVRSystem_FnTable *VRSystemFnTable() {
    char fnTableName[128];
    int result = sprintf(fnTableName, "FnTable:%s", IVRSystem_Version);

	EVRInitError initError;
    struct VR_IVRSystem_FnTable *fnTable =
    	(struct VR_IVRSystem_FnTable *)VR_GetGenericInterface(fnTableName, &initError);

    if (fnTable == 0) {
	    printf("Init error: %s\n", VR_GetVRInitErrorAsEnglishDescription(initError));
    }

    return fnTable;
}

void copyProjectionMatrixForEye(int eye, float znear, float zfar, float *out) {

	struct VR_IVRSystem_FnTable *vrSystem = VRSystemFnTable();

    HmdMatrix44_t matrix = vrSystem->GetProjectionMatrix(
    	eye == 0 ? EVREye_Eye_Left : EVREye_Eye_Right,
    	znear, zfar, EGraphicsAPIConvention_API_OpenGL);

    fillFromMatrix44(matrix, out);
}

void copyProjectionRawForEye(int eye, float *pfLeft, float *pfRight, float *pfTop, float *pfBottom) {

    struct VR_IVRSystem_FnTable *vrSystem = VRSystemFnTable();

    vrSystem->GetProjectionRaw(
        eye == 0 ? EVREye_Eye_Left : EVREye_Eye_Right,
        pfLeft, pfRight, pfTop, pfBottom);
}

void copyEyeToHeadTransformForEye(int eye, float *out) {
	struct VR_IVRSystem_FnTable *vrSystem = VRSystemFnTable();

	HmdMatrix34_t transform = vrSystem->GetEyeToHeadTransform(
		eye == 0 ? EVREye_Eye_Left : EVREye_Eye_Right);

	fillFromMatrix34(transform, out);
}


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
