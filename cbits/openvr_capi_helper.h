// Note that these still must only be called after VR_Init

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C
#endif

EXTERN_C void copyProjectionMatrixForEye(int eye, float znear, float zfar, float *out);
EXTERN_C void copyProjectionRawForEye(int eye, float *pfLeft, float *pfRight, float *pfTop, float *pfBottom);

EXTERN_C void copyEyeToHeadTransformForEye(int eye, float *out);

EXTERN_C void fillFromMatrix44(HmdMatrix44_t matrix, float* out);
EXTERN_C void fillFromMatrix34(HmdMatrix34_t matrix, float* out);
