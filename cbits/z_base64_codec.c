#include <HsFFI.h>
#include <stdint.h>

#include <chromiumbase64.h>
#if defined(__AVX512BW__)
#include <fastavx512bwbase64.h>
#elif defined(__AVX2__)
#include <fastavxbase64.h>
#endif

////////////////////////////////////////////////////////////////////////////////
// Base64 codec

void z_base64_encode(char* output, HsInt output_off, const char* input, HsInt off, HsInt len){
#if defined(__AVX512BW__)
    fast_avx512bw_base64_encode(output+output_off, input+off, len);
#elif defined(__AVX2__)
    fast_avx2_base64_encode(output+output_off, input+off, len);
#else
    chromium_base64_encode(output+output_off, input+off, len);
#endif
}

HsInt z_base64_decode(char* output, const char* input, HsInt off, HsInt len){
#if defined(__AVX512BW__)
    size_t r = fast_avx512bw_base64_decode(output, input+off, len);
#elif defined(__AVX2__)
    size_t r = fast_avx2_base64_decode(output, input+off, len);
#elif defined(__aarch64__)
    size_t r = neon_base64_decode(output, input+off, len);
#else
    size_t r = chromium_base64_decode(output, input+off, len);
#endif
    if (r == MODP_B64_ERROR) return 0;
    else return (HsInt)r;
}
