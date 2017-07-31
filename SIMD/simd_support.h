
#ifndef __SIMD_SUPPORT_H__
#define __SIMD_SUPPORT_H__

/* File version granularity. */
#if !defined (SIMD_SUPPORT_VERSION_MAJOR)
#define SIMD_SUPPORT_VERSION_MAJOR 1
#endif

#if !defined (SIMD_SUPPORT_VERSION_MINOR)
#define SIMD_SUPPORT_VERSION_MINOR 0
#endif

#if !defined (SIMD_SUPPORT_VERSION_MICRO)
#define SIMD_SUPPORT_VERSION_MICRO 0
#endif

#if !defined (SIMD_SUPPORT_CREATE_DATE)
#define SIMD_SUPPORT_CREATE_DATE "Date: 2017-02-27, Time: 19:41 PM GMT+2"
#endif

// Set this value to last successful build date.
#if !defined (SIMD_SUPPORT_BUILD_DATE)
#define SIMD_SUPPORT_BUILD_DATE " "
#endif

#if !defined (SIMD_SUPPORT_AUTHOR)
#define SIMD_SUPPORT_AUTHOR "Name: Bernard Gingold , contact: beniekg@gmail.com "
#endif

/* Determine SIMD architecture ISA Set and
   its corresponding intrinsic set. */

#if  !defined SIMD_MARCH
#if defined (__AVX512__) || defined (__AVX512CD__) || defined (__AVX512DQ__) \
	|| defined (__AVX512ER__) || defined (__AVX512F__) || defined (__AVXPF__)
#define SIMD_MARCH 9
#elif defined (__AVX2__)
#define SIMD_MARCH 8
#elif defined (__AVX__)
#define SIMD_MARCH 7
#elif defined (__SSE4_2__)
#define SIMD_MARCH 6
#elif defined (__SSE4__)
#define SIMD_MARCH 5
#elif defined (__SSSE3__)
#define SIMD_MARCH 4
#elif defined (__SSE3__)
#define SIMD_MARCH 3
#elif defined (__SSE2__)
#define SIMD_MARCH 2
#elif defined (__SSE__)
#define SIMD_MARCH 1
#elif defined (_M_IX86_FP)
#define SIMD_MARCH _M_IX86_FP
#else
#define SIMD_MARCH 0
#endif
#endif

#if defined __INTEL_COMPILER
#if SIMD_MARCH > 8
#include <zmmintrin.h>
#elif SIMD_MARCH == 8
#include <immintrin.h>
#elif SIMD_MARCH == 7
#include <immintrin.h>
#elif SIMD_MARCH == 6
#include <nmmintrin.h>
#elif SIMD_MARCH == 5
#include <smmintrin.h>
#elif SIMD_MARCH == 4
#include <tmmintrin.h>
#elif SIMD_MARCH == 3
#include <emmintrin.h>
#elif SIMD_MARCH == 2
#include <xmmintrin.h>
#elif defined _MSC_VER
#include <intrin.h>
#else
#error "ERROR: Unsupported Compiler Detected!!"
#endif
#endif







#endif /*__SIMD_SUPPORT_H__*/