
#ifndef __CONFIG_H__
#define __CONFIG_H__

#ifndef CONFIG_VERSION_MAJOR
#define CONFIG_VERSION_MAJOR 1
#endif

#ifndef CONFIG_VERSION_MINOR
#define CONFIG_VERSION_MINOR 0
#endif

#ifndef CONFIG_VERSION_MICRO
#define CONFIG_VERSION_MICRO 0
#endif

#ifndef CONFIG_CREATE_DATE
#define CONFIG_CREATE_DATE "Date: 2017-02-23 Time: 19:29 GMT+2"
#endif

// Set this value to last successful build date/time.
#ifndef CONFIG_BUILD_DATE
#define CONFIG_BUILD_DATE " "
#endif

#ifndef CONFIG_AUTHOR
#define CONFIG_AUTHOR "Programmer: Bernard Gingold , contact: beniekg@gmail.com"
#endif

/* Start of Compiler specific declarations.*/

/* Compiler supported CPP version
as reported by reading __cplusplus macro def.*/

#if defined (__cplusplus)
#define ForAVXLib_CXX_98 199711L
#define ForAVXLib_CXX_11 201103L
#define ForAVXLib_CXX_14 201402L
#endif

/* Detrmine version supported by ICC.*/
#if defined (__cplusplus) && !defined (__INTEL_CXX11_MODE__)
#if ForAVXLib_CXX_98 < ForAVXLib_CXX_11
#define ForAVXLib_DEFAULT_CXX_VERSION 199711L
#else
#define ForAVXLib_DEFAULT_CXX_VERSION 201103L
#endif
#endif

/* Is Intel Compiler choosen as a default compiler? */
#if defined __INTEL_COMPILER 
#define ForAVXLib_COMPILED_BY_ICC
#else
#define ForAVXLib_COMPILED_BY_MSVC
#endif

/* Is 64-bit long mode a current one? */
#if  (defined (_M_AMD64) || defined (_M_X64_) || defined (__amd64) ) \
	&& !defined (__x86_64__)
#define __x86_64__ 1
#else 
#define __x86_64__ 0
#endif

/* Determine architectural support for full set of GP registers */
#if __x86_64__ == 1
#define ForAVXLib_HAS_FULL_GPR_SET 16
#elif __x86_64__ == 0
#define ForAVXLib_HAS_FULL_GPR_SET 8
#else
#error "ERROR: Cannot Determine 32-bit or 64-bit mode!!"
#endif

/* Determine architectural support for full set of 32 SIMD registers. */
#if  __x86_64__ == 1
#define ForAVXLib_HAS_FULL_SIMD_REG_SET 32
#elif __x86_64__ == 0
#define ForAVXLib_HAS_FULL_SIMD_REG_SET 16
#else
#error "ERROR: Cannot Determine 32-bit or 64-bit mode!!"
#endif

/* OPENMP support*/
#if !defined (USE_OPENMP)
#define USE_OPENMP 1
#if defined (ForAVXLib_COMPILED_BY_ICC)
#include <omp.h>
#elif defined (ForAVXLib_COMPILED_BY_MSVC)
#include <omp.h>
#else
#error "Unsupported Compiler or compiler version!!"
#endif
#endif

#if defined (USE_OPENMP)
#define OMP_VER_10 1000
#define OMP_VER_15 1500
#define OMP_VER_20 2000
#define OMP_VER_25 2500
#define OMP_VER_30 3000
#define OMP_VER_35 3500

#if __INTEL_COMPILER >= 1500
#define OMP_VER_40 4000
#endif
#endif




/* DEBUG mode */
#if defined (_DEBUG) || !defined (NDEBUG)
#define ForAVXLib_DEBUG_ON 1
#include <crtdbg.h>
#undef _DEBUG
#else
#define ForAVXLib_DEBUG_ON 0
#endif

/* Compiler manual-vectorisation.
   Default set is false '0' 
   Set to '1' if auto-vectorizer failed to vectorise code.*/
#if defined (ForAVXLib_COMPILED_BY_ICC) 
#define AUTO_VECTORISATION_FAILED  0
#endif

#if AUTO_VECTORISATION_FAILED == 1
#define USE_MANUAL_VECTORISATION 1
#else
#define USE_MANUAL_VECTORISATION 0
#endif

/* Compiler software prefetching settings. */
#if defined (ForAVXLib_COMPILED_BY_ICC)
#define SOFT_PREFETCH_L1 1
#define SOFT_PREFETCH_L2 2
#define SOFT_PREFETCH_L3 3
#define SOFT_PREFETCH_NTA 4
/* Prefetching in term of loop cycles */
#define SOFT_PREFETCH_IN_CYCLES_L1 1
#define SOFT_PREFETCH_IN_CYCLES_L2 2
#define SOFT_PREFETCH_IN_CYCLES_L3 3
#endif

#if defined (SOFT_PREFETCH_L1)
#define L1_SHORT_DIST_ITERS 4
#define L1_LONG_DIST_ITERS  8
#endif

/* Manual unrolling settings. */
#if defined (ForAVXLib_COMPILED_BY_ICC) 
 
#define USE_MANUAL_UNROLLING 1
#else
#define USE_MANUAL_UNROLLING 0
#endif

#if defined (USE_MANUAL_UNROLLING) && \
  USE_MANUAL_UNROLLING == 1
#define UNROLL_2X 2
#define UNROLL_4X 4
#define UNROLL_8X 8
#if  __x86_64__ == 1
#define UNROLL_16 16
#define UNROLL_32 32
#endif
#endif

#if defined (SOFT_PREFETCH_L1) && \
	SOFT_PREFETCH_L1 == 1
//constexpr int L1_MAX_SP{ 8000 };
//constexpr int L1_MAX_DP{ 4000 };
#define L1_MAX_SP 8000
#define L1_MAX_DP 4000
#endif

#if  defined (SOFT_PREFETCH_L2) && \
	SOFT_PREFETCH_L2 == 2
constexpr int L2_MAX_SP{ 8 * L1_MAX_SP };
constexpr int L2_MAX_DP{ 8 * L1_MAX_DP };
#endif

#if defined (SOFT_PREFETCH_L3) && \
	SOFT_PREFETCH_L3 == 3
constexpr  int L3_MAX_SP{ 1572864 };
constexpr  int L3_MAX_DP{ 786432 };
#endif

#if  defined (SOFT_PREFETCH_IN_CYCLES_L1) && \
	SOFT_PREFETCH_IN_CYCLES_L1 == 1
constexpr int DIST_NCYCLES_L1_SMALL{ 4 };
constexpr int DIST_NCYCLES_L1_LARGE{ 8 };
#endif

#if defined (SOFT_PREFETCH_IN_CYCLES_L2) && \
	SOFT_PREFETCH_IN_CYCLES_L1 == 2
constexpr int DIST_NCYCLES_L2_SMALL{16};
constexpr int DIST_NCYCLES_L2_LARGE{32};
#endif

#if defined (SOFT_PREFETCH_IN_CYCLES_L3) && \
	SOFT_PREFETCH_IN_CYCLES_L3 == 3
constexpr int DIST_NCYCLES_L3_SMALL{ 64 };
constexpr int DIST_NCYCLES_L3_LARGE{ 128 };
#endif

#if !defined COLLECT_PERF_MEASURE
#define COLLECT_PERF_MEASURE 1
#endif

#if defined (COLLECT_PERF_MEASURE) && \
	COLLECT_PERF_MEASURE == 1
#if !defined (HIGH_PRECISION_PERF_MEASURE)
// Use RDTSCP intrinsic function by default.
#define HIGH_PRECISION_PERF_MEASURE 1
#endif
#if !defined (LOW_PRECISION_PERF_MEASURE)
#define LOW_PRECISION_PERF_MEASURE 0

#endif
#endif

#if LOW_PRECISION_PERF_MEASURE == 1
#include <intrin.h>
#endif

// Set this macro to 1 if you want to
// enable narrowing conversion from double
// precision to single precision.
#if !defined (USE_NARROWING_CONVERSION)
#define USE_NARROWING_CONVERSION 0
#endif

// FP Exception codes.
constexpr int dom_err{ -1 };
constexpr int rng_err{ -2 };
constexpr int pol_err{ -3 };

#endif /*__CONFIG_H__*/