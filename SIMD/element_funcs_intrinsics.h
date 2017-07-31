
#ifndef __ELEMENT_FUNCS_INTRINSICS_H__
#define __ELEMENT_FUNCS_INTRINSICS_H__

// Tab:  5 functions declarations
// Tab:  7 Comments: Functions parameter description
// Tab:  2 col distance between function declaration components.
// Line: 1 line between code and comments.

#if !defined (ELEMENT_FUNCS_INTRINSICS_VERSION_MAJOR)
#define ELEMENT_FUNCS_INTRINSICS_VERSION_MAJOR 1
#endif

#if !defined (ELEMENT_FUNCS_INTRINSICS_VERSION_MINOR)
#define ELEMENT_FUNCS_INTRINSICS_VERSION_MINOR 0
#endif

#if !defined (ELEMENT_FUNCS_INTRINSICS_VERSION_MICRO)
#define ELEMENT_FUNCS_INTRINSICS_VERSION_MICRO 0
#endif

// File version computed as follows: 1000 * version_major value + 100 * version_minor + 10 * version_micro.
#if !defined (ELEMENT_FUNCS_INTRINSICS_FILE_VERSION)
#define ELEMENT_FUNCS_INTRINSICS_FILE_VERSION 1000
#endif

#if !defined (ELEMENT_FUNCS_INTRINSICS_CREATE_DATE)
#define ELEMENT_FUNCS_INTRINSICS_CREATE_DATE "Date: 05-03-2017,Mar-03-2017 , Time: 10:56 AM GMT+2 -200"
#endif

// Set this value to last successful build date and time.
#if !defined (ELEMENT_FUNCS_INTRINSICS_BUILD_DATE)
#define ELEMENT_FUNCS_INTRINSICS_BUILD_DATE " "
#endif

#if !defined (ELEMENT_FUNCS_INTRINSICS_AUTHOR)
#define ELEMENT_FUNCS_INTRINSICS_AUTHOR "Programmer: Bernard Gingold , contact: beniekg@gmail.com"
#endif

#include "config.h"
#include "simd_support.h"

#if defined ForAVXLib_DEFAULT_CXX_VERSION && \
  ForAVXLib_DEFAULT_CXX_VERSION >= 199711L


extern "C" {

	/************************************************
	 'C' implementation of Intel Intrinsics.
	  Elementary functions declarations.
	  Callable from Fortran side.
	*************************************************/

	/*
	  @Description: Declaration of 'rcp_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  rcp_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'rsqrt_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  rsqrt_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'sqrt_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  sqrt_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'sqrt_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  sqrt_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'cbrt_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cbrt_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'cbrt_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cbrt_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'cexp_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cexp_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'cexp_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cexp_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'clog_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  clog_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'clog_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  clog_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'exp10_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  exp10_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'exp10_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  exp10_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'exp2_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  exp2_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'exp2_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  exp2_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'exp_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  exp_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'exp_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  exp_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);


	/*
	  @Description: Declaration of 'expm1_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  expm1_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'expm1_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  expm1_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'invcbrt_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  invcbrt_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'invcbrt_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  invcbrt_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'invsqrt_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  invsqrt_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'invsqrt_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  invsqrt_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'log10_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  log10_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'log10_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  log10_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'log1p_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  log1p_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'log1p_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  log1p_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'log2_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  log2_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'log2_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  log2_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'logb_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  logb_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'logb_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  logb_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'pow_avx256_ps' void function.

	  @Params:  source1 array of floats (32-bit, 24-bit precision)
	  @Params:  source2 array of floats (32-bit, 24-bit precision)
	  @Params:  length of source1 array
	  @Params:  length of source2 array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  pow_avx256_ps(const float* __restrict src1, const int src1_len, 
						const float* __restrict src2, const int src2_len,
	                    float* __restrict dst,        const int dst_len);

	/*
	  @Description: Declaration of 'pow_avx256_pd' void function.

	  @Params:  source1 array of doubles (64-bit, 56-bit precision)
	  @Params:  source2 array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source1 array
	  @Params:  length of source2 array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  pow_avx256_pd(const double* __restrict src1, const int src1_len, 
	                    const double* __restrict src2, const int src2_len,
	                    double*       __restrict dst,  const int dst_len );

	
}



#endif /* C++ compilation in use. */

#endif /*__ELEMENT_FUNCS_INTRINSICS_H__*/