
#ifndef __SPEC_FUNCS_INTRINSICS_H__
#define __SPEC_FUNCS_INTRINSICS_H__


// Tab:  5 functions declarations
// Tab:  7 Comments: Functions parameter description
// Tab:  2 col distance between function declaration components.
// Line: 1 line between code and comments.

#if !defined (SPEC_FUNCS_INTRINSICS_VER_MAJOR)
#define  SPEC_FUNCS_INTRINSICS_VER_MAJOR 1
#endif

#if !defined (SPEC_FUNCS_INTRINSICS_VER_MINOR)
#define SPEC_FUNCS_INTRINSICS_VER_MINOR 0
#endif

#if !defined (SPEC_FUNCS_INTRINSICS_VER_MICRO)
#define SPEC_FUNCS_INTRINSICS_VER_MICRO 0
#endif

// File version computed as follows: 1000 * version_major value + 100 * version_minor + 10 * version_micro.
#if !defined (SPEC_FUNCS_INTRINSICS_FILE_VERSION)
#define SPEC_FUNCS_INTRINSICS_FILE_VERSION 1000
#endif

#if !defined (SPEC_FUNCS_INTRINSICS_CREATE_DATE)
#define SPEC_FUNCS_INTRINSICS_CREATE_DATE "Date: 05-03-2017,Mar-05-2017 , time: 13:03 PM GMT+2 -200"
#endif

// Set this value to last successful build date and time.
#if !defined (SPEC_FUNCS_INTRINSICS_BUILD_DATE)
#define SPEC_FUNCS_INTRINSICS_BUILD_DATE " "
#endif

#if !defined (SPEC_FUNCS_INTRINSICS_AUTHOR)
#define SPEC_FUNCS_INTRINSICS_AUTHOR "Programmer: Bernard Gingold , contact: beniekg@gmail.com"
#endif

#include "config.h"
#include "simd_support.h"

#if defined ForAVXLib_DEFAULT_CXX_VERSION && \
  ForAVXLib_DEFAULT_CXX_VERSION >= 199711L

extern "C" {

	/************************************************
	 'C' implementation of Intel Intrinsics.
	  Special functions declarations.
	  Callable from Fortran side.
	*************************************************/

	/*
	  @Description: Declaration of 'cdfnorm_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cdfnorm_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'cdfnorm_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cdfnorm_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'cdfnorminv_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cdfnorminv_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'cdfnorminv_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cdfnorminv_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'erf_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  erf_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'erf_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  erf_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'erfc_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  erfc_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'erfc_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  erfc_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'erfcinv_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  erfcinv_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'erfcinv_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  erfcinv_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'erfinv_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  erfinv_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len);

	/*
	  @Description: Declaration of 'erfcinv_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  erfinv_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len);


}



#endif /* C++ compilation in use. */


#endif /*__SPEC_FUNCS_INTRINSICS_H__*/