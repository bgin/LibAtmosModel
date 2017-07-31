
#ifndef __TRIGONOMETRIC_INTRINSICS_H__
#define __TRIGONOMETRIC_INTRINSICS_H__

// Tab:  5 functions declarations
// Tab:  7 Comments: Functions parameter description
// Tab:  2 col distance between function declaration components.
// Line: 1 line between code and comments.


#if !defined (TRIGONOMETRIC_INTRINSICS_VERSION_MAJOR)
#define TRIGONOMETRIC_INTRINSICS_VERSION_MAJOR 1
#endif

#if !defined (TRIGONOMETRIC_INTRINSICS_VERSION_MINOR)
#define TRIGONOMETRIC_INTRINSICS_VERSION_MINOR 0
#endif

#if !defined (TRIGONOMETRIC_INTRINSICS_VERSION_MICRO)
#define TRIGONOMETRIC_INTRINSICS_VERSION_MICRO 0
#endif

// File version computed as follows: 1000 * version_major value + 100 * version_minor + 10 * version_micro.
#if !defined (TRIGONOMETRIC_INTRINSICS_FILE_VERSION)
#define TRIGONOMETRIC_INTRINSICS_FILE_VERSION 1000
#endif

#if !defined (TRIGONOMETRIC_INTRINSICS_CREATE_DATE)
#define TRIGONOMETRIC_INTRINSICS_CREATE_DATE "Date: 04-03-2017 04 Mar 2017 , Time: 12:10 PM GMT+2 -200"
#endif

// Set this value to last successful build date and time.
#if !defined (TRIGONOMETRIC_INTRINSICS_BUILD_DATE)
#define TRIGONOMETRIC_INTRINSICS_BUILD_DATE " "
#endif

#if !defined (TRIGONOMETRIC_INTRINSICS_AUTHOR)
#define TRIGONOMETRIC_INTRINSICS_AUTHOR "Programmer: Bernard Gingold , contact: beniekg@gmail.com"
#endif


#include "config.h"
#include "simd_support.h"

#if defined ForAVXLib_DEFAULT_CXX_VERSION && \
	ForAVXLib_DEFAULT_CXX_VERSION >= 199711L


extern "C" {

	/************************************************
	    'C' implementation of Intel Intrinsics.
		Trigonometric functions declarations.
	    Callable from Fortran side.
	*************************************************/

	/*
	  @Description: Declaration of 'acos_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  acos_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len, int* fperr);

	/*
	  @Description: Declaration of 'acos_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  acos_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len, int* fperr);

	/*
	  @Description: Declaration of 'acosh_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  acosh_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len, int* fperr);

	/*
	  @Description: Declaration of 'acosh_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  acosh_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len, int* fperr);

	/*
	  @Description: Declaration of 'asin_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  asin_avx256_ps(const float* __restrict src, const int src_len, float* __restrict dst, const int dst_len, int* fperr);

	/*
	  @Description: Declaration of 'asin_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  asin_avx256_pd(const double* __restrict src, const int src_len, double* __restrict dst, const int dst_len, int* fperr);

	/*
	  @Description: Declaration of 'asinh_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  asinh_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'asinh_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  asinh_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'atan2_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  atan2_avx256_ps(const float src1[], const int src1_len,
						  const float src2[], const int src2_len,
						  float dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'atan2_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  atan2_avx256_pd(const double src1[], const int src1_len,
						  const double src2[], const int src2_len,
						  double dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'atanh_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  atanh_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'atanh_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  atanh_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'cos_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cos_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'cos_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cos_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'cosd_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cosd_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'cosd_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cosd_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'cosh_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cosh_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'cosh_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  cosh_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'hypot_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  hypot_avx256_ps(const float src1[], const int src1_len,
						  const float src2[], const int src2_len,
						  float dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'atan2_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  hypot_avx256_pd(const double src1[], const int src1_len,
						  const double src2[], const int src2_len,
						  double dst[],        const int dst_len );


	/*
	  @Description: Declaration of 'sind_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  sind_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'sind_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  sind_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'sinh_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  sinh_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'sinh_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  sinh_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'tan_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  tan_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);


	/*
	  @Description: Declaration of 'tan_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  tan_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'tand_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  tand_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);


	/*
	  @Description: Declaration of 'tand_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  tand_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'tanh_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  tanh_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'tand_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  tanh_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);



}





#endif /* C++ compilation in use. */


#endif /*__TRIGONOMETRIC_INTRINSICS_H__*/