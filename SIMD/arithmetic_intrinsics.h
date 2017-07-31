
#ifndef __ARITHMETIC_INTRINSICS_H__
#define __ARITHMETIC_INTRINSICS_H__

// Tab:  5 functions declarations
// Tab:  7 Comments: Functions parameter description
// Tab:  2 col distance between function declaration components.
// Line: 1 line between code and comments.

#if !defined (ARITHMETIC_INTRINSICS_VERSION_MAJOR)
#define ARITHMETIC_INTRINSICS_VERSION_MAJOR 1
#endif

#if !defined (ARITHEMTIC_INTRINSICS_VERSION_MINOR)
#define ARITHEMTIC_INTRINSICS_VERSION_MINOR 0
#endif

#if !defined (ARITHMETIC_INTRINSICS_VERSION_MICRO)
#define ARITHMETIC_INTRINSICS_VERSION_MICRO 0
#endif

// File version computed as follows: 1000 * version_major value + 100 * version_minor + 10 * version_micro.
#if !defined (ARITHEMTIC_INTRINSICS_FILE_VERSION)
#define ARITHMEMTIC_INTRINSICS_FILE_VERSION 1000
#endif

#if !defined (ARITHMETIC_INTRINSICS_CREATE_DATE)
#define ARITHMETIC_INTRINSICS_CREATE_DATE "Date: 01-03-2017 , Time: 18:42 PM GMT+2 -200"
#endif

// Set this value to last successful build date and time.
#if !defined (ARITHMETIC_INTRINSICS_BUILD_DATE)
#define ARITHMETIC_INTRINSICS_BUILD_DATE " "
#endif

#if !defined (ARITHMEMTIC_INTRINSICS_AUTHOR)
#define ARITHMETIC_INTRINSICS_AUTHOR "Programmer: Bernard Gingold , contact: beniekg@gmail.com"
#endif

#include "config.h"
#include "simd_support.h"

#if defined ForAVXLib_DEFAULT_CXX_VERSION && \
  ForAVXLib_DEFAULT_CXX_VERSION >= 199711L

extern "C" {

	/************************************************
	  'C' implementation of Intel Intrinsics.
	   Arithmemtic version declarations.
	   Callable from Fortran side.
	*************************************************/

	/*
	  @Description: Declaration of 'add_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  add_avx256_ps(const float src1[], const int src1_len, 
		                const float src2[], const int src2_len,
						float dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'add_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  add_avx256_pd(const double src1[], const int src1_len,
		                const double src2[], const int src2_len,
						double dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'addsub_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  addsub_avx256_ps(const float src1[], const int src1_len,
		                   const float src2[], const int src2_len,
						   float dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'addsub_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  addsub_avx256_pd(const double src1[], const int src1_len,
						   const double src2[], const int src2_len,
						   double dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'div_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  div_avx256_ps(const float src1[], const int src1_len,
						const float src2[], const int src2_len,
						float dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'div_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  div_avx256_pd(const double src1[], const int src1_len,
		                const double src2[], const int src2_len,
						double dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'mul_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  mul_avx256_ps(const float src1[], const int src1_len,
					    const float src2[], const int src2_len,
						double dst[],       const int dst_len );

	/*
	  @Description: Declaration of 'mul_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  mul_avx256_pd(const double src1[], const int src1_len,
					    const double src2[], const int src2_len,
						double dest[],       const int dst_len );

	/*
	  @Description: Declaration of 'sub_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  sub_avx256_ps(const float src1[], const int src1_len,
					    const float src2[], const int src2_len,
						float dst[],		const int dst_len );

	/*
	  @Description: Declaration of 'sub_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  sub_avx256_pd(const double src1[], const int src1_len,
					    const double src2[], const int src2_len,
						double dst[],		 const int dst_len );

	/*
	  @Description: Declaration of 'hsub_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/
	
	void  hsub_avx256_ps(const float src1[], const int src1_len,
					     const float src2[], const int src2_len,
						 float dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'hsub_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  hsub_avx256_pd(const double src1[], const int src1_len,
						 const double src2[], const int src2_len,
						 double dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'dp_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  integer(32-bit) argument imm
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  dp_avx256_ps(const float src1[], const int src1_len,
					   const float src2[], const int src2_len,
					   float dst[],		   const int dst_len );

	/*
	  @Description: Declaration of 'hadd_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  hadd_avx256_ps(const float src1[], const int src1_len,
					     const float src2[], const int src2_len,
						 float dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'hadd_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  hadd_avx256_pd(const double src1[], const int src1_len,
						 const double src2[], const int src2_len,
						 double dst[],		  const int dst_len );

	/*
	  @Description: Declaration of 'add_avx256_epi32' void function.

	  @Params:  1st source array of integers (32-bit)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of integers (32-bit)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold integers (32-bit)

	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  add_avx256_epi32(const int src1[], const int src1_len,
						   const int src2[], const int src2_len,
						   int  dst[],		 const int dst_len );

	/*
	  @Description: Declaration of 'add_avx256_epi64' void function.

	  @Params:  1st source array of integers (64-bit)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of integers (64-bit)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold integers (64-bit)

	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  add_avx256_epi64(const long long src1[], const int src1_len,
						   const long long src2[], const int src2_len,
						   long long dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'sub_avx256_epi32' void function.

	  @Params:  1st source array of integers (32-bit)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of integers (32-bit)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold integers (32-bit)

	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  sub_avx256_epi32(const int src1[], const int src1_len,
						   const int src2[], const int src2_len,
						   int  dst[],		 const int dst_len );

	/*
	  @Description: Declaration of 'sub_avx256_epi64' void function.

	  @Params:  1st source array of integers (64-bit)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of integers (64-bit)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold integers (64-bit)

	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  sub_avx256_epi64(const long long src1[], const int src1_len,
						   const long long src2[], const int src2_len,
						   long long dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'abs_avx256_epi32' void function.

	  @Params:  1st source array of integers (32-bit)
	  @Params:  length of 1st source array
	
	  @Params:  destination array assumed to hold integers (32-bit)

	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  abs_avx256_epi32(const int src[], const int src_len, int dst[], const int dst_len);

	/*
	  @Description: Declaration of 'and_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  and_avx256_ps(const float src1[], const int src1_len,
		                const float src2[], const int src2_len,
		                float dst[],        const int dst_len);

	/*
	  @Description: Declaration of 'and_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  and_avx256_pd(const double src1[], const int src1_len,
		const double src2[], const int src2_len,
		double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'andnot_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  andnot_avx256_pd(const double src1[], const int src1_len,
		const double src2[], const int src2_len,
		double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'andnot_avx256_si256' void function.

	  @Params:  1st source array of integers (32-bit)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of integers (32-bit)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold integers (32-bit)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  andnot_avx256_si256(const int src1[], const int src1_len,
		const int src2[], const int src2_len,
		int dst[], const int dst_len);

	/*
	  @Description: Declaration of 'xor_avx256_ps' void function.

	@Params:  1st source array of floats (32-bit, 24-bit precision)
	@Params:  length of 1st source array
	@Params:  2nd source array of floats (32-bit, 24-bit precision)
	@Params:  length of 2nd source array
	@Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	@Params:  length of 2nd destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  xor_avx256_ps(const float src1[], const int src1_len,
		const float src2[], const int src2_len,
		float dst[], const int dst_len);

	/*
	@Description: Declaration of 'xor_avx256_pd' void function.

	@Params:  1st source array of doubles (64-bit, 56-bit precision)
	@Params:  length of 1st source array
	@Params:  2nd source array of doubles (64-bit, 56-bit precision)
	@Params:  length of 2nd source array
	@Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	@Params:  length of 2nd destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  xor_avx256_pd(const double src1[], const int src1_len,
		const double src2[], const int src2_len,
		double dst[], const int dst_len);

	/*
	@Description: Declaration of 'xor_avx256_si256' void function.

	@Params:  1st source array of integers (32-bit)
	@Params:  length of 1st source array
	@Params:  2nd source array of integers(32-bit)
	@Params:  length of 2nd source array
	@Params:  destination array assumed to hold integers (32-bit)
	@Params:  length of 2nd destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  xor_avx256_si256(const int src1[], const int src1_len,
		const int src2[], const int src2_len,
		int dst[], const int dst_len);

	/*
	@Description: Declaration of 'or_avx256_ps' void function.

	@Params:  1st source array of floats (32-bit, 24-bit precision)
	@Params:  length of 1st source array
	@Params:  2nd source array of floats (32-bit, 24-bit precision)
	@Params:  length of 2nd source array
	@Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	@Params:  length of 2nd destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  or_avx256_ps(const float src1[], const int src1_len,
		const float src2[], const int src2_len,
		float dst[], const int dst_len);

	/*
	@Description: Declaration of 'or_avx256_pd' void function.

	@Params:  1st source array of doubles (64-bit, 56-bit precision)
	@Params:  length of 1st source array
	@Params:  2nd source array of doubles (64-bit, 56-bit precision)
	@Params:  length of 2nd source array
	@Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	@Params:  length of 2nd destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  or_avx256_pd(const double src1[], const int src1_len,
		const double src2[], const int src2_len,
		double dst[], const int dst_len);

	/*
	@Description: Declaration of 'or_avx256_si256' void function.

	@Params:  1st source array of integers (32-bit)
	@Params:  length of 1st source array
	@Params:  2nd source array of integers (32-bit)
	@Params:  length of 2nd source array
	@Params:  destination array assumed to hold integers (32-bit)
	@Params:  length of 2nd destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  or_avx256_si256(const int src1[], const int src1_len,
		const int src2[], const int src2_len,
		int dst[], const int dst_len);

	/*
	  @Description: Declaration of 'idiv_avx256_epi32' void function.

	  @Params:  1st source array of integers (32-bit)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of integers (32-bit)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold integers (32-bit)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  idiv_avx256_epi32(const int src1[], const int src1_len,
						    const int src2[], const int src2_len,
							int  dst[],       const int  dst_len );

	/*
	  @Description: Declaration of 'rem_avx256_epi32' void function.

	  @Params:  1st source array of integers (32-bit)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of integers (32-bit)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold integers (32-bit)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  rem_avx256_epi32(const int src1[], const int src1_len,
						   const int src2[], const int src2_len,
						   int  dst[],       const int  dst_len );

	/*
	  @Description: Declaration of 'rem_avx256_epi64' void function.

	  @Params:  1st source array of integers (64-bit)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of integers (64-bit)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold integers (64-bit)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  rem_avx256_epi64(const long long src1[], const int src1_len,
						   const long long src2[], const int src2_len,
						   long long  dst[],       const int dst_len );



	/*
	  @Description: Declaration of 'ceil_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  ceil_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	@Description: Declaration of 'ceil_avx256_pd' void function.

	@Params:  source array of doubles (64-bit, 56-bit precision)
	@Params:  length of source array

	@Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	@Params:  length of destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  ceil_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	@Description: Declaration of 'floor_avx256_ps' void function.

	@Params:  source array of floats (32-bit, 24-bit precision)
	@Params:  length of source array

	@Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	@Params:  length of destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  floor_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	@Description: Declaration of 'floor_avx256_pd' void function.

	@Params:  source array of doubles (64-bit, 56-bit precision)
	@Params:  length of source array

	@Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	@Params:  length of destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  floor_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	@Description: Declaration of 'max_avx256_ps' void function.

	@Params:  source array of floats (32-bit, 24-bit precision)
	@Params:  length of source array

	@Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	@Params:  length of destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  max_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	@Description: Declaration of 'max_avx256_pd' void function.

	@Params:  source array of doubles (64-bit, 56-bit precision)
	@Params:  length of source array

	@Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	@Params:  length of destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  max_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	@Description: Declaration of 'min_avx256_ps' void function.

	@Params:  source array of floats (32-bit, 24-bit precision)
	@Params:  length of source array

	@Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	@Params:  length of destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  min_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	@Description: Declaration of 'min_avx256_pd' void function.

	@Params:  source array of doubles (64-bit, 56-bit precision)
	@Params:  length of source array

	@Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	@Params:  length of destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  min_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	@Description: Declaration of 'round_avx256_ps' void function.

	@Params:  source array of floats (32-bit, 24-bit precision)
	@Params:  length of source array

	@Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	@Params:  length of destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  round_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	@Description: Declaration of 'round_avx256_pd' void function.

	@Params:  source array of doubles (64-bit, 56-bit precision)
	@Params:  length of source array

	@Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	@Params:  length of destination array
	@Returns: Nothing
	@Throws:  Nothing
	*/

	void  round_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);


	/*
	  @Description: Declaration of 'trunc_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  trunc_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'trunc_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array

	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  trunc_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'fmadd_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  fmadd_avx256_ps(const float src1[], const float src2[],
						  const float src3[], const int src_len,
						  float dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'fmadd_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  fmadd_avx256_pd(const double src1[], const double src2[],
						  const double src3[], const int src_len,
						  double dst[],        const int dst_len  );

	/*
	  @Description: Declaration of 'fmaddsub_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  fmaddsub_avx256_ps(const float src1[], const float src2[],
							 const float src3[], const int src_len, 
							 float dst[],        const int dst_len  );

	/*
	  @Description: Declaration of 'fmaddsub_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  fmaddsub_avx256_pd(const double src1[], const double src2[],
							 const double src3[], const int src_len, 
							 double dst[],        const int dst_len  );

	/*
	  @Description: Declaration of 'fmsub_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  fmasub_avx256_ps(const float src1[], const float src2[],
						   const float src3[], const int  src_len,
						   float dst[],        const int dst_len  );

	/*
	  @Description: Declaration of 'fmsub_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  fmasub_avx256_pd(const double src1[], const double src2[],
						   const double src3[], const int  src_len,
						   double dst[],        const int dst_len  );

	/*
	  @Description: Declaration of 'fmsubadd_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  fmsubadd_avx256_ps(const float src1[], const float src2[],
						     const float src3[], const int  src_len,
							 float dst[],        const int dst_len );

	/*
	  @Description: Declaration of 'fmsubadd_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  fmsubadd_avx256_pd(const double src1[], const double src2[],
							 const double src3[], const int  src_len,
							 double dst[],        const int dst_len  );

	/*
	  @Description: Declaration of 'fnmadd_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  fnmadd_avx256_ps(const float src1[], const float src2[],
						   const float src3[], const int   src_len,
						   float dst[],        const int   dst_len );

	/*
	  @Description: Declaration of 'fnmadd_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  fnmadd_avx256_pd(const double src1[], const double src2[],
						   const double src3[], const int  src_len,
						   double dst[],        const int  dst_len );

	/*
	  @Description: Declaration of 'fnmsub_avx256_ps' void function.

	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/
	
	void  fnmsub_avx256_ps(const float src1[], const float src2[],
						   const float src3[], const int  src_len,
						   float dst[],        const int  dst_len );

	/*
	  @Description: Declaration of 'fnmsub_avx256_pd' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  fnmsub_avx256_pd(const double src1[], const double src2[],
						   const double src3[], const int  src_len,
						   double dst[],        const int  dst_len );


}


#endif /* C++ compilation in use. */



#endif /*__ARITHMETIC_INTRINSICS_H__*/