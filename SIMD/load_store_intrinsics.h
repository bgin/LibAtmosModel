
#ifndef __LOAD_STORE_INTRINSICS_H__
#define __LOAD_STORE_INTRINSICS_H__

// Tab:  5 functions declarations
// Tab:  7 Comments: Functions parameter description
// Tab:  2 col distance between function declaration components.
// Line: 1 line between code and comments.

#if !defined (LOAD_STORE_INTRINSICS_VERSION_MAJOR)
#define LOAD_STORE_INTRINSICS_VERSION_MAJOR 1
#endif

#if !defined (LOAD_STORE_INTRINSICS_VERSION_MINOR)
#define LOAD_STORE_INTRINSICS_VERSION_MINOR 0
#endif

#if !defined (LOAD_STORE_INTRINSICS_VERSION_MICRO)
#define LOAD_STORE_INTRINSICS_VERSION_MICRO 0
#endif

#if !defined (LOAD_STORE_INTRINSICS_CREATE_DATE)
#define LOAD_STORE_INTRINSICS_CREATE_DATE "Date: 2017-02-28 , Time: 18:28 PM GMT+2, -200"
#endif

/* Set this value to last successful file build date/time. */
#if !defined (LOAD_STORE_INTRINSICS_BUILD_DATE)
#define LOAD_STORE_INTRINSICS_BUILD_DATE " "
#endif

#if !defined (LOAD_STORE_INTRINSICS_AUTHOR)
#define LOAD_STORE_INTRINSICS_AUTHOR "Programmer: Bernard Gingold , contact: beniekg@gmail.com "
#endif


#include "config.h"
#include "simd_support.h"

#if defined ForAVXLib_DEFAULT_CXX_VERSION && \
  ForAVXLib_DEFAULT_CXX_VERSION >= 199711L

extern "C" {

	/************************************************
	  'C' implementation of Intel Intrinsics.
	  Load-Store version declarations.
	  Callable from Fortran side.
	*************************************************/

	/*
	   @Description: Declaration of 'uload_store_avx256_ps' void function.
					 Unaligned load-store operation.
	   @Params: source array of floats (32-bit, 24-bit precision)
	   @Params: length of source array
	   @Params: destination array assumed to hold floats (32-bit, 24-bit precision)
	   @Params: length of destination array
	   @Returns: Nothing
	   @Throws: Nothing
	*/

	void  uload_store_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'uload_store_avx256_pd' void function.
				    Unaligned load-store operation.
	  @Params: source array of doubles (64-bit, 56-bit precision)
	  @Params: length of source array
	  @Params: destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params: length of destination array
	  @Returns: Nothing
	  @Throws: Nothing
	*/

	void  uload_store_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'aload_store_avx256_ps' void function.
				    Aligned load-store operation.
	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  aload_store_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'aload_store_avx256_pd' void function.
	                Aligned load-store operation.
	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  aload_store_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'broadcast_avx256_ss' void function.
	                Unaligned load-store operation.
	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  broadcast_avx256_ss(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'broadcast_avx256_sd' void function.
	                Unaligned load-store operation.
	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  broadcast_avx256_sd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'load2_store_avx256_m128' void function.
	                
	  @Params:  1st source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  load2_store_avx256_m128(const float hiaddr[], const int hiaddr_len, 
	                              const float loaddr[], const int loaddr_len,
								  float dst[],          const int dst_len  );

	/*
	  @Description: Declaration of 'load2_store_avx256_m128d' void function.

	  @Params:  1st source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st source array
	  @Params:  2nd source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd source array.
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  load2_store_avx256_m128d(const double hiaddr[], const int hiaddr_len,
		                           const double loaddr[], const int loaddr_len,
								   double dst[],          const int dst_len  );

	/*
	  @Description: Declaration of 'stream_load_avx256_si256' void function.
	 
	  @Params:  source array of integers (32-bit)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold integers (32-bit)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/
    
	void  stream_load_avx256_si256(const int src[], const int src_len, int dst[], const int dst_len);

	/*
	  @Description: Declaration of 'maskload_avx256_epi32' void function.

	  @Params:  source array of integers (32-bit)
	  @Params:  length of source array
	  @Params:  mask static integer array
	  @Params:  destination array assumed to hold integers (32-bit)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  maskload_avx256_epi32(const int src[], const int src_len, const int mask[8], int dst[], const int dst_len);

	/*
	  @Description: Declaration of 'movedup_avx256_pd' void function.
	
	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  movedup_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'movehdup_avx256_ps' void function.
	
	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  movehdup_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'moveldup_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  moveldup_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'store_avx256_ps' void function.
					Aligned memory load-store operation.
	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  store_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'store_avx256_pd' void function.
				    Aligned memory load-store operation.
	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  store_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'storeu_avx256_ps' void function.
	                Unaligned memory load-store operation.
	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  storeu_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'storeu_avx256_pd' void function.
	                Unaligned memory load-store operation.
	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  storeu_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'storeu2_avx256_m128' void function.
	                Unaligned memory load-store operation.
	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of  source array
	  @Params:  1st destination array of floats (32-bit, 24-bit precision)
	  @Params:  length of 1st destination array
	  @Params:  2nd destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  storeu2_avx256_m128(const float src[], const int src_len, 
							  float dst1[],      const int dst1_len,
							  float dst2[],      const int dst2_len );

	/*
	  @Description: Declaration of 'storeu2_avx256_m128d' void function.
	                Unaligned memory load-store operation.
	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of  source array
	  @Params:  1st destination array of doubles (64-bit, 56-bit precision)
	  @Params:  length of 1st destination array
	  @Params:  2nd destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  storeu2_avx256_m128d(const double src[],  const int src_len,
		                       double dst1[],       const int dst1_len,
							   double dst2[],       const int dst2_len );

	/*
	  @Description: Declaration of 'stream_avx256_ps' void function.
	  
	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  stream_avx256_ps(const float src[], const int src_len, float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'stream_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  stream_avx256_pd(const double src[], const int src_len, double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'maskstore_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
	  @Params:  length of source array
	  @Params:  mask static integer(32-bit) array of length 8
	  @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  maskstore_avx256_ps(const float src[], const int src_len, const int mask[8], float dst[], const int dst_len);

	/*
	  @Description: Declaration of 'maskstore_avx256_pd' void function.

	  @Params:  source array of doubles (64-bit, 56-bit precision)
	  @Params:  length of source array
	  @Params:  mask static integer(32-bit) array of length 8
	  @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
	  @Params:  length of destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  maskstore_avx256_pd(const double src[], const int src_len, const int mask[8], double dst[], const int dst_len);

	/*
	  @Description: Declaration of 'blend_avx256_epi32' void function.
	
	  @Params:  source array of integers (32-bit)
	  @Params:  length of  source array
	  @Params:  1st destination array of integers (32-bit)
	  @Params:  length of 1st destination array
	  @Params:  2nd destination array assumed to hold integers (32-bit)
	  @Params:  integer(32-bit) imm
	  @Params:  length of 2nd destination array
	  @Returns: Nothing
	  @Throws:  Nothing
	*/

	void  blend_avx256_epi32(const int src1[], const int src1_len,
							 const int src2[], const int src2_len,
							 const int imm,    int dst[], const int dst_len );

	

}

#endif /* CXX compilation.*/



#endif /*__LOAD_STORE_INTRINSICS_H__*/