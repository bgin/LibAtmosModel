
#include "element_funcs_intrinsics.h"
#include <iostream>
#include <iomanip>



/*
      @Description: Implementation of 'rcp_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  rcp_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);
#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in rcp_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in rcp_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in rcp_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif


	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
	__asm {cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for(int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_rcp_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8], _mm256_rcp_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_rcp_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_rcp_ps(_mm256_loadu_ps(&src[i+24])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i],      _mm256_rcp_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_rcp_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_rcp_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_rcp_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm {cpuid}
	unsigned __int64 stop_clock{__rdtscp(&ia32_tsc_aux)};
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_rcp_ps intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "RDTSCP current read out: " << stop_clock - start_clock << ".\n";
	}

#endif
#else

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_rcp_ps(_mm256_loadu_ps(&src[i])));
		}
   }
   else {

	   for (int i{0}; i != src_len; i += INCR_BY_8 ) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
		   _mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
		   // On non Haswell CPU using aligned load-store operations
		   // Caller must ensure, that src and dst arrays are aligned
		   // on 32-byte boundaries, otherwise #GP error will occure.
		   _mm256_store_ps(&dst[i], _mm256_rcp_ps(_mm256_load_ps(&src[i])));
	   }
   }


#endif
}

/*
      @Description: Implementation of 'rsqrt_avx256_ps' void function.

	  @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  rsqrt_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);
#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in rsqrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in rsqrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in rsqrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	_asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for(int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
 IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_rsqrt_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_rsqrt_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_rsqrt_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_rsqrt_ps(_mm256_loadu_ps(&src[i+24])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i],      _mm256_rsqrt_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_rsqrt_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_rsqrt_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_rsqrt_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux)};
	
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_rsqrt_ps intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock  << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}


#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_rsqrt_ps(_mm256_loadu_ps(&src[i])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif

			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_rsqrt_ps(_mm256_load_ps(&src[i])));
		}
	}

#endif

}

/*
	  @Description: Implementation of 'sqrt_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  sqrt_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _In_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);
#else

	if(src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in sqrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in sqrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in sqrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for(int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_sqrt_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_sqrt_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_sqrt_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_sqrt_ps(_mm256_loadu_ps(&src[i+24])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i],      _mm256_sqrt_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_sqrt_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_sqrt_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_sqrt_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_sqrt_ps intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_sqrt_ps(_mm256_loadu_ps(&src[i])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif

			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_sqrt_ps(_mm256_load_ps(&src[i])));
		}
	}

#endif


}

/*
      @Description: Implementation of 'sqrt_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  sqrt_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in sqrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in sqrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in sqrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_sqrt_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4], _mm256_sqrt_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8], _mm256_sqrt_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_sqrt_pd(_mm256_loadu_pd(&src[i+12])));
       }

  }
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_sqrt_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4], _mm256_sqrt_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8], _mm256_sqrt_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_sqrt_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_sqrt_pd intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {

		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}

#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_sqrt_pd(_mm256_loadu_pd(&src[i])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]));
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_sqrt_pd(_mm256_load_pd(&src[i])));
		}
	}

#endif

}

/*
	  @Description: Implementation of 'cbrt_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  cbrt_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in cbrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	
	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in cbrt_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n"
				<< "value of src_len=" << src_len << "\n"
				<< "value of dst_len=" << dst_len << "\n"
				<< "Cannot recover -- calling exit!!";
			std::exit(-1);
		}
	
	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in cbrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif
	
	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for(int i{0}; i != src_len; i += INCR_BY_32 ) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_cbrt_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_cbrt_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_cbrt_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_cbrt_ps(_mm256_loadu_ps(&src[i+24])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i],      _mm256_cbrt_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_cbrt_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_cbrt_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_cbrt_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_cbrt_ps intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value.\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";

	}

#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu__ps(&dst[i], _mm256_cbrt_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {
		
		for (int i{0}; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_cbrt_ps(_mm256_load_ps(&src[i])));
		}
}

#endif

}

/*
      @Description: Implementation of 'cbrt_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  cbrt_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in cbrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len > 0 || dst > 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in cbrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in cbrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined ForAVXLib_DEBUG_ON
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for(int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i],      _mm256_cbrt_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_cbrt_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_cbrt_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_cbrt_pd(_mm256_loadu_pd(&src[i+12])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i],      _mm256_cbrt_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_cbrt_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_cbrt_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_cbrt_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_cbrt_pd intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value.\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_cbrt_pd(_mm256_loadu_pd(&src[i])));

		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_cbrt_pd(_mm256_load_pd(&src[i])));
		}
	}

#endif


}

/*
	  @Description: Implementation of 'cexp_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  cexp_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in cexp_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in cexp_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in cexp_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for(int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_cexp_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_cexp_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_cexp_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_cexp_ps(_mm256_loadu_ps(&src[i+24])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i],      _mm256_cexp_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_cexp_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_cexp_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_cexp_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_cexp_ps intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_cexp_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_cexp_ps(_mm256_load_ps(&src[i])));
		}
	}


#endif	

}

/*
	  @Description: Implementation of 'cexp_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
	  @Warning: This function performs narrowing type cast operations (double precision->single precision)
				Use it with caution!!
*/

void  cexp_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0 ));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in cexp_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in cexp_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in cexp_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
	constexpr int INCR_BY_32{ 8 * INCR_BY_4};
#define IS_EXCEEDING_L1 (src_len)

	float* __restrict tmpIn  = reinterpret_cast<float*>(_mm_malloc(src_len * sizeof(float),32));
	float* __restrict tmpOut = reinterpret_cast<float*>(_mm_malloc(src_len * sizeof(float),32));
	if (tmpIn == NULL || tmpOut == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in cexp_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer tmpIn =" << std::hex << "0x" << tmpIn << "\n"
			<< "value of pointer tmpOut=" << std::hex << "0x" << tmpOut << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}



	
#if defined USE_MANUAL_UNROLLING

	// FP 64-bit to FP 32-bit conversion stage.
#pragma SIMD vectorlength(4,8)
	for (int i{ 0 }; i != src_len; ++i) {
#pragma prefetch src:0:4
		tmpIn[i] = static_cast<float>(src[i]);
	}

#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif
		
	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		// Haswell CPU
		for (int i{0}; i != src_len; i += INCR_BY_32) {

#if defined	SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T1);
#else
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
#endif
			
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&tmpOut[i],      _mm256_cexp_ps(_mm256_loadu_ps(&tmpIn[i])));
			_mm256_storeu_ps(&tmpOut[i + 8],  _mm256_cexp_ps(_mm256_loadu_ps(&tmpIn[i+8])));
			_mm256_storeu_ps(&tmpOut[i + 16], _mm256_cexp_ps(_mm256_loadu_ps(&tmpIn[i+16])));
			_mm256_storeu_ps(&tmpOut[i + 24], _mm256_cexp_ps(_mm256_loadu_ps(&tmpIn[i+24])));
		}
	}
	else {
			
		for (int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1	> L1_MAX_SP
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T1);
#else
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
#endif

			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&tmpOut[i],      _mm256_cexp_ps(_mm256_load_ps(&tmpIn[i])));
			_mm256_store_ps(&tmpOut[i + 8],  _mm256_cexp_ps(_mm256_load_ps(&tmpIn[i+8])));
			_mm256_store_ps(&tmpOut[i + 16], _mm256_cexp_ps(_mm256_load_ps(&tmpIn[i+16])));
			_mm256_store_ps(&tmpOut[i + 24], _mm256_cexp_ps(_mm256_load_ps(&tmpIn[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		 std::cout << "Crude approximation of _mm256_cexp_pd intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif 

	// Second conversion stage from FP 32-bit to FP 64-bit.
#pragma SIMD vectorlength(4,8)
	for (int i{ 0 }; i != src_len; ++i) {
#pragma prefetch tmpOut:0:4
		dst[i] = static_cast<double>(tmpOut[i]);
	}

	_mm_free(tmpIn);
	_mm_free(tmpOut);

#else
		
// FP 64-bit to FP 32-bit conversion stage.
#pragma SIMD vectorlength(4,8)
      for (int i{ 0 }; i != src_len; ++i) {
#pragma prefetch src:0:4
	       tmpIn[i] = static_cast<float>(src[i]);
  }
	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += 8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&tmpOut[i], _mm256_cexp_ps(_mm256_loadu_ps(tmpIn[i])));
		}
	}
	else {
		
		for (int i{0}; i != src_len; i += 8) {
#if defined SOFT_PREFETCH_L1 
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occur.
			_mm256_store_ps(&tmpOut[i], _mm256_cexp_ps(_mm256_load_ps(&tmpIn[i])));
		}
	}

	// Second conversion stage from FP 32-bit to FP 64-bit.
#pragma SIMD vectorlength(4,8)
	for (int i{ 0 }; i != src_len; ++i) {
#pragma prefetch tmpOut:0:4
		dst[i] = static_cast<double>(tmpOut[i]);
	}
	_mm_free(tmpIn);
	_mm_free(tmpOut);

#endif

}

/*
	  @Description: Implementation of 'clog_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  clog_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in clog_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in clog_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in clog_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for(int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);

#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_clog_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_clog_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_clog_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_clog_ps(_mm256_loadu_ps(&src[i+24])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occur.
			_mm256_store_ps(&dst[i], _mm256_clog_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8], _mm256_clog_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_clog_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_clog_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_clog_ps intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}

#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_clog_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_clog_ps(_mm256_load_ps(&src[i])));
		}
	}

#endif


}

/*
      @Description: Implementation of 'clog_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
	  @Warning: This function performs narrowing type cast conversion from double precision
				to single precision.
*/

void  clog_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0 ));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in clog_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in clog_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in clog_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{4 * INCR_BY_8};
#define IS_EXCEEDING_L1 (src_len)
	float* __restrict tmpIn  = reinterpret_cast<float *>(_mm_malloc(src_len * sizeof(float),32));
	float* __restrict tmpOut = reinterpret_cast<float *>(_mm_malloc(src_len * sizeof(float),32));
	if (tmpIn == NULL || tmpOut == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in clog_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer tmpIn =" << std::hex << "0x" << tmpIn << "\n"
			<< "value of pointer tmpOut=" << std::hex << "0x" << tmpOut << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#if defined USE_MANUAL_UNROLLING

	// Copy stage auto-vectorized.
#pragma SIMD vectorlength(4,8)
	for (int i{ 0 }; i != src_len; ++i){
#pragma prefetch src:0:4
		tmpIn[i] = static_cast<float>(src[i]);
	}

#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif
			
	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		// Haswell CPU
		for (int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T1);
#else
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
#endif		
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&tmpOut[i],    _mm256_clog_ps(_mm256_loadu_ps(&tmpIn[i])));
			_mm256_storeu_ps(&tmpOut[i+8],  _mm256_clog_ps(_mm256_loadu_ps(&tmpIn[i+8])));
			_mm256_storeu_ps(&tmpOut[i+16], _mm256_clog_ps(_mm256_loadu_ps(&tmpIn[i+16])));
			_mm256_storeu_ps(&tmpOut[i+24], _mm256_clog_ps(_mm256_loadu_ps(&tmpIn[i+24])));
		}
	}
	else {
			
		for(int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T1);
#else
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&tmpOut[i],      _mm256_clog_ps(_mm256_load_ps(&tmpIn[i])));
			_mm256_store_ps(&tmpOut[i + 8],  _mm256_clog_ps(_mm256_load_ps(&tmpIn[i+8])));
			_mm256_store_ps(&tmpOut[i + 16], _mm256_clog_ps(_mm256_load_ps(&tmpIn[i+16])));
			_mm256_store_ps(&tmpOut[i + 24], _mm256_clog_ps(_mm256_load_ps(&tmpIn[i+24])));
		}
   }
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_clog_pd intrinsic(internally operating on floats). \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
	
#endif
	// FP 32-bit to FP 64-bit conversion stage - auto vectorised.
#pragma SIMD vectorlength(4,8)
	for (int i{ 0 }; i != src_len; ++i) {
#pragma prefetch tmpOut:0:4
		dst[i] = static_cast<double>(tmpOut[i]);
	}
	_mm_free(tmpIn);
	_mm_free(tmpOut);
#else

	// FP 64-bit to FP 32-bit conversion stage.
#pragma SIMD vectorlength(4,8)
	for (int i{ 0 }; i != src_len; ++i) {
#pragma prefetch src:0:4
		tmpIn[i] = static_cast<float>(src[i]);
	}
	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += 8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&tmpOut[i], _mm256_clog_ps(_mm256_loadu_ps(tmpIn[i])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += 8) {
#if defined SOFT_PREFETCH_L1 
#pragma noprefetch tmpIn,tmpOut
			_mm_prefetch(reinterpret_cast<const char*>(&tmpIn[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occur.
			_mm256_store_ps(&tmpOut[i], _mm256_clog_ps(_mm256_load_ps(&tmpIn[i])));
		}
	}

	// Second conversion stage from FP 32-bit to FP 64-bit.
#pragma SIMD vectorlength(4,8)
	for (int i{ 0 }; i != src_len; ++i) {
#pragma prefetch tmpOut:0:4
		dst[i] = static_cast<double>(tmpOut[i]);
	}
	_mm_free(tmpIn);
	_mm_free(tmpOut);


#endif

}

/*
      @Description: Implementation of 'exp10_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  exp10_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in exp10_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in exp10_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in exp10_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };

#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_exp10_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_exp10_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_exp10_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_exp10_ps(_mm256_loadu_ps(&src[i+24])));
		}
   }
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occur.
			_mm256_store_ps(&dst[i],      _mm256_exp10_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_exp10_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_exp10_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_exp10_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_exp10_ps intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_exp10_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_exp10_ps(_mm256_load_ps(&src[i])));
		}
	}

#endif


}

/*
      @Description: Implementation of 'exp10_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  exp10_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in exp10_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in exp10_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in exp10_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for(int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);

#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_exp10_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4], _mm256_exp10_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8], _mm256_exp10_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_exp10_pd(_mm256_loadu_pd(&src[i+12])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i],      _mm256_exp10_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_exp10_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_exp10_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_exp10_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_exp10_pd intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock  << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
		
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_exp10_pd(_mm256_loadu_pd(&src[i])));

		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_exp10_pd(_mm256_load_pd(&src[i])));
		}
	}

#endif


}

/*
	  @Description: Implementation of 'exp2_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  exp2_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL && dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in exp2_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in exp2_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in exp2_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_32) {
			
#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_exp2_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8], _mm256_exp2_ps(_mm256_loadu_ps(&dst[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_exp2_ps(_mm256_loadu_ps(&dst[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_exp2_ps(_mm256_loadu_ps(&dst[i+24])));
		}
   }
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_exp2_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8], _mm256_exp2_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_exp2_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_exp2_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_exp2_ps intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << "\n";
	}

#endif
#else
	
	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_exp2_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_exp2_ps(_mm256_load_ps(&src[i])));
		}
	}


#endif

}

/*
      @Description: Implementation of 'exp2_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  exp2_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in exp2_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in exp2_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in exp2_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i],      _mm256_exp2_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_exp2_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_exp2_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_exp2_pd(_mm256_loadu_pd(&src[i+12])));
		}
   }
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i],      _mm256_exp2_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_exp2_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_exp2_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_exp2_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_exp2_pd intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value.\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}

#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_exp2_pd(_mm256_loadu_pd(&src[i])));

		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_exp2_pd(_mm256_load_pd(&src[i])));
		}
	}


#endif

}

/*
	  @Description: Implementation of 'exp_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  exp_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len <= 0 && dst_len <= 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in exp_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in exp_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in exp_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_exp_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8], _mm256_exp_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_exp_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_exp_ps(_mm256_loadu_ps(&src[i+24])));
		}
    }
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_exp_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8], _mm256_exp_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_exp_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_exp_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_exp_ps intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value.\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif	
#else
	
	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_exp_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_exp_ps(_mm256_load_ps(&src[i])));
		}
	}


#endif

}

/*
      @Description: Implementation of 'exp_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  exp_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in exp_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in exp_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in exp_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif
		
	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for(int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_exp_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4], _mm256_exp_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8], _mm256_exp_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_exp_pd(_mm256_loadu_pd(&src[i+12])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else

#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occur.
			_mm256_store_pd(&dst[i], _mm256_exp_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4], _mm256_exp_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8], _mm256_exp_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_exp_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_exp_pd intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value.\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif

#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_exp_pd(_mm256_loadu_pd(&src[i])));

		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_exp_pd(_mm256_load_pd(&src[i])));
		}
	}


#endif

}

/*
      @Description: Implementation of 'expm1_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  expm1_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if(src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in expm1_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in expm1_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in expm1_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif
		
	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_expm1_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8], _mm256_expm1_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_expm1_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_expm1_ps(_mm256_loadu_ps(&src[i+24])));
		}
   }
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_expm1_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8], _mm256_expm1_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_expm1_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_expm1_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_expm1_ps intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value.\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}

#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_expm1_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_expm1_ps(_mm256_load_ps(&src[i])));
		}
	}


#endif


}

/*
      @Description: Implementation of 'expm1_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  expm1_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in expm1_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in expm1_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in expm1_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for(int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i],      _mm256_expm1_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_expm1_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_expm1_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_expm1_pd(_mm256_loadu_pd(&src[i+12])));
		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_expm1_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4], _mm256_expm1_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8], _mm256_expm1_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_expm1_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_expm1_pd intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value.\n"
			<< "TSC current read out: " << stop_clock - start_clock << "\n";
	}

#endif
#else
		
	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_expm1_pd(_mm256_loadu_pd(&src[i])));

		}
	}
	else {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_expm1_pd(_mm256_load_pd(&src[i])));
		}
	}

		
#endif

}

/*
	  @Description: Implementation of 'invcbrt_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  invcbrt_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else
	
	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in invcbrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in invcbrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in invcbrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for(int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_invcbrt_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_invcbrt_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_invcbrt_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_invcbrt_ps(_mm256_loadu_ps(&src[i+24])));
		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src) & 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst) & 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in invcbrt_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i],      _mm256_invcbrt_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_invcbrt_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_invcbrt_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_invcbrt_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_invcbrt_ps intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned invalid argument.\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif

#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_invcbrt_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src) & 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst) & 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in invcbrt_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src) & 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" : 
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" : 
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_invcbrt_ps(_mm256_load_ps(&src[i])));
		}
	}

#endif

}

/*
	  @Description: Implementation of 'invcbrt_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  invcbrt_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in invcbrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in invcbrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in invcbrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for(int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i],      _mm256_invcbrt_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_invcbrt_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_invcbrt_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_invcbrt_pd(_mm256_loadu_pd(&src[i+12])));
		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src) & 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst) & 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in invcbrt_avx256_pd!!\n"
				      << "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				      << "*****ERROR-DETAILS***** \n";
					  (reinterpret_cast<uintptr_t>(src) & 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" : 
					                                                   std::cout << " src unaligned on 32-byte boundary.\n";
			          (reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" : 
						                                               std::cout << " dst unaligned on 32-byte boundary.\n";
		    std::cerr  << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif
			
		for(int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);

#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i],    _mm256_invcbrt_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i+4],  _mm256_invcbrt_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i+8],  _mm256_invcbrt_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i+12], _mm256_invcbrt_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_invcbrt_pd intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value.\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif

#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_invcbrt_pd(_mm256_loadu_pd(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in invcbrt_avx256_pd!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_invcbrt_pd(_mm256_load_pd(&src[i])));
		}
	}

#endif

}

/*
	  @Description: Implementation of 'invsqrt_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  invsqrt_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if(src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in invsqrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in invsqrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in invsqrt_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{8};
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for(int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_invsqrt_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_invsqrt_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_invsqrt_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_invsqrt_ps(_mm256_loadu_ps(&src[i+24])));
		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
	
		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in invsqrt_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif
		
		for (int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i],      _mm256_invsqrt_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_invsqrt_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_invsqrt_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_invsqrt_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_invsqrt_ps intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {

		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif

#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i+4]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_invsqrt_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in invsqrt_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i+4]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_invsqrt_ps(_mm256_load_ps(&src[i])));
		}
	}


#endif

}

/*
	  @Description: Declaration of 'invsqrt_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  invsqrt_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in invsqrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in invsqrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in invsqrt_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif
		
	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i],      _mm256_invsqrt_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_invsqrt_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_invsqrt_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_invsqrt_pd(_mm256_loadu_pd(&src[i+12])));
		}
    }
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);
#else
		
		if (((reinterpret_cast<uintptr_t>(src) & 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst) & 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in invsqrt_avx256_pd!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}

#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_invsqrt_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4], _mm256_invsqrt_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8], _mm256_invsqrt_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_invsqrt_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_invsqrt_pd intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {

		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_invsqrt_pd(_mm256_loadu_pd(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in invsqrt_avx256_pd!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_invsqrt_pd(_mm256_load_pd(&src[i])));
		}
	}


#endif
}

/*
      @Description: Implementation of 'log10_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  log10_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else
	
	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in log10_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in log10_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in log10_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_log10_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8], _mm256_log10_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_log10_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_log10_ps(_mm256_loadu_ps(&src[i+24])));

		}
    }
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);
#else

		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log10_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occur.
			_mm256_store_ps(&dst[i],      _mm256_log10_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_log10_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 16], _mm256_log10_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_log10_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_log10_ps intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << "\n";
	}
#endif
#else
	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_log10_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src) & 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst) & 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log10_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src) & 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_log10_ps(_mm256_load_ps(&src[i])));
		}
	}

	
#endif

}

/*
	  @Description: Implementation of 'log10_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  log10_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len <= 0 && dst_len <= 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in log10_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in log10_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in log10_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i],      _mm256_log10_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_log10_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_log10_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_log10_pd(_mm256_loadu_pd(&src[i+12])));
		}
    }
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);
#else

		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log10_avx256_pd!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src) & 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i],      _mm256_log10_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_log10_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_log10_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_log10_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_log10_pd intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif	

#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_log10_pd(_mm256_loadu_pd(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log10_avx256_pd!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_log10_pd(_mm256_load_pd(&src[i])));
		}
	}

#endif

}

/*
	  @Description: Implementation of 'log1p_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  log1p_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in log1p_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in log1p_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in log1p_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFTCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_log1p_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8], _mm256_log1p_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_log1p_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_log1p_ps(_mm256_loadu_ps(&src[i+24])));
		}
    }
	else {
			
#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);

#else

		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log1p_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif	

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_log1p_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8], _mm256_log1p_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_log1p_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_log1p_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_log1p_ps intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 8]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_log1p_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log1p_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 8]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_log1p_ps(_mm256_load_ps(&src[i])));
		}
	}
#endif


}

/*
      @Description: Declaration of 'log1p_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
	  @Throws:  Nothing
*/

void  log1p_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in log1p_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in log1p_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in log1p_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma prefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i],      _mm256_log1p_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i+4],    _mm256_log1p_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i+8],    _mm256_log1p_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_log1p_pd(_mm256_loadu_pd(&src[i+12])));
		}
    }
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);

#else

		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log1p_avx256_pd!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i],      _mm256_log1p_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_log1p_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_log1p_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_log1p_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_log1p_pd intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_log1p_pd(_mm256_loadu_pd(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log1p_avx256_pd!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_log1p_pd(_mm256_load_pd(&src[i])));
		}
	}
#endif




}

/*
	  @Description: Declaration of 'log2_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  log2_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in log2_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in log2_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in log2_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_32 ) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_log2_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_log2_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_log2_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_log2_ps(_mm256_loadu_ps(&src[i+24])));
		}
	}
	else {
			
#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
#else

		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log2_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i],      _mm256_log2_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_log2_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_log2_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_log2_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_log2_ps intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}

#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 8]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_log2_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log2_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 8]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_log2_ps(_mm256_load_ps(&src[i])));
		}
	}
#endif


}

/*
	  @Description: Implementation of 'log2_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  log2_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in log2_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in log2_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in log2_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_log2_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4], _mm256_log2_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8], _mm256_log2_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_log2_pd(_mm256_loadu_pd(&src[i+12])));
		}
    }
	else {
			
#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);
#else

		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log2_avx256_pd!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_log2_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4], _mm256_log2_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8], _mm256_log2_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_log2_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_log2_pd intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_log2_pd(_mm256_loadu_pd(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in log2_avx256_pd!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_log2_pd(_mm256_load_pd(&src[i])));
		}
	}
#endif

}

/*
	  @Description: Implementation of 'logb_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  logb_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in logb_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in logb_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in log2_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_32 ) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_logb_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8], _mm256_logb_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_logb_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_logb_ps(_mm256_loadu_ps(&src[i+24])));
		}
     }
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);

#else

		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in logb_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_logb_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i+8], _mm256_logb_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_logb_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_logb_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_logb_ps intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 8]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_logb_ps(_mm256_loadu_ps(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in logb_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 8]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i], _mm256_logb_ps(_mm256_load_ps(&src[i])));
		}
	}

#endif


}

/*
	  @Description: Implementation of 'logb_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  logb_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in logb_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in logb_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in logb_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
			
		for(int i{0}; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i],      _mm256_logb_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_logb_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_logb_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_logb_pd(_mm256_loadu_pd(&src[i+12])));
		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);

#else
	
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in logb_avx256_pd!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}

#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i],      _mm256_logb_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_logb_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_logb_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_logb_pd(_mm256_load_pd(&src[i + 12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_logb_pd intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
#else

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_logb_pd(_mm256_loadu_pd(&src[i])));

		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in logb_avx256_pd!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_logb_pd(_mm256_load_pd(&src[i])));
		}
	}

#endif


}

/*
      @Description: Implementation of 'pow_avx256_ps' void function.

      @Params:  source1 array of floats (32-bit, 24-bit precision)
	  @Params:  source2 array of floats (32-bit, 24-bit precision)
      @Params:  length of source1 array
	  @Params:  length of source2 array
      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  pow_avx256_ps(_In_ const float* __restrict src1, _In_ const int src1_len, 
                    _In_ const float* __restrict src2, _In_ const int src2_len,
                    _Inout_ float*    __restrict dst,  _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src1 != NULL &&  src2 != NULL && dst != NULL);
	_ASSERTE((src1_len == src2_len) && (src1_len == dst_len) && (src1_len > 0 && src2_len > 0 && dst_len > 0));
	_ASSERTE((src1_len % 8) == 0);

#else

	if (src1 == NULL || src2 == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in pow_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src1=" << std::hex << "0x" << src1 << "\n"
			<< "value of pointer src2=" << std::hex << "0x" << src2 << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src1_len != src2_len) || (src2_len != dst_len) || (src1_len <= 0 || src2_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in pow_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src1_len=" << src1_len << "\n"
			<< "value of src2_len=" << src2_len << "\n"
			<< "value of dst_len =" << dst_len  << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src1_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in pow_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src1_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif
		
	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src1_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src1,src2,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T1);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T1);
#else
#pragma noprefetch src1,src2,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T0);
#endif

			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_pow_ps(_mm256_loadu_ps(&src1[i]),    _mm256_loadu_ps(&src2[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_pow_ps(_mm256_loadu_ps(&src1[i+8]),  _mm256_loadu_ps(&src2[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_pow_ps(_mm256_loadu_ps(&src1[i+16]), _mm256_loadu_ps(&src2[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_pow_ps(_mm256_loadu_ps(&src1[i+24]), _mm256_loadu_ps(&src2[i+24])));
		}
    }
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src1) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(src2) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)  & 0x1F) == 0);
#else

		if (((reinterpret_cast<uintptr_t>(src1) & 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0) || 
		    ((reinterpret_cast<uintptr_t>(src2) & 0x1F) != 0) ) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in pow_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src1)& 0x1F) == 0 ? std::cout << " src1 aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(src2)& 0x1F) == 0 ? std::cout << " src2 aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << "  dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif

		for (int i{ 0 }; i != src1_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src1,src2,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T1);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T1);
#else
#pragma noprefetch src1,src2,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i],      _mm256_pow_ps(_mm256_load_ps(&src1[i]),      _mm256_load_ps(&src2[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_pow_ps(_mm256_load_ps(&src1[i + 8]),  _mm256_load_ps(&src2[i + 8])));
			_mm256_store_ps(&dst[i + 16], _mm256_pow_ps(_mm256_load_ps(&src1[i + 16]), _mm256_load_ps(&src2[i + 16])));
			_mm256_store_ps(&dst[i + 24], _mm256_pow_ps(_mm256_load_ps(&src1[i + 24]), _mm256_load_ps(&src2[i + 24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_pow_ps intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src1_len << ".\n"
			<< "Unrolled loop iterations: " << src1_len / 8 << ".\n"
			<< "src1 array size:          " << static_cast<double>(src1_len * 4) / 1024.0 << "KiB.\n"
			<< "src2 array size:          " << static_cast<double>(src2_len * 4) / 1024.0 << "KiB. \n"
			<< "dst array size:           " << static_cast<double>(dst_len  * 4) / 1024.0 << "KiB.   \n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src1_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value.\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
#else

         if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

	         for (int i{ 0 }; i != src1_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src1,src2,dst
		           _mm_prefetch(reinterpret_cast<const char*>(&src1[i + 8]), _MM_HINT_T0);
		           _mm_prefetch(reinterpret_cast<const char*>(&src2[i + 8]), _MM_HINT_T0);
#endif
		// Safely assume non-aligned memory load-store.
		// On Haswell CPU this should not cause any memory
		// operation penalty.
		          _mm256_storeu_ps(&dst[i], _mm256_pow_ps(_mm256_loadu_ps(&src1[i]), _mm256_loadu_ps(&src2[i])));

	}
}
else {

#if defined ForAVXLib_DEBUG_ON
	   _ASSERTE((reinterpret_cast<uintptr_t>(src1) & 0x1F) == 0);
	   _ASSERTE((reinterpret_cast<uintptr_t>(src2) & 0x1F) == 0)
	   _ASSERTE((reinterpret_cast<uintptr_t>(dst)  & 0x1F) == 0);
#else
	if (((reinterpret_cast<uintptr_t>(src1) & 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0) || 
		((reinterpret_cast<uintptr_t>(src2) & 0x1F) != 0) ) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in pow_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n";
		(reinterpret_cast<uintptr_t>(src1)& 0x1F) == 0 ? std::cout << " src1 aligned on 32-byte boundary.\n" :
			std::cout << " src unaligned on 32-byte boundary.\n";
		(reinterpret_cast<uintptr_t>(src2)& 0x1F) == 0 ? std::cout << " src2 aligned on 32-byte boundary.\n" :
			std::cout << " src unaligned on 32-byte boundary.\n";
		(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << "  dst aligned on 32-byte boundary.\n" :
			std::cout << " dst unaligned on 32-byte boundary.\n";
		std::cerr << "Cannot recover -- calling exit(-1)!!\n";
		std::exit(-1);
	}
#endif

	for (int i{ 0 }; i != src1_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src1,src2,dst
		_mm_prefetch(reinterpret_cast<const char*>(&src1[i + 8]), _MM_HINT_T0);
		_mm_prefetch(reinterpret_cast<const char*>(&src2[i + 8]), _MM_HINT_T0);
#endif
		// On non Haswell CPU using aligned load-store operations
		// Caller must ensure, that src and dst arrays are aligned
		// on 32-byte boundaries, otherwise #GP error will occure.
		_mm256_store_ps(&dst[i], _mm256_pow_ps(_mm256_load_ps(&src1[i]), _mm256_load_ps(&src2[i])));
	}
}

#endif


}

/*
	  @Description: Implementation of 'pow_avx256_pd' void function.

      @Params:  source1 array of doubles (64-bit, 56-bit precision)
      @Params:  source2 array of doubles (64-bit, 56-bit precision)
      @Params:  length of source1 array
      @Params:  length of source2 array
      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  pow_avx256_pd(_In_ const double* __restrict src1, _In_ const int src1_len,
	                _In_ const double* __restrict src2, _In_ const int src2_len,
	                _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src1 != NULL && src2 != NULL && dst != NULL);
	_ASSERTE((src1_len == src2_len) && (src2_len == dst_len) && (src1_len > 0 && src2_len > 0 && dst_len > 0));
	_ASSERTE((src1_len % 4) == 0);

#else

	if (src1 == NULL || src2 == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in pow_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src1=" << std::hex << "0x" << src1 << "\n"
			<< "value of pointer src2=" << std::hex << "0x" << src2 << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src1_len != src2_len) || (src1_len != dst_len) || (src1_len <= 0 || src2_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in pow_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src1_len=" << src1_len << "\n"
			<< "value of src2_len=" << src2_len << "\n"
			<< "value of dst_len =" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src1_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in pow_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src1_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src1_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src1,src2,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T1);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T1);
#else
#pragma noprefetch src1,src2,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i],      _mm256_pow_pd(_mm256_loadu_pd(&src1[i]),      _mm256_loadu_pd(&src2[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_pow_pd(_mm256_loadu_pd(&src1[i + 4]),  _mm256_loadu_pd(&src2[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_pow_pd(_mm256_loadu_pd(&src1[i + 8]),  _mm256_loadu_pd(&src2[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_pow_pd(_mm256_loadu_pd(&src1[i + 12]), _mm256_loadu_pd(&src2[i+12])));
		}
     }
	else {

#if defined ForAVXLib_DEBUG_ON
		_ASSERTE((reinterpret_cast<uintptr_t>(src1) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(src2) & 0x1F) == 0);
	    _ASSERTE((reinterpret_cast<uintptr_t>(dst)  & 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src1)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0) ||
			((reinterpret_cast<uintptr_t>(src2)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in pow_avx256_ps!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				<< "*****ERROR-DETAILS***** \n";
			(reinterpret_cast<uintptr_t>(src1)& 0x1F) == 0 ? std::cout << " src1 aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(src2)& 0x1F) == 0 ? std::cout << " src2 aligned on 32-byte boundary.\n" :
				std::cout << " src unaligned on 32-byte boundary.\n";
			(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << "  dst aligned on 32-byte boundary.\n" :
				std::cout << " dst unaligned on 32-byte boundary.\n";
			std::cerr << "Cannot recover -- calling exit(-1)!!\n";
			std::exit(-1);
		}
#endif
			
		for (int i{0}; i != src1_len; i += INCR_BY_16) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch src1,src2,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T1);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T1);
#else
#pragma noprefetch src1,src2,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src1[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src2[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i],      _mm256_pow_pd(_mm256_load_pd(&src1[i]),      _mm256_load_pd(&src2[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_pow_pd(_mm256_load_pd(&src1[i + 4]),  _mm256_load_pd(&src2[i+4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_pow_pd(_mm256_load_pd(&src1[i + 8]),  _mm256_load_pd(&src2[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_pow_pd(_mm256_load_pd(&src1[i + 12]), _mm256_load_pd(&src2[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_pow_pd intrinsic.\n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src1_len << ".\n"
			<< "Unrolled loop iterations: " << src1_len / 4 << ".\n"
			<< "src1 array size:          " << static_cast<double>(src1_len * 8) / 1024.0 << "KiB.\n"
			<< "src2 array size:          " << static_cast<double>(src2_len * 8) / 1024.0 << "KiB. \n"
			<< "dst array size:           " << static_cast<double>(dst_len *  8) / 1024.0 << "KiB.   \n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src1_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned negative value!!\n"
			<< "TSC current read out: " << stop_clock - start_clock << ".\n";
	}
#endif
#else
	
         if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

	          for (int i{ 0 }; i != src1_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src1,src2,dst
		_mm_prefetch(reinterpret_cast<const char*>(&src1[i + 4]), _MM_HINT_T0);
		_mm_prefetch(reinterpret_cast<const char*>(&src2[i + 4]), _MM_HINT_T0);
#endif
		// Safely assume non-aligned memory load-store.
		// On Haswell CPU this should not cause any memory
		// operation penalty.
		_mm256_storeu_pd(&dst[i], _mm256_pow_pd(_mm256_loadu_pd(&src1[i]), _mm256_loadu_pd(&src2[i])));

	}
}
else {

#if defined ForAVXLib_DEBUG_ON
	_ASSERTE((reinterpret_cast<uintptr_t>(src1)& 0x1F) == 0);
	_ASSERTE((reinterpret_cast<uintptr_t>(src2)& 0x1F) == 0)
		_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
	if (((reinterpret_cast<uintptr_t>(src1)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0) ||
		((reinterpret_cast<uintptr_t>(src2)& 0x1F) != 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in pow_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n";
		(reinterpret_cast<uintptr_t>(src1)& 0x1F) == 0 ? std::cout << " src1 aligned on 32-byte boundary.\n" :
			std::cout << " src unaligned on 32-byte boundary.\n";
		(reinterpret_cast<uintptr_t>(src2)& 0x1F) == 0 ? std::cout << " src2 aligned on 32-byte boundary.\n" :
			std::cout << " src unaligned on 32-byte boundary.\n";
		(reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << "  dst aligned on 32-byte boundary.\n" :
			std::cout << " dst unaligned on 32-byte boundary.\n";
		std::cerr << "Cannot recover -- calling exit(-1)!!\n";
		std::exit(-1);
	}
#endif

	for (int i{ 0 }; i != src1_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src1,src2,dst
		_mm_prefetch(reinterpret_cast<const char*>(&src1[i + 4]), _MM_HINT_T0);
		_mm_prefetch(reinterpret_cast<const char*>(&src2[i + 4]), _MM_HINT_T0);
#endif
		// On non Haswell CPU using aligned load-store operations
		// Caller must ensure, that src and dst arrays are aligned
		// on 32-byte boundaries, otherwise #GP error will occure.
		_mm256_store_pd(&dst[i], _mm256_pow_pd(_mm256_load_pd(&src1[i]), _mm256_load_pd(&src2[i])));
	}
}


#endif

}
