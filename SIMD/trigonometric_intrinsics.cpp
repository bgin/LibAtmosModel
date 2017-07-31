
#include "trigonometric_intrinsics.h"
#include "common.h"
#include <cmath>
#include <cfenv>
#include <cerrno>
#include <iostream>
#include <iomanip>


/*
	  @Description: Implementation of 'acos_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  acos_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, 
                     _Inout_ float* __restrict dst,    _In_ const int dst_len,_Inout_ int* fperr) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);
	
#else
	
	check_null_ptr_2args(src, dst, "acos_avx256_ps");
	check_arrays_size(src_len,dst_len,"acos_avx256_ps");
	check_mod8_array_size(src_len, "acos_avx256_ps");
	

#endif
	
	

	constexpr int INCR_BY_8{ 8 }; 
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
	int err = -1;
#define IS_EXCEEDING_L1 (src_len)
	// Error checking code
	// if x < -1 || x > 1 , set fperr to -1 i.e. (Domain Error) and return to the caller.
	is_domain_f32_bt_ones(src,src_len,&err);
	if (err == 1) {
		*fperr = -1;
		return;
	}

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{};
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
			_mm256_storeu_ps(&dst[i], _mm256_acos_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8], _mm256_acos_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_acos_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_acos_ps(_mm256_loadu_ps(&src[i+24])));
		  }
     }
	else {

#if defined ForAVXLib_DEBUG_ON
	
		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);
		
#else
		
		check_32alignment_2args(src, dst, "acos_avx256_ps");
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
			_mm256_store_ps(&dst[i], _mm256_acos_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8], _mm256_acos_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_acos_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_acos_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		dump_timing_stats(start_clock, src_len, src_len / 8, dst_len, 4, stop_clock, ia32_tsc_aux, "_mm256_acos_ps");
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
		       _mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
		// Safely assume non-aligned memory load-store.
		// On Haswell CPU this should not cause any memory
		// operation penalty.
		      _mm256_storeu_ps(&dst[i], _mm256_acos_ps(_mm256_loadu_ps(&src[i])));

	}
}
else {

#if defined ForAVXLib_DEBUG_ON
	_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
	_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
	check_32alignment_2args(src,dst, "acos_avx256_ps");
#endif

	for (int i{ 0 }; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
		_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
		// On non Haswell CPU using aligned load-store operations
		// Caller must ensure, that src and dst arrays are aligned
		// on 32-byte boundaries, otherwise #GP error will occure.
		_mm256_store_ps(&dst[i], _mm256_acos_ps(_mm256_load_ps(&src[i])));
	}
}

#endif

}


/*
	  @Description: Implementation of 'acos_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  acos_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, 
                     _Inout_ double* __restrict dst,    _In_ const int dst_len, _Inout_ int* fperr) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in acos_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in acos_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in acos_avx256_pd!!\n"
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
	
	__m256d v_maskgt(_mm256_setzero_pd());
	__m256d v_masklt(_mm256_setzero_pd());
	const __m256d v_done(_mm256_set1_pd(1.0));
	const __m256d v_dnegone(_mm256_set1_pd(-1.0));
	for (int i{0}; i != src_len; i += INCR_BY_4) {
		v_maskgt = _mm256_cmp_pd(v_done, _mm256_loadu_pd(&src[i]),_CMP_GT_OQ);
		v_masklt = _mm256_cmp_pd(v_dnegone, _mm256_loadu_pd(&src[i]),_CMP_LT_OQ);
		if ((_mm256_testc_pd(v_maskgt, _mm256_setzero_pd())) || 
							(_mm256_testc_pd(v_masklt, _mm256_setzero_pd()))) {
			 *fperr = dom_err;
			 return;
		}
	}

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{};
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
			_mm256_storeu_pd(&dst[i], _mm256_acos_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4], _mm256_acos_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8], _mm256_acos_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_acos_pd(_mm256_loadu_pd(&src[i+12])));

		  }
     }
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);

#else

		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in acos_avx256_pd!!\n"
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
			_mm256_store_pd(&dst[i],      _mm256_acos_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_acos_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_acos_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_acos_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_acos_pd intrinsic.\n"
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
		   _mm256_storeu_pd(&dst[i], _mm256_acos_pd(_mm256_loadu_pd(&src[i])));
    
	   }
  }
   else {

#if defined ForAVXLib_DEBUG_ON
	_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
	_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
	if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in acos_avx256_pd!!\n"
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
		_mm256_store_pd(&dst[i], _mm256_acos_pd(_mm256_load_pd(&src[i])));
	}
}

#endif

}

/*
	  @Description: Implementation of 'acosh_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  acosh_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, 
					  _Inout_ float* __restrict dst, _In_ const int dst_len, _Inout_ int* fperr) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in acosh_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in acosh_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in acosh_avx256_ps!!\n"
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

#pragma STDC FENV_ACCESS ON
	if (math_errhandling & MATH_ERREXCEPT) {
		feclearexcept(FE_ALL_EXCEPT);
	}
	errno = 0;


#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{};
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
			_mm256_storeu_ps(&dst[i],      _mm256_acosh_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_acosh_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_acosh_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_acosh_ps(_mm256_loadu_ps(&src[i+24])));
		}

		if ((math_errhandling & MATH_ERREXCEPT) && errno != 0) {
			std::cerr << "***WARNING***\n"
				      << "Range Exception detected by 'fetestexcpet'!!\n"
				      << "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			        *fperr = rng_err;
		}
		else if ((math_errhandling & MATH_ERREXCEPT) && 
			fetestexcept(FE_INVALID  | FE_INEXACT | 
			             FE_OVERFLOW | FE_UNDERFLOW ) != 0) {
			std::cerr << "***WARNING***\n" 
				      << "Range Exception detected by 'fetestexcept'!!\n"
					  << "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
					*fperr = rng_err;
			}
				
		
    }
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);

#else

		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in acosh_avx256_ps!!\n"
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
			_mm256_store_ps(&dst[i],      _mm256_acosh_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_acosh_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_acosh_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_acosh_ps(_mm256_load_ps(&src[i+24])));
		}



		if ((math_errhandling & MATH_ERREXCEPT) && errno != 0) {
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcept'!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			*fperr = rng_err;
		}
		else if ((math_errhandling & MATH_ERREXCEPT) &&
			fetestexcept(FE_INVALID | FE_DIVBYZERO |
			FE_OVERFLOW | FE_UNDERFLOW) != 0) {
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcept'!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			*fperr = rng_err;
		}

#if HIGH_PRECISION_PERF_MEASURE == 1
		__asm{cpuid}
		unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
		if ((stop_clock - start_clock) > 0ULL) {
			std::cout << "Crude approximation of _mm256_acosh_ps intrinsic.\n"
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
	}

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
		_mm256_storeu_ps(&dst[i], _mm256_acosh_ps(_mm256_loadu_ps(&src[i])));

	}

			if ((math_errhandling & MATH_ERREXCEPT) && errno != 0) {
				std::cerr << "***WARNING***\n"
					<< "Range Exception detected by 'fetestexcpet'!!\n"
					<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
				*fperr = rng_err;
			}
			else if ((math_errhandling & MATH_ERREXCEPT) && 
				fetestexcept(FE_INVALID  | FE_DIVBYZERO | 
				FE_OVERFLOW | FE_UNDERFLOW ) != 0) {
				std::cerr << "***WARNING***\n" 
					<< "Range Exception detected by 'fetestexcept'!!\n"
					<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
				*fperr = rng_err;
			}
}
else {

#if defined ForAVXLib_DEBUG_ON
	_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
	_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
	if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in acosh_avx256_ps!!\n"
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
		_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
		// On non Haswell CPU using aligned load-store operations
		// Caller must ensure, that src and dst arrays are aligned
		// on 32-byte boundaries, otherwise #GP error will occure.
		_mm256_store_ps(&dst[i], _mm256_acosh_ps(_mm256_load_ps(&src[i])));
	}

	if ((math_errhandling & MATH_ERREXCEPT) && errno != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcpet'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
	else if ((math_errhandling & MATH_ERREXCEPT) &&
		fetestexcept(FE_INVALID | FE_DIVBYZERO |
		FE_OVERFLOW | FE_UNDERFLOW) != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcept'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
}
		
#endif

}

/*
	  @Description: Implementation of 'acosh_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  acosh_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, 
                      _Inout_ double* __restrict dst, _In_ const int dst_len, _Inout_ int* fperr) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in acosh_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in acosh_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in acosh_avx256_pd!!\n"
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

#pragma STD FENV_ACCESS ON
	
	if (math_errhandling & MATH_ERREXCEPT) {
		feclearexcept(FE_ALL_EXCEPT);
	}

	errno = 0;

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
			
		for (int i{0}; i != src_len; i += INCR_BY_16 ) {

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
			_mm256_storeu_pd(&dst[i],      _mm256_acosh_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_acosh_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_acosh_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_acosh_pd(_mm256_loadu_pd(&src[i+12])));
		}

		if ((math_errhandling & MATH_ERRNO) && errno != 0) {
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcept'!!\n"
			    << "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
				*fperr = rng_err;
		}
		else if ((math_errhandling & MATH_ERREXCEPT) &&
			       fetestexcept(FE_INVALID | FE_DIVBYZERO |
			                   FE_OVERFLOW | FE_UNDERFLOW) != 0) {
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcept!!'\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
				*fperr = rng_err;
		}
    }
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);

#else

		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in acosh_avx256_pd!!\n"
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
			_mm256_store_pd(&dst[i],      _mm256_acosh_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_acosh_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_acosh_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_acosh_pd(_mm256_load_pd(&src[i+12])));
		}

		if ((math_errhandling & MATH_ERRNO) && errno != 0) {
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcept'!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			*fperr = rng_err;
		}
		else if ((math_errhandling & MATH_ERREXCEPT) &&
			        fetestexcept(FE_INVALID | FE_DIVBYZERO |
			                     FE_OVERFLOW | FE_UNDERFLOW) != 0) {
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcept!!'\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			*fperr = rng_err;
		}

#if HIGH_PRECISION_PERF_MEASURE == 1
		__asm{cpuid}
		unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
		if ((stop_clock - start_clock) > 0ULL) {
			std::cout << "Crude approximation of _mm256_acosh_pd intrinsic.\n"
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
	}

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
		_mm256_storeu_pd(&dst[i], _mm256_acosh_pd(_mm256_loadu_pd(&src[i])));

	}

	if ((math_errhandling & MATH_ERREXCEPT) && errno != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcpet'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
	else if ((math_errhandling & MATH_ERREXCEPT) &&
		        fetestexcept(FE_INVALID | FE_DIVBYZERO |
		                     FE_OVERFLOW | FE_UNDERFLOW) != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcept'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
}
else {

#if defined ForAVXLib_DEBUG_ON
	_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
	_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
	if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in acosh_avx256_pd!!\n"
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
		_mm256_store_pd(&dst[i], _mm256_acosh_pd(_mm256_load_pd(&src[i])));
	}

	if ((math_errhandling & MATH_ERREXCEPT) && errno != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcpet'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
	else if ((math_errhandling & MATH_ERREXCEPT) &&
		fetestexcept(FE_INVALID | FE_DIVBYZERO |
		FE_OVERFLOW | FE_UNDERFLOW) != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcept'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
}
#endif

}

/*
      @Description: Implementation of 'asin_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  asin_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len,
	                _Inout_ float* __restrict dst, _In_ const int dst_len, _Inout_ int* fperr) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in asin_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in asin_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in acosh_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) =" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{4 * INCR_BY_8};
#define IS_EXCEEDING_L1 (src_len)
	// Error checking code
	// if x < -1 || x > 1 , set fperr to -1 i.e. (Domain Error) and return to the caller.
	__m256 v_fmaskgt(_mm256_setzero_ps());
	__m256 v_fmasklt(_mm256_setzero_ps());
	const __m256 v_fone(_mm256_set1_ps(1.F));
	const __m256 v_fnegone(_mm256_set1_ps(-1.F));

	for (int i{ 0 }; i != src_len; i += INCR_BY_8) {

		v_fmaskgt = _mm256_cmp_ps(v_fone, _mm256_loadu_ps(&src[i]), _CMP_GT_OQ);
		v_fmasklt = _mm256_cmp_ps(v_fnegone, _mm256_loadu_ps(&src[i]), _CMP_LT_OQ);
		if ((_mm256_testc_ps(v_fmaskgt, _mm256_setzero_ps())) ||
			            (_mm256_testc_ps(v_fmasklt, _mm256_setzero_ps()))) {

			*fperr = dom_err;
			return;
		}
	}

#pragma STDC FENV_ACCESS ON
	
	if (math_errhandling & MATH_ERREXCEPT) {
		feclearexcept(FE_ALL_EXCEPT);
	}

	errno = 0;

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{};
	__asm{cpuid}
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

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
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_asin_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_asin_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_asin_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_asin_ps(_mm256_loadu_ps(&src[i+24])));
		}

		if ((math_errhandling & MATH_ERRNO) && errno != 0) {
			std::cerr << "***WARNING***\n"
				      << "Range Exception detected by 'fetestexcpet'!!\n"
				      << "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			*fperr = rng_err;
		}
		else if ((math_errhandling & MATH_ERREXCEPT) &&
			      fetestexcept(FE_INVALID | FE_DIVBYZERO |
			             FE_INEXACT | FE_OVERFLOW | FE_UNDERFLOW) != 0) {
			std::cerr << "***WARNING***\n"
				      << "Range Exception detected by 'fetestexcpet'!!\n"
				      << "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			*fperr = rng_err;
		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);
#else
		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in asin_avx256_ps!!\n"
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
#pragma noprefech src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i],      _mm256_asin_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_asin_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_asin_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_asin_ps(_mm256_load_ps(&src[i+24])));
		}

		if ((math_errhandling & MATH_ERRNO) && errno != 0) {
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcpet'!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			*fperr = rng_err;
		}
		else if ((math_errhandling & MATH_ERREXCEPT) &&
			fetestexcept(FE_INVALID | FE_DIVBYZERO |
			FE_INEXACT | FE_OVERFLOW | FE_UNDERFLOW) != 0) {
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcpet'!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			*fperr = rng_err;
		}

#if HIGH_PRECISION_PERF_MEASURE == 1
		__asm{cpuid}
		unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
		if ((stop_clock - start_clock) > 0ULL) {
			std::cout << "Crude approximation of _mm256_asin_ps intrinsic.\n"
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
	}
		
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
		_mm256_storeu_ps(&dst[i], _mm256_asin_ps(_mm256_loadu_ps(&src[i])));

	}

	if ((math_errhandling & MATH_ERREXCEPT) && errno != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcpet'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
	else if ((math_errhandling & MATH_ERREXCEPT) &&
		fetestexcept(FE_INVALID | FE_DIVBYZERO |
		FE_OVERFLOW | FE_UNDERFLOW) != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcept'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
}
else {

#if defined ForAVXLib_DEBUG_ON
	_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
	_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
	if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in asin_avx256_ps!!\n"
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
		_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
#endif
		// On non Haswell CPU using aligned load-store operations
		// Caller must ensure, that src and dst arrays are aligned
		// on 32-byte boundaries, otherwise #GP error will occure.
		_mm256_store_ps(&dst[i], _mm256_asin_ps(_mm256_load_ps(&src[i])));
	}

	if ((math_errhandling & MATH_ERREXCEPT) && errno != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcpet'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
	else if ((math_errhandling & MATH_ERREXCEPT) &&
		fetestexcept(FE_INVALID | FE_DIVBYZERO |
		FE_OVERFLOW | FE_UNDERFLOW) != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcept'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
}

#endif


}

/*
      @Description: Implementation of 'asin_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 53-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  asin_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len,
	                _Inout_ double* __restrict dst, _In_ const int dst_len, _Inout_ int* fperr) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE((src_len == dst_len) && (src_len > 0 && dst_len > 0));
	_ASSERTE((src_len % 4) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in asin_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len != dst_len) || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in asin_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in asin_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) =" << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{4 * INCR_BY_4};
#define IS_EXCEEDING_L1 (src_len)

	__m256d v_dmaskgt(_mm256_setzero_pd());
	__m256d v_dmasklt(_mm256_setzero_pd());
	const __m256d v_done(_mm256_set1_pd(1.0));
	const __m256d v_dnegone(_mm256_set1_pd(-1.0));

	for (int i{ 0 }; i != src_len; i += INCR_BY_4) {

		v_dmaskgt = _mm256_cmp_pd(v_done,    _mm256_loadu_pd(&src[i]), _CMP_GT_OQ);
		v_dmasklt = _mm256_cmp_pd(v_dnegone, _mm256_loadu_pd(&src[i]), _CMP_LT_OQ);
		if ((_mm256_testc_pd(v_dmaskgt, _mm256_setzero_pd())) ||
			              (_mm256_testc_pd(v_dmasklt, _mm256_setzero_pd()))) {
				*fperr = dom_err;
				return;
		}
	}

#pragma STDC FENV_ACCESS ON
		
	if (math_errhandling & MATH_ERREXCEPT) {
		feclearexcept(FE_ALL_EXCEPT);
	}

	errno = 0;

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
			_mm256_storeu_pd(&dst[i],    _mm256_asin_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i+4],  _mm256_asin_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i+8],  _mm256_asin_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i+12], _mm256_asin_pd(_mm256_loadu_pd(&src[i+12])));
		}

		if ((math_errhandling & MATH_ERRNO) && errno != 0) {
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcpet'!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			*fperr = rng_err;
		}
		else if((math_errhandling & MATH_ERREXCEPT) && 
		         fetestexcept(FE_DENORMAL | FE_INEXACT |
							  FE_INVALID  | FE_OVERFLOW | FE_UNDERFLOW) != 0 ) {
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcpet'!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			*fperr = rng_err;
		}
	}
	else {

#if defined ForAVXLib_DEBUG_ON

		_ASSERTE((reinterpret_cast<uintptr_t>(src) & 0x1F) == 0);
		_ASSERTE((reinterpret_cast<uintptr_t>(dst) & 0x1F) == 0);

#else

		if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
			std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in asin_avx256_pd!!\n"
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
			_mm256_store_pd(&dst[i],    _mm256_asin_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i+4],  _mm256_asin_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i+8],  _mm256_asin_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i+12], _mm256_asin_pd(_mm256_load_pd(&src[i+12])));
		}

		if ((math_errhandling & MATH_ERRNO) && errno != 0) {
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcpet'!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			    *fperr = rng_err;
		}
		else if ((math_errhandling & MATH_ERREXCEPT) && 
			              fetestexcept(FE_INVALID | FE_ALL_EXCEPT |
						         FE_INEXACT | FE_OVERFLOW | FE_UNDERFLOW) != 0) {
			
			std::cerr << "***WARNING***\n"
				<< "Range Exception detected by 'fetestexcpet'!!\n"
				<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
			*fperr = rng_err;
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	__asm{cpuid}
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_asin_pd intrinsic.\n"
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
			<< "TSC current read out: " << stop_clock - start_clock << "\n";
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
		_mm256_storeu_pd(&dst[i], _mm256_asin_pd(_mm256_loadu_pd(&src[i])));

	}

	if ((math_errhandling & MATH_ERREXCEPT) && errno != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcpet'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
	else if ((math_errhandling & MATH_ERREXCEPT) &&
		       fetestexcept(FE_INVALID | FE_INEXACT |
		                    FE_OVERFLOW | FE_UNDERFLOW) != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcept'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
}
else {

#if defined ForAVXLib_DEBUG_ON
	_ASSERTE((reinterpret_cast<uintptr_t>(src)& 0x1F) == 0);
	_ASSERTE((reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0);
#else
	if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) || ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in asin_avx256_pd!!\n"
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
		_mm256_store_pd(&dst[i], _mm256_asin_pd(_mm256_load_pd(&src[i])));
	}

	if ((math_errhandling & MATH_ERREXCEPT) && errno != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcpet'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
	else if ((math_errhandling & MATH_ERREXCEPT) &&
		      fetestexcept(FE_INVALID | FE_INEXACT |
	              	      FE_OVERFLOW | FE_UNDERFLOW) != 0) {
		std::cerr << "***WARNING***\n"
			<< "Range Exception detected by 'fetestexcept'!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n";
		*fperr = rng_err;
	}
}


#endif

}