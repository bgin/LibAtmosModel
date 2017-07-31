
#include "spec_funcs_intrinsics.h"
#include <iostream>
#include <iomanip>


/*
	@Description: Stub(empty body) function for manual CPU dispatching
	              Currently supported two types of CPU architecture
				  1) 4th generation Core i7 for non-penalty use of unaligned load-store operations.
				  2) 2nd generation Core i7 for use of aligned load-store operations.
				  Stub for cdfnorm_avx256_ps function.
*/

__declspec(cpu_dispatch(core_4th_gen_avx,core_2nd_gen_avx))
void  cdfnorm_avx256_ps(_In_ const float src[], _In_ const int src_len, _Inout_ float dst[], _In_ const int dst_len) {}

/*
      @Description: Implementation of 'cdfnorm_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
	  @Notice:  Experimental version for Core i7 4th gen CPU
*/

__declspec(cpu_specific(core_4th_gen_avx))
void  cdfnorm_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {


#if defined ForAVXLib_DEBUG_ON
	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 8) == 0);
#else
	if(src == NULL || dst == NULL) {
	   std::cerr << "[" << __DATE__ <<":"<<__TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in cdfnorm_avx256_ps!!\n"
			     << "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			     << "*****ERROR-DETAILS***** \n"
				 << "value of pointer src=" << std::hex << "0x" << src << "\n"
				 << "value of pointer dst=" << std::hex << "0x" << dst << "\n"
				 << "Cannot recover -- calling exit!!";
	   std::exit(-1);
	}

	if (src_len != dst_len || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in cdfnorm_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in cdfnorm_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Modulo of src_len=" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif
	constexpr int EIGHT{ 8 };
	constexpr int UNROLL4{4*EIGHT};
#define IS_EXCEEDING_L1 (src_len)
    
	

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	unsigned __int64 start_clock = __rdtscp(&ia32_tsc_aux);
#endif
		for (int i{ 0 }; i != src_len; i += UNROLL4) {
#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			_mm256_storeu_ps(&dst[i],     _mm256_cdfnorm_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8], _mm256_cdfnorm_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i+16],  _mm256_cdfnorm_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i+24],  _mm256_cdfnorm_ps(_mm256_loadu_ps(&src[i+24])));
		}
#if HIGH_PRECISION_PERF_MEASURE == 1
		
		unsigned __int64 stop_clock = __rdtscp(&ia32_tsc_aux);
		if ((stop_clock - start_clock ) > 0ULL ) {
		      std::cout << "Crude approximation of _mm256_cdfnorm_ps intrinsic. \n"
			            << "--------------- Dumping Stats ---------------- \n"
			            << "----------------------------------------------- \n"
			            << "Loop iterations:          " << src_len << ".\n"
			            << "Unrolled loop iterations: " << src_len / 4 << ".\n"
				        << "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
				        << "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
						<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux <<  ".\n"
				        << "TSC on entry:             " << start_clock << " TSC-cycles. \n"
				        << "TSC on exit:              " << stop_clock  << " TSC-cycles.  \n"
						<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
						<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
						<< "--------------- End of Dump ------------------- \n";
			}
			else {
				std::cerr << "ERROR: RDTSCP returned negative TSC value!! \n"
					      << "TSC current read out = " << stop_clock - start_clock << "\n";
			}

#endif
#else
		for (int i{0}; i  != src_len; i += EIGHT) {
#if defined SOFT_PREFETCH_L1
			_mm_prefetch(reinterpret_cast<const char*>(&src[i+4]), _MM_HINT_T0);
			_mm256_storeu_ps(&dst[i], _mm256_cdfnorm(_mm256_loadu_ps(&src[i])));
#endif
		}
#endif
	
	
}
		

/*
      @Description: Implementation of 'cdfnorm_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
      @Notice:  Experimental version for Core i7 2nd gen CPU.
*/

__declspec(cpu_specific(core_2nd_gen_avx))
void  cdfnorm_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {


#if defined ForAVXLib_DEBUG_ON
	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 8) == 0);
#else
	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Invalid Pointer in cdfnorm_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if (src_len != dst_len || (src_len <= 0 || dst_len <= 0)) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array size mismatch in cdfnorm_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Modulo division error in cdfnorm_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Modulo of src_len=" << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!";
		std::exit(-1);
	}

#endif
	constexpr int EIGHT{ 8 };
	constexpr int UNROLL4{ 4 * EIGHT };
#define IS_EXCEEDING_L1 (src_len)



#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
	unsigned __int64 start_clock = __rdtscp(&ia32_tsc_aux);
#endif
	for (int i{ 0 }; i != src_len; i += UNROLL4) {
#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
		_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
		_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
		_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
		_mm256_store_ps(&dst[i], _mm256_cdfnorm_ps(_mm256_load_ps(&src[i])));
		_mm256_store_ps(&dst[i + 8], _mm256_cdfnorm_ps(_mm256_load_ps(&src[i + 8])));
		_mm256_store_ps(&dst[i + 16], _mm256_cdfnorm_ps(_mm256_load_ps(&src[i + 16])));
		_mm256_store_ps(&dst[i + 24], _mm256_cdfnorm_ps(_mm256_load_ps(&src[i + 24])));
	}
#if HIGH_PRECISION_PERF_MEASURE == 1

	unsigned __int64 stop_clock = __rdtscp(&ia32_tsc_aux);
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_cdfnorm_ps intrinsic. \n"
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
		std::cerr << "ERROR: RDTSCP returned negative TSC value!! \n"
			<< "TSC current read out = " << stop_clock - start_clock << "\n";
	}

#endif
#else
	for (int i{ 0 }; i != src_len; i += EIGHT) {
#if defined SOFT_PREFETCH_L1
		_mm_prefetch(reinterpret_cast<const char*>(&src[i + 4]), _MM_HINT_T0);
		_mm256_store_ps(&dst[i], _mm256_cdfnorm(_mm256_load_ps(&src[i])));
#endif
	}
#endif


}



/*
      @Description: Implementation of 'cdfnorm_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  cdfnorm_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if  defined ForAVXLib_DEBUG_ON
	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 4) == 0);
#else
	
	if(src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Invalid Pointer in cdfnorm_avx256_pd!!\n"
			      << "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			      << "*****ERROR-DETAILS***** \n"
				  << "value of pointer src=" << std::hex << "0x" << src << "\n"
		          << "value of pointer dst=" << std::hex << "0x" << dst << "\n"
				  << "Cannot recover -- calling exit!!\n";
		std::exit(-1);

		
	}

	if (src_len != dst_len || (src_len <= 0 || dst_len )) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Array size mismatch in cdfnorm_avx256_pd!!\n"
			      << "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
				  << "*****ERROR-DETAILS***** \n"
				  << "value of src_len=" << std::dec << src_len << "\n"
				  << "value of dst_len=" << dst_len << "\n"
				  << "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Modulo division error in cdfnorm_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) = " << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}
#endif

	constexpr int FOURTH{ 4 };
	constexpr int UNROLL4{ 4 * FOURTH };
#define IS_EXCEEDING_L1 (src_len)



#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	unsigned __int64 start_clock = __rdtscp(&ia32_tsc_aux);

#endif
		for (int i{ 0 }; i != src_len; i += UNROLL4) {
#if  defined SOFT_PREFETCH_L1 && \
 IS_EXCEEDING_L1 > L1_MAX_DP
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			_mm256_storeu_pd(&dst[i],    _mm256_cdfnorm_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i+4],  _mm256_cdfnorm_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i+8],  _mm256_cdfnorm_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i+12], _mm256_cdfnorm_pd(_mm256_loadu_pd(&src[i+12])));
		}
#if HIGH_PRECISION_PERF_MEASURE == 1

		unsigned __int64 stop_clock = __rdtscp(&ia32_tsc_aux);
		if ((stop_clock - start_clock) > 0ULL ) {
			std::cout << "Crude approximation of _mm256_cdfnorm_pd intrinsic.\n"
				      << "---------------- Dumping Stats -------------------- \n"
					  << "---------------------------------------------------  \n"
					  << "Loop iterations:          " << src_len <<     ".\n"
					  << "Unrolled loop iterations: " << src_len / 4 << ".\n"
					  << "src array size =          " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
					  << "dst array size =          " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB. \n"
					  << "IA32_TSC_AUX MSR=         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
					  << "TSC on entry:             " << start_clock << " TSC-cycles. \n"
					  << "TSC on  exit:             " << stop_clock  << " TSC-cycles. \n"
					  << "TSC Delta:                " << stop_clock - start_clock << " TSC-cycles. \n"
					  << "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles. \n"
					  << "--------------- End of Dump ------------------------   \n";



		}
		else {
			std::cerr << "ERROR: TSC returned negative result!! \n"
				      << " TSC current read out = " << stop_clock - start_clock << " TSC-cycles. \n";
		}


#endif
#else

		for (int i{ 0 }; i != src_len; i += FOURTH) {
#if defined SOFT_PREFETCH_L1
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			_mm256_storeu_pd(&dst[i], _mm256_cdfnorm_pd(_mm256_loadu_pd(&src[i])));

		}
#endif
	
	
}


/*
	  @Description: Implementation of 'cdfnorminv_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  cdfnorminv_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src_len != NULL && dst_len != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 8) == 0);
#else

	if (src_len == NULL || dst_len == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Invalid Pointer in cdfnorminv_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if (src_len != dst_len) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Array size mismatch in cdfnorminv_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" <<  src_len << "\n"
			<< "value of dst_len=" <<  dst_len << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Modulo division error in cdfnorminv_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) = " << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{0};
	unsigned __int64 start_clock{__rdtscp(&ia32_tsc_aux)};

#endif
	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

	    for (int i{ 0 }; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch
		_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
		_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch
		_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
		// Safely assume non-aligned memory load-store.
		// On Haswell CPU this should not cause any memory
		// operation penalty.
		_mm256_storeu_ps(&dst[i],      _mm256_cdfnorminv_ps(_mm256_loadu_ps(&src[i])));
		_mm256_storeu_ps(&dst[i + 8],  _mm256_cdfnorminv_ps(_mm256_loadu_ps(&src[i + 8])));
		_mm256_storeu_ps(&dst[i + 16], _mm256_cdfnorminv_ps(_mm256_loadu_ps(&src[i + 16])));
		_mm256_storeu_ps(&dst[i + 24], _mm256_cdfnorminv_ps(_mm256_loadu_ps(&src[i + 24])));
	}
 }
	else {
		for (int i{0}; i != src_len; i += INCR_BY_32) {

#if defined  SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);

#else
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_ps(&dst[i],      _mm256_cdfnorminv_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_cdfnorminv_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_cdfnorminv_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_cdfnorminv_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1

	unsigned __int64 stop_clock{__rdtscp(&ia32_tsc_aux)};
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_cdfnorminv_ps intrinsic. \n"
			      << "--------------- Dumping Stats ---------------- \n"
			      << "----------------------------------------------- \n"
			      << "Loop iterations:          " << src_len << ".\n"
			      << "Unrolled loop iterations: " << src_len / 8 << ".\n"
			      << "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			      << "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			      << "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux <<  ".\n"
			      << "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			      << "TSC on exit:              " << stop_clock  << " TSC-cycles.  \n"
			      << "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			      << "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			      << "--------------- End of Dump ------------------- \n";
	}
	else {

		std::cerr << "ERROR: TSC returned a negative value!! \n"
			      << "TSC current read out = " << stop_clock - start_clock << " TSC-cycles. \n";
	}

#endif
#else
	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

	   for (int i{ 0 }; i != src_len; i += INCR_BY_8) {

#if defined SOFT_PREFETCH_L1
		_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
		// Safely assume non-aligned memory load-store.
		_mm256_storeu_ps(&dst[i], _mm256_cdfnorminv_ps(_mm256_loadu_ps(&src[i])));
	   }
	}
	  else {
		
		  for (int i{0}; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
		 _mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Use aligned memory load-store intrinsics.
		 _mm256_store_ps(&dst[i], _mm256_cdfnorminv_ps(_mm256_load_ps(&src[i])));
		  }
	  }

#endif
	


}

/*
      @Description: Implementation of 'cdfnorminv_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  cdfnorminv_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 4) != 0);
#else
	
	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Invalid Pointer in cdfnorminv_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if (src_len != dst_len) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Array size mismatch in cdfnorminv_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Modulo division error in cdfnorminv_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) = " << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };

#endif

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {

	    for(int i{0}; i != src_len; i += INCR_BY_16) {
#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch
		_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
		_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch
		_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif

		// Safely use unaligned load-store should not have 
		// any impact since Haswell March.
		_mm256_storeu_pd(&dst[i],       _mm256_cdfnorminv_pd(_mm256_loadu_pd(&src[i])));
		_mm256_storeu_pd(&dst[i + 4],   _mm256_cdfnorminv_pd(_mm256_loadu_pd(&src[i+4])));
		_mm256_storeu_pd(&dst[i + 8],   _mm256_cdfnorminv_pd(_mm256_loadu_pd(&src[i+8])));
		_mm256_storeu_pd(&dst[i + 12],  _mm256_cdfnorminv_pd(_mm256_loadu_pd(&src[i+12])));
	   }
	}
	else {
		
		for (int i{0}; i != src_len; i += INCR_BY_16) {
#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_DP
#pragma noprefetch
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			
			// On non Haswell CPU using aligned load-store
			// memory operations in order to increase performance.
			_mm256_store_pd(&dst[i],       _mm256_cdfnorminv_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],   _mm256_cdfnorminv_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8],   _mm256_cdfnorminv_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12],  _mm256_cdfnorminv_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1

	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_cdfnorminv_pd intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 4)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {

		std::cerr << "ERROR: TSC returned a negative value!! \n"
			      << "TSC current read out = " << stop_clock - start_clock << " TSC-cycles. \n";
	}


#endif
#else
	
	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {

	    for (int i{ 0 }; i != src_len; i += INCR_BY_4) {

#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
		_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
		// Safely assume non-aligned memory load-store.
		_mm256_storeu_pd(&dst[i], _mm256_cdfnorminv_pd(_mm256_loadu_pd(&src[i])));
	   }
  }
   else {
		
	   for (int i{0}; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
		   _mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			
		   _mm256_store_pd(&dst[i], _mm256_cdfnorminv_pd(_mm256_load_pd(&src[i])));
	   }
   }
#endif

}


/*
	  @Description: Implementation of 'erf_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  erf_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 8) == 0);
#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Invalid Pointer in erf_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if (src_len != dst_len) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Array size mismatch in erf_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Modulo division error in erf_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) = " << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };

#endif
	
	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for(int i{0}; i != src_len; i += INCR_BY_32) {

#if defined SOFT_PREFETCH_L1 && \
	IS_EXCEEDING_L1 > L1_MAX_SP
#pragma noprefetch src, dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T1);
#else
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i],      _mm256_erf_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_erf_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_erf_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_erf_ps(_mm256_loadu_ps(&src[i+24])));
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
			_mm256_store_ps(&dst[i],      _mm256_erf_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_erf_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_erf_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_erf_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1

	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_erf_ps intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
			<< "IA32_TSC_AUX MSR:         " << std::hex << "0x" << ia32_tsc_aux << ".\n"
			<< "TSC on entry:             " << start_clock << " TSC-cycles. \n"
			<< "TSC on exit:              " << stop_clock << " TSC-cycles.  \n"
			<< "TSC Delta:                " << stop_clock - start_clock << "cycles. \n"
			<< "TSC per loop cycle:       " << (static_cast<double>(stop_clock - start_clock) / (src_len / 8)) << " TSC-cycles.\n"
			<< "--------------- End of Dump ------------------- \n";
	}
	else {
		std::cerr << "ERROR: RDTSCP returned anegative value!!\n"
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
			_mm256_storeu_ps(&dst[i], _mm256_erf_ps(_mm256_loadu_ps(&src[i])));
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
			_mm256_store_ps(&dst[i], _mm256_erf_ps(_mm256_load_ps(&src[i])));
		}
	}


#endif



}


/*
      @Description: Implementation of 'erf_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  erf_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE(src_len != dst_len);
	_ASSERTE((src_len % 4) == 0);
#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Invalid Pointer in erf_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if (src_len != dst_len) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Array size mismatch in erf_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Modulo division error in erf_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) = " << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
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
			_mm256_storeu_pd(&dst[i],      _mm256_erf_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_erf_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_erf_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_erf_pd(_mm256_loadu_pd(&src[i+12])));
		}
	}
	else {

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
			_mm256_store_pd(&dst[i], _mm256_erf_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4], _mm256_erf_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8], _mm256_erf_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_erf_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_erf_pd intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
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
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_erf_pd(_mm256_loadu_pd(&src[i])));
		}
	}
	else {
		for (int i{0}; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_erf_pd(_mm256_load_pd(&src[i])));
		}
	}

#endif

}

/*
      @Description: Implementation of 'erfc_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  erfc_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 8) == 0);
#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Invalid Pointer in erfc_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if (src_len != dst_len) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Array size mismatch in erfc_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Modulo division error in erfc_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) = " << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
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
			_mm256_storeu_ps(&dst[i],      _mm256_erfc_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_erfc_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_erfc_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_erfc_ps(_mm256_loadu_ps(&src[i+24])));
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
			_mm256_store_ps(&dst[i],      _mm256_erfc_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_erfc_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_erfc_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_erfc_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL ) {
		std::cout << "Crude approximation of _mm256_erfc_ps intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 8 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 8) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 8) / 1024.0 << "KiB.\n"
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

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
		for (int i{0}; i != src_len; i += INCR_BY_8) {

#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_erfc_ps(_mm256_loadu_ps(&src[i])));
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
		  _mm256_store_ps(&dst[i], _mm256_erfc_ps(_mm256_load_ps(&src[i])));
	  }
  }

#endif

}

/*
      @Description: Implementation of 'erfc_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  erfc_avx256_ps(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 4) == 0);
#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Invalid Pointer in erfc_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if (src_len != dst_len) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Array size mismatch in erfc_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Modulo division error in erfc_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) = " << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
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
			_mm256_storeu_pd(&dst[i],      _mm256_erfc_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_erfc_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_erfc_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_erfc_pd(_mm256_loadu_pd(&src[i+12])));
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
			_mm256_store_pd(&dst[i],      _mm256_erfc_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_erfc_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_erfc_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_erfc_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_erfc_pd intrinsic. \n"
			<< "--------------- Dumping Stats ---------------- \n"
			<< "----------------------------------------------- \n"
			<< "Loop iterations:          " << src_len << ".\n"
			<< "Unrolled loop iterations: " << src_len / 4 << ".\n"
			<< "src array size:           " << static_cast<double>(src_len * 4) / 1024.0 << "KiB.\n"
			<< "dst array size:           " << static_cast<double>(dst_len * 4) / 1024.0 << "KiB.\n"
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

		for(int i{0}; i != src_len; i += INCR_BY_4 ) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i], _mm256_erfc_pd(_mm256_loadu_pd(&src[i])));
		}
	}
	else {
		
		for (int i{0}; i != src_len; i += INCR_BY_4) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// On non Haswell CPU using aligned load-store operations
			// Caller must ensure, that src and dst arrays are aligned
			// on 32-byte boundaries, otherwise #GP error will occure.
			_mm256_store_pd(&dst[i], _mm256_erfc_pd(_mm256_load_pd(&src[i])));
		}
	}
#endif

}

/*
	  @Description: Implementation of 'erfcinv_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  erfcinv_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 8) == 0);

#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Invalid Pointer in erfcinv_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if (src_len != dst_len) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Array size mismatch in erfcinv_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Modulo division error in erfcinv_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) = " << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}
#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
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
			_mm256_storeu_ps(&dst[i],      _mm256_erfcinv_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],  _mm256_erfcinv_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16], _mm256_erfcinv_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24], _mm256_erfcinv_ps(_mm256_loadu_ps(&src[i+24])));
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
			_mm256_store_ps(&dst[i],      _mm256_erfcinv_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_erfcinv_ps(_mm256_load_ps(&src[i+8])));
			_mm256_store_ps(&dst[i + 16], _mm256_erfcinv_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_erfcinv_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		 std::cout << "Crude approximation of _mm256_erfcinv_ps intrinsic. \n"
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

		for (int i{0}; i != src_len; i += INCR_BY_8 ) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_erfcinv_ps(_mm256_loadu_ps(&src[i])));
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
			_mm256_store_ps(&dst[i], _mm256_erfcinv_ps(_mm256_load_ps(&src[i])));
		}
	}

#endif
}

/*
	  @Description: Implementation of 'erfcinv_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  erfcinv_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 4) == 0);
#else

	if (src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Invalid Pointer in erfcinv_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if (src_len != dst_len) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Array size mismatch in erfcinv_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Modulo division error in erfcinv_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) = " << (src_len % 4) << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{ 4 };
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
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
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_pd(&dst[i],      _mm256_erfcinv_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_erfcinv_pd(_mm256_loadu_pd(&src[i + 4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_erfcinv_pd(_mm256_loadu_pd(&src[i + 8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_erfcinv_pd(_mm256_loadu_pd(&src[i + 12])));
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
			_mm256_store_pd(&dst[i],      _mm256_erfcinv_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_erfcinv_pd(_mm256_load_pd(&src[i + 4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_erfcinv_pd(_mm256_load_pd(&src[i + 8])));
			_mm256_store_pd(&dst[i + 12], _mm256_erfcinv_pd(_mm256_load_pd(&src[i + 12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		std::cout << "Crude approximation of _mm256_erfcinv_pd intrinsic. \n"
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
		std::cerr << "ERROR: RDTSC returned negative value!!\n"
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
			_mm256_storeu_pd(&dst[i], _mm256_erfcinv_pd(_mm256_loadu_pd(&src[i])));
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
			_mm256_store_pd(&dst[i], _mm256_erfcinv_pd(_mm256_load_pd(&src[i])));
		}
	}

#endif

}

/*
      @Description: Implementation of 'erfinv_avx256_ps' void function.

      @Params:  source array of floats (32-bit, 24-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  erfinv_avx256_ps(_In_ const float* __restrict src, _In_ const int src_len, _Inout_ float* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL || dst != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 8) == 0);
#else

	if(src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Invalid Pointer in erfinv_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if (src_len != dst_len) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Array size mismatch in erfinv_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if ((src_len % 8) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Modulo division error in erfinv_avx256_ps!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,8) = " << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_8{ 8 };
	constexpr int INCR_BY_32{ 4 * INCR_BY_8 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if (_may_i_use_cpu_feature(_FEATURE_FMA)) {
		
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
			_mm256_storeu_ps(&dst[i],       _mm256_erfinv_ps(_mm256_loadu_ps(&src[i])));
			_mm256_storeu_ps(&dst[i + 8],   _mm256_erfinv_ps(_mm256_loadu_ps(&src[i+8])));
			_mm256_storeu_ps(&dst[i + 16],  _mm256_erfinv_ps(_mm256_loadu_ps(&src[i+16])));
			_mm256_storeu_ps(&dst[i + 24],  _mm256_erfinv_ps(_mm256_loadu_ps(&src[i+24])));
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
			_mm256_store_ps(&dst[i],      _mm256_erfinv_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 8],  _mm256_erfinv_ps(_mm256_load_ps(&src[i])));
			_mm256_store_ps(&dst[i + 16], _mm256_erfinv_ps(_mm256_load_ps(&src[i+16])));
			_mm256_store_ps(&dst[i + 24], _mm256_erfinv_ps(_mm256_load_ps(&src[i+24])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		  std::cout << "Crude approximation of _mm256_erfinv_ps intrinsic. \n"
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
		
		for (int i{0}; i != src_len; i += INCR_BY_8) {
#if defined SOFT_PREFETCH_L1
#pragma noprefetch src,dst
			_mm_prefetch(reinterpret_cast<const char*>(&src[i]), _MM_HINT_T0);
#endif
			// Safely assume non-aligned memory load-store.
			// On Haswell CPU this should not cause any memory
			// operation penalty.
			_mm256_storeu_ps(&dst[i], _mm256_erfinv_ps(_mm256_loadu_ps(&src[i])));
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
			_mm256_store_ps(&dst[i], _mm256_erfinv_ps(_mm256_load_ps(&src[i])));
		}
	}


#endif

}

/*
      @Description: Implementation of 'erfinv_avx256_pd' void function.

      @Params:  source array of doubles (64-bit, 56-bit precision)
      @Params:  length of source array

      @Params:  destination array assumed to hold doubles (64-bit, 56-bit precision)
      @Params:  length of destination array
      @Returns: Nothing
      @Throws:  Nothing
*/

void  erfinv_avx256_pd(_In_ const double* __restrict src, _In_ const int src_len, _Inout_ double* __restrict dst, _In_ const int dst_len) {

#if defined ForAVXLib_DEBUG_ON

	_ASSERTE(src != NULL && dst != NULL);
	_ASSERTE(src_len == dst_len);
	_ASSERTE((src_len % 4) == 0);
#else

	if(src == NULL || dst == NULL) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Invalid Pointer in erfinv_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of pointer src=" << std::hex << "0x" << src << "\n"
			<< "value of pointer dst=" << std::hex << "0x" << dst << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if (src_len != dst_len) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Array size mismatch in erfinv_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "value of src_len=" << src_len << "\n"
			<< "value of dst_len=" << dst_len << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

	if ((src_len % 4) != 0) {
		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "*****FATAL-ERROR*****: Modulo division error in erfinv_avx256_pd!!\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n"
			<< "Mod(src_len,4) = " << (src_len % 8) << "\n"
			<< "Cannot recover -- calling exit!!\n";
		std::exit(-1);
	}

#endif

	constexpr int INCR_BY_4{4};
	constexpr int INCR_BY_16{ 4 * INCR_BY_4 };
#define IS_EXCEEDING_L1 (src_len)

#if defined USE_MANUAL_UNROLLING
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned int ia32_tsc_aux{ 0 };
	unsigned __int64 start_clock{ __rdtscp(&ia32_tsc_aux) };
#endif

	if(_may_i_use_cpu_feature(_FEATURE_FMA)) {

		for(int i{0}; i != src_len; i += INCR_BY_16 ) {

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
			_mm256_storeu_pd(&dst[i],      _mm256_erfinv_pd(_mm256_loadu_pd(&src[i])));
			_mm256_storeu_pd(&dst[i + 4],  _mm256_erfinv_pd(_mm256_loadu_pd(&src[i+4])));
			_mm256_storeu_pd(&dst[i + 8],  _mm256_erfinv_pd(_mm256_loadu_pd(&src[i+8])));
			_mm256_storeu_pd(&dst[i + 12], _mm256_erfinv_pd(_mm256_loadu_pd(&src[i+12])));
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
			_mm256_store_pd(&dst[i],      _mm256_erfinv_pd(_mm256_load_pd(&src[i])));
			_mm256_store_pd(&dst[i + 4],  _mm256_erfinv_pd(_mm256_load_pd(&src[i+4])));
			_mm256_store_pd(&dst[i + 8],  _mm256_erfinv_pd(_mm256_load_pd(&src[i+8])));
			_mm256_store_pd(&dst[i + 12], _mm256_erfinv_pd(_mm256_load_pd(&src[i+12])));
		}
	}
#if HIGH_PRECISION_PERF_MEASURE == 1
	unsigned __int64 stop_clock{ __rdtscp(&ia32_tsc_aux) };
	if ((stop_clock - start_clock) > 0ULL) {
		  std::cout << "Crude approximation of _mm256_erfinv_pd intrinsic. \n"
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
			_mm256_storeu_pd(&dst[i], _mm256_erfinv_pd(_mm256_loadu_pd(&src[i])));
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
			_mm256_store_pd(&dst[i], _mm256_erfinv_pd(_mm256_load_pd(&src[i])));
		}
	}

#endif

}