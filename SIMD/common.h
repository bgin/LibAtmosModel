
#ifndef __COMMON_H__
#define __COMMON_H__

#if !defined (COMMON_VERSION_MAJOR)
#define COMMON_VERSION_MAJOR 1
#endif


#if !defined (COMMON_VERSION_MINOR)
#define COMMON_VERSION_MINOR 0
#endif

#if !defined (COMMON_VERSION_MICRO)
#define COMMON_VERSION_MICRO 0
#endif

// File version computed as follows: 1000 * version_major value + 100 * version_minor + 10 * version_micro.
#if !defined (COMMON_FILE_VERSION)
#define COMMON_FILE_VERSION 1000
#endif

#if !defined (COMMON_CREATE_DATE)
#define COMMON_CREATE_DATE "Date: 08-04-2017 08 Apr 2017 , Time: 11:44 AM GMT+2 -200"
#endif

// Set this macro to latest successful build date/time.
#if !defined (COMMON_BUILD_DATE)
#define COMMON_BUILD_DATE ""
#endif

#if !defined (COMMON_AUTHOR)
#define COMMON_AUTHOR "Programmer: Bernard Gingold , contact: beniekg@gmail.com"
#endif

// File version , date of build/creation and author data auxiliary functions.

#include <type_traits>


extern "C" {

	// common.h major version
	unsigned int get_common_major_version();

	// common.h minor version
	unsigned int get_common_minor_version();

	// common.h micro version
	unsigned int get_common_micro_version();

	// This file (common.h) version
	unsigned int get_common_file_version();

	// common.h build date
	const char * get_common_build_date();

	// common.h creation date
	const char * get_common_create_date();

	// common.h author
	const char * get_common_file_author();


	
}

	
         /* 
		  *   Declaration of various error condition checking and error raising helpers.
		  *   These functions are mangled and hence not callable from Fortran side.
		 */


      /*
        @Description: Checks for C-style array's NULL pointer.
                      Calls exit(-1) upon detecting NULL pointer.
        @Params:  source array of floats (32-bit, 24-bit precision)
        @Params:  none

        @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
     */

void  check_null_ptr_2args( const float* __restrict, float* __restrict, const char * );

		
     /*
        @Description: Checks for C-style array's NULL pointer.
                      Calls exit(-1) upon detecting NULL pointer.
        @Params:  source array of doubles (64-bit, 53-bit precision)
        @Params:  none

        @Params:  destination array assumed to hold doubles (64-bit, 53-bit precision)
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
     */
	
void  check_null_ptr_2args( const double* __restrict, double* __restrict, const char *);

	 
     /*
        @Description: Checks for C-style array's NULL pointer.
                      Calls exit(-1) upon detecting NULL pointer.
        @Params:  1st source array of floats (32-bit, 24-bit precision)
        @Params:  2nd source array of floats (32-bit, 24-bit precision)

        @Params:  destination array assumed to hold floats (32-bit, 24-bit precision)
        @Params:  executing function name, const char *
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
     */

void  check_null_ptr_3args( const float* __restrict, const float* __restrict, 
                                   float* __restrict, const char *          );

	 
     /*
        @Description: Checks for C-style array's NULL pointer.
                      Calls exit(-1) upon detecting NULL pointer.
        @Params:  1st source array of doubles (64-bit, 53-bit precision)
        @Params:  2nd source array of doubles (64-bit, 53-bit precision)

        @Params:  destination array assumed to hold doubles (64-bit, 53-bit precision)
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
     */

void  check_null_ptr_3args( const double* __restrict, const double* __restrict, 
                                    double* __restrict, const char *           );


    /*
        @Description: Checks for C-style array's NULL pointer.
                      Calls exit(-1) upon detecting NULL pointer.
        @Params:  source array of 32-bit signed integers
        @Params:  none

        @Params:  destination array assumed to hold 32-bit signed integers
        @Params:  const char * func_name , pointer to function name
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
     */

void  check_null_ptr_2args( const int* __restrict, int* __restrict, const char *);

	
     /*
        @Description: Checks for C-style array's NULL pointer.
                      Calls exit(-1) upon detecting NULL pointer.
        @Params:  source array of 64-bit signed integers
        @Params:  none

        @Params:  destination array assumed to hold 64-bit signed integers
        @Params:  const char * func_name , pointer to function name
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
*/

void  check_null_ptr_2args( const long long* __restrict, long long* __restrict, const char *);

		
     /*
        @Description: Checks for C-style array's NULL pointer.
                      Calls exit(-1) upon detecting NULL pointer.
        @Params:  1st source array of signed integers (32-bit)
        @Params:  2nd source array of signed integers (32-bit)

        @Params:  destination array assumed to hold signed integers (32-bit)
        @Params:  executing function name, const char *
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
     */

void  check_null_ptr_3args(const int* __restrict, const int* __restrict, 
                                 int* __restrict, const char *          );


     /*
        @Description: Checks for C-style array's NULL pointer.
                      Calls exit(-1) upon detecting NULL pointer.
        @Params:  1st source array of signed integers (64-bit)
        @Params:  2nd source array of signed integers (64-bit)

        @Params:  destination array assumed to hold signed integers (64-bit)
        @Params:  executing function name, const char *
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
     */

void  check_null_ptr_3args(const long long* __restrict, const long long* __restrict,
								const long long* __restrict, const char *          );



	 /*
        @Description: Checks for source/destination arrays length mismatch.
					  Checks for src/dst length arguments being less/or zero.
                      Calls exit(-1) upon detecting error condition.
        @Params:  source array length: 32-bit signed integer
        @Params:  destination array length: 32-bit signed integer

        @Params:  executing(calling) function name, const char *
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
     */

void  check_arrays_size(const int, const int, const char *);


     /*
        @Description: Checks for Mod(src_len,4).
                      Arrays must have number of elements divisable by 4.
                      Calls exit(-1) upon detecting error condition i.e. Mod(src_len,4) != 0
        @Params:  source array length: 32-bit signed int
        @Params:  none

        @Params:  executing(calling) function name, const char *
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
    */
	
void  check_mod4_array_size(const int size, const char * );


	 /*
        @Description: Checks for Mod(src_len,8).
                      Arrays must have number of elements divisable by 8.
                      Calls exit(-1) upon detecting error condition i.e. Mod(src_len,8) != 0
        @Params:  source array length: 32-bit signed int
        @Params:  none

        @Params:  executing(calling) function name, const char *
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
	 */

void  check_mod8_array_size(const int size, const char *);

	
     /*
        @Description: Checks 32-byte pointer alignment.
                      
                      Calls exit(-1) upon detecting error condition i.e. ((T * src) & 0x1F) != 0
		@Params:  Pointer of type Ptr * to source array
        @Params:  Pointer of type Ptr * to destination array

        @Params:  executing(calling) function name, const char *
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
     */

template<typename Ptr>  
std::enable_if<std::is_pointer<Ptr>::value,void>
check_32alignment_2args(_In_ const Ptr* __restrict src, 
						_In_ const Ptr* __restrict dst, _In_ const char * func_name) {

	 if (((reinterpret_cast<uintptr_t>(src)& 0x1F) != 0) ||
				    ((reinterpret_cast<uintptr_t>(dst)& 0x1F) != 0)) {

		 std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in" << func_name << "\n"
			 << "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			 << "*****ERROR-DETAILS***** \n";
		   (reinterpret_cast<uintptr_t>(src)& 0x1F) == 0 ? std::cout << " src aligned on 32-byte boundary.\n" :
			 std::cout << " src unaligned on 32-byte boundary.\n";
		   (reinterpret_cast<uintptr_t>(dst)& 0x1F) == 0 ? std::cout << " dst aligned on 32-byte boundary.\n" :
			 std::cout << " dst unaligned on 32-byte boundary.\n";
		     std::cerr << "Cannot recover -- calling exit(-1)!!\n";
		 std::exit(-1);
	}
}


	 /*
        @Description: Checks 32-byte pointer alignment.

                      Calls exit(-1) upon detecting error condition i.e. ((T * src) & 0x1F) != 0
        @Params:  Pointer of type Ptr * to 1st source array
        @Params:  Pointer of type Ptr * to 2nd source array
		@Params:  Pointer of type Ptr * to destination array
        @Params:  executing(calling) function name, const char *
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   std::exit(-1)
     */

template<typename Ptr>
std::enable_if<std::is_pointer<Ptr>::value,void> 
check_32alignment_3args(_In_ const Ptr* __restrict src1_ptr, _In_ const Ptr* __restrict src2_ptr,
						_In_ Ptr* __restrict dst_ptr, _In_ const char * func_name) {
	
	if (((reinterpret_cast<uintptr_t>(src1_ptr)& 0x1F) != 0) ||
		     ((reinterpret_cast<uintptr_t>(src2_ptr)& 0x1F) != 0) ||
		          ((reinterpret_cast<uintptr_t>(dst_ptr)& 0x1F) != 0)) {

		std::cerr << "[" << __DATE__ << ":" << __TIME__ << "]" << "***FATAL-ERROR***: Array(s) unaligned on 32-byte boundary in" << func_name << "\n"
			<< "at " << __FILE__ << ":" << __LINE__ << "(" << std::hex << "0x" << __FUNCTIONW__ << ")" << "\n"
			<< "*****ERROR-DETAILS***** \n";
		(reinterpret_cast<uintptr_t>(src1_ptr)& 0x1F) == 0 ? std::cout << " src1_ptr aligned on 32-byte boundary.\n" :
			std::cout << " src1_ptr unaligned on 32-byte boundary.\n";
		(reinterpret_cast<uintptr_t>(src2_ptr)& 0x1F) == 0 ? std::cout << " src2_ptr aligned on 32-byte boundary.\n" :
			std::cout << " src2_ptr unaligned on 32-byte boundary.\n";
		(reinterpret_cast<uintptr_t>(dst_ptr)& 0x1F) == 0 ? std::cout << " dst_ptr aligned on 32-byte boundary.\n" :
			std::cout << " dst_ptr unaligned on 32-byte boundary.\n";
		std::cerr << "Cannot recover -- calling exit(-1)!!\n";
		std::exit(-1);
	}
}

	
	 /*
		@Description: Dumps timing stats.

                      
        @Params:  TSC start value (wall clock)
        @Params:  TSC stop  value (wall clock)
        @Params:  Number of loop iterations
        @Params:  executing(calling) function name, const char *
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  dump_timing_stats(_In_ const unsigned __int64, _In_ const int,
						_In_ const int, _In_ const int, _In_ const int,
                        _In_ const unsigned __int64, _In_ const int, _In_ const char * );



     /*
        @Description: Checks for input domain of x >= 0.0
					  Upon detecting error condition 'domain range'
                      indicator is set to true (1).
					  Manually vectorised version.
        @Params:  source array holding floats (32-bit)
        @Params:  lenght of source array
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Params:  none
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f32_lt_zero(_In_ const float* __restrict, 
						    _In_ const int, _Inout_ int* );


    /*
        @Description: Checks for input domain of x >= 0.0
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  1st source array holding floats (32-bit)
        @Params:  2nd source array holding floats (32-bit)
        @Params:  length of both source arrays.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f32_lt_zero(_In_ const float* __restrict, _In_ const float* __restrict,
							_In_ const int,               _Inout_  int *             );


    /*
        @Description: Checks for input domain of x >= 0.0
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  source array holding floats (64-bit)
        @Params:  lenght of source array
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Params:  none
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f64_lt_zero(_In_ const double* __restrict,
	                        _In_ const int,   _Inout_ int *); 


	 
     /*
        @Description: Checks for input domain of x >= 0.0
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  1st source array holding doubles (64-bit)
        @Params:  2nd source array holding doubles (64-bit)
        @Params:  length of both source arrays.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f64_lt_zero(_In_ const double* __restrict, _In_ const double* __restrict,
							_In_ const int,                _Inout_  int *               );


	  
     /*
        @Description: Checks for input domain of -1.0F <= x <= 1.0F
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  source array holding float (32-bit)
        @Params:  lenght of source array
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Params:  none
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
	 */

void  is_domain_f32_bt_ones(_In_ const float* __restrict, 
							_In_ const int, _Inout_ int *);


     /*
        @Description: Checks for input domain of -1.0F <= 0 <= 1.0F
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
       @Params:  1st source array holding doubles (32-bit)
       @Params:  2nd source array holding doubles (32-bit)
       @Params:  length of both source arrays.
       @Params:  none
       @Params:  pointer to variable holding 'Domain Range'error value.
       @Returns: Nothing
       @Throws:  Nothing
       @Calls:   Nothing
     */

void  is_domain_f32_bt_ones(_In_ const float* __restrict, _In_ const float* __restrict,
							_In_ const int ,              _Inout_  int*             );



	 /*
        @Description: Checks for input domain of -1.0F <= x <= 1.0F
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  source array holding doubles (64-bit)
        @Params:  lenght of source array
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Params:  none
        @Params:  none
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f64_bt_ones(_In_ const double* __restrict, 
							_In_ const int,    _Inout_ int* );


    /*
        @Description: Checks for input domain of -1.0F <= 0 <= 1.0F
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  1st source array holding doubles (64-bit)
        @Params:  2nd source array holding doubles (64-bit)
        @Params:  length of both source arrays.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f64_bt_ones(_In_ const double* __restrict, _In_ const double* __restrict,
						    _In_ const int,                _Inout_  int*               );


     /*
        @Description: Checks for input domain of  x != 0 && y != 0
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  1st source array holding floats (32-bit)
        @Params:  2nd source array holding floats (32-bit)
        @Params:  length of both source arrays.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f32_ne_zero(_In_ const float* __restrict, _In_ const float* __restrict,
						    _In_ const int ,              _Inout_  int*              );


     /*
        @Description: Checks for input domain of  x != 0 && y != 0
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  1st source array holding doubles (64-bit)
        @Params:  2nd source array holding doubles (64-bit)
        @Params:  length of both source arrays.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
	 */

void  is_domain_f64_ne_zero(_In_ const double* __restrict, _In_ const double* __restrict,
						    _In_ const int,                _Inout_  int*                );


     /*
        @Description: Checks for input domain of  x != 0.F
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  1st source array holding floats (32-bit)
        @Params:  none
        @Params:  length of  source array.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f32_ne_zero(_In_ const float* __restrict,
						    _In_ const int,  _Inout_ int* );


	 /*
        @Description: Checks for input domain of  x != 0.0L
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  1st source array holding doubles (64-bit)
        @Params:  none
        @Params:  length of  source array.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f64_ne_zero(_In_ const double* __restrict,
						    _In_ const int, _Inout_ int* );


     /*
        @Description: Checks for input domain of  x >= -1.F
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  1st source array holding floats (32-bit)
        @Params:  none
        @Params:  length of  source array.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f32_ne_negone(_In_ const float* __restrict,
							  _In_ const int, _Inout_ int* );


     /*
        @Description: Checks for input domain of  x >= -1.0
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  1st source array holding floats (32-bit)
        @Params:  none
        @Params:  length of  source array.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f64_ne_negone(_In_ const double* __restrict,
							  _In_ const int, _Inout_ int* );


	 
     /*
        @Description: Checks for existance of denormal values in
					  input range i.e. for some |x| < FLT_MIN
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  1st source array holding floats (32-bit)
        @Params:  none
        @Params:  length of  source array.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_denormal_f32_present(_In_ const float* __restrict,
							  _In_ const int,   _Inout_ int* );


     /*
        @Description: Checks for existance of denormal values in
                      input range i.e. for some |x| < DBL_MIN
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1).
                      Manually vectorised version.
        @Params:  1st source array holding doubles (64-bit)
        @Params:  none
        @Params:  length of  source array.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_denormal_f64_present(_In_ const double* __restrict,
							  _In_ const int,  _Inout_ int* );

							  
	 /*
        @Description: Checks for existance of denormal values in
                      input range i.e. for some |x| < FLT_MIN
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1) and feraiseexcept(FE_DENORMAL)
					  is called.
                      Manually vectorised version.
        @Params:  1st source array holding floats (32-bit)
        @Params:  none
        @Params:  length of  source array.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  check_denormf32_raise_except(_In_ const float* __restrict,
								   _In_ const int,  _Inout_ int* );


     /*
        @Description: Checks for existance of denormal values in
                      input range i.e. for some |x| < DBL_MIN
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1) and feraiseexcept(FE_DENORMAL)
                      is called.
                      Manually vectorised version.
        @Params:  1st source array holding floats (32-bit)
        @Params:  none
        @Params:  length of  source array.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  check_denormf64_raise_except(_In_ const double* __restrict,
								   _In_ const int,   _Inout_ int* );


     /*
        @Description: Checks for existance of NaN and INF values
                      in input range i.e. for some |x| == NaN OR |x| == INF
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1) 
                      
                      Scalar version.
        @Params:      1st source array holding floats (32-bit)
        @Params:  none
        @Params:  length of  source array.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f32_nan_inf(_In_ const float* __restrict,
							_In_ const int,   _Inout_ int*);


     /*
         @Description: Checks for existance of NaN and INF values
                      in input range i.e. for some |x| == NaN OR |x| == INF
                      Upon detecting error condition 'domain range'
                      indicator is set to true (1)

                     Scalar version
        @Params:      1st source array holding doubles (64-bit)
        @Params:  none
        @Params:  length of  source array.
        @Params:  none
        @Params:  pointer to variable holding 'Domain Range'error value.
        @Returns: Nothing
        @Throws:  Nothing
        @Calls:   Nothing
     */

void  is_domain_f64_nan_inf(_In_ const double* __restrict,
						    _In_ const int,   _Inout_ int* );


     /*
        @Description: Clears all floating-point state exceptions
                      Exception cleared:
					  FE_DENORMAL, FE_INVALID, FE_INEXACT, FE_UNDERFLOW
					  FE_OVERFLOW.
                      Scalar version
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'err' which indicates success or error as a return
				  value from library 'feclearexcept' function
				  Non-zero value means error.
        @Throws:  Nothing
        @Calls:   'feclearexcept'
     */

int  clear_all_feexcept();


     /*
        @Description: Clears only FE_DENORMAL exception
                      Exception cleared:
                      FE_DENORMAL
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'err' which indicates success or error as a return
                  value from library 'feclearexcept' function
                  Non-zero value means error.
        @Throws:  Nothing
        @Calls:   'feclearexcept'
     */

int  clear_fe_denormal();


	 /*
        @Description: Clears only FE_INEXACT exception
                      Exception cleared:
                      FE_INEXACT
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'err' which indicates success or error as a return
                  value from library 'feclearexcept' function
                  Non-zero value means error.
        @Throws:  Nothing
        @Calls:   'feclearexcept'
     */

int  clear_fe_inexact();


     /*
        @Description: Clears only FE_INVALID exception
                      Exception cleared:
                      FE_INVALID
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'err' which indicates success or error as a return
                  value from library 'feclearexcept' function
                  Non-zero value means error.
        @Throws:  Nothing
        @Calls:   'feclearexcept'
     */

int  clear_fe_invalid();


	 /*
        @Description: Clears only FE_DIVBYZERO exception
                      Exception cleared:
                      FE_DIVBYZERO
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'err' which indicates success or error as a return
                  value from library 'feclearexcept' function
                  Non-zero value means error.
        @Throws:  Nothing
        @Calls:   'feclearexcept'
     */

int  clear_fe_divbyezero();


     /*
        @Description: Clears only FE_OVERFLOW exception
                      Exception cleared:
                      FE_OVERFLOW
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'err' which indicates success or error as a return
                  value from library 'feclearexcept' function
                  Non-zero value means error.
       @Throws:  Nothing
       @Calls:   'feclearexcept'
     */
	
int  clear_fe_overflow();

	
     /*
        @Description: Clears only FE_UNDERFLOW exception
                      Exception cleared:
                      FE_UNDERFLOW
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'err' which indicates success or error as a return
                  value from library 'feclearexcept' function
                  Non-zero value means error.
        @Throws:  Nothing
        @Calls:   'feclearexcept'
     */

int  clear_fe_underflow();


     /*
        @Description: Tests all floating-point exceptions.
                      
        @Params:  All 7 floating-point exception types (exception values must be or'ed).
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'val' which indicates success or error as a return
                  value from library 'fetestexcept' function
                  
        @Throws:  Nothing
        @Calls:   'fetestexcept'
     */

int  test_fe_excepts(const int);


     /*
        @Description: Tests for existance of FE_INVALID exception.

        @Params:  argument FE_INVALID macro.
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'val' which indicates success or error as a return
                  value from library 'fetestexcept' function

        @Throws:  Nothing
        @Calls:   'fetestexcept'
     */

int  test_fe_invalid(const int);


     /*
        @Description: Tests for existance of FE_INEXACT exception.

        @Params:  argument FE_INEXACT macro.
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'val' which indicates success or error as a return
                  value from library 'fetestexcept' function

        @Throws:  Nothing
        @Calls:   'fetestexcept'
	 */

int  test_fe_inexact(const int);


     /*
        @Description: Tests for existance of FE_DIVBYZERO exception.

        @Params:  argument FE_DIVBYZERO macro.
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'val' which indicates success or error as a return
                  value from library 'fetestexcept' function

        @Throws:  Nothing
        @Calls:   'fetestexcept'
     */

int test_fe_divbyzero(const int);


     /*
        @Description: Tests for existance of FE_UNNORMAL exception.

        @Params:  argument FE_UNNORMAL macro.
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'val' which indicates success or error as a return
                  value from library 'fetestexcept' function

        @Throws:  Nothing
        @Calls:   'fetestexcept'
     */

int test_fe_unnormal(const int);


     /*
        @Description: Tests for existance of FE_UNDERFLOW exception.

        @Params:  argument FE_UNDERFLOW macro.
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'val' which indicates success or error as a return
                  value from library 'fetestexcept' function

        @Throws:  Nothing
        @Calls:   'fetestexcept'
     */

int test_fe_underflow(const int);


     /*
        @Description: Tests for existance of FE_OVERFLOW exception.

        @Params:  argument FE_OVERFLOW macro.
        @Params:  none
        @Params:  none
        @Params:  none
        @Params:  none
        @Returns: integer 'val' which indicates success or error as a return
                  value from library 'fetestexcept' function

        @Throws:  Nothing
        @Calls:   'fetestexcept'
     */

int  test_fe_overflow(const int);


	


#endif /*__COMMON_H__*/