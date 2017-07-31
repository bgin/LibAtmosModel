
    !
    ! ----------------------------------------------------------------------------85
    ! ----------------------------  DESCRIPTION  ---------------------------------85
    !
    !               Module Name:
    !                               'module_kinds'
    !
    !               Module Purpose:
    !                                Provide centralized definition of
    !                                various intrinsic types.
    !
    !               Author:
    !                           Bernard Gingold
    !
    !               E-mail:
    !                           beniekg@gmail.com
    !
    !               History:
    !                           Date: 17-12-2016   , Time: 13:37
    !
    !         File  Version:
    !
    !                           Major:  1
    !                           Minor:  0
    !                           Micro:  0
    !
    !-------------------------------------------------------------------------------85
    
include "Config.fpp"

    
    module      module_kinds
    
      use ISO_FORTRAN_ENV , only:  INT32,INT64
      use intrinsic :: IEEE_ARITHMETIC
    implicit none
             
             public :: print_prologue
             public :: print_epilogue
             
    !  Integral intrinsic types
                
             
             integer, parameter :: I8P = selected_int_kind(2)    ! returns  1
             
             integer, parameter :: I16P = selected_int_kind(4)   ! returns  2
             
             integer, parameter :: I32P = INT32   ! returns  4
             
             integer, parameter :: I64P = INT64  ! returns  8
    
    
    !  Floating-point intrinsic types
             
             integer, parameter :: R32P = selected_real_kind(6,37)
             
             integer, parameter :: R64P = selected_real_kind(15,307)
             
             integer, parameter :: R128P = selected_real_kind(33,4931)
             
#if  defined (WRF_COUPLING)

             integer, parameter :: r64_kind = selected_real_kind(15)
#endif

                ! Used for debuging purpose.
                ! Prints functions call-tree.
             logical, public :: debug_flag = .true.
             
             real(R64P), parameter, private :: SHIFT_BY_1000 = 1000.0_R64P
             
                ! Very small constant
             real(R64P), parameter, private :: SMALL = 1.0E-16
             
             real(R64P), parameter :: SMALLEST = TINY(SMALL)
             
                 ! Prevent potential sliding towards denormal numbers
             real(R64P), parameter :: TINY_NUM = SHIFT_BY_1000 * SMALLEST
             
                 ! Define smallest allowed numeric value of Pressure Field
             real(R64P), parameter, public :: TINY_PRESSURE = TINY_NUM
             
                ! Very large constant
             real(R64P), parameter :: LARGE = 1.0E+35
             
             real(R64P), parameter, private :: LARGEST = HUGE(LARGE)
             
                ! Initialize by zero constant.
             real(R64P), parameter    :: ZeroInit = 0.0_R64P
             
             real(R64P), parameter    :: ZEROR64   = 0.0_R64P
             
             real(R32P), parameter    :: ZEROR32   = 0.0_R32P
             
             integer(I64P), parameter :: ONE = 1_I64P
             
             integer(I32P), parameter :: ZERO_I32P = 0_I32P
             
             integer(I64P), parameter :: ZERO_I64P = 0_I64P
             
             real(R64P),    parameter :: MACHEPSF64 =  0.00000000000000022204460492503131_R64P
             
             real(R64P),    parameter :: MODSTATOL  = 0.0000000000001_R64P
             
             real(R64P),    parameter :: ONER64P = 1.0_R64P
             
             real(R64P),    parameter :: QUARTER64P = 0.25_R64P
             
             real(R64P),    parameter :: HALFR64P   = 0.25_R64P
             
             real(R64P),    parameter :: INVALID    = IEEE_VALUE(1._R64P,IEEE_POSITIVE_INF)
    contains
    
             subroutine   print_prologue(name)
             
                character(len=*), intent(in) :: name
                ! Executable statements.
                print*, name
                
             end   subroutine
             
             subroutine   print_epilogue(name)
             
                character(len=*), intent(in) :: name
                ! Executable statements.
                print*, name
                
             end   subroutine
    
    end  module  module_kinds