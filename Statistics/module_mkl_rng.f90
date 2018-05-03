
#include "Config.fpp"
include 'mkl_vsl.f90'

module mod_mkl_rng

    !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_mkl_rng'
 !          
 !          Purpose:
 !                   Wrapper around Intel MKL Fortran    VSL_STREAM_STATE functions
 !          History:
 !                        Date: 03-05-2018
 !                        Time: 12:23 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                  Bernard Gingold
 !                 
 !          References:
 !         
 !                 Intel MKL library manual. 
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    
    use module_kinds, only : I32P
    use mkl_vsl_type
    use mkl_vsl
    implicit none
    private
    
    ! Major version
    integer(I32P), parameter, public :: MOD_MKL_RNG_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_MKL_RNG_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_MKL_RNG_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_MKL_RNG_FULLVER = 1000_I32P*MOD_MKL_RNG_MAJOR + &
                                                              100_I32P*MOD_MKL_RNG_MINOR  + &
                                                              10_I32P*MOD_MKL_RNG_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_MKL_RNG_CREATE_DATE = "03-05-2018 12:27 +00200 (THR 03 MAY 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation date/time)
    character(*),  parameter, public :: MOD_MKL_RNG_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_MKL_RNG_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_MKL_RNG_DESCRIPT = "Fortran 2003 wrapper around Intel MKL VSL_STREAM_STATE functions."
    
    !===================================
    !    type: MKLRandGen_t
    !===================================
    type, public :: MKLRandGen_t
        
          private
          
          integer(I32P) :: m_method
          
          integer(I32P) :: m_brng
          
          integer(I32P) :: m_seed
          
          integer(I32P) :: m_error
          
          logical(I32P) :: m_isset
          
          type(VSL_STREAM_STATE) :: m_stream
          
          contains
    
          !===================================
          !  Initialization, deinitialzation
          !===================================
    
          procedure, pass(this), public :: init_stream
          
          procedure, pass(this), public :: deinit_stream
          
          !========================================
          !    Getter procedures
          !======================================== 
          
          procedure, pass(this), public :: get_method
          
          procedure, pass(this), public :: get_brng
          
          procedure, pass(this), public :: get_seed
          
          procedure, pass(this), public :: get_error
          
          procedure, pass(this), public :: get_stream
          
    end type MKLRandGen_t
          
    contains
    
    subroutine init_stream(this,method,brng,seed)
           use IFPORT, only :  TRACEBACKQQ
           class(MKLRandGen_t), intent(inout) :: this
           integer(I32P),       intent(in)    :: method,brng,seed
           ! Executable ststemtns
           this%m_method = method
           this%m_brng   = brng
           this%m_seed   = seed
           this%m_error  = 1_I32P
           this%m_isset  = .false.
           this%m_error = vslnewstream(this%m_stream,this%m_brng,this%m_seed)
           if(VSL_ERROR_OK /= this%m_error) then
              print*, "vslnewstream failed with an error: ", this%m_error
              call TRACEBACKQQ(STRING="vslnewstream failed",USER_EXIT_CODE=-1)
              this%m_isset = .false.
              return
           end if
           this%m_isset = .true.
    end subroutine
    
    subroutine deinit_stream(this)
         use IFPORT, only :  TRACEBACKQQ
         class(MKLRandGen_t),   intent(inout) :: this
         ! Executable statemetns
         if(this%m_isset == .true. .AND.  &
            VSL_ERROR_OK == this%m_error ) then
                this%m_error = vsldeletestream(this%m_stream)
                if(VSL_ERROR_OK /= this%m_error) then
                     print*, "vsldeletestream failed with an error: ", this%m_error
                     call TRACEBACKQQ(STRING="vsldeletestream failed",USER_EXIT_CODE=-1)
                     return
                end if
                this%m_isset = .false.
         end if
    end subroutine        
             
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_method
!DIR$ ENDIF 
    pure function get_method(this) result(method)
          class(MKLRandGen_t),  intent(in) :: this
          integer(I32P) :: method
          method = this%m_method
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_brng
!DIR$ ENDIF
    pure function get_brng(this) result(brng)
          class(MKLRandGen_t),  intent(in)  :: this
          integer(I32P) :: brng
          brng = this%m_brng
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_seed
!DIR$ ENDIF 
    pure function get_seed(this) result(seed)
          class(MKLRandGen_t),  intent(in) :: this
          integer(I32P) :: seed
          seed = this%m_seed
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_error
!DIR$ ENDIF
    pure function get_error(this) result(error)
         class(MKLRandGen_t),   intent(in) :: this
         integer(I32P) :: error
         error = this%m_error
    end function
    
 !DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_stream
!DIR$ ENDIF
    pure function get_stream(this) result(stream)
          class(MKLRandGen_t),  intent(in) :: this
          type(VSL_STREAM_STATE) :: stream
          stream = this%m_stream
    end function


end module mod_mkl_rng