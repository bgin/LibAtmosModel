
#include "Config.fpp"
include 'mkl_vsl.f90'

module mod_mkl_betarng


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_mkl_betarng'
 !          
 !          Purpose:
 !                   Wrapper around Intel MKL Fortran    vdrngbeta subroutine
 !          History:
 !                        Date: 01-05-2018
 !                        Time: 13:32 GMT+2
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
    
    use module_kinds, only : I32P, R64P
    use mkl_vsl_type
    use mkl_vsl
    implicit none
    private
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_MKL_BETARNG_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_MKL_BETARNG_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_MKL_BETARNG_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_MKL_BETARNG_FULLVER = 1000_I32P*MOD_MKL_BETARNG_MAJOR + &
                                                                  100_I32P*MOD_MKL_BETARNG_MINOR  + &
                                                                  10_I32P*MOD_MKL_BETARNG_MICRO
    
    ! Module/file creation date
    character(*),  parameter, public :: MOD_MKL_BETARNG_CREATE_DATE = "01-05-2018 13:42 +00200 (TUE 01 MAY 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_MKL_BETARNG_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_MKL_BETARNG_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_MKL_BETARNG_DESCRIPT = "Fortran 2003 wrapper around Intel MKL vdrngbeta function."
    
    !==================================================
    !  type: MKLBetaRNG_t
    !==================================================
    type, public :: MKLBetaRNG_t
        
          private
          
          integer(I32P) :: m_nvalues
          
          integer(I32P) :: m_brng
          
          integer(I32P) :: m_seed
          
          integer(I32P) :: m_error
          
          real(R64P)    :: m_p
          
          real(R64P)    :: m_q
          
          real(R64P)    :: m_a
          
          real(R64P)    :: m_beta
          ! public in order to eliminate copying procedures
          real(R64P), allocatable, dimension(:), public :: m_rvec
          
          contains
          
          !========================================
          !    Getter procedures
          !======================================== 
          
          procedure, pass(this), public :: get_nvalues
          
          procedure, pass(this), public :: get_brng
          
          procedure, pass(this), public :: get_seed
          
          procedure, pass(this), public :: get_error
          
          procedure, pass(this), public :: get_p
          
          procedure, pass(this), public :: get_q
          
          procedure, pass(this), public :: get_a
          
          procedure, pass(this), public :: get_beta
          
          !========================================
          !    Read/write procedures
          !========================================
          
          procedure, nopass, public :: read_state
          
          procedure, nopass, public :: write_state
          
          !=======================================
          !  Class helper procedures
          !=======================================
          
          procedure, pass(this), public :: dgb_info
          
          !======================================
          !    Generic operators
          !======================================
          
          procedure, public :: copy_assign
          
          generic :: assignment (=) => copy_assign
          
          !=========================================
          !    Computational procedures
          !=========================================
          
          procedure, pass(this), public :: compute_betarng1
          
          procedure, pass(this), public :: compute_betarng2
          
        
    end type MKLBetaRNG_t
          
          interface MKLBetaRNG_t
          
             procedure :: constructor
             
          end interface MKLBetaRNG_t
          
    contains
    
    !=================================================!
    !  @function: constructor                                          
    !  Initialization of object state.                                          
    !  Allocation and initialization to default values
    !  of real arrays
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already  or invalid argument
    !   2) -2 -- Invalid argument (any of them)  ! Not used here
    !=================================================!
    type(MKLBetaRNG_t) function constructor(nvalues,brng,seed,p,q,a,beta,  &
                                            logging,verbose,fname,append  )
          use mod_print_error, only : handle_fatal_memory_error
          use mod_constants,   only : LAM_PINF
          integer(I32P),    intent(in) :: nvalues, &
                                          brng, seed
          real(R64P),       intent(in) :: p,q,a,beta
          logical(I32P),    intent(in) :: logging,verbose
          character(len=*), intent(in) :: fname
          logical(I32P),    intent(in) :: append
          ! Locals
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          ! Satrt of executable statements
          constructor%m_nvalues = nvalues
          constructor%m_brng    = brng
          constructor%m_seed    = seed
          constructor%m_error   = 1_I32P
          constructor%m_p       = p
          constructor%m_q       = q
          constructor%m_a       = a
          constructor%m_beta    = beta
          associate(n=>constructor%m_nvalues) 
                allocate(constructor%m_rvec(n),  &
                        STAT=aerr,               &
                        ERRMSG=emsg  )
          end associate
          if(aerr /= 0) then
              call handle_fatal_memory_error( logging,verbose,append,fname,   &
                                             "logger:210 --> mod_mkl_betarng/constructor: Memory Allocation Failure !!", &
                                             "mod_mkl_betarng/constructor:210 -- Memory Allocation Failure !!",         &
                                             emsg,__LINE__)
          end if
          constructor%m_rvec = LAM_PINF
    end function constructor
    
     !==========================================
    !       Getter procedures
    !==========================================
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nvalues
!DIR$ ENDIF
    pure function get_nvalues(this) result(nvalues)
          class(MKLBetaRNG_t), intent(in) :: this
          integer(I32P) :: nvalues
          nvalues = this%m_nvalues
    end function get_nvalues
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_brng
!DIR$ ENDIF
    pure function get_brng(this) result(brng)
          class(MKLBetaRNG_t), intent(in) :: this
          integer(I32P) :: brng
          brng = this%m_brng
    end function get_brng
    
 !DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_seed
!DIR$ ENDIF
    pure function get_seed(this) result(seed)
          class(MKLBetaRNG_t), intent(in) :: this
          integer(I32P) :: seed
          seed = this%m_seed
    end function   get_seed
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_error
!DIR$ ENDIF
    pure function get_error(this) result(error)
          class(MKLBetaRNG_t), intent(in) :: this
          integer(I32P) :: error
          error = this%m_error
    end function  get_error
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_p
!DIR$ ENDIF
    pure function get_p(this) result(p)
          class(MKLBetaRNG_t), intent(in) :: this
          real(R64P) :: p
          p = this%m_p
    end function get_p
    
 !DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_q
!DIR$ ENDIF
    pure function get_q(this) result(q)
          class(MKLBetaRNG_t), intent(in) :: this
          real(R64P) :: q
          q = this%m_q
    end function get_q
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_a
!DIR$ ENDIF   
    pure function get_a(this) result(a)
         class(MKLBetaRNG_t), intent(in) :: this
         real(R64P) :: a
         a = this%m_a
    end function get_a
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_beta
!DIR$ ENDIF
    pure function get_beta(this) result(beta)
         class(MKLBetaRNG_t), intent(in) :: this
         real(R64P) :: beta
         beta = this%m_beta
    end function get_beta
    
     !============================================
    !   Read/write procedures
    !============================================
    
     subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(MKLBetaRNG_t), intent(in)    :: this
          character(len=*),        intent(in)    :: form
          integer(I32P),           intent(in)    :: unit
          integer(I32P),           intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine read_state
    
    subroutine write_state(this,form,unit,ioerr)
          implicit none
          class(MKLBetaRNG_t), intent(in)    :: this
          character(len=*),        intent(in)    :: form
          integer(I32P),           intent(in)    :: unit
          integer(I32P),           intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine write_state
    
    subroutine dbg_info(this)
         class(MKLBetaRNG_t), intent(in) :: this
         print*, "================================================"
         print*, "      Dump of MKLBetaRNG_t object state         "
         print*, "================================================"
         print*, "  Collected at: ", __DATE__, ":", __TIME__
         print*, "================================================"
         print*, " m_nvalues: ", this%m_nvalues
         print*, " m_brng:    ", this%m_brng
         print*, " m_seed:    ", this%m_seed
         print*, " m_error:   ", this%m_error
         print*, " m_p:       ", this%m_p
         print*, " m_q:       ", this%m_q
         print*, " m_a:       ", this%m_a
         print*, " m_beta:    ", this%m_beta
         print*, " m_rvec:    ", this%m_rvec
         print*, "================================================"
    end subroutine dbg_info
    
    subroutine copy_assign(lhs,rhs)
          class(MKLBetaRNG_t), intent(inout) :: lhs
          class(MKLBetaRNG_t), intent(in)    :: rhs
          lhs%m_nvalues = rhs%m_nvalues
          lhs%m_brng    = rhs%m_brng
          lhs%m_seed    = rhs%m_seed
          lhs%m_error   = rhs%m_error
          lhs%m_p       = rhs%m_p
          lhs%m_q       = rhs%m_q
          lhs%m_a       = rhs%m_a
          lhs%m_beta    = rhs%m_beta
          lhs%m_rvec    = rhs%m_rvec
    end subroutine copy_assign
    
    subroutine compute_betarng1(this,method)
           use IFPORT, only :  TRACEBACKQQ
           class(MKLBetaRNG_t), intent(inout) :: this
           integer(I32P),       intent(in)    :: method
           ! Locals
           type(VSL_STREAM_STATE) :: stream
           this%m_error = vslnewstream(stream,this%m_brng,this%m_seed)
           if(VSL_ERROR_OK /= m_error) then
              print*, "vslnewstream failed with an error: ", this%m_error
              call TRACEBACKQQ(STRING="vslnewstream failed",USER_EXIT_CODE=-1)
              return
           end if
           this%m_error = vdrngbeta(method,stream,this%m_nvalues,this%m_rvec,  &
                                    this%m_p,this%m_q,this%m_a,this%m_beta)
          if(VSL_ERROR_OK /= this%m_error) then
              print*, "vdrngbeta failed with an error: ", this%m_error
              call TRACEBACKQQ(STRING="vdrngbeta failed",USER_EXIT_CODE=-1)
              return
          end if
           this%m_error = vsldeletestream(stream)
          if(VSL_ERROR_OK /= this%m_error) then
              print*, "vsldeletestream failed with an error: ", this%m_error
              call TRACEBACKQQ(STRING="vsldeletestream failed",USER_EXIT_CODE=-1)
              return
          end if
    end subroutine compute_betarng1
    
    subroutine compute_betarng2(this,stream,method)
            use IFPORT, only :  TRACEBACKQQ
            class(MKLBetaRNG_t),    intent(inout) :: this
            type(VSL_STREAM_STATE), intent(inout) :: stream
            integer(I32P),          intent(in)    :: method
            this%m_error = vdrngbeta(method,stream,this%m_nvalues,this%m_rvec,  &
                                    this%m_p,this%m_q,this%m_a,this%m_beta  )
            if(VSL_ERROR_OK /= this%m_error) then
              print*, "vdrngbeta failed with an error: ", this%m_error
              call TRACEBACKQQ(STRING="vdrngbeta failed",USER_EXIT_CODE=-1)
              return
          end if
    end subroutine compute_betarng2

end module mod_mkl_betarng