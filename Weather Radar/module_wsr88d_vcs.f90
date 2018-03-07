
#include "Config.fpp"


module mod_wsr88d_vcs

    !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_vcs'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Velocity Cross Section product.
 !          History:
 !                        Date: 06-03-2018
 !                        Time: 17:09 GMT+2
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
 !                 Document: 2620001W    
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    use module_kinds,  only : I32P, R64P
    implicit none
    private

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_WSR88D_VCS_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_VCS_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_VCS_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_VCS_FULLVER = 1000_I32P*MOD_WSR88D_VCS_MAJOR + &
                                                                 100_I32P*MOD_WSR88D_VCS_MINOR  + &
                                                                 10_I32P*MOD_WSR88D_VCS_MICRO
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_VCS_CREATE_DATE = "07-03-2018 17:47 +00200 (WED 07 MAR 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_VCS_CREATE_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_VCS_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSR88D_VCS_DESCRIPT = "WSR-88D Velocity Cross Section product."
    
    !=============================
    ! type: VCS_t
    !=============================
    type, public :: VCS_t
        
          private
          
          integer(I32P) :: m_nsize ! size of measurements
          
          real(R64P), allocatable, dimension(:), public :: m_Azimuth  ! Azimuth, deg, 0-359, 1
          
          real(R64P), allocatable, dimension(:), public :: m_Range    ! Range, nmi, 0.0-124.0, 1.0
          
          real(R64P), allocatable, dimension(:), public :: m_MaxV     ! ! Max Velocity, Kts, 0.0-245.0, 1.0
          
          real(R64P), allocatable, dimension(:), public :: m_HMaxV    ! Height of Max Velocity, Kft, 0.0-70.0, 1.0
          
          real(R64P), allocatable, dimension(:), public :: m_MinV     ! Min Velocity, Kts, -247.0-0.0, 1.0
          
          real(R64P), allocatable, dimension(:), public :: m_HMinV    ! Height of Min Velocity, Kft, 0.0-70.0, 1.0
          
          contains
    
         !========================================
         !    Getter procedures
         !======================================== 
         procedure, pass(this), public :: get_nsize
         
          !==========================================
          !   Read/write procedures
          !==========================================
         procedure, nopass, public :: read_state
         
         procedure, nopass, public :: write_state
         
          !============================================
          !  Class helper procedures
          !============================================
         procedure, pass(this), private :: dbg_info
         
         !=============================================
         !     Generic operators
         !=============================================
         procedure, public :: copy_assign
         
         generic :: assignment (=) => copy_assign
        
    end type VCS_t
          
    interface VCS_t
     procedure :: constructor
    end interface VCS_t
    
    contains
    
     !========================================!
     !    Implementation                      !
     !========================================!
    
    !=================================================!
    !  @function: constructor                                          
    !  Initialization of object state.                                          
    !  Allocation and initialization to default values
    !  of real arrays
    !  Argument: MRefPos should be default initialized prior
    !            to calling constructor.
    ! 
    !=================================================!
    type(VCS_t) function constructor(nsize,logging,verbose,append,fname )
          use mod_print_error, only : handle_fatal_memory_error
          use mod_constants,   only : LAM_PINF
          integer(I32P),       intent(in) :: nsize
          logical(I32P),       intent(in) :: logging,verbose,append
          character(len=*),    intent(in) :: fname
          ! Locals
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          ! Start of executable statements
          constructor%m_nsize = nsize
          associate(n=>constructor%m_nsize )
               allocate(constructor%m_Azimuth(n),  &
                        constructor%m_Range(n),    &
                        constructor%m_MaxV(n),     &
                        constructor%m_HMaxV(n),    &
                        constructor%m_MinV(n),     &
                        constructor%m_HMinV(n),    &
                        STAT=aerr,                 &
                        ERRMSG=emsg     )
          end associate
          if(aerr /= 0) then
              call handle_fatal_memory_error(logging,verbose,append,fname,   &
                                             "logger:162 --> mod_wsr88d_vcs/constructor: Memory Allocation Failure !!", &
                                             "mod_wsr88d_vcs/constructor:162 -- Memory Allocation Failure !!"  ,  &
                                             emsg,__LINE__  )
          end if
          constructor%m_Azimuth = LAM_PINF
          constructor%m_Range   = LAM_PINF
          constructor%m_MaxV    = LAM_PINF
          constructor%m_HMaxV   = LAM_PINF
          constructor%m_MinV    = LAM_PINF
          constructor%m_HMinV   = LAM_PINF
    end function constructor
    
    !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nsize
!DIR$   ENDIF   
    pure function get_nsize(this) result(nsize)
          class(VCS_t),  intent(in) :: this
          integer(I32P) :: nsize
          ! Start of executable ststements
          nsize = this%m_nsize
    end function get_nsize
    
     !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          
          class(VCS_t),       intent(in) :: this
          character(len=*),   intent(in) :: form
          integer(I32P),      intent(in) :: unit
          integer(I32P),      intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine read_state
    
    subroutine write_state(this,form,unit,ioerr)
         
          class(VCS_t),       intent(in) :: this
          character(len=*),   intent(in) :: form
          integer(I32P),      intent(in) :: unit
          integer(I32P),      intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine  write_state
    
    subroutine dbg_info(this)
          class(VCS_t),  intent(in) :: this
          ! Start of executable ststements
          print*, "==========================================="
          print*, "     *** Dump of VCS_t object state ***    "
          print*, "==========================================="
          print*, "Collected at: ", __DATE__,":", __TIME__, &
                  "in file: ", __FILE__, "at line: ", __LINE__
          print*, "n_size:          ", this%m_nsize
          print*, "m_Azimuth:       ", this%m_Azimuth
          print*, "m_Range:         ", this%m_Range
          print*, "m_MaxV:          ", this%m_MaxV
          print*, "m_HMax:          ", this%m_HMaxV
          print*, "m_MinV:          ", this%m_MinV
          print*, "m_HMinV:         ", this%m_HMinV
          print*, "==========================================="
    end subroutine dbg_info
    
    !====================================
    !  operator assignment (=)
    !====================================
    subroutine copy_assign(lhs,rhs)
          class(VCS_t),  intent(inout) :: lhs
          class(VCS_t),  intent(in)    :: rhs
          ! Start of executable statemetns
          lhs%m_nsize   = rhs%m_nsize
          lhs%m_Azimuth = rhs%m_Azimuth
          lhs%m_Range   = rhs%m_Range
          lhs%m_MaxV    = rhs%m_MaxV
          lhs%m_HMaxV   = rhs%m_HMaxV
          lhs%m_MinV    = rhs%m_MinV
          lhs%m_HMinV   = rhs%m_HMinV
    end subroutine copy_assign
    
end module mod_wsr88d_vcs