

#include "Config.fpp"

module mod_wsr88d_rcs

  !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_rcs'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Radar Cross Section product.
 !          History:
 !                        Date: 04-03-2018
 !                        Time: 09:09 GMT+2
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
    use module_kinds,        only : I32P, R64P
   
    use mod_wsr88d_mreflpos, only : MaxRefPos_t
    
    implicit none
    private
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(I32P), parameter, public :: MOD_WSR88D_RCS_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_RCS_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_RCS_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_RCS_FULLVER = 1000_I32P*MOD_WSR88D_RCS_MAJOR + &
                                                                 100_I32P*MOD_WSR88D_RCS_MINOR  + &
                                                                 10_I32P*MOD_WSR88D_RCS_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_RCS_CREATE_DATE = "04-03-2018 09:09 +00200 (SUN 04 MAR 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_RCS_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_RCS_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSR88D_RCS_DESCRIPT = "WSR-88D Radar Cross Section product."
    
    !==============================
    ! Type: RCS_t
    !==============================
    type, public :: RCS_t
        
          private
          
          
          
          integer(I32P) :: nsize  ! Number of measurements taken (size of arrays)
          
          real(R64P), allocatable, dimension(:), public :: m_Azimuth  ! Azimuth, deg, 0.0-359.0, 1.0 ,(per one measurement) of RCS
          
          real(R64P), allocatable, dimension(:), public :: m_Range    ! Range, nmi, 0.0-124.0, 1.0  , (per one measurement) of RCS
          
          real(R64P), allocatable, dimension(:), public :: m_MaxRef   ! Max Reflectivity, dBZ, -32.0- +95.0, 1.0
          
          real(R64P), allocatable, dimension(:), public :: m_HMaxRef  !  Height of Max Reflectivity,  Kft, 0.0-70.0, 1.0
          
          type(MaxRefPos_t), allocatable :: m_MRefPos                              ! Maximum reflectivity position (each value of pair [azimuth,range] per each measurement
          
         
          
          contains
    
         
          
         
        
          
         !========================================
         !    Getter procedures
         !======================================== 
         procedure, pass(this), public :: get_nsize
         
         procedure, pass(this), public :: get_MRefPos
         
        
         
          !==========================================
          !   Read/write procedures
          !==========================================
         procedure, nopass, public :: read_state
         
         procedure, nopass, public :: write_state
         
          !============================================
          !  Class helper procedures
          !============================================
         procedure, pass(this), private :: dbg_info
         
         !=======================================
         !   Generic operators
         !=======================================
         procedure, public :: copy_assign
         
         generic :: assignment(=) => copy_assign
         
          end type RCS_t
          
     interface RCS_t
         procedure constructor
     end interface RCS_t
          
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
    type(RCS_t) function constructor(nsize,MRefPos,logging,verbose,append,fname )
          use mod_print_error,   only : handle_fatal_memory_error
          use mod_constants,     only : LAM_PINF                             
          integer(I32P),                  intent(in)    :: nsize
          type(MaxRefPos_t), allocatable, intent(inout) :: MRefPos
          logical(I32P),                  intent(in)    :: logging,verbose , &
                                                           append
          character(len=*),               intent(in)    :: fname
           ! Locals
          
          character(len=256)  :: emsg
          integer(I32P)       :: aerr
          ! Start of executable statemetns
          constructor%m_nsize = nsize
          associate(n=>constructor%m_nsize )
               allocate(constructor%m_Azimuth(n),  &
                        constructor%m_Range(n),    &
                        constructor%m_MaxRef(n),   &
                        constructor%m_HMaxRef(n),  &
                        STAT=aerr,          &
                        ERRMSG=emsg     )
          end associate
          if(aerr /= 0) then
             
              call handle_fatal_memory_error(logging,verbose,append,fname,   &
                                             "logger:198 --> mod_wsr88d_rcs/constructor: Memory Allocation Failure !!" , &
                                             "mod_wsr88d_rcs/constructor:198 -- Memory Allocation Failure !!" , &
                                             emsg,__LINE__ )
          end if
          call move_alloc(MRefPos, constructor%m_MRefPos)
          constructor%m_Azimuth = LAM_PINF
          constructor%m_Range   = LAM_PINF
          constructor%m_MaxRef  = LAM_PINF
          constructor%m_HMaxRef = LAM_PINF
    end function constructor     
          
          
         
          
         
    
    
    !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nsize
!DIR$   ENDIF
    pure function get_nsize(this) result(nsize)
          class(RCS_t),  intent(in) :: this
          integer(I32P) :: nsize
          ! Start of executable stateemtns
          nsize = this%m_nsize
    end function
    
  !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_MRefPos
!DIR$   ENDIF
    pure function get_MRefPos(this) result(MRefPos)
          class(RCS_t),  intent(in) :: this
          type(MaxRefPos_t), allocatable  :: MRefPos
          ! Start of executable ststemetns
          allocate(MRefPos)
          MRefPos%m_nsize = this%m_RefPos%m_nsize
          
          MRefPos%m_Azimuth = this%m_RefPos%m_Azimuth
          MRefPos%m_Range   = this%m_RefPos%m_Range
    end function get_MRefPos
    

     !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
        
          class(MaxRefPos_t), intent(in) :: this
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
         
          class(MaxRefPos_t), intent(in) :: this
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
    end subroutine 

    

    
    subroutine dbg_info(this)
          
          class(RCS_t),  intent(in) :: this
          ! Start of executable ststemetns
          print*, "======================================================="
          print*, "         *** Dump of RCS_t object state ***            "
          print*, "======================================================="
          print*, "Collected at: ", __DATE__,":",__TIME__, &
                  "in file: ", __FILE__, "at line: ",__LINE__
          print*, "n_size:          ", this%m_nsize
          print*, "m_Azimuth:       ", this%m_Azimuth
          print*, "m_Range:         ", this%m_Range
          print*, "m_MaxRef:        ", this%m_MaxRef
          print*, "m_HMaxRef:       ", this%m_HMaxRef
          print*, "         *** MaxRefPos_t component state***           "
          print*, "======================================================="
          call this%m_MRefPos%dbg_info()
          print*, "======================================================="
          print*, "======================================================="
         
    end subroutine dbg_info
    
    !====================================
    !  operator assignment (=)
    !====================================
    subroutine copy_assign(lhs,rhs)
          class(RCS_t),  intent(inout) :: lhs
          class(RCS_t),  intent(in)    :: rhs
          ! Start of executable ststements
          lhs%m_nsize   = rhs%m_nsize
          lhs%m_Azimuth = rhs%m_Azimuth
          lhs%m_Range   = rhs%m_Range
          lhs%m_MaxRef  = rhs%m_MaxRef
          lhs%m_HMaxTef = rhs%m_HMaxRef
          lhs%m_MRefPos = rhs%m_MRefPos
          lhs%m_MRefPos%m_nsize = rhs%m_MRefPos%m_nsize
          lhs%m_MRefPos%m_Azimuth = rhs%m_MRefPos%m_Azimuth
          lhs%m_MRefPos%m_Range   = rhs%m_MRefPos%m_Range
    end subroutine copy_assign
    
    
end module mod_wsr88d_rcs