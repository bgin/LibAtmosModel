
module mod_wsr88d_tvs

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_tvs'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Tornado Vortex Signature product.
 !          History:
 !                        Date: 03-03-2018
 !                        Time: 10:26 GMT+2
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
   
    use mod_datetime,  only : datetime
                              
    implicit none
    private
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_WSR88D_TSV_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_TSV_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_TSV_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_TSV_FULLVER = 1000_I32P*MOD_WSR88D_TSV_MAJOR + &
                                                                 100_I32P*MOD_WSR88D_TSV_MINOR  + &
                                                                 10_I32P*MOD_WSR88D_TSV_MICRO
    
    ! Module date creation
    character(*),  parameter, public :: MOD_WSR88D_TSV_CREATE_DATE = "03-03-2018 10:26 +00200 (SAT 03 MAR 2018 GMT+2) "
    
    ! Module build time (should be set after successful build)
    character(*),  parameter, public :: MOD_WSR88D_TSV_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_TSV_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module  short description
    character(*),  parameter, public :: MOD_WSR88D_TSV_DESCRIPT = "WSR-88D Tornado Vortex Signature product."
    
    !=================================
    ! Type: TSV_t
    !=================================
    type, public :: TSV_t
        
          private
          
          integer(I32P) :: m_nsize   ! size of parameter arrays
          
          integer(I32P) :: m_radID   ! Detecting radar ID   0-999
                                          ! Volume Scan start date, and time
          type(datetime) :: m_dtime       ! Months: 1-12, Days: 1-31, Years: 0-99
                                          ! Hours: 1-12, Minutes: 0-59, Seconds: 0-59
          character(len=2), dimension(66), public :: m_cellID ! Storm cell ID , Values A0-Z0, A1-Z1,A2-Z2, and sequence repeats.
          
          ! Parameter arrays
          real(R64P), allocatable, dimension(:), public :: m_Azimuth !  Azimuth,  deg,  0.0-359.0,  1.0
          
          real(R64P), allocatable, dimension(:), public :: m_Range   !  Range,    Nmi,  0.0-124.0,  1.0
          
          real(R64P), allocatable, dimension(:), public :: m_AvgDv   !  Average Delta Velocity, kts, 0.0-494.0, 1.0
          
         
          
          contains
    
         
          !========================================
          !    Getter procedures
          !========================================
          
          procedure, pass(this), public :: get_nsize
          
          procedure, pass(this), public :: get_radID
          
          procedure, pass(this), public :: get_dtime
          
          
          
         
          
          !==========================================
          !   Read/write procedures
          !==========================================
          procedure, nopass, public :: read_state
          
          procedure, nopass, public :: write_state
          
          !============================================
          !  Class helper procedures
          !============================================
          
          procedure, pass(this), private :: dbg_info
          
          !===========================================
          !   Class generic operators
          !===========================================
          procedure, public :: copy_assign
          
          generic :: assignment(=) => copy_assign
          
    end type TSV_t
          
    interface TVS_t
         procedure constructor
    end interface TVS_t
          
    contains
    
     !========================================!
     !    Implementation                      !
     !========================================!
    
    !=================================================!
    !  @function: constructor                                          
    !  Initialization of object state.                                          
    !  Allocation and initialization to default values
    !  of real arrays
    !  @Warning
    !            none
    !=================================================!
    type(TVS_t) function constructor(nsize,radID,dtval,tz,logging,verbose,  &
                                     append,fname                    )
          use mod_print_error, only :  handle_fatal_memory_error 
          use mod_constants,   only : LAM_PINF                           
          integer(I32P),               intent(in)    :: nsize,radID
          integer(I32P), dimension(7), intent(in)    :: dtval
          real(R64P),                  intent(in)    :: tz
          logical(I32P),               intent(in)    :: logging,verbose,append,dbg
          character(len=*),            intent(in)    :: fname
          ! Locals
          character(len=40)   :: sdate,stime
          character(len=256)  :: emsg
          integer(I32P)       :: aerr
          ! Start of executable statements
          ! Begin construction
          constructor%m_nsize = nsize
          constructor%m_radID = radID
          constructor%m_dtime = datetime(dtval(1),dtval(2),dtval(3),  &
                                  dtval(4),dtval(5),dtval(6),dtval(7),tz)
          constuctor%m_cellID = "AA"
          associate(n=>constructor%m_nsize) 
              allocate(constructor%m_Azimuth(n), &
                       constructor%m_Range(n),   &
                       constructor%m_AvgDv(n),   &
                       STAT=aerr,         &
                       ERRMSG=errmsg )
          end associate
          if(aerr /= 0) then
             
              call handle_fatal_memory_error(logging,verbose,append,fname,   &
                                             "logger:191 --> mod_wsr88d_tsv/constructor: Memory Allocation Failure!!" , &
                                             "mod_wsr88d_tsv/constructor:191 -- Memory Allocation Failure!!" , &
                                             emsg,__LINE__)
          end if
          constructor%m_Azimuth = LAM_PINF
          constructor%m_Range   = LAM_PINF
          constructor%m_AvgDv   = LAM_PINF
   end function constructor      
         
         
         
   
                    
 
   
    
    !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nsize
!DIR$   ENDIF
    pure function get_nsize(this) result(nsize)
          implicit none
          class(TVS_t),   intent(in) :: this
          integer(I32P) :: nsize
          ! Start of executable ststements
          nsize = this%m_nsize
    end function get_nsize
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_radID
!DIR$   ENDIF 
    pure function get_radID(this) result(radID)
          implicit none
          class(TVS_t),  intent(in) :: this
          integer(I32P) :: radID
          ! Start of executable ststements
          radID = this%m_radID
    end function get_radID
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_dtime
!DIR$   ENDIF    
   pure function get_dtime(this) result(dtime)
          implicit none
          class(TVS_t),  intent(in) :: this
          type(datetime) :: dtime
          ! Start of executable ststemetns
          dtime = this%m_dtime
   end function get_dtime
   

     !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(TVS_t),       intent(in) :: this
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
          implicit none
          class(TVS_t),       intent(in) :: this
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
          implicit none
          class(TSV_t),  intent(in) :: this
          ! Start of executable ststements
          print*, "======================================================="
          print*, "          *** Dump of TSV_t object state ***           "
          print*, "======================================================="
          print*, "Collected at: ", __DATE__,":",__TIME__,  &
                  "in file: ", __FILE__, "at line: ", __LINE__
          print*, "nsize:          ", this%m_nsize
          print*, "radar ID:       ", this%m_radID
          print*, "Date and time:  ", this%m_dtime
          print*, "cells ID:       ", this%m_cellID
          print*, "Azimuth:        ", this%m_Azimuth
          print*, "Range:          ", this%m_Range
          print*, "Average Delta V:", this%m_AvgDv
          print*, "======================================================="
         
    end subroutine dbg_info
    
    !====================================
    !  operator assignment (=)
    !====================================
    subroutine copy_assign(lhs,rhs)
          class(TSV_t),  intent(inout) :: lhs
          class(TSV_t),  intent(in)    :: rhs
          ! Start of executable ststemetns
          lhs%m_nsize   = rhs%m_nsize
          lhs%m_radID   = rhs%m_radID
          lhs%m_dtime   = rhs%m_dtime
          lhs%m_cellID  = rhs%m_cellID
          lhs%m_Azimuth = rhs%m_Azimuth
          lhs%m_Range   = rhs%m_Range
          lhs%m_AvgDv   = rhs%m_AvgDv
    end subroutine copy_assign
                    
end module mod_wsr88d_tvs