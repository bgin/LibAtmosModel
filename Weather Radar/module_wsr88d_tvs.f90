 #include "Config.fpp"
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
    use module_logger, only : log_startup,  &
                              log_UsrMsg,   &
                              log_shutdown
    use IFPORT,        only : TRACEBACKQQ
    use mod_datetime,  only : datetime,     &
                              getYear,      &
                              getMonth,     &
                              getDay,       &
                              getHour,      &
                              getMinute,    &
                              getSecond,    &
                              getMilisecond
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
          
          logical(I32P) :: m_isbuilt ! built indicator
          
          contains
    
          !=====================================
          ! Construction,copy and destruction
          ! methods.
          !=====================================
          ! Constructor for default initialization only.
          procedure, pass(this), public :: init
          
          ! Copy-Constructor
          procedure, pass(this), public :: copy
          
          ! Move-Constructor
          procedure, pass(this), public :: move
          
          ! Destructor    set to 0 if you prefer to use language built deallocation functions
!DIR$ IF(DECLARE_PROC_DESTRUCTOR .EQ. 1)
          procedure, pass(this), public :: destroy
!DIR$ ENDIF          
          !========================================
          !    Getter procedures
          !========================================
          
          procedure, pass(this), public :: get_nsize
          
          procedure, pass(this), public :: get_radID
          
          procedure, pass(this), public :: get_dtime
          
          
          
          procedure, pass(this), public :: get_isbuilt
          
          !==========================================
          !   Read/write procedures
          !==========================================
          procedure, nopass, public :: read_state
          
          procedure, nopass, public :: write_state
          
          !============================================
          !  Class helper procedures
          !============================================
          
          procedure, pass(this), private :: dbg_info
          
    end type TSV_t
          
    contains
    
     !========================================!
     !    Implementation                      !
     !========================================!
    
    !=================================================!
    !  @subroutine: init                                          
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
    subroutine init(this,nsize,radID,dtval,tz,ierr,logging,verbose,  &
                    dbg,append,fname                    )
          use mod_print_error, only : print_non_fatal_error,  &
                                      print_fatal_error
          use mod_constants,   only : LAM_PINF
          implicit none
          class(TSV_t),                intent(inout) :: this
          integer(I32P),               intent(in)    :: nsize,radID
          integer(I32P), dimension(7), intent(in)    :: dtval
          real(R64P),                  intent(in)    :: tz
          integer(I32P),               intent(inout) :: ierr
          logical(I32P),               intent(in)    :: logging,verbose,append,dbg
          character(len=*),            intent(in)    :: fname
          ! Locals
          character(len=40)   :: sdate,stime
          character(len=256)  :: emsg
          integer(I32P)       :: i, aerr
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true.   .OR. &
             nsize <= 0_I32P        ) then
              if(logging == .true.)  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:191 --> mod_wsr88d_tsv/init: TSV_t already built, or invalid array size argument!!")
                  call log_shutdown()
              else if (verbose == .true. ) then
                  call print_nont_fatal_error("========================= NON_FATAL =========================" , &
                                              "mod_wsr88d_tsv/init: TSV_t already built, or invalid array size argument!!" , &
                                              sdate,stime,__LINE__ )
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin construction
          this%m_nsize = nsize
          this%m_radID = radID
          this%m_dtime = datetime(dtval(1),dtval(2),dtval(3),  &
                                  dtval(4),dtval(5),dtval(6),dtval(7),tz)
          this%m_cellID = "AA"
          associate(n=>this%m_nsize) 
              allocate(this%m_Azimuth(n), &
                       this%m_Range(n),   &
                       this%m_AvgDv(n),   &
                       STAT=aerr,         &
                       ERRMSG=errmsg )
          end associate
          if(aerr /= 0) then
              if(logging == .true. ) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:218 --> mod_wsr88d_tsv/init: Memory Allocation Failure!!")
                  call log_shutdown()
              else if(verbose == .true.) then
                  call print_fatal_error("========================= FATAL =========================", &
                                         "mod_wsr88d_tsv/init:218 -- Memory Allocation Failure!!",    &
                                         emsg,sdate,stime,__LINE__)
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_tsv/init:218 [FATAL-ERROR] -- Memory Allocation Failure !!"
          end if
          ! Initialize arrays
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1_I32P, this%m_nsize
              this%m_Azimuth(i) = LAM_PINF
              this%m_Range(i)   = LAM_PINF
              this%m_AvgDv(i)   = LAM_PINF
          end do
          this%m_isbuilt = .true.
          if(dbg == .true.) then
              call this%dbg_info()
          end if
    end subroutine init
                    
     !=================================================!
    !  @subroutine: copy                                          
    !  @Purpose:
    !            Copying of object state.
    !            Deep copy semantics in use.
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already
    !   
    !==================================================!
    subroutine copy(this,other,logging,fname,append,verbose,ierr )
          use mod_print_error, only : print_non_fatal_error
          implicit none
          class(TVS_t),     intent(inout) :: this
          class(TVS_t),     intent(in)    :: other
          logical(I32P),    intent(in)    :: logging
          character(len=*), intent(in)    :: fname
          logical(I32P),    intent(in)    :: append,verbose
          integer(I32P),    intent(inout) :: ierr
          ! Locals
          character(len=40)  :: sdate,stime
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true.  .OR. &
             other%m_isbuilt == .false.   ) then
              if(logging == .true.  ) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:274 --> mod_wsr88d_tvs/copy: Invalid argument(s) state!!")
                  call log_shutdown()
              else if (verbose == .true. ) then
                  call print_non_fatal_error("========================= NON-FATAL ========================="  , &
                                             "mod_wsr88d_tvs/copy:274 -- Invalid argument(s) state!!",          &
                                             sdate,stime,__LINE__ )
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin copy-construction
          this%m_nsize   = other%m_nsize
          this%m_radID   = other%m_radID
          this%m_dtime   = other%m_dtime
          this%m_cellID  = other%m_cellID
          this%m_Azimuth = other%m_Azimuth
          this%m_Range   = other%m_Range
          this%m_AvgDv   = other%m_AvgDv
          this%m_isbuilt = .true.
    end subroutine copy
    
    !=================================================!
    !  @subroutine: move                                          
    !  @Purpose:
    !            Copying of object state.
    !            Using move_alloc function.
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already
    !   
    !==================================================!
    subroutine move(this,other,logging,fname,append,verbose,ierr)
          use mod_print_error, only : print_non_fatal_error
          implicit none
          class(TVS_t),      intent(inout) :: this
          class(TVS_t),      intent(inout) :: other
          logical(I32P),     intent(in)    :: logging
          character(len=*),  intent(in)    :: fname
          logical(I32P),     intent(in)    :: append,verbose
          integer(I32P),     intent(inout) :: ierr
          ! Locals
          character(len=40)   :: sdate,stime
          ! Start of executable ststements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true. .OR. &
             other%m_isbuilt == .false. ) then
              if(logging == .true. ) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:327 --> mod_wsr88d_tvs/move: Invalid argument(s) state !!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_non_fatal_error("========================= NON-FATAL =========================", &
                                             "mod_wsr88d_tvs/move:327 -- Invalid argument(s) state !!" , &
                                             sdate,stime,__LINE__ )
              end if
              ierr = -1_I32P
              return
            end if
            ! Begin move-construction
            this%m_nsize = other%m_nsize
            this%m_radID = other%m_radID
            this%m_dtime = other%m_dtime
            this%m_cellID = other%m_cellID
            call move_alloc(other%m_Azimuth, this%m_Azimuth)
            call move_alloc(other%m_Range,   this%m_Range)
            call move_alloc(other%m_AvgDv,   this%m_AvgDv)
            other%m_nsize = 0_I32P
            other%m_radID = -1_I32P
            other%m_dtime = datetime()
            other%m_cellID = "AA"
            other%m_isbuilt = .false.
            this%m_isbuilt = .true.
    end subroutine move
    
    !=================================================!
    !  @subroutine: destroy                                          
    !  @Purpose:
    !            Destroys object state by allocatable
    !            arrays deallocation and setting
    !            member scalar variables to default values.
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already
    !=================================================!
!DIR$ IF (DECLARE_PROC_DESTRUCTOR .EQ. 1)
    
    subroutine destroy(this,logging,fname,append,verbose,ierr)
          use mod_print_error, only : print_non_fatal_error,  &
                                      print_fatal_error
          implicit none
          class(TVS_t),     intent(inout) :: this
          logical(I32P),    intent(in)    :: logging
          character(len=*), intent(in)    :: fname
          logical(I32P),    intent(in)    :: append,verbose
          integer(I32P),    intent(inout) :: ierr
          ! Locals
          character(len=40)   :: sdate,stime
          character(len=256)  :: emsg
          integer(I32P)       :: derr
          ! Start of executable ststements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .false.) then
              if(logging == .true.) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:390 --> mod_wsr88d_tvs/destroy -- TVS_t already destroyed !!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_non_fatal_error("========================= NON-FATAL ========================="  , &
                                             "mod_wsr88d_tvs/destroy:390 -- TVS_t already destroyed !!"  , &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          this%m_nsize = 0_I32P
          this%m_radID = -1_I32P
          this%m_dtime = datetime()
          this%m_cellID = "AA"
          deallocate(this%m_Azimuth,  &
                     this%m_Range,    &
                     this%m_AvgDv,    &
                     STAT=derr,       &
                     ERRMSG=emsg )
          if(derr /= 0) then
              if(logging == .true. ) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:412 --> mod_wsr88d_tvs/destroy: Memory Deallocation Failure !!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_fatal_error("========================= FATAL =========================" , &
                                         "mod_wsr88d_tvs/destroy: Memory Deallocation Failure !!" ,    &
                                         emsg,sdate,stime,__LINE__)
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_tvs/destroy:412 [FATAL-ERROR] --> Memory Deallocation Failure !!"
          end if
          this%m_isbuilt = .false.
    end subroutine destroy
!DIR$  ENDIF
    
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
   
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_isbuilt
!DIR$   ENDIF
    pure function get_isbuilt(this) result(isbuilt)
          implicit none
          class(TVS_t),  intent(in) :: this
          logical(I32P) :: isbuilt
          ! Start of executable ststements
          isbuilt = this%m_isbuilt
    end function  get_isbuilt
    
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
          print*, "built status:   ", this%m_isbuilt
          print*, "======================================================="
    end subroutine dbg_info
                    
                    
end module mod_wsr88d_tvs