
module mod_wsr88d_forecastmove


  !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_forecastmove'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Forecast Movement product.
 !          History:
 !                        Date: 24-02-2018
 !                        Time: 10:08 GMT+2
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
    implicit none
    private
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTMOVE_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTMOVE_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTMOVE_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTMOVE_FULLVER = 1000_I32*MOD_WSR88D_FORECASTMOVE_MAJOR + &
                                                                          100_I32P*MOD_WSR88D_FORECASTMOVE_MINOR + &
                                                                          10_I32P*MOD_WSR88D_FORECASTMOVE_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_FORECASTMOVE_CREATE_DATE = "24-02-2018 10:19 +00200 (SAT 24 FEB 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_FORECASTMOVE_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_FORECASTMOVE_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSR88D_FORECASTMOVE_DESCRIPT = "WSR-88D Forecast Movement product."
    
    !=====================================
    ! Type: ForecastMov_t
    !=====================================
    type, public :: ForecastMov_t
        
          private
          
          integer(I32P) :: nss,nsd ! nss - storm speed array length
                                   ! nsd - storm directory array length
          
          real(R64P), allocatable, dimension(:), public :: m_StormSpeed  ! Storm Speed, Kts, 0.0-999.0, 1.0
          
          real(R64P), allocatable, dimension(:), public :: m_StormDir    ! Storm Direction, deg, 0.0-359.0, 1.0
          
          logical(I32P) :: m_isbuilt ! built indicator
          
          contains
    
          !=====================================
          ! Construction,copy and destruction
          ! methods.
          !=====================================
          
          ! Constructor - default initialization only
          procedure, pass(this), public :: init
          
          ! Copy-Constructor
          procedure, pass(this), public :: copy
          
          ! Move-Constructor
          procedure, pass(this), public :: move
          
          ! Destructor
          procedure, pass(this), public :: destroy
          
          !========================================
          !    Getter procedures
          !========================================
          
          procedure, pass(this), public :: get_nss
          
          procedure, pass(this), public :: get_nsd
          
          procedure, pass(this), public :: get_StormSpeed
          
          procedure, pass(this), public :: get_StormDir
          
          procedure, pass(this), public :: get_isbuilt
          
          !========================================
          !    Read/write procedures
          !========================================
          
          procedure, nopass, public :: read_state
          
          procedure, nopass, public :: write_state
          
          !=======================================
          !  Class helper procedures
          !=======================================
          
          procedure, pass(this), private :: dbg_info
        
    end type ForecastMov_t
          
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
    subroutine init(this,nss,nsd,ierr,logging,verbose,dbg,append,fname)
          use mod_print_error, only : print_non_fatal_error,   &
                                      print_fatal_error
          use mod_constants,   only : LAM_PINF
          implicit none
          class(ForecastMov_t),   intent(inout) :: this
          integer(I32P),          intent(in)    :: nss,nsd
          integer(I32P),          intent(inout) :: ierr
          logical(I32P),          intent(in)    :: logging, verbose, dbg, append
          character(len=*),       intent(in)    :: fname
          ! Locals
          character(len=40) :: sdate, stime
          character(len=256) :: emsg
          integer(I32P) :: i, aerr
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true.  .OR.  &
             nss <= 0_I32P .OR. nsd <= 0_I32P  ) then
              if(logging == .true. )  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:182 --> mod_wsr88d_forecastmove/init -- ForecastMov_t already initialized, or invalid array size arguments !!")
                  call log_shutdown()
              else if (verbose == .true. ) then
                  call print_non_fatal_error("========================= NON_FATAL =========================",  &
                                             "mod_wsr88d_forecastmove/init:182 --> ForecastMov_t already initialized, or invalid array size arguments !!", &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin construction
          this%m_nss = nss
          this%m_nsd = nsd
          associate(ls=>this%m_nss,  &
                    ld=>this%m_nsd  )
               allocate(this%m_StormSpeed(ls),  &
                        this%m_StormDir(ld),    &
                        STAT=aerr,              &
                        ERRMSG=emsg  )
         end associate
         if(aerr /= 0)  then
             if(logging == .true. ) then
                 call log_startup(fname,append)
                 call log_UsrMsg("logger:206 --> mod_wsr88d_forecastmove/init: Memory Allocation Failure!!")
                 call log_shutdown()
             else if (verbose == .true.) then
                 call print_fatal_error("========================= FATAL =========================", &
                                        "mod_wsr88d_forecastmove/init:206 -- Memory Allocation Failure!!", &
                                        emsg,sdate,stime,__LINE__)
             end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
             call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_forecastmove/init:206 [FATAL-ERROR] -- Memory Allocation Failure !!" 
         end if
!DIR$    SIMD VECTORLENGTHFOR(REAL(KIND=8))
         do i = 1_I32P, this%m_nss
             this%m_StormSpeed(i) = LAM_PINF
         end do
!DIR$    SIMD VECTORLENGTHFOR(REAL(KIND=8))
         do i = 1_I32P, this%m_nsd
             this%m_StormDir(i)  = LAM_PINF
         end do
         this%m_isbuilt == .true.
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
    subroutine copy(this,other,logging,fname,append,verbose,ierr)
          use mod_print_error, only : print_non_fatal_error,  &
                                      print_fatal_error
          implicit none
          class(ForecastMov_t),  intent(inout) :: this
          class(ForecastMov_t),  intent(in)    :: other
          logical(I32P),         intent(in)    :: logging
          character(len=*),      intent(in)    :: fname
          logical(I32P),         intent(in)    :: append, verbose
          integer(I32P),         intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true.  .OR.  &
             other%m_isbuilt == .false.   ) then
              if(logging == .true.)  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:264 --> mod_wsr88d_forecastmove/copy: Invalid argument(s) state !!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_non_fatal_error("========================= NON-FATAL =========================" , &
                                             "mod_wsr88d_forecastmove/copy:264 -- Invalid argument(s) state !!" , &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin copu-construction
          this%m_nss = other%m_nss
          this%m_nsd = other%m_nsd
          this%m_StormSpeed = other%m_StormSpeed
          this%m_StormDir   = other%m_StormDir
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
          use mod_print_error, only : print_non_fatal_error,  &
                                      print_fatal_error
          implicit none
          class(ForecastMov_t),  intent(inout) :: this
          class(ForecastMov_t),  intent(inout) :: other
          logical(I32P),         intent(in)    :: logging
          character(len=*),      intent(in)    :: fname
          logical(I32P),         intent(in)    :: append, verbose
          integer(I32P),         intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true.  .OR.  &
             other%m_isbuilt == .false.  ) then
              if(logging == .true.) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:315 --> mod_wsr88d_forecastmove/move: Invalid argument(s) state !!")
                  call log_shutdown()
              else if(verbose == .true.) then
                  call print_non_fatal_error("========================= NON-FATAL ========================="  , &
                                             "mod_wsr88d_forecastmove/move:315 -- Invalid argument(s) state !!" , &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin move-construction
          this%m_nss = other%m_nss
          this%m_nsd = other%m_nsd
          call move_alloc(other%m_StormSpeed, this%m_StormSpeed)
          call move_alloc(other%m_StormDir,   this%m_StormDir)
          other%m_nss = 0_I32P
          other%m_nsd = 0_I32P
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
    subroutine destroy(this,logging,fname,append,verbose,ierr)
          use mod_print_error,  only : print_non_fatal_error,  &
                                       print_fatal_error
          implicit none
          class(ForecastMov_t),  intent(inout) :: this
          logical(I32P),         intent(in)    :: logging
          character(len=*),      intent(in)    :: fname
          logical(I32P),         intent(in)    :: append, verbose
          integer(I32P),         intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate, stime
          character(len=256) :: emsg
          integer(I32P)  :: derr
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .false. ) then
              if(logging == .true.) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:371 --> mod_wsr88d_forecastmove/destroy: ForecastMov_t already destroyed !!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_non_fatal_error("========================= NON-FATAL =========================", &
                                             "mod_wsr88d_forecastmove/destroy:371 ForecastMov_t already destroyed !!", &
                                              sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          this%m_nss = 0_I32P
          this%m_nsd = 0_I32P
          deallocate(this%m_StormSpeed,   &
                     this%m_StormDir,     &
                     STAT=derr,           &
                     ERRMSG=emsg  )
          if(derr /= 0) then
              if(logging == .true. )  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:389 --> mod_wsr88d_forecastmove/destroy: Memory Deallocation Failure !!")
                  call log_shutdown()
              else if (verbose == .true.)  then
                  call print_fatal_erorr("========================= FATAL ========================="  , &
                                         "mod_wsr88d_forecastmove/destroy:389 -- Memory Deallocation Failure !!" , &
                                         emsg,sdate,stime,__LINE__)
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP  "mod_wsr88d_forecastmove/destroy:389 [FATAL-ERROR] --> Memory Deallocation Failure!!"
          end if
          this%m_isbuilt = .false.
    end subroutine destroy
    
    !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nss
!DIR$ ENDIF
    pure function get_nss(this) result(nss)
          implicit none
          class(ForecastMov_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: nss
          ! Start of executable statemetns
          nss = this%m_nss
    end function get_nss
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nsd
!DIR$ ENDIF
    pure function get_nsd(this) result(nsd)
          implicit none
          class(ForecastMov_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: nsd
          ! Start of executable statemetns
          nsd = this%m_nsd
    end function get_nsd
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_StormSpeed
!DIR$ ENDIF
    pure function get_StormSpeed(this) result(StormSpeed)
          implicit none
          class(ForecastMov_t),  intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: StormSpeed
!DIR$     ATTRIBUTES ALIGN : 64 :: StormSpeed
          ! Start of executable statements
          StormSpeed = this%m_StormSpeed
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_StormDir
!DIR$ ENDIF  
    pure function get_StormDir(this) result(StormDir)
          implicit none
          class(ForecastMov_t),  intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: StormDir
!DIR$     ATTRIBUTES ALIGN : 64 :: StormDir
          ! Start of executable statemetns
          StormDir = this%m_StormDir
    end function get_StormDir
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_isbuilt
!DIR$ ENDIF  
    pure function get_isbuilt(this) result(isbuilt)
          implicit none
          class(ForecastMov_t), intent(in) :: this
          ! Locals
          logical(I32P) :: isbuilt
          ! Start of executable statemetns
          isbuilt = this%m_isbuilt
    end function
    
    !============================================
    !   Read/write procedures
    !============================================
    
     subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(ForecastMov_t), intent(in)    :: this
          character(len=*),     intent(in)    :: form
          integer(I32P),        intent(in)    :: unit
          integer(I32P),        intent(inout) :: ioerr
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
          class(ForecastMov_t), intent(in)    :: this
          character(len=*),     intent(in)    :: form
          integer(I32P),        intent(in)    :: unit
          integer(I32P),        intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
    subroutine dbg_info(this)
          implicit none
          class(ForecastMov_t), intent(in) :: this
          ! Start of executable statemetns
          print*, " Dump 'ForecastMov_t' state!! "
          print*, "==================================================="
          print*, "Collected at: ", __DATE__ , ":" , __TIME__
          print*, "==================================================="
          print*, "Length of StormSpeed array: ", this%m_nss
          print*, "Length of StormDir array:   ", this%m_nsd
          print*, "StormSpeed initialized to:  ", this%m_StormSpeed
          print*, "StormDir   initialized to:  ", this%m_StormDir
          print*, "built status:               ", this%m_isbuilt
          print*, "===================================================="
    end subroutine dbg_info

end module mod_wsr88d_forecastmove