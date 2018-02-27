
module mod_wsr88d_forecasterr

  !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_forecasterr'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Forecast Error product.
 !          History:
 !                        Date: 25-02-2018
 !                        Time: 12:51 GMT+2
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
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTERR_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTERR_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTERR_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTERR_FULLVER = 1000_I32P*MOD_WSR88D_FORECASTERR_MAJOR + &
                                                                         100_I32P*MOD_WSR88D_FORECASTERR_MINOR  + &
                                                                         10_I32P*MOD_WSR88D_FORECASTERR_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_FORECASTERR_CREATE_DATE = "25-02-2018 12:59 +00200 (SUN 25 FEB 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_FORECASTERR_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_FORECASTERR_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSR88D_FORECASTERR_DESCRIPT = "WSR-88D Forecast Error product"
    
    !=============================
    ! Type: ForecastErr_t
    !=============================
    type, public :: ForecastErr_t
        
          private
          
          integer(I32P) :: m_nerr, m_nmean ! size of array m_Err, size of array m_Mean
          
          ! Public access in order to eliminate  unnecesary copy operations
          
          real(R64P), allocatable, dimension(:), public :: m_Err  ! Error, nmi, 0.0-99.0, 0.1
          
          real(R64P), allocatable, dimension(:), public :: m_Mean ! Mean, nmi, 0.0-99.0, 0.1
          
          logical(I32P) :: m_isbuilt ! built status
          
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
          
          ! Destructor
          procedure, pass(this), public :: destroy
          
          !========================================
          !    Getter procedures
          !========================================
          
          procedure, pass(this), public :: get_nerr
          
          procedure, pass(this), public :: get_nmean
          
          procedure, pass(this), public :: get_Err
          
          procedure, pass(this), public :: get_Mean
          
          procedure, pass(this), public :: get_isbuilt
          
          !==========================================
          !   Read/write procedures
          !==========================================
          
          procedure, nopass, public :: read_state
          
          procedure, nopass, public :: write_state
          
          !============================================
          !  Class helper procedures
          !============================================
          
          procedure, pass(this), public :: dbg_info
          
    end type ForecastErr_t
          
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
    subroutine init(this,nerr,nmean,ierr,logging,verbose,dbg,append,fname)
          use mod_print_error, only : print_non_fatal_error,  &
                                      print_fatal_error
          use mod_constants,   only : LAM_PINF
          implicit none
          class(ForecastErr_t),  intent(inout) :: this
          integer(I32P),         intent(in)    :: nerr,nmean
          integer(I32P),         intent(inout) :: ierr
          logical(I32P),         intent(in)    :: logging,verbose,dbg,append
          character(len=*),      intent(in)    :: fname
          ! Locals
          character(len=40) :: sdate, stime
          character(len=256) :: emsg
          integer(I32P) :: i , aerr
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true. .OR. &
             nerr <= 0_I32P .OR. nmean <= 0_I32P ) then
              if(logging == .true.)  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:182 --> mod_wsr88d_forecasterr/init: ForecastErr_t already initialized, or invalid array size argument(s) !!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_non_fatal_error("========================= NON_FATAL =========================" , &
                                             " mod_wsr88d_forecasterr/init:182 -- ForecastErr_t already initialized, or invalid array size argument(s) !!" , &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin construction
          this%m_nerr  = nerr
          this%m_nmean = nmean
          associate(le=>this%m_nerr,  &
                    lm=>this%m_nmean )
               allocate(this%m_Err(le),  &
                        this%m_Mean(lm), &
                        STAT=aerr,       &
                        ERRMSG=emsg )
          end associate
          if(aerr /= 0) then
              if(logging == .true.)  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:206 --> mod_wsr88d_forecasterr/init: Memory Allocation Failure !!")
                  call log_shutdown()
              else if (verbose == .true. ) then
                  call print_fatal_error("========================= FATAL =========================", &
                                         "mod_wsr88d_forecasterr/init:206 Memory Allocation Failure !!", &
                                         errmsg,sdate,stime,__LINE__ )  
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_forecasterr/init:206 [FATAL-ERROR] -- Memory Allocation Failure !!"
          end if
          !Initialize arrays
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1_I32P, this%m_nerr
              this%m_Err(i) = LAM_PINF
          end do
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1_I32P, this%m_nmean
              this%m_Mean(i) = LAM_PINF
          end do
          this%m_isbuilt = .true.
          if(dbg == .true. ) then
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
          use mod_print_error, only : print_non_fatal_error
          implicit none
          class(ForecastErr_t),  intent(inout) :: this
          class(ForecastErr_t),  intent(in)    :: other
          logical(I32P),         intent(in)    :: logging
          character(len=*),      intent(in)    :: fname
          logical(I32P),         intent(in)    :: append,verbose
          integer(I32P),         intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          ! Strat of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true. .OR.  &
             other%m_isbuilt == .false. ) then
              if(logging == .false.)  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:264 --> mod_wsr88d_forecasterr/copy: Invalid argument(s) state !!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_non_fatal_error("========================= NON-FATAL =========================" , &
                                             "mod_wsr88d_forecasterr/copy:264 -- Invalid argument(s) state" ,  &
                                             sdate,stime,__LINE__ )
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin copy-construction
          this%m_nerr  = other%m_nerr
          this%m_nmean = other%m_nnmean
          this%m_Err   = other%m_Err
          this%m_Mean  = other%m_Mean
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
          class(ForecastErr_t),  intent(inout) :: this
          class(ForecastErr_t),  intent(inout) :: other
          logical(I32P),         intent(in)    :: logging
          character(len=*),      intent(in)    :: fname
          logical(I32P),         intent(in)    :: append,verbose
          integer(I32P),         intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true. .OR. &
             other%m_isbuilt == .false. )  then
              if(logging == .true. )  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:314 --> mod_wsr88d_forecasterr/move: Invalid argument(s) state !!")
                  call log_shutdown()
              else if (verbose == .true. ) then
                  call print_non_fatal_error("========================= NON-FATAL =========================",  &
                                             "mod_wsr88d_forecasterr/move:314 -- Invalid argument(s) state !!" , &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin move-construction
          this%m_nerr  = other%m_nerr
          this%m_nmean = other%m_nmean
          call move_alloc(other%m_Err, this%m_Err)
          call move_alloc(other%m_Mean, this%m_Mean)
          other%m_nerr = 0_I32P
          other%m_nmean = 0_I32P
          other%m_isbuilt = .false.
          this%m_isbuilt  = .true.
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
          use mod_print_error, only : print_non_fatal_error,  &
                                      print_fatal_error
          implicit none
          class(ForecastErr_t),  intent(inout) :: this
          logical(I32P),         intent(in)    :: logging
          character(len=*),      intent(in)    :: fname
          logical(I32P),         intent(in)    :: append, verbose
          integer(I32P),         intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate, stime
          character(len=256) :: emsg
          integer(I32P) :: derr
          ! Start of executable statemetns
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .false. ) then
              if(logging == .true. ) then
                  call log_startup(fname,sppend)
                  call log_UsrMsg("logger:369 --> mod_wsr88d_forecasterr/destroy: ForecastErr_t already destroyed !!")
                  call log_shutdown()
              else if (verbose == .true. ) then
                  call print_non_fatal_error("========================= NON-FATAL =========================" , &
                                             "mod_wsr88d_forecasterr/destroy:369 -- ForecastErr_t already destroyed!!",  &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          this%m_nerr = 0_I32P
          this%m_nmean = 0_I32P
          deallocate(this%m_Err,  &
                     this%m_Mean, &
                     STAT=derr,   &
                     ERRMSG=emsg )
          if(derr /= 0) then
              if(logging == .true. ) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:388 -- mod_wsr88d_forecasterr/destroy: Memory Deallocation Failure !!")
                  call log_shutdown()
              else if (verbose == .true. ) then
                  call print_fatal_error("========================= FATAL =========================" , &
                                         "mod_wsr88d_forecasterr/destroy:388 -- Memory Deallocation Failure !!", &
                                         emsg,sdate,stime,__LINE__)
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF 
              ERROR STOP "mod_wsr88d_forecasterr/destroy:388 [FATAL-ERROR] --> Memory Deallocation Failure !!"
          end if
          this%m_isbuilt = .false.
    end subroutine destroy
    
    !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nerr
!DIR$   ENDIF 
    pure function get_nerr(this) result(nerr)
          implicit none
          class(ForecastErr_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: nerr
          ! Start of executable statements
          nerr= this%m_nerr
    end function  get_nerr
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nmean
!DIR$   ENDIF  
    pure function get_nmean(this) result(nmean)
          implicit none
          class(ForecastErr_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: nmean
          ! Start of executable statement
          nmean = this%m_nmean
    end function  get_nmean
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Err
!DIR$   ENDIF 
    pure function get_Err(this) result(Err)
          implicit none
          class(ForecastErr_t),  intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: Err
!DIR$     ATTRIBUTES ALIGN : 64 :: Err
          ! Start of executable statements
          Err = this%m_Err
    end function get_Err
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Mean
!DIR$   ENDIF 
    pure function get_Mean(this) result(Mean)
          implicit none
          class(ForecastErr_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: Mean
!DIR$     ATTRIBUTES ALIGN : 64 :: Mean
          ! Start of executable statements
          Mean = this%m_Mean
    end function get_Mean
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_isbuilt
!DIR$   ENDIF  
    pure function get_isbuilt(this) result(isbuilt)
          implicit none
          class(ForecastErr_t), intent(in) :: this
          ! Locals
          logical(I32P) :: isbuilt
          ! Start of executable statements
          isbuilt = this%m_isbuilt
    end function
    
    !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(ForecastErr_t), intent(in) :: this
          character(len=*),     intent(in) :: form
          integer(I32P),        intent(in) :: unit
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
          class(ForecastErr_t), intent(in) :: this
          character(len=*),     intent(in) :: form
          integer(I32P),        intent(in) :: unit
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
          class(ForecastErr_t),  intent(in) :: this
          ! Start of executable statements
          print*, "          ***Dump of 'ForecastErr_t' state*** !!"
          print*, "=========================================================="
          print*, "Collected on: ", __DATE__,":", __TIME__ ,  &
                  "in file: ", __FILE__, "at line: ", __LINE__
          print*, "=========================================================="
          print*, "Length of m_Err array:           ", this%m_nerr
          print*, "Length of m_Mean array:          ", this%m_nmean
          print*, "Array m_Err initialized to INF:  ", this%m_Err
          print*, "Array m_Mean initialized to INF: ", this%m_Mean
          print*, "built status:                    ", this%m_isbuilt
          print*, "=========================================================="
    end subroutine dbg_info

end module mod_wsr88d_forecasterr