
module mod_wsr88d_hailtalt


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_hailtalt'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Hail Temperature and Altitude product.
 !          History:
 !                        Date: 24-02-2018
 !                        Time: 14:58 GMT+2
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
    ! Tab:10,11 col - Type , function and subroutine code blocks.     dd

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
    integer(I32P), parameter, public :: MOD_WSR88D_HAILTALT_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_HAILTALT_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_HAILTALT_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_HAILTALT_FULLVER = 1000_I32P*MOD_WSR88D_HAILTALT_MAJOR + &
                                                                      100_I32P*MOD_WSR88D_HAILTALT_MINOR  + &
                                                                      10_I32P*MOD_WSR88D_HAILTALT_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_HAILTALT_CREATE_DATE = "24-02-2018 15:08 +00200 (SAT 24 FEB 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_HAILTALT_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_HAILTALT_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSR88D_HAILTALT_DESCRIPT = "WSR-88D Hail Temperature at Altitude"
    
    !===============================
    ! Type: HailTAlt_t
    !===============================
    type, public :: HailTAlt_t
        
          private
          
          integer(I32P) :: m_nht0c, m_nht20c ! ht0c size, ht20c size
          
            ! Access public in order to eliminate unnecessary copy operations.
          real(R64P), allocatable, dimension(:), public :: m_HT0C  !  Kft, 0.0-70.0 0.1   (0C)
          
          real(R64P), allocatable, dimension(:), public :: m_HT20C !  Kft, 0.0-70.0, 0.1  (-20.0C)
          
          logical(I32P) :: m_isbuilt ! built indicator
          
          contains
          
          !=====================================
          ! Construction,copy and destruction
          ! methods.
          !=====================================
    
          ! Constructor - default initialization
          procedure, pass(this), public :: init
          
          ! Copy-Constructor
          procedure, pass(this), public :: copy
          
          ! Move-constructor
          procedure, pass(this), public :: move
          
          ! Destructor
          procedure, pass(this), public :: destroy
          
          !========================================
          !    Getter procedures
          !========================================
          
          procedure, pass(this), public :: get_nht0c
          
          procedure, pass(this), public :: get_nht20c
          
          procedure, pass(this), public :: get_HT0C
          
          procedure, pass(this), public :: get_HT20C
          
          procedure, pass(this), public :: get_isbuilt
          
          !==========================================
          !   Read/write procedurs
          !==========================================
          
          procedure, nopass, public :: read_state
          
          procedure, nopass, public :: write_state
          
          !==========================================
          !   Class helper procedures
          !==========================================
        
          procedure, pass(this), public :: dbg_info
          
    end type HailTAlt_t
          
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
    subroutine init(this,nh0c,nh20c,ierr,logging,verbose,dbg,append,fname)
          use mod_print_error,  only : print_non_fatal_error,   &
                                       print_fatal_error
          use mod_constants,    only : LAM_PINF
          implicit none
          class(HailTAlt_t),    intent(inout) :: this
          integer(I32P),        intent(in)    :: nh0c,nh20c
          integer(I32P),        intent(inout) :: ierr
          logical(I32P),        intent(in)    :: logging, verbose, dbg, append
          character(len=*),     intent(in)    :: fname
          ! Locals
          character(len=40) :: sdate, stime
          character(len=256) :: emsg
          integer(I32P) :: i, aerr
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true. .OR. &
             nh0c <= 0_I32P .OR. nh20c <= 0_I32P ) then
              if(logging == .true.) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:183 --> mod_wsr88d_hailtalt/init: HailTAlt_t already initialized, or invalid array size argument(s) !!")
                  call log_shutdown()
              else if (verbose == .true. ) then
                  call print_non_fatal_error("========================= NON_FATAL =========================", &
                                             "mod_wsr88d_hailtalt/init:183 -- HailTAlt_t already initialized, or invalid array size argument(s)", &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
           end if
           ! Begin construction
           this%m_nh0c  = nh0c
           this%m_nh20c = nh20c
           associate(lh0=>this%m_nh0c,   &
                     lh20=>this%m_nh20c  )
                allocate(this%m_HT0C(lh0),   &
                         this%m_HT20C(lh20), &
                         STAT=aerr,          &
                         ERRMSG=emsg )
           end associate
           if(aerr /= 0) then
               if(logging == .true.) then
                   call log_startup(fname,append)
                   call log_UsrMsg("logger:207 --> mod_wsr88d_hailtalt/init: -- Memory Allocation Failure !!")
                   call log_shutdown()
               else if (verbose == .true. ) then
                   call print_fatal_error("========================= FATAL =========================",  &
                                          "mod_wsr88d_hailtalt/init:207 -- Memory Allocation Failure !!", &
                                          emsg,sdate,stime,__LINE__)
               end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_hailtalt/init:207 -- [FATAL-ERROR] -- Memory Allocation Failure "
           end if
           !Array initilization
!DIR$      SIMD VECTORLENGTHFOR(REAL(KIND=8))
           do i = 1_I32P, this%m_nht0c
               this%m_HT0C(i) = LAM_PINF
           end do
!DIR$      SIMD VECTORLENGTHFOR(REAL(KIND=8))
           do i = 1_I32P, this%m_nht20c
               this%m_HT20C(i) = LAM_PINF
           end do
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
          use mod_print_error, only : print_non_fatal_error
                                     
          implicit none
          class(HailTAlt_t),  intent(inout) :: this
          class(HailTAlt_t),  intent(in)    :: other
          logical(I32P),      intent(in)    :: logging
          character(len=*),   intent(in)    :: fname
          logical(I32P),      intent(in)    :: append, verbose
          integer(I32P),      intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate, stime
          ! Start of executable satteemtns
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true.  .OR. &
             other%m_isbuilt == .false. ) then
              if(logging == .true.)  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:256 --> mod_wsr88d_hailtalt/copy: Invalid argument(s) state !!")
                  call log_shutdown()
              else if (verbose == .true.)  then
                  call print_non_fatal_error("========================= NON-FATAL =========================",  &
                                             "mod_wsr88d_hailtalt/copy:256 -- Invalid argument(s) state !!",   &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin copy-construction
          this%m_nht0c = other%m_nht0c
          this%m_nht20c = other%m_nht20c
          this%m_HT0C = other%m_HT0C
          this%m_HT20C = other%m_HT20C
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
          class(HailTAlt_t),  intent(inout) :: this, other
          logical(I32P),      intent(in)    :: logging
          character(len=*),   intent(in)    :: fname
          logical(I32P),      intent(in)    :: append, verbose
          integer(I32P),      intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate, stime
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true. .OR.  &
             other%m_isbuilt == .false. )  then
              if(logging == .true. )  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:314 --> mod_wsr88d_hailtalt/move: Invalid argument(s) state !!")
                  call log_shutdown()
              else if(verbose == .true.) then
                  call print_non_fatal_error("========================= NON-FATAL =========================" , &
                                             "mod_wsr88d_hailtalt/move:314 -- Invalid argument(s) state !!" , &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
        ! Begin move-construction
          this%m_nht0c = other%m_nht0c
          this%m_nht20c = other%m_nht20c
          call move_alloc(other%m_HT0C, this%m_HT0C)
          call move_alloc(other%m_HT20C, this%m_HT20C)
          other%m_nht0c = 0_I32P
          other%m_nht20c = 0_I32P
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
          use mod_print_error, only : print_non_fatal_error,  &
                                      print_fatal_error
          implicit none
          class(HailTALt_t),  intent(inout) :: this
          logical(I32P),      intent(in)    :: logging
          character(len=*),   intent(in)    :: fname
          logical(I32P),      intent(in)    :: append,verbose
          integer(I32P),      intent(inout) :: ierr
          ! Locals
          character(len=40) :: stime,sdate
          character(len=256) :: emsg
          integer(I32P) :: derr
          ! Start of executable stateemtns
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .false.) then
              if(logging == .true.) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:369 --> mod_wsr88d_hailtalt/destroy: HailTAlt_t already destroyed !!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_non_fatal_error("========================= NON-FATAL ========================="  , &
                                             "mod_wsr88d_hailtalt/destroy:369 -- HailTAlt_t already destroyed !!" , &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          this%m_nht0c = 0_I32P
          this%m_nht20c = 0_I32P
          deallocate(this%m_HT0C,  &
                     this%m_HT20C,  &
                     STAT=derr,     &
                     ERRMSG=emsg)
          if(derr /= 0) then
              if(logging == .true.) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:388 --> mod_wsr88d_hailtalt/destroy -- Memory Deallocation Failure !!")
                  call log_shutdown(0
              else if(verbose == .true.) then
                  call print_fatal_error("========================= FATAL =========================" , &
                                         "mod_wsr88d_hailtalt/destroy:388 -- Memory Deallocation Failure !!"  , &
                                         emsg,sdate,stime,__LINE__)
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_hailtalt/destroy:388 [FATAL-ERROR] --> Memory Deallocation Failure !! "
          end if
          this%m_isbuilt = .false.
    end subroutine destroy
    
    !==========================================
    !       Getter procedures
    !==========================================

!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nht0c
!DIR$   ENDIF 
    pure function get_nht0c(this) result(nht0c)
          implicit none
          class(HailTAlt_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: nht0c
          ! Start of executable statetmetns
          nht0c = this%m_nht0c
    end function get_nht0c
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nht20c
!DIR$   ENDIF 
    pure function get_nht20c(this) result(nht20c)
          implicit none
          class(HailTAlt_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nht20c
          ! Start of executable satemetns
          nht20c = this%m_nht20c
    end function get_nht20c
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_HT0C
!DIR$   ENDIF  
    pure function get_HT0C(this) result(HT0C)
          implicit none
          class(HailTAlt_t),  intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: HT0C
!DIR$     ATTRIBUTES ALIGN : 64 :: HT0C
          ! Strat of executable statements
          HT0C = this%m_HT0C
    end function get_HT0C
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_HT20C
!DIR$   ENDIF 
    pure function get_HT20C(this) result(HT20C)
          implicit none
          class(HailTAlt_t),  intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: HT20C
!DIR$     ATTRIBUTES ALIGN : 64 :: HT20C
          ! Start of executable statements
          HT20C = this%m_HT20C
    end function  get_HT20C
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_isbuilt
!DIR$   ENDIF     
    pure function get_isbuilt(this) result(isbuilt)
          implicit none
          class(HailTAlt_t), intent(in) :: this
          ! Locals
          logical(I32P) :: isbuilt
          ! Start of executable statemetns
          isbuilt = this%m_isbuilt
    end function get_isbuilt
    
    !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(HailTAlt_t), intent(in) :: this
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
          class(HailTAlt_t), intent(in) :: this
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
          implicit none
          class(HailTAlt_t),  intent(in) :: this
          ! Start of executable statemetns
          print*, "           Dump of 'HailTAlt' state !!                 "
          print*, "======================================================="
          print*, "Collected at: ", __DATE__, ":", __TIME__ ,      &
                  "in file: ", __FILE__, "at line: ", __LINE__
          print*, "========================================================"
          print*, "1) Length of HT0C array:           ", this%m_nht0c
          print*, "2) Length of HT20C array:          ", this%m_nht20c
          print*, "3) Array HT0C initialized to INF:  ", this%m_HT0C
          print*, "4) Array HT20C initialized to INF: ", this%m_HT20C
          print*, "5) HailTAlt_t built state:         ", this%m_isbuilt
          print*, "========================================================"
    end subroutine dbg_info
    
end module mod_wsr88d_hailtalt