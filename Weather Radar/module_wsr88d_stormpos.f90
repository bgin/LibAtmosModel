
module mod_wsr88d_stormpos

  !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_stormpos'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Storm Position product.
 !          History:
 !                        Date: 22-02-2018
 !                        Time: 17:59 GMT+2
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
    implicit none
    private
    use module_kinds,    only : I32P, R64P
    use module_logger,   only : log_startup, &
                                 log_UsrMsg,  &
                                 log_shutdown
    !use ISO_FORTRAN_ENV,  only : stderr=>ERROR_UNIT , &
    !                            stdout=>OUTPUT_UNIT
    use IFPORT,           only : TRACEBACKQQ
    
   
    
     
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_WSR88D_STORMPOS_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_STORMPOS_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_STORMPOS_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_STORMPOS_FULLVER = 1000_I32P*MOD_WSR88D_STORMPOS_MAJOR + &
                                                                      100_I32P*MOD_WSR88D_STORMPOS_MINOR  + &
                                                                      10_I32P*MOD_WSR88D_STORMPOS_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_STORMPOS_CREATE_DATE = "22-02-2018 17:59 +00200 (THR 22 FEB 2018 GMT+2) "
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_STORMPOS_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_STORMPOS_AUTHOR_INFO = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSR88D_STORMPOS_DESCRIPT = "WSR-88D Storm Postion product"
    
    ! Module constants
    
    !==================================
    ! Type: StormPos_t
    !==================================
    type, public :: StormPos_t
        
          private
          
          ! azimuth size, range size
          integer(I32P) :: m_na, m_nr
          
          ! Access public in order to eliminate unnecessary copy operations.
          real(R64P), allocatable, dimension(:), public :: m_Azimuth    ! Azimuth, deg, 0.0-359.0, 1.0
          
          real(R64P), allocatable, dimension(:), public :: m_Range      ! Range, nmi, 0.0-248.0, 1.0
          
          logical(I32P) :: m_isbuilt ! build indicator
          
          contains
    
          !=====================================
          ! Construction,copy and destruction
          ! methods.
          !=====================================
    
          ! Constructor for default initialization only
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
          
          procedure, pass(this), public :: get_na
          
          procedure, pass(this), public :: get_nr
          
          procedure, pass(this), public :: get_Azimuth
          
          procedure, pass(this), public :: get_Range
          
          procedure, pass(this), public :: get_isbuilt
          
          !==========================================
          !   Read/write procedurs
          !==========================================
          
          procedure, nopass, public :: read_state
          
          procedure, nopass, public :: write_state
          
           
          !============================================
          !  Class helper procedures
          !============================================
          
          procedure, pass(this), public :: dbg_info
          
    end type StormPos_t
          
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
    subroutine init(this,na,nr,ierr,logging,verbose,dbg,append,fname)
        use mod_print_error, only : print_non_fatal_error,   &
                                    print_fatal_error
        use mod_constants,   only : LAM_PINF
        implicit none
        class(StormPos_t),  intent(inout) :: this
        integer(I32P),      intent(in)    :: na, nr
        integer(I32P),      intent(inout) :: ierr
        logical(I32P),      intent(in)    :: logging, verbose, dbg, append
        character(len=*),   intent(in)    :: fname
        ! Locals
        character(len=40)  :: sdate, stime
        character(len=256) :: emsg
        integer(I32P) :: i, aerr
        ! Strat of eexecutable statements
        if(ierr < 0_I32P) ierr = 0_I32P
        if(this%m_isbuilt == .true.  .OR.  &
           na <= 0_I32P .OR. nr <= 0_I32P  )  then
            if(logging == .true.)    then
                call log_startup(fname, append)
                call log_UsrMsg("logger:188 --> mod_wsr88d_stormpos/init: StormPos_t already initialized, or invalid array size argument(s)!!")
                call shutdown()
            else if (verbose == .true.)  then
                call print_non_fatal_error("========================= NON_FATAL =========================" , &
                                            "mod_wsr88d_stormpos/init:188 --> StormPos_t already initialized, or invalid array size argument(s)", &
                                           sdate,stime,__LINE__)
            end if
            ierr = -1_I32P
            return
        end if
        ! Begin construction
        this%m_na = na
        this%m_nr = nr
        associate(la=>this%m_na,   &
                  lr=>this%m_nr   )
            allocate(this%m_Azimuth(la),  &
                     this%m_Range(lr),    &
                     STAT=aerr,           &
                     ERRMSG=emsg  )
        end associate
        if(aerr /= 0) then
            if(logging == .true. )  then
                call log_startup(fname,append)
                call log_UsrMsg("logger:212 --> mod_wsr88d_stormpos/init: Memory Allocation Failure!!")
                call log_shutdown()
            else if(verbose == .true.)  then
                call print_fatal_error("========================= FATAL ========================="  , &
                                       "mod_wsr88d_stormpos/init:212 --> Memory Allocation Failure!!", &
                                       emsg,sdate,stime,__LINE__)
            end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_stormpos/init:212 [FATAL-ERROR] -- Memory Allocation Failure !!"
        end if
        ! Initialize arrays
!DIR$   SIMD VECTORLENGTHFOR(REAL(KIND=8))
         do i = 1_I32P, this%m_na
              this%m_Azimuth(i) = LAM_PINF
          end do
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1_I32P, this%m_nr
              this%m_Range(i)   = LAM_PINF
          end do
          this%m_isbuilt = .true.
          if(dbg == .true.)  then
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
          class(StormPos_t),   intent(inout) :: this
          class(StormPos_t),   intent(in)    :: other
          logical(I32P),       intent(in)    :: logging
          character(len=*),    intent(in)    :: fname
          logical(I32P),       intent(in)    :: append, verbose
          integer(I32P),       intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate, stime
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true.  .OR.  &
             other%m_isbuilt == .false. )  then
              if(logging == .true.)   then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:271 --> mod_wsr88d_stormpos/copy: Invalid argument(s) state !!")
                  call log_shutdown()
              else if (verbose == .true.)  then
                  call print_non_fatal_error("========================= NON-FATAL =========================" , &
                                             "mod_wsr88d_stormpos/copy:271 --> Invalid argument(s) state !!",  &
                                             sdate,stime,__LINE__ )
              end  if
              ierr = -1_I32P
              return
           end if
           ! Begin copy-construction
           this%m_na = other%m_na
           this%m_nr = other%m_nr
           this%m_Azimuth = other%m_Azimuth
           this%m_Range   = other%m_Range
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
          class(StormPos_t), intent(inout) :: this
          class(StormPos_t), intent(in)    :: other
          logical(I32P),     intent(in)    :: logging
          character(len=*),  intent(in)    :: fname
          logical(I32P),     intent(in)    :: append,verbose
          integer(I32P),     intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate, stime
          ! Start of executable statememts
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true.  .OR.  &
             other%m_isbuilt == .false.    ) then
              if(logging == .true.)   then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:321 --> mod_wsr88d_stormpos/move: Invalid argument(s) state !!")
                  call log_shutdown()
              else if(verbose == .true.)  then
                  call print_non_fatal_error("========================= NON-FATAL =========================" ,  &
                                             "mod_wsr88d_stormpos/move:321 -- Invalid argument(s) state !!",    &
                                              sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
           end if
           ! Begin move-construction
            this%m_na = other%m_na
            this%m_nr = other%m_nr
           call move_alloc(other%m_Azimuth, this%m_Azimuth)
           call move_alloc(other%m_Range,   this%m_Range)
           other%m_na = 0_I32P
           other%m_nr = 0_I32P
           other%m_isbuilt = .false.
           this5m_isbuilt = .true.
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
          class(StormPos_t),  intent(inout) :: this
          logical(I32P),      intent(in)    :: logging
          character(len=*),   intent(in)    :: fname
          logical(I32P),      intent(in)    :: append, verbose
          integer(I32P),      intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          character(len=256) :: emsg
          integer(I32P) :: derr
          ! Start of executable statemetns
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .false.) then
              if(logging == .true.) then
                  call log_stsrtup(fname,append)
                  call log_UsrMsg("logger:377 --> mod_wsr88d_stormpos/destroy: StormPos_t already destroyed !!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_non_fatal_error("========================= NON-FATAL =========================" ,  &
                                             "mod_wsr88d_stormpos/destroy:377 -- StormPos_t already destroyed !!", &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          this%m_na = 0_I32P
          this%m_nr = 0_I32P
          deallocate(this%m_Azimuth,   &
                     this%m_Range,     &
                     STAT=derr,        &
                     ERRMSG=emsg  )
          if(derr /= 0) then
              call log_startup(fname,append)
              call log_UsrMsg("logger:377 --> mod_wsr88d_stormpos/destroy -- Memory Deallocation Failure !!")
              call log_shutdown()
          else if (verbose == .true.)  then
              call print_fatal_error("========================= FATAL =========================" , &
                                     "mod_wsr88d_stormpos/destroy:377 -- Memory Deallocation Failure !!", &
                                     emsg,sdate,stime,__LINE__)
          end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP  "mod_wsr88d_stormpos/destroy:337 [FATAL-ERROR] --> Memory Deallocation Failure!!"
          end if
          this%m_isbuilt = .false.         
    end subroutine destroy
    
     !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_na
!DIR$   ENDIF 
    pure function get_na(this) result(na)
          implicit none
          class(StormPos_t), intent(in) :: this
          ! Locals
          integer(I32P) :: na
          ! Start of executable statements
          na = this%m_na
    end function get_na 
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nr
!DIR$   ENDIF 
    pure function get_nr(this) result(nr)
          implicit none
          class(StormPos_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nr
          ! Start of executable statements
          nr = this%m_nr
    end function get_nr
   
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Azimuth
!DIR$   ENDIF 
    pure function get_Azimuth(this) result(Azimuth)
          implicit none
          class(StormPos_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: Azimuth
!DIR$     ATTRIBUTES ALIGN : 64 :: Azimuth    ! Will it be aligned on 64-bytes boundary I do not think so!
          ! Start of executable statements
          Azimuth = this%m_Azimuth
    end function  get_Azimuth
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Range
!DIR$   ENDIF 
    pure function get_Range(this) result(Range)
          implicit none
          class(StormPos_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: Range
!DIR$     ATTRIBUTES ALIGN : 64 :: Range
          ! Start of executable sattements
          Range = this%m_Range
    end function get_Range
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_isbuilt
!DIR$   ENDIF 
    pure function get_isbuilt(this) result(isbuilt)
          implicit none
          class(StormPos_t), intent(in) :: this
          ! Locals
          logical(I32P) :: isbuilt
          ! Start of executable sattements
          isbuilt = this%m_isbuilt
    end function get_isbuilt
    
    !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(StormPos_t), intent(in) :: this
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
          class(StormPos_t), intent(in) :: this
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
          class(StormPos_t),  intent(in) :: this
          ! Start of executable statements
           print*, " Dump 'StormPos_t' state!!"
           print*, "============================================"
           print*, " Length of Range:        ", this%m_nr
           print*, " Length of Azimuth:      ", this%m_na
           print*, " Azimuth initialization: ", this%m_Azimuth
           print*, " Range   initialization: ", this%m_Range
           print*, " Built status:           ", this%m_isbuilt
           print*, "============================================="
    end subroutine dbg_print
        
end module mod_wsr88d_stormpos