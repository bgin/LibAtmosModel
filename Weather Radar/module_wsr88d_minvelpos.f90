
module mod_wsr88d_minvelpos

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_minvelpos'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Minimum Velocity Position product.
 !          History:
 !                        Date: 18-02-2018
 !                        Time: 15:49 GMT+2
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
      use module_kinds,   only : I32P, R64P
      use module_logger,  only : log_startup,  &
                                 log_UsrMsg,   &
                                 log_shutdown
      use IFPORT,         only : TRACEBACKQQ
      
      !=====================================================59
      !  File and module information:
      !  version,creation and build date, author,description
      !=====================================================59
     
      ! Mahjor version
      integer(I32P), parameter, public :: MOD_WSR88D_MINVELPOS_MAJOR = 1_I32P
      
      ! Minor version
      integer(I32P), parameter, public :: MOD_WSR88D_MINVELPOS_MINOR = 0_I32P
      
      ! Micro version
      integer(I32P), parameter, public :: MOD_WSR88D_MINVELPOS_MICRO = 0_I32P
      
      ! Module full version
      integer(I32P), parameter, public :: MOD_WSR88D_MINVELPOS_FULLVER = 1000_I32P*MOD_WSR88D_MINVELPOS_MAJOR + &
                                                                         100_I32P*MOD_WSR88D_MINVELPOS_MINOR  + &
                                                                         10_I32P*MOD_WSR88D_MINVELPOS_MICRO
      
      ! Module creation date
      character(*),  parameter, public :: MOD_WSR88D_MINVELPOS_CREATE_DATE = "18-02-2018 15:49 +00200 (SUN 18 FEB 2018 GMT+2) "
      
      ! Module build date (should be set after successful compilation)
      character(*),  parameter, public :: MOD_WSR88D_MINVELPOS_BUILD_DATE = " "
      
      ! Module author info
      character(*),  parameter, public :: MOD_WSR88D_MINVELPOS_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
      
      ! Module short description
      character(*),  parameter, public :: MOD_WSR88D_MINVELPOS_DESCRIPT = "WSR-88D Minimum velocity and position product."
      
      !========================
      ! Type: MinVelPos_t
      !========================
      type, public :: MinVelPos_t
          
            private
            
            integer(I32P) :: m_na, m_nr  ! Azimuth array size, and Range array size
            
            ! Public access in order to eliminate  unnecesary copy operations
            
            real(R64P), allocatable, dimension(:), public :: m_Azimuth !   Azimuth, deg, 0.0-359.0, 1.0
            
            real(R64P), allocatable, dimension(:), public :: m_Range   !   Range, nmi, 0.0-124.0, 1.0
            
            logical(I32P) :: m_isbuilt ! built indicator
            
            contains
      
           !=====================================
           ! Construction,copy and destruction
           ! methods.
           !=====================================
      
           ! Constructor performs default initialization (to INF) only
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
          
           procedure, pass(this), private :: dbg_info
           
    end type MinVelPos_t
            
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
          use mod_print_error, only : print_non_fatal_error,  &
                                      print_fatal_error
          use mod_constants,   only : LAM_PINF
          implicit none
          class(MinVelPos_t),  intent(inout) :: this
          integer(I32P),       intent(in)    :: na, nr
          integer(I32P),       intent(inout) :: ierr
          logical(I32P),       intent(in)    :: logging, verbose, dbg, append
          character(len=*),    intent(in)    :: fname
          ! Locals
          character(len=40)  :: sdate, stime
          character(len=256) :: emsg
          integer(I32P) :: i, aerr
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true. .OR.  &
             na <= 0_I32P .OR. nr <= 0_I32P) then
              if(logging == .true.) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:179 --> mod_wsr88d_minvelpos/init: MinVelPos_t already initialzied, or invalid array size argument(s)!!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_non_fatal_error("========================= NON_FATAL =========================", &
                                             "mod_wsr88d_minvelpos/init:179 -- MinVelPos_t already initialzied, or invalid array size argument(s)!!",
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
           end if
           ! Begin construction
           this%m_na = na
           this%m_nr = nr
           associate(la=>this%m_na,   &
                     lr=>this%m_nr  )
               allocate(this%m_Azimuth(la),  &
                        this%m_Range(lr),    &
                        STAT=aerr,           &
                        ERRMSG=emsg  )
          end associate
          if(aerr /= 0) then
              if(logging == .true.) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:203 --> mod_wsr88d_minvelpos/init: Memory Allocation Failure !!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_fatal_error("========================= FATAL =========================" , &
                                         "mod_wsr88d_minvelpos/init:203 -- Memory Allocation Failure !!" , &
                                         emsg,sdate,stime,__LINE__)
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_minvelpos/init:203 -- [FATAL-ERROR] -- Memory Allocation Failure!!"
          end if
          ! Initialize arrays
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1_I32P, this%m_na
              this%m_Azimuth(i) = LAM_PINF
          end do
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1_I32P, this%m_nr
              this%m_Range(i)  = LAM_PINF
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
    subroutine copy(this,other,logging,fname,append,verbose,ierr)
          use mod_print_error, only : print_non_fatal_error,  &
                                      print_fatal_error
          implicit none
          class(MinVelPos_t), intent(inout) :: this
          class(MinVelPos_t), intent(in)    :: other
          logical(I32P),      intent(in)    :: logging
          character(len=*),   intent(in)    :: fname
          logical(I32P),      intent(in)    :: append, verbose
          integer(I32P),      intent(inout) :: ierr
          ! Locals
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true.  .OR.  &
             other%m_isbuilt == .false.  )  then
              if(logging == .true.)  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:259 --> mod_wsr88d_minvelpos/init: Invalid argument(s) state !!")
                  call log_shutdown()
              else if (verbose == .true. ) then
                  call print_non_fatal_error("========================= NON_FATAL =========================",  &
                                             "mod_wsr88d_minvelpos/init:259 -- Invalid argument(s) state !!" , &
                                             sdate,stime,__LINE__)
              end if
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
    !            moving of object state.
    !            Relying on call to move_alloc function.
    !            Scalar members (of other) are set to zero and array
    !            members are deallocated
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
          use mod_print_error, only : print_non_fatal_error,   &
                                      print_fatal_error
          implicit none
          class(MinVelPos_t), intent(inout) :: this
          class(MinVelPos_t), intent(inout) :: other
          logical(I32P),      intent(in)    :: logging
          character(len=*),   intent(in)    :: fname
          logical(I32P),      intent(in)    :: append, verbose
          integer(I32P),      intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate, stime
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true.  .OR.   &
             other%m_isbuilt == .false.   ) then
              if(logging == .true. )  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:316 --> mod_wsr88d_minvelpos/move -- Invalid argument(s) state !!")
                  call log_shutdown()
              else if (verbose == .true. )  then
                  call print_non_fatal_error("========================= NON_FATAL =========================" , &
                                             "mod_wrs88d_minvelpos/move:316 -- Invalid argument(s) state !!",  &
                                             sdate, stime, __LINE__ )
              end if
              ierr = -1_I32P
              return
          end if
          this%m_na = other%m_na
          this%m_nr = other%m_nr
          ! Call move_alloc
          call move_alloc(other%m_Azimuth,this%m_Azimuth)
          call move_alloc(other%m_Range,this%m_Range)
          ! Nullify other scalar varaibles
          other%m_na = 0_I32P
          other%m_nr = 0_I32P
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
          class(MinVelPos_t),  intent(inout) :: this
          logical(I32P),       intent(in)    :: logging
          character(len=*),    intent(in)    :: fname
          logical(I32P),       intent(in)    :: append, verbose
          integer(I32P),       intent(inout) :: ierr
          ! Locals
          character(len=40)  :: sdate, stime
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .false.)  then
              if(logging == .true.)  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:312 --> mod_wsr88d_minvelpos/init: MinVelPos_t already destroyed!!")
                  call log_shutdown()
              else if (verbose == .true.)  then
                  call print_non_fatal_error("========================= NON-FATAL =========================" ,   &
                                             "mod_wsr88d_minvelpos/init:312 -- MinVelPos_t already destroyed!!", &
                                             sdate, stime, __LINE__)
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
          if(derr /= 0)  then
              if(logging == .true.  ) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:331 --> mod_wsr88d_minvelpos/init: Memory Deallocation Failure!!")
                  call log_shutdown()
              else if (verbose == .true.)  then
                  call print_fatal_error("========================= FATAL =========================", &
                                         "mod_wsr88d_minvelpos/init:331 -- Memory Deallocation Failure !!",  &
                                         emsg,sdate,stime,__LINE__ )
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF            
              ERROR STOP "mod_wsr88d_minvelpos/init:331 -- [FATAL-ERROR] --> Memory Deallocation Failure !!"
          end if
          this%m_isbuilt = .false.
    end subroutine  destroy
    
    !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_na
!DIR$   ENDIF 
    pure function get_na(this) result(na)
          implicit none
          class(MinVelPos_t), intent(in) :: this
          ! Locals
          integer(I32P) :: na
          ! Start of executable sattemetns
          na = this%m_na
    end function get_na
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nr
!DIR$   ENDIF 
    pure function get_nr(this) result(nr)
          implicit none
          class(MinVelPos_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nr
          ! Start of executable sattements
          nr = this%m_nr
    end function  get_nr
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Azimuth
!DIR$   ENDIF 
    pure function get_Azimuth(this)  result(Azimuth)
          implicit none
          class(MinVelPos_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: Azimuth
!DIR$     ATTRIBUTES ALIGN : 64 :: Azimuth
          ! Start of executbale statements
          Azimuth = this%m_Azimuth
    end function  get_Azimuth
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Range
!DIR$   ENDIF 
     pure function get_Range(this)  result(Range)
          implicit none
          class(MinVelPos_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: Range
!DIR$     ATTRIBUTES ALIGN : 64 :: Range
          ! Start of executbale statements
          Range = this%m_Range
     end function  get_Range
     
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_isbuilt
!DIR$   ENDIF 
     pure function get_isbuilt(this)  result(isbuilt)
          implicit none
          class(MinVelPos_t),  intent(in) :: this
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
          class(MinVelPos_t), intent(in) :: this
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
          class(MinVelPos_t), intent(in) :: this
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
          class(MinVelPos_t), intent(in) :: this
          ! Start of executable statements
           print*, " Dump 'MinVelPos_t' state!!"
           print*, "============================================"
           print*, " Length of Range:        ", this%m_nr
           print*, " Length of Azimuth:      ", this%m_na
           print*, " Azimuth initialization: ", this%m_Azimuth
           print*, " Range   initialization: ", this%m_Range
           print*, " Built status:           ", this%m_isbuilt
           print*, "============================================="
    end subroutine dbg_info
    
    
end module mod_wsr88d_minvelpos