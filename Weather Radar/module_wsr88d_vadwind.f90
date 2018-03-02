
module mod_wsr88d_vadwind

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_vadwind'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  VAD Wind Profile product.
 !          History:
 !                        Date: 28-02-2018
 !                        Time: 17:56 GMT+2
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
    integer(I32P), parameter, public :: MOD_WSR88D_VADWIND_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_VADWIND_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_VADWIND_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_VADWIND_FULLVER = 1000_I32P*MOD_WSR88D_VADWIND_MAJOR + &
                                                                     100_I32P*MOD_WSR88D_VADWIND_MINOR  + &
                                                                     10_I32P*MOD_WSR88D_VADWIND_MICRO
    
    ! Module creation date
    character(*), parameter, public :: MOD_WSR88D_VADWIND_CREATE_DATE = "28-02-2018 18:03 +00200 (WED 28 FEB 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation of this module)
    character(*), parameter, public :: MOD_WSR88D_VADWIND_BUILD_DATE = " "
    
    ! Module author info
    character(*), parameter, public :: MOD_WSR88D_VADWIND_AUTHOR = "Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*), parameter, public :: MOD_WSR88D_VADWIND_DESCRIPT = "WSR-88D VAD Wind profile product."
    
    !===================================
    ! Type: VADWind_t
    !===================================
    type, public :: VADWind_t
        
          private
          
          ! Indexing variables of wind fields conform to WRF memory indexing
          ! i.e. ims:ime,kms:kme,jms:jme
          integer(I32P) :: ims,ime,kms,kme,jms,jme
          
          integer(I32P) :: m_sdim1
          
          ! Public array member fields in order to eliminate unnecessary copy operations
          
          real(R64P), allocatable, dimension(:), public     :: m_Alt !  ! Altitude,  100ft,  0.0-700.0,        1.0
          
          real(R64P), allocatable, dimension(:,:,:), public :: m_U !   U,        m/s,   -127.0-126.0,  0.1
          
          real(R64P), allocatable, dimension(:,:,:), public :: m_V !   V,        m/s,   -127.0-126.0,  0.1
          
          real(R64P), allocatable, dimension(:,:,:), public :: m_W !   W,     cm/s   -999.9-9999.9, 0.1   
          
          real(R64P), allocatable, dimension(:), public     :: m_Dir !  DIR       deg,     0-360,       1
          
          real(R64P), allocatable, dimension(:), public     :: m_SPD !  SPD       knots,   0-999,       1
          
          real(R64P), allocatable, dimension(:), public     :: m_RMS !  RMS       knots,   0-30.0,      0.1
          
          real(R64P), allocatable, dimension(:), public     :: m_SRNG ! SRNG    nm,      0-124.0,     0.01  
          
          real(R64P), allocatable, dimension(:), public     :: m_ELEV ! deg,     -1.0-45.0,   0.1
          
          logical(I32P) :: m_isbuilt
          
          contains
    
          !=====================================
          ! Construction,copy and destruction
          ! methods.
          !=====================================
     
          ! Constructor for default initialization only
          procedure, pass(this), public :: init
    
          ! Copy-Constructor
          procedure, pass(this), public :: copy
          
          ! Move-Constructor (simulated)
          procedure, pass(this), public :: move
          
          ! Destructor
          procedure, pass(this), public :: destroy
          
          !========================================
          !    Getter procedures  for scalar 
          !    members only.
          !========================================
          
          procedure, pass(this), public :: get_ims
          
          procedure, pass(this), public :: get_ime
          
          procedure, pass(this), public :: get_kms
          
          procedure, pass(this), public :: get_kme
          
          procedure, pass(this), public :: get_jms
          
          procedure, pass(this), public :: get_jme
          
          procedure, pass(this), public :: get_sdim1
          
          procedure, pass(this), public :: get_isbuilt
          
         
          
          !==========================================
          !   Read/write procedures
          !==========================================
          
          procedure, nopass, public :: read_state
          
          procedure, nopass, public ::  write_state
          
          !============================================
          !  Class helper procedures
          !============================================
          
          procedure, pass(this), public :: dbg_info
          
    end type VADWind_t
          
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
    !            NO ERROR CHECKING OF ARRAY SIZE VARIABLES
    !            RESPONSIBILITY OF THE CALLER!!
    !            Values of variable 'err'
    !   1) -1 -- Object built already  or invalid argument
    !   2) -2 -- Invalid argument (any of them)  ! Not used here
    !=================================================!
    subroutine init(this,ims,ime,kms,kme,jms,jme,sdim1, &
                    ierr,logging, verbose,dbg,append,fname     )
                              
          use mod_print_error, only : print_non_fatal_error,  &
                                      print_fatal_error
          use mod_constants,   only : LAM_PINF
          implicit none
          class(VADWind_t),   intent(inout) :: this
          integer(I32P),      intent(in)    :: ims,ime,kms,  &
                                               kme,jms,jme
          integer(I32P),      intent(in)    :: sdim1
          integer(I32P),      intent(inout) :: ierr
          logical(I32P),      intent(in)    :: logging,verbose,dbg,append
          character(len=*),   intent(in)    :: fname
          ! Locals
          character(len=40)  :: sdate,stime
          character(len=256) :: emsg
          integer(I32P)      :: i,j,k,aerr
          ! Start of executable ststements
          if(ierr < 0_I32P) ierr = 0_I32P
          if( this%m_isbuilt == .true.              .OR.   &
              (ims == 0_I32P   .AND. ime == 0_I32P) .OR.   &
              (kms == 0_I32P   .AND. kme == 0_I32P) .OR.   &
              (jms == 0_I32P   .AND. jme == 0_I32P) .OR.   &
               sdim1 <= 0_I32P                       ) then
               if(logging == .true.)  then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:224 --> mod_wsr88d_vadwind/init: VADWind_t already initialized, or invalid arrays size argument(s)!! ")
                  call log_shutdown()
               else if (verbose == .true. ) then
                  call print_non_fatal_error("========================= NON_FATAL =========================", &
                                             "mod_wsr88d_vadwind/init:224 -- VADWind_t already initialized, or invalid arrays size argument(s)!!", &
                                             sdate,stime,__LINE__ )
               end if
              ierr = -1_I32P
              return
         end if
          ! Begin construction
          this%m_ims = ims;this%m_ime = ime
          this%m_kms = kms;this%m_kme = kme
          this%m_jme = jme;this%m_jme = jme
          this%m_sdim1=sdim1
        associate(d1s=>this%m_ims,  &
                             d1e=>this%m_ime,  &
                             d2s=>this%m_kms,  &
                             d2e=>this%m_kme,  &
                             d3s=>this%m_jms,  &
                             d3e=>this%m_jme,  &
                             sd1=>this%m_sdim1  )
              
               allocate(this%m_Alt(al),                    &
                        this%m_U(d1s:d1e,d2s:d2e,d3s:d3e), &
                        this%m_V(d1s:d1e,d2s:d2e,d3s:d3e), &
                        this%m_W(d1s:d1e,d2s:d2e,d3s:d3e), &
                        this%m_Dir(sd1),                    &
                        this%m_SPD(sd1),                    &
                        this%m_RMS(sd1),                    &
                        this%m_SRNG(sd1),                  &
                        this%m_ELEV(sd1),                   &
                        STAT=aerr,                         &
                        ERRMSG=emsg    )
        end associate 
        if(aerr /= 0) then
            if(logging == .true.)  then
                call log_startup(fname,append)
                call log_UsrMsg("logger:275 --> mod_wsr88d_vadwind/init: Memory Allocation Failure !!")
                call log_shutdown()
            else if (verbose == .true.)  then
                call print_fatal_error("========================= FATAL =========================" , &
                                       "mod_wsr88d_vadwind/init:275 -- Memory Allocation Failure !! ",     &
                                       emsg,sdate,stime,__LINE__ )
            end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_vadwind/init:275 [FATAL-ERROR] -- Memory Allocation Failure !!"
        end if
        !Initialize arrays
!DIR$   SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i = 1_I32P, this%m_sdim1
            this%m_Alt(i) = LAM_PINF
        end do
        do j = this%m_jms, this%m_jme
            do k = this%m_kms, this%m_kme
!DIR$       SIMD VECTORLENGTHFOR(REAL(KIND=8))
                do i = this%m_ims, this%m_ime
                    this%m_U(i,k,j) = LAM_PINF
                    this%m_V(i,k,j) = LAM_PINF
                    this%m_W(i,k,j) = LAM_PINF
                end do
            end do
        end do
!DIR$   SIMD VECTORLENGTHFOR(REAL(KIND=8))
        do i = 1, this%m_sdim1
            this%m_Dir(i)  = LAM_PINF
            this%m_SPD(i)  = LAM_PINF
            this%m_RMS(i)  = LAM_PINF 
            this%m_SRNG(i) = LAM_PINF
            this%m_ELEV(i) = LAM_PINF
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
          use mod_print_error, only : print_non_fatal_error
          implicit none
          class(VADWind_t),  intent(inout) :: this
          class(VADWind_t),  intent(in)    :: other
          logical(I32P),     intent(in)    :: logging
          character(len=*),  intent(in)    :: fname
          logical(I32P),     intent(in)    :: append, verbose
          integer(I32P),     intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          ! Start of execuatble statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true. .OR.  &
             other%m_isbuilt == .false.   ) then
              if(logging == .true. ) then
                 call log_startup(fname,append)
                 call log_UsrMsg("logger:329 --> mod_wsr88d_vadwind/copy -- Invalid argument(s) state !!")
                 call log_shutdown()
              else if (verbose == .true.) then
                 call print_non_fatal_error("========================= NON-FATAL =========================" , &
                                            " mod_wsr88d_vadwind/copy -- Invalid argument(s) state !!" ,      &
                                            sdate,stime,__LINE__ )
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin copy-construction   
          this%m_ims   = other%m_ims
          this%m_ime   = other%m_ime
          this%m_kms   = other%m_kms
          this%m_kme   = other%m_kme
          this%m_jms   = other%m_jms
          this%m_jme   = other%m_jme
          this%m_sdim1 = other%m_sdim1
          this%m_Alt   = other%m_Alt
          this%m_U     = other%m_U
          this%m_V     = other%m_V
          this%m_W     = other%m_W
          this%m_Dir   = other%m_Dir
          this%m_SPD   = other%m_SPD
          this%m_RMS   = other%m_RMS
          this%m_SRNG  = other%m_SRNG
          this%m_ELEV  = other%m_ELEV
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
          class(VADWind_t),  intent(inout) :: this
          class(VADWind_t),  intent(inout) :: other
          logical(I32P),     intent(in)    :: logging
          character(len=*),  intent(in)    :: fname
          logical(I32P),     intent(in)    :: append,verbose
          integer(I32P),     intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
           ! Start of executable sattements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true.  .OR.  &
             other%m_isbuilt == .false.  ) then
              if(logging == .true. ) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:391 --> mod_wsr88d_vadwind/move: Invalid argument(s) state !!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_non_fatal_error("========================= NON-FATAL =========================" , &
                                             "mod_wsr88d_vadwind/move:391 -- Invalid argument(s) state !! ",   &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin move construction
          this%m_ims   = other%m_ims
          this%m_ime   = other%m_ime
          this%m_kms   = other%m_kms
          this%m_kme   = other%m_kme
          this%m_jms   = other%m_jms
          this%m_jme   = other%m_jme
          this%m_sdim1 = other%m_sdim1
          call move_alloc(other%m_Alt, this%m_Alt)
          call move_alloc(other%m_U,   this%m_U)
          call move_alloc(other%m_V,   this%m_V)
          call move_alloc(other%m_W,   this%m_W)
          call move_alloc(other%m_Dir, this%m_Dir)
          call move_alloc(other%m_SPD, this%m_SPD)
          call move_alloc(other%m_RMS, this%m_RMS)
          call move_alloc(other%m_SRNG,this%m_SRNG)
          call move_alloc(other%m_ELEV,this%m_ELEV)
          other%m_ims = 0_I32P
          other%m_ime = 0_I32P
          other%m_kms = 0_I32P
          other%m_kme = 0_I32P
          other%m_jms = 0_I32P
          other%m_jme = 0_I32P
          other%m_sdim1 = 0_I32P
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
          class(VADWind_t),  intent(inout) :: this
          logical(I32P),     intent(in)    :: logging
          character(len=*),  intent(in)    :: fname
          logical(I32P),     intent(in)    :: append, verbose
          integer(I32P),     intent(inout) :: ierr
          ! Locals
          character(len=40)  :: sdate,stime
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable statemetns
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .false. ) then
              if(logging == .true. ) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:463 --> mod_wsr88d_vadwind/destroy: VADWind_t already destroyed !!")
                  call log_shutdown()
              else if (verbose == .true.)  then
                  call print_non_fatal_error("========================= NON-FATAL =========================",  &
                                             "mod_wsr88d_vadwind/destroy:463 -- VADWind_t already destroyed !!" , &
                                             sdate,stime,__LINE__)
              end if
              ierr = -1_I32P
              return
          end if
          this%m_ims = 0_I32P
          this%m_ime = 0_I32P
          this%m_kms = 0_I32P
          this%m_kme = 0_I32P
          this%m_jms = 0_I32P
          this%m_jme = 0_I32P
          this%m_sdim1 = 0_I32P
          deallocate(this%m_Alt,  &
                     this%m_U,    &
                     this%m_V,    &
                     this%m_W,    &
                     this%m_Dir,  &
                     this%m_SPD,  &
                     this%m_RMS,  &
                     this%m_SRNG, &
                     this%m_ELEV, &
                     STAT=derr,   &
                     ERRMSG=emsg  )
          if(derr /= 0) then
              if(logging == .true. ) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:494 --> mod_wsr88d_vadwind/destroy: Failed to Deallocate Memory !!")
                  call log_shutdown()
              else if (verbose == .true.)  then
                  call print_fatal_error("========================= FATAL =========================", &
                                         "mod_wsr88d_vadwind/destroy:463 -- VADWind_t already destroyed !!" , &
                                         emsg,sdate,stime,__LINE__ )
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_vadwind/destroy:463 [FATAL-ERROR] --> Failed to Deallocate Memory!!"
          end if
          this%m_isbuilt = .false.
    end subroutine destroy
    
    !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_ims
!DIR$   ENDIF 
    pure function get_ims(this) result(ims)
          implicit none
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: ims
          ! Start of executable statements
          ims = this%m_ims
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_ime
!DIR$   ENDIF 
    pure function get_ime(this) result(ime)
          implicit none
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: ime
          ! Start of executable statements
          ime = this%m_ime
    end function  
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_kms
!DIR$   ENDIF 
    pure function get_kms(this) result(kms)
          implicit none
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: kms
          ! Start of executable statements
          kms = this%m_kms
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_kme
!DIR$   ENDIF 
    pure function get_kme(this) result(kme)
          implicit none
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: kme
          ! Start of executable statements
          kme = this%m_kme
    end function
    
 !DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_jms
!DIR$   ENDIF 
    pure function get_jms(this) result(jms)
          implicit none
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: jms
          ! Start of executable statements
          jms = this%m_jms
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_jme
!DIR$   ENDIF 
    pure function get_jme(this) result(jme)
          implicit none
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: jme
          ! Start of executable statements
          jme = this%m_jme
    end function 
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_isbuilt
!DIR$   ENDIF  
    pure function get_isbuilt(this) result(isbuilt)
          implicit none
          class(VADWind_t), intent(in) :: this
          logical(I32P),  intent(in) :: isbuilt
          ! Start of executable statements
          isbuilt = this%m_isbuilt
    end function
    
     !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(VADWind_t),   intent(in) :: this
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
          class(VADWind_t),   intent(in) :: this
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
    
    subroutine dbg_info(this,verbose)
          implicit none
          class(VADWind_t),  intent(in) :: this
          logical(I32P),     intent(in) :: verbose
          ! Strat of executable statements
          print*, "==========================================================="
          print*, "        **** Dump of VADWind_t object state ***            "
          print*, "==========================================================="
          print*, "Collected at: ", __DATE__, ":", __TIME__, &
                  "in file: ", __FILE__, "at line: ", __LINE__
          print*, " m_ims:     ", this%m_ims, " m_ime: ", this%m_ime
          print*, " m_kms:     ", this%m_kms, " m_kme: ", this%m_kme
          print*, " m_jms:     ", this%m_jms, " m_jme: ", this%m_jme
          print*, " m_sdim1:   ", m_sdim1
          print*, " Wind altitude -- m_Alt:     ", this%m_Alt
          if(verbose == .true.) then
              print*, " Warning!! -- Printing possibly huge arrays!! "
              print*, " Wind V-component: ", this%m_V
              print*, " Wind U-component: ", this%m_U
              print*, " Wind W-component: ", this%m_W
          else
              print *, " Skipping over printing huge arrays."
          end if
          print*, " Wind speed -- m_SPD:       ", this%m_SPD 
          print*, " Wind speed (rms) -- m_RMS: ", this%m_RMS
          print*, " Wind SRNG:                 ", this%m_SRNG
          print*, " Antennae elevation:        ", this%m_ELEV
          print*, " built indicator:           ", this%m_isbuilt
          print*, "==========================================================="
    end subroutine dbg_info

end module mod_wsr88d_vadwind