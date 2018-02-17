
module mod_wsr88d_mreflpos

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_mreflpos'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Maximum Reflectivity Position product.
 !          History:
 !                        Date: 17-02-2018
 !                        Time: 09:39 GMT+2
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
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT , &
                                stdout=>OUTPUT_UNIT
    use IFPORT,          only : TRACEBACKQQ
    
    private :: print_non_fatal_error
    private :: print_fatal_error

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(I32P), parameter, public :: MOD_WSR88D_MREFLPOS_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_MREFLPOS_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_MREFLPOS_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_MREFLPOS_FULLVER = 1000_I32P*MOD_WSR88D_MREFLPOS_MAJOR + &
                                                                      100_I32P*MOD_WSR88D_MREFLPOS_MINOR  + &
                                                                      10_I32P*MOD_WSR88D_MREFLPOS_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_MREFLPOS_CREATE_DATE = "17-02-2018 09:37 +00200 (SAT 17 FEB 2018 GMT+2) "
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_MREFLPOS_BUILD_DATE = " "
    
    ! Module author
    character(*),  parameter, public :: MOD_WSR88D_MREFLPOS_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module description
    character(*),  parameter, public :: MOD_WSR88D_MREFLPOS_DESCRIPT = "Maximum Reflectivity Position a part of WSR-88D product."
    
    ! Module constants
    
    
    !=======================================
    ! Type: MaxRefPos_t
    !=======================================
    type, public :: MaxRefPos_t
        
          private
          
          ! Array lengths
          integer(I32P) :: m_na
          
          integer(I32P) :: m_nr
           
          ! These members are public in order to eliminate
          ! copy operations
          real(R64P), allocatable, dimension(:), public :: m_Azimuth  ! Azimuth, deg, 0.0-359.0, 1.0
          
          real(R64P), allocatable, dimension(:), public :: m_Range     ! Range, nmi, 0.0-124.0, 1.0
          
          logical(I32P) :: m_isbuilt
          
          contains
    
          !=====================================
          ! Construction,copy and destruction
          ! methods.
          !=====================================
    
          ! Constructor default initialization only
          procedure, pass(this), public :: init
          
          ! Copy content
          procedure, pass(this), public :: copy
          
          ! Destroy content
          procedure, pass(this), public :: destroy
          
          !==================================
          !    Getter procedures
          !==================================
          
          procedure, pass(this), public :: get_na
          
          procedure, pass(this), public :: get_nr
          
          procedure, pass(this), public :: get_Azimuth
          
          procedure, pass(this), public :: get_Range
          
          procedure, pass(this), public :: get_isbuilt
          
          
          
          !=========================================
          !  Read/write  procedures
          !=========================================
          
          procedure, nopass, public :: read_state
          
          procedure, nopass, public :: write_state
          
          !=========================================
          !   Class helper procedures
          !=========================================
        
          procedure, pass(this), private :: dbg_info
          
          
          
    end type MaxRefPos_t
          
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
    subroutine init(this,na,nr,ierr,logging, verbose,dbg,append, fname)
          implicit none
          class(MaxRefPos_t), intent(inout) :: this
          integer(I32P),      intent(in)    :: nr,na
          integer(I32P),      intent(out)   :: ierr
          logical(I32P),      intent(in)    :: logging, verbose, dbg, append
          character(len=*),   intent(in)    :: fname
          ! Locals
          character(len=40) :: sdate,stime
          character(len=256) :: emsg
          integer(I32P) :: i, aerr
          
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt == .true. .OR. &
             na <= 0_I32P .OR. nr <= 0_I32P) then
              if(logging == .true.) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:189 -> mod_wsr88d_mreflpos/init: MaxReflPos_t already initialized, or invalid argument passed!!")
                  call log_shutdown()
              else if(verbose == .true.) then
                  call print_non_fatal_error("======================= NON-FATAL =============================" , &
                                       "mod_wsr88d_mreflpos/init:189 -- MaxReflPos_t already initialized, or invalid argument passed!!" , &
                                              sdate,stime )
                 
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin construction
          this%m_na = na
          this%m_nr = nr
          associate(la=>this%m_na,  &
                    lr=>this%m_nr   )
              allocate(this%m_Azimuth(la),  &
                       this%m_Range(lr),    &
                       STAT=aerr,           &
                       ERRMSG=emsg    )
          end associate
          if(aerr /= 0) then
              if(logging == .true.) then
                  call log_startup(fname,append)
                  call log_UsrMsg("logger:214 -> mod_wsr88d_mreflpos/init: Memory allocation failure !!")
                  call log_shutdown()
              else if(verbose == .true.) then
                  call print_fatal_error( "======================== FATAL ============================" ,   &
                                          "mod_wsr88d_maxreflpos/init:214 -- Memory allocation failure!!" , &
                                          emsg, sdate, stime)
                 
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_maxreflpos/init:214 [FATAL-ERROR] -- Memory Allocation Failure!!"
          end if
          ! Initialize arrays
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1, this%m_na
              this%m_Azimuth(i) = -1._R64P
          end do
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1, this%m_nr
              this%m_Range(i)   = -1._R64P
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
          implicit none
          class(MaxRefPos_t), intent(inout) :: this
          class(MaxRefPos_t), intent(in)    :: other
          logical(I32P),      intent(in)    :: logging
          character(len=*),   intent(in)    :: fname
          logical(I32P),      intent(in)    :: append, verbose
          integer(I32P),      intent(out)   :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if( this%m_isbuilt == .true.  .OR. &
             other%m_isbuilt == .false.   )  then
               if(logging == .true. ) then
                   call log_startup(fname,append)
                   call log_UsrMsg("logger:288 --> mod_wsr88d_mreflpos/init: this -- initialized and other -- destroyed!!")
                   call shutdown()
               else if (verbose == .true.)  then
                   call print_non_fatal_error("======================= NON-FATAL =============================", &
                                              " mod_wsr88d_mreflpos/init:288 -->  this -- initialized and other -- destroyed!! ", &
                                              sdate, stime)
                   
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
    subroutine destroy(this,logging,filename,append,verbose,ierr)
          implicit none
          class(MaxRefPos_t), intent(inout) :: this
          logical(I32P),      intent(in)    :: logging
          character(len=*),   intent(in)    :: filename
          logical(I32P),      intent(in)    :: append, verbose
          integer(I32P),      intent(out)   :: ierr
          ! Locals
          character(len=40)  :: sdate, stime
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(this%m_isbuilt = .false.) then
              if(logging == .true. ) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:338 --> mod_wsr88d_maxreflpos/destroy: MaxRefPos_t is already destroyed!!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_non_fatal_error("======================= NON-FATAL =============================", &
                                              "mod_wsr88d_maxreflpos/destroy:338 -- MaxReflPos_t already destroyed!!",
                                             sdate, stime)
              end if
              ierr = -1_I32P
              return
          end if
          ! Begin destruction
          this%m_na = 0_I32P
          this%m_nr = 0_I32P
          deallocate(this%m_Azimuth,  &
                     this%m_Range,    &
                     STAT=derr,       &
                     ERRMSG=emsg   )
          if(derr /= 0) then
              if(logging == .true.) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:358 --> mod_wsr88d_maxrelpos/destroy: Failed to deallocate memory!!")
                  call log_shutdown()
              else if (verbose == .true.) then
                  call print_fatal_error( "======================== FATAL ============================", &
                                          "mod_wsr88d_maxreflpos/destroy:338 --> Failed to deallocate memory!! ", &
                                          smsg, sdate, stime )
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wsr88d_maxreflpos/destroy:338 [FATAL-ERROR] -- Memory Deallocation Failure!!"
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
          class(MaxRefPos_t), intent(in) :: this
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
          class(MaxRefPos_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nr
          ! Start of executable statemetns
          nr = this%m_nr
    end function get_nr
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Azimuth
!DIR$   ENDIF   
    pure function get_Azimuth(this) result(Azimuth)
          implicit none
          class(MaxRefPos_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: Azimuth
!DIR$     ATTRIBUTES ALIGN : 64 :: Azimuth
          ! Start of executable statemetns
          Azimuth = this%m_Azimuth
    end function get_Azimuth
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_Range
!DIR$   ENDIF  
    pure function get_Range(this)  result(Range)
          implicit none
          class(MaxRefPos_t), intent(in) :: this
          !  Locals
          real(R64P), allocatable, dimension(:) :: Range
!DIR$     ATTRIBUTES ALIGN : 64 :: Range
          ! Start of executable statements
          Range = this%m_Range
    end function get_Range
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_isbuilt
!DIR$   ENDIF 
    pure function get_isbuilt(this) result(isbuilt)
          implicit none
          class(MaxRefPos_t), intent(in) :: this
          ! Locals
          logical(I32P) :: isbuilt
          ! Start of executable statements
          isbuilt = this%m_isbuilt
    end function get_isbuilt
    
    !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(MaxRefPos_t), intent(in)    :: this
          character(len=*),   intent(in)    :: form
          integer(I32P),      intent(in)    :: uint
          integer(I32P),      intent(inout) :: ioerr
          ! Start of executable statemetns
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine  read_state
    
    subroutine write_state(this,form,unit,ioerr)
          implicit none
          class(MaxRefPos_t), intent(in)    :: this
          character(len=*),   intent(in)    :: form
          integer(I32P),      intent(in)    :: unit
          integer(I32P),      intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
    !===================================
    !    Class helper procedures
    !===================================
    subroutine dgb_info(this)
          implicit none
          class(MaxRefPos_t), intent(in) :: this
          ! Start of executable statemetns
           print*, " Dump 'MaxRefPos_t' state!!"
           print*, "============================================"
           print*, " Length of Range:        ", this%m_nr
           print*, " Length of Azimuth:      ", this%m_na
           print*, " Azimuth initialization: ", this%m_Azimuth
           print*, " Range   initialization: ", this%m_Range
           print*, " Built status:           ", this%m_isbuilt
           print*, "============================================="
    end subroutine  dbg_info
    
    subroutine print_non_fatal_error(header,msg,sdate,stime )
          implicit none
          character(len=*),  intent(in) :: header,msg
          character(len=40), intent(in) :: sdate,stime
          ! Start of executable statements
          call DATE_AND_TIME(date=sdate,time=stime)
          write(stderr,*) header
          write(stderr,*) msg
          write(stderr,*) " ( Non-Fatal Error at:) ",  &
                              sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                              stime(1:2),":",stime(3:4),":",stime(5:6)
          write(stderr,*) header
    end subroutine  print_non_fatal_error
    
    subroutine print_fatal_error(header,msg,smsg,sdate,stime)
          implicit none
          character(len=*), intent(in) :: header, msg, smsg
          character(len=40), intent(in) :: sdate, stime
          ! Start of executable statements
          call DATE_AND_TIME(date=sdate,time=stime)
          write(stderr,*) header
          write(stderr,*) msg
          write(stderr,*) "System message: ", smsg
          write(stderr,*) " (Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
          write(stderr,*) header
    end subroutine print_fatal_error
    
end module mod_wsr88d_mreflpos