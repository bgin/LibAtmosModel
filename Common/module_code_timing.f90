
module mod_code_timing

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_code_timing'
 !          
 !          Purpose:
 !                    This module will be used to obtain crude and precise
 !                    performance of specific code block.
 !                    Crude and precise time measurement with the help
 !                    of 4 timers, additionaly time spent in process/thread
 !                    and kernel/user space is calculated by the calls to
 !                    kernal32 Fortran wrappers.
 !                    This module will be used mainly for performance tests
 !                    During such a test function or subroutine will be called
 !                    large number of time e.g 1000 in order to obtain as much
 !                    as possible running time samples which later be used to
 !                    compute basic statistical moments up to fourth.
 !                   
 !                     
 !          History:
 !                        Date: 02-08-2017
 !                        Time: 14:06 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
 !          
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

    implicit none
    use module_kinds
    use kernel32
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT , &
                                stdout=>OUTPUT_UNIT
     
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_CODE_TIMING_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_CODE_TIMING_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_CODE_TIMING_MICRO = 0
    
    ! Module/file full version
    integer(I32P), parameter, public :: MOD_CODE_TIMING_FULLVER = 1000*MOD_CODE_TIMING_MAJOR+100*MOD_CODE_TIMING_MINOR + &
                                                                  10*MOD_CODE_TIMING_MICRO
    
    ! Creation date
    character(*),  parameter, public :: MOD_CODE_TIMING_CREATE_DATE = "02-08-2017 14:39 +00200 (WED 02 AUG 2017 GMT+2)"
    
    ! Build date (should be set to latest successfull build date/time)
    character(*),  parameter, public :: MOD_CODE_TIMING_BUILD_DATE = " "
    
    ! Module/file author info
    character(*),  parameter, public :: MOD_CODE_TIMING_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_CODE_TIMING_DESCRIPT = "Crude and precise timing of code blocks."
    
    !================================================================70
    !  type: SCTimer_t
    !        Timer based on SYSTEM_CLOCK
    !================================================================70
    type :: SCTimer_t
        
        private
        
        character(len=32) :: m_tname  ! timer name
        
        integer(I64P) :: m_on,m_off,m_delta
        
        integer(I32P) :: m_loc ! line of measured code.
        
        character(len=128) :: m_fpname ! File and procedure name
        
        logical(I32P)  :: m_isbuilt
        
    end type SCTimer_t
    
    !================================================================70
    !  type: CTTimer_t
    !        Timer based on CPU_TIME intrinsic
    !================================================================70
    type :: CTTimer_t
        
        private
        
        character(len=32) :: m_tname   ! timer name
        
        real(R32P) :: m_on,m_off,m_delta
        
        integer(I32P) :: m_loc  ! line of measured code.
        
        character(len=128) :: m_fpname  ! File and procedure name
        
        logical(I32P) :: m_isbuilt
        
    end type CTTimer_t
    
    !================================================================70
    ! type: DCTimer_t
    !       Timer based on DCLOCK intrinsic
    !================================================================70
    type :: DCTimer_t
        
        private
        
        character(len=32) :: m_tname   ! timer name
        
        real(R64P) :: m_on,m_off,m_delta
        
        integer(I32P) :: m_loc  ! line of measured code.
        
        character(len=128) :: m_fpname   ! File and procedure name
        
    end type DCTimer_t
    
    !================================================================70
    !  type: QPCTimer_t
    !        Timer based on QueryPerformanceCounter
    !================================================================70
    type :: QPCTimer_t
        
        private
        
        character(len=32) :: m_tname   ! timer name
        
        type(T_LARGE_INTEGER) :: m_on,m_off
        
        real(R64P)    :: m_delta
        
        real(R64P) :: m_cpufreq  
        
        integer(I32P) :: m_loc  ! line of measured code.
        
        character(len=128) :: m_fpname  ! File and procedure name
    
    end type
    
    contains
    
    !==============================================52
    !  ***********Implementation*************
    !==============================================52
    
    ! SCTimer_t object initialization.
    subroutine sctimer_init(this,tname,fpname)
          
          implicit none
          type(SCTimer_t),   intent(inout) :: this
          character(len=*),  intent(in)    :: tname
          character(len=*),  intent(in)    :: fpname
          ! Start of executable statements
          if(this%m_isbuilt) then
              write(stderr,*) "sctimer_init: Timer is already initialized!"
              return
          end if
          this%m_tname   = tname
          this%m_on      = 0_I64P
          this%m_off     = 0_I64P
          this%m_delta   = 0_I64P
          this%m_loc     = 0
          this%m_fpname  = fpname
          this%m_isbuilt = .true.
    end subroutine
    
    ! SCTimer_t object destroy
    subroutine sctimer_destroy(this)
          implicit none
          type(SCTimer_t),  intent(inout) :: this
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              write(stderr,*) "sctimer_destroy: Timer has been destroyed!"
              return
          end if
          this%m_tname   = " "
          this%m_on      = -1_I64P
          this%m_off     = -1_I64P
          this%m_delta   = -1_R64P
          this%m_loc     = -1
          this%m_fpname  = " "
          this%m_isbuilt = .false.
    end subroutine
    
    ! SCTimer object reset
    subroutine sctimer_reset(this)
          implicit none
          type(SCTimer_t),  intent(inout) :: this
          ! Start of executable statements
          this%m_on    = 0_I64P
          this%m_off   = 0_I64P
          this%m_delta = 0_I64P
    end subroutine
    
    ! SCTimer object start measurement
    subroutine sctimer_start(this)
          implicit none
          type(SCTimer_t), intent(inout) :: this
          ! Start of executable statemetns
          call SYSTEM_CLOCK(this%m_on)
    end subroutine
    
    ! SCTimer_t object stop measurement
    subroutine sctimer_stop(this)
          implicit none
          type(SCTimer_t), intent(inout) :: this
          ! Start of executable statemetns
          call SYSTEM_CLOCK(this%m_off)
    end subroutine
    
    ! SCTimer_t object compute delta of measurements
    subroutine sctimer_delta(this,ifail)
          implicit none
          type(SCTimer_t), intent(inout) :: this
          logical(I32P),   intent(out)   :: ifail
          ! Start of executable statements
          if((this%m_off-this%m_on).GT.0_I64P) then
              this%m_delta = this%m_off-this%m_on
          else
              this%m_delta = -2_I64P
              ifail = .true.
              return
          end if
    end subroutine

    !print SCTimer_t object
    subroutine sctimer_print(this)
          implicit none
          type(SCTimer_t), intent(in) :: this
          ! Start of executable statements
          write(stdout,*) "====================================================="
          write(stdout,*)   "1) Timer name: ",  this%m_tname
          write(stdout,10) this%m_on
10        format("2) start value=",I24.20)
          write(stdout,20) this%m_off
20        format("3) stop  value=",I24.20)
          write(stdout,30) this%m_delta
30        format("4) delta value=",I24.20)
          write(stdout,*)   "5) line of code: ", this%m_loc
          write(stdout,*)   "6) File name/procedure name: ",  this%m_fpname
          write(stdout,*) "====================================================="
    end subroutine
    
    ! CTTimer_t object initialiation
    subroutine cttimer_init(this,tname,fpname)
          implicit none
          type(CTTimer_t),  intent(inout) :: this
          character(len=*), intent(in)    :: tname,fpname
          ! Start of executable statements
          if(this%m_isbuilt) then
              write(stderr,*) "cttimer_init: CTTimer_t is already initialized!"
              return
          end if
          this%m_tname   = tname
          this%m_on      = 0._R32P
          this%m_off     = 0._R32P
          this%m_delta   = 0._R32P
          this%m_loc     = -1
          this%m_fpname  = fpname
          this%m_isbuilt = .true.
    end subroutine
    
    ! CTTimer_t object destroy
    subroutine cttimer_destroy(this)
          implicit none
          type(CTTimer_t),  intent(inout) :: this
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              write(stderr,*) "cttimer_destroy: CTTimer_t already destroyed!"
              return
          end if
          this%m_tname   = " "
          this%m_on      = -1._R32P
          this%m_off     = -1._R32P
          this%m_delta   = -1._R32P
          this%m_loc     = -1
          this%m_fpname  = " "
          this%m_isbuilt = .false.
    end subroutine
    
    ! CTTimer_t object state reset
    subroutine cttimer_reset(this)
          implicit none
          type(CTTimer_t), intent(inout) :: this
          ! Start of executable statements
          this%m_on    = 0._R32P
          this%m_off   = 0._R32P
          this%m_delta = 0._R32P
    end subroutine
    
    ! CTTimer_t start measurement
    subroutine cttimer_start(this)
          implicit none
          type(CTTimer_t), intent(inout) :: this
          ! Start of executable statements
          call CPU_TIME(this%m_on)
    end subroutine
    
    ! CTTimer_t stop measurement
    subroutine cttimer_stop(this)
          implicit none
          type(CTTimer_t), intent(inout) :: this
          ! Start of executable statements
          call CPU_TIME(this%m_off)
    end subroutine
    
    ! CTTimer_t compute measurement delta
    subroutine cttimer_delta(this,ifail)
          implicit none
          type(CTTimer_t), intent(inout) :: this
          logical(I32P),   intent(out)   :: ifail
          ! Start of executable statements
          if((this%m_off-this%m_on).GT.0._R32P) then
              this%m_delta = this%m_off-this%m_on
          else
              this%m_delta = -2._R64P
              ifail = .true.
          end if
    end subroutine
    
    ! CTTimer print object state
    subroutine cttimer_print(this)
          implicit none
          type(CTTimer_t), intent(in) :: this
          ! Start of executable satements
          write(stdout,*) "==========================================="
          write(stdout,*) " 1) Timer  name: ", this%m_tname
          write(stdout,10) this%m_on
10        format(" 2) start value=",F10.6) 
          write(stdout,20) this%m_off
20        format(" 3) stop  value=",F10.6)
          write(stdout,30) this%m_delta
30        format(" 4) delta value=",F10.6)
          write(stdout,*) " 5) line of code:", this%m_loc
          write(stdout,*) " 6) File name/proc name: ", this%m_fpname
          write(stdout,*) "============================================"
    end subroutine
    
    ! DCTimer_t object initialiation
    subroutine dctimer_init(this,tname,fpname)
          implicit none
          type(DCTimer_t),  intent(inout) :: this
          character(len=*), intent(in)    :: tname,fpname
          ! Start of executable statemetns
          if(this%m_isbuilt) then
              write(stderr,*) " dctimer_init: DCTimer_t already initialized"
              return
          end if
          this%m_tname   = tname
          this%m_on      = 0._R64P
          this%m_off     = 0._R64P
          this%m_delta   = 0._R64P
          this%m_loc     = -1
          this%m_fpname  = fpname
          this%m_isbuilt = .true.
    end subroutine
    
    ! DCTimer_t  object destruction
    subroutine dctimer_destroy(this)
          implicit none
          type(DCTimer_t), intent(inout) :: this
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              write(stderr,*) " dctimer_destroy: DCTimer_t is already destroyed!!"
              return
          end if
          this%m_tname   = " "
          this%m_on      = -1._R64P
          this%m_off     = -1._R64P
          this%m_delta   = -1._R64P
          this%m_loc     = -1
          this%m_fpname  = " "
          this%m_isbuilt = .false.
    end subroutine
    
    ! DCTimer_t reset measurement to 0.0
    subroutine dctimer_reset(this)
          implicit none
          type(DCTimer_t), intent(inout) :: this
          ! Start of executable statements
          this%m_on    = 0._R64P
          this%m_off   = 0._R64P
          this%m_delta = 0._R64P
    end subroutine
    
    ! DCTimer_t start measurement
    subroutine dctimer_start(this)
          use ifport
          implicit none
          type(DCTimer_t), intent(inout) :: this
          ! Start of executable statements
          this%m_on = DCLOCK()
    end subroutine
    
    ! DCTimer_t stop measurement
    subroutine dctimer_stop(this)
          use ifport
          implicit none
          type(DCTimer_t), intent(inout) :: this
          ! Start of executable statements
          this%m_off = DCLOCK()
    end subroutine
    
    ! DCTimer measurememt delta
    subroutine dctimer_delta(this,ifail)
          implicit none
          type(DCTimer_t), intent(inout) :: this
          logical(I32P),   intent(out)   :: ifail
          ! Start of executable statements
          ifail = .false.
          if((this%m_off-this%m_on).GT.0._R64P) then
              this%m_delta = this%m_off-this%m_on
          else
              this%m_delta = -2._R64P
              ifail = .true.
          end if
    end subroutine
    
    ! DCTimer_t print state
    subroutine dctimer_print(this)
          implicit none
          type(DCTimer_t), intent(inout) :: this
          ! Start of executable statements
          write(stdout,*) "======================================"
          write(stdout,*) " 1) Timer name: ", this%m_tname
          write(stdout,10) this%m_on
10        format(" 2) start value=",F10.15)
          write(stdout,20) this%m_off
20        format(" 3) stop  value=",F10.15)
          write(stdout,30) this%m_delta
30        format(" 4) delta value=",F10.15)
          write(stdout,*)  " 5) line of code: ", this%m_loc
          write(stdout,*)  " 6) File name/proc name: ", this%m_fpname
    end subroutine
    
    ! QPCTimer_t object initialization
    subroutine qpctimer_init(this,tname,fpname)
          implicit none
          type(QPCTimer_t), intent(inout) :: this
          character(len=*), intent(in)    :: tname,fpname
          ! Start of executable statements
          if(this%m_isbuilt) then
              write(stderr,*) " qpctimer_init: QPCTimer_t already initialized"
              return
          end if
          this%m_tname   = tname
          this%m_on      = T_LARGE_INTEGER(0,0)
          this%m_off     = T_LARGE_INTEGER(0,0)
          this%m_delta   = 0._R64P
          this%m_cpufreq = 0._R64P
          this%m_loc     = -1
          this%m_fpname  = fpname
          this%m_isbuilt = .true.
    end subroutine
    
    ! QPCTimer_t destroy object
    subroutine qpctimer_destroy(this)
          implicit none
          type(QPCTimer_t), intent(inout) :: this
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              write(stderr,*) " qpctimer_destroy: QPCTimer_t is already destroyed!"
              return
          end if
          this%m_tname = " "
          this%m_on      = T_LARGE_INTEGER(-1,-1)
          this%m_off     = T_LARGE_INTEGER(-1,-1)
          this%m_delta   = -1._R64P
          this%m_cpufreq =  0._R64P
          this%m_loc     = -1
          this%m_fpname  = " "
          this%m_isbuilt = .false.
    end subroutine
    
    ! QPCTimer_t reset object state
    subroutine qpctimer_reset(this)
          implicit none
          type(QPCTimer_t), intent(inout) :: this
          ! Start of executable statements
          this%m_on      = T_LARGE_INTEGER(0,0)
          this%m_off     = T_LARGE_INTEGER(0,0)
          this%m_delta   = 0._R64P
          this%m_cpufreq = 0._R64P
    end subroutine
    
    ! QPCTimer_t start measurement
    subroutine qpctimer_start(this,ifail)
          implicit none
          type(QPCTimer_t), intent(inout) :: this
          integer(BOOL),    intent(out)   :: ifail
          ! Locals
          type(T_LARGE_INTEGER) :: sval,tmp
          ! Start of executable statemetns
          !ifail = 0
          ifail = QueryPerformanceFrequency(sval)
          if(ifail.EQ.0) then
              write(stderr,*) " qpctimer_start: QueryPerformanceCounter failed with value: ",ifail
              return
          end if
          tmp = INT(IOR(LSHIFT(sval.HighPart,32),sval.LowPart),KIND=8)
          this%m_cpufreq = DBLE(tmp)*0.001_R64P
          ifail = QueryPerformanceCounter(sval)
          this%m_on = sval
    end subroutine
    
    ! QPCTimer_t stop measurement
    subroutine qpctimer_stop(this,ifail)
          implicit none
          type(QPCTimer_t), intent(inout) :: this
          integer(BOOL),    intent(out)   :: ifail
          ! Start of executable statements
          !ifail = 0
          ifail = QueryPerformanceCounter(this%m_off)
          if(fail.EQ.0) then
             return
          end if
    end subroutine      
    
    ! QPCTimer_t compute measurement delta
    subroutine qpctimer_delta(this,ifail)
          implicit none
          type(QPCTimer_t), intent(inout) :: this
          logical(I32P),    intent(out)   :: ifail
          ! Locals
          integer(I64P) :: tmp1,tmp2
          ! Start of executable sattements
          associate(v1=>this%m_on, &
                    v2=>this%m_off )
              tmp1 = INT(IOR(LSHIFT(v1.HighPart,32),v1.LowPart),KIND=8)
              tmp2 = INT(IOR(LSHIFT(v2.HighPart,32),v2.LowPart),KIND=8)
              if((tmp2-tmp1).GT.0_I64P) then
                  this%m_delta = REAL(tmp2-tmp1)/this%m_cpufreq
              else
                  this%m_delta = -2._R64P
                  ifail = .true.
                  
              end if
          end associate
    end subroutine
    
    ! QPCTimer print object state
    subroutine qpctimer_print(this)
          implicit none
          type(QPCTimer_t), intent(in) :: this
          ! Start of executable statemetns
          write(stdout,*) "============================================"
          write(stdout,*) " 1) Timer name: ", this%m_tname
          write(stdout,10) this%m_on%LowPart
10        format(" 2) start LowPart:",I24.20)
          write(stdout,20) this%m_on%HighPart
20        format(" 2) start HighPart:",I24.20)
          write(stdout,30) this%m_off%LowPart
30        format(" 3) stop  LowPart:",I24.20)
          write(stdout,40) this%m_off%HighPart
40        format(" 3) stop  HighPart:",I24.20)
          write(stdout,50) this%m_delta
50        format( " 4) delta: ", F10.15)
          write(stdout,60) this%m_cpufreq
60        format( " 5) CPU freq:",F10.15)
          write(stdout,*) " 6) line of code: ", this%m_loc
          write(stdout,*) " 7) File name/proc name: ", this%m_fpname
          write(stdout,*) "============================================="
    end subroutine
    
    
    
end module mod_code_timing