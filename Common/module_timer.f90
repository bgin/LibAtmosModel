
module mod_timer

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_timer'
 !          
 !          Purpose:
 !                    Crude and precise time measurement with the help
 !                    of 4 timers, additionaly time spent in process/thread
 !                    and kernel/user space is calculated by the calls to
 !                    kernal32 Fortran wrappers.
 !                    This module will be used mainly for performance tests
 !                    During such a test function or subroutine will be called
 !                    large number of time e.g 1000 in order to obtain as much
 !                    as possible running time samples which later be used to
 !                    compute basic statistical moments up to fourth.
 !                    This module is not appropriate for testing single block
 !                    of code.
 !                     
 !          History:
 !                        Date: 29-07-2017
 !                        Time: 10:12 GMT+2
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
    !use IFPORT
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(I32P), parameter, public :: MOD_TIMER_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_TIMER_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_TIMER_MICRO = 0
    
    ! File/module full version
    integer(I32P), parameter, public :: MOD_TIMER_FULLVER = 1000*MOD_TIMER_MAJOR+100*MOD_TIMER_MINOR+ &
                                                            10*MOD_TIMER_MICRO
    
    ! Creation date
    character(*),  parameter, public :: MOD_TIMER_CREATION_DATE = "29-07-2017 10:20 +00200 (SAT 29 JUL 2017 GMT+2)"
    
    ! Build date (should be set to latest successful build date/time)
    character(*),  parameter, public :: MOD_TIMER_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_TIMER_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_TIMER_DESCRIPTION = "Crude and precise time measurement module."
    
    ! Private constants
    
    integer(I32P), parameter, private :: MINRUNS = 10
    
    integer(I32P), parameter, private :: NROWS = 3
    
    integer(I32P), parameter, private :: SCROWS = 2
    
    
    
    !================================================================70
    !  Derived type: PerfTimer_t
    !  Will be used to perform mainly comprehensive time measurement
    !  during the performance testing.
    !================================================================70
    
    type :: PerfTimer_t
        
       private
       
       ! Timer_t metadata describing exact location
       ! of function beign measured.
       
       ! file name
       character(len=80)  :: m_fname
       
       ! procedure name
       character(len=80)  :: m_procname
       
       ! line of code in test-runner subroutine
       integer(I32P)      :: m_loc
       
       ! short description message e.g. "timing#1: coagulation function"
       character(len=256) :: m_msg
       
       ! Date of measurement
       character(len=40)  :: m_date
       
       ! Time of measurement
       character(len=40)  :: m_time
       
       ! Parent process handle
       integer(HANDLE)    :: m_phand
       
       ! Parent process ID
       integer(DWORD)     :: m_pid
       
       ! Current thread handle 
       integer(HANDLE)    :: m_thand
       
       ! Current thread ID
       integer(DWORD)     :: m_tid
       
       ! Timer names
       character(len=64), dimension(4), parameter :: m_tnames = ["CPU_TIME", "SYSTEM_CLOCK", "DCLOCK", &
                                                                 "QueryPerfomanceCounter", "OpenMP timing"]
       
       ! Number of measurement runs
       integer(I32P) ::    m_runs
       
       !! Clock facility used to perform time measurement
        ! Position        1 - CPU_TIME * supported
        !   -||-          2 - SYSTEM_CLOCK  * supportd
        !   -||-          3 - C intrinsic like 'rdtsc' ..etc  (experimental)
        !   -||-          4 - DCLOCK * supported
        !   -||-          5 - QueryPerformanceCounter * supprted
        !   -||-          6 - OpenMP timing procedures * supported
       !!
       
       ! CPU_TIME measurements represented as an array a(3,N), where
       ! a(1,N) - start clock values
       ! a(2,N) - stop  clock values
       ! a(3,N) - start-stop values (delta)
       real(R32P), allocatable, dimension(:,:) :: m_ctresults
       
       ! SYSTEM_CLOCK measurements reeprsented asa an array a(3,N), where
       ! a(1,N) - start clock values
       ! a(2,N) - stop  clock values
       
       integer(I64P), allocatable, dimension(:,:) :: m_scresults
       
       ! SYSTEM_CLOCK measurements delta repreesented by double-
       ! precision floating point array.
       real(R64P),    allocatable, dimension(:)   :: m_scdelta
       ! DCLOCK measurememts represented as an array a(3,N), where
       ! a(1,N) - start clock values
       ! a(2,N) - stop  clock values
       ! a(3,N) - start-stop values (delta)
       real(R64P), allocatable, dimension(:,:)   :: m_dcresults
       
       ! QueryPerformanceCounter measurements represented as an
       ! array a(3,N) where,
       ! a(1,N) - start clock values
       ! a(2,N) - stop  clock values
       ! a(3,N) - start-stop values (delta)
       type(T_LARGE_INTEGER), allocatable, dimension(:,:) :: m_qpcresult
       
       ! Time Process spent in User mode
       type(T_FILETIME)    :: m_puser
       
       ! Time Process spent in Kernel mode
       type(T_FILETIME)    :: m_pkernel
       
       ! Time Thread spent in User mode
       type(T_FILETIME)    :: m_tuser
       
       ! Time Thread spent in kernel mode
       type(T_FILETIME)    :: m_tkernel
       
       ! Built/creation status
       logical(I32P)   :: m_isbuilt
       
       ! Windows error - Non critical
       integer(BOOL)   :: m_werr
       
       ! Has timer reset been performed?
       logical(I32P)   :: m_wasreset
       
    end type PerfTimer_t
    
    contains
    
    ! Subroutine init
    subroutine perf_timer_init(this,fname,procname,loc,msg,runs)
                               
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          type(PerfTimer_t), intent(inout) :: this
          character(len=*),  intent(in)    :: fname,procname
          integer(I32P),     intent(in)    :: loc
          character(len=*),  intent(in)    :: msg
          integer(I32P),     intent(in)    :: runs
          ! Locals
          integer(I32P)      :: erralloc
          character(len=256) :: emsg
          character(len=40)  :: dstr,tstr
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .true.) then
              call DATE_AND_TIME(date=dstr,time=tstr)
              write(ERROR_UNIT,*) "=========================NON-FATAL======================="
              write(ERROR_UNIT,*) "  (perf_timer_init: PerfTimer_t already initialized) "
              write(ERROR_UNIT,*) " ( Non-Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
              write(ERROR_UNIT,*) "=========================NON-FATAL======================="
              return
          end if
          if(runs.LE.0) then
              runs = MINRUNS
          end if
          ! Begin initialization
          this%m_fname = fname
          this%m_procname = procname
          this%m_loc = loc
          this%m_msg = msg
          call DATE_AND_TIME(date=this%m_date,time=this%m_time)
          this%m_phand = GetCurrentProcess()
          this%m_pid   = GetCurrentProcessId()
          this%m_thand = GetCurrentThread()
          this%m_tid   = GetCurrentThreadId()
          this%m_runs  = runs
          allocate(this%m_ctresults(NROWS, this%m_runs), &
                   this%m_scresults(SCROWS,this%m_runs), &
                   this%m_scdelta(this%m_runs),          &
                   this%m_dcresults(NROWS, this%m_runs), &
                   this%m_qpcresult(NROWS, this%m_runs), &
                   STAT=erralloc,ERRMSG=emsg)
          if(erralloc.NE.0) then
              call DATE_AND_TIME(date=dstr,time=tstr)
              write(ERROR_UNIT,*) "========================FATAL====================="
              write(ERROR_UNIT,*) " (perf_timer_init: Allocation memory failure!)"
              write(ERROR_UNIT,*) " ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
              write(ERROR_UNIT,*) "========================FATAL====================="
              ERROR STOP "perf_timer_init: FATAL: [Allocation memory failure!]"
          end if
          this%m_ctresults = 0._R32P
          this%m_scresults = 0_I64P
          this%m_scdelta   = -1._R64P
          this%m_dcresults = 0._R64P
          this%m_qpcresult = T_LARGE_INTEGER(-1,-1)
          this%m_puser     = T_FILETIME(-1,-1)
          this%m_pkernel   = T_FILETIME(-1,-1)
          this%m_tuser     = T_FILETIME(-1,-1)
          this%m_tkernel   = T_FILETIME(-1,-1)
          this%m_isbuilt   = .true.
          this%m_werr      = 0
          this%m_wasreset  = .false.
    end subroutine
    
    ! Subroutine destroy
    ! Deallocates 4 allocatable arrays and sets
    ! variable m_isbuilt to false.
    ! Upon occurrence of dealocation error
    ! STOP is executed
    subroutine perf_timer_destroy(this) 
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          type(PerfTimer_t), intent(inout) :: this
          ! Locals
          character(len=40) :: dstr,tstr
          integer(I32P)      :: errdeal
          character(len=256) :: errmsg
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              call DATE_AND_TIME(date=dstr,time=tstr)
              write(ERROR_UNIT,*) "=========================NON-FATAL======================="
              write(ERROR_UNIT,*) "  (perf_timer_destroy: PerfTimer_t already destroyed) "
              write(ERROR_UNIT,*) " ( Non-Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
              write(ERROR_UNIT,*) "=========================NON-FATAL======================="
              return
          end if
          if(allocated(this%m_ctresults)) then
              deallocate(this%m_ctresults,STAT=errdeal, &
                         ERRMSG=emsg)
              if(errdeal.NE.0) then
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (perf_timer_destroy: Memory dealocation failed: array m_ctresults) "
                  write(ERROR_UNIT,*) " ( Non-Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "perf_timer_destroy: [FATAL]: Memory deallocation failed! " \\ emsg
              end if
          end if
          if(allocated(this%m_scresults)) then
              deallocate(this%m_scresults,STAT=errdeal, &
                         ERRMSG=emsg)
              if(errdeal.NE.0) then
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (perf_timer_destroy: Memory dealocation failed: array m_scresults) "
                  write(ERROR_UNIT,*) " ( Non-Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "perf_timer_destroy: [FATAL]: Memory deallocation failed! " \\ emsg
              end if
          end if
          if(allocated(this%m_scdelta)) then
              deallocate(this%m_scdelta,STAT=errdeal, &
                         ERRMSG=emsg)
              if(errdeal.NE.0) then
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (perf_timer_destroy: Memory dealocation failed: array m_scresults) "
                  write(ERROR_UNIT,*) " ( Non-Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "perf_timer_destroy: [FATAL]: Memory deallocation failed! " \\ emsg
              end if
          end if
          if(allocated(this%m_dcresults)) then
              deallocate(this%m_dcresults,STAT=errdeal, &
                         ERRMSG=emsg)
              if(errdeal.NE.0) then
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (perf_timer_destroy: Memory dealocation failed: array m_dcresults) "
                  write(ERROR_UNIT,*) " ( Non-Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "perf_timer_destroy: [FATAL]: Memory deallocation failed! " \\ emsg
              end if
          end if
          if(allocated(this%m_qpcresult)) then
              deallocate(this%m_qpcresult,STAT=errdeal, &
                         ERRMSG=emsg)
              if(errdeal.NE.0) then
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  write(ERROR_UNIT,*) "  (perf_timer_destroy: Memory dealocation failed: array m_qpcresult) "
                  write(ERROR_UNIT,*) " ( Non-Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(ERROR_UNIT,*) "=========================FATAL======================="
                  ERROR STOP "perf_timer_destroy: [FATAL]: Memory deallocation failed! " \\ emsg
              end if
          end if
          this%m_isbuilt = .false.
    end subroutine
    
    ! Subroutine: perf_timer_clear
    ! Sets to zero or clears the arrays with accumulted results
    ! T_FILETIME structures are zeroed also.
    subroutine perf_timer_clear(this)
          implicit none
          type(PerfTimer_t), intent(inout) :: this
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          ! Check if timer was not cleared
          if(this%m_wasreset .EQ. .true. ) then
            
              call DATE_AND_TIME(date=dstr,time=tstr)
              write(ERROR_UNIT,*) "=========================NON-FATAL======================="
              write(ERROR_UNIT,*) "  (perf_timer_destroy: PerfTimer_t already clared) "
              write(ERROR_UNIT,*) " ( Non-Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
              write(ERROR_UNIT,*) "=========================NON-FATAL======================="
              return
          end if
          ! Start clearing arrays and structures
          this%m_ctresults = 0._R32P
          this%m_scresults = 0_I64P
          this%m_scdelta   = -1._R64P
          this%m_dcresults = 0._R64P
          this%m_qpcresult = T_LARGE_INTEGER(-1,-1)
          this%m_puser = T_FILETIME(-1,-1)
          this%m_pkernel = T_FILETIME(-1,-1)
          this%m_tuser   = T_FILETIME(-1,-1)
          this%m_tkernel = T_FILETIME(-1,-1)
          this%m_wasreset = .true.
    end subroutine
    ! Starts CPU_TIME clock
    ! 1st measurement
    subroutine cpu_time_start(this,idx)
          implicit none
          type(PerfTimer_t), intent(inout) :: this
          integer(I32P),     intent(in)    :: idx
          ! Start of executable statements
          call CPU_TIME(this%m_ctresults(1,idx))
    end subroutine
    
    ! Stops CPU_TIME clock 
    ! 2nd measurement
    subroutine cpu_time_stop(this,idx)
          implicit none
          type(PerfTimer_t), intent(inout) :: this
          integer(I32P),     intent(in)    :: idx
          ! Start of executable statements
          call CPU_TIME(this%m_ctresults(2,idx))
    end subroutine
    
    ! Computes CPU_TIME clock measurement delta i.e
    ! (stop-start)
    subroutine cpu_time_delta(vals,nvals,invi)
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R32P), dimension(:,:), intent(inout) :: vals
          integer(I32P),              intent(in)    :: nvals
          integer(I32P),              intent(inout) :: invi  ! Counter of invalid values
         
          ! Locals
          integer(I32P) :: i,j
          ! Start of executable statements
          ! Scan for invalid value i.e negative real(kind=4)
          invi = 0
          do j = 1, 2
              do i = 1, nvals
                  if(vals(j,i).LT.0._32P) then
                      invi = invi + 1
                      write(ERROR_UNIT,*) "invalid value at:", "j=",j, "i=",i
                      ! Correct to 0.0
                      vals(j,i) = 0._R32P
                  end if
              end do
          end do
          ! Compute delta
          do i = 1, nvals
              vals(3,i) = vals(2,i)-vals(1,i)
          end do
    end subroutine
    
    ! Minimum and maximum values of start,stop and delta.
    ! Min(row: 1), Max(row: 1)
    ! Min(row: 2), Max(row: 2)
    ! Min(row: 3), Max(row: 3)
    subroutine cpu_time_minmax( vals,nvals,minv,maxv,minl,maxl,invi)
          implicit none
          real(R32P), dimension(:,:), intent(in)  :: vals
          integer(I32P),              intent(in)  :: nvals
          real(R32P), dimension(3),   intent(out) :: minv,maxv
          integer(I32P), dimension(3),intent(out) :: minl,maxl
          integer(I32P),              intent(in)  :: invi
          ! Start of executable statements
          ! Check if invalid exist
          if(invi.GT.0) then
              minv = MINVAL(vals,dim=2,mask=vals.GT.0._R32P)
              maxv = MAXVAL(vals,dim=2,mask=vals.GT.0._R32P)
              minl = MINLOC(vals,dim=2,mask=vals.GT.0._R32P)
              maxl = MAXLOC(vals,dim=2,mask=vals.GT.0._R32P)
          else
              minv = MINVAL(vals,dim=2)
              maxv = MAXVAL(vals,dim=2)
              minl = MINLOC(vals,dim=2)
              maxl = MAXLOC(vals,dim=2)
          end if
    end subroutine
    
    ! Compute statistics of CPU_TIME clock
    ! measurements
    ! Following statistical calculation are in use
    ! 1) Arithemtic mean of n-runs
    ! 2) Average deviation of n-runs
    ! 3) Standard deviation of n-runs
    ! 4) Skewness 
    ! 5) Kurtosis
    subroutine cpu_time_stats(vals,nvals,mean,adev,sdev,
                              skewness,kurtosis,invi,ccerr,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF                              
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R32P), dimension(:,:),  intent(in)    :: vals
          integer(I32P),               intent(in)    :: nvals
          real(R32P),                  intent(out)   :: mean,adev,sdev, &
                                                        skewness,kurtosis
          integer(I32P),               intent(in)    :: invi
          logical(I32P), dimension(2), intent(out)   :: ccerr
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          integer(I32P) :: nfrac,i
          real(R32P)    :: sum,var,t,t2,t3,t4,t5,isdev,fracp,ct2
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF          
          ! Start of executable statements
          ! If invalid values are present(set to 0.0)
          if(ANY(ccerr)) then
              ccerr = .false.
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          mean     = 0._R32P
          adev     = 0._R32P
          sdev     = 0._R32P
          skewness = 0._R32P
          kurtosis = 0._R32P
          sum = 0._R32P
          t = 0._R32P
          fracp = -1._R32P
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF            
          if(invi.GT.0) then
              nfrac = nvals-invi
              ! Compute mean
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=4))
              do i = 1, nfrac
                  sum = sum+vals(3,i)
              end do
              mean = sum/real(nfrac,kind=4)
              ! Compute average deviation and variance
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=4))              
              do i = 1, nfrac
                 t  = ABS(vals(3,i)-mean) 
                 adev = adev + t
                 t2 = (vals(3,i)-mean)*(vals(3,i)-mean)
                 ct2 = t2  ! Checks if catastrophic cancellation has occurred
                 fracp = ct2-INT(ct2,kind=4)  ! Warning: not robust method. CADNA should be used
                 if(fracp.LE.EPSILON(1._R32P)) then
                     write(ERROR_UNIT,*) "cpu_time_stats: Losing significant digits!!"
                     ccerr(1) = .true.
                 end if
                 var = var+t2
              end do    
              adev = adev/real(nfrac,kind=4)
              if(var.LE.0._R32P) then
                 write(ERROR_UNIT,*) "cpu_time_stats: Invalid variance=",var
                 ccerr = .true.
                 return
              end if
              var = var/real(nfrac,kind=4)
              sdev = SQRT(var)
              isdev = 1._R32P/sdev
              t3 = 0._R32P
              t4 = 0._R32P
              t5 = 0._R32P
              fracp = -1._R32P
              do i = 1, nfrac
                  t3 = (vals(3,i)-mean)*isdev
                  ct2 = t3                      ! Warning: not robust method. CADNA should be used.
                  fracp = t3-INT(ct2,kind=4)   ! Checks if catastrophic cancellation has occurred
                  if(fracp.LE.EPSILON(1._R32P)) then
                      write(ERROR_UNIT,*) "cpu_time_stats: Losing significand digits!!"
                      ccerr(2) = .true.
                  end if
                  t4 = t3**3
                  skewness = skewness+t4
                  t5 = t4*t3
                  kurtosis = kurtosis+t5
              end do
              skewness = skewness/real(nfrac,kind=4)
              kurtosis = kurtosis/(real(nfrac,kind=4))-3._R32P 
              if(kurtosis.LT.1._R32P ) then
                  write(ERROR_UNIT,*) "cpu_time_stats: Invalid kurtosis!"
                  write(ERROR_UNIT,*) "kurtosis=", kurtosis
                  ccerr = .true.
                  return
              end if
          else 
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=4))
              do i = 1, nvals
                 sum = sum+vals(3,i)
              end do
              mean =sum/REAL(nvals,KIND=4)
              ! Compute average deviation and variance
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=4))              
              do i = 1, nvals
                  t = ABS(vals(3,i)-mean)
                  adev = adev+t
                  t2 = (vals(3,i)-mean)*(vals(3,i)-mean)
                  ct2 = t2  ! Checks if catastrophic cancellation has occurred
                  fracp =  t2-INT(ct2,kind=4)  ! Warning: not robust method. CADNA should be used
                  if(fracp.LE.EPSILON(1._R32P)) then
                     write(ERROR_UNIT,*) "cpu_time_stats: Losing significant digits!!"
                     ccerr(1) = .true.
                  end if
                  var = var+t2
              end do
              adev = adev/REAL(nvals,KIND=4)
               if(var.LE.0._R32P) then
                 write(ERROR_UNIT,*) "cpu_time_stats: Invalid variance=",var
                 ccerr = .true.
                 return
              end if
              var = var/REAL(nvals,KIND=4)
              sdev = SQRT(var)
              isdev = 1._R32P/sdev
              do i = 1, nvals
                  t3 = (vals(3,i)-mean)*isdev
                  ct2 = t3                      ! Warning: not robust method. CADNA should be used.
                  fracp = t3-INT(ct2,kind=4)   ! Checks if catastrophic cancellation has occurred
                  if(fracp.LE.EPSILON(1._R32P)) then
                      write(ERROR_UNIT,*) "cpu_time_stats: Losing significand digits!!"
                      ccerr(2) = .true.
                  end if
                  t4 = t3**3
                  skewness = skewness+t4
                  t5 = t4*t3
                  kurtosis = kurtosis+t5
              end do
              skewness = skewness/REAL(nvals,KIND=4)
              kurtosis = kurtosis/(REAL(nvals,KIND=4))-3._R32P
              if(kurtosis.LT.1._R32P ) then
                  write(ERROR_UNIT,*) "cpu_time_stats: Invalid kurtosis!"
                  write(ERROR_UNIT,*) "kurtosis=", kurtosis
                  
                  return
              end if
          end if  
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " cpu_time_stats: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF          
    end subroutine   
    
    ! Compute Correlation of two measurement data sets
    ! by using real FFT.
    ! Data sets were obtained by the calls to CPU_TIME wrapper
    ! subroutine.
    subroutine cpu_time_correlation(vals1,vals2,corres,nvals, &
                                    ip,w, fp_flags)
          use mod_fftsg, only : rdft
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF                              
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(0:nvals-1), intent(in)    :: vals1,vals2 ! Input data sets(contains results of timing measuremets
          real(R64P), dimension(0:nvals-1), intent(out)   :: corres  ! result correlation by FFT
          integer(I32P),                    intent(in)    :: nvals
          integer(I32P), dimension(0:*),    intent(inout) :: ip
          real(R64P),    dimension(0:*),    intent(inout) :: w
          logical(I32P), dimension(5),      intent(inout) :: fp_flags
          ! Locals
          real(R64P), dimension(0:nvals-1) :: tmp
          real(R64P)    :: t,invals
          integer(I32P) :: i,snvals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF          
          ! Start of executable statements
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          ! Copy samples to tmp and to corres
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 0, nvals-1
              corres(i) = vals1(i)
              tmp(i)    = vals2(i)
          end do
          snvals = RSHIFT(nvals,1)
          invals = 1._R64P/REAL(snvals,kind=8)
          ip(0) = 0
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          call rdft(nvals,1,corres,ip,w)
          call rdft(nvals,1,tmp,ip,w)
          do i = 2, nvals, 2
              t = corres(i)
              corres(i) = (corres(i)*tmp(i)+corres(i+1)*tmp(i+1))*invals
              corres(i+1) = (corres(i+1)*tmp(i)-t*tmp(i+1))*invals
          end do
          corres(0) = corres(0)*tmp(0)*invals
          corres(1) = corres(1)*tmp(1)*invals
          call rdft(nvals,-1,corres,ip,w)     ! Inverse FFT
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             ccerr = .true.
             write(ERROR_UNIT,*) "=============================================================="
             write(ERROR_UNIT,*) " cpu_time_correlation: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "=============================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF            
    end subroutine
    
    
    ! SYSTEM_CLOCK start
    ! 1st measurement
    subroutine sys_clock_start(this,idx)
          implicit none
          type(PerfTimer_t), intent(inout) :: this
          integer(I32P),     intent(in)    :: idx
          ! Start of executable statements
          call SYSTEM_CLOCK(this%m_scresults(1,idx))
    end subroutine
    
    ! SYSTEM_CLOCK stop
    ! 2nd measurement
    subroutine sys_clock_stop(this,idx)
          implicit none
          type(PerfTimer_t), intent(inout) :: this
          integer(I32P),     intent(in)    :: idx
          ! Start of executable statements
          call SYSTEM_CLOCK(this%m_scresults(2,idx))
    end subroutine
    
    ! Computes SYSTEM_CLOCK measurements delta.
    subroutine sys_clock_delta(vals,delta,nvals,invi)
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          integer(I64P), dimension(:,:), intent(inout) :: vals
          real(R64P),    dimension(:),   intent(out)   :: delta
          integer(I32P),                 intent(in)    :: nvals
          integer(I32P),                 intent(out)   :: invi   ! Counter of invalid values
          ! Loclas
          integer(I32P) :: i,j
          ! Start of executable statements
          ! Check for invalid values
          invi = 0
          do j = 1, 2
              do i = 1, nvals
                  if(vals(i,j).LE.0_I64P) then
                      invi = invi+1
                      write(ERROR_UNIT,*) "sys_clock_delta: Invalid value at:", "i=",i, "j=",j
                      write(ERROR_UNIT,*) " Counter of invalide values=", invi
                      ! Correct invalid value
                      vals(i,j) = 0_I64P
                  end if
              end do
          end do
          ! Compute measurements delta
          do i = 1, nvals
              delta(i) = DBLE(vals(2,i)-vals(1,i))
          end do
    end subroutine      
    
    ! subroutine: sys_clock_minmax
    ! Minimum and maximum values of start,stop and delta.
    ! Min(row: 1), Max(row: 1)
    ! Min(row: 2), Max(row: 2)
    ! Min(row: 1), Max(row: 1)
    subroutine sys_clock_minmax(vals,delta,nvals,invi, &
                                minv,maxv,minl,maxl,   &
                                mins,maxs,minsl,maxsl  )
          implicit none
          integer(I64P), dimension(:,:), intent(in)  :: vals
          real(R64P),    dimension(:),   intent(in)  :: delta
          integer(I32P),                 intent(in)  :: nvals
          integer(I32P),                 intent(in)  :: invi
          integer(I64P), dimension(2),   intent(out) :: minv,maxv
          integer(I32P), dimension(2),   intent(out) :: minl,maxl
          real(R64P),                    intent(out) :: mins,maxs
          integer(I32P),                 intent(out) :: minsl,maxsl
          ! Start of executable statements
          ! Check if invalid exists
          if(invi.GT.0) then
              minv  = MINVAL(vals,dim=2,mask=vals.GT.0_I64P)
              maxv  = MAXVAL(vals,dim=2,mask=vals.GT.0_I64P)
              minl  = MINLOC(vals,dim=2,mask=vals.GT.0_I64P)
              maxl  = MAXLOC(vals,dim=2,mask=vals.GT.0_I64P)
              mins  = MINVAL(delta,mask=delta.GT.0._R64P)
              maxs  = MAXVAL(delta,mask=delta.GT.0._R64P)
              minsl = MINLOC(delta,mask=delta.GT.0._R64P)
              maxsl = MAXLOC(delta,mask=delta.GT.0._R64P)
          else
              minv  = MINVAL(vals,dim=2)
              maxv  = MAXVAL(vals,dim=2)
              minl  = MINLOC(vals,dim=2)
              maxl  = MAXLOC(vals,dim=2)
              mins  = MINVAL(delta)
              maxs  = MAXVAL(delta)
              minsl = MINLOC(delta)
              maxsl = MAXLOC(delta)
          end if
    end subroutine
    
    ! Compute statistics of SYSTEM_CLOCK timer
    ! measurements
    ! Following statistical calculation are in use
    ! 1) Arithemtic mean of n-runs
    ! 2) Average deviation of n-runs
    ! 3) Standard deviation of n-runs
    ! 4) Skewness 
    ! 5) Kurtosis                            
    subroutine sys_clock_stats(vals,nvals,mean,adev,sdev,skew,  &
                               kurt,invi,ccerr,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF                              
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:),    intent(in)    :: vals
          integer(I32P),               intent(in)    :: nvals
          real(R64P),                  intent(out)   :: mean,adev,sdev, &
                                                        skew,kurt
          integer(I32P),               intent(inout) :: invi
          logical(I32P), dimension(2), intent(inout) :: ccerr
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)    :: sum,var,isdev,t,t2,t3,t4,t5,fracp,ct2
          integer(I32P) :: i,nfrac
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of inputs
          if(ANY(ccerr)) then
              ccerr = .false.
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          mean = 0._R64P
          adev = 0._R64P
          sdev = 0._R64P
          skew = 0._R64P
          kurt = 0._R64P
          sum  = 0._R64P
          t   = 0._R64P
          var = 0._R64P
          t2  = 0._R64P
          t3  = 0._R64P
          t4  = 0._R64P
          t5  = 0._R64P
          ct2 = 0._R64P
          fracp = 1._R64P
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          if(invi.GT.0) then
              nfrac = nvals-invi
              ! Compute sample mean
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))
              do i = 1, nfrac
                 sum = sum+vals(i)
              end do
              mean = sum/DBLE(nfrac)
              ! Compute average deviation and variance
             
              do i = 1, nfrac
                  t = DABS(vals(i)-mean)
                  adev = adev+t
                  t2 = (vals(i)-mean)*(vals(i)-mean)
                  ct2 = t2
                  fracp = t2-INT(ct2,KIND=8)
                  if(fracp.LE.EPSILON(1._R64P)) then
                      write(ERROR_UNIT,*) "sys_clock_stats: Losing siginificance digits!!"
                      ccerr(1) = .true.
                  end if
                  var = var+t2
              end do
              adev = adev/(DBLE(nfrac)
              if(var.LE.0._R64P) then
                 write(ERROR_UNIT,*) "sys_clock_stats: Invalid variance=",var
                
                 return
              end if
              var = var/DBLE(nfrac-1)
              sdev = DSQRT(var)
              isdev = 1._R64P/sdev
             
              ! Compute kurtosis and skewness
              fracp = -1._R64P
              do i = 1, nfrac
                 t3 = (vals(i)-mean)*isdev
                 ct2 = t3
                 fracp = t3-INT(ct2,KIND=8)
                 if(fracp.LE.EPSILON(1._R64P)) then
                     write(ERROR_UNIT,*) "sys_clock_stats: Losing significance digits!!"
                     ccerr(2) = .true.
                 end if
                 t4 = t3**3
                 skew = skew+t4
                 t5 = t4*t3
                 kurt = kurt+t5
              end do
              skew = skew/DBLE(nfrac)
              kurt = kurt/(DBLE(nfrac))-3._R64P
              if(kurt.LT.1._R64P) then
                  write(ERROR_UNIT,*) "cpu_time_stats: Invalid kurtosis!"
                  write(ERROR_UNIT,*) "kurtosis=", kurtosis
                  
                  return
              end if 
          else
              !DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))
              do i = 1, nvals
                 sum = sum+vals(i)
              end do
              mean = sum/DBLE(nvals)
              ! Compute average deviation and variance
             
              do i = 1, nvals
                  t = DABS(vals(i)-mean)
                  adev = adev+t
                  t2 = (vals(i)-mean)*(vals(i)-mean)
                  ct2 = t2
                  fracp = t2-INT(ct2,KIND=8)
                  if(fracp.LE.EPSILON(1._R64P)) then
                      write(ERROR_UNIT,*) "sys_clock_stats: Losing siginificance digits!!"
                      ccerr(1) = .true.
                  end if
                  var = var+t2
              end do
              adev = adev/(DBLE(nvals)
              if(var.LE.0._R64P) then
                 write(ERROR_UNIT,*) "sys_clock_stats: Invalid variance=",var
                 ccerr = .true.
                 return
              end if
              var = var/DBLE(nvals-1)
              sdev = DSQRT(var)
              isdev = 1._R64P/sdev
             
              ! Compute kurtosis and skewness
              fracp = -1._R64P
              do i = 1, nvals
                 t3 = (vals(i)-mean)*isdev
                 ct2 = t3
                 fracp = t3-INT(ct2,KIND=8)
                 if(fracp.LE.EPSILON(1._R64P)) then
                     write(ERROR_UNIT,*) "Losing significance digits!"
                     ccerr(2) = .true.
                 end if
                 t4 = t3**3
                 skew = skew+t4
                 t5 = t4*t3
                 kurt = kurt+t5
              end do
              skew = skew/DBLE(nvals)
              kurt = kurt/(DBLE(nvals))-3._R64P
              if(kurt.LT.1._R64P) then
                  write(ERROR_UNIT,*) "cpu_time_stats: Invalid kurtosis!"
                  write(ERROR_UNIT,*) "kurtosis=", kurtosis
                 
                  return
              end if 
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
            
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " sys_clock_stats: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
                               
    ! SYSTEM_CLOCK timing results dataset correlation.
    ! Compute Correlation of two measurement data sets
    ! by using real FFT.
    ! Data sets were obtained by the calls to SYSTEM_CLOCK wrapper
    ! subroutine.                           
    subroutine sys_clock_correlation(vals1,vals2,corres,nvals,ip,w,fp_flags)
          use mod_fftsg, only : rdft
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF                              
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none    
          real(R64P), dimension(0:nvals-1), intent(in)    :: vals1,vals2 ! Input datasets
          real(R64P), dimension(0:nvals-1), intent(out)   :: corres ! Output-correlated values
          integer(I32P),                    intent(in)    :: nvals ! Must be power of 2
          integer(I32P), dimension(0:*),    intent(inout) :: ip
          real(R64P),    dimension(0:*),    intent(inout) :: w ! Work array
          ! Locals
          real(R64P), dimension(0:nvals-1) :: tmp
          real(R64P)  :: t,invals
          integer(I32P) :: i,snvals ! rshifted by 1 i.e snvals = RSHIFT(nvals,1)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity checking on inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          ! Copy samples to tmp and to corres
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 0, nvals-1
              tmp(i)    = vals(i)
              corres(i) = vals(i)
          end do
          snvals = RSHIFT(nvals,1)
          invals = 1._R64P/DBLE(snvals)
          ! Call FFT 
          ip(0) = 0
          ! Prepare floating-point environment
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
          call rdft(nvals,1,tmp,ip,w)
          call rdft(nvals,1,corres,ip,w)
          do i = 2, nvals, 2
              t = corres(i)
              corres(i) = (corres(i)*tmp(i)+corres(i+1)*tmp(i+1))*invals
              corres(i+1) = (corres(i+1)*tmp(i)-t*tmp(i+1))*invals
          end do
          corres(0) = corres(0)*tmp(0)*invals
          corres(1) = corres(1)*tmp(1)*invals
          call rdft(nvals,-1,corres,ip,w)     ! Inverse FFT contains correlated data of two sets
                                              ! of timing measurement: vals1 and vals2
          ! Grab the result of any floating-point exception(s) (up to 5 exceptions)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
            
             write(ERROR_UNIT,*) "=============================================================="
             write(ERROR_UNIT,*) " sys_clock_correlation: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "=============================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine   
    
    ! DCLOCK timer - start measurement
    subroutine dclock_start(this,idx)
          use IFPORT
          implicit none
          type(PerfTimer_t), intent(inout) :: this
          integer(I32P),     intent(in)    :: idx
          ! Start of executable statements
          this%m_dcresults(1,idx) = DCLOCK()
    end subroutine
    
    ! DCLOCK timer - stop measurement
    subroutine dclock_stop(this,idx)
          use IFPORT
          implicit none
          type(PerfTimer_t), intent(inout) :: this
          integer(I32P),     intent(in)    :: idx
          ! Start of executable statements
          this%m_dcresults(2,idx) = DCLOCK()
    end subroutine
    
    ! Computes DCLOCK timer measurement delta
    subroutine dclock_delta(vals,nvals,invi)
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:), intent(inout) :: vals
          integer(I32P),              intent(in)    :: nvals
          logical(I32P), dimension(5),intent(out) :: invi
          ! Locals
          integer(I32P) :: j,i
          ! Start of executable statements
          ! Check for invalid values.
          ! Assumes that bad value will be either zero or negative one.
          invi = 0
          do j = 1, 2
              do i = 1, nvals
                  if(vals(i,j).LE.0._R64P) then
                      invi = invi+1
                      write(ERROR_UNIT,*) "dclock_delta: Invalid value at:", "j:", j, "i:",i
                      write(ERROR_UNIT,*) "Counter of invalid values: ", invi
                      ! Correct the bad ones
                      vals(i,j) = 0._R64P
                  end if
              end do
          end do
          ! Compute the deltas
          do i = 1, nvals
              vals(3,i) = vals(2,i)-vals(1,i)
          end do
    end subroutine
    
    ! Computes dclock time measurements maximum and minimum
    ! per start values,stop values and delta values
    subroutine dclock_minmax(vals,minv,maxv,minl,maxl,invi)
          implicit none
          real(R64P), dimension(:,:), intent(inout) :: vals
          real(R64P), dimension(3),   intent(out)   :: minv,maxv
          integer(I32P), dimension(3),intent(out)   :: minl,maxl
          integer(I32P),              intent(in)    :: invi
          ! Start of executable statemetns
          ! Check if invalid values were corrected
          if(invi.GT.0) then
              minv = MINVAL(vals,dim=2,mask=vals.GT.0._R64P)
              maxv = MAXVAL(vals,dim=2,mask=vals.GT.0._R64P)
              minl = MINLOC(vals,dim=2,mask=vals.GT.0._R64P)
              maxl = MAXLOC(vals,dim=2,mask=vals.GT.0._R64P)
          else
              minv = MINVAL(vals,dim=2)
              maxv = MAXVAL(vals,dim=2)
              minl = MINLOC(vals,dim=2)
              maxl = MAXLOC(vals,dim=2)
          end if
    end subroutine
    
    ! Compute statistics of DCLOCK timer
    ! measurements
    ! Following statistical calculation are in use
    ! 1) Arithemtic mean of n-runs
    ! 2) Average deviation of n-runs
    ! 3) Standard deviation of n-runs
    ! 4) Skewness 
    ! 5) Kurtosis 
    subroutine dclock_stats(vals,nvals,mean,adev,sdev,skew, &
                             kurt,invi,ccerr,fp_flags  )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF                              
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:),  intent(in)    :: vals
          integer(I32P),               intent(in)    :: nvals
          real(R64P),                  intent(out)   :: mean,adev,sdev, &
                                                        skew,kurt
          integer(I32P),               intent(in)    :: invi   ! Invalid values indicator
          logical(I32P), dimension(2), intent(out)   :: ccerr  ! Catastrophic cancellation indicator
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)    :: sum,var,isdev,t,t2,t3,t4,t5,fracp,ct2
          integer(I32P) :: i,nfrac
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity checking of inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          if(ANY(ccerr)) then
              ccerr = .false.
          end if
          mean = 0._R64P
          adev = 0._R64P
          sdev = 0._R64P
          skew = 0._R64P
          kurt = 0._R64P
          sum  = 0._R64P
          var  = 0._R64P
          t  = 0._R64P;t2 = 0._R64P
          t3 = 0._R64P;t4 = 0._R64P
          t5 = 0._R64P;ct2 = 0._R64P
          fracp = -1._R64P
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF         
          if(invi.GT.0) then
              nfrac = nvals-invi
              ! Compute sample mean
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))
              do i = 1, nfrac
                  sum = sum+vals(3,i)
              end do
              mean = sum/DBLE(nfrac)
              ! Compute absolute deviation and standard deviation
              do i = 1, nfrac
                  t = DABS(vals(3,i)-mean)
                  adev = adev+t
                  t2 = (vals(3,i)-mean)*(vals(3,i)-mean)
                  ct2 = t2
                  fracp = t2-INT(ct2,KIND=8))
                  if(fracp.LE.EPSILON(1._R64P)) then
                      write(ERROR_UNIT,*) "dclock_stats: Losing significance digits!"
                      ccerr(1) = .true.
                  end if
                  var = var+t2
              end do
              adev = adev/DBLE(nfrac)
              if(var.LE.0._R64P) then
                  write(ERROR_UNIT,*) "dclock_stats: Invalid value of variance: ", var
                  return
              end if
              var = var/DBLE(nfrac-1)
              sdev = DSQRT(var)
              isdev = 1._R64P/sdev
              ! Compute skewness and kurtosis
              fracp = -1._R64P
              do i = 1, nfrac
                 t3 = (vals(3,i)-mean)*isdev
                 ct2 = t3
                 fracp = t3-INT(ct2,KIND=8)
                 if(fracp.LE.EPSILON(1._R64P)) then
                     write(ERROR_UNIT,*) "dclock_stats: Losing significance digits!"
                     ccerr(2) = .true.
                 end if
                 t4 = t3**3
                 skew = skew+t4
                 t5 = t4*t3
                 kurt = kurt+t5
              end do
              skew = skew/DBLE(nfrac)
              kurt = kurt/(DBLE(nfrac))-3._R64P
              if(kurt.LT.1._R64P) then
                  write(ERROR_UNIT,*) "cpu_time_stats: Invalid kurtosis!"
                  write(ERROR_UNIT,*) "kurtosis=", kurtosis
                  return
              end if
          else
               ! Compute sample mean
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))
              do i = 1, nvals
                  sum = sum+vals(3,i)
              end do
              mean = sum/DBLE(nvals)
              ! Compute absolute deviation and standard deviation
              do i = 1, nvals
                  t = DABS(vals(3,i)-mean)
                  adev = adev+t
                  t2 = (vals(3,i)-mean)*(vals(3,i)-mean)
                  ct2 = t2
                  fracp = t2-INT(ct2,KIND=8))
                  if(fracp.LE.EPSILON(1._R64P)) then
                      write(ERROR_UNIT,*) "dclock_stats: Losing significance digits!"
                      ccerr(1) = .true.
                  end if
                  var = var+t2
              end do
              adev = adev/DBLE(nfrac)
              if(var.LE.0._R64P) then
                  write(ERROR_UNIT,*) "dclock_stats: Invalid value of variance: ", var
                  return
              end if
              var = var/DBLE(nfrac-1)
              sdev = DSQRT(var)
              isdev = 1._R64P/sdev
              ! Compute skewness and kurtosis
              fracp = -1._R64P
              do i = 1, nvals
                 t3 = (vals(3,i)-mean)*isdev
                 ct2 = t3
                 fracp = t3-INT(ct2,KIND=8)
                 if(fracp.LE.EPSILON(1._R64P)) then
                     write(ERROR_UNIT,*) "dclock_stats: Losing significance digits!"
                     ccerr(2) = .true.
                 end if
                 t4 = t3**3
                 skew = skew+t4
                 t5 = t4*t3
                 kurt = kurt+t5
              end do
              skew = skew/DBLE(nfrac)
              kurt = kurt/(DBLE(nfrac))-3._R64P
              if(kurt.LT.1._R64P) then
                  write(ERROR_UNIT,*) "cpu_time_stats: Invalid kurtosis!"
                  write(ERROR_UNIT,*) "kurtosis=", kurtosis
                  return
              end if
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
            
             write(ERROR_UNIT,*) "=============================================================="
             write(ERROR_UNIT,*) " dclock_correlation: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "=============================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
                             
     ! Compute two measurememt datasets correlation.
     ! FFT is used to do the computation
     subroutine dclock_correlation(vals1,vals2,corres,nvals,ip,w,fp_flags)
          use mod_fftsg, only : rdft
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF                              
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(0:nvals-1), intent(in)    :: vals1,vals2
          real(R64P), dimension(0:nvals-1), intent(out)   :: corres
          integer(I32P),                    intent(in)    :: nvals ! Must be of power 2
          integer(I32P), dimension(0:*),    intent(inout) :: ip
          real(R64P),    dimension(0:*),    intent(inout) :: w
          logical(I32P), dimension(5),      intent(inout) :: fp_flags
          ! Locals
          real(R64P), dimension(0:nvals-1) :: temp
          real(R64P)    :: invals
          integer(I32P) :: i,snvals
          ! Start of executable statements
          ! Sanity check of inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          ! Copy samples to tmp and to corres
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 0, nvals-1
              tmp(i)    = vals1(i)
              corres(i) = vals2(i)
          end do
          snvals = RSHIFT(nvals,1)
          invals = 1._R64P/DBLE(snvals)
          ip(0) = 0
          ! Prepare floating-point environment
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          ! Call rdft
          call rdft(nvals,1,tmp,ip,w)
          call rdft(nvals,1,corres,ip,w)
          do i = 2, nvals, 2
              t = corres(i)
              corres(i) = (corres(i)*tmp(i)+corres(i+1)*tmp(i+1))*invals
              corres(i+1) = (corres(i+1)*tmp(i)-t*tmp(i+1))*invals
          end do
          corres(0) = corres(0)*tmp(0)*invals
          corres(1) = corres(1)*tmp(1)*invals
          call rdft(nvals,-1,corres,ip,w) ! Inverse FFT
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
            
             write(ERROR_UNIT,*) "=============================================================="
             write(ERROR_UNIT,*) " dclock_correlation: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "=============================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
     end subroutine 
     
     ! QueryPerformanceCounter timer - start measurement
     subroutine qpc_start(this,idx,cfreq,bfail)
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          type(PerfTimer_t), intent(inout) :: this
          integer(I32P),     intent(in)    :: idx
          real(R64P),        intent(inout) :: cfreq
          integer(BOOL),     intent(inout) :: bfail
          ! Locals
          type(T_LARGE_INTEGER) :: ifreq
          integer(I64P)         :: tmp
          ! Start of executable statements
          bfail = QueryPerformanceFrequency(ifreq)
          if(bfail .EQ. 0) then
              write(ERROR_UNIT,*) "qpc_start: Failed to obtain frequency counter!!"
              return
          end if
          tmp = INT(IOR(LSHIFT(ifreq.HighPart,32),ifreq.LowPart),KIND=8) 
          cfreq = DBLE(tmp)/1000._R64P
          bfail = QueryPerformanceCounter(ifreq)
          this%m_qpcresult(1,idx) = ifreq
     end subroutine
     
     ! QueryPerformanceCounter timer - stop measurement
     subroutine qpc_stop(
     

end module mod_timer