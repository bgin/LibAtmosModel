
module mod_sawtooth_signal

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_sawtooth_signal'
 !          
 !          Purpose:
 !                   Sawtooth Signal representation
 !                   approximated by Fourier series
 !                   and additionally reprsented by
 !                     
 !          History:
 !                        Date: 14-08-2017
 !                        Time: 15:41 GMT+2
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
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT , &
                                stdout=>OUTPUT_UNIT
    use IFPORT, only : TRACEBACKQQ
    use module_kinds
    use mod_constants
    
    use module_logger
    use mod_code_timing
    use mod_complex_arithm , only : vcmag
    use mod_jonesvec
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(I32P), parameter, public :: MOD_SAWTOOTH_SIGNAL_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_SAWTOOTH_SIGNAL_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_SAWTOOTH_SIGNAL_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_SAWTOOTH_SIGNAL_FULLVER = 1000*MOD_SAWTOOTH_SIGNAL_MAJOR+100*MOD_SAWTOOTH_SIGNAL_MINOR + &
                                                                      10*MOD_SAWTOOTH_SIGNAL_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_SAWTOOTH_SIGNAL_CREATE_DATE = "14-08-2017 15:59 +00200 (MON 14 AUG 2017 GMT+2)"
    
    ! Module build date (should be set after every successful build)
    character(*),  parameter, public :: MOD_SAWTOOTH_SIGNAL_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_SAWTOOTH_SIGNAL_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_SAWTOOTH_SIGNAL_DESCRIPT = "Sawtooth signal approximated by Fourier Series."
    
    !======================================================60
    !  Type: SawtoothSignal_t
    !======================================================60
    
    type :: SawtoothSignal_t
        
        private
        
        ! Signal name
        character(len=64)   :: m_name
        
        ! Signal ID
        integer(I32P)       :: m_sid
        
        ! Number of signal samples
        integer(I32P)       :: m_nsamp
        
        ! Number of Fourier series
        integer(I32P)       :: m_maxk
        
        ! Signal duration
        real(R64P)          :: m_dur
        
        ! Sawtooth amplitude
        real(R64P)          :: m_A
        
        ! Sawtooth initial time
        real(R64P)          :: m_sinit
        
        ! Sawtooth increment time-step
        real(R64P)          :: m_stsinc
        
        ! Sawtooth frequency
        real(R64P)          :: m_freq
        
        ! List of constructors
        character(len=32)   :: m_ctors
        
        ! Jones Vector
        type(JonesVector_t) :: m_jvec
        
        ! Sine components of K sinusoids
        real(R64P), allocatable, dimension(:,:) :: m_scomp
!DIR$   ATTRIBUTES ALIGN : 32 :: m_scomp
        
        ! Cosine components of K cosinusoids
        real(R64P), allocatable, dimension(:,:) :: m_ccomp
!DIR$   ATTRIBUTES ALIGN : 32 :: m_ccomp
        
        ! Sawtooth carrier frequency*t
        real(R64P), allocatable, dimension(:)   :: m_wct
!DIR$   ATTRIBUTES ALIGN : 32 :: m_phi
        
        ! Signal natural envelope
        real(R64P), allocatable, dimension(:)   :: m_nenvp
!DIR$   ATTRIBUTES ALIGN : 32 :: m_nenvp 
        
        ! Signal  i.e. Sawtooth(phi(t))
        ! approximated by K sinusoids
        real(R64P), allocatable, dimension(:)   :: m_sawtooth
        
        ! Sawtooth complex representation
        complex(R64P), allocatable, dimension(:) :: m_cform
!DIR$   ATTRIBUTES ALIGN : 32 :: m_canform 
        
        ! Electric field
        complex(R64P), allocatable, dimension(:) :: m_E
!DIR$   ATTRIBUTES ALIGN : 32 :: m_E
        
         ! Signal amplitude  A(phi,theta)/r used for calculation electric field
         ! far from emmiter(antenna)
         real(R64P), allocatable, dimension(:)   :: m_amp
!DIR$   ATTRIBUTES ALIGN : 32 :: m_amp
         
 ! Time-averaged power density
      
         complex(R64P), allocatable, dimension(:) :: m_S
!DIR$   ATTRIBUTES ALIGN : 32 :: m_S
         
 ! Logical member denoting built status
        logical(I32P) :: m_isbuilt
        
        contains
    
        !========================================
        !  Constructor, destructor subroutines
        !========================================
    
        ! Default constructor creates zero waveform
        ! Can be used as zero interval in signal train
        procedure, pass(this), public :: default_signal
        
        ! Construct Sawtooth signal approximated by its
        ! Fourier series
        procedure, pass(this), public :: create_signal
        
        ! Copy-signal, effectively a deep copy constructor
        procedure, pass(this), public :: copy_signal
        
        ! Destroy signal 
        procedure, pass(this), public :: destroy_signal
        
        !==========================================52
        !   Getter pure functions
        !==========================================52
        
        procedure, pass(this), public :: get_name
        
        procedure, pass(this), public :: get_sid
        
        procedure, pass(this), public :: get_nsamp
        
        procedure, pass(this), public :: get_maxk
        
        procedure, pass(this), public :: get_dur
        
        procedure, pass(this), public :: get_A
        
        procedure, pass(this), public :: get_sinit
        
        procedure, pass(this), public :: get_stsinc
        
        procedure, pass(this), public :: get_freq
        
        procedure, pass(this), public :: get_ctors
        
        procedure, pass(this), public :: get_jvec
        
        procedure, pass(this), public :: get_scomp
        
        procedure, pass(this), public :: get_ccomp
        
        procedure, pass(this), public :: get_wct
        
        procedure, pass(this), public :: get_nenvp
        
        procedure, pass(this), public :: get_sawtooth
        
        procedure, pass(this), public :: get_cform
        
        procedure, pass(this), public :: get_E
        
        procedure, pass(this), public :: get_amp
        
        procedure, pass(this), public :: get_S
        
        procedure, pass(this), public :: get_built_stat
        
        !==================================================60
        ! Computational subroutines
        !==================================================60
        
        procedure, pass(this), public :: dphi_dt
        
       
        
        !==============================================56
        ! write/read subroutines
        !==============================================56
        
        procedure, nopass,     public :: to_screenu
        
    end type SawtoothSignal_t
        
    contains
    
    !======================================================60
    ! subroutine: default_signal
    !             default initialization (no physical meaning)
    !======================================================60
    subroutine default_signal(this,name,sid,nsamp,maxk,logging, &
                               filename,append,dbg  )
          implicit none
          class(SawtoothSignal_t), intent(inout) :: this
          character(len=*),        intent(in)    :: name
          integer(I32P),           intent(in)    :: sid
          integer(I32P),           intent(in)    :: nsamp,maxk
          logical(I32P),           intent(in)    :: logging
          character(len=*),        intent(in)    :: filename
          logical(I32P),           intent(in)    :: append,dbg
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: i,j,aerr
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:263, In->mod_sawtooth_signal/default_signal: SawtoothSignal_t already initialized!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_sawtooth_signal/default_signal:263, SawtoothSignal_t already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          if(nsamp.LT.LAM_MINSAMP) then
              nsamp = LAM_MINSAMP
          end if
          if(K.LT.LAM_MINK) then
              K = LAM_MINK
          end if
          ! Begin construction
          this%m_name   = name
          this%m_sid    = sid
          this%m_nsamp  = nsamp
          this%m_maxk   = maxk
          this%m_dur    = LAM_PINF
          this%m_A      = LAM_PINF
          this%m_sinit  = LAM_PINF
          this%m_stsinc = LAM_PINF
          this%m_freq   = LAM_PINF
          this%m_ctors(1) = "default_signal"
          this%m_jvec = JonesVector_t()
          associate(k=>this%m_maxk,n=>this%m_nsamp)
              allocate(this%m_scomp(k,n),  &
                       this%m_ccomp(k,n),  &
                       this%m_wct(n),      &
                       this%m_nenvp(n),    &
                       this%m_sawtooth(n), &
                       this%m_cform(n),    &
                       this%m_E(n),        &
                       this%m_amp(n),      &
                       this%m_S(n),        &
                       STAT=aerr,          &
                       ERRMSG=emsg )
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:309, In->mod_sawtooth_signal/default_signal: Memory allocation failure!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_sawtooth_signal/default_signal:309, Memory allocation failure!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_sawtooth_signal/default_signal:309 -> [FATAL-ERROR]: Terminating execution"
          end if 
          do i = 1, this%m_maxk
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))              
              do j = 1, this%m_nsamp
                  this%m_scomp(i,j) = LAM_PINF
                  this%m_ccomp(i,j) = LAM_PINF
              end do
          end do
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))          
          do i = 1, this%m_nsamp
              this%m_wct(i) = LAM_PINF
              this%m_nenvp(i) = LAM_PINF
              this%m_sawtooth(i) = LAM_PINF
              this%m_cform(i) = DCMPLX(LAM_PINF,LAM_PINF)
              this%m_E(i) = DCMPLX(LAM_PINF,LAM_PINF)
              this%m_amp(i) = LAM_PINF
              this%m_S(i) = DCMPLX(LAM_PINF,LAM_PINF)
          end do
          this%m_isbuilt = .true.
          if(dbg) then
              print*, "Sine component:   ", this%m_scomp
              print*, "Cosine component: ", this%m_ccomp
              print*, "Wcarrier*time:    ", this%m_wct
              print*, "Natural-envelope: ", this%m_nenvp
              print*, "Sawtooth:         ", this%m_sawtooth
              print*, "Complex form:     ", this%m_canform
              print*, "Electric field:   ", this%m_E
              print*, "Amplitude:        ", this%m_amp
              print*, "PSD:              ", this%m_S
          end if
    end subroutine
                               
    !======================================================60
    !  subroutine: create_signal                        
    !              Physical representation of  Sawtooth
    !              approximated by K sinusoids (Fourier-
    !              series)
    !======================================================60                                   
    subroutine create_signal(this,name,sid,nsamp,maxk,dur,A, &
                             sinit,stsinc,freq,r,h,v,     &
                             logging,filename,append,     &
                             dbg,profiling,qpctimer      )
          implicit none
          class(SawtoothSignal_t),  intent(inout) :: this
          character(len=*),         intent(in)    :: name
          integer(I32P),            intent(in)    :: sid
          integer(I32P),            intent(inout) :: nsamp,maxk
          real(R64P),               intent(in)    :: dur,A
          real(R64P),               intent(in)    :: sinit,stsinc,freq
          real(R64P), dimension(:), intent(in)    :: r
!DIR$     ASSUME_ALIGNED r:32
          complex(R64P),            intent(in)    :: h,v
          logical(I32P)             intent(in)    :: logging
          character(len=*),         intent(in)    :: filename
          logical(I32P),            intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),         intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: k,j,aerr
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail
          real(R64P)         :: insamp,t,t2,delta,hA,API
          real(R64P),    allocatable, dimension(:) :: tcos,ts,tc
          !complex(R64P), allocatable, dimension(:) :: tmp
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:399, In->mod_sawtooth_signal/create_signal: SawtoothSignal already initialized!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_sawtooth_signal/create_signal:399, SawtoothSignal_t already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          if(nsamp.LT.LAM_MINSAMP) then
              nsamp = MINSAMP
          end if
          if(maxk.LT.LAM_MINK) then
              maxk =LAM_MINK
          end if
          if(dur.LE.LAM_ZR8 .OR. &
             A.LE.LAM_ZR8 ) then
              if(logging) then
                   call log_startup(filename,append)
                   call log_UsrMsg("logger:421, In->mod_sawtooth_signal/create_signal: Invalid arguments i.e. A or dur!")
                   call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_sawtooth_signal/create_signal:421, Invalid arguments i.e. A or dur  !)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          ! Begin construction
          this%m_name = name
          this%m_sid  = sid
          this%m_nsamp = nsamp
          this%m_maxk = maxk
          this%m_dur = dur
          this%m_A  = A
          this%m_sinit = sinit
          this%m_stsinc = stsinc
          this%m_freq = freq
          this%m_ctors(2) = "create_signal"
          this%m_jvec = JonesVector_t(h,v)
          associate(mk=>this%m_maxk,n=>this%m_nsamp)
              allocate(this%m_scomp(mk,n),  &
                       this%m_ccomp(mk,n),  &
                       this%m_wct(n),       &
                       this%m_nenvp(n),     &
                       this%m_sawtooth(n),  &
                       this%m_cform(n),     &
                       this%m_E(n),         &
                       this%m_amp(n),       &
                       this%m_S(n),         &
                       tcos(n),             &
                       ts(n),               &
                       tc(n),               &
                       STAT=aerr,           &
                       ERRMSG=emsg )
                        
                       
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:464, In->mod_sawtooth_singnal/create_signal: Memory allocation failure!!"
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_sawtoth_signal/create_signal:464, Memory allocation failure!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_sawtooth_signal/create_signal:464 -> [FATAL-ERROR]: Terminating execution!!"
          end if
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_sawtooth_signal/create_signal: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
          ! Create sine and cosine Fourier series components
          hA = LAM_HR64P*this%m_A
          API = this%m_A/LAM_PI
          insamp = 1._R64P/DBLE(this%m_nsamp)
          do k = 1, this%m_maxk
              do j = 1, this%m_nsamp
                  this%m_sinit = this%m_sinit+this%m_stsinc
                  delta = DBLE(i)*insamp
                  t = this%m_sinit*delta
                 
                  this%m_scomp(k,j) = -1**k*DSIN(LAM_2PI*k*this%m_freq*t)/k
                  this%m_ccomp(k,j) = -1**k*DCOS(LAM_2PI*k*this%m_freq*t)/k
              end do
          end do
          ! Do summation over the columns
          this%m_sawtooth = SUM(this%m_scomp,dim=2)
          tcos = SUM(this%m_ccomp,dim=2)
          ! Create sawtooth signal (final approximation) and
          ! create signal complex representation
          do j = 1, this%m_nsamp
              this%m_sinit = this%m_sinit+this%m_stsinc
              delta = DBLE(i)*insamp
              t = this%m_sinit*delta
              this%m_wct(j) = t
              this%m_sawtooth(j) = hA-(API*this%m_sawtooth(j))
              tcos(j) = hA-(API*tcos(j))
              this%m_cform(i) = DCMPLX(tcos(j),this%m_sawtooth(j))
          end do
          ! Compute natural envelope
          call vcmag(tmp,this%m_nenvp)
          ! Create electrical field far from the emitter.
          do k = 1, this%m_maxk
              do j = 1, this%m_nsamp
                  this%m_sinit = this%m_sinit+this%m_stsinc
                  delta = DBLE(i)*insamp
                  t = this%m_sinit*delta
                  t2 = t-r(i)/LAM_c
                  this%m_scomp(k,j) = -1**k*DSIN(LAM_2PI*k*this%m_freq*t2)/k
                  this%m_ccomp(k,j) = -1**k*DCOS(LAM_2PI*k*this%m_freq*t2)/k
              end do
          end do
          ! Do summation over the columns
          tc = SUM(this%m_ccomp,dim=2)
          ts = SUM(this%m_scomp,dim=2)
          do j = 1, this%m_nsamp
               tc(j) = hA-(API*tc(j))
               ts(j) = hA-(API*ts(j))
               this%m_E(i) = DCMPLX(tc(j),ts(j))
          end do
          ! Compute A(phi,theta)/r - magnitude of electric field E
          call vcmag(this%m_E,this%m_amp)
          ! Compute time-average power density i.e. 0.5xExE*/n
          do j = 1, this%m_nsamp
              this%m_S(j) = LAM_HR64P*this%m_E(j)*DCONJG(this%m_E(j))*0.0026525198938992_R64P
          end do
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_sawtooth_signal/create_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_sawtooth_signal/create_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
          
     end subroutine
                             
    !======================================================60                         
    ! subroutine: copy_signal
    !======================================================60                        
    subroutine copy_signal(this,other,logging,filename,append)
          implicit none
          class(SawtoothSignal_t), intent(inout) :: this
          class(SawtoothSignal_t), intent(in)    :: other
          logical(I32P),           intent(in)    :: logging
          character(len=*),        intent(in)    :: filename
          logical(I32P),           intent(in)    :: append
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          if(LOC(this).EQ.LOC(other) .OR. &
             this%m_isbuilt .EQ. .true. ) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:578, In->mod_sawtooth_signal/copy_signal: Attempted self-assignment, or argument in invalid state!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_sawtooth_signal/copy_signal:578, Attempted self_assignment, or argument in invalid state!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
             end if
             return
          end if
          this%m_name     = other%m_name
          this%m_sid      = other%m_sid
          this%m_nsamp    = other%m_nsamp
          this%m_maxk     = other%m_maxk
          this%m_dur      = other%m_dur
          this%m_A        = other%m_A
          this%m_sinit    = other%m_sinit
          this%m_stsinc   = other%m_stsinc
          this%m_freq     = other%m_freq
          this%m_ctors(3) = "copy_signal"
          this%m_jvec     = other%m_jvec
          this%m_scomp    = other%m_scomp
          this%m_ccomp    = other%m_ccomp
          this%m_wct      = other%m_wct
          this%m_nenvp    = other%m_nenvp
          this%m_sawtooth = other%m_sawtooth
          this%m_cform    = other%m_cform
          this%m_E        = other%m_E
          this%m_amp      = other%m_amp
          this%m_S        = other%m_S
          this%m_isbuilt  = .true.
    end subroutine
    
    !======================================================60
    !  subroutine: destroy_signal
    !======================================================60
    subroutine destroy_signal(this,logging,filename,append)
          implicit none
          class(SawtoothSignal_t), intent(inout) :: this
          logical(I32P),           intent(in)    :: logging
          character(len=*),        intent(in)    :: filename
          logical(I32P),           intent(in)    :: append
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:628, In->mod_sawtooth_signal/destroy_signal: SawtoothSignal_t already destroyed!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL============================================="
                  write(sdterr,*) " ( mod_sawtooth_signal/destroy_signal:628, SawtoothSignal_t already destroyed!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL============================================="
              end if
              return
          end if
          ! Set scalar members to default values
          this%m_name = " "
          this%m_sid  = -1
          this%m_nsamp = 0
          this%m_maxk = LAM_PINF
          this%m_dur = LAM_PINF
          this%m_A = LAM_PINF
          this%m_sinit = LAM_PINF
          this%m_stsinc = LAM_PINF
          this%m_freq = LAM_PINF
          call this%m_jvec%destroy_jvec()
          ! Array members deallocations
          if(ALLOCATED(this%m_scomp)) then
              deallocate(this%m_scomp, &
                         STAT=derr,    &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                       call log_startup(filename,append)
                       call log_UsrMsg("logger:660, In->mod_sawtooth_signal/destroy_signal: Deallocation of [m_scomp]: failed!")
                       call log_shutdown()
                  else
                       call DATE_AND_TIME(date=dstr,time=tstr)
                       write(stderr,*) "=========================FATAL=================================================="
                       write(stderr,*) "  ( mod_sawtooth_signal/destroy_signal:660, Deallocation of [m_scomp]: failed)"
                       write(stderr,*) "   System: ", emsg
                       write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                       write(stderr,*) "=========================FATAL=================================================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_sawtooth_signal/destroy_signal:660, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_ccomp)) then
              deallocate(this%m_ccomp, &
                         STAT=derr,    &
                         ERRMSG=emsg )
                if(derr.NE.0) then
                    if(logging) then
                        call log_startup(filename,append)
                        call log_UsrMsg("logger:685, In->mod_sawtooth_signal/destroy_signal: Deallocation of [m_ccomp]: failed!")
                        call log_shutdown()
                    else
                        call DATE_AND_TIME(date=dstr,time=tstr)
                        write(stderr,*) "=========================FATAL===================================================="
                        write(stderr,*) "  ( mod_sawtooth_signal/destroy_signal:685, Deallocation of [m_ccomp]: failed)"
                        write(stderr,*) "   System: ", emsg
                        write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                        write(stderr,*) "=========================FATAL===================================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_sawtooth_signal/destroy_signal:685, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_wct)) then
              deallocate(this%m_wct,   &
                         STAT=derr,    &
                         ERRMSG=emsg  )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:710, In->mod_sawtooth_signal/destroy_signal: Deallocation of [m_wct]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL================================================"
                      write(stderr,*) "  ( mod_sawtooth_signal/destroy_signal:710, Deallocation of [m_wct]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                        write(stderr,*) "=========================FATAL=============================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_sawtooth_signal/destroy_signal:710, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_nenvp)) then
              deallocate(this%m_nenvp,  &
                         STAT=derr,     &
                         ERRMSG=emsg  )
               if(derr.NE.0) then
                   if(logging) then
                       call log_startup(filename,append)
                       call log_UsrMsg("logger:735, In->mod_sawtooth_signal/destroy_signal: Deallocation of [m_nenvp]: failed!")
                       call log_shutdown()
                   else
                       call DATE_AND_TIME(date=dstr,time=tstr)
                       write(stderr,*) "=========================FATAL==================================================="
                       write(stderr,*) "  ( mod_sawtooth_signal/destroy_signal:735, Deallocation of [m_nenvp]: failed)"
                       write(stderr,*) "   System: ", emsg
                       write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                         write(stderr,*) "=========================FATAL=================================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_sawtooth_signal/destroy_signal:735, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_sawtooth)) then
              deallocate(this%m_sawtooth, &
                         STAT=derr,       &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:760, In->mod_sawtooth_signal/destroy_signal: Deallocation of [m_sawtooth]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL======================================================"
                      write(stderr,*) "  ( mod_sawtooth_signal/destroy_signal:760, Deallocation of [m_sawtooth]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                         write(stderr,*) "=========================FATAL==================================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_sawtooth_signal/destroy_signal:760, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_cform)) then
              deallocate(this%m_cform, &
                         STAT=derr,    &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:784, In->mod_sawtooth_signal/destroy_signal: Deallocation of [m_cform]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL==================================================="
                      write(stderr,*) "  ( mod_sawtooth_signal/destroy_signal:784, Deallocation of [m_cform]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                         write(stderr,*) "=========================FATAL================================================"
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_sawtooth_signal/destroy_signal:785, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_E)) then
               deallocate(this%m_E,   &
                          STAT=derr,  &
                          ERRMSG=emsg )
               if(derr.NE.0) then
                   if(logging) then
                       call log_startup(filename,append)
                       call log_UsrMsg("logger:810, In->mod_sawtooth_signal/destroy_signal: Deallocation of [m_E]: failed!")
                       call log_shutdown()
                   else
                       call DATE_AND_TIME(date=dstr,time=tstr)
                       write(stderr,*) "=========================FATAL==============================================="
                       write(stderr,*) "  ( mod_sawtooth_signal/destroy_signal:810, Deallocation of [m_E]: failed)"
                       write(stderr,*) "   System: ", emsg
                       write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                       write(stderr,*) "=========================FATAL==============================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_sawtooth_signal/destroy_signal:810, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_amp)) then
              deallocate(this%m_amp,  &
                         STAT=derr,   &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:835, In->mod_sawtooth_signal/destroy_signal: Deallocation of [m_amp]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL==================================================="
                      write(stderr,*) "  ( mod_sawtooth_signal/destroy_signal:835, Deallocation of [m_amp]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                      write(stderr,*) "=========================FATAL==================================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_sawtooth_signal/destroy_signal:835, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_S)) then
              deallocate(this%m_S,  &
                         STAT=derr, &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:860, In->mod_sawtooth_signal/destroy_signal: Deallocation of [m_S]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL==================================================="
                      write(stderr,*) "  ( mod_sawtooth_signal/destroy_signal:860, Deallocation of [m_S]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                      write(stderr,*) "=========================FATAL==================================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_sawtooth_signal/destroy_signal:860, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          this%m_isbuilt = .false.
    end subroutine
    
    !======================================================60
    ! Getter pure functions
    !======================================================60
    
    pure function get_name(this) result(name)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          character(len=*) :: name
          ! Start of executable statements
          name = this%m_name
    end function
    
    pure function get_sid(this) result(sid)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: sid
          ! Start of executable statements
          sid = this%m_sid
    end function
    
    pure function get_nsamp(this) result(nsamp)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nsamp
          ! Start of executable statements
          nsamp = this%m_nsamp
    end function
    
    pure function get_maxk(this) result(maxk)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: maxk
          ! Start of executable statements
          maxk = this%m_maxk
    end function
    
    pure function get_dur(this) result(dur)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: dur
          ! Start of executable statements
          dur = this%m_dur
    end function
    
    pure function get_A(this) result(A)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: A
          ! Start of executable statements
          A = this%m_A
    end function
    
    pure function get_sinit(this) result(sinit)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: sinit
          ! Start of executable statements
          sinit = this%m_sinit
    end function
    
    pure function get_stsinc(this) result(stsinc)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: stsinc
          ! Start of executable statemetns
          stsinc = this%m_stsinc
    end function
    
    pure function get_freq(this) result(freq)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: freq
          ! Start of executable statements
          freq = this%m_freq
    end function
    
    pure function get_ctors(this) result(ctors)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          character(len=32), dimension(3) :: ctors
          ! Start of executable statements
          ctors = this%m_ctors
    end function
    
    pure function get_jvec(this) result(jvec)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          type(JonesVector_t) :: jvec
          ! Start of executable statements
          jvec = this%m_jvec
    end function
    
    pure function get_scomp(this) result(scomp)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: scomp
!DIR$     ATTRIBUTES ALIGN : 32 :: scomp
          ! Start of executable statements
          scomp = this%m_scomp
    end function
    
    pure function get_ccomp(this) result(ccomp)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: ccomp
!DIR$     ATTRIBUTES ALIGN : 32 :: ccomp
          ! Start of executable statements
          ccomp = this%m_ccomp
    end function
    
    pure function get_wct(this) result(wct)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: wct
!DIR$     ATTRIBUTES ALIGN : 32 :: wct
          ! Start of executable statements
          wct = this%m_wct
    end function
    
    pure function get_nenvp(this) result(nenvp)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: nenvp
!DIR$     ATTRIBUTES ALIGN : 32 :: nenvp
          ! Start of executable statements
          nenvp = this%m_nenvp
    end function
    
    pure function get_sawtooth(this) result(sawtooth)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: sawtooth
!DIR$     ATTRIBUTES ALIGN : 32 :: sawtooth
          ! Satrt of executable statements
          sawtooth = this%m_sawtooth
    end function
    
    pure function get_cform(this) result(cform)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: cform
!DIR$     ATTRIBUTES ALIGN : 32 :: cform
          ! Start of executable statements
          cform = this%m_cform
    end function
    
    pure function get_E(this) result(E)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: E
!DIR$     ATTRIBUTES ALIGN : 32 :: E
          ! Start of executable statemetns
          E = this%m_E
    end function
    
    pure function get_amp(this) result(amp)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: amp
!DIR$     ATTRIBUTES ALIGN : 32 :: amp
          ! Start of executable statements
          amp = this%m_amp
    end function
    
    pure function get_S(this) result(S)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: S
!DIR$     ATTRIBUTES ALIGN : 32 :: S
          ! Start of executable statements
          S = this%m_S
    end function
    
    !======================================================60
    !    Computational procedures
    !======================================================60
    ! TODO:
    !        ! Consider using CADNA to test for cancellation errors
    subroutine dphi_dt(this,dphi,sfac)
          implicit none
          class(SawtoothSignal_t),  intent(in)     :: this
          real(R64P), dimension(:), intent(out)    :: dphi
!DIR$     ASSUME_ALIGNED dphi:32
          integer(I64P),            intent(inout)  :: sfac
          ! Locals
          integer(I32P) :: i
          real(R64P)    :: eps,isfac,tmp
          ! Start of executable sttements
          if(sfac.LE.LAM_IZER8) then
              sfac =  1000000000000000_I64P
          end if
          isfac = 1._R64P/DBLE(sfac)
          eps = LAM_MEPS8**0.3333333333333333333333333333_R64P
          do i = 2, this%m_nsamp-1
              tmp = this%m_wct(i+1)-this%m_wct(i-1)
              dphi(i) = this%m_wct(i+1)-this%m_wct(i-1) / &
                        (2._R64P*eps*DMAX1(DABS(tmp),isfac)*DSIGN(tmp,tmp))
          end do
    end subroutine
    
    !======================================================60
    !  subroutine: to_screenu
    !======================================================60  
    subroutine to_screenu(this,verbose)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          logical(I32P),           intent(in) :: verbose
          ! Locals
          ! Start of executable statements
          call DATE_AND_TIME(date=dstr,time=tstr)
          print*, "======================================================"
          print*, "   ***Printing components of SawtoothSignal_t***"
          print*, "   printing subroutine called at: " , &
                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                     stime(1:2),":",stime(3:4),":",stime(5:6)
          print*, "           Dumping scalar components "
          print*, "======================================================"
          print*, "Name:               ", this%m_name
          print*, "Signal ID:          ", this%m_pid
          print*, "Number of samples:  ", this%m_nsamp
          print*, "Maximum 'k':        ", this%m_maxk
          print*, "Signal duration:    ", this%m_dur
          print*, "scalar amplitude:   ", this%m_A
          print*, "Initial time-point: ", this%m_sinit
          print*, "Time-step increment:", this%m_stsinc
          print*, "Carrier frequency:  ", this%m_freq
          print*, "           Dumping array components  "
          print*, "======================================================"
          print*, "Constructors:       ", this%m_ctors
          print*, "Signal polarization:", this%m_jvec
          if(verbose) then
              print*, "Sine series components:  ", this%m_scomp
              print*, "Cosine series components:", this%m_ccomp
          end if
          print*, "Time (t):             ",   this%m_wct
          print*, "Natural Envelope:     ",   this%m_nenvp
          print*, "Sawtooth time-domain: ",   this%m_sawtooth
          print*, "Complex form:         ",   this%m_cform
          print*, "Electric field:       ",   this%m_E
          print*, "Amplitude:            ",   this%m_amp
          print*, "Time-average power:   ",   this%m_S
    end subroutine
    
    
end module mod_sawtooth_signal