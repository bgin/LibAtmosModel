
module mod_expchirp_signal

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_expchirp_signal'
 !          
 !          Purpose:
 !                   Exponential Chirp Signal
 !                   Implementation of exponential chirp signal
 !                   in complex domain
 !                     
 !          History:
 !                        Date: 09-08-2017
 !                        Time: 11:36 GMT+2
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
    integer(I32P), parameter, public :: MOD_EXPCHIRP_SIGNAL_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_EXPCHIRP_SIGNAL_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_EXPCHIRP_SIGNAL_MICRO = 0
    
    ! Module/file full version
    integer(I32P), parameter, public :: MOD_EXPCHIRP_SIGNAL_FULLVER = 1000*MOD_EXPCHIRP_SIGNAL_MAJOR+100*MOD_EXPCHIRP_SIGNAL_MINOR + &
                                                                      10*MOD_EXPPCHIRP_SIGNAL_MICRO
    
    ! Module/file creation date
    character(*),  parameter, public :: MOD_EXPCHIRP_SIGNAL_CREATE_DATE = "09-08-2017 12:21 +00200 (WED 09 AUG 2017 GMT+2)"
    
    ! Module/file build date (should be set to latest  successful build date/time)
    character(*),  parameter, public :: MOD_EXPCHIRP_SIGNAL_BUILD_DATE = " "
    
    ! Module/file author info
    character(*),  parameter, public :: MOD_EXPCHIRP_SIGNAL_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_EXPCHIRP_SIGNAL_DESCRIPT = "Exponential Chirp complex domain implememtation."
    
    
    
    real(R64P),    parameter, private :: MINFREQ = 1.0e+7_R64P
    
   
    
   
    
   
    
    real(R64P),    parameter, private :: SMALLTS = 0.0000152587890625_R64P ! smallest time step = 1/2^-16
    
    !======================================================60
    !  Type: ExpChirpSignal_t
    !======================================================60
    
    type :: ExpChirpSignal_t
        
         private
         
         character(len=64)   :: m_name      ! Name of this signal
                                         
         integer(I32P)       :: m_sid       ! Signal id i.e. cardinal number in signal train
         
         integer(I32P)       :: m_nsamp     ! number of samples
         
         real(R64P)          :: m_dur       ! signal duration in ns
         
         real(R64P)          :: m_sfreq     ! Chirp start frequency (f0)
         
         real(R64P)          :: m_efreq     ! Chirp end frequency (f1)
         
         real(R64P)          :: m_stsinc    ! Chirp signal time step increment.
         
         real(R64P)          :: m_siphi     ! Chirp signal initial phase (radians)
         
         real(R64P)          :: m_chrate    ! Chirp rate i.e. [f1-f0/T]
         
         real(R64P)          :: m_sinit     ! Chirp signal starting time point
         
         character(len=32), dimension(5)   :: m_ctors     ! name of constructors which created this object
         
         type(JonesVector_t), allocatable, dimension(:) :: m_jvec      ! Jones Vector field(polarization)
         
       
         ! Electric field far from radar source
         complex(R64P), allocatable, dimension(:) :: m_E
!DIR$   ATTRIBUTES ALIGN : 32 :: m_E
        
        ! Signal enevelope i.e natural envelope  
         real(R64P), allocatable, dimension(:) :: m_nenvp
!DIR$   ATTRIBUTES ALIGN : 32 :: m_nenvp
        
        ! Signal phase (phi(t))
         real(R64P), allocatable, dimension(:) :: m_phi
!DIR$   ATTRIBUTES ALIGN : 32 :: m_phi
        
        ! Signal canonical form i.e. S(t) = gc(t)*cos(wct)-gs(t)*sin(wct)
         complex(R64P), allocatable, dimension(:) :: m_canform
!DIR$   ATTRIBUTES ALIGN : 32 :: m_canform
        
        ! Signal basic form i.e. S(t) = g(t)*cos[wct+phi(t)]
         real(R64P), allocatable, dimension(:) :: m_basform
!DIR$   ATTRIBUTES ALIGN : 32 :: m_basform        
        ! Signal complex envelope
         complex(R64P), allocatable, dimension(:) :: m_cenvp
!DIR$   ATTRIBUTES ALIGN : 32 :: m_cenvp
        
        ! Signal amplitude  A(phi,theta)/r used for calculation electric field
        ! far from emmiter(antenna)
         real(R64P), allocatable, dimension(:) :: m_amp
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
        
        ! Construct complex exponential chirp signal
        procedure, pass(this), public :: create_signal
        
         ! 2nd signal constructor,
        ! Creates signal with noise modulated phase
        procedure, pass(this), public :: phase_noise_signal
        
        ! 3rd signal constructor
        ! Creates signal with noise modulated phase
        ! coupled with background additive noise.
        procedure, pass(this), public :: additive_noise_signal
        
        ! Copy signal (deep copy constructor)
        procedure, pass(this), public :: copy_signal
        
        ! Destroy signal
        procedure, pass(this), public :: destroy_signal
        
        !==========================================52
        !   Getter pure functions
        !==========================================52
        
        procedure, pass(this), public :: get_name
        
        procedure, pass(this), public :: get_sid
        
        procedure, pass(this), public :: get_nsamp
        
        procedure, pass(this), public :: get_dur
        
        procedure, pass(this), public :: get_sfreq
        
        procedure, pass(this), public :: get_efreq
        
        procedure, pass(this), public :: get_stsinc
        
        procedure, pass(this), public :: get_siphi
        
        procedure, pass(this), public :: get_chrate
        
        procedure, pass(this), public :: get_sinit
        
        procedure, pass(this), public :: get_ctors
        
        procedure, pass(this), public :: get_jvec
        
        procedure, pass(this), public :: get_E
        
        procedure, pass(this), public :: get_nenvp
        
        procedure, pass(this), public :: get_canform
        
        procedure, pass(this), public :: get_basform
        
        procedure, pass(this), public :: get_cenvp
        
        procedure, pass(this), public :: get_amp
        
        procedure, pass(this), public :: get_S
        
        procedure, pass(this), public :: get_built_stat
        
        ! Computational subroutines
        
        procedure, pass(this), public :: dphi_dt
        
        procedure, pass(this), public :: analytic_signal
        
        !==============================================56
        ! write/read subroutines
        !==============================================56
        
        procedure, nopass, public :: to_screenu
        
        
    end type ExpChirpSignal_t   
        
    contains
    
    !======================================================60
    ! subroutine: default_signal
    !             default initialization (no physical meaning)
    !======================================================60
    subroutine default_signal(this,name,sid,nsamp,logging, &
                              filename,append,dbg  )
          implicit none
          class(ExpChirpSignal_t), intent(inout) :: this
          character(len=*),        intent(in)    :: name
          integer(I32P),           intent(in)    :: sid
          integer(I32P),           intent(inout) :: nsamp
          logical(I32P),           intent(in)    :: logging
          character(len=*),        intent(in)    :: filename
          logical(I32P),           intent(in)    :: append,dbg
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: i,aerr
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:258, In->mod_expchirp_signal/default_signal: ExpChirpSignal_t already initialized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( default_signal:261, ExpChirpSignal_t already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          if(nsamp.LE.MINSAMP) then
              nsamp = MINSAMP
          end if
          ! Begin construction
          this%m_name   = name
          this%m_sid    = sid
          this%m_nsamp  = nsamp
          this%m_dur    = 0._R64P
          this%m_sfreq  = 0._R64P
          this%m_efreq  = 0._R64P
          this%m_stsinc = 0._R64P
          this%m_siphi  = 0._R64P
          this%m_chrate = 0._R64P
          this%m_sinit  = 0._R64P
          this%m_ctors(1) = "default_signal"
          
          associate(n=>this%m_nsamp)
              allocate(this%m_jvec(n),    &
                       this%m_E(n),       &
                       this%m_nenvp(n),   &
                       this%m_phi(n),     &
                       this%m_canform(n), &
                       this%m_basform(n), &
                       this%m_cenvp(n),   &
                       this%m_amp(n),     &
                       this%m_S(n),       &
                       STAT=aerr,         &
                       ERRMSG=emsg )
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:306, In->mod_expchirp_signal/default_signal: Memory allocation failure!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_expchirp_signal/default_signal:239, Memory allocation failure!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_expchirp_signal/default_signal:239 -> [FATAL-ERROR]: Terminating execution"
          end if
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1, this%m_nsamp
              this%m_jvec(i) = JonesVector_t()
              this%m_E(i)    = DCMPLX(0._R64P,0._R64P)
              this%m_nenvp(i) = 0._R64P
              this%m_phi(i) = 0._R64P
              this%m_canform(i) = DCMPLX(0._R64P,0._R64P)
              this%m_basform(i) = 0._R64P
              this%m_cenvp(i)  = DCMPLX(0._R64P,0._R64P)
              this%m_amp(i) = 0._R64P
              this%m_S(i) = DCMPLX(0._R64P,0._R64P)
          end do
          this%m_isbuilt = .true.
          if(dbg) then
              print*, "Jones vec field", this%m_jvec
              print*, "E-field", this%m_E
              print*, "Natural-envelope", this%m_nenvp
              print*, "Phase part", this%m_phi
              print*, "Canonical form", this%m_canform
              print*, "Basic form", this%m_basform
              print*, "Cmplx envelope", this%m_cenvp
              print*, "Amplitude", this%m_amp
              print*, "PSD" , this%m_S
          end if
    end subroutine
    
    !======================================================60
    !  subroutine: create_signal                        
    !              Physical representation of complex expo-
    !              nential chirp signal            
    !======================================================60     
    subroutine create_signal(this,name,sid,nsamp,dur,sfreq,efreq,   &
                             stsinc,siphi,chrate,sinit,r,h,v,logging, &
                             filename,append,dbg,profiling,qpctimer )
          implicit none
          class(ExpChirpSignal_t),     intent(inout) :: this
          character(len=*),            intent(in)    :: name
          integer(I32P),               intent(in)    :: sid
          integer(I32P),               intent(inout) :: nsamp
          real(R64P),                  intent(inout) :: dur
          real(R64P),                  intent(inout) :: sfreq,efreq
          real(R64P),                  intent(in)    :: stsinc,siphi
          real(R64P),                  intent(in)    :: chrate,sinit
          real(R64P), dimension(:),    intent(in)    :: r    ! range
!DIR$     ASSUME_ALIGNED r:32          
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:32,v:32
          logical(I32P),               intent(in)    :: logging
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P) :: i,aerr
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P) :: insamp,t,t2,delta,lnK,ilnK,tc,ts
          complex(R64P), allocatable, dimension(:) :: tmp
!DIR$     ATTRIBUTES ALIGN : 32 :: tmp
          real(R64P), allocatable, dimension(:) :: ct ! copy of running 't' variable.
!DIR$     ATTRIBUTES ALIGN : 32 :: ct
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:400, In->mod_expchirp_signal/create_signal: ExpChirpSignal_t already initialized!!"
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_expchirp_signal/create_signal:400, ExpChirpSignal_t already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          if(nsamp.LE.LAM_MINSAMP) then
              nsamp = LAM_MINSAMP
          end if
          if(efreq.LE.sfreq) then
               if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:419, In->mod_expchirp_signal/create_signal: f1 <= f0 !!"
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_expchirp_signal/create_signal:419, f1 <= f0 !)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          if(dur.LE.0._R64P) then
              dur =  LAM_NS
          end if
          ! Begin construction
          this%m_name     = name
          this%m_sid      = sid
          this%m_nsamp    = nsamp
          this%m_dur      = dur
          this%m_sfreq    = sfreq
          this%m_efreq    = efreq
          this%m_stsinc   = stsinc
          this%m_siphi    = siphi
          this%m_chrate   = chrate
          this%m_sinit    = sinit
          this%m_ctors(2) = "create_signal"
          
          associate(n=>this%m_nsamp)
              allocate(this%m_jvec(n),    &
                       this%m_E(n),       &
                       this%m_nenvp(n),   &
                       this%m_phi(n),     &
                       this%m_canform(n), &
                       this%m_basform(n), &
                       this%m_cenvp(n),   &
                       this%m_amp(n),     &
                       this%m_S(n),       &
                       tmp(n),            &
                       ct(n),             &
                       STAT=aerr,         &
                       ERRMSG=emsg )
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:445, In->mod_expchirp_signal/create_signal: Memory allocation failure!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_expchirp_signal/create_signal:445, Memory allocation failure!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_expchirp_signal/create_signal:445 -> [FATAL-ERROR]: Terminating execution"
          end if
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1, this%m_nsamp
              this%m_jvec(i) = JonesVector_t(h(i),v(i))
              this%m_E(i) = DCMPLX(0._R64P,0._R64P)
              this%m_nenvp(i) = 0._R64P
              this%m_phi(i) = 0._R64P
              this%m_canform(i) = DCMPLX(0._R64P,0._R64P)
              this%m_basform(i) = 0._R64P
              this%m_cenvp(i)  = DCMPLX(0._R64P,0._R64P)
              this%m_amp(i) = 0._R64P
              this%m_S(i) = DCMPLX(0._R64P,0._R64P)
              tmp(i) = DCMPLX(0._R64P,0._R64P)
              ct(i)  = 0._R64P
          end do
          t = 0._R64P
          t2 = 0._R64P
          delta = 0._R64P
          insamp = 1._R64P/DBLE(this%m_nsamp)
          lnK = DLOG(this%m_chrate)
          ilnK = 1._R64P/lnK
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_expchirp_signal/create_signal: qpctimer_start failed to query performance frequency counter!!"
              end if
          end if
          ! Construct Electric field
          do i = 1, this%m_nsamp
               this%m_sinit = this%m_sinit+this%m_stsinc
               delta = DBLE(i)*insamp
               t = this%m_sinit*delta
               t2 = t-r(i)/LAM_c
               this%m_phi(i) = this%m_siphi+(LAM_2PI*this%m_sfreq)*(this%m_chrate**t2-1)*ilnK
               this%m_E(i) = DCMPLX(DCOS(this%m_phi(i)),DSIN(this%m_phi(i)))
          end do
           ! Compute A(phi,theta)/r - magnitude of electric field E
          call vcmag(this%m_E,this%m_amp)
          ! Create signal complex components
          do i = 1, this%m_nsamp
              this%m_sinit = this%m_sinit+this%m_stsinc
              delta = DBLE(i)*insamp
              t = this%m_sinit*delta
              ct(i) = t ! use later in canonical form 
              this%m_phi(i) = this%m_siphi+(LAM_2PI*this%m_sfreq)*(this%m_chrate**t-1)*ilnK
              tmp(i) = DCMPLX(DCOS(this%m_phi(i)),DSIN(this%m_phi(i)))
          end do
          ! Compute natural envelope
          call vcmag(tmp,this%m_nenvp)
          tc = 0._R64P
          ts = 0._R64P
          ! Create canonical representation
          do i = 1, this%m_nsamp
              tc = this%m_nenvp(i)*DCOS(this%m_phi(i))
              ts = this%m_nenvp(i)*DSIN(this%m_phi(i))
              this%m_cenvp(i) = DCMPLX(tc,ts)
              this%m_canform(i) = DCMPLX(tc*DCOS(ct(i)),ts*DSIN(ct(i)))
              this%m_basform(i) = DREAL(this%m_canform(i))
          end do
          ! Compute time-average power density i.e. 0.5xExE*/n
          do i = 1, this%m_nsamp
              this%m_S(i) = 0.5_R64P*this%m_E(i)*DCONJG(this%m_E(i))*0.0026525198938992_R64P
          end do                   
          this%m_isbuilt = .true.
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(ERROR_UNIT,*) "mod_expchirp_signal/create_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(ERROR_UNIT,*) "mod_expchirp_signal/create_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
     end subroutine
                             
    !======================================================60
    !  subroutine: phase_noise_signal                        
    !              Physical representation of Exponential Chirp
    !              signal.
    !              This signal is corrupted by phase additive
    !              noise.       
    !======================================================60 
    subroutine phase_noise_signal(this,name,sid,nsamp,dur,sfreq,efreq,   &
                                 stsinc,siphi,chrate,sinit,phnoise,r,h,v, &
                                 logging,filename,append,dbg,profiling,qpctimer )
          implicit none
          class(ExpChirpSignal_t),     intent(inout) :: this
          character(len=*),            intent(in)    :: name
          integer(I32P),               intent(in)    :: sid
          integer(I32P),               intent(inout) :: nsamp
          real(R64P),                  intent(inout) :: dur
          real(R64P),                  intent(inout) :: sfreq,efreq
          real(R64P),                  intent(in)    :: stsinc,siphi
          real(R64P),                  intent(in)    :: chrate,sinit
          real(R64P), dimension(:),    intent(in)    :: phnoise,r    ! range
!DIR$     ASSUME_ALIGNED phnoise:32,r:32          
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:32,v:32
          logical(I32P),               intent(in)    :: logging
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P) :: i,aerr
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P) :: insamp,t,t2,delta,lnK,ilnK,tc,ts,tn
          complex(R64P), allocatable, dimension(:) :: tmp
!DIR$     ATTRIBUTES ALIGN : 32 :: tmp
          real(R64P), allocatable, dimension(:) :: ct ! copy of running 't' variable.
!DIR$     ATTRIBUTES ALIGN : 32 :: ct
          ! Start of executable statements
           if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:603, In->mod_expchirp_signal/phase_noise_signal: ExpChirpSignal_t already initialized!!"
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_expchirp_signal/phase_noise_signal:603, ExpChirpSignal_t already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          if(nsamp.LE.LAM_MINSAMP) then
              nsamp = LAM_MINSAMP
          end if
          if(efreq.LE.sfreq) then
               if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:622, In->mod_expchirp_signal/phase_noise_signal: f1 <= f0 !!"
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_expchirp_signal/phase_noise_signal:622, f1 <= f0 !)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          if(dur.LE.0._R64P) then
              dur =  LAM_NS
          end if
          ! Begin construction
          this%m_name     = name
          this%m_sid      = sid
          this%m_nsamp    = nsamp
          this%m_dur      = dur
          this%m_sfreq    = sfreq
          this%m_efreq    = efreq
          this%m_stsinc   = stsinc
          this%m_siphi    = siphi
          this%m_chrate   = chrate
          this%m_sinit    = sinit
          this%m_ctors(3) = "phase_noise_signal"
           associate(n=>this%m_nsamp)
              allocate(this%m_jvec(n),    &
                       this%m_E(n),       &
                       this%m_nenvp(n),   &
                       this%m_phi(n),     &
                       this%m_canform(n), &
                       this%m_basform(n), &
                       this%m_cenvp(n),   &
                       this%m_amp(n),     &
                       this%m_S(n),       &
                       tmp(n),            &
                       ct(n),             &
                       STAT=aerr,         &
                       ERRMSG=emsg )
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:668, In->mod_expchirp_signal/phase_noise_signal: Memory allocation failure!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_expchirp_signal/phase_noise_signal:668, Memory allocation failure!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_expchirp_signal/phase_noise_signal:668 -> [FATAL-ERROR]: Terminating execution"
          end if
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1, this%m_nsamp
              this%m_jvec(i) = JonesVector_t(h(i),v(i))
              this%m_E(i) = DCMPLX(0._R64P,0._R64P)
              this%m_nenvp(i) = 0._R64P
              this%m_phi(i) = 0._R64P
              this%m_canform(i) = DCMPLX(0._R64P,0._R64P)
              this%m_basform(i) = 0._R64P
              this%m_cenvp(i)  = DCMPLX(0._R64P,0._R64P)
              this%m_amp(i) = 0._R64P
              this%m_S(i) = DCMPLX(0._R64P,0._R64P)
              tmp(i) = DCMPLX(0._R64P,0._R64P)
              ct(i)  = 0._R64P
          end do
          t = 0._R64P
          t2 = 0._R64P
          delta = 0._R64P
          insamp = 1._R64P/DBLE(this%m_nsamp)
          lnK = DLOG(this%m_chrate)
          ilnK = 1._R64P/lnK
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_expchirp_signal/phase_noise_signal: qpctimer_start failed to query performance frequency counter!!"
              end if
          end if
          ! Construct Electric field
          do i = 1, this%m_nsamp
               this%m_sinit = this%m_sinit+this%m_stsinc
               delta = DBLE(i)*insamp
               t = this%m_sinit*delta
               t2 = t-r(i)/LAM_c
               tn = t2+phnoise(i)
               this%m_phi(i) = this%m_siphi+(LAM_2PI*this%m_sfreq)*(this%m_chrate**tn-1)*ilnK
               this%m_E(i) = DCMPLX(DCOS(this%m_phi(i)),DSIN(this%m_phi(i)))
          end do
           ! Compute A(phi,theta)/r - magnitude of electric field E
          call vcmag(this%m_E,this%m_amp)
          ! Create signal complex components
          do i = 1, this%m_nsamp
              this%m_sinit = this%m_sinit+this%m_stsinc
              delta = DBLE(i)*insamp
              t = this%m_sinit*delta
              tn = t+phnoise(i)
              ct(i) = tn ! use later in canonical form 
              this%m_phi(i) = this%m_siphi+(LAM_2PI*this%m_sfreq)*(this%m_chrate**tn-1)*ilnK
              tmp(i) = DCMPLX(DCOS(this%m_phi(i)),DSIN(this%m_phi(i)))
          end do
          ! Compute natural envelope
          call vcmag(tmp,this%m_nenvp)
          tc = 0._R64P
          ts = 0._R64P
          ! Create canonical representation
          do i = 1, this%m_nsamp
              tc = this%m_nenvp(i)*DCOS(this%m_phi(i))
              ts = this%m_nenvp(i)*DSIN(this%m_phi(i))
              this%m_cenvp(i) = DCMPLX(tc,ts)
              this%m_canform(i) = DCMPLX(tc*DCOS(ct(i)),ts*DSIN(ct(i)))
              this%m_basform(i) = DREAL(this%m_canform(i))
          end do
          ! Compute time-average power density i.e. 0.5xExE*/n
          do i = 1, this%m_nsamp
              this%m_S(i) = 0.5_R64P*this%m_E(i)*DCONJG(this%m_E(i))*0.0026525198938992_R64P
          end do                   
          this%m_isbuilt = .true.
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(ERROR_UNIT,*) "mod_expchirp_signal/phase_noise_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(ERROR_UNIT,*) "mod_expchirp_signal/phase_noise_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
     end subroutine
                                 
    !======================================================60
    !  subroutine: additive_noise_signal                        
    !              Physical representation of Exponential Chirp
    !              signal.
    !              This signal is corrupted by phase additive
    !              noise and by background additive noise.      
    !======================================================60 
    subroutine additive_noise_signal(this,name,sid,nsamp,dur,sfreq,efreq,   &
                                 stsinc,siphi,chrate,sinit,phnoise,bnoise,r,h,v, &
                                 logging,filename,append,dbg,profiling,qpctim )
          implicit none
          class(ExpChirpSignal_t),     intent(inout) :: this
          character(len=*),            intent(in)    :: name
          integer(I32P),               intent(in)    :: sid
          integer(I32P),               intent(inout) :: nsamp
          real(R64P),                  intent(inout) :: dur
          real(R64P),                  intent(inout) :: sfreq,efreq
          real(R64P),                  intent(in)    :: stsinc,siphi
          real(R64P),                  intent(in)    :: chrate,sinit
          real(R64P), dimension(:),    intent(in)    :: phnoise,bnoise,r    ! range
!DIR$     ASSUME_ALIGNED phnoise:32,bnoise:32,r:32          
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:32,v:32
          logical(I32P),               intent(in)    :: logging
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P) :: i,aerr
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P) :: insamp,t,t2,delta,lnK,ilnK,tc,ts,tn
          complex(R64P), allocatable, dimension(:) :: tmp
!DIR$     ATTRIBUTES ALIGN : 32 :: tmp
          real(R64P), allocatable, dimension(:) :: ct ! copy of running 't' variable.
!DIR$     ATTRIBUTES ALIGN : 32 :: ct
          ! Start of executable statements
            if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:807, In->mod_expchirp_signal/additive_noise_signal: ExpChirpSignal_t already initialized!!"
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_expchirp_signal/additive_noise_signal:807, ExpChirpSignal_t already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          if(nsamp.LE.LAM_MINSAMP) then
              nsamp = LAM_MINSAMP
          end if
          if(efreq.LE.sfreq) then
               if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:826, In->mod_expchirp_signal/additive_noise_signal: f1 <= f0 !!"
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_expchirp_signal/additive_noise_signal:826, f1 <= f0 !)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          if(dur.LE.0._R64P) then
              dur =  LAM_NS
          end if
          ! Begin construction
          this%m_name     = name
          this%m_sid      = sid
          this%m_nsamp    = nsamp
          this%m_dur      = dur
          this%m_sfreq    = sfreq
          this%m_efreq    = efreq
          this%m_stsinc   = stsinc
          this%m_siphi    = siphi
          this%m_chrate   = chrate
          this%m_sinit    = sinit
          this%m_ctors(4) = "additive_noise_signal"
           associate(n=>this%m_nsamp)
              allocate(this%m_jvec(n),    &
                       this%m_E(n),       &
                       this%m_nenvp(n),   &
                       this%m_phi(n),     &
                       this%m_canform(n), &
                       this%m_basform(n), &
                       this%m_cenvp(n),   &
                       this%m_amp(n),     &
                       this%m_S(n),       &
                       tmp(n),            &
                       ct(n),             &
                       STAT=aerr,         &
                       ERRMSG=emsg )
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:873, In->mod_expchirp_signal/additive_noise_signal: Memory allocation failure!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_expchirp_signal/additive_noise_signal:873, Memory allocation failure!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_expchirp_signal/additve_noise_signal:873 -> [FATAL-ERROR]: Terminating execution"
          end if
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
            do i = 1, this%m_nsamp
              this%m_jvec(i) = JonesVector_t(h(i),v(i))
              this%m_E(i) = DCMPLX(0._R64P,0._R64P)
              this%m_nenvp(i) = 0._R64P
              this%m_phi(i) = 0._R64P
              this%m_canform(i) = DCMPLX(0._R64P,0._R64P)
              this%m_basform(i) = 0._R64P
              this%m_cenvp(i)  = DCMPLX(0._R64P,0._R64P)
              this%m_amp(i) = 0._R64P
              this%m_S(i) = DCMPLX(0._R64P,0._R64P)
              tmp(i) = DCMPLX(0._R64P,0._R64P)
              ct(i)  = 0._R64P
          end do
          t = 0._R64P
          t2 = 0._R64P
          delta = 0._R64P
          insamp = 1._R64P/DBLE(this%m_nsamp)
          lnK = DLOG(this%m_chrate)
          ilnK = 1._R64P/lnK
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_expchirp_signal/additive_noise_signal: qpctimer_start failed to query performance frequency counter!!"
              end if
          end if
          ! Construct Electric field
          do i = 1, this%m_nsamp
               this%m_sinit = this%m_sinit+this%m_stsinc
               delta = DBLE(i)*insamp
               t = this%m_sinit*delta
               t2 = t-r(i)/LAM_c
               tn = t2+phnoise(i)
               this%m_phi(i) = this%m_siphi+(LAM_2PI*this%m_sfreq)*(this%m_chrate**tn-1)*ilnK
               this%m_E(i) = DCMPLX(bnoise(i)+DCOS(this%m_phi(i)),bnoise(i)+DSIN(this%m_phi(i)))
          end do
           ! Compute A(phi,theta)/r - magnitude of electric field E
          call vcmag(this%m_E,this%m_amp)
          ! Create signal complex components
          do i = 1, this%m_nsamp
              this%m_sinit = this%m_sinit+this%m_stsinc
              delta = DBLE(i)*insamp
              t = this%m_sinit*delta
              tn = t+phnoise(i)
              ct(i) = tn ! use later in canonical form 
              this%m_phi(i) = this%m_siphi+(LAM_2PI*this%m_sfreq)*(this%m_chrate**tn-1)*ilnK
              tmp(i) = DCMPLX(bnoise(i)+DCOS(this%m_phi(i)),bnoise(i)+DSIN(this%m_phi(i)))
          end do
          ! Compute natural envelope
          call vcmag(tmp,this%m_nenvp)
          tc = 0._R64P
          ts = 0._R64P
          ! Create canonical representation
          do i = 1, this%m_nsamp
              tc = this%m_nenvp(i)*DCOS(this%m_phi(i))
              ts = this%m_nenvp(i)*DSIN(this%m_phi(i))
              this%m_cenvp(i) = DCMPLX(tc,ts)
              this%m_canform(i) = DCMPLX(tc*DCOS(ct(i)),ts*DSIN(ct(i)))
              this%m_basform(i) = DREAL(this%m_canform(i))
          end do
          ! Compute time-average power density i.e. 0.5xExE*/n
          do i = 1, this%m_nsamp
              this%m_S(i) = 0.5_R64P*this%m_E(i)*DCONJG(this%m_E(i))*0.0026525198938992_R64P
          end do                   
          this%m_isbuilt = .true.
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(ERROR_UNIT,*) "mod_expchirp_signal/additive_noise_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(ERROR_UNIT,*) "mod_expchirp_signal/additive_noise_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
    end subroutine
    !======================================================60                         
    ! subroutine: copy_signal
    !======================================================60 
    subroutine copy_signal(this,other,logging,filename,append)
          implicit none
          class(ExpChirpSignal_t), intent(inout) :: this
          class(ExpChirpSignal_t), intent(in)    :: other
          logical(I32P),           intent(in)    :: logging
          character(len=*),        intent(in)    :: filename
          logical(I32P),           intent(in)    :: append
          ! Loclas
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          if(LOC(this).EQ.LOC(other) .OR. &
             this%m_isbuilt .EQ. .true. ) then
              if(logging) then
                   call log_startup(filename,append)
                   call log_UsrMsg("logger:558, In->mod_expchirp_signal/copy_signal: Attempted self-assignment, or argument in deleted state!"
                   call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_expchirp_signal/copy_signal:558, Attempted self_assignment, or argument in deleted state!)"
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
         this%m_dur      = other%m_dur
         this%m_sfreq    = other%m_sfreq
         this%m_efreq    = other%m_sfreq
         this%m_stsinc   = other%m_stsinc
         this%m_siphi    = other%m_siphi
         this%m_chrate   = other%m_chrate
         this%m_sinit    = other%m_sinit
         this%m_ctors(5) = "copy_signal"
         this%m_jvec     = other%m_jvec
         this%m_E        = other%m_E
         this%m_nenvp    = other%m_nenvp
         this%m_phi      = other%m_phi
         this%m_canform  = other%m_canform
         this%m_basform  = other%m_basform
         this%m_cenvp    = other%m_cenvp
         this%m_amp      = other%m_amp
         this%m_S        = other%m_S
         this%m_isbuilt  = .true.
    end subroutine
    
    !======================================================60
    ! subroutine: destroy signal
    !              Deallocates allocated arrays and sets to
    !              other members to default values.
    !
    !======================================================60
    subroutine destroy_signal(this,logging,filename,append)
          implicit none
          class(ExpChirpSignal_t), intent(inout) :: this
          logical(I32P),           intent(in)    :: logging
          character(len=*),        intent(in)    :: filename
          logical(I32P),           intent(in)    :: append
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: derr,i
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:615, In->mod_expchirp_signal/destroy_signal: ExpChirpSignal_t already destroyed!"
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_expchirp_signal/destroy_signal:615, ExpChirpSignal_t already destroyed!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          ! Nullify or set to default values scalar members
          do i = 1, this%m_nsamp
              call this%m_jvec(i)%destroy_jvec
          end do
          this%m_name   = " "
          this%m_sid    = -1
          this%m_nsamp  = 0
          this%m_dur    = 0._R64P
          this%m_sfreq  = 0._R64P
          this%m_efreq  = 0._R64P
          this%m_stsinc = 0._R64P
          this%m_siphi  = 0._R64P
          this%m_chrate = 0._R64P
          this%m_sinit  = 0._R64P
          this%m_ctors  = " "
         
          ! Array members deallocations one by one
          if(ALLOCATED(this%m_jvec)) then
              deallocate(this%m_jvec, &
                         STAT=derr, &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:645, In->mod_expchirp_signal/destroy_signal: Deallocation of [m_jvec]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                       write(stderr,*) "=========================FATAL======================="
                       write(stderr,*) "  ( mod_expchirp_signal/destroy_signal:645, Deallocation of [m_jvec]: failed)"
                       write(stderr,*) "   System: ", emsg
                       write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                       write(stderr,*) "=========================FATAL======================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP " mod_expchirp_signal/destroy_signal:645, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(allocated(this%m_E)) then
              deallocate(this%m_E,  &
                         STAT=derr, &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:645, In->mod_expchirp_signal/destroy_signal: Deallocation of [m_E]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                       write(stderr,*) "=========================FATAL======================="
                       write(stderr,*) "  ( mod_expchirp_signal/destroy_signal:645, Deallocation of [m_E]: failed)"
                       write(stderr,*) "   System: ", emsg
                       write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                       write(stderr,*) "=========================FATAL======================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP " mod_expchirp_signal/destroy_signal:645, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(allocated(this%m_nenvp)) then
              deallocate(this%m_nenvp, &
                         STAT=derr,    &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:670, In->mod_expchirp_signal/destroy_signal: Deallocation of [m_nenvp]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL======================="
                      write(stderr,*) "  ( mod_expchirp_signal/destroy_signal:670, Deallocation of [m_nenvp]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                      write(stderr,*) "=========================FATAL======================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_expchirp_signal/destroy_signal:670, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(allocated(this%m_phi)) then
              deallocate(this%m_phi, &
                         STAT=derr,  &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:695, In->mod_expchirp_signal/destroy_signal: Deallocation of [m_phi]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL======================="
                      write(stderr,*) "  ( mod_expchirp_signal/destroy_signal:695, Deallocation of [m_phi]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                      write(stderr,*) "=========================FATAL======================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_expchirp_signal/destroy_signal:695, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(allocated(this%m_canform)) then
              deallocate(this%m_canform, &
                         STAT=derr,      &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:720, In->mod_expchirp_signal/destroy_signal: Deallocation of [m_canform]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL======================="
                      write(stderr,*) "  ( mod_expchirp_signal/destroy_signal:720, Deallocation of [m_canform]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                      write(stderr,*) "=========================FATAL======================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_expchirp_signal/destroy_signal:720, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(allocated(this%m_basform)) then
              deallocate(this%m_basform, &
                          STAT=derr,     &
                          ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:745, In->mod_expchirp_signal/destroy_signal: Deallocation of [m_basform]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL======================="
                      write(stderr,*) "  ( mod_expchirp_signal/destroy_signal:745, Deallocation of [m_basform]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                      write(stderr,*) "=========================FATAL======================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_expchirp_signal/destroy_signal:745, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(allocated(this%m_cenvp)) then
              deallocate(this%m_cenvp, &
                         STAT=derr,    &
                         ERRMSG=emsg  )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:770, In->mod_expchirp_signal/destroy_signal: Deallocation of [m_cenvp]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL======================="
                      write(stderr,*) "  ( mod_expchirp_signal/destroy_signal:770, Deallocation of [m_cenvp]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                      write(stderr,*) "=========================FATAL======================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_expchirp_signal/destroy_signal:770, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(allocated(this%m_amp)) then
              deallocate(this%m_amp, &
                         STAT=derr,  &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:795, In->mod_expchirp_signal/destroy_signal: Deallocation of [m_amp]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL======================="
                      write(stderr,*) "  ( mod_expchirp_signal/destroy_signal:795, Deallocation of [m_amp]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                      write(stderr,*) "=========================FATAL======================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_expchirp_signal/destroy_signal:795, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(allocated(this%m_S)) then
              deallocate(this%m_S,  &
                         STAT=derr, &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:820, In->mod_expchirp_signal/destroy_signal: Deallocation of [m_S]: failed")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL======================="
                      write(stderr,*) "  ( mod_expchirp_signal/destroy_signal:820, Deallocation of [m_S]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                      write(stderr,*) "=========================FATAL======================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_expchirp_signal/destroy_signal:820, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          this%m_isbuilt = .false.
    end subroutine
    
    !======================================================60
    ! Getter pure functions
    !======================================================60
                             
    pure function get_name(this) result(name)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          character(len=64) :: name
          ! Start of executable statements
          name = this%m_name
    end function      
    
    pure function get_sid(this) result(sid)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: sid
          ! Start of executable statements
          sid = this%m_sid
    end function
    
    pure function get_nsamp(this) result(nsamp)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nsamp
          ! Start of executable statements
          nsamp = this%m_nsamp
    end function
    
    pure function get_dur(this) result(dur)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: dur
          ! Start of executable statements
          dur = this%m_dur
    end function
    
    pure function get_sfreq(this) result(sfreq)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: sfreq
          ! Start of executable statements
          sfreq = this%m_sfreq
    end function
    
    pure function get_efreq(this) result(efreq)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Loclas
          real(R64P) :: efreq
          ! Start of executable statements
          efreq = this%m_efreq
    end function
    
    pure function get_stsinc(this) result(stsinc)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: stsinc
          ! Start of executable sttements
          stsinc = this%m_stsinc
    end function
    
    pure function get_siphi(this) result(siphi)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: siphi
          ! Start pf executable statements
          siphi = this%m_siphi
    end function
    
    pure function get_chrate(this) result(chrate)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Loclas
          real(R64P) :: chrate
          ! Start of executable statements
          chrate = this%m_chrate
    end function
    
    pure function get_sinit(this) result(sinit)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Loclas
          real(R64P) :: sinit
          ! Start of executable statements
          sinit = this%m_sinit
    end function
    
    pure function get_ctors(this) result(ctors)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Loclas
          character(len=32), dimension(3) :: ctors
          ! Start of executable statements
          ctors = this%m_ctors
    end function
    
    pure function get_jvec(this) result(jvec)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          type(JonesVector_t), allocatable, dimension(:) :: jvec
          ! Start of executable statements
          jvec = this%m_jvec
    end function
    
    pure function get_E(this) result(E)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: E
!DIR$     ATTRIBUTES ALIGN : 32 :: E
          ! Start of executable statements
          E = this%m_E
    end function
    
    pure function get_nenvp(this) result(nenvp)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Loclas
          real(R64P), allocatable, dimension(:) :: nenvp
 !DIR$    ATTRIBUTES ALIGN : 32 :: nenvp
          ! Start of executable statements
          nenvp = this%m_nenvp
    end function
    
    pure function get_phi(this) result(phi)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: phi
!DIR$     ATTRIBUTES ALIGN : 32 :: phi
          ! Start of executable statements
          phi = this%m_phi
    end function
    
    pure function get_canform(this) result(canform)   
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: canform
!DIR$     ATTRIBUTES ALIGN : 32 :: canform
          ! Start of executable statements
          canform = this%m_canform
    end function
    
    pure function get_basform(this) result(basform)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: basform
!DIR$     ATTRIBUTES ALIGN : 32 :: basform
          ! Start of executable statements
          basform = this%m_basform
    end function
    
    pure function get_cenvp(this) result(cenvp)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: cenvp
!DIR$     ATTRIBUTES ALIGN : 32 :: cenvp
          ! Strat of executable statements
          cenvp = this%m_cenvp
    end function
    
     pure function get_amp(this) result(amp)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: amp
!DIR$     ATTRIBUTES ALIGN : 32 :: amp
          ! Start of executable statements
          amp = this%m_amp
    end function
    
    pure function get_S(this) result(S)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
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
          class(ExpChirpSignal_t),  intent(in)     :: this
          real(R64P), dimension(:), intent(out)    :: dphi
!DIR$     ASSUME_ALIGNED dphi:32
          integer(I64P),            intent(inout)  :: sfac   ! scaling factor up to 16 digits of precision
          ! Locals
          integer(I32P) :: i
          real(R64P)    :: eps,tmp,isfac
          ! Start of executable statements
          if(sfac.LE.0) then
              sfac =  1000000000000000_I64P
          end if
          isfac = 1._R64P/DBLE(sfac)
          eps = MACHEPSF64**0.333333333333333333333333_R64P 
          do i = 2, this%m_nsamp-1
              tmp = this%m_phi(i+1)-this%m_phi(i-1)
              dphi(i) = this%m_phi(i+1)-this%m_phi(i-1)/ &
                        (2._R64P*eps*DMAX1(DABS(tmp),isfac)*DSIGN(tmp,tmp))
          end do
    end subroutine
    
    !======================================================60
    ! subroutine: analytic signal
    !======================================================60
    subroutine analytic_signal(this,asig,tmplen,wlen,iplen, &
                                profiling,qpctimer )
          use mod_fftsg, only : rdft
          implicit none
          class(ExpChirpSignal_t),     intent(inout) :: this
          complex(R64P), dimension(:), intent(out)   :: asig
!DIR$     ASSUME_ALIGNED asig:32
          integer(I32P),               intent(in)    :: tmplen   ! nsamp-1
          integer(I32P),               intent(in)    :: wlen     ! nsamp/2-1
          integer(I32P),               intent(in)    :: iplen    ! 2+2**(int(log(n/2+0.5)/log(2.0))/2)
          logical(I32P),               intent(in)    :: profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          integer(I32P) :: i
          real(R64P), dimension(0:tmplen)   :: tmp,tmp2
          real(R64P), dimension(0:wlen)     :: w
          integer(I32P), dimension(0:iplen) :: ip
          ! Start of executable statements
          
          if(profiling) then
             call qpctimer_start(qpctimer,ifail)
             if(ifail.EQ.0) then
                 write(stderr,*) "mod_expchirp_signal/analytic_signal:996, qpctimer_start failed to query performance frequency counter!!"
             end if
          end if
          ! Copy real part of canonical representation
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 0, tmplen
              tmp(i) = DREAL(this%m_canform(i))
          end do
          ! Real FFT
          ip(0) = 0
          call rdft(tmplen,1,tmp,ip,w)
          do i = 0, tmplen
              tmp2(i) = tmp(i)+DSIGN(tmp(i),tmp(i)))*tmp(i)  ! Construct argument to Inverse FFT
          end do
          ! Inverse real FFT
          call rdft(tmplen,-1,tmp2,ip,w)
          ! Construct analytic signal content
          do i = 0, tmplen
              asig(i) = DCMPLX(DREAL(this%m_canform(i)),tmp2(i))
          end do
          if(profiling) then
               if(ifail.EQ.0) then
                   call qpctimer_stop(qpctimer,ifail)
                   call qpctimer_delta(qpctimer,bfail)
                   if(bfail .EQ. .false.) then
                        call qpctimer_print(qpctimer)
                   else
                        write(stderr,*) "mod_expchirp_signal/analytic_signal:1020, qpctimer_delta: failed to compute delta measurement!!"
                   end if  
               else
                       write(stderr,*) "module_expchirp_signal/analytic_signal:1020 Unable to read performance counter -- fatal!!"
               end if
          end if
    end subroutine
    
    !======================================================60
    !  subroutine: to_screenu
    !======================================================60  
    subroutine to_screenu(this)
          implicit none
          class(ExpChirpSignal_t), intent(in) :: this
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          call DATE_AND_TIME(date=dstr,time=tstr)
          print*, "======================================================"
          print*, "   ***Printing components of ExpChirpSignal_t***"
          print*, "   printing subroutine called at: " , &
                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                     stime(1:2),":",stime(3:4),":",stime(5:6)
          print*, "           Dumping scalar components "
          print*, "======================================================"
          print*, "Name:               ", this%m_name
          print*, "Signal ID:          ", this%m_pid
          print*, "Number of samples:  ", this%m_nsamp
          print*, "Signal duration:    ", this%m_dur
          print*, "Frequency (f0):     ", this%m_sfreq
          print*, "Frequency (f1):     ", this%m_efreq
          print*, "Time step:          ", this%m_stsinc
          print*, "Initial phase:      ", this%m_siphi
          print*, "Chirp rate(f1-f0/T),", this%m_chrate
          print*, "Start time-point:   ", this%m_sinit
          print*, "           Dumping array components  "
          print*, "===================================================="
          print*, "Constructors:       ", this%m_ctors
          print*, "Signal polarization:", this%m_jvec
          print*, "Electric field:     ", this%m_E
          print*, "Natural Envelope:   ", this%m_nenvp
          print*, "Phase:              ", this%m_phi
          print*, "Canonical form:     ", this%m_canform
          print*, "Base form:          ", this%m_basform
          print*, "Complex envelope:   ", this%m_cenvp
          print*, "Amplitude:          ", this%m_amp
          print*, "Time-average power: ", this%m_S
          print*,"======================================================"
          print*, "Object built stat :", this%m_isbuilt
          print*, "           End                     "
    end subroutine
                                
                                
                                
                                
end module mod_expchirp_signal