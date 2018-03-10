
#include "Config.fpp"

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
 !            Modified:
 !                      By Bernard Gingold on: 10-03-2018, 11:19 GMT+2
 !                   1) Refactoring to Fortran 2003 standard.
 !                   2) Removing unnecessary type-bound members
 !                   3) Adding computational subroutines (additive noise)
 !                   4) Removing unnecessary destructor,copy and initialization subroutines.
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

   
   
    use module_kinds ,  only : I32P, R64P
    use mod_code_timing
    use mod_complex_arithm , only : vcmag
    use mod_jonesvec
    implicit none
    private
   
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
    
    type, public :: ExpChirpSignal_t
        
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
         
        
         
         type(JonesVector_t), allocatable, dimension(:),  public :: m_jvec      ! Jones Vector field(polarization)
         
       
         ! Electric field far from radar source
         complex(R64P), allocatable, dimension(:),        public :: m_E
!DIR$   ATTRIBUTES ALIGN : 64 :: m_E
        
        ! Signal enevelope i.e natural envelope  
         real(R64P), allocatable, dimension(:),           public :: m_nenvp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_nenvp
        
        ! Signal phase (phi(t))
         real(R64P), allocatable, dimension(:),           public :: m_phi
!DIR$   ATTRIBUTES ALIGN : 64 :: m_phi
        
        ! Signal canonical form i.e. S(t) = gc(t)*cos(wct)-gs(t)*sin(wct)
         complex(R64P), allocatable, dimension(:),        public  :: m_canform
!DIR$   ATTRIBUTES ALIGN : 64 :: m_canform
        
        ! Signal basic form i.e. S(t) = g(t)*cos[wct+phi(t)]
         real(R64P), allocatable, dimension(:),           public  :: m_basform
!DIR$   ATTRIBUTES ALIGN : 64 :: m_basform        
        ! Signal complex envelope
         complex(R64P), allocatable, dimension(:),        public  :: m_cenvp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_cenvp
        
        ! Signal amplitude  A(phi,theta)/r used for calculation electric field
        ! far from emmiter(antenna)
         real(R64P), allocatable, dimension(:),           public   :: m_amp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_amp
        
        ! Time-averaged power density
      
         complex(R64P), allocatable, dimension(:),        public   :: m_S
!DIR$   ATTRIBUTES ALIGN : 64 :: m_S
        
       
        
        contains
    
        !========================================
        !  Constructor, destructor subroutines
        !========================================
    
        ! Default constructor creates zero waveform
        ! Can be used as zero interval in signal train
        !procedure, pass(this), public :: default_signal
        
        ! Construct complex exponential chirp signal
        procedure, pass(this), public :: expchirp_signal
        
        
        ! Creates signal with noise modulated phase
        procedure, pass(this), public :: phase_noise_expchirp_signal
        
      
        ! Creates signal with noise modulated phase
        ! coupled with background additive noise.
        procedure, pass(this), public :: additive_noise_expchirp_signal
        
        
        
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
        
        
        ! Computational subroutines
        
        procedure, pass(this), public :: dphi_dt
        
        procedure, pass(this), public :: analytic_signal
        
        !==============================================56
        ! write/read subroutines
        !==============================================56
        procedure, nopass, public :: read_state
        
        procedure, nopass, public :: write_state
        
        !==============================================56
        !  Class helper procedures
        !==============================================56
        procedure, nopass, public :: dbg_info
        
        !==============================================56
        !  Generic operators
        !==============================================56
        procedure, public :: copy_assign
        
        generic :: assignment (=) => copy_assign
        
    end type ExpChirpSignal_t  
        
    interface ExpChirpSignal_t
          procedure :: constructor
    end interface ExpChirpSignal_t
        
    contains
    
    !======================================================60
    ! @function:  constructor
    !             default initialization (no physical meaning)
    !======================================================60
    type(ExpChirpSignal_t) function constructor(name,sid,nsamp,logging, &
                                            verbose, filename,append  )
          use mod_print_error,  only : handle_fatal_memory_error
          use mod_constants,    only : LAM_MINSAMP, LAM_PINF
          character(len=*),        intent(in)    :: name
          integer(I32P),           intent(in)    :: sid
          integer(I32P),           intent(inout) :: nsamp
          logical(I32P),           intent(in)    :: verbose,logging
          character(len=*),        intent(in)    :: filename
          logical(I32P),           intent(in)    :: append
          ! Locals
         
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          ! Start of executable statements
          if(nsamp <= LAM_MINSAMP) then
              nsamp = LAM_MINSAMP
          end if
           ! Begin construction
          constructor%m_name   = name
          constructor%m_sid    = sid
          constructor%m_nsamp  = nsamp
          constructor%m_dur    = LAM_PINF
          constructor%m_sfreq  = LAM_PINF
          constructor%m_efreq  = LAM_PINF
          constructor%m_stsinc = LAM_PINF
          constructor%m_siphi  = LAM_PINF
          constructor%m_chrate = LAM_PINF
          constructor%m_sinit  = LAM_PINF
          associate(n=>constructor%m_nsamp)
              allocate(constructor%m_jvec(n),    &
                       constructor%m_E(n),       &
                       constructor%m_nenvp(n),   &
                       constructor%m_phi(n),     &
                       constructor%m_canform(n), &
                       constructor%m_basform(n), &
                       constructor%m_cenvp(n),   &
                       constructor%m_amp(n),     &
                       constructor%m_S(n),       &
                       STAT=aerr,         &
                       ERRMSG=emsg )
          end associate
         if(aerr /= 0) then
            
              call handle_fatal_memory_error(logging,verbose,append,filename,   &
                                             "logger:300 --> mod_expchirp_signal/constructor: Memory Allocation Failure!",  &
                                             "mod_expchirp_signal/constructor:300 -- Memory Allocation Failure !"  ,        &
                                             emsg,__LINE__ )
         end if  
         constructor%m_jvec    = JonesVector_t() 
         constructor%m_E       = CMPLX(LAM_PINF,LAM_PINF,R64P)
         constructor%m_nenvp   = LAM_PINF                                   
         constructor%m_phi     = LAM_PINF
         constructor%m_canform = CMPLX(LAM_PINF,LAM_PINF,R64P)
         constructor%m_basform = LAM_PINF
         constructor%cenvp     = CMPLX(LAM_PINF,LAM_PINF,R64P)
         constructor%m_amp     = LAM_PINF
         constructor%m_S       = CMPLX(LAM_PINF,LAM_PINF,R64P)
    end function constructor      
           
          
    
    
    !======================================================60
    !  @subroutine: expchirp_signal                        
    !              Physical representation of complex expo-
    !              nential chirp signal            
    !======================================================60     
    subroutine expchirp_signal(this,ierr,dur,sfreq,efreq,   &
                             stsinc,siphi,chrate,sinit,r,h,v,logging, &
                             verbose,filename,append,profiling,qpctimer )
          use mod_print_error,  only : handle_fatal_memory_error
          use mod_constants,    only : LAM_NS, LAM_2PI, LAM_c
          class(ExpChirpSignal_t),     intent(inout) :: this
          integer(I32P),               intent(inout) :: ierr
          real(R64P),                  intent(inout) :: dur
          real(R64P),                  intent(inout) :: sfreq,efreq
          real(R64P),                  intent(in)    :: stsinc,siphi
          real(R64P),                  intent(in)    :: chrate,sinit
          real(R64P), dimension(:),    intent(in)    :: r    ! range
!DIR$     ASSUME_ALIGNED r:64          
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:64,v:64
          logical(I32P),               intent(in)    :: logging,verbose
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          
          character(len=256) :: emsg
          integer(I32P) :: i,aerr
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P) :: insamp,t,t2,delta,lnK,ilnK,tc,ts
          complex(R64P), allocatable, dimension(:) :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          real(R64P), allocatable, dimension(:) :: ct ! copy of running 't' variable.
!DIR$     ATTRIBUTES ALIGN : 64 :: ct
          ! Start of executable statements
         
          if(ierr < 0_I32P) ierr = 0_I32P
          if(efreq == sfreq) then
              ierr = -1_I32P
              return
          end if
          if(dur <= 0._R64P) then
              dur =  LAM_NS
          end if
          ! Begin construction
          this%m_dur      = dur
          this%m_sfreq    = sfreq
          this%m_efreq    = efreq
          this%m_stsinc   = stsinc
          this%m_siphi    = siphi
          this%m_chrate   = chrate
          this%m_sinit    = sinit 
          associate(n=>this%m_nsamp)
              allocate( tmp(n),            &
                        ct(n),             &
                        STAT=aerr,         &
                        ERRMSG=emsg      )
                        
          end associate
          
          
         
          if(aerr /= 0) then
             
              call handle_fatal_memory_error(logging,verbose,append,filename,   &
                                             "logger:380 --> mod_expchirp_signal/expchirp_signal: Memory Allocation Failure!" ,  &
                                             "mod_expchirp_signal/expchirp_signal:380 -- Memory Allocation Failure" ,  &
                                             emsg,__LINE__  )
          end if
          
          t = 0._R64P
          t2 = 0._R64P
          delta = 0._R64P
          insamp = 1._R64P/DBLE(this%m_nsamp)
          lnK = DLOG(this%m_chrate)
          ilnK = 1._R64P/lnK
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail == 0) then
                  write(stderr,*) "mod_expchirp_signal/expchirp_signal: qpctimer_start failed to query performance frequency counter!!"
              end if
          end if
          ! Construct Electric field
          do i = 1_I32P, this%m_nsamp
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
          do i = 1_I32P, this%m_nsamp
              tc = this%m_nenvp(i)*DCOS(this%m_phi(i))
              ts = this%m_nenvp(i)*DSIN(this%m_phi(i))
              this%m_cenvp(i) = DCMPLX(tc,ts)
              this%m_canform(i) = DCMPLX(tc*DCOS(ct(i)),ts*DSIN(ct(i)))
              this%m_basform(i) = DREAL(this%m_canform(i))
          end do
          ! Compute time-average power density i.e. 0.5xExE*/n
          do i = 1_I32P, this%m_nsamp
              this%m_S(i) = 0.5_R64P*this%m_E(i)*DCONJG(this%m_E(i))*0.0026525198938992_R64P
          end do                   
          
          if(profiling) then
                if(ifail == 0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(ERROR_UNIT,*) "mod_expchirp_signal/expchirp_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(ERROR_UNIT,*) "mod_expchirp_signal/expchirp_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
     end subroutine expchirp_signal
                             
    !======================================================60
    !  @subroutine: phase_noise_expchirp_signal                        
    !              Physical representation of Exponential Chirp
    !              signal.
    !              This signal is corrupted by phase additive
    !              noise.       
    !======================================================60 
    subroutine phase_noise_expchirp_signal(this,ierr,dur,sfreq,efreq,   &
                                 stsinc,siphi,chrate,sinit,phnoise,r,h,v, &
                                 verbose,logging,filename,append,profiling,qpctimer )
          use mod_print_error,  only : handle_fatal_memory_error
          use mod_constants,    only : LAM_NS, LAM_c, LAM_2PI, LAM_PINF
          class(ExpChirpSignal_t),     intent(inout) :: this
          integer(I32P),               intent(inout) :: ierr
          real(R64P),                  intent(inout) :: dur
          real(R64P),                  intent(inout) :: sfreq,efreq
          real(R64P),                  intent(in)    :: stsinc,siphi
          real(R64P),                  intent(in)    :: chrate,sinit
          real(R64P), dimension(:),    intent(in)    :: phnoise,r    ! range
!DIR$     ASSUME_ALIGNED phnoise:64,r:64          
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:64,v:64
          logical(I32P),               intent(in)    :: verbose,logging
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          character(len=256) :: emsg
          integer(I32P) :: i,aerr
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P) :: insamp,t,t2,delta,lnK,ilnK,tc,ts,tn
          complex(R64P), allocatable, dimension(:) :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          real(R64P), allocatable, dimension(:) :: ct ! copy of running 't' variable.
!DIR$     ATTRIBUTES ALIGN : 64 :: ct
         
          ! Start of executable statements
          
          if(ierr < 0_I32P) ierr = 0_I32P
          if(efreq <= sfreq) then
              ierr = -1_I32P
              return
          end if
          if(dur.LE.0._R64P) then
              dur =  LAM_NS
          end if
          ! Begin construction
          this%m_dur      = dur
          this%m_sfreq    = sfreq
          this%m_efreq    = efreq
          this%m_stsinc   = stsinc
          this%m_siphi    = siphi
          this%m_chrate   = chrate
          this%m_sinit    = sinit
          associate(n=>this%m_nsamp)
              allocate( tmp(n),             &
                        ct(n),              &
                        STAT=aerr,          &
                        ERRMSG=emsg   )
                        
          end associate
          if(aerr /= 0) then
            
              call handle_fatal_memory_error(logging,verbose,append,filename,    &
                                             "logger:515 --> mod_expchirp_signal/phase_noise_expchirp_signal: Memory Allocation Failure"  , &
                                             "mod_expchirp_signal/phase_noise_expchirp_signal:515 -- Memory Allocation Failure!"   , &
                                             emsg,__LINE__ )
          end if
          tmp = CMPLX(LAM_PINF,LAM_PINF,R64P)
          ct  = LAM_PINF
          t = 0._R64P
          t2 = 0._R64P
          delta = 0._R64P
          insamp = 1._R64P/DBLE(this%m_nsamp)
          lnK = DLOG(this%m_chrate)
          ilnK = 1._R64P/lnK
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail == 0) then
                  write(stderr,*) "mod_expchirp_signal/phase_noise_expchirp_signal: qpctimer_start failed to query performance frequency counter!!"
              end if
          end if
           ! Construct Electric field
          do i = 1_I32P, this%m_nsamp
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
          do i = 1_I32P, this%m_nsamp
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
          do i = 1_I32P, this%m_nsamp
              tc = this%m_nenvp(i)*DCOS(this%m_phi(i))
              ts = this%m_nenvp(i)*DSIN(this%m_phi(i))
              this%m_cenvp(i) = DCMPLX(tc,ts)
              this%m_canform(i) = DCMPLX(tc*DCOS(ct(i)),ts*DSIN(ct(i)))
              this%m_basform(i) = DREAL(this%m_canform(i))
          end do
          ! Compute time-average power density i.e. 0.5xExE*/n
          do i = 1_I32P, this%m_nsamp
              this%m_S(i) = 0.5_R64P*this%m_E(i)*DCONJG(this%m_E(i))*0.0026525198938992_R64P
          end do                   
         
          if(profiling) then
                if(ifail == 0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail == .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(ERROR_UNIT,*) "mod_expchirp_signal/phase_noise_expchirp_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(ERROR_UNIT,*) "mod_expchirp_signal/phase_noise_expchirp_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
    end subroutine phase_noise_expchirp_signal     
         
        
    
                                 
    !======================================================60
    !  @subroutine: additive_noise_expchirp_signal                        
    !              Physical representation of Exponential Chirp
    !              signal.
    !              This signal is corrupted by phase additive
    !              noise and by background additive noise.      
    !======================================================60 
    subroutine additive_noise_expchirp_signal(this,ierr,dur,sfreq,efreq,   &
                                 stsinc,siphi,chrate,sinit,phnoise,bnoise,r,h,v, &
                                 verbose,logging,filename,append,profiling,qpctimer )
          use mod_print_error,  only : handle_fatal_memory_error
          use mod_constants,    only : LAM_NS, LAM_c, LAM_2PI, LAM_PINF
          class(ExpChirpSignal_t),     intent(inout) :: this
          integer(I32P),               intent(inout) :: ierr
          real(R64P),                  intent(inout) :: dur
          real(R64P),                  intent(inout) :: sfreq,efreq
          real(R64P),                  intent(in)    :: stsinc,siphi
          real(R64P),                  intent(in)    :: chrate,sinit
          real(R64P), dimension(:),    intent(in)    :: phnoise,bnoise,r    ! range
!DIR$     ASSUME_ALIGNED phnoise:64,bnoise:64,r:64          
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:64,v:64
          logical(I32P),               intent(in)    :: verbose,logging
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          
          character(len=256) :: emsg
          integer(I32P) :: i,aerr
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P) :: insamp,t,t2,delta,lnK,ilnK,tc,ts,tn
          complex(R64P), allocatable, dimension(:) :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
          real(R64P), allocatable, dimension(:) :: ct ! copy of running 't' variable.
!DIR$     ATTRIBUTES ALIGN : 64 :: ct
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(efreq <= sfreq) then
              ierr = -1_I32P
              return
          end if
          if(dur <= 0._R64P) then
              dur =  LAM_NS
          end if
          this%m_dur      = dur
          this%m_sfreq    = sfreq
          this%m_efreq    = efreq
          this%m_stsinc   = stsinc
          this%m_siphi    = siphi
          this%m_chrate   = chrate
          this%m_sinit    = sinit
          associate(n=>this%m_nsamp)
              allocate(  tmp(n),            &
                         ct(n),             &
                         STAT=aerr,         &
                         ERRMSG=emsg   )
                       
          end associate
          if(aerr /= 0) then
              
              call handle_fatal_memory_error(logging,verbose,append,filename,   &
                                             "logger:651 --> mod_expchirp_signal/additive_noise_expchirp_signal: Memory Allocation Failure", &
                                             "mod_expchirp_signal/additive_noise_expchirp_signal:651 -- Memory Allocation Failure!"  ,  &
                                             emsg,__LINE__ )
          end if
          tmp = CMPLX(LAM_PINF,LAM_PINF,R64P)
          ct  = LAM_PINF

          t = 0._R64P
          t2 = 0._R64P
          delta = 0._R64P
          insamp = 1._R64P/DBLE(this%m_nsamp)
          lnK = DLOG(this%m_chrate)
          ilnK = 1._R64P/lnK
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail == 0) then
                  write(stderr,*) "mod_expchirp_signal/additive_noise_expchirp_signal: qpctimer_start failed to query performance frequency counter!!"
              end if
          end if
          ! Construct Electric field
          do i = 1_I32P, this%m_nsamp
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
          do i = 1_I32P, this%m_nsamp
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
          do i = 1_I32P, this%m_nsamp
              tc = this%m_nenvp(i)*DCOS(this%m_phi(i))
              ts = this%m_nenvp(i)*DSIN(this%m_phi(i))
              this%m_cenvp(i) = DCMPLX(tc,ts)
              this%m_canform(i) = DCMPLX(tc*DCOS(ct(i)),ts*DSIN(ct(i)))
              this%m_basform(i) = DREAL(this%m_canform(i))
          end do
          ! Compute time-average power density i.e. 0.5xExE*/n
          do i = 1_I32P, this%m_nsamp
              this%m_S(i) = 0.5_R64P*this%m_E(i)*DCONJG(this%m_E(i))*0.0026525198938992_R64P
          end do                   
          this%m_isbuilt = .true.
          if(profiling) then
                if(ifail == 0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(ERROR_UNIT,*) "mod_expchirp_signal/additive_noise_expchirp_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(ERROR_UNIT,*) "mod_expchirp_signal/additive_noise_expchirt_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
    end subroutine  additive_noise_expchirp_signal
 
    

    
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
    
  
    !======================================================60
    !    Computational procedures
    !======================================================60
    ! TODO:
    !        ! Consider using CADNA to test for cancellation errors
    subroutine dphi_dt(this,dphi,sfac)
          use mod_constants, LAM_MEPS8
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
           
          class(ExpChirpSignal_t),     intent(inout) :: this
          complex(R64P), dimension(:), intent(out)   :: asig
!DIR$     ASSUME_ALIGNED asig:64
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
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp2
!DIR$     ATTRIBUTES ALIGN : 64 :: w
          ! Start of executable statements
          
          if(profiling) then
             call qpctimer_start(qpctimer,ifail)
             if(ifail.EQ.0) then
                 write(stderr,*) "mod_expchirp_signal/analytic_signal:879, qpctimer_start failed to query performance frequency counter!!"
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
    end subroutine  analytic_signal
    
    !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
        
          class(ExpChirpSignal_t), intent(in) :: this
          character(len=*),        intent(in) :: form
          integer(I32P),           intent(in) :: unit
          integer(I32P),           intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine read_state
    
    subroutine write_state(this,form,unit,ioerr)
         
          class(ExpChirpSignal_t), intent(in) :: this
          character(len=*),        intent(in) :: form
          integer(I32P),           intent(in) :: unit
          integer(I32P),           intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine write_state
    
    
    
    !======================================================60
    !  subroutine: dbg_info
    !======================================================60  
    subroutine dbg_info(this)
         
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
          print*, "Signal ID:          ", this%m_sid
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
          
          print*, "           End                     "
    end subroutine  dbg_info
                                
    !============================================
    !  Operator assignment (=)
    !============================================                            
    subroutine copy_assign(lhs,rhs)
          class(ExpChirpSignal_t),  intent(inout) :: lhs
          class(ExpChirpSignal_t),  intent(in)    :: rhs
          ! Start of executable statements
          lhs%m_name    = rhs%m_name
          lhs%m_sid     = rhs%m_sid
          lhs%m_nsamp   = rhs%m_nsamp
          lhs%m_dur     = rhs%m_dur
          lhs%m_sfreq   = rhs%m_sfreq
          lhs%m_efreq   = rhs%m_efreq
          lhs%m_stsinc  = rhs%m_stsinc
          lhs%m_siphi   = rhs%m_siphi
          lhs%m_chrate  = rhs%m_chrate
          lhs%m_sinit   = rhs%m_sinit
          lhs%m_jvec    = rhs%m_jvec
          lhs%m_E       = rhs%m_E
          lhs%m_nenvp   = rhs%m_nenvp
          lhs%m_phi     = rhs%m_phi
          lhs%m_canform = rhs%m_canform
          lhs%m_basform = rhs%m_basform
          lhs%m_cenvp   = rhs%m_cenvp
          lhs%m_amp     = rhs%m_amp
          lhs%m_S       = rhs%m_S
    end subroutine copy_assign
                                
end module mod_expchirp_signal