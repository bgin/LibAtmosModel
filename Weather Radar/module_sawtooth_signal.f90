
#include "Config.fpp"     
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
 !           Modified:
 !                     By Bernard Gingold on: 09-03-2018, 17:33 GMT+2
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
    
   
    
    use module_kinds,        only : I32P, R64P
    use mod_code_timing
    use mod_complex_arithm , only : vcmag
    use mod_jonesvec
    implicit none
    
   
   
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
    
    type, public :: SawtoothSignal_t
        
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
        
        
        
        ! Jones Vector
        type(JonesVector_t), allocatable, dimension(:),   public :: m_jvec
        
        ! Sine components of K sinusoids
        real(R64P), allocatable, dimension(:,:),          public :: m_scomp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_scomp
        
        ! Cosine components of K cosinusoids
        real(R64P), allocatable, dimension(:,:),          public :: m_ccomp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_ccomp
        
        ! Sawtooth carrier frequency*t
        real(R64P), allocatable, dimension(:),            public   :: m_wct
!DIR$   ATTRIBUTES ALIGN : 64 :: m_phi
        
        ! Signal natural envelope
        real(R64P), allocatable, dimension(:),            public   :: m_nenvp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_nenvp 
        
        ! Signal  i.e. Sawtooth(phi(t))
        ! approximated by K sinusoids
        real(R64P), allocatable, dimension(:),            public   :: m_sawtooth
!DIR$   ATTRIBUTES ALIGN : 64 :: m_sawtooth
        
        ! Sawtooth complex representation
        complex(R64P), allocatable, dimension(:),         public   :: m_cform
!DIR$   ATTRIBUTES ALIGN : 64 :: m_canform 
        
        ! Electric field
        complex(R64P), allocatable, dimension(:),         public   :: m_E
!DIR$   ATTRIBUTES ALIGN : 64 :: m_E
        
         ! Signal amplitude  A(phi,theta)/r used for calculation electric field
         ! far from emmiter(antenna)
         real(R64P), allocatable, dimension(:),           public   :: m_amp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_amp
         
 ! Time-averaged power density
      
         complex(R64P), allocatable, dimension(:),        public    :: m_S
!DIR$   ATTRIBUTES ALIGN : 64 :: m_S
         
        
        
        contains
    
      
        
        ! Creates Sawtooth signal approximated by its
        ! Fourier series
        procedure, pass(this), public :: sawtooth_signal
        
        ! 
        ! Creates signal with noise modulated phase
        procedure, pass(this), public :: phase_noise_sawtooth_signal
        
        
        ! Creates signal with noise modulated phase
        ! coupled with background additive noise.
        procedure, pass(this), public :: additive_noise_sawtooth_signal
        
       
        
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
        
       
        
        !==================================================60
        ! Computational subroutines
        !==================================================60
        
        procedure, pass(this), public :: dphi_dt
        
       
        
        !==============================================56
        !      write/read subroutines
        !==============================================56
        procedure, nopass, public :: write_state
        
        procedure, nopass, public :: read_state
        !==============================================56
        !   Class helper procedures
        !==============================================56
        
        procedure, pass(this),  public :: dbg_info
        
        !==============================================56
        !    Generic operators
        !==============================================56
        
        procedure, public :: copy_assign
        
        generic :: assignment (=) => copy_assign
        
    end type SawtoothSignal_t
        
    interface SawtoothSignal_t
        procedure :: constructor
    end interface SawtoothSignal_t
        
    contains
               ! type(SawtoothSignal_t) :: sig
               ! sig = SawtoothSignal_t(1,2,3,4)
               ! call sig%sawtooth_signal(...,..,..
    !======================================================60
    ! @function: constructor
    !             default initialization (no physical meaning)
    !======================================================60
    type(SawtoothSignal_t) function constructor(name,sid,nsamp,maxk,logging, &
                                                verbose, filename,append  )
          use mod_print_error,  only : handle_fatal_memory_error   
          use mod_constants,    only : LAM_PINF, LAM_MINSAMP, &
                                       LAM_MINK
          character(len=*),        intent(in)    :: name
          integer(I32P),           intent(in)    :: sid
          integer(I32P),           intent(inout) :: nsamp,maxk
          logical(I32P),           intent(in)    :: logging,verbose
          character(len=*),        intent(in)    :: filename
          logical(I32P),           intent(in)    :: append
          ! locals
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          ! Start of executable statements
          if(nsamp <  LAM_MINSAMP) then
              nsamp = LAM_MINSAMP
          end if
          if(maxk <  LAM_MINK) then
              maxk = LAM_MINK
          end if
           ! Begin construction
          constructor%m_name   = name
          constructor%m_sid    = sid
          constructor%m_nsamp  = nsamp
          constructor%m_maxk   = maxk
          constructor%m_dur    = LAM_PINF
          constructor%m_A      = LAM_PINF
          constructor%m_sinit  = LAM_PINF
          constructor%m_stsinc = LAM_PINF
          constructor%m_freq   = LAM_PINF
          associate(k=>constructor%m_maxk,        &
                    n=>constructor%m_nsamp)
              allocate(constructor%m_jvec(n),     &
                       constructor%m_scomp(k,n),  &
                       constructor%m_ccomp(k,n),  &
                       constructor%m_wct(n),      &
                       constructor%m_nenvp(n),    &
                       constructor%m_sawtooth(n), &
                       constructor%m_cform(n),    &
                       constructor%m_E(n),        &
                       constructor%m_amp(n),      &
                       constructor%m_S(n),        &
                       STAT=aerr,                 &
                       ERRMSG=emsg )
          end associate
          if(aerr /= 0) then
             
              call handle_fatal_memory_error( logging,verbose,append,filename,   &
                                              "logger:319 --> mod_sawtooth_signal/constructor: Memory Allocation Failure!!" , &
                                              "mod_sawtooth_signal/constructor:319 -- Memory Allocation Failure!" ,  &
                                              emsg,__LINE__ )
          end if 
          constructor%m_jvec     = JonesVector() 
          constructor%m_scomp    = LAM_PINF
          constructor%m_ccomp    = LAM_PINF
          constructor%m_wct      = LAM_PINF
          constructor%m_nenvp    = LAM_PINF
          constructor%m_sawtooth = LAM_PINF
          constructor%m_cform    = CMPLX(LAM_PINF,LAM_PINF,R64P)
          constructor%m_E        = CMPLX(LAM_PINF,LAM_PINF,R64P)
          constructor%m_amp      = LAM_PINF
          constructor%m_S        = CMPLX(LAM_PINF,LAM_PINF,R64P)
   end function constructor     
       
   
                               
    !======================================================60
    !  subroutine: sawtooth_signal                        
    !              Physical representation of  Sawtooth
    !              approximated by K sinusoids (Fourier-
    !              series)
    !======================================================60                                   
    subroutine sawtooth_signal(this,ierr,dur,A,                      &
                             sinit,stsinc,freq,r,h,v,                &
                             verbose,logging,filename,append,        &
                              profiling,qpctimer      )
          use mod_print_error,   only : handle_fatal_memory_error
          use mod_constants,     only : LAM_ZR8, LAM_HR64P, LAM_2PI,LAM_PI, LAM_PINF
          class(SawtoothSignal_t),     intent(inout) :: this
          integer(I32P),               intent(inout) :: ierr
          real(R64P),                  intent(inout) :: dur,A
          real(R64P),                  intent(in)    :: sinit,stsinc,freq
          real(R64P), dimension(:),    intent(in)    :: r
!DIR$     ASSUME_ALIGNED r:64
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:64,v:64
          logical(I32P),               intent(in)    :: verbose,logging
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          character(len=256) :: emsg
          integer(I32P)      :: k,j,aerr
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail
          real(R64P)         :: insamp,t,t2,delta,hA,API
          real(R64P),    allocatable, dimension(:) :: tcos,ts,tc
!DIR$     ATTRIBUTES ALIGN : 64 ::  tcos
!DIR$     ATTRIBUTES ALIGN : 64 ::  ts
!DIR$     ATTRIBUTES ALIGN : 64 ::  tc
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(dur <=   LAM_ZR8 .OR. &
             A   <=   LAM_ZR8 ) then
              ierr = -1_I32P
              return
          end if
          this%m_dur = dur
          this%m_A  = A
          this%m_sinit = sinit
          this%m_stsinc = stsinc
          this%m_freq = freq
          associate(n=>this%m_nsamp)
              allocate(tcos(n),             &
                       ts(n),               &
                       tc(n),               &
                       STAT=aerr,           &
                       ERRMSG=emsg         )
        end associate               
        if(aerr /= 0) then
             
              call handle_fatal_memory_error( logging,verbose,append,filename,    &
                                              "logger:397 --> mod_sawtooth_singnal/sawtooth_signal: Memory Allocation Failure!!", &
                                              "mod_sawtooth_signal/sawtooth_signal:397 --  Memory Allocation Failure!" ,          &
                                              emsg,__LINE__ )
        end if  
        tcos = LAM_PINF
        ts   = LAM_PINF
        tc   = LAM_PINF
        if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail == 0) then
                  write(stderr,*) "mod_sawtooth_signal/sawtooth_signal: qpctimer_start failed to query performance frequency counter!"
              end if
        end if  
          ! Set up jones vector field
        do j = 1, this%m_nsamp
              this%m_jvec(j) = JonesVector_t(h(j),v(j))
        end do
          ! Create sine and cosine Fourier series components
        hA = LAM_HR64P*this%m_A
        API = this%m_A/LAM_PI
        insamp = 1._R64P/DBLE(this%m_nsamp)
        do k = 1, this%m_maxk
            do j = 1, this%m_nsamp
                  this%m_sinit = this%m_sinit+this%m_stsinc
                  delta = DBLE(j)*insamp
                  t = this%m_sinit*delta
                  
                  this%m_scomp(k,j) = -1**k*DSIN(LAM_2PI*k*this%m_freq*t)/k
                  this%m_ccomp(k,j) = -1**k*DCOS(LAM_2PI*k*this%m_freq*t)/k
             end do
        end do
          ! Do summation over the rows
        this%m_sawtooth = SUM(this%m_scomp,dim=2)
        tcos = SUM(this%m_ccomp,dim=2)
          ! Create sawtooth signal (final approximation) and
          ! create signal complex representation
        do j = 1, this%m_nsamp
              this%m_sinit = this%m_sinit+this%m_stsinc
              delta = DBLE(j)*insamp
              t = this%m_sinit*delta
              this%m_wct(j) = t
              this%m_sawtooth(j) = hA-(API*this%m_sawtooth(j))
              tcos(j) = hA-(API*tcos(j))
              this%m_cform(i) = DCMPLX(tcos(j),this%m_sawtooth(j))
        end do
          ! Compute natural envelope
        call vcmag(this%m_cform,this%m_nenvp)
          ! Create electrical field far from the emitter.
        do k = 1, this%m_maxk
            do j = 1, this%m_nsamp
                  this%m_sinit = this%m_sinit+this%m_stsinc
                  delta = DBLE(j)*insamp
                  t = this%m_sinit*delta
                  t2 = t-r(j)/LAM_c
                  this%m_scomp(k,j) = -1**k*DSIN(LAM_2PI*k*this%m_freq*t2)/k
                  this%m_ccomp(k,j) = -1**k*DCOS(LAM_2PI*k*this%m_freq*t2)/k
            end do
        end do
          ! Do summation over the rows
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
                         write(stderr,*) "mod_sawtooth_signal/sawtooth_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_sawtooth_signal/sawtooth_signal: Unable to read performance counter -- fatal!!"
                end if
        end if
    end subroutine sawtooth_signal        
        
         
          
        
         
          
         
         
         
         
        
       
    
                             
    !======================================================60
    !  @subroutine: phase_noise_sawtooth_signal                        
    !              Physical representation of Sawtooth wave
    !              approximated by K sinusoids (Fourier-
    !              series)
    !              This signal is corrupted by phase additive
    !              noise.       
    !======================================================60
    subroutine phase_noise_sawtooth_signal(this,ierr,dur,A,                 &
                                  sinit,stsinc,freq,phnoise,r,h,v,          &
                                  verbose,logging,filename,append,          &
                                  profiling,qpctimer  )
          use mod_print_error,  only : handle_fatal_memory_error
          use mod_constants,    only : LAM_ZR8, LAM_HR64P, LAM_2PI, LAM_PI, LAM_PINF
          class(SawtoothSignal_t),     intent(inout) :: this
          integer(I32P),               intent(inout) :: ierr
          real(R64P),                  intent(inout) :: dur,A
          real(R64P),                  intent(in)    :: sinit,stsinc,freq
          real(R64P), dimension(:),    intent(in)    :: phnoise,r
!DIR$     ASSUME_ALIGNED phnoise:64,r:64
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:64,v:64
          logical(I32P),               intent(in)    :: verbose,logging
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          
          character(len=256) :: emsg
          integer(I32P)      :: k,j,aerr
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail
          real(R64P)         :: insamp,t,t2,delta,hA,API,tn
          real(R64P),    allocatable, dimension(:) :: tcos,ts,tc
!DIR$     ATTRIBUTES ALIGN : 64 :: tcos
!DIR$     ATTRIBUTES ALIGN : 64 :: ts
!DIR$     ATTRIBUTES ALIGN : 64 :: tc
          ! Start of executable statements
         
          if(ierr < 0_I32P) ierr = 0_I32P
          if(dur <= LAM_ZR8 .OR. &
             A   <= LAM_ZR8  ) then
              ierr = -1_I32P
              return
          end if
          this%m_dur    = dur
          this%m_A      = A
          this%m_sinit  = sinit
          this%m_stsinc = stsinc
          this%m_freq   = freq
          associate(n=>this%m_nsamp)
              allocate(  tcos(n),             &
                         ts(n),               &
                         tc(n),               &
                         STAT=aerr,           &
                         ERRMSG=emsg      )
          end associate
          if(aerr /= 0) then
           
              call handle_fatal_memory_error( logging,verbose,append,filename,   &
                                              "logger:541 --> mod_sawtooth_singnal/phase_noise_sawtooth_signal: Memory Allocation Failure!!" , &
                                              "mod_sawtooth_signal/phase_noise_sawtooth_signal:541 -- Memory Allocation Failure!" ,     &
                                              emsg,__LINE__ )
          end if
          tcos = LAM_PINF
          ts   = LAM_PINF
          tc   = LAM_PINF
            if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail == 0) then
                  write(stderr,*) "mod_sawtooth_signal/phase_noise_signal: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
          ! Set up jones vector field
          do j = 1, this%m_nsamp
              this%m_jvec(j) = JonesVector_t(h(j),v(j))
          end do  
           ! Create sine and cosine Fourier series components
          hA = LAM_HR64P*this%m_A
          API = this%m_A/LAM_PI
          insamp = 1._R64P/DBLE(this%m_nsamp)
          do k = 1, this%m_maxk
              do j = 1, this%m_nsamp
                  this%m_sinit = this%m_sinit+this%m_stsinc
                  delta = DBLE(j)*insamp
                  t = this%m_sinit*delta
                  tn = t+phnoise(j)
                  this%m_scomp(k,j) = -1**k*DSIN(LAM_2PI*k*this%m_freq*tn)/k
                  this%m_ccomp(k,j) = -1**k*DCOS(LAM_2PI*k*this%m_freq*tn)/k
              end do
          end do
          ! Do summation over the rows
          this%m_sawtooth = SUM(this%m_scomp,dim=2)
          tcos = SUM(this%m_ccomp,dim=2)
          ! Create sawtooth signal (final approximation) and
          ! create signal complex representation
          do j = 1, this%m_nsamp
              this%m_sinit = this%m_sinit+this%m_stsinc
              delta = DBLE(j)*insamp
              t = this%m_sinit*delta
              tn = t+phnoise(j)
              this%m_wct(j) = tn
              this%m_sawtooth(j) = hA-(API*this%m_sawtooth(j))
              tcos(j) = hA-(API*tcos(j))
              this%m_cform(i) = DCMPLX(tcos(j),this%m_sawtooth(j))
          end do
          ! Compute natural envelope
          call vcmag(this%m_cform,this%m_nenvp)
          ! Create electrical field far from the emitter.
          do k = 1, this%m_maxk
              do j = 1, this%m_nsamp
                  this%m_sinit = this%m_sinit+this%m_stsinc
                  delta = DBLE(j)*insamp
                  t = this%m_sinit*delta
                  t2 = t-r(j)/LAM_c
                  tn = t2+phnoise(j)
                  this%m_scomp(k,j) = -1**k*DSIN(LAM_2PI*k*this%m_freq*tn)/k
                  this%m_ccomp(k,j) = -1**k*DCOS(LAM_2PI*k*this%m_freq*tn)/k
              end do
          end do
          ! Do summation over the rows
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
                if(ifail == 0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail == .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_sawtooth_signal/phase_noise_sawtooth_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_sawtooth_signal/phase_sawtooth_signal_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
   end subroutine phase_noise_sawtooth_signal                  
                        
                       
          
         
        
   
                                  
    !======================================================60
    !  subroutine: additive_noise_signal                        
    !              Physical representation of Sawtooth wave
    !              approximated by K sinusoids (Fourier-
    !              series)
    !              This signal is corrupted by phase additive
    !              noise and by background additive noise.      
    !======================================================60 
    subroutine additive_noise_sawtooth_signal(this,ierr,dur,A,             &
                                  sinit,stsinc,freq,phnoise,bnoise,r,h,v,  &
                                  verbose,logging,filename,append,         &
                                  profiling,qpctimer  ) 
          use mod_print_error,  only : handle_fatal_memory_error
          use mod_constants,    only : LAM_ZR8, LAM_HR64P, LAM_2PI, LAM_PI
          class(SawtoothSignal_t),     intent(inout) :: this
          integer(I32P),               intent(inout) :: ierr
          real(R64P),                  intent(inout) :: dur,A
          real(R64P),                  intent(in)    :: sinit,stsinc,freq
          real(R64P), dimension(:),    intent(in)    :: phnoise,bnoise,r
!DIR$     ASSUME_ALIGNED phnoise:64,bnoise:64,r:64
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:64,v:64
          logical(I32P),               intent(in)    :: verbose,logging
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          
          character(len=256) :: emsg
          integer(I32P)      :: k,j,aerr
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail
          real(R64P)         :: insamp,t,t2,delta,hA,API,tn
          real(R64P),    allocatable, dimension(:) :: tcos,ts,tc
!DIR$     ATTRIBUTES ALIGN : 64 :: tcos
!DIR$     ATTRIBUTES ALIGN : 64 :: ts
!DIR$     ATTRIBUTES ALIGN : 64 :: tc
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(dur <= LAM_ZR8 .OR. &
             A   <= LAM_ZR8 ) then
             ierr = -1_I32P
              return
          end if
          this%m_dur = dur
          this%m_A  = A
          this%m_sinit = sinit
          this%m_stsinc = stsinc
          this%m_freq = freq
          associate(n=>this%m_nsamp)
              allocate(  tcos(n),             &
                         ts(n),               &
                         tc(n),               &
                         STAT=aerr,           &
                         ERRMSG=emsg   )
          end associate                
          if(aerr /= 0) then
            
              call handle_fatal_memory_error(logging,verbose,append,filename,    &
                                             "logger:697 --> mod_sawtooth_singnal/additive_noise_sawtooth_signal: Memory Allocation Failure!!" , &
                                             "mod_sawtooth_signal/additive_noise_sawtooth_signal:697 -- Memory Allocation Failure!" , &
                                             emsg,__LINE__ )
          end if    
            if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail == 0) then
                  write(stderr,*) "mod_sawtooth_signal/additive_noise_sawtooth_signal: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
          ! Set up jones vector field
          do j = 1, this%m_nsamp
              this%m_jvec(j) = JonesVector_t(h(j),v(j))
          end do  
           ! Create sine and cosine Fourier series components
          hA = LAM_HR64P*this%m_A
          API = this%m_A/LAM_PI
          insamp = 1._R64P/DBLE(this%m_nsamp)
          do k = 1, this%m_maxk
              do j = 1, this%m_nsamp
                  this%m_sinit = this%m_sinit+this%m_stsinc
                  delta = DBLE(j)*insamp
                  t = this%m_sinit*delta
                  tn = t+phnoise(j)
                  this%m_scomp(k,j) = -1**k*DSIN(LAM_2PI*k*this%m_freq*tn)/k
                  this%m_scomp(k,j) = bnoise(j)+this%m_scomp(k,j)
                  this%m_ccomp(k,j) = -1**k*DCOS(LAM_2PI*k*this%m_freq*tn)/k
                  this%m_ccomp(k,j) = bnoise(j)+this%m_ccomp(k,j)
              end do
          end do
          ! Do summation over the rows
          this%m_sawtooth = SUM(this%m_scomp,dim=2)
          tcos = SUM(this%m_ccomp,dim=2)
          ! Create sawtooth signal (final approximation) and
          ! create signal complex representation
          do j = 1, this%m_nsamp
              this%m_sinit = this%m_sinit+this%m_stsinc
              delta = DBLE(j)*insamp
              t = this%m_sinit*delta
              tn = t+phnoise(j)
              this%m_wct(j) = tn
              this%m_sawtooth(j) = hA-(API*this%m_sawtooth(j))
              tcos(j) = hA-(API*tcos(j))
              this%m_cform(i) = DCMPLX(tcos(j),this%m_sawtooth(j))
          end do
          ! Compute natural envelope
          call vcmag(this%m_cform,this%m_nenvp)
          ! Create electrical field far from the emitter.
          do k = 1, this%m_maxk
              do j = 1, this%m_nsamp
                  this%m_sinit = this%m_sinit+this%m_stsinc
                  delta = DBLE(j)*insamp
                  t = this%m_sinit*delta
                  t2 = t-r(j)/LAM_c
                  tn = t2+phnoise(j)
                  this%m_scomp(k,j) = -1**k*DSIN(LAM_2PI*k*this%m_freq*tn)/k
                  this%m_scomp(k,j) = bnoise(j)+this%m_scomp(k,j)
                  this%m_ccomp(k,j) = -1**k*DCOS(LAM_2PI*k*this%m_freq*tn)/k
                  this%m_ccomp(k,j) = bnoise(j)+this%m_ccomp(k,j)
              end do
          end do
          ! Do summation over the rows
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
                         write(stderr,*) "mod_sawtooth_signal/additive_noise_sawtooth_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_sawtooth_signal/additive_noise_sawtooth_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
   end subroutine additive_noise_sawtooth_signal      
          
           
                  
                       
       
        
        
   
  
 



    
    !======================================================60
    ! Getter pure functions
    !======================================================60
    
    pure function get_name(this) result(name)
          implicit none
          class(SawtoothSignal_t), intent(in) :: this
          ! Locals
          character(len=64) :: name
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
    
 
    
    !======================================================60
    !    Computational procedures
    !======================================================60
    ! TODO:
    !        ! Consider using CADNA to test for cancellation errors
    subroutine dphi_dt(this,dphi,sfac)
          use mod_constants, only : LAM_IZER8, LAM_MEPS8
          class(SawtoothSignal_t),  intent(in)     :: this
          real(R64P), dimension(:), intent(out)    :: dphi
!DIR$     ASSUME_ALIGNED dphi:64
          integer(I64P),            intent(inout)  :: sfac
          ! Locals
          integer(I32P) :: i
          real(R64P)    :: eps,isfac,tmp
          ! Start of executable sttements
          if(sfac <=  LAM_IZER8) then
              sfac =  1000000000000000_I64P
          end if
          isfac = 1._R64P/DBLE(sfac)
          eps = LAM_MEPS8**0.3333333333333333333333333333_R64P
          do i = 2, this%m_nsamp-1
              tmp = this%m_wct(i+1)-this%m_wct(i-1)
              dphi(i) = this%m_wct(i+1)-this%m_wct(i-1) / &
                        (2._R64P*eps*DMAX1(DABS(tmp),isfac)*DSIGN(tmp,tmp))
          end do
    end subroutine  dphi_dt
    
     !============================================
    !   Read/write procedures
    !============================================
       subroutine read_state(this,form,unit,ioerr)
        
          class(SawtoothSignal_t), intent(in) :: this
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
         
          class(SawtoothSignal_t), intent(in) :: this
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
    subroutine dbg_info(this,verbose)
          
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
          print*, "Signal ID:          ", this%m_sid
          print*, "Number of samples:  ", this%m_nsamp
          print*, "Maximum 'k':        ", this%m_maxk
          print*, "Signal duration:    ", this%m_dur
          print*, "scalar amplitude:   ", this%m_A
          print*, "Initial time-point: ", this%m_sinit
          print*, "Time-step increment:", this%m_stsinc
          print*, "Carrier frequency:  ", this%m_freq
          print*, "           Dumping array components  "
          print*, "======================================================"
          
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
    end subroutine dbg_info
    
    !============================================
    !  Operator assignment (=)
    !============================================
    subroutine copy_assign(lhs,rhs)
          class(SawtoothSignal_t),  intent(inout) :: lhs
          class(SawtoothSignal_t),  intent(in)    :: rhs
          ! Start of executable ststements
          lhs%m_name     = rhs%m_name
          lhs%m_sid      = rhs%m_sid
          lhs%m_nsamp    = rhs%m_nsamp
          lhs%m_maxk     = rhs%m_maxk
          lhs%m_dur      = rhs%m_dur
          lhs%m_A        = rhs%m_A
          lhs%m_sinit    = rhs%m_sinit
          lhs%m_stsinc   = rhs%m_stsinc
          lhs%m_freq     = rhs%m_freq
          lhs%m_jvec     = rhs%m_jvec
          lhs%m_scomp    = rhs%m_scomp
          lhs%m_ccomp    = rhs%m_ccomp
          lhs%m_wct      = rhs%m_wct
          lhs%m_nenvp    = rhs%m_nenvp
          lhs%m_sawtooth = rhs%m_sawtooth
          lhs%m_cform    = rhs%m_cform
          lhs%m_E        = rhs%m_E
          lhs%m_amp      = rhs%m_amp
          lhs%m_S        = rhs%m_S
    end subroutine copy_assign
    
end module mod_sawtooth_signal