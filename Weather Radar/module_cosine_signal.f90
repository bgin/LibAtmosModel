
#include "Config.fpp"


module mod_cosine_signal

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_purecose_pulse'
 !          
 !          Purpose:
 !                   Pure cosine signal pulse
 !                   
 !                     
 !          History:
 !                        Date: 05-08-2017
 !                        Time: 13:19 GMT+2
 !          Modified:
 !                    Bernard Gingold on: 12-03-2018, 17:05 GMT+2
 !                   1) Refactoring to Fortran 2003 standard.
 !                   2) Removing unnecessary type-bound members
 !                   3) Adding computational subroutines (additive noise)
 !                   4) Removing unnecessary destructor,copy and initialization subroutines.
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

    
    
   
    use module_kinds, only : I32P, R64P
    use mod_code_timing
    use mod_complex_arithm , only : vcmag
    use mod_jonesvec
    implicit none
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_COSINE_SIGNAL_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_COSINE_SIGNAL_MINOR = 0
                                           
    ! Micro version
    integer(I32P), parameter, public :: MOD_COSINE_SIGNAL_MICRO = 0
    
    ! Module/file full version
    integer(I32P), parameter, public :: MOD_COSINE_SIGNAL_FULLVER = 1000*MOD_COSINE_SIGNAL_MAJOR+100*MOD_COSINE_SIGNAL_MINOR + &
                                                                    10*MOD_COSINE_SIGNAL_MICRO
    
    ! Module/file creation date
    character(*),  parameter, public :: MOD_COSINE_SIGNAL_CREATE_DATE = "05-08-2017 13:19 +00200 (SAT 05 AUG 2017 GMT+2)"
    
    ! Module build date/time (should be set to latest build date/time)
    character(*),  parameter, public :: MOD_COSINE_SIGNAL_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_COSINE_SIGNAL_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Midule short description
    character(*),  parameter, public :: MOD_COSINE_SIGNAL_DESCRIPT = "Cosine complex domain implementation."
    
    integer(I32P), parameter, private :: MINSAMP = 32
    
    real(R64P),    parameter, private :: MINFREQ = 1.0e+7_R64P
    
    real(R64P),    parameter, private :: NS = 0.000000001_R64P
    
    real(R64P),    parameter, private :: TWOPI = 6.283185307179586476925286766559_R64P
    
    real(R64P),    parameter, private :: c = 3._R64P*1.0e+8_R64P
    
    real(R64P),    parameter, private :: SMALLTS = 0.0000152587890625_R64P ! smallest time step = 1/2^-16
    
    !======================================================
    !  Type: CosineSignal_t  
    !======================================================
    
    type :: CosineSignal_t
        
        private
        
        ! derived type components
        
        ! Pulse name
        character(len=64) :: m_name
        
       
        
        
        ! Signal id .i.e cardinal number in signal train
        integer(I32P)     :: m_sid
        
        ! Number of samples
        integer(I32P)     :: m_nsamp
        
        ! Signal duration
        real(R64P)        :: m_dur
        
        ! Starting time-point of samples measurement
        real(R64P)        :: m_initime
        
        ! Samples interval
        real(R64P)        :: m_sinterv
        
        ! Carrier frequency
        real(R64P)        :: m_cfreq
        
        ! Evnevlope signal frequency
        real(R64P)        :: m_envfreq
        
        real(R64P)        :: m_tstep ! time step
        
        
        
        ! Jones Vector contains amplitudes of voltage
        ! i.e. Enl,Enk
        !complex(R64P), dimension(2) :: m_jvec
        type(JonesVector_t), allocatable, dimension(:), public :: m_jvec
        ! Electric field wave component.
        ! Phasor representation.
        complex(R64P), allocatable, dimension(:),       public :: m_E
!DIR$   ATTRIBUTES ALIGN : 64 :: m_E
        
        ! Signal enevelope i.e natural envelope  set to 1
        real(R64P), allocatable, dimension(:),          public :: m_nenvp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_nenvp
        
        ! Signal phase
        real(R64P), allocatable, dimension(:),          public :: m_phi
!DIR$   ATTRIBUTES ALIGN : 64 :: m_phi
        
        ! Signal canonical form i.e. S(t) = gc(t)*cos(wct)-gs(t)*sin(wct)
        complex(R64P), allocatable, dimension(:),       public :: m_canform
!DIR$   ATTRIBUTES ALIGN : 64 :: m_canform
        
        ! Signal basic form i.e. S(t) = g(t)*cos[wct+phi(t)]
        real(R64P), allocatable, dimension(:),          public :: m_basform
!DIR$   ATTRIBUTES ALIGN : 64 :: m_basform        
        ! Signal complex envelope
        complex(R64P), allocatable, dimension(:),       public :: m_cenvp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_cenvp
        
        ! Signal amplitude  A(phi,theta)/r used forcalculation electric field
        ! far from emmiter(antenna)
        real(R64P), allocatable, dimension(:),          public :: m_amp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_amp
        
        ! Time-averaged power density
        ! To be properly implemented
        complex(R64P), allocatable, dimension(:),       public :: m_S
!DIR$   ATTRIBUTES ALIGN : 64 :: m_S
        
        
        
        
        contains
    
        !========================================
        !  Constructor subroutines
        !========================================
    
        
        ! Constructor creates pure cosine pulse
        
        
        procedure, pass(this), public :: cosine_signal
        
        procedure, pass(this), public :: phase_noise_cosine_signal
        
        procedure, pass(this), public :: additive_noise_cosine_signal
        
        !==========================================52
        !   Getter functions
        !==========================================52
        
        procedure, pass(this), public :: get_name
        
        procedure, pass(this), public :: get_pid
        
        procedure, pass(this), public :: get_nsamp
        
        procedure, pass(this), public :: get_dur
        
        procedure, pass(this), public :: get_initime
        
        procedure, pass(this), public :: get_sinterv
        
        procedure, pass(this), public :: get_cfreq
        
        procedure, pass(this), public :: get_envfreq
        
        procedure, pass(this), public :: get_tstep
        
      
        
        !==================================================60
        ! Computational subroutines
        !==================================================60
        
        
        procedure, pass(this), public :: dphi_dt
        
        procedure, pass(this), public :: analytic_signal
        
        !==============================================56
        ! write/read subroutines
        !==============================================56
        
        procedure, nopass,     public :: read_state
        
        procedure, nopass,     public :: write_state
        
        !==============================================56
        ! write/read subroutines
        !==============================================56
        
        procedure, pass(this), public :: dbg_info
        
        !===============================================
        !   Generic operators
        !===============================================
        procedure, public :: copy_assign
        
        generic :: assignment (=) => copy_assign
        
    end type CosineSignal_t
        
    interface CosineSignal_t
         procedure :: constructor
    end interface CosineSignal_t
    
    contains
    
    !======================================================60
    ! @subroutine:  constructor
    !               default_signal (no physical meaning)
    !======================================================60
   type(CosineSignal_t) function constructor(name,sid,nsamp,dur,logging, &
                                            verbose,filename,append      )
          use mod_print_error,  only : handle_fatal_memory_error
          use mod_constants,    only : LAM_PINF, LAM_MINSAMP
                                       
          character(len=*),         intent(in)    :: name
          integer(I32P),            intent(in)    :: sid
          integer(I32P),            intent(inout) :: nsamp
          real(R64P),               intent(in)    :: dur
          logical(I32P),            intent(in)    :: verbose,logging
          character(len=*),         intent(in)    :: filename
          logical(I32P),            intent(in)    :: append
          ! Locals
          
          integer(I32P)      :: aerr
          character(len=256) :: emsg
          ! Start of executable statements
          
          if(nsamp < 32) then
              nsamp = LAM_MINSAMP
          end if
          
          ! Begin construction
          constructor%m_name     = name
          constructor%m_sid      = sid
          constructor%m_nsamp    = nsamp
          constructor%m_dur      = dur
          constructor%m_initime  = LAM_PINF
          constructor%m_sinterv  = LAM_PINF 
          constructor%m_cfreq    = LAM_PINF
          constructor%m_envfreq  = LAM_PINF
          constructor%m_tstep    = LAM_PINF
         
          
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
                       STAT=aerr,     &
                       ERRMSG=emsg)
          end associate
          if(aerr /= 0) then
            
              call handle_fatal_memory_error(logging,verbose,append,filename,    &
                                             "logger:308 --> mod_cosine_signal/constructor: Memory Allocation Failure!", &
                                             "mod_cosine_signal/constructor:308 -- Memory Allocation Failure!"  , &
                                             emsg,__LINE__ )
          end if
          constructor%m_jvec    = JonesVector_t()
          constructor%m_E       = CMPLX(LAM_PINF,LAM_PINF,R64P)
          constructor%m_nenvp   = LAM_PINF
          constructor%m_phi     = LAM_PINF
          constructor%m_canform = CMPLX(LAM_PINF,LAM_PINF,R64P)
          constructor%m_basform = LAM_PINF
          constructor%m_cenvp   = CMPLX(LAM_PINF,LAM_PINF,R64P)
          constructor%m_amp     = LAM_PINF
          constructor%m_S       = CMPLX(LAM_PINF,LAM_PINF,R64P)
    end function constructor
         
    
                              
    !======================================================60                          
    !  @subroutine: cosine_signal
    !======================================================60
    subroutine cosine_signal(this,ierr,dur,initime, &
                            sinterv,cfreq,envfreq,tstep,r,c1,c2,logging,   &
                            verbose,filename,append,profiling,qpctimer   )
          use mod_print_error,  only : handle_fatal_memory_error ,  &
                                        stderr
          use mod_constants,    only : LAM_ZR8,LAM_PINF,LAM_2PI,LAM_c
          class(CosineSignal_t),       intent(inout) :: this
          integer(I32P),               intent(inout) :: ierr
          real(R64P),                  intent(in)    :: dur,initime,tstep
          real(R64P),                  intent(inout) :: sinterv
          real(R64P),                  intent(inout) :: cfreq,envfreq
          real(R64P), dimension(:),    intent(in)    :: r ! range
!DIR$     ASSUME_ALIGNED r:64          
          complex(R64P), dimension(:), intent(in)    :: c1,c2
!DIR$     ASSUME_ALIGNED c1:64,c2:64
          logical(I32P),            intent(in)    :: logging,verbose
          character(len=*),         intent(in)    :: filename
          logical(I32P),            intent(in)    :: append,profiling
          type(QPCTimer_t),         intent(inout) :: qpctimer
          ! Locals
          
          character(len=256) :: emsg
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          integer(I32P) :: i,aerr
          real(R64P)    :: insamp,delta,t,t2,tc,ts
          complex(R64P), allocatable, dimension(:) :: tmp
          real(R64P), allocatable, dimension(:) :: ct
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: ct
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(dur < LAM_ZR8) then
              ierr = -1_I32P
              return
          end if
          this%m_dur     = dur
          this%m_initime = initime
          this%m_sinterv = sinterv
          this%m_cfreq   = cfreq
          this%m_envfreq = envfreq
          this%m_tstep   = tstep
          associate(n=>this%m_nsamp)
              allocate(  tmp(n),            &
                         ct(n),             &
                         STAT=aerr,     &
                         ERRMSG=emsg)
          end associate
          if(aerr /= 0) then
             
              call handle_fatal_memory_error(logging,verbose,append,filename,   &
                                             "logger:390 --> mod_cosine_signal/cosine_signal: Memory Allocation Failure!"   , &
                                             "mod_cosine_signal/cosine_signal:390 -- Memory Allocation Failure"  , &
                                             emsg,__LINE__ )
          end if
          tmp = CMPLX(LAM_PINF,LAM_PINF,R64P)
          ct  = LAM_PINF
          delta = 0._R64P
          t = 0._R64P
          t2 = 0._R64P
          insamp = 1._R64P/DBLE(this%m_nsamp)
          if(profiling) then
               call qpctimer_start(qpctimer,ifail)
               if(ifail == 0) then
                    write(ERROR_UNIT,*) "cosine_signal: qpctimer_start failed to query performance frequency counter!!"
               end if
          end if
           ! Create Jones vector field.
          do i = 1_I32P, this%m_nsamp
              this%m_jvec(i) = JonesVector_t(h(i),v(i))
          end do
          do i = 1_I32P, this%m_nsamp
               this%m_initime = this%m_initime+this%m_tstep
               delta = DBLE(i)*insamp
               t = this%m_initime*delta
               t2 = t-r(i)/LAM_c
               this%m_phi(i) = LAM_2PI*this%m_freq*t2
               !this%m_amp(i) = this%m_amp(i)/this%m_r(i)
               this%m_E(i) = DCMPLX(DCOS(this%m_phi(i)),DSIN(this%m_phi(i)))
               !tmp(i) = this%m_E(i) ! copy signal components to temporaray arary.
               !this%m_E(i) = this%m_E(i)*this%m_amp(i)
          end do
           ! Compute A(phi,theta)/r - magnitude of electric field E
          call vcmag(this%m_E,this%m_amp)
          ! Create signal complex components
          do i = 1_I32P, this%m_nsamp
               this%m_initime = this%m_initime+this%m_tstep
               delta = DBLE(i)*insamp
               t = this%m_initime*delta
               ct(i) = t ! to be used for creation canonical form
               this%m_phi(i) = LAM_2PI*this%m_freq*t
               tmp(i) = DCMPLX(DCOS(this%m_phi(i)),DSIN(this%m_phi))
          end do
           ! Compute natural envelope
          call vcmag(tmp,this%m_nenvp)
          tc = 0._R64P
          ts = 0._R64P
          ! Create canonical representation
          do i = 1_I32P, this%m_nsamp
               tc = this%m_nenvp(i)*DCOS(this%m_phi(i))   ! gc(t)
               ts = this%m_nenvp(i)*DSIN(this%m_phi(i))   ! gs(t)
               this%m_cenvp(i) = DCMPLX(tc,ts)
               this%m_canform(i) = DCMPLX(tc*DCOS(ct(i)),ts*DSIN(ct(i)))
               this%m_basform(i) = DREAL(this%m_canform(i)) ! base form
          end do
          ! Compute time-average power density i.e. 0.5xExE*/n
          do i = 1, this%m_nsamp
              this%m_S(i) =  0.5_R64P*this%m_E(i)*DCONJG(this%m_E(i))*0.00265251989389920424403183023873_R64P
                              
          end do
         if(profiling) then
                if(ifail == 0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail == .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "cosine_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "cosine_signal: Unable to read performance counter -- fatal!!"
                end if
          end if            
    end subroutine cosine_signal     
         

    !======================================================60
    !  @subroutine: phase_noise_cosine_signal                        
    !              Physical representation of Cosine
    !              Signal.
    !              This signal is corrupted by phase additive
    !              noise.       
    !======================================================60     
     subroutine phase_noise_cosine_signal(this,ierr,dur,initime, &
                            sinterv,cfreq,envfreq,tstep,r,phinoise, &
                            logging,verbose,filename,append,profiling,qpctimer )
          use mod_print_error,  only : handle_fatal_memory_error ,  &
                                        stderr
          use mod_constants,    only : LAM_ZR8,LAM_PINF,LAM_2PI,LAM_c                    
          class(Cosine_Signal_t),   intent(inout)    :: this
          integer(I32P),            intent(inout)    :: ierr
          real(R64P),               intent(in)       :: dur,initime,sinterv, &
                                                        cfreq,envfreq,tstep
          real(R64P), dimension(:), intent(in)       :: r
!DIR$     ASSUME_ALIGNED r:64
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:64,v:64
          real(R64P), dimension(:),    intent(in)    :: phinoise
!DIR$     ASSUME_ALIGNED phinoise:64
          logical(I32P),               intent(in)    :: logging,verbose
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          character(len=256) :: emsg
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          integer(I32P) :: i,aerr
          real(R64P)    :: insamp,delta,t,t2,tc,ts,tn
          complex(R64P), allocatable, dimension(:) :: tmp
          real(R64P), allocatable, dimension(:) :: ct
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: ct
          ! Start of executable ststemetns
          if(ierr < 0_I32P) ierr = 0_I32P
          if(dur  < LAM_ZR8) then
              ierr = -1_I32P
              return
          end if
          this%m_dur     = dur
          this%m_initime = initime
          this%m_sinterv = sinterv
          this%m_cfreq   = cfreq
          this%m_envfreq = envfreq
          this%m_tstep   = tstep
          associate(n=>this%m_nsamp)
               allocate(tmp(n),    &
                        ct(n),     &
                        STAT=aerr, &
                        ERRMSG=emsg )
          end associate
          if(aerr /= 0) then
              call handle_fatal_memory_error(logging,verbose,append,filename,   &
                                             "logger:512 --> mod_cosine_signal/phase_noise_cosine_signal: Memory Allocation Failure!"   , &
                                             "mod_cosine_signal/phase_noise_cosine_signal:512 -- Memory Allocation Failure"  , &
                                             emsg,__LINE__ )
          end if
          tmp = CMPLX(LAM_PINF,LAM_PINF,R64P)
          ct  = LAM_PINF
          delta = 0._R64P
          t = 0._R64P
          t2 = 0._R64P
          tn =  0._R64P
          insamp = 1._R64P/DBLE(this%m_nsamp)
          if(profiling) then
               call qpctimer_start(qpctimer,ifail)
               if(ifail == 0) then
                    write(ERROR_UNIT,*) "phase_noise_cosine_signal: qpctimer_start failed to query performance frequency counter!!"
               end if
          end if
           ! Create Jones vector field.
          do i = 1_I32P, this%m_nsamp
              this%m_jvec(i) = JonesVector_t(h(i),v(i))
          end do
         do i = 1_I32P, this%m_nsamp
               this%m_initime = this%m_initime+this%m_tstep
               delta = DBLE(i)*insamp
               t = this%m_initime*delta
               t2 = t-r(i)/LAM_c
               tn = t2 + phinoise(i)
               this%m_phi(i) = LAM_2PI*this%m_freq*tn
               !this%m_amp(i) = this%m_amp(i)/this%m_r(i)
               this%m_E(i) = DCMPLX(DCOS(this%m_phi(i)),DSIN(this%m_phi(i)))
               !tmp(i) = this%m_E(i) ! copy signal components to temporaray arary.
               !this%m_E(i) = this%m_E(i)*this%m_amp(i)
         end do
            ! Compute A(phi,theta)/r - magnitude of electric field E
          call vcmag(this%m_E,this%m_amp)
          ! Create signal complex components
          do i = 1_I32P, this%m_nsamp
               this%m_initime = this%m_initime+this%m_tstep
               delta = DBLE(i)*insamp
               t = this%m_initime*delta
               tn = t+phinoise(i)
               ct(i) = tn ! to be used for creation canonical form
               this%m_phi(i) = LAM_2PI*this%m_freq*tn
               tmp(i) = DCMPLX(DCOS(this%m_phi(i)),DSIN(this%m_phi))
          end do
           ! Compute natural envelope
          call vcmag(tmp,this%m_nenvp)
          tc = 0._R64P
          ts = 0._R64P
          ! Create canonical representation
          do i = 1_I32P, this%m_nsamp
               tc = this%m_nenvp(i)*DCOS(this%m_phi(i))   ! gc(t)
               ts = this%m_nenvp(i)*DSIN(this%m_phi(i))   ! gs(t)
               this%m_cenvp(i) = DCMPLX(tc,ts)
               this%m_canform(i) = DCMPLX(tc*DCOS(ct(i)),ts*DSIN(ct(i)))
               this%m_basform(i) = DREAL(this%m_canform(i)) ! base form
          end do
          ! Compute time-average power density i.e. 0.5xExE*/n
          do i = 1, this%m_nsamp
              this%m_S(i) =  0.5_R64P*this%m_E(i)*DCONJG(this%m_E(i))*0.00265251989389920424403183023873_R64P
                              
          end do
         if(profiling) then
                if(ifail == 0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail == .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "phase_noise_cosine_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "phase_noise_cosine_signal: Unable to read performance counter -- fatal!!"
                end if
          end if            
     end subroutine phase_noise_cosine_signal   
          
    !======================================================60
    !  @subroutine: additive_noise_cosine_signal                        
    !              Physical representation of Cosine
    !              signal.
    !              This signal is corrupted by phase additive
    !              noise and by background additive noise.      
    !======================================================60 
    subroutine additive_noise_cosine_signal(this,ierr,dur,initime, &
                            sinterv,cfreq,envfreq,tstep,r,h,v,phinoise,bnoise, &
                            logging,verbose,filename,append,profiling,qpctimer )                        
          use mod_print_error,  only : handle_fatal_memory_error ,  &
                                        stderr
          use mod_constants,    only : LAM_ZR8,LAM_PINF,LAM_2PI,LAM_c 
                             
          class(Cosine_Signal_t),   intent(inout)    :: this
          integer(I32P),            intent(inout)    :: ierr
          real(R64P),               intent(in)       :: dur,initime,sinterv, &
                                                        cfreq,envfreq,tstep
          real(R64P), dimension(:), intent(in)       :: r
!DIR$     ASSUME_ALIGNED r:64
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:64,v:64
          real(R64P), dimension(:),    intent(in)    :: phinoise,bnoise
!DIR$     ASSUME_ALIGNED phinoise:64,bnoise:64
          logical(I32P),               intent(in)    :: logging,verbose
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          character(len=256) :: emsg
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          integer(I32P) :: i,aerr
          real(R64P)    :: insamp,delta,t,t2,tc,ts,tn
          complex(R64P), allocatable, dimension(:) :: tmp
          real(R64P), allocatable, dimension(:) :: ct
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: ct
          ! Start of executable ststemetns
          if(ierr < 0_I32P) ierr = 0_I32P
          if(dur  < LAM_ZR8) then
              ierr = -1_I32P
              return
          end if
          this%m_dur     = dur
          this%m_initime = initime
          this%m_sinterv = sinterv
          this%m_cfreq   = cfreq
          this%m_envfreq = envfreq
          this%m_tstep   = tstep
          associate(n=>this%m_nsamp)
               allocate(tmp(n),    &
                        ct(n),     &
                        STAT=aerr, &
                        ERRMSG=emsg )
          end associate
          if(aerr /= 0) then
              call handle_fatal_memory_error(logging,verbose,append,filename,   &
                                             "logger:643 --> mod_cosine_signal/additive_noise_cosine_signal: Memory Allocation Failure!"   , &
                                             "mod_cosine_signal/additive_noise_cosine_signal:643 -- Memory Allocation Failure"  , &
                                             emsg,__LINE__ )
          end if
          tmp = CMPLX(LAM_PINF,LAM_PINF,R64P)
          ct  = LAM_PINF
          delta = 0._R64P
          t = 0._R64P
          t2 = 0._R64P
          tn =  0._R64P
          insamp = 1._R64P/DBLE(this%m_nsamp)
          if(profiling) then
               call qpctimer_start(qpctimer,ifail)
               if(ifail == 0) then
                    write(ERROR_UNIT,*) "additive_noise_cosine_signal: qpctimer_start failed to query performance frequency counter!!"
               end if
          end if
           ! Create Jones vector field.
          do i = 1_I32P, this%m_nsamp
              this%m_jvec(i) = JonesVector_t(h(i),v(i))
          end do
         do i = 1_I32P, this%m_nsamp
               this%m_initime = this%m_initime+this%m_tstep
               delta = DBLE(i)*insamp
               t = this%m_initime*delta
               t2 = t-r(i)/LAM_c
               tn = t2 + phinoise(i)
               this%m_phi(i) = LAM_2PI*this%m_freq*tn
               !this%m_amp(i) = this%m_amp(i)/this%m_r(i)
               this%m_E(i) = DCMPLX(bnoise(i)+DCOS(this%m_phi(i)),bnoise(i)+DSIN(this%m_phi(i)))
               !tmp(i) = this%m_E(i) ! copy signal components to temporaray arary.
               !this%m_E(i) = this%m_E(i)*this%m_amp(i)
         end do
            ! Compute A(phi,theta)/r - magnitude of electric field E
          call vcmag(this%m_E,this%m_amp)
          ! Create signal complex components
          do i = 1_I32P, this%m_nsamp
               this%m_initime = this%m_initime+this%m_tstep
               delta = DBLE(i)*insamp
               t = this%m_initime*delta
               tn = t+phinoise(i)
               ct(i) = tn ! to be used for creation canonical form
               this%m_phi(i) = LAM_2PI*this%m_freq*tn
               tmp(i) = DCMPLX(bnoise(i)+DCOS(this%m_phi(i)),bnoise(i)+DSIN(this%m_phi))
          end do
           ! Compute natural envelope
          call vcmag(tmp,this%m_nenvp)
          tc = 0._R64P
          ts = 0._R64P
          ! Create canonical representation
          do i = 1_I32P, this%m_nsamp
               tc = this%m_nenvp(i)*DCOS(this%m_phi(i))   ! gc(t)
               ts = this%m_nenvp(i)*DSIN(this%m_phi(i))   ! gs(t)
               this%m_cenvp(i) = DCMPLX(tc,ts)
               this%m_canform(i) = DCMPLX(tc*DCOS(ct(i)),ts*DSIN(ct(i)))
               this%m_basform(i) = DREAL(this%m_canform(i)) ! base form
          end do
          ! Compute time-average power density i.e. 0.5xExE*/n
          do i = 1, this%m_nsamp
              this%m_S(i) =  0.5_R64P*this%m_E(i)*DCONJG(this%m_E(i))*0.00265251989389920424403183023873_R64P
                              
          end do
         if(profiling) then
                if(ifail == 0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail == .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "additive_noise_cosine_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "additive_noise_cosine_signal: Unable to read performance counter -- fatal!!"
                end if
          end if            
    end subroutine additive_noise_cosine_signal
  
    
    !======================================================60
    ! ************* Getter pure functions ****************
    !======================================================60
    
    pure function get_name(this) result(name)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          character(len=64) :: name
          ! Start of executable statements
          name = this%m_name
    end function
    
    pure function get_pid(this) result(pid)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: pid
          ! Start of executable satteements
          pid = this%m_pid
    end function
    
    pure function get_nsamp(this) result(nsamp)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nsamp
          ! Start of executable statements
          nsamp = this%m_nsamp
    end function
    
    pure function get_initime(this) result(initime)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: initime
          ! Start of executable statements
          initime = this%m_initime
    end function
    
    pure function get_sinterv(this) result(sinterv)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: sinterv
          ! Start of executable statements
          sinterv = this%m_sinterv
    end function
    
    pure function get_cfreq(this) result(cfreq)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: cfreq
          ! Start of executable statements
          cfreq = this%m_cfreq
    end function
    
    pure function get_envfreq(this) result(envfreq)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: envfreq
          ! Start of executable sattements
          envfreq = this%m_envfreq
    end function
    
    pure function get_tstep(this) result(tstep)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: tstep
          ! Start of executable statements
          tstep = this%m_tstep
    end function
    
 
   
    !======================================================60
    ! sbroutine: dphi_dt
    !            Phase signal time derivative
    !======================================================60
    subroutine dphi_dt(this,dphi,sfac)
          use mod_constants, only : LAM_MEPS8
          class(CosineSignal_t),    intent(in)     :: this
          real(R64P), dimension(:), intent(out)    :: dphi
          integer(I64P),            intent(inout)  :: sfac ! scaling factor up to 16 digits of precision
          ! Locals
          integer(I64P) :: i,
          real(R64P)    :: eps,tmp,isfac  ! eps**0.3
          ! Start of executable statements
          if(sfac.LE.0) then
              sfac = 1000000000000000_I64P
          end if
          isfac = 1._R64P/DBLE(sfac)
          eps = LAM_MEPS8**0.333333333333333333333333_R64P   
          do i = 2, this%m_nsamp-1
              tmp = this%m_phi(i+1)-this%m_phi(i-1) ! Consider using CADNA to test for cancellation errors
              dphi(i) = this%m_phi(i+1)-this%m_phi(i-1)/ &
                        (2._R64P*eps*DMAX1(DABS(tmp),isfac)*DSIGN(tmp,tmp))
          end do
    end subroutine    
          
    !======================================================
    !  subroutine: analytic signal
    !              Using this formula: 
    !======================================================
    subroutine analytic_signal(this,asig,tmplen,wlen,iplen,profiling,qpctimer)
          use mod_fftsg, only : rdft
         
          class(CosineSignal_t),    intent(inout) :: this
          complex(R64P), dimension(:), intent(out)   :: asig ! analytic signal
          integer(I32P),               intent(in)    :: tmplen ! nsamp-1
          integer(I32P),               intent(in)    :: wlen ! nsamp/2-1
          integer(I32P),               intent(in)    :: iplen ! 2+2**(int(log(n/2+0.5)/log(2.0))/2)
          logical(I32P),               intent(in)    :: profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          integer(I32P) :: i
          real(R64P),    dimension(0:tmplen) :: tmp,tmp2  
          real(R64P),    dimension(0:wlen)   :: w
          integer(I32P), dimension(0:iplen)  :: ip
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp
!DIR$     ATTRIBUTES ALIGN : 64 :: tmp2
!DIR$     ATTRIBUTES ALIGN : 64 :: w
          ! Start of executable statements
          ! Copy real part of canonical representation
          if(profiling) then
             call qpctimer_start(qpctimer,ifail)
             if(ifail.EQ.0) then
                 write(stderr,*) "analytic_signal:859, qpctimer_start failed to query performance frequency counter!!"
             end if
          end if
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))          
          do i = 0, tmplen
              tmp(i) = DREAL(this%m_canform(i))   ! Copy of real part
          end do
          ! Real FFT
          ip(0) = 0
          call rdft(tmplen,1,tmp,ip,w)
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))          
          do i = 0, tmplen
              tmp2(i) = tmp(i)+DSIGN(tmp(i),tmp(i))*tmp(i)  ! Construct argument to Inverse FFT
          end do
          call rdft(tmplen,-1,tmp2,ip,w)  ! Inverse FFT 
          ! Construct analytic signal content
          do  i = 0, tmplen
              asig(i) = DCMPLX(DREAL(this%m_conform(i)),tmp2(i))
          end do
           if(profiling) then
               if(ifail.EQ.0) then
                   call qpctimer_stop(qpctimer,ifail)
                   call qpctimer_delta(qpctimer,bfail)
                   if(bfail .EQ. .false.) then
                        call qpctimer_print(qpctimer)
                   else
                        write(stderr,*) "analytic_signal:884 -- qpctimer_delta: failed to compute delta measurement!!"
                   end if  
               else
                       write(stderr,*) "analytic_signal:884 -- Unable to read performance counter -- fatal!!"
               end if
           end if
           
    end subroutine  analytic_signal
    
      !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
        
          class(CosineSignal_t), intent(in) :: this
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
         
          class(CosineSignal_t), intent(in) :: this
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
    ! subroutine: dbg_info
    !             Print unformatted stream to screen
    !======================================================60
    subroutine dbg_info(this)
         
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          call DATE_AND_TIME(date=dstr,time=tstr)
          print*, "======================================================"
          print*, "   ***Printing components of CosineSignal_t***"
          print*, "   printing subroutine called at: " , &
                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                     stime(1:2),":",stime(3:4),":",stime(5:6)
          print*, "           Dumping scalar components "
          print*, "======================================================"
          print*, "Name:              ", this%m_name
          print*, "Signal ID:          ",this%m_sid
          print*, "Number of samples: ", this%m_nsamp
          print*, "Pulse duration:    ", this%m_dur
          print*, "Pulse initial time:", this%m_initime
          print*, "Sample interval:   ", this%m_sinterv
          print*, "Carrier frequency: ", this%m_cfreq
          print*, "Envelope frequency:", this%m_envfreq
          print*, "Time step:         ", this%m_tstep
          print*, "           Dumping array components "
          print*, "====================================================="
         
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
         
          print*, "           End                                       "
    end subroutine  dbg_info
    
    !============================================
    !  Operator assignment (=)
    !============================================ 
    subroutine copy_assign(lhs,rhs)
          class(CosineSignal_t),    intent(inout) :: lhs
          class(CosineSignal_t),    intent(in)    :: rhs
          ! Start of executable statements
          this%m_name    = rhs%m_name
          this%m_sid     = rhs%m_sid
          this%m_nsamp   = rhs%m_nsamp
          this%m_dur     = rhs%m_dur
          this%m_initime = rhs%m_initime
          this%m_sinterv = rhs%m_sinterv
          this%m_cfreq   = rhs%m_cfreq
          this%m_envfreq = rhs%m_envfreq
          this%m_tstep   = rhs%m_tstep
          this%m_jvec    = rhs%m_jvec
          this%m_E       = rhs%m_E
          this%m_nenvp   = rhs%m_nenvp
          this%m_phi     = rhs%m_phi
          this%m_canform = rhs%m_canform
          this%m_basform = rhs%m_basform
          this%m_cenvp   = rhs%m_cenvp
          this%m_amp     = rhs%m_amp
          this%m_S       = rhs%m_S
    end subroutine copy_assign
    
                          
end module mod_cosine_signal