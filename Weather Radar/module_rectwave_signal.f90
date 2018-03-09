
#include "Config.fpp"


module mod_rectw_signal

  !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_rectw_signal'
 !          
 !          Purpose:
 !                  Rect-Wave Signal representation
 !                  approximated by Fourier series.
 !                   
 !                     
 !          History:
 !                        Date: 28-08-2017
 !                        Time: 14:23 GMT+2
 !          Modified:
 !                    Bernard Gingold on: 07-03-2018, 18:33 GMT+2
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
    use module_kinds,        only : I32P, R64P
    
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
    integer(I32P), parameter, public :: MOD_RECTW_SIGNAL_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_RECTW_SIGNAL_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_RECTW_SIGNAL_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_RECTW_SIGNAL_FULLVER = 1000*MOD_RECTW_SIGNAL_MAJOR+100*MOD_RECTW_SIGNAL_MINOR+ &
                                                                   10*MOD_RECTW_SIGNAL_MICRO
    
    ! Module/file creation date
    character(*),  parameter, public :: MOD_RECTW_SIGNAL_CREATE_DATE = "28-08-2017 14:59 +00200 (MON 28 AUG 2017 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_RECTW_SIGNAL_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_RECTW_SIGNAL_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_RECTW_SIGNAL_DESCRIPT = "Rect-wave signal approximated by Fourier series."
    
    !======================================================60
    !  Type: RectWSignal_t
    !======================================================60
    
    type, public  :: RectWSignal_t
        
         private
         
         ! Signal name
         character(len=64) :: m_name
         
         ! Signal ID
         integer(I32P)     :: m_sid
         
         ! Number of samples
         integer(I32P)     :: m_nsamp
         
         ! Number of Fourier sinusoids
         integer(I32P)     :: maxk
         
         ! Signal duration i.e. T
         real(R64P)        :: m_dur
         
         ! Rect Wave initial time-point
         real(R64P)        :: m_sinit
         
         ! Rect wave time-step increment
         real(R64P)        :: m_stsinc
         
         
         
         ! Jones vector field
         type(JonesVector_t), allocatable, dimension(:), public :: m_jvec
         
          ! Sine components of k sinusoids
         real(R64P), allocatable, dimension(:,:),        public :: m_scomp
!DIR$    ATTRIBUTES ALIGN : 64 :: m_scomp  
         
         ! Cosine components of k cosinusoids
         real(R64P), allocatable, dimension(:,:),        public :: m_ccomp
!DIR$    ATTRIBUTES ALIGN : 64 :: m_ccomp
         
         ! Time argument
         real(R64P), allocatable, dimension(:),          public   :: m_wt
!DIR$    ATTRIBUTES ALIGN : 64 :: m_wct
         
         ! Signal natural envelope
         real(R64P), allocatable, dimension(:),          public   :: m_nenvp
!DIR$    ATTRIBUTES ALIGN : 64 :: m_nenvp
         
         ! Signal squarewave approximated by k sinusoids
         real(R64P), allocatable, dimension(:),          public   :: m_rectw
!DIR$    ATTRIBUTES ALIGN : 64 :: m_squarewave
         
         ! SquareWave complex representation
         real(R64P), allocatable, dimension(:),          public   :: m_cform
!DIR$    ATTRIBUTES ALIGN : 64 :: m_cform
         
         ! Electric field
        complex(R64P), allocatable, dimension(:),        public   :: m_E
!DIR$   ATTRIBUTES ALIGN : 64 :: m_E
        
         ! Signal amplitude  A(phi,theta)/r used for calculation electric field
         ! far from emmiter(antenna)
         real(R64P), allocatable, dimension(:),         public   :: m_amp
!DIR$   ATTRIBUTES ALIGN : 64 :: m_amp
         
 ! Time-averaged power density
      
         complex(R64P), allocatable, dimension(:),      public :: m_S
!DIR$   ATTRIBUTES ALIGN : 64 :: m_S
         
 
         
         contains
         
        ! Construct SquareWave signal approximated by its
        ! Fourier series
         procedure, pass(this), public :: rectwave_signal
    
       
        
        
        
       
        
       
        ! Creates signal with noise modulated phase
        ! coupled with background additive noise.
        procedure, pass(this), public :: noisy_rectwave_signal
        
        
         
        !==========================================52
        !   Getter pure functions
        !==========================================52
         
         procedure, pass(this), public :: get_name
         
         procedure, pass(this), public :: get_sid
         
         procedure, pass(this), public :: get_nsamp
         
         procedure, pass(this), public :: get_maxk
         
         procedure, pass(this), public :: get_dur
         
         procedure, pass(this), public :: get_sinit
         
         procedure, pass(this), public :: get_stsinc
         
         
        
         
        !==================================================60
        ! Computational subroutines
        !==================================================60
        
        procedure, pass(this), public :: dphi_dt
        
        !==============================================56
        ! write/read subroutines
        !==============================================56
        
        procedure, nopass,     public :: read_state
        
        procedure, nopass,     public :: write_state
        
        !===============================================
        !  Class helper procedures
        !===============================================
        procedure, pass(this), public :: dbg_info
        !===============================================
        !   Generic operators
        !===============================================
        procedure, public :: copy_assign
        
        generic :: assignment (=) => copy_assign
        
    end type RectWSignal_t
         
   interface RectWSignal_t
         procedure :: constructor
   end interface RectWSignal_t
         
   contains
    
    !======================================================60
    ! @function:  constructor
    !             Memory allocation and
    !             default initialization of array and scalar
    !             member variables.
    !======================================================60
    type(RectWSignal_t) function constructor(name,sid,nsamp,maxk, verbose,  &
                                            logging,filename,append)
          use mod_print_error,  only : handle_fatal_memory_error
          use mod_constants,    only : LAM_PINF, LAM_MINSAMP,  &
                                       LAM_MINK
          character(len=*),     intent(in)       :: name
          integer(I32P),        intent(in)       :: sid
          integer(I32P),        intent(inout)    :: nsamp,maxk
          logical(I32P),        intent(in)       :: logging , verbose
          character(len=*),     intent(in)       :: filename
          logical(I32P),        intent(in)       :: append
          ! Locals
         
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          ! Start of executable statements
          ! Allow this for error correction.
          if(nsamp < LAM_MINSAMP) then
              nsamp = LAM_MINSAMP
          end if
          if(maxk < LAM_MINK) then
              maxk = LAM_MINK
          end if
          ! Begin construction
          constructor%m_name   = name
          constructor%m_sid    = sid
          constructor%m_nsamp  = nsamp
          constructor%m_maxk   = maxk
          constructor%m_dur    = LAM_PINF
          constructor%m_sinit  = LAM_PINF
          constructor%m_stsinc = LAM_PINF
          
          associate(k=>constructor%m_maxk,       &
                    n=>constructor%m_nsamp)
              allocate(constructor%m_jvec(n),    &
                       constructor%m_scomp(k,n), &
                       constructor%m_ccomp(k,n), &
                       constructor%m_wt(n),      &
                       constructor%m_nenvp(n),   &
                       constructor%m_rectw(n),   &
                       constructor%m_cform(n),   &
                       constructor%m_E(n),       &
                       constructor%m_amp(n),     &
                       constructor%m_S(n),       &
                       STAT=aerr,         &
                       ERRMSG=emsg)
          end associate
          if(aerr /= 0) then
             
              call handle_fatal_memory_error(logging,verbose,append,filename,  &
                                             "logger:311 --> mod_rectwave_signal/constructor: Memory Allocation Failure!!"  , &
                                             "mod_rectwave_signal/constructor:311 --  Memory Allocation Failure!!" , &
                                             emsg,__LINE__ )
          end if
          constructor%m_jvec  = JonesVector()
          constructor%m_scomp = LAM_PINF
          constructor%m_ccomp = LAM_PINF
          constructor%m_wt    = LAM_PINF
          constructor%m_nenvp = LAM_PINF
          constructor%m_rectw = LAM_PINF
          constructor%m_cform = CMPLX(LAM_PINF,LAM_PINF,R64P)
          constructor%m_E     = CMPLX(LAM_PINF,LAM_PINF,R64P)
          constructor%m_amp   = LAM_PINF
          constructor%m_S     = CMPLX(LAM_PINF,LAM_PINF,R64P)
         
         
    end function constructor
                              
    !======================================================60
    !  subroutine: create_signal                        
    !              Physical representation of Rect wave
    !              approximated by K sinusoids (Fourier-
    !              series)
    !======================================================60    
    subroutine rectwave_signal(this,ierr,dur,dc,sinit,stsinc,r,h,v, &
                             logging,verbose,append,fname,profiling,qpctimer )
           use mod_print_error,  only : handle_fatal_memory_error ,  &
                                        stderr
           use mod_constants,    only : LAM_ZR8
           class(RectWSignal_t),        intent(inout) :: this  
           integer(I32P),               intent(inout) :: ierr
           real(R64P),                  intent(in)    :: dur,dc,sinit,stsinc
           real(R64P), dimension(:),    intent(in)    :: r 
!DIR$     ASSUME_ALIGNED r:64
           complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:64,v:64
           logical(I32P),               intent(in)    :: profiling,logging, &
                                                         verbose,append
           character(len=*),            intent(in)    :: fname
           type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
           integer(I32P)      :: k,j,aerr
           character(len=256) :: emsg
           integer(BOOL)      :: ifail
           logical(I32P)      :: bfail
           real(R64P)         :: insamp,t,t2,delta,sa,ca,idur,ratio
           real(R64P), allocatable, dimension(:) :: tcos,ts,tc
!DIR$     ATTRIBUTES ALIGN : 64 :: tcos
!DIR$     ATTRIBUTES ALIGN : 64 :: ts
!DIR$     ATTRIBUTES ALIGN : 64 :: tc 
          if(ierr < 0_I32P) ierr = 0_I32P
          if(dur  < LAM_ZR8) then
              ierr = -1_I32P
              return
          end if
          this%m_dur    = dur
          this%m_sinit  = sinit
          this%m_stsinc = stsinc
          associate(n=>this%m_nsamp)
              allocate(
                       tcos(n),           &
                       tc(n),             &
                       ts(n),             &
                       STAT=aerr,         &
                       ERRMSG=emsg )
          end associate
          if(aerr /= 0) then
              
              call handle_fatal_memory_error( logging,verbose,append,fname,   &
                                              "logger:391,--> mod_rectwave_signal/rectwave_signal: Memory Allocation Failure !!", &
                                              "mod_rectwave_signal/rectwave_signal:391 -- Memory Allocation Failure!" ,           &
                                              emsg,__LINE__ )
          end if                                    
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail == 0) then
                  write(stderr,*) "mod_rectwave_signal/rectwave_signal: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
          ! Create Jones vector field
          do j = 1_I32P, this%m_nsamp
              this%m_jvec(j) = JonesVector_t(h(j),v(j))
          end do
           ! Create sine and cosine Fourier series components
          insamp = 1._R64P/DBLE(this%m_nsamp)
          idur = 1._R64P/this%m_dur
          do k = 1, this%m_maxk
              do j = 1_I32P, this%m_nsamp
                  this%m_sinit = this%m_sinit+this%m_stsinc
                  delta = DBLE(j)*insamp
                  t = this%m_sinit*delta
                  sa = LAM_PI*k*dc*idur
                  ca = LAM_2PI*k*idur
                  this%m_scomp(k,j) = (2._R64P/k*LAM_PI)*DSIN(sa)*DCOS(ca*t)
                  this%m_ccomp(k,j) = (2._R64P/k*LAM_PI)*DCOS(sa)*DSIN(ca*t)
              end do
          end do
          ! Do summation over rows
          this%m_rectw = SUM(this%m_scomp,dim=2)
          tcos = SUM(this%m_ccomp,dim=2)
          ! Create Rect wave signal (final approximation) and
          ! create signal complex representation
          ratio = dc/dur
          do j = 1_I32P, this%m_nsamp
              this%m_sinit = this%m_sinit+this%m_stsinc
              delta = DBLE(j)*insamp
              t = this%m_sinit*delta
              this%m_wt(j) = t
              this%m_rectw(j) = ratio+this%m_rectw(j)
              tcos(j) = ratio+tcos(j)
              this%m_cform(j) = DCMPLX(tcos(j),this%m_rectw(j))
          end do
           ! Compute natural envelope
          call vcmag(this%m_cform,this%m_nenvp)
           ! Create electrical field far from the emitter.
          do k = 1, this%m_maxk
              do j = 1, this%m_nsamp
                  t2 = this%m_wt(j)-r(j)/LAM_c
                  sa = LAM_PI*k*dc*idur
                  ca = LAM_2PI*k*idur
                  this%m_scomp(k,j) = (2._R64P/k*LAM_PI)*DSIN(sa)*DCOS(ca*t2)
                  this%m_ccomp(k,j) = (2._R64P/k*LAM_PI)*DCOS(sa)*DSIN(ca*t2)
              end do
          end do
            ! Do summation over the rows
          tc = SUM(this%m_ccomp,dim=2)
          ts = SUM(this%m_scomp,dim=2)
          do j = 1, this%m_nsamp
              tc(j) = ratio+tc(j)
              ts(j) = ratio+ts(j)
              this%m_E(j) = DCMPLX(tc(j),ts(j))
          end do
          ! Compute A(phi,theta)/r - magnitude of electric field E
          call vcmag(this%m_E,this%m_amp)
            ! Compute time-average power density i.e. 0.5xExE*/n
          do j = 1, this%m_nsamp
              this%m_S(j) =  LAM_HR64P*this%m_E(j)*DCONJG(this%m_E(j))*0.0026525198938992_R64P
          end do
          if(profiling) then
                if(ifail == 0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail == .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_rectw_signal/rectwave_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_rectw_signal/rectwave_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
    end subroutine  rectwave_signal    
       
         
        
         
         
         
        
         
          
         
    
                             
    !======================================================60
    !  subroutine: noisy_rectwave_signal                        
    !              Physical representation of Rect wave
    !              approximated by K sinusoids (Fourier-
    !              series)
    !              This signal is corrupted by background 
    !              additive noise.     
    !======================================================60 
    subroutine noisy_rectwave_signal(this,ierr, dur,dc, &
                                     sinit,stsinc,r,bnoise,h,v,logging, &
                                     filename,append,dbg,profiling,qpctimer )
          use mod_print_error,   only : handle_fatal_memory_error,  &
                                        stderr
          use mod_constants,     only : LAM_ZR8
          class(RectWSignal_t),        intent(inout) :: this
          integer(I32P),               intent(inout) :: ierr
          real(R64P),                  intent(in)    :: dur,dc,sinit,stsinc
          real(R64P), dimension(:),    intent(in)    :: r,bnoise
!DIR$     ASSUME_ALIGNED r:64,bnoise:32          
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUME_ALIGNED h:64,v:32
          logical(I32P),               intent(in)    :: logging
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          
          character(len=256) :: emsg
          integer(I32P)      :: k,j,aerr
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail
          real(R64P)         :: insamp,t,t2,delta,sa,ca,idur,ratio
          real(R64P), allocatable, dimension(:) :: tcos,ts,tc
!DIR$     ATTRIBUTES ALIGN : 64 :: tcos
!DIR$     ATTRIBUTES ALIGN : 64 :: ts
!DIR$     ATTRIBUTES ALIGN : 64 :: tc
          ! Start of executable statements
          if(ierr < 0_I32P) ierr = 0_I32P
          if(dur.LT.LAM_ZR8) then
              ierr = -1_I32P
              return
          end if
          ! Begin construction
          
          this%m_dur    = dur
          this%m_sinit  = sinit
          this%m_stsinc = stsinc
          
          associate(n=>this%m_nsamp)
              allocate(
                       tcos(n),           &
                       tc(n),             &
                       ts(n),             &
                       STAT=aerr,         &
                       ERRMSG=emsg )
          end associate
          if(aerr /= 0) then
            
              call  handle_fatal_memory_error(logging,verbose,append,filename,   &
                                              "logger:532 --> mod_rectw_signal/noisy_rectwave_signal: Memory allocation failure!" , &
                                              "mod_rectw_signal/noisy_rectwave_signal:532 --  Memory allocation failure!"  , &
                                              emsg,__LINE__ )
          end if
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail == 0) then
                  write(stderr,*) "mod_rectw_signal/noisy_rectwave_signal: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
           ! Create Jones vector field
          do j = 1, this%m_nsamp
              this%m_jvec(j) = JonesVector_t(h(j),v(j))
          end do
           ! Create sine and cosine Fourier series components
          insamp = 1._R64P/DBLE(this%m_nsamp)
          idur = 1._R64P/this%m_dur
          do k = 1, this%m_maxk
              do j = 1, this%m_nsamp
                  this%m_sinit = this%m_sinit+this%m_stsinc
                  delta = DBLE(j)*insamp
                  t = this%m_sinit*delta
                  sa = LAM_PI*k*dc*idur
                  ca = LAM_2PI*k*idur
                  this%m_scomp(k,j) = (2._R64P/k*LAM_PI)*DSIN(sa)*DCOS(ca*t)
                  this%m_scomp(k,j) = bnoise(j)+this%m_scomp(k,j)
                  this%m_ccomp(k,j) = (2._R64P/k*LAM_PI)*DCOS(sa)*DSIN(ca*t)
                  this%m_ccomp(k,j) = bnoise(j)+this%m_ccomp(k,j)
              end do
          end do
          ! Do summation over rows
          this%m_rectw = SUM(this%m_scomp,dim=2)
          tcos = SUM(this%m_ccomp,dim=2)
          ! Create Rect wave signal (final approximation) and
          ! create signal complex representation
          ratio = dc/dur
          do j = 1, this%m_nsamp
              this%m_sinit = this%m_sinit+this%m_stsinc
              delta = DBLE(j)*insamp
              t = this%m_sinit*delta
              this%m_wt(j) = t
              this%m_rectw(j) = ratio+this%m_rectw(j)
              tcos(j) = ratio+tcos(j)
              this%m_cform(j) = DCMPLX(tcos(j),this%m_rectw(j))
          end do
          ! Compute natural envelope
          call vcmag(this%m_cform,this%m_nenvp)
           ! Create electrical field far from the emitter.
          do k = 1, this%m_maxk
              do j = 1, this%m_nsamp
                  t2 = this%m_wt(j)-r(j)/LAM_c
                  sa = LAM_PI*k*dc*idur
                  ca = LAM_2PI*k*idur
                  this%m_scomp(k,j) = (2._R64P/k*LAM_PI)*DSIN(sa)*DCOS(ca*t2)
                  this%m_scomp(k,j) = bnoise(j)+this%m_scomp(k,j)
                  this%m_ccomp(k,j) = (2._R64P/k*LAM_PI)*DCOS(sa)*DSIN(ca*t2)
                  this%m_ccomp(k,j) = bnoise(j)+this%m_ccomp(k,j)
              end do
          end do
          ! Do summation over the rows
          tc = SUM(this%m_ccomp,dim=2)
          ts = SUM(this%m_scomp,dim=2)
          do j = 1, this%m_nsamp
              tc(j) = ratio+tc(j)
              ts(j) = ratio+ts(j)
              this%m_E(j) = DCMPLX(tc(j),ts(j))
          end do
          ! Compute A(phi,theta)/r - magnitude of electric field E
          call vcmag(this%m_E,this%m_amp)
            ! Compute time-average power density i.e. 0.5xExE*/n
          do j = 1, this%m_nsamp
              this%m_S(j) =  LAM_HR64P*this%m_E(j)*DCONJG(this%m_E(j))*0.0026525198938992_R64P
          end do
          this%m_isbuilt = .true.
          if(profiling) then
                if(ifail == 0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail == .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_rectw_signal/noisy_rectwave_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_rectw_signal/noisy_rectwave_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
    end subroutine noisy_rectwave_signal
                                     
   
    
 
   
    !======================================================60
    ! Getter pure functions
    !======================================================60
    
    pure function get_name(this) result(name)
          implicit none
          class(RectWSignal_t), intent(in) :: this
          ! Locals
          character(len=64) :: name
          ! Start of executable statements
          name = this%m_name
    end function
    
    pure function get_sid(this) result(sid)
          implicit none
          class(RectWSignal_t), intent(in) :: this
          ! LOCALS
          integer(I32P) :: sid
          ! Start of executable statements
          sid = this%m_sid
    end function
    
    pure function get_nsamp(this) result(nsamp)
          implicit none
          class(RectWSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nsamp
          ! Start of executable statements
          nsamp = this%m_nsamp
    end function
    
    pure function get_maxk(this) result(maxk)
          implicit none
          class(RectWSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: maxk
          ! Start of executable statements
          maxk = this%m_maxk
    end function
    
    pure function get_dur(this) result(dur)
          implicit none
          class(RectWSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: dur
          ! Start of executable statements
          dur = this%m_dur
    end function
    
    pure function get_sinit(this) result(sinit)
          implicit none
          class(RectWSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: sinit
          ! Start of executable statements
          sinit = this%m_sinit
    end function
    
    pure function get_stsinc(this) result(stsinc)
          implicit none
          class(RectWSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: stsinc
          ! Start of executable statements
          stsinc = this%m_stsinc
    end function
    
   
   
  
    
   
    
   
    
    !======================================================60
    !    Computational procedures
    !======================================================60
    ! TODO:
    !        ! Consider using CADNA to test for cancellation errors
    subroutine dphi_dt(this,dphi,sfac)
          
          class(RectWSignal_t),     intent(in)    :: this
          real(R64P), dimension(:), intent(out)   :: dphi
          integer(I64P),            intent(inout) :: sfac
          ! Locals
          integer(I32P) :: i
          real(R64P)    :: eps,isfac,tmp
          ! Start of executable statements
          if(sfac.LE.LAM_IZER8) then
              sfac =  1000000000000000_I64P
          end if
          isfac = 1._R64P/DBLE(sfac)
          eps = LAM_MEPS8**0.3333333333333333333333333333_R64P
          do i = 2, this%m_nsamp-1
              tmp = this%m_wt(i+1)-this%m_wt(i-1)
              dphi(i) = this%m_wt(i+1)-this%m_wt(i-1) / &
                           (2._R64P*eps*DMAX1(DABS(tmp),isfac)*DSIGN(tmp,tmp))
          end do
    end subroutine
    
    !============================================
    !   Read/write procedures
    !============================================
       subroutine read_state(this,form,unit,ioerr)
        
          class(RectWSignal_t), intent(in) :: this
          character(len=*),     intent(in) :: form
          integer(I32P),        intent(in) :: unit
          integer(I32P),        intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine read_state
    
    subroutine write_state(this,form,unit,ioerr)
         
          class(RectWSignal_t), intent(in) :: this
          character(len=*),     intent(in) :: form
          integer(I32P),        intent(in) :: unit
          integer(I32P),        intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine write_state
      
    !======================================================60
    !  subroutine: dgb_info
    !======================================================60  
    subroutine dbg_info(this,verbose)
          implicit none
          class(RectWSignal_t),    intent(in) :: this
          logical(I32P),           intent(in) :: verbose
          ! Locals
          ! Start of executable statements
          call DATE_AND_TIME(date=dstr,time=tstr)
          print*, "======================================================"
          print*, "   ***Printing components of RectWSignal_t***"
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
          
          print*, "Initial time-point: ", this%m_sinit
          print*, "Time-step increment:", this%m_stsinc
          
          print*, "           Dumping array components  "
          print*, "======================================================"
          print*, "******************************************************"
         
          if(verbose) then
              print*, "Sine series components:  ", this%m_scomp
              print*, "Cosine series components:", this%m_ccomp
          end if
          print*, "Signal polarization:  ",   this%m_jvec
          print*, "Time (t):             ",   this%m_wt
          print*, "Natural Envelope:     ",   this%m_nenvp
          print*, "Rect-wave(t):         ",   this%m_rectw
          print*, "Complex form:         ",   this%m_cform
          print*, "Electric field:       ",   this%m_E
          print*, "Amplitude:            ",   this%m_amp
          print*, "Time-average power:   ",   this%m_S
    end subroutine
    
    !======================================================60
    !  subroutine: copy_assign
    !              Overloaded assignment (=)
    !======================================================60
    subroutine copy_assign(this,other)
          
          class(RectWSignal_t), intent(inout) :: this
          class(RectWSignal_t), intent(in)    :: other
          ! Start of executable statements
          this%m_name     = other%m_name 
          this%m_sid      = other%m_sid
          this%m_nsamp    = other%m_nsamp
          this%m_maxk     = other%m_maxk
          this%m_dur      = other%m_dur
          this%m_sinit    = other%m_sinit
          this%m_stsinc   = other%m_stsinc
          this%m_jvec     = other%m_jvec
          this%m_scomp    = other%m_scomp
          this%m_ccomp    = other%m_ccomp
          this%m_wt       = other%m_wt
          this%m_nenvp    = other%m_nenvp
          this%m_rectw    = other%m_rectw
          this%m_cform    = other%m_cform
          this%m_E        = other%m_E
          this%m_amp      = other%m_amp
          this%m_S        = other%m_S
   end subroutine copy_assign       
         
         
   
    
end module mod_rectw_signal