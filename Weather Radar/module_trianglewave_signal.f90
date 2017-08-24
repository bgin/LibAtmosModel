
module mod_trianglew_signal

    
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_trianglew_signal'
 !          
 !          Purpose:
 !                   Triangle Wave Signal
 !                   approximated by Fourier Series.
 !
 !                     
 !          History:
 !                        Date: 22-08-2017
 !                        Time: 07:25 GMT+2
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
    integer(I32P), parameter, public :: MOD_TRIANGLEW_SIGNAL_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_TRIANGLEW_SIGNAL_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_TRIANGLEW_SIGNAL_MICRO = 0
    
    ! Module/file full version
    integer(I32P), parameter, public :: MOD_TRIANGLEW_SIGNAL_FULLVER = 1000*MOD_TRIANGLEW_SIGNAL_MAJOR+100*MOD_TRIANGLEW_SIGNAL_MINOR+ &
                                                                       10*MOD_TRIANGLEW_SIGNAL_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_TRIANGLEW_SIGNAL_CREATE_DATE = "22-08-2017 07:25 +00200 (TUE 22 AUG 2017 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_TRIANGLEW_SIGNAL_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_TRIANGLEW_SIGNAL_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_TRIANGLEW_SIGNAL_DESCRIPT = "Fourier series approximation of triangle wave."
    
    !======================================================60
    !  Type: TriangleWSignal_t
    !======================================================60
    
    type :: TriangleWSignal_t
        
        private
        
        ! Signal name
        character(len=64) :: m_name
        
        ! Signal ID
        integer(I32P)     :: m_sid
        
        ! Number of signal samples
        integer(I32P)     :: m_nsamp
        
        ! Number of Fourier series
        integer(I32P)     :: m_maxk
        
        ! Signal duration
        real(R64P)        :: m_dur
        
        ! Triangle wave initial time (starting time-point)
        real(R64P)        :: m_sinit
        
        ! Triangle wave increment time-step
        real(R64P)        :: m_stsinc
        
        ! Triangle wave carrier frequency
        real(R64P)        :: m_freq
        
        ! List of constructors
        character(len=32), dimension(3) :: m_ctors
        
        ! Jones vector field
        type(JonesVector_t), allocatable, dimension(:) :: m_jvec
        
        ! Sine components of k sinusoids
        real(R64P), allocatable, dimension(:,:) :: m_scomp
!DIR$   ATTRIBUTES ALIGN : 32 :: m_scomp
        
        ! Cosine components of k sinusoids
        real(R64P), allocatable, dimension(:,:) :: m_ccomp
!DIR$   ATTRIBUTES ALIGN : 32 :: m_ccomp
        
        ! Triangle carrier frequency (f*t)
        real(R64P), allocatable, dimension(:)   :: m_wct
!DIR$   ATTRIBUTES ALIGN : 32 :: m_wct
        
        ! Signal natural envelope
        real(R64P), allocatable, dimension(:)   :: m_nenvp
!DIR$   ATTRIBUTES ALIGN : 32 :: m_nenvp
        
        ! Triangle wave aproximated by k 
        ! sinusoids
        real(R64P), allocatable, dimension(:)   :: m_triangle
!DIR$   ATTRIBUTES ALIGN : 32 :: m_triangle
        
        ! Triangle wave complex representation
        complex(R64P), allocatable, dimension(:) :: m_cform
!DIR$   ATTRIBUTES ALIGN : 32 :: m_cform
        
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
        ! No physical meaning
        procedure, pass(this), public :: default_signal
        
        ! Main signal constructor.
        ! Creates Triangle wave approximated by Fourier
        ! series.
        procedure, pass(this), public :: create_signal
        
        ! Copy-constructor , performs deep copy
        procedure, pass(this), public :: copy_signal
        
        ! Destroys signal
        procedure, pass(this), public :: destroy_signal
        
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
        
        procedure, pass(this), public :: get_freq
        
        procedure, pass(this), public :: get_ctors
        
        procedure, pass(this), public :: get_jvec
        
        procedure, pass(this), public :: get_scomp
        
        procedure, pass(this), public :: get_ccomp
        
        procedure, pass(this), public :: get_wct
        
        procedure, pass(this), public :: get_nenvp
        
        procedure, pass(this), public :: get_triangle
        
        procedure, pass(this), public :: get_cform
        
        procedure, pass(this), public :: get_E
        
        procedure, pass(this), public :: get_amp
        
        procedure, pass(this), public :: get_S
        
        procedure, pass(this), public :: get_isbuilt
        
        !==================================================60
        ! Computational subroutines
        !==================================================60
        
        procedure, pass(this), public :: dphi_dt
        
        !==================================================60
        !  write/read subroutines
        !==================================================60
        
        procedure, nopass, public :: to_screenu
        
    end type TriangleWSignal_t
        
    contains
    
    !======================================================60
    ! subroutine: default_signal
    !             default initialization (no physical meaning)
    !======================================================60
    subroutine default_signal(this,name,sid,nsamp,maxk,  &
                              logging,filename,append,dbg)
          implicit none
          class(TriangleWSignal_t), intent(inout) :: this
          character(len=*),         intent(in)    :: name
          integer(I32P),            intent(in)    :: sid
          integer(I32P),            intent(inout) :: nsamp,maxk
          logical(I32P),            intent(in)    :: logging
          character(len=*),         intent(in)    :: filename
          logical(I32P),            intent(in)    :: append,dbg
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: i,j,aerr
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call UsrMsg("logger:256, In->mod_trianglew_signal/default_signal: TriangleWSignal_t already initialized!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_trianglew_signal/default_signal:256, TriangleWSignal_t already initialized!)"
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
          if(maxk.LT.LAM_MINK) then
              maxk = LAM_MINK
          end if
          ! Begin construction
          this%m_name   = name
          this%m_sid    = sid
          this%m_nsamp  = nsamp
          this%m_maxk   = maxk
          this%m_dur    = LAM_PINF
          this%m_sinit  = LAM_PINF
          this%m_stsinc = LAM_PINF
          this%m_freq   = LAM_PINF
          this%m_ctors(1) = "default_signal"
          !this%m_jvec = JonesVector_t()
          associate(k=>this%m_maxk,n=>this%m_nsamp)
              allocate(this%m_jvec(n),         &
                       this%m_scomp(k,n),      &
                       this%m_ccomp(k,n),      &
                       this%m_wct(n),          &
                       this%m_nenvp(n),        &
                       this%m_triangle(n),     &
                       this%m_cform(n),        &
                       this%m_E(n),            &
                       this%m_amp(n),          &
                       this%m_S(n),            &
                       STAT=aerr,              &
                       ERRMSG=emsg)
          end associate
          if(aerr.NE.0) then
              if(logging) then
                   call log_startup(filename,append)
                   call UsrMsg("logger:311, In->mod_trianglew_signal/default_signal: Memory allocation failure!!")
                   call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_trianglew_signal/default_signal:311, Memory allocation failure!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/default_signal:311 -> [FATAL-ERROR]: Terminating execution"
          end if 
          do i = 1, this%m_nsamp
              this%m_jvec(i) = JonesVector_t()
          end do
          do i = 1, this%m_maxk
!DIR$         SIMD VECTORLENGTHFOR(RAL(KIND=8))
              do j = 1, this%m_nsamp
                  this%m_scomp(i,j) = LAM_PINF
                  this%m_ccomp(i,j) = LAM_PINF
              end do
          end do
!DIR$        SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do j = 1, this%m_nsamp
              this%m_wct(j)          = LAM_PINF
              this%m_nenvp(j)        = LAM_PINF
              this%m_triangle(i)     = LAM_PINF
              this%m_cform(j)        = DCMPLX(LAM_PINF,LAM_PINF)
              this%m_E(j)            = DCMPLX(LAM_PINF,LAM_PINF)
              this%m_amp(j)          = LAM_PINF
              this%m_S(j)            = DCMPLX(LAM_PINF,LAM_PINF)
          end do
          this%m_isbuilt = .true.
          if(dbg) then
              print*, "Jones vector field:", this%m_jvec
              print*, "Sine component:    ", this%m_scomp
              print*, "Cosine component:  ", this%m_ccomp
              print*, "Wcarrier*time:     ", this%m_wct
              print*, "Natural-envelope:  ", this%m_nenvp
              print*, "Triangle wave:     ", this%m_triangle
              print*, "Complex form:      ", this%m_canform
              print*, "Electric field:    ", this%m_E
              print*, "Amplitude:         ", this%m_amp
              print*, "PSD:               ", this%m_S
          end if
    end subroutine
                              
    !======================================================60
    !  subroutine: create_signal                        
    !              Physical representation of  Triangle wave
    !              approximated by K sinusoids (Fourier-
    !              series)
    !======================================================60                            
    subroutine create_signal(this,name,sid,nsamp,maxk,dur, &
                             sinit,stsinc,freq,r,h,v,      &
                             logging,filename,append,      &
                             dbg,profiling,qpctimer       )
          implicit none
          class(TriangleWSignal_t),    intent(inout) :: this
          character(len=*),            intent(in)    :: name
          integer(I32P),               intent(in)    :: sid
          integer(I32P),               intent(inout) :: nsamp,maxk
          real(R64P),                  intent(inout) :: dur
          real(R64P),                  intent(in)    :: sinit,stsinc,freq
          real(R64P), dimension(:),    intent(in)    :: r
!DIR$     ASSUME_ALIGNED r:32
          complex(R64P), dimension(:), intent(in)    :: h,v
!DIR$     ASSUMED_ALIGNED h:32,v:32
          logical(I32P),               intent(in)    :: logging
          character(len=*),            intent(in)    :: filename
          logical(I32P),               intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),            intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: i,j,k
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail,invalid
          real(R64P)         :: insamp,t,t2,delta
          real(R64P), allocatable, dimension(:) :: tcos,ts,tc
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:395, In->mod_trianglew_signal/create_signal: TriangleWSignal_t already initialized!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_trianglew_signal/create_signal:395, TriangleWSignal_t already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          ! Check if stsinc value lies in interval 1/2^-1 -> 1/2^-16
          invalid = .false.
          do i = 1, 16
              if(DABS(stsinc)-DABS(LAM_dtcoeff(i)).GT.LAM_MEPS8) then
                   invalid = .true.
              end if
          end do
          if(invalid) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:418, In->mod_trianglew_signal/create_signal: Invalid time-step increment!!"
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_trianglew_signal/create_signal:418, Invalid time-step increment!!)"
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
          if(maxk.LT.LAM_MINK) then
              maxk =LAM_MINK
          end if
          if(dur.LE.LAM_ZR8) then
            
              if(logging) then
                   call log_startup(filename,append)
                   call log_UsrMsg("logger:440, In->mod_trianglew_signal/create_signal: Invalid arguments: dur!")
                   call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_trianglew_signal/create_signal:440, Invalid arguments: dur  !)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
            ! Begin construction
          this%m_name   = name
          this%m_sid    = sid
          this%m_nsamp  = nsamp
          this%m_maxk   = maxk
          this%m_dur    =  dur
          this%m_sinit  = sinit
          this%m_stsinc = stsinc
          this%m_freq   = freq
          this%m_ctors(2) = "create_signal"
         ! this%m_jvec     = JonesVector_t(h,v)
          associate(k=this%m_maxk,n=>this%m_nsamp)
              allocate(this%m_jvec(n),         &
                       this%m_scomp(k,n),      &
                       this%m_ccomp(k,n),      &
                       this%m_wct(n),          &
                       this%m_nenvp(n),        &
                       this%m_triangle(n),     &
                       this%m_cform(n),        &
                       this%m_E(n),            &
                       this%m_amp(n),          &
                       this%m_S(n),            &
                       tcos(n),                &
                       ts(n),                  &
                       tc(n),                  &
                       STAT=aerr,              &
                       ERRMSG=emsg)
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:484, In->mod_trianglew_signal/create_signal: Memory allocation failure!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_trianglew_signal/create_signal:484, Memory allocation failure!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/create_signal:484 -> [FATAL-ERROR]: Terminating execution!!"
          end if
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_trianglew_signal/create_signal: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
          ! Create Jones vector field.
          do i = 1, this%m_nsamp
              this%m_jvec(i) = JonesVector_t(h(i),v(i))
          end do
          ! Create sine and cosine Fourier series components
          
          insamp = 1._R64P/DBLE(this%m_nsamp)
          k = 0
          do i = 1, this%m_maxk
                 k = k+1
                 do j = 1, this%m_nsamp
                     this%m_sinit = this%m_sinit+this%m_stsinc
                     delta = DBLE(j)*insamp
                     t = this%m_sinit*delta
                     this%m_scomp(i,j) = -1**k*DSIN(LAM_2PI*(2*k+1)*this%m_freq*t)/((2*k+1)*(2*k+1))
                     this%m_ccomp(i,j) = -1**k*DCOS(LAM_2PI*(2*k+1)*this%m_freq*t)/((2*k+1)*(2*k+1))
                 end do
          end do
           ! Do summation over the rows
          this%m_triangle =  SUM(this%m_scomp,dim=2)
          tcos            =  SUM(this%m_ccomp,dim=2)
          ! Create sawtooth signal (final approximation) and
          ! create signal complex representation
          do j = 1, this%m_nsamp
              this%m_sinit = this%m_sinit+this%m_stsinc
              delta = DBLE(j)*insamp
              t = this%m_sinit*delta
              this%m_wct(j) = t
              this%m_triangle(j) = LAM_8OPIE2*this%m_triangle(j)
              tcos(j)            = LAM_8OPIE2*tcos(j)
              this%m_cform(j) = DCMPLX(tcos(j),this%m_triangle(j))
          end do
           ! Compute natural envelope
          call vcmag(this%m_cform,this%m_nenvp)
             ! Create electrical field far from the emitter.
          k = 0
          do i = 1, this%m_maxk
                 k = k+1
                 do j = 1, this%m_nsamp
                     this%m_sinit = this%m_sinit+this%m_stsinc
                     delta = DBLE(j)*insamp
                     t = this%m_sinit*delta
                     t2 = t-r(j)/LAM_c
                     this%m_scomp(i,j) = -1**k*DSIN(LAM_2PI*(2*k+1)*this%m_freq*t2)/((2*k+1)*(2*k+1))
                     this%m_ccomp(i,j) = -1**k*DCOS(LAM_2PI*(2*k+1)*this%m_freq*t2)/((2*k+1)*(2*k+1))
                 end do
          end do
           ! Do summation over the rows
          tc = SUM(this%m_ccomp,dim=2)
          ts = SUM(this%m_scomp,dim=2)
          do j = 1, this%m_nsamp
              tc(j) = LAM_8OPIE2*tc(j)
              ts(j) = LAM_8OPIE2*ts(j)
              this%m_E(j) = DCMPLX(tc(j),ts(j))
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
                         write(stderr,*) "mod_trianglew_signal/create_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_trianglew_signal/create_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
    end subroutine

    !======================================================60
    ! subroutine: copy_signal                        
    !======================================================60
    subroutine copy_signal(this,other,logging,filename,append)
          implicit none
          class(TriangleWSignal_t), intent(inout) :: this
          class(TriangleWSignal_t), intent(in)    :: other
          logical(I32P),            intent(in)    :: logging
          character(len=*),         intent(in)    :: filename
          logical(I32P),            intent(in)    :: append
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          if(LOC(this).EQ.LOC(other) .OR. &
             this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:606, In->mod_trianglew_signal/copy_signal: Attempted self-assignment or argument in invalid state!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_trianglew_signal/copy_signal:606, Attempted self_assignment, or argument in invalid state!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
             end if
             return
         end if
         this%m_name = other%m_name
         this%m_sid  = other%m_sid
         this%m_nsamp = other%m_nsamp
         this%m_maxk  = other%m_maxk
         this%m_dur   = other%m_dur
         this%m_sinit = other%m_sinit
         this%m_stsinc = other%m_stsinc
         this%m_freq = other%m_freq
         this%m_ctors(3) = "copy_signal"
         this%m_jvec = other%m_jvec
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
          class(TriangleWSignal_t), intent(inout) :: this
          logical(I32P),            intent(in)    :: logging
          character(len=*),         intent(in)    :: filename
          logical(I32P),            intent(in)    :: append
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: derr,i
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:659, In->mod_trianglew_signal/destroy_signal: TriangleWSignal_t already destroyed!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL============================================="
                  write(sdterr,*) " ( mod_trianglew_signal/destroy_signal:659, TriangleWSignal_t already destroyed!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL============================================="
              end if
              return
          end if
          ! Destroy jvec field components
          do i = 1, this%m_nsamp
              call this%m_jvec(i)%destroy_jvec
          end do
          this%m_name = ""
          this%m_sid  = -1
          this%m_nsamp = 0
          this%m_maxk = 0
          this%m_dur = LAM_PINF
          this%m_sinit = LAM_PINF
          this%m_stsinc = LAM_PINF
          this%m_freq = LAM_PINF
          this%m_ctors = " "
          ! Array members deallocation
          if(ALLOCATED(this%m_jvec)) then
              deallocate(this%m_jvec,  &
                         STAT=derr,    &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:689, In->mod_trianglew_signal/destroy_signal: Deallocation of [m_jvec]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL=================================================="
                      write(stderr,*) "  ( mod_trianglew_signal/destroy_signal:689, Deallocation of [m_jvec]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                       write(stderr,*) "=========================FATAL=================================================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/destroy_signal:689, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_scomp)) then
              deallocate(this%m_scomp, &
                         STAT=derr,    &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:718, In->mod_trianglew_signal/destroy_signal: Deallocation of [m_scomp]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL=================================================="
                      write(stderr,*) "  ( mod_trianglew_signal/destroy_signal:718, Deallocation of [m_scomp]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                       write(stderr,*) "=========================FATAL=================================================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/destroy_signal:718, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_ccomp)) then
              deallocate(this%m_ccomp, &
                         STAT=derr,    &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:743, In->mod_trianglew_signal/destroy_signal: Deallocation of [m_ccomp]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL=================================================="
                      write(stderr,*) "  ( mod_trianglew_signal/destroy_signal:743, Deallocation of [m_ccomp]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                       write(stderr,*) "=========================FATAL=================================================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/destroy_signal:743, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_wct)) then
              deallocate(this%m_wct, &
                         STAT=derr,  &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:768, In->mod_trianglew_signal/destroy_signal: Deallocation of [m_wct]: failed!" )
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL=================================================="
                      write(stderr,*) "  ( mod_trianglew_signal/destroy_signal:768, Deallocation of [m_wct]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                       write(stderr,*) "=========================FATAL=================================================="
                  end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/destroy_signal:768, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_nenvp)) then
              deallocate(this%m_nenvp, &
                         STAT=derr,    &
                         ERRMSG=emsg)
                if(derr.NE.0) then
                   if(logging) then
                       call log_startup(filename,append)
                       call log_UsrMsg("logger:793, In->mod_trianglew_signal/destroy_signal: Deallocation of [m_nenvp]: failed!")
                       call log_shutdown()
                   else
                       call DATE_AND_TIME(date=dstr,time=tstr)
                       write(stderr,*) "=========================FATAL==================================================="
                       write(stderr,*) "  ( mod_trianglew_signal/destroy_signal:795, Deallocation of [m_nenvp]: failed)"
                       write(stderr,*) "   System: ", emsg
                       write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                         write(stderr,*) "=========================FATAL=================================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/destroy_signal:795, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_triangle)) then
              deallocate(this%m_triangle, &
                         STAT=derr,       &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:818, In->mod_trianglew_signal/destroy_signal: Deallocation of [m_triangle]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL======================================================"
                      write(stderr,*) "  ( mod_trianglew_signal/destroy_signal:818, Deallocation of [m_trianglew]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                         write(stderr,*) "=========================FATAL==================================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/destroy_signal:818, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_cform)) then
              deallocate(this%m_cform, &
                         STAT=derr,    &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:843, In->mod_trianglew_signal/destroy_signal: Deallocation of [m_cform]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL==================================================="
                      write(stderr,*) "  ( mod_trianglew_signal/destroy_signal:843, Deallocation of [m_cform]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                         write(stderr,*) "=========================FATAL================================================"
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/destroy_signal:843, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_E)) then
               deallocate(this%m_E,   &
                          STAT=derr,  &
                          ERRMSG=emsg )
               if(derr.NE.0) then
                   if(logging) then
                       call log_startup(filename,append)
                       call log_UsrMsg("logger:867, In->mod_trianglew_signal/destroy_signal: Deallocation of [m_E]: failed!")
                       call log_shutdown()
                   else
                       call DATE_AND_TIME(date=dstr,time=tstr)
                       write(stderr,*) "=========================FATAL==============================================="
                       write(stderr,*) "  ( mod_trianglew_signal/destroy_signal:867, Deallocation of [m_E]: failed)"
                       write(stderr,*) "   System: ", emsg
                       write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                       write(stderr,*) "=========================FATAL==============================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/destroy_signal:867, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_amp)) then
              deallocate(this%m_amp,  &
                         STAT=derr,   &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:893, In->mod_trianglew_signal/destroy_signal: Deallocation of [m_amp]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL==================================================="
                      write(stderr,*) "  ( mod_trianglew_signal/destroy_signal:893, Deallocation of [m_amp]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                      write(stderr,*) "=========================FATAL==================================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/destroy_signal:893, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          if(ALLOCATED(this%m_S)) then
              deallocate(this%m_S,  &
                         STAT=derr, &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:918, In->mod_trianglew_signal/destroy_signal: Deallocation of [m_S]: failed!")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(stderr,*) "=========================FATAL==================================================="
                      write(stderr,*) "  ( mod_trianglewsignal/destroy_signal:918, Deallocation of [m_S]: failed)"
                      write(stderr,*) "   System: ", emsg
                      write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                      write(stderr,*) "=========================FATAL==================================================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_trianglew_signal/destroy_signal:918, [FATAL-ERROR]: Terminating execution!"
              end if
          end if
          this%m_isbuilt = .false.
    end subroutine
    
    !======================================================60
    !  Getter pure functions
    !======================================================60
    
    pure function get_name(this) result(name)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          character(len=64) :: name
          ! Start of executable statements
          name = this%m_name
    end function
    
    pure function get_sid(this) result(sid)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: sid
          ! Start of executable statements
          sid = this%m_sid
    end function
    
    pure function get_nsamp(this) result(nsamp)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nsamp
          ! Start of executable statements
          nsamp = this%m_nsamp
    end function
    
    pure function get_maxk(this) result(maxk)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          integer(I32P) :: maxk
          ! Start of executable statements
          maxk = this%m_maxk
    end function
    
    pure function get_dur(this) result(dur)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: dur
          ! Start of executable satements
          dur = this%m_dur
    end function
    
    pure function get_sinit(this) result(sinit)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: sinit
          ! Start of executable satatements
          sinit = this%m_sinit
    end function
    
    pure function get_stsinc(this) result(stsinc)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: stsinc
          ! Start of executable statements
          stsinc = this%m_stsinc
    end function
    
    pure function get_freq(this) result(freq)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          real(R64P) :: freq
          ! Start of executable sattaements
          freq = this%m_freq
    end function
    
    pure function get_ctors(this) result(ctors)
         implicit none
         class(TriangleWSignal_t), intent(in) :: this
         ! Locals
         character(len=32), dimension(3) :: ctors
         ! Start of executable sattemetns
         ctors = this%m_ctors
    end function
    
    pure function get_jvec(this) result(jvec)
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          type(JonesVector_t), allocatable, dimension(:) :: jvec
          ! Start of executable statements
          jvec = this%m_jvec
    end function
    
     pure function get_scomp(this) result(scomp)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: scomp
!DIR$     ATTRIBUTES ALIGN : 32 :: scomp
          ! Start of executable statements
          scomp = this%m_scomp
    end function
    
    pure function get_ccomp(this) result(ccomp)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: ccomp
!DIR$     ATTRIBUTES ALIGN : 32 :: ccomp
          ! Start of executable statements
          ccomp = this%m_ccomp
    end function
    
    pure function get_wct(this) result(wct)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: wct
!DIR$     ATTRIBUTES ALIGN : 32 :: wct
          ! Start of executable statements
          wct = this%m_wct
    end function
    
    pure function get_nenvp(this) result(nenvp)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: nenvp
!DIR$     ATTRIBUTES ALIGN : 32 :: nenvp
          ! Start of executable statements
          nenvp = this%m_nenvp
    end function
    
    pure function get_sawtooth(this) result(sawtooth)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: sawtooth
!DIR$     ATTRIBUTES ALIGN : 32 :: sawtooth
          ! Satrt of executable statements
          sawtooth = this%m_sawtooth
    end function
    
    pure function get_cform(this) result(cform)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: cform
!DIR$     ATTRIBUTES ALIGN : 32 :: cform
          ! Start of executable statements
          cform = this%m_cform
    end function
    
    pure function get_E(this) result(E)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: E
!DIR$     ATTRIBUTES ALIGN : 32 :: E
          ! Start of executable statemetns
          E = this%m_E
    end function
    
    pure function get_amp(this) result(amp)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: amp
!DIR$     ATTRIBUTES ALIGN : 32 :: amp
          ! Start of executable statements
          amp = this%m_amp
    end function
    
    pure function get_S(this) result(S)
          implicit none
          class(TriangleWSignal_t), intent(in) :: this
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
          class(TriangleWSignal_t),  intent(in)     :: this
          real(R64P), dimension(:),  intent(out)    :: dphi
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
          class(TriangleWSignal_t), intent(in) :: this
          logical(I32P),            intent(in) :: verbose
          ! Locals
          ! Start of executable statements
          call DATE_AND_TIME(date=dstr,time=tstr)
          print*, "======================================================"
          print*, "   ***Printing components of TriangleWSignal_t***"
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
         
          if(verbose) then
              print*, "Sine series components:  ", this%m_scomp
              print*, "Cosine series components:", this%m_ccomp
          end if
          print*, "Signal polarization:  ",   this%m_jvec
          print*, "Time (t):             ",   this%m_wct
          print*, "Natural Envelope:     ",   this%m_nenvp
          print*, "Sawtooth time-domain: ",   this%m_sawtooth
          print*, "Complex form:         ",   this%m_cform
          print*, "Electric field:       ",   this%m_E
          print*, "Amplitude:            ",   this%m_amp
          print*, "Time-average power:   ",   this%m_S
          print*, "====================================================="
    end subroutine
    
    
                             
end module mod_trianglew_signal