
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
    use module_class_error_check, only : array1D_not_alloc
    use module_logger
    use mod_code_timing
    use mod_complex_arithm , only : vcmag
    use mod_jonesvec
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
        integer(I32P)     :: m_pid
        
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
        
        ! Build by which constructor
        character(len=32), dimension(3) :: m_ctors
        
        ! Jones Vector contains amplitudes of voltage
        ! i.e. Enl,Enk
        !complex(R64P), dimension(2) :: m_jvec
        type(JonesVector_t) :: m_jvec
        ! Electric field wave component.
        ! Phasor representation.
        complex(R64P), allocatable, dimension(:) :: m_E
!DIR$   ATTRIBUTES ALIGN : 32 :: m_E
        
        ! Signal enevelope i.e natural envelope  set to 1
        real(R64P), allocatable, dimension(:) :: m_nenvp
!DIR$   ATTRIBUTES ALIGN : 32 :: m_nenvp
        
        ! Signal phase
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
        
        ! Signal amplitude  A(phi,theta)/r used forcalculation electric field
        ! far from emmiter(antenna)
        real(R64P), allocatable, dimension(:) :: m_amp
!DIR$   ATTRIBUTES ALIGN : 32 :: m_amp
        
        ! Time-averaged power density
        ! To be properly implemented
        complex(R64P), allocatable, dimension(:) :: m_S
!DIR$   ATTRIBUTES ALIGN : 32 :: m_S
        
        ! Logical member denoting built status
        logical(I32P) :: m_isbuilt
        
        
        
        contains
    
        !========================================
        !  Constructor subroutines
        !========================================
    
        ! Default constructor creates zero waveform
        ! Can be used as zero interval in pulse train
        
        procedure, pass(this), public :: default_signal
        
        ! Constructor creates pure cosine pulse
        
        
        procedure, pass(this), public :: create_signal
        
        ! Copy constructor
        
        procedure, pass(this), public :: copy_signal
        
        ! Class destructor
        procedure, pass(this), public :: destroy_signal
        
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
        
        procedure, pass(this), public :: dphi_dt
        
        procedure, pass(this), public :: analytic_signal
        
        !==============================================56
        ! write/read subroutines
        !==============================================56
        
        procedure, nopass, public :: to_screenu
        
       
        
        
    end type CosineSignal_t
    
    contains
    
    !======================================================60
    ! subroutine:
    !               default_signal (no physical meaning)
    !======================================================60
    subroutine default_signal(this,name,pid,nsamp,dur,logging, &
                              filename,append,dbg      )
          implicit none
          class(CosineSignal_t), intent(inout) :: this
          character(len=*),         intent(in)    :: name
          integer(I32P),            intent(in)    :: pid
          integer(I32P),            intent(inout) :: nsamp
          real(R64P),               intent(in)    :: dur
          logical(I32P),            intent(in)    :: logging
          character(len=*),         intent(in)    :: filename
          logical(I32P),            intent(in)    :: append,dbg
          ! Locals
          character(len=40)  :: dstr,tstr
          integer(I32P)      :: allocerr,i
          character(len=256) :: emsg
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .true.) then
              if(logging .EQ. .true.) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:261, In->default_signal: PureCosinePulse_t already initialized!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( default_signal:261, CosineSignal_t already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
             end if
             return
          end if
          if(nsamp.LE.32) then
              nsamp = MINSAMP
          end if
          
          ! Begin construction
          this%m_name     = name
          this%m_pid      = pid
          this%m_nsamp    = nsamp
          this%m_dur      = dur
          this%m_initime  = 0._R64P
          this%m_sinterv  = 0._R64P 
          this%m_cfreq    = 0._R64P
          this%m_envfreq  = 0._R64P
          this%m_tstep    = 0._R64P
          this%m_ctors(1) = "default"
          this%m_jvec     = JonesVector_t()
          associate(n=>this%m_nsamp)
              allocate(this%m_E(n),       &
                       this%m_nenvp(n),   &
                       this%m_phi(n),     &
                       this%m_canform(n), &
                       this%m_basform(n), &
                       this%m_cenvp(n),   &
                       this%m_amp(n),     &
                       this%m_S(n),       &
                       STAT=allocerr,     &
                       ERRMSG=emsg)
          end associate
          if(allocerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:293, In->default_pulse: Memory allocation failure!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (default_signal:239, Memory allocation failure!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "default_pulse: [FATAL-ERROR]: Terminating execution!!"
          end if
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1, this%m_nsamp
              this%m_E(i)       = DCMPLX(0._R64P,0._R64P)
              this%m_nenvp(i)   = 0._R64P
              this%m_phi(i)     = 0._R64P
              this%m_canform(i) = DCMPLX(0._R64P,0._R64P)
              this%m_basform(i) = 0._R64P
              this%m_cenvp(i)   = DCMPLX(0._R64P,0._R64P)
              this%m_amp(i)     = 0._R64P
              this%m_S(i)       = DCMPLX(0._R64P,0._R64P)
          end do
          this%m_isbuilt = .true.
          if(dbg) then
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
    !  subroutine: create_pulse
    !======================================================60
    subroutine create_signal(this,name,pid,nsamp,dur,initime, &
                            sinterv,cfreq,envfreq,tstep,r,c1,c2,logging,   &
                            filename,append,dbg,profiling,qpctimer   )
          implicit none
          class(CosineSignal_t), intent(inout)    :: this
          character(len=*),         intent(in)    :: name
          integer(I32P),            intent(in)    :: pid
          integer(I32P),            intent(inout) :: nsamp
          real(R64P),               intent(in)    :: dur,initime,tstep
          real(R64P),               intent(inout) :: sinterv
          real(R64P),               intent(inout) :: cfreq,envfreq
          real(R64P), dimension(:), intent(in)    :: r ! range
          complex(R64P),            intent(in)    :: c1,c2
!DIR$     ASSUME_ALIGNED r:32
          logical(I32P),            intent(in)    :: logging
          character(len=*),         intent(in)    :: filename
          logical(I32P),            intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),         intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          integer(I32P) :: i,allocerr
          real(R64P)    :: insamp,delta,t,t2,tc,ts
          complex(R64P), allocatable, dimension(:) :: tmp
          real(R64P), allocatable, dimension(:) :: ct
          ! Start of executable statements
          if(this%m_isbuilt) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:364, In->create_signal: CosineSignal_t already initialized!!"
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( create_signal:261, CosineSignal_t already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          if(nsamp.LE.32) then
              nsamp = MINSAMP
          end if
          if(cfreq.LT.MINFREQ) then
              cfreq = MINFREQ
          end if
          if(dur.LE.0._R64P) then
              dur =  NS
          end if
          if(tstep.LT.SMALLTS) then
              tstep = SMALLTS
          end if
          this%m_name    = name
          this%m_pid     = pid
          this%m_nsamp   = nsamp
          this%m_dur     = dur
          this%m_initime = initime
          this%m_sinterv = sinterv
          this%m_cfreq   = cfreq
          this%m_envfreq = envfreq
          this%m_tstep   = tstep
          this%m_ctors(2) = "create_pulse"
          this%m_jvec = JonesVector_t(c1,c2)
          associate(n=>this%m_nsamp)
              allocate(this%m_E(n)        &
                       this%m_nenvp(n),   &
                       this%m_phi(n),     &
                       this%m_canform(n), &
                       this%m_basform(n), &
                       this%m_cenvp(n),   &
                       this%m_amp(n),     &
                       this%m_S(n),       &
                       tmp(n),            &
                       ct(n),             &
                       STAT=allocerr,     &
                       ERRMSG=emsg)
          end associate
          if(allocerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:410, In->create_pulse: Memory allocation failure!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (create_pulse:410, Memory allocation failure!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "create_pulse:410, [FATAL-ERROR]: Terminating execution!"
          end if
!DIR$     SIMD VECTORLENGTHFOR(REAL(KIND=8))
          do i = 1, this%m_nsamp
              this%m_E(i)       = DCMPLX(0._R64P,0._R64P)
              this%m_nenvp(i)   = 0._R64P
              this%m_phi(i)     = 0._R64P
              this%m_canform(i) = DCMPLX(0._R64P,0._R64P)
              this%m_basform(i) = 0._R64P
              this%m_cenvp(i)   = DCMPLX(0._R64P,0._R64P)
              this%m_amp(i)     = 0._R64P
              this%m_S(i)       = DCMPLX(0._R64P,0._R64P)
              tmp(i)            = DCMPLX(0._R64P,0._R64P)
          end do
          delta = 0._R64P
          t = 0._R64P
          t2 = 0._R64P
          insamp = 1._R64P/DBLE(this%m_nsamp)
          if(profiling) then
               call qpctimer_start(qpctimer,ifail)
               if(ifail.EQ.0) then
                    write(ERROR_UNIT,*) "create_signal: qpctimer_start failed to query performance frequency counter!!"
               end if
          end if
          do i = 1, this%m_nsamp
               this%m_initime = this%m_initime+this%m_tstep
               delta = DBLE(i)*insamp
               t = this%m_initime*delta
               t2 = t-r(i)/c
               this%m_phi(i) = TWOPI*this%m_freq*t2
               !this%m_amp(i) = this%m_amp(i)/this%m_r(i)
               this%m_E(i) = DCMPLX(DCOS(this%m_phi(i)),DSIN(this%m_phi(i)))
               !tmp(i) = this%m_E(i) ! copy signal components to temporaray arary.
               !this%m_E(i) = this%m_E(i)*this%m_amp(i)
          end do
          ! Compute A(phi,theta)/r - magnitude of electric field E
          call vcmag(this%m_E,this%m_amp)
          ! Create signal complex components
          do i = 1, this%m_nsamp
               this%m_initime = this%m_initime+this%m_tstep
               delta = DBLE(i)*insamp
               t = this%m_initime*delta
               ct(i) = t ! to be used for creation canonical form
               this%m_phi(i) = TWOPI*this%m_freq*t
               tmp(i) = DCMPLX(DCOS(this%m_phi(i)),DSIN(this%m_phi))
          end do
          ! Compute natural envelope
          call vcmag(tmp,this%m_nenvp)
          tc = 0._R64P
          ts = 0._R64P
          ! Create canonical representation
          do i = 1, this%m_nsamp
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
          this%m_built = .true.
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(ERROR_UNIT,*) "create_signal: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(ERROR_UNIT,*) "create_signal: Unable to read performance counter -- fatal!!"
                end if
          end if
          
    end subroutine
                            
    !=======================================================61
    ! subroutine: copy_pulse
    !
    !=======================================================61                        
    subroutine copy_signal(this,other,logging,   &
                          filename,append)
          implicit none
          class(CosineSignal_t),  intent(inout) :: this
          class(CosineSignal_t),  intent(in)    :: other
          ! Locals
          character(len=40) :: dstr,tstr
          !Start of executable sattements
          
          if(LOC(this).EQ.LOC(other) .OR. &
             other%m_isbuilt .EQ. .false. ) then
               if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:552, In->copy_pulse: Attempted self-assignment, or argument in deleted state!")
                    call log_shutdown()
               else
                   call DATE_AND_TIME(date=dstr,time=tstr)
                   write(stderr,*) "===========================NON-FATAL=========================="
                   write(sdterr,*) " ( copy_pulse:552, Attempted self_assignment, or argument in deleted state!)"
                   write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                   write(stderr,*) "===========================NON-FATAL=========================="
               end if
               return
          end if
          this%m_name    = other%m_name
          this%m_pid     = other%m_pid
          this%m_nsamp   = other%m_nsamp
          this%m_dur     = other%m_dur
          this%m_initime = this%m_initime
          this%m_sinterv = this%m_sinterv
          this%m_cfreq   = this%m_cfreq
          this%m_envfreq = this%m_envfreq
          this%m_tstep   = this%m_tstep
          this%m_ctors(3) = "copy_pulse"
          this%m_jvec = other%m_jvec
          this%m_E = other%m_E
          this%m_nenvp = other%m_nenvp
          this%m_phi = other%m_phi
          this%m_canform = other%m_canform
          this%m_basform = other%m_basform
          this%m_cenvp = other%m_cenvp
          this%m_amp = other%m_amp
          this%m_S = other%m_S
          this%m_isbuilt = .true.
     end subroutine
                          
    !======================================================60                      
    !  subroutine: destroy_pulse
    !              Deallocates allocated arrays and sets to
    !              other members to default values.
    !======================================================60
    subroutine destroy_signal(this,logging,filename,append)
          implicit none
          class(CosineSignal_t), intent(inout) :: this
          logical(I32P),            intent(in)    :: logging
          character(len=*),         intent(in)    :: filename
          logical(I32P),            intent(in)    :: append
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable stateemmtns
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:608, In->destroy_signal: CosineSignal_t already destroyed!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( destroy_signal:608, CosineSignal_t already destroyed!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              return
          end if
          ! Nullify scalar varaiables
          this%m_name    = " "
          this%m_pid     = 0
          this%m_nsamp   = 0._R64P
          this%m_dur     = 0._R64P
          this%m_initime = 0._R64P
          this%m_sinterv = 0._R64P
          this%m_cfreq   = 0._R64P
          this%m_envfreq = 0._R64P
          this%m_tstep   = 0._R64P
          this%m_ctors   = " "
          this%m_jvec    = JonesVector_t()
          ! Begin array deallocation stage.
          if(allocated(this%m_E)) then
              deallocate(this%m_E,     &
                         STAT=derr, &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  write(stderr,*) "=========================FATAL======================="
                  write(stderr,*) "  ( destroy_signal:637, Failed deallocate: m_E) "
                  write(stderr,*) "   System: ", emsg
                  write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(stderr,*) "=========================FATAL======================="
                  ERROR STOP "destroy_signal:637, [FATAL]:  Failed deallocate: m_E!"
              end if
          end if
          if(allocated(this%m_nenvp)) then
              deallocate(this%m_nenvp, &
                         STAT=derr,    &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  write(stderr,*) "=========================FATAL======================="
                  write(stderr,*) "  ( destroy_pulse:652, Failed deallocate: m_nenvp) "
                  write(stderr,*) "   System: ", emsg
                  write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(stderr,*) "=========================FATAL======================="
                  ERROR STOP "destroy_pulse:652, [FATAL]:  Failed deallocate: m_nenvp!"
              end if
          end if
          if(allocated(this%m_phi)) then
              deallocate(this%m_phi, &
                         STAT=derr,  &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  write(stderr,*) "=========================FATAL======================="
                  write(stderr,*) "  ( destroy_pulse:667, Failed deallocate: m_phi) "
                  write(stderr,*) "   System: ", emsg
                  write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(stderr,*) "=========================FATAL======================="
                  ERROR STOP "destroy_pulse:667, [FATAL]:  Failed deallocate: m_phi!"
              end if
          end if
          if(allocated(this%m_canform)) then
              deallocate(this%m_canform, &
                         STAT=derr,      &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  write(stderr,*) "=========================FATAL======================="
                  write(stderr,*) "  ( destroy_pulse:682, Failed deallocate: m_canform) "
                  write(stderr,*) "   System: ", emsg
                  write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(stderr,*) "=========================FATAL======================="
                  ERROR STOP "destroy_pulse:682, [FATAL]:  Failed deallocate: m_canform!"
              end if
          end if
          if(allocated(this%m_basform)) then
              deallocate(this%m_basform, &
                         STAT=derr,      &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  write(stderr,*) "=========================FATAL======================="
                  write(stderr,*) "  ( destroy_pulse:697, Failed deallocate: m_basform) "
                  write(stderr,*) "   System: ", emsg
                  write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(stderr,*) "=========================FATAL======================="
                  ERROR STOP "destroy_pulse:697, [FATAL]:  Failed deallocate: m_basform!"
              end if
          end if
          if(allocated(this%m_cenvp)) then
              deallocate(this%m_cenvp, &
                         STAT=derr,    &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  write(stderr,*) "=========================FATAL======================="
                  write(stderr,*) "  ( destroy_pulse:712, Failed deallocate: m_cenvp) "
                  write(stderr,*) "   System: ", emsg
                  write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(stderr,*) "=========================FATAL======================="
                  ERROR STOP "destroy_pulse:712, [FATAL]:  Failed deallocate: m_cenvp!"
              end if
          end if
          if(allocated(this%m_amp)) then
              deallocate(this%m_amp, &
                         STAT=derr,  &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  write(stderr,*) "=========================FATAL======================="
                  write(stderr,*) "  ( destroy_pulse:727, Failed deallocate: m_amp) "
                  write(stderr,*) "   System: ", emsg
                  write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(stderr,*) "=========================FATAL======================="
                  ERROR STOP "destroy_pulse:727, [FATAL]:  Failed deallocate: m_amp!"
              end if
          end if
          if(allocated(this%m_S)) then
              deallocate(this%m_S,  &
                         STAT=derr, &
                         ERRMSG=emsg )
              if(derr.NE.0) then
                  write(stderr,*) "=========================FATAL======================="
                  write(stderr,*) "  ( destroy_pulse:742, Failed deallocate: m_S) "
                  write(stderr,*) "   System: ", emsg
                  write(stderr,*) "  ( Fatal Error at:) ",  &
                                     dstr(1:4),"-",dstr(5:6),"-",dstr(7:8), " ", &
                                     tstr(1:2),":",tstr(3:4),":",tstr(5:6)
                  write(stderr,*) "=========================FATAL======================="
                  ERROR STOP "destroy_pulse:742, [FATAL]:  Failed deallocate: m_S!"
              end if
          end if
          this%m_isbuilt = .false.
    end subroutine
    
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
    
    pure function get_ctors(this) result(ctors)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          character(len=32), dimension(3) :: ctors
          ! Start of executable statements
          ctors = this%m_ctors
    end function
    
    pure function get_jvec(this) result(jvec)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          type(JonesVector_t) :: jvec
          ! Start of excutable sattements
          jvec = this%m_jvec
    end function
    
    pure function get_E(this) result(E)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: E
!DIR$     ATTRIBUTES ALIGN : 32 :: E
          ! Start of executable statements
          E = this%m_E ! allocation on asignment
    end function
    
    pure function get_nenvp(this) result(nenvp)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: nenvp
!DIR$     ATTRIBUTES ALIGN : 32 :: nenvp
          ! Start of executable statements
          nenvp = this%m_nenvp
    end function
    
    pure function get_phi(this) result(phi)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: phi
!DIR$     ATTRIBUTES ALIGN : 32 :: phi
          ! Start of executable satatenemtns
          phi = this%m_phi
    end function
    
    pure function get_canform(this) result(canform)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: canform
!DIR$     ATTRIBUTES ALIGN : 32 :: canform
          ! Start of executable statements
          canform = this%m_canform
    end function
    
    pure function get_basform(this) result(basform)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: basform
!DIR$     ATTRIBUTES ALIGN : 32 :: basform
          ! Start of executable statements
          basform = this%m_basform
    end function
    
    pure function get_cenvp(this) result(cenvp)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: cenvp
!DIR$     ATTRIBUTES ALIGN : 32 :: cenvp
          ! Start of executable statements
          cenvp = this%m_cenvp
    end function
    
    pure function get_amp(this) result(amp)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:) :: amp
!DIR$     ATTRIBUTES ALIGN : 32 :: amp
          ! Start of executable statements
          amp = this%m_amp
    end function
    
    pure function get_S(this) result(S)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          complex(R64P), allocatable, dimension(:) :: S
!DIR$     ATTRIBUTES ALIGN : 32 :: S
          ! Start of executable statements
          S = this%m_S
    end function
    
    pure function get_built_stat(this) result(isbuilt)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          logical(I32P) :: isbuilt
          ! Start of executable statements
          isbuilt = this%m_isbuilt
    end function
    
    !======================================================60
    ! sbroutine: dphi_dt
    !            Phase signal time derivative
    !======================================================60
    subroutine dphi_dt(this,dphi,sfac)
          implicit none
          class(CosineSignal_t), intent(in)  :: this
          real(R64P), dimension(:), intent(out) :: dphi
          integer(I64P),            intent(in)  :: sfac ! scaling factor up to 16 digits of precision
          ! Locals
          integer(I64P) :: i,
          real(R64P)    :: eps,tmp,isfac  ! eps**0.3
          ! Start of executable statements
          if(sfac.LE.0) then
              sfac = 1000000000000000_I64P
          end if
          isfac = 1._R64P/DBLE(sfac)
          eps = MACHEPSF64**0.333333333333333333333333_R64P   
          do i = 2, this%m_nsamp-1
              tmp = this%m_phi(i+1)-this%m_phi(i-1) ! Consider using CADNA to test for cancellation errors
              dphi(i) = this%m_phi(i+1)-this%m_phi(i-1)/ &
                        (2*eps*DMAX1(DABS(tmp),isfac)*DSIGN(tmp,tmp))
          end do
    end subroutine    
          
    !======================================================
    !  subroutine: analytic signal
    !              Using this formula: 
    !======================================================
    subroutine analytic_signal(this,asig,tmplen,wlen,iplen,profiling,qpctimer)
          use mod_fftsg, only : rdft
          implicit none
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
          ! Start of executable statements
          ! Copy real part of canonical representation
          if(profiling) then
             call qpctimer_start(qpctimer,ifail)
             if(ifail.EQ.0) then
                 write(stderr,*) "analytic_signal:996, qpctimer_start failed to query performance frequency counter!!"
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
                        write(stderr,*) "analytic_signal:1020, qpctimer_delta: failed to compute delta measurement!!"
                   end if  
               else
                       write(stderr,*) "analytic_signal:1020 Unable to read performance counter -- fatal!!"
               end if
           end if
           
    end subroutine
    
    !======================================================60
    ! subroutine: to_screenu
    !             Print unformatted stream to screen
    !======================================================60
    subroutine to_screenu(this)
          implicit none
          class(CosineSignal_t), intent(in) :: this
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          call DATE_AND_TIME(date=dstr,time=tstr)
          print*, "======================================================"
          print*, "   ***Printing components of PureCosinePulse_t***"
          print*, "   printing subroutine called at: " , &
                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                     stime(1:2),":",stime(3:4),":",stime(5:6)
          print*, "           Dumping scalar components "
          print*, "======================================================"
          print*, "Name:              ", this%m_name
          print*, "Pulse ID:          ", this%m_pid
          print*, "Number of samples: ", this%m_nsamp
          print*, "Pulse duration:    ", this%m_dur
          print*, "Pulse initial time:", this%m_initime
          print*, "Sample interval:   ", this%m_sinterv
          print*, "Carrier frequency: ", this%m_cfreq
          print*, "Envelope frequency:", this%m_envfreq
          print*, "Time step:         ", this%m_tstep
          print*, "           Dumping array components "
          print*, "====================================================="
          print*, "Constructors:      ", this%m_ctors
          print*, "Pulse Jones Vector:", this%m_jvec
          print*, "Electric field:    ", this%m_E
          print*, "Natural Envelope:  ", this%m_nenvp
          print*, "Phase:             ", this%m_phi
          print*, "Canonical form:    ", this%m_canform
          print*, "Base form:         ", this%m_basform
          print*, "Complex envelope:  ", this%m_cenvp
          print*, "Amplitude:         ", this%m_amp
          print*, "Time-average power:", this%m_S
          print*,"======================================================"
          print*, "Object built stat :", this%m_isbuilt
          print*, "           End                                       "
    end subroutine
    
    
                          
end module mod_cosine_signal