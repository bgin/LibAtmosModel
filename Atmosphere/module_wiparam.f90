
module mod_wiparam

       
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wiparam'
 !          
 !          Purpose:
 !                   Parametrization of Water and Ice.
 !                   
 !                     
 !          History:
 !                        Date: 23-09-2017
 !                        Time: 12:15 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
 !          References:
 !         
 !                     "Thermodynamics, kinetics and microphysics of clouds"
 !                      by Vitaliy I. Khvorostyanov & Judith A. Curry
 !                      Page 108, chapter 4.4
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    implicit none
    private
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT , &
                                stdout=>OUTPUT_UNIT
    use IFPORT, only : TRACEBACKQQ
    use module_class_error_check, only : array3D_not_alloc
                                       
    use module_kinds
    use mod_constants
    use module_logger
    use mod_code_timing 
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Version major
    integer(I32P), parameter, public :: MOD_WIPARAM_MAJOR = 1
    
    ! Version minor
    integer(I32P), parameter, public :: MOD_WIPARAM_MINOR = 0
    
    ! Version micro
    integer(I32P), parameter, public :: MOD_WIPARAM_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WIPARAM_FULLVER = 1000*MOD_WIPARAM_MAJOR+100*MOD_WIPARAM_MINOR+ &
                                                              10*MOD_WIPARAM_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WIPARAM_CREATE_DATE = "23-09-2017 12:15 +00200 (SAT 23 SEP 2017 GMT+2)"
    
    ! Module compilation date (should be set after successful build date/time)
    character(*),  parameter, public :: MOD_WIPARAM_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WIPARAM_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WIPARAM_DESCRIPT = "Water and Ice saturated pressure computation."
    
   
    
    
   
    
   
    
    !============================================50
    !   Type: water_ice_thermodynamic_t
    !============================================50  
    type, public :: water_ice_thermodynamic_t
        
          private
          
          ! indices  (conforms to WRF model)
          ! m_ims,m_ime,m_kms,m_kme
          ! m_jms,m_jme,m_ids,m_ide
          ! m_kds,m_kde,m_jds,m_jde
          ! m_its,m_ite,m_kts,m_kte
          ! m_jts,m_jte
          integer(I64P), dimension(A3DNidx) :: m_idx
          
          ! Saturated pressure of water i.e. m_ews in (Pa)
          real(R64P), allocatable, dimension(:,:,:) :: m_ews
!DIR$     ATTRIBUTES ALIGN : 32 :: m_ews
          
          ! Saturated over ice vapour pressure i.e. m_eis in (Pa)
          real(R64P), allocatable, dimension(:,:,:) :: m_eis
!DIR$     ATTRIBUTES ALIGN : 32 :: m_eis
          
          ! Heat capacity of water i.e. m_cw in (cal/g)
          real(R64P), allocatable, dimension(:,:,:) :: m_cw
!DIR$     ATTRIBUTES ALIGN : 32 :: m_cw
          
          ! Isobaric molar heat capacity of ice i.e. m_cpim in (J mol^-1 K^-1)
          real(R64P), allocatable, dimension(:,:,:) :: m_cpim
!DIR$     ATTRIBUTES ALIGN : 32 :: m_cpim
          
          ! Difference in molar heat capacity betweem water and ice m_dcpm in (J mol^-1 K^-1)
          real(R64P), allocatable, dimension(:,:,:) :: m_dcpm
!DIR$     ATTRIBUTES ALIGN : 32 :: m_dpcm
          
          ! Latent heat of evaporation i.e. m_Lv in (cal/g) , T(C)
          real(R64P), allocatable, dimension(:,:,:) :: m_Lv
!DIR$     ATTRIBUTES ALIGN : 32 :: m_Lv
          
          ! Molar latent heat of sublimation i.e. m_Ls in (J mol^-1), T(K)
          ! fitted to integral for T(K) > 30.0
          real(R64P), allocatable, dimension(:,:,:) :: m_Ls
!DIR$     ATTRIBUTES ALIGN : 32 :: m_Ls
          
          ! Specific melting heat i.e. m_Lm  in (cal/g)
          ! for range T(K) > 229
          real(R64P), allocatable, dimension(:,:,:) :: m_Lm
!DIR$     ATTRIBUTES ALIGN : 32 :: m_Lm
          
          ! Surface tension between water and air in (erg/cm^2) i.e. m_gamwa at
          ! T(C) > -40 , Prupacher and Klent (1997)
          real(R64P), allocatable, dimension(:,:) :: m_gamwa
!DIR$     ATTRIBUTES ALIGN : 32 :: m_gamwa
          
          ! Surface tension between ice and water in (erg/cm^2) i.e. m_gamwi
          ! for following ranges:
          ! 1) -36 < T(C) < 0
          ! 2) -44 < T(C) < -36
          real(R64P), allocatable, dimension(:,:) :: m_gamwi
!DIR$     ATTRIBUTES ALIGN : 32 :: m_gamwi
          
          ! Surface tension between ice and air/vapour in (erg/cm^2) i.e. m_gamiv
          real(R64P), allocatable, dimension(:,:) :: m_gamiv
!DIR$     ATTRIBUTES ALIGN : 32 :: m_gamiv
          
          ! Density of water in (g/cm^3) i.e. m_psiw
          ! for 0 <= T(C) < 100, Kell (1975)
          ! for -33 <= T(C) <= 0, Hare & Sorensen (1987)
          real(R64P), allocatable, dimension(:,:,:) :: m_psiw
!DIR$     ATTRIBUTES ALIGN : 32 :: m_psiw
          
          ! Density of ice (hexagonal ice, Ih) in (g/cm^3) i.e. m_psii
          ! for -180 <= T(C) <= 0, Pruppacher & Klett (1997)
          real(R64P), allocatable, dimension(:,:,:) :: m_psii
!DIR$     ATTRIBUTES ALIGN : 32 :: m_psii
          
          ! Built indicator (logical)
          logical(I32P) :: m_isbuilt
          
    contains
          
          !======================================50
          !  Constructor and Destructor
          !======================================50
          
          procedure, pass(this), public :: init
          
          procedure, pass(this), public :: copy
          
          procedure, pass(this), public :: destroy
          
          !======================================50
          !   getters
          !======================================50
          
          procedure, pass(this), public :: get_ews
          
          procedure, pass(this), public :: get_eis
          
          procedure, pass(this), public :: get_cw
          
          procedure, pass(this), public :: get_cpim
          
          procedure, pass(this), public :: get_dcpm
          
          procedure, pass(this), public :: get_Lv
          
          procedure, pass(this), public :: get_Ls
          
          procedure, pass(this), public :: get_Lm
          
          procedure, pass(this), public :: get_gamwa
          
          procedure, pass(this), public :: get_gamwi
          
          procedure, pass(this), public :: get_gamiv
          
          procedure, pass(this), public :: get_psiw
          
          procedure, pass(this), public :: get_psii
          
          !============================================56
          !     Computational procedures
          !============================================56
          
          procedure, pass(this), public :: compute_ews
          
          procedure, pass(this), public :: compute_eis
          
          procedure, pass(this), public :: compute_cw
          
          procedure, pass(this), public :: compute_cpim
          
          procedure, pass(this), public :: compute_dcpm
          
          procedure, pass(this), public :: compute_Lv
          
          procedure, pass(this), public :: compute_Ls
          
          procedure, pass(this), public :: compute_Lm
          
          procedure, pass(this), public :: compute_gamwa
          
          procedure, pass(this), public :: compute_gamwi
          
          procedure, pass(this), public :: compute_gamiv
          
          procedure, pass(this), public :: compute_psiw
          
          procedure, pass(this), public :: compute_psii
          
    end type water_ice_thermodynamic_t
    
    contains
    
    !============================================50
    !      Implementation
    !============================================50
    
    !========================================================62
    !  @subroutine:
    !               init
    !  @Purpose:
    !               Default initialization of type
    !               bound members scalars and allocata-
    !               ble arrays
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !               Upon detection of critical error
    !               'STOP' statement will be executed.
    !========================================================62
    subroutine init(this,indices,logging,filename,append,ierr)
          implicit none
          class(water_ice_thermodynamic_t),  intent(inout) :: this
          integer(I64P), dimension(A3DNidx), intent(in)    :: idx
          logical(I32P),                     intent(in)    :: logging
          character(len=*),                  intent(in)    :: filename
          logical(I32P),                     intent(in)    :: append
          integer(I32P),                     intent(inout) :: ierr
          ! Locals
          character(len=40)  :: sdate,stime
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          integer(I64P)      :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P)      :: jj,kk,ii
!DIR$     ENDIF
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .true. ) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:310, In->mod_wiparam/init: water_ice_thermodynamic_t already initialized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_wiparam/init:310,  water_ice_thermodynamic_t already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
              return
          end if
          ! Begin construction
          this%m_idx(:) = idx(:)
          ! Begin arrays allocation
          associate(d1s=>this%m_idx(1), & ! ims
                    d1e=>this%m_idx(2), & ! ime
                    d2s=>this%m_idx(3), & ! kms
                    d2e=>this%m_idx(4), & ! kme
                    d3s=>this%m_idx(5), & ! jms
                    d3e=>this%m_idx(6) )  ! jme
              allocate(this%m_ews(d1s:d1e,d2s:d2e,d3s:d3e),   &
                       this%m_eis(d1s:d1e,d2s:d2e,d3s:d3e),   &
                       this%m_cw(d1s:d1e,d2s:d2e,d3s:d3e),    &
                       this%m_cpim(d1s:d1e,d2s:d2e,d3s:d3e),  &
                       this%m_dcpm(d2s:d1e,d2s:d2e,d3s:d3e),  &
                       this%m_Lv(d1s:d1e,d2s:d2e,d3s:d3e),    &
                       this%m_Ls(d1s:d1e,d2s:d2e,d3s:d3e),    &
                       this%m_Lm(d1s:d1e,d2s:d2e,d3s:d3e),    &
                       this%m_gamwa(d1s:d1e,d3s:d3e),         &
                       this%m_gamwi(d1s:d1e,d3s:d3e),         &
                       this%m_gamiv(d1s:d1e,d3s:d3e),         &
                       this%m_psiw(d1s:d1e,d2s:d2e,d3s:d3e),  &
                       this%m_psii(d1s:d1e,d2s:d2e,d3s:d3e),  &
                       STAT=aerr,                             &
                       ERRMSG=emsg   )
         end associate
         if(aerr.NE.0) then
             if(logging) then
                 call log_startup(filename,append)
                 call log_UsrMsg("logger:355, In->mod_wiparam/init: Memory allocation failure")
                 call log_shutdown()
             else
                 call DATE_AND_TIME(date=sdate,time=stime)
                 write(ERROR_UNIT,*) "===========================FATAL========================="
                 write(ERROR_UNIT,*) "   (mod_wiparam/init:355, Memory allocation failure!)"
                 write(ERROR_UNIT,*) "   (System message:)", emsg
                 write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                 write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wiparam/init:355 -> [FATAL-ERROR]: Terminating execution!!"
         end if 
         ! Arrays initialization
!DIR$    IF (USE_LOOP_BLOCKING .EQ. 1)
         do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE ! jts,jte
            do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE ! kts,kte
                do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE ! its, ite
                    do jj = j, DEFAULT_BLOCK_SIZE
                        do kk = k, DEFAULT_BLOCK_SIZE
!DIR$                       SIMD VECTORLENGTHFOR(REAL(KIND=8))                            
                            do ii = i, DEFAULT_BLOCK_SIZE
                            this%m_ews(ii,kk,jj)   = 0._R64P
                            this%m_eis(ii,kk,jj)   = 0._R64P
                            this%m_cw(ii,kk,jj)    = 0._R64P
                            this%m_cpim(ii,kk,jj)  = 0._R64P
                            this%m_dcpm(ii,kk,jj)  = 0._R64P
                            this%m_Lv(ii,kk,jj)    = 0._R64P
                            this%m_Ls(ii,kk,jj)    = 0._R64P
                            this%m_Lm(ii,kk,jj)    = 0._R64P
                            this%m_gamwa(ii,jj)    = 0._R64P
                            this%m_gamwi(ii,jj)    = 0._R64P
                            this%m_gamiv(ii,jj)    = 0._R64P
                            this%m_psiw(ii,kk,jj)  = 0._R64P
                            this%m_psii(ii,kk,jj)  = 0._R64P
                            end do
                        end do
                    end do
                end do
            end do
         end do
!DIR$    ELSE
         do j = this%m_idx(17), this%m_idx(18)
             do k = this%m_idx(15), this%m_idx(16)
!DIR$            SIMD VECTORLENGTHFOR(REAL(KIND=8))                 
                 do i = this%m_idx(13), this%m_idx(14)
                        this%m_ews(i,k,j)   = 0._R64P
                        this%m_eis(i,k,j)   = 0._R64P
                        this%m_cw(i,k,j)    = 0._R64P
                        this%m_cpim(i,k,j)  = 0._R64P
                        this%m_dcpm(i,k,j)  = 0._R64P
                        this%m_Lv(i,k,j)    = 0._R64P
                        this%m_Ls(i,k,j)    = 0._R64P
                        this%m_Lm(i,k,j)    = 0._R64P
                        this%m_gamwa(i,j)   = 0._R64P
                        this%m_gamwi(i,j)   = 0._R64P
                        this%m_gamiv(i,j)   = 0._R64P
                        this%m_psiw(i,k,j)  = 0._R64P
                        this%m_psii(i,k,j)  = 0._R64P
                 end do
             end do
         end do
!DIR$    ENDIF
         this%m_isbuilt = .true.
    end subroutine
    
    !========================================================62
    !  @subroutine:
    !               copy
    !  @Purpose:
    !               Copy-constructs  derived type state
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !               
    !========================================================62
    subroutine copy(this,other,logging,filename,append,ierr)
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          class(water_ice_thermodynamic_t), intent(in)    :: other
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) ::  sdate,stime
          ! Start of executable statemetns
          if(ierr.LT.0) ierr = 0
          if(LOC(this).EQ.LOC(other) .OR. &
             this%m_isbuilt .EQ. .true. ) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:454, In->mod_wiparam/copy: Attempted self-assignment or invalid argument")
                  call log_shutdown()
              else
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_wiparam/copy:454, Attempted self-assignment or invalid argument !)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
              return
          end if
          ! Begin construction   
          this%m_idx(:) = other%m_idx(:)
          this%m_ews    = other%m_ews
          this%m_eis    = other%m_eis
          this%m_cw     = other%m_cw
          this%m_cpim   = other%m_cpim
          this%m_dcpm   = other%m_dcpm
          this%m_Lv     = other%m_Lv
          this%m_Ls     = other%m_Ls
          this%m_Lm     = other%m_Lm
          this%m_gamwa  = other%m_gamwa
          this%m_gamwi  = other%m_gamwi
          this%m_gamiv  = other%m_gamiv
          this%m_psiw   = other%m_psiw
          this%m_psii   = other%m_psii
          this%m_isbuilt = other5m_isbuilt
    end subroutine
    
    !========================================================62
    !  @subroutine:
    !               destroy
    !  @Purpose:
    !               Destroys (deallocates)  derived type arrays
    !               and sets isbuilt indicator to false.
    !               indexing static array is nullified.
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !               Upon detection of critical error
    !               'STOP' statement is executed.
    !========================================================62
    subroutine destroy(this,logging,filename,append,ierr)
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append
          integer(I32P),                    intent(in)    :: ierr
          ! Locals
          character(len=40)  :: sdate,stime
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                   call log_startup(filename,append)
                   call log_UsrMsg("logger:517, In->mod_wiparam/destroy: water_ice_thermodynamic_t already destroyed!!")
                   call log_shutdown()
              else
                   call DATE_AND_TIME(date=sdate,time=stime)
                   write(stderr,*) "===========================NON-FATAL=========================="
                   write(sdterr,*) " ( mod_wiparam/copy:517, water_ice_thermodynamic_t already destroyed!!)"
                   write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                   write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if
          ! Begin destruction
          this%m_idx(:) = 0
          deallocate(this%m_ews,   &
                     this%m_eis,   &
                     this%m_cw,    &
                     this%m_cpim,  &
                     this%m_dcpm,  &
                     this%m_Lv,    &
                     this%m_Ls,    &
                     this%m_Lm,    &
                     this%m_gamwa, &
                     this%m_gamwi, &
                     this%m_gamiv, &
                     this%m_psiw,  &
                     this%m_psii,  &
                     STAT=derr,    &
                     ERRMSG=emsg )
          if(derr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:551, In->mod_wiparam/destroy: Memory deallocation failure!!)
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_wiparam/destroy:551, Memory deallocation failure!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_wiparam/destroy:551 -> [FATAL-ERROR]: Terminating execution!!"
          end if 
          this%m_isbuilt = .false.
    end subroutine
    
    !============================================50
    !       Getters
    !============================================50
    
    pure function get_ews(this) result(ews)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: ews
!DIR$     ATTRIBUTES ALIGN : 32 :: ews
          ! Start of executable statements
          ews = this%m_ews
    end function
    
    pure function get_eis(this) result(eis)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: eis
!DIR$     ATTRIBUTES ALIGN : 32 :: eis
          ! Start of executable statements
          eis = this%m_eis
    end function
    
    pure function get_cw(this) result(cw)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: cw
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: cw
!DIR$     ATTRIBUTES ALIGN : 32 :: cw
          ! Start of executable sttements
          cw = this%m_cw
    end function
    
    pure function get_cpim(this) result(cpim)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: cpim
!DIR$     ATTRIBUTES ALIGN : 32 :: cpim
          ! Start of executable statements
          cpim = this%m_cpim
    end function
    
    pure function get_dcpm(this) result(dcpm)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: dcpm
!DIR$     ATTRIBUTES ALIGN : 32 :: dcpm
          ! Start of executable statements
          dcpm = this%m_dcpm
    end function
    
    pure function get_Lv(this) result(Lv)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: Lv
!DIR$     ATTRIBUTES ALIGN : 32 :: Lv
          ! Start of executable statements
          Lv = this%m_Lv
    end function
    
    pure function get_Ls(this) result(Ls)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: Ls
!DIR$     ATTRIBUTES ALIGN : 32 :: Ls
          ! Start of executable statements
          Ls = this%m_Ls
    end function
    
    pure function get_Lm(this) result(Lm)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: Lm
!DIR$     ATTRIBUTES ALIGN : 32 :: Lm
          ! Start of executable statements
          Lm = this%m_Lm
    end function
    
    pure function get_gamwa(this) result(gamwa)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: gamwa
!DIR$     ATTRIBUTES ALIGN : 32 :: gamwa
          ! Start of executable statements
          gamwa = this%m_gamwa
    end function
    
    pure function get_gamwi(this) result(gamwi)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: gamwi
!DIR$     ATTRIBUTES ALIGN : 32 :: gamwi
          ! Start of executable statements
          gamwi = this%m_gamwi
    end function
    
    pure function get_gamiv(this) result(gamiv)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: gamiv
!DIR$     ATTRIBUTES ALIGN : 32 :: gamiv
          ! Start of executable statements
          gamiv = this%m_gamiv
    end function
    
    pure function get_psiw(this) result(psiw)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: psiw
!DIR$     ATTRIBUTES ALIGN : 32 :: psiw
          ! Start of executable statements
          psiw = this%m_psiw
    end function
    
    pure function get_psii(this) result(psii)
          implicit none
          class(water_ice_thermodynamic_t), intent(in) :: psii
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: psii
!DIR$     ATTRIBUTES ALIGN : 32 :: psii
          ! Strat of executable statements
          psii = this%m_psii
    end function
    
    !======================================================60
    !               Computational procedures
    !======================================================60
    
    !========================================================62
    !  @subroutine:
    !               compute_ews
    !  @Purpose:
    !               Performs computation of saturated pressure
    !               of water.
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_ews.
    !               This is not checked.
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !               
    !========================================================62
    subroutine compute_ews(this,t3d,logging,filename,append, &
                           dbg,profiling,qpctimer,ierr )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          real(R64P) :: T1,T2,T3
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          ! Wexler (1976) calculated coefficients of saturated pressure of water  i.e. e_ws
          real(R64P), parameter, private :: a0 =  -2991.2729_R64P
          real(R64P), parameter, private :: a1 =  -6017.0128_R64P
          real(R64P), parameter, private :: a2 =   18.87643854_R64P
          real(R64P), parameter, private :: a3 =  -0.028354721_R64P
          real(R64P), parameter, private :: a4 =   0.000017838301_R64P
          real(R64P), parameter, private :: a5 =  -0.00000000084150417_R64P
          real(R64P), parameter, private :: a6 =   0.00000000000044412543_R64P
          real(R64P), parameter, private :: a7 =   2.858487_R64P
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(array3D_not_alloc(t3d) .OR. &
             this%m_isbuilt .EQ. .false. ) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:727, In->mod_wiparam/compute_ews: Unallocated allocatable array [m_t3d]")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_wiparam/compute_ews:727, Unallocated allocatable array [m_t3d])"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if
          T1 = 0._R64P
          T2 = 0._R64P
          T3 = 0._R64P
        if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_ews: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
              
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
              do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                  do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                      do jj = j, DEFAULT_BLOCK_SIZE
                          do kk = k, DEFAULT_BLOCK_SIZE
!DIR$                         SIMD VECTORLENGTHFOR(REAL(KIND=8))                              
                              do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                         IF (USE_SOFT_PREFETCH .EQ. 1)
                                  call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                         ENDIF
                               T1 = t3d(ii,kk,jj)
                               T2 = t3d(ii,kk,jj)
                               T3 = t3d(ii,kk,jj)
                               T1 = (a0*T1**-2)+(a1*T1**-1)
                               T2 = a2+(a3+T2*(a4+T2*(a5+T2*(a6+T2))))
                               T3 = a7*DLOG10(T3)
                               this%m_ews(ii,kk,jj) = DEXP(T1+T2+T3)
                              end do
                          end do
                      end do
                  end do
              end do
          end do
!DIR$     ELSE
          do j = this%m_idx(17), this%m_idx(18)   ! jts, jte
              do k = this%m_idx(15), this%m_idx(16)    ! kts, kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))      ! its, ite            
                  do i = this%m_idx(13), this%m_idx(14)
!DIR$             IF (USE_SOFT_PREFETCH .EQ. 1)
                      call MM_PREFETCH(t3d(i+2,k,j),1)
!DIR$             ENDIF
                      T1 = t3d(i,k,j)
                      T2 = t3d(i,k,j)
                      T3 = t3d(i,k,j)
                      T1 = (a0*T1**-2)+(a1*T1**-1)
                      T2 = a2+(a3+T2*(a4+T2*(a5+T2*(a6+T2))))
                      T3 = a7*DLOG10(T3)
                      this%m_ews(i,k,j) = DEXP(T1+T2+T3)
                  end do
              end do
          end do
!DIR$     ENDIF 
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_ews: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_ews: Unable to read performance counter -- fatal!!"
                end if
          end if
          if(dbg) then
              print*, "Fitt of ews(T): ", this%m_ews
          end if
    end subroutine
                           
    !========================================================62
    !  @subroutine:
    !               compute_eis
    !  @Purpose:
    !               Performs computation of saturated over
    !               ice vapour pressure.
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_ews.
    !               This is not checked.
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !               
    !========================================================62                       
    subroutine compute_eis(this,t3d,logging,filename,append, &
                           dbg,profiling,qpctimer,ierr  )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          real(R64P) :: T1,T2,T3
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          ! Hyland and Wexler (1983) coefficients of saturated over ice vapour pressure i.e. e_is
          ! for T (K) range 173.16<T<273.16
          real(R64P), parameter, private :: is_a0 = -5674.5359_R64P
          real(R64P), parameter, private :: is_a1 = 6.3925247_R64P
          real(R64P), parameter, private :: is_a2 = -0.009677843_R64P
          real(R64P), parameter, private :: is_a3 =  0.00000062215701_R64P
          real(R64P), parameter, private :: is_a4 =  0.0000000020747825_R64P
          real(R64P), parameter, private :: is_a5 =  -0.0000000000009484024_R64P
          real(R64P), parameter, private :: is_a6 =  4.1635019_R64P
          ! Murphy and Koop (2005) parametrization of e_is for T(K) > 110K
          real(R64P), parameter, private :: is2_a0 = 9.550426_R64P
          real(R64P), parameter, private :: is2_a1 = -5723.265_R64P
          real(R64P), parameter, private :: is2_a2 = 3.53068_R64P
          real(R64P), parameter, private :: is2_a3 = -0.00728332_R64P
          ! Start of executable statements
          if(err.LT.0) ierr = 0
          if(array3D_not_alloc(t3d) .OR. &
             this%m_isbuilt .EQ. .false. ) then
             if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:895, In->mod_wiparam/compute_eis: Unallocated allocatable array [m_t3d]")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_wiparam/compute_eis:895, Unallocated allocatable array [m_t3d])"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
        end if
        T1 = 0._R64P
        T2 = 0._R64P
        T3 = 0._R64P
         if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_eis: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
!DIR$   IF (USE_LOOP_BLOCKING .EQ. 1)
        do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
            do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                    do jj = j, DEFAULT_BLOCK_SIZE
                        do kk = k, DEFAULT_BLOCK_SIZE
                            do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                       IF (USE_SOFT_PREFETCH .EQ. 1)
                                call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                       ENDIF
                                if(t3d(ii,kk,jj)>=173.16_R64P .AND. t3d(ii,kk,jj)<=273.16_R64P) then
                                    T1 = t3d(ii,kk,jj)
                                    T2 = t3d(ii,kk,jj)
                                    T3 = t3d(ii,kk,jj)
                                    T1 = is_a0/T1
                                    T2 = is_a1+(is_a2+T2*(is_a3+T2*(is_a4+T2*(is_a5+T2))))
                                    T3 = is_a6*DLOG10(T3)
                                    this%m_eis(ii,kk,jj) = DEXP(T1+T2+T3)
                                else if(t3d(ii,kk,jj)>110._R64P .AND. t3d(ii,kk,jj)<173.16_R64P) then
                                     T1 = t3d(ii,kk,jj)
                                     T2 = t3d(ii,kk,jj)
                                     T1 = is2_a0+(is2_a1/T1)
                                     T2 = (is2_a2*DLOG(T2))+(is2_a3*T2)
                                     this%m_eis(ii,kk,jj) = DEXP(T1+T2)
                                end if
                            end do
                        end do
                    end do
                end do
            end do
        end do
!DIR$   ELSE
        do j = this%m_idx(17), this%m_idx(18)
            do k = this%m_idx(15), this%m_idx(16)
                do i = this%m_idx(13), this%m_idx(14)
!DIR$           IF (USE_SOFT_PREFETCH .EQ. 1)
                    call MM_PREFETCH(t3d(i+2,k,j),1)
!DIR$           ENDIF
                      if(t3d(i,k,j)>=173.16_R64P .AND. t3d(i,k,j)<=273.16_R64P) then
                         T1 = t3d(i,k,j)
                         T2 = t3d(i,k,j)
                         T3 = t3d(i,k,j)
                         T1 = is_a0/T1
                         T2 = is_a1+(is_a2+T2*(is_a3+T2*(is_a4+T2*(is_a5+T2))))
                         T3 = is_a6*DLOG10(T3)
                         this%m_eis(i,k,j) = DEXP(T1+T2+T3)
                      else if(t3d(i,k,j)>110._R64P .AND. t3d(i,k,j)<173.16_R64P) then
                          T1 = t3d(ii,kk,jj)
                          T2 = t3d(ii,kk,jj)
                          T1 = is2_a0+(is2_a1/T1)
                          T2 = (is2_a2*DLOG(T2))+(is2_a3*T2)
                          this%m_eis(i,k,j) = DEXP(T1+T2)
                      end if
                end do
            end do
        end do
         if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_eis: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_eis: Unable to read performance counter -- fatal!!"
                end if
          end if
          if(dbg) then
              print*, "Fitting of m_eis: ", this%m_eis
          end if
                           end subroutine
    
    !========================================================62
    !  @subroutine:
    !               compute_cw
    !  @Purpose:
    !               Performs computation of water heat
    !               capacity
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_ews.
    !               This is not checked.
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62
    subroutine compute_cw(this,t3d,logging,filename,append, &
                          dbg,profiling,qpctimer,ierr  )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          real(R64P) :: T1,T2
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P), parameter :: cw0 = 0.9979_R64P,       &
                                   a1  = 0.0000031_R64P,    &
                                   a2  = 0.0000000038_R64P, &
                                   Tc1 = 35.0_R64P
          real(R64P), parameter :: a00 = 1.000938_R64P,      &
                                   a11 = -0.0270052_R64P,    &
                                   a22 = -0.000023235_R64P,  &
                                   a33 =  0.0000043778_R64P, &
                                   a44 =  0.00000027316_R64P
          ! Start of executable satements
          if(ierr.LT.0) ierr = 0
          if(array3D_not_alloc(t3d) .OR. &
             this%m_isbuilt .EQ. .false. ) then
                if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:1040, In->mod_wiparam/compute_cw: Unallocated allocatble array [m_t3d]")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_cw:1040, Unallocated allocatable array [m_t3d])"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
         end if
         T1 = 0._R64P
         T2 = 0._R64P
         if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_cw: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
!DIR$    IF (USE_LOOP_BLOCKING .EQ. 1)
         do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
             do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                 do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                     do jj = j, DEFAULT_BLOCK_SIZE
                         do kk = k, DEFAULT_BLOCK_SIZE
                             do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                       IF (USE_SOFT_PREFETCH .EQ. 1)
                                 call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                       ENDIF
                                 if(t3d(ii,kk,jj)>=0._R64P .AND. t3d(ii,kk,jj)<=Tc1) then
                                     T1 = a1*(t3d(ii,kk,jj)-Tc1)**2
                                     T2 = a2*(t3d(ii,kk,jj)-Tc1)**4
                                     this%m_cw(ii,kk,jj) = cw0+T1+T2
                                 else if(t3d(ii,kk,jj)>= -37._R64P .AND. t3d(ii,kk,jj)<=0._R64P) then
                                     T1 = t3d(ii,kk,jj)
                                     this%m_cw(ii,kk,jj) = a00+T1*(a11+T1*(a22+T1*(a33+T1)))
                                 end if
                             end do
                         end do
                     end do
                 end do
             end do
         end do
!DIR$    ELSE
         do j = this%m_idx(17), this%m_idx(18)
             do k = this%m_idx(15), this%m_idx(16)
                 do i = this%m_idx(13), this%m_idx(14)
!DIR$            IF (USE_SOFT_PREFETCH .EQ. 1)
                     call MM_PREFETCH(t3d(i+2,k,j),1)
!DIR$            ENDIF
                       if(t3d(i,k,j)>=0._R64P .AND. t3d(i,k,j)<=Tc1) then
                          T1 = a1*(t3d(i,k,j)-Tc1)**2
                          T2 = a2*(t3d(i,k,j)-Tc1)**4
                          this%m_cw(i,k,j) = cw0+T1+T2
                       else if(t3d(i,k,j)>= -37._R64P .AND. t3d(i,k,j)<=0._R64P) then
                          T1 = t3d(i,k,j)
                          this%m_cw(i,k,j) = a00+T1*(a11+T1*(a22+T1*(a33+T1)))
                       end if
                 end do
             end do
         end do
         if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_cw: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_cw: Unable to read performance counter -- fatal!!"
                end if
         end if
         if(dbg) then
             print*, "Fitting of cw: ", this%m_cw
         end if
    end subroutine
                          
    !========================================================62
    !  @subroutine:
    !               compute_cpim
    !  @Purpose:
    !               Performs computation of molar ice heat capacity.
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_cpim.
    !               This is not checked.
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62                      
    subroutine compute_cpim(this,t3d,logging,filename,append, &
                            dbg,profiling,qpctimer,ierr  )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          real(R64P) :: T1,T2
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P), parameter :: a1 = -2.0572_R64P
          real(R64P), parameter :: a2 = 0.14644_R64P
          real(R64P), parameter :: a3 = 0.06163_R64P
          real(R64P), parameter :: a4 = 125.1_R64P
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false. .OR. &
             array3D_not_alloc(t3d)    ) then
                if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:1167, In->mod_wiparam/compute_cpim: Unallocated allocatble array [m_t3d]")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_cpim:1167, Unallocated allocatable array [m_t3d])"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if
          T1 = 0._R64P
          T2 = 0._R64P
         if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_cpim: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
              do k = this%m_idx(15), this%m_idx(16), DEFUALT_BLOCK_SIZE
                  do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                      do jj = j, DEFAULT_BLOCK_SIZE
                          do kk = k, DEFAULT_BLOCK_SIZE
                              do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                         IF (USE_SOFT_PREFETCH .EQ. 1)
                                  call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                         ENDIF
                                  T1 = t3d(ii,kk,jj)
                                  T2 = t3d(ii,kk,jj)
                                  T1 = a1+a2*T1+a3*T1
                                  T2 = DEXP(-(T2/a4)**2)
                                  this%m_cpim(ii,kk,jj) = T1+T2
                              end do
                          end do
                      end do
                  end do
              end do
          end do
!DIR$     ELSE
          do j = this%m_idx(17), this%m_idx(18)
              do k = this%m_idx(15), this%m_idx(16)
                  do i = this%m_idx(13), this%m_idx(14)
!DIR$                         IF (USE_SOFT_PREFETCH .EQ. 1)
                                  call MM_PREFETCH(t3d(i+2,k,j),1)
!DIR$                         ENDIF
                                  T1 = t3d(i,k,j)
                                  T2 = t3d(i,k,j)
                                  T1 = a1+a2*T1+a3*T1
                                  T2 = DEXP(-(T2/a4)**2)
                                  this%m_cpim(i,k,j) = T1+T2   
                  end do
              end do
          end do
!DIR$     ENDIF
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_cpim: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_cpim: Unable to read performance counter -- fatal!!"
                end if
          end if
          if(dbg) then
              print*, "Fitting of cpim: ", this%m_cpim
         end if
    end subroutine
                            
    !========================================================62
    !  @subroutine:
    !               compute_dcpm
    !  @Purpose:
    !               Performs computation of difference in
    !               in molar heat capacity between vapour and ice 
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_dcpm.
    !               This is not checked.
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62  
    subroutine compute_dcpm(this,t3d,logging,filename,append, &
                            dbg,profiling,qpctimer,ierr )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          real(R64P) :: T1,T2
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P), parameter :: a1 = -35.319_R64P
          real(R64P), parameter :: a2 = 0.14457_R64P
          real(R64P), parameter :: a3 = 0.06155_R64P
          real(R64P), parameter :: a4 = 129.85_R64P
          ! Strat of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false. .OR. &
             array3D_not_alloc(t3d)    ) then
                if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:1290, In->mod_wiparam/compute_dcpm: Unallocated allocatble array [m_t3d]")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_dcpm:1290, Unallocated allocatable array [m_t3d])"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if
          T1 = 0._R64P
          T2 = 0._R64P
         if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_dcpm: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
              do k = this%m_idx(15), this%m_idx(16), DEFUALT_BLOCK_SIZE
                  do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                      do jj = j, DEFAULT_BLOCK_SIZE
                          do kk = k, DEFAULT_BLOCK_SIZE
                              do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                         IF (USE_SOFT_PREFETCH .EQ. 1)
                                  call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                         ENDIF
                                  T1 = t3d(ii,kk,jj)
                                  T2 = t3d(ii,kk,jj)
                                  T1 = a1+a2*T1+a3*T1
                                  T2 = DEXP(-(T2/a4)**2)
                                  this%m_cpim(ii,kk,jj) = T1+T2
                              end do
                          end do
                      end do
                  end do
              end do
          end do
!DIR$     ELSE
          do j = this%m_idx(17), this%m_idx(18)
              do k = this%m_idx(15), this%m_idx(16)
                  do i = this%m_idx(13), this%m_idx(14)
!DIR$                         IF (USE_SOFT_PREFETCH .EQ. 1)
                                  call MM_PREFETCH(t3d(i+2,k,j),1)
!DIR$                         ENDIF
                                  T1 = t3d(i,k,j)
                                  T2 = t3d(i,k,j)
                                  T1 = a1+a2*T1+a3*T1
                                  T2 = DEXP(-(T2/a4)**2)
                                  this%m_cpim(i,k,j) = T1+T2   
                  end do
              end do
          end do
!DIR$     ENDIF
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_dcpm: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_dcpm: Unable to read performance counter -- fatal!!"
                end if
          end if
          if(dbg) then
              print*, "Fitting of dcpm: ", this%m_dcpm
         end if
    end subroutine
                            
    !========================================================62
    !  @subroutine:
    !               compute_Lv
    !  @Purpose:
    !               Performs computation of evaporation latent heat
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_dcpm.
    !               This is not checked.
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62                          
    subroutine compute_Lv(this,t3d,logging,filename,append,dbg, &
                          profiling,qpctimer,ierr    )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          real(R64P) :: tmp
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P), parameter :: c1 = 597.3_R64P, &
                                   c2 = 0.561_R64P
          real(R64P), parameter :: a0 = -1412.3_R64P,   &
                                   a1 = -338.82_R64P,   &
                                   a2 = -122.347_R64P,  &
                                   a3 = -0.7526_R64P,   &
                                   a4 = -0.011595_R64P, &
                                   a5 = -0.00007313_R64P
          real(R64P), parameter :: t1 = -44._R64P, &   ! All those constants(t1,t2,t3) expresed in Celsius
                                   t2 = -20._R64P, &
                                   t3 =  40._R64P
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false. .OR. &
             array3D_not_alloc(t3d)      ) then
               if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:1392, In->mod_wiparam/compute_Lv: Unallocated allocatble array [m_t3d]")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_Lv:1392, Unallocated allocatable array [m_t3d])"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
         end if
         tmp = 0._R64P
         if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_Lv: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
!DIR$    IF (USE_LOOP_BLOCKING .EQ. 1)
         do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
             do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                 do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                     do jj = j, DEFAULT_BLOCK_SIZE
                         do kk = k, DEFAULT_BLOCK_SIZE
                             do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                        IF (USE_SOFT_PREFETCH .EQ. 1)
                                 call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                        ENDIF                                 
                                 if(t3d(ii,kk,jj)>=t1 .AND. t3d(ii,kk,jj)<=t2) then
                                      tmp = t3d(ii,kk,jj)
                                      tmp = a0+tmp*(a1+tmp*(a2+tmp*(a3+tmp*(a4+tmp*(a5+tmp)))))
                                      this%m_Lv(ii,kk,jj) = tmp
                                 else if(t3d(ii,kk,jj)>t2 .AND. t3d(ii,kk,jj)<=t3) then
                                      this%m_Lv(ii,kk,jj) = c1-c2*t3d(ii,kk,jj)
                                 end if
                             end do
                         end do
                     end do
                 end do
             end do
         end do
!DIR$    ELSE
         do j = this%m_idx(17), this%m_idx(18)
             do k = this%m_idx(15), this%m_idx(16)
                 do i = this%m_idx(13), this%m_idx(14)
!DIR$            IF (USE_SOFT_PREFETCH .EQ. 1)
                     call MM_PREFETCH(t3d(i+2,k,j),1)
!DIR$            ENDIF                                 
                      if(t3d(i,k,j)>=t1 .AND. t3d(i,k,j)<=t2) then
                          tmp = t3d(i,k,j)
                          tmp = a0+tmp*(a1+tmp*(a2+tmp*(a3+tmp*(a4+tmp*(a5+tmp)))))
                          this%m_Lv(i,k,j) = tmp
                      else if(t3d(i,k,j)>t2 .AND. t3d(i,k,j)<=t3) then
                          this%m_Lv(i,k,j) = c1-c2*t3d(i,k,j)
                      end if   
                 end do
             end do
         end do
!DIR$    ENDIF
         if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_Lv: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_Lv: Unable to read performance counter -- fatal!!"
                end if
          end if
          if(dbg) then
              print*, "Fitting of Lv: ", this%m_Lv
          end if
    end subroutine
                          
    !========================================================62
    !  @subroutine:
    !               compute_Ls
    !  @Purpose:
    !               Performs computation of sublimation
    !               molar latent heat.       
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_dcpm.
    !               This is not checked.
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62 
    subroutine compute_Ls(this,t3d,logging,filename,append, &
                          dbg,profiling,qpctimer,ierr  )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          character(len=40) :: sdate,stime
          real(R64P) :: tmp1,tmp2
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P), parameter :: b0 = 46782.5_R64P, &
                                   b1 = 35.8925_R64P, &
                                   b2 = -0.07414_R64P, &
                                   b3 = 541.5_R64P,    &
                                   b4 = 0.00808080808080808080808_R64P
         !Start of executable statemetns
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false. .OR. &
             array3D_not_alloc(t3d)      ) then
               if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:1521, In->mod_wiparam/compute_Ls: Unallocated allocatble array [t3d]")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_Ls:1521, Unallocated allocatable array [t3d])"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if
          tmp1 = 0._R64P
          tmp2 = 0._R64P
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_Ls: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_idx(17), this%m_idx(18), DEFAULT_LOOP_BLOCKING
              do k = this%m_idx(15), this%m_idx(16), DEFAULT_LOOP_BLOCKING
                  do k = this%m_idx(13), this%m_idx(14), DEFAULT_LOOP_BLOCKING
                      do jj = j, DEFAULT_LOOP_BLOCKING
                          do kk = k, DEFAULT_LOOP_BLOCKING
                              do ii = i, DEFAULT_LOOP_BLOCKING
!DIR$                         IF (USE_SOFT_PREFETCH .EQ. 1)
                                  call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                         ENDIF
                                  tmp1 = t3d(ii,kk,jj)
                                  tmp2 = t3d(ii,kk,jj)
                                  tmp1 = b0+b1+tmp1*(b2+tmp2)
                                  tmp2 = b3*DEXP(-(tmp2*b4)**2)
                                  this%m_Ls(ii,kk,jj) = tmp1+tmp2
                              end do
                          end do
                      end do
                  end do
              end do
          end do
!DIR$     ELSE
          do j = this%m_idx(17), this%m_idx(18)
              do k = this%m_idx(15), this%m_idx(16)
                  do i = this%m_idx(13), this%m_idx(14)
 !DIR$              IF (USE_SOFT_PREFETCH .EQ. 1)
                        call MM_PREFETCH(t3d(i+2,k,j),1)
!DIR$               ENDIF
                        tmp1 = t3d(i,k,j)
                        tmp2 = t3d(i,k,j)
                        tmp1 = b0+b1+tmp1*(b2+tmp2)
                        tmp2 = b3*DEXP(-(tmp2*b4)**2)
                        this%m_Ls(i,k,j) = tmp1+tmp2  
                  end do
              end do
          end do
!DIR$     ENDIF
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_Ls: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_Ls: Unable to read performance counter -- fatal!!"
                end if
          end if
          if(dbg) then
              print*, "Fitting of Ls: ", this%m_Ls
          end if
    end subroutine
                          
    !========================================================62
    !  @subroutine:
    !               compute_Lm
    !  @Purpose:
    !               Performs computation of melting heat point.
    !                    
    !               Temperature parameter array 't3d' must
    !               fully conform to member array m_dcpm.
    !               This is not checked.
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62                        
    subroutine compute_Lm(this,t3d,logging,filename,append, &
                          dbg,profiling,qpctimer,ierr)
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          real(R64P) :: tmp1
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P), parameter :: a0 = 79.7_R64P, &
                                   a1 = -0.12_R64P,&
                                   a2 = -0.080481_R64P, &
                                   a3 = -0.0032376_R64P, &
                                   a4 = -0.0000425533_R64P
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false. .OR. &
             array3D_not_alloc(t3d)      ) then
               if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:1645, In->mod_wiparam/compute_L,: Unallocated allocatble array [t3d]")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_Lm:1645, Unallocated allocatable array [t3d])"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
         end if
         tmp1 = 0._R64P
         if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_Lm: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
!DIR$    IF (USE_LOOP_BLOCKING .EQ. 1)
         do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
             do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                 do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                     do jj = j, DEFAULT_BLOCK_SIZE
                         do kk = k, DEFAULT_BLOCK_SIZE
                             do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                        IF (USE_SOFT_PREFETCH .EQ. 1)
                                 call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                        ENDIF
                                 tmp1 = t3d(ii,kk,jj)
                                 tmp1 = a0+tmp1*(a1+tmp1*(a2+tmp1*(a3+tmp1*(a4+tmp1))))
                                 this%m_Lm(ii,kk,jj) = tmp1
                             end do
                         end do
                     end do
                 end do
             end do
         end do
!DIR$    ELSE
         do j = this%m_idx(17), this%m_idx(18)
             do k = this%m_idx(15), this%m_idx(16)
                 do i = this%m_idx(13), this%m_idx(14)
!DIR$           IF (USE_SOFT_PREFETCH .EQ. 1)
                     call MM_PREFETCH(t3d(i+2,k,j),1)
!DIR$           ENDIF
                     tmp1 = t3d(i,k,j)
                     tmp1 = a0+tmp1*(a1+tmp1*(a2+tmp1*(a3+tmp1*(a4+tmp1))))
                     this%m_Lm(i,k,j) = tmp1      
                 end do
             end do
         end do
!DIR$    ENDIF
         if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_Lm: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_Lm: Unable to read performance counter -- fatal!!"
                end if
         end if
         if(dbg) then
             print*, "Fitting of Lm: ", this%m_Lm
         end if
    end subroutine
                          
    !========================================================62
    !  @subroutine:
    !               compute_gamwa
    !  @Purpose:
    !               Performs computation of surface tension
    !               betweem water and air.     
    !               
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62  
    subroutine compute_gamwa(this,t3d,logging,filename,append, &
                             dbg,profiling,qpctimer,ierr )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          real(R64P) :: tmp1
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P), parameter :: a0 = 75.93_R64P, &
                                   a1 = 0.115_R64P, &
                                   a2 = 0.06818_R64P, &
                                   a3 = 0.006511_R64P, &
                                   a4 = 0.0002933_R64P, &
                                   a5 = 0.000006283_R64P, &
                                   a6 = 0.00000005285_R64P
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false. .OR. &
             array3D_not_alloc(t3d)      ) then
               if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:1766, In->mod_wiparam/compute_gamwa,: Unallocated allocatble array [t3d]")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_gamwa:1766, Unallocated allocatable array [t3d])"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if
          tmp1 = 0._R64P
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_gamwa: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
              do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                  do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                      do jj = j, DEFAULT_BLOCK_SIZE
                          do kk = k, DEFAULT_BLOCK_SIZE
                              do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                        IF (USE_SOFT_PREFETCH .EQ. 1)
                                  call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                        ENDIF
                                  tmp1 = t3d(ii,kk,jj)
                                  tmp1 = a0+tmp1*(a1+tmp1*(a2+tmp1*(a3+tmp1*(a4+tmp1*(a5+tmp1*(a6+tmp1))))))
                                  this%m_gamwa(ii,jj) = tmp1
                              end do
                          end do
                      end do
                  end do
              end do
          end do
!DIR$     ELSE
          do j = this%m_idx(17), this%m_idx(18)
              do k = this%m_idx(15), this%m_idx(16)
                  do i = this%m_idx(13), this%m_idx(14)
!DIR$           IF (USE_SOFT_PREFETCH .EQ. 1)
                     call MM_PREFETCH(t3d(i+2,k,j),1)
!DIR$           ENDIF
                     tmp1 = t3d(i,k,j)
                     tmp1 = a0+tmp1*(a1+tmp1*(a2+tmp1*(a3+tmp1*(a4+tmp1*(a5+tmp1*(a6+tmp1))))))
                     this%m_gamwa(i,j) = tmp1  
                  end do
              end do
          end do
!DIR$     ENDIF
         if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_gamwa: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_gamwa: Unable to read performance counter -- fatal!!"
                end if
         end if
         if(dbg) then
             print*, "Fitting of gamwa: ", this%m_gamwa
         end if
    end subroutine
    
    !========================================================62
    !  @subroutine:
    !               compute_gamiw
    !  @Purpose:
    !               Performs computation of surface tension
    !               betweem water and ice.     
    !              
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62 
    subroutine compute_gamiw(this,t3d,logging,filename,append, &
                             dbg,profiling,qpctimer,ierr  )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          real(R64P) :: tmp1
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P), parameter :: c0 = 28._R64P, &
                                   c1 = 0.25_R64P
          real(R64P), parameter :: a0 = 189.081_R64P, &
                                   a1 = 13.1625_R64P, &
                                   a2 = 0.3469_R64P,  &
                                   a3 = 0.003125_R64P
          real(R64P), parameter :: t1 = -36._R64P, &
                                   t2 = 0._R64P,   &
                                   t3 = -44._R64P
          ! Start of executable satements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false. .OR. &
             array3D_not_alloc(t3d)      ) then
               if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:1889, In->mod_wiparam/compute_gamiw,: Unallocated allocatble array [t3d]")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_gamiw:1889, Unallocated allocatable array [t3d])"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if
          tmp1 = 0._R64P
         if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_gamiw: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
!DIR$      IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
              do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                  do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                      do jj = j, DEFAULT_BLOCK_SIZE
                          do kk = k, DEFAULT_BLOCK_SIZE
                              do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                         IF (USE_SOFT_PREFETCH .EQ. 1)
                                  call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                         ENDIF
                                  if(t3d(ii,kk,jj)>=t1 .AND. t3d(ii,kk,jj)<=t2) then
                                      this%m_gamiw(ii,jj) = c0+c1*t3d(ii,kk,jj)
                                  else if(t3d(ii,kk,jj)>=t3 .AND. t3d(ii,kk,jj)<=t1) then
                                      tmp1 = t3d(ii,kk,jj)
                                      tmp1 = a0+a1+tmp1*(a2+tmp1*(a3+tmp1))
                                      this%m_gamiw(ii,jj) = tmp1
                                  end if
                              end do
                          end do
                      end do
                  end do
              end do
          end do
!DIR$     ELSE
          do j = this%m_idx(17), this%m_idx(18)
              do k = this%m_idx(15), this%m_idx(16)
                  do i = this%m_idx(13), this%m_idx(14)
!DIR$           IF (USE_SOFT_PREFETCH .EQ. 1)
                       call MM_PREFETCH(t3d(i+2,k,j),1)
!DIR$           ENDIF
                       if(t3d(i,k,j)>=t1 .AND. t3d(i,k,j)<=t2) then
                            this%m_gamiw(i,j) = c0+c1*t3d(i,k,j)
                       else if(t3d(i,k,j)>=t3 .AND. t3d(i,k,j)<=t1) then
                             tmp1 = t3d(i,k,j)
                             tmp1 = a0+a1+tmp1*(a2+tmp1*(a3+tmp1))
                             this%m_gamiw(i,j) = tmp1
                       end if  
                  end do
              end do
          end do
!DIR$     ENDIF
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_gamiw: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_gamiw: Unable to read performance counter -- fatal!!"
                end if
          end if
          if(dbg) then
              print*, "Fitting of gamiw: ", this%m_gamiw
          end if
    end subroutine
                             
    !========================================================62
    !  @subroutine:
    !               compute_gamiv
    !  @Purpose:
    !               Performs computation of surface tension
    !               betweem ice and air or vapour.     
    !               This subroutine must be called only
    !               after saubroutines:
    !               1) compute_gamwa
    !               2) compute_gamiw
    !               finished to execute successfully,
    !               otherwise result will be obvioulsy wrong.          
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62 
    subroutine compute_gamiv(this,logging,filename,append,  &
                             dbg,profiling,qpctimer,ierr  )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
         
          integer(I64P) :: j,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false.)  then
               
               if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:2008, In->mod_wiparam/compute_gamiv,: water_ice_thermodynamic_t in invalid state")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_gamv:2008, water_ice_thermodynamic_t in invalid state)"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_gamiv: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
!DIR$    IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
              do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                  do jj = j, DEFAULT_BLOCK_SIZE
!DIR$                 SIMD VECTORLENGTHFOR(REAL(KIND=8))                      
                      do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                 IF (USE_SOFT_PREFETCH .EQ. 1)
                          call MM_PREFETCH(this%m_gamwa(ii+2,jj),1)
                          call MM_PREFETCH(this%m_gamiw(ii+2,jj),1)
!DIR$                 ENDIF
                          this%m_gamiv(ii,jj) = this%m_gamwa(ii,jj) + &
                                                this%m_gamiw(ii,jj)
                      end do
                  end do
              end do
          enddo
!DIR$     ELSE
          do j = this%m_idx(17), this%m_idx(18)
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))              
              do i = this%m_idx(13), this%m_idx(14)
!DIR$         IF (USE_SOFT_PREFETCH .EQ. 1)
                  call MM_PREFETCH(this%m_gamwa(i+2,j),1)
                  call MM_PREFETCH(this%m_gamiw(i+2,j),1)
!DIR$         ENDIF
                  this%m_gamiv(i,j) = this%m_gamwa(i,j) + &
                                      this%m_gamiw(i,j) 
              end do
          end do
!DIR$     ENDIF
          if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_gamiV: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_gamiV: Unable to read performance counter -- fatal!!"
                end if
          end if
          if(dbg) then
              print*, "Fitting of m_gamiv: ", this%m_gamiv
          end if
   end subroutine
                             
     
    !========================================================62
    !  @subroutine:
    !               compute_psiw
    !  @Purpose:
    !               Performs computation of water density   
    !               at pressure = 1 atm.
    !               Parameter array 't3d' i.e. temperature
    !                of water must conform to array m_psiw.        
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62                           
    subroutine compute_psiw(this,t3d,logging,filename,append, &
                            dbg,profiling,qpctimer,ierr  )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! Locals
          character(len=40) :: sdate,stime
          real(R64P)    :: tmp1,tmp2
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P), parameter :: t33 = -33._R64P, &
                                   t0  = 0._R64P,   &
                                   t100 = 100._R64P
          real(R64P), parameter :: a0 = 0.9998396_R64P, &
                                   a1 = 0.018224944_R64P, &
                                   a2 = -0.000007922210_R64P, &
                                   a3 = -0.00000005544846_R64P, &
                                   a4 = 0.0000000001497562_R64P, &
                                   a5 = -0.0000000000003932952_R64P
                                   Bpw = 0.018159725_R64P
          real(R64P), parameter :: a00 = 0.99986_R64P, &
                                   a11 = 0.00006690_R64P, &
                                   a22 = -0.000008486_R64P, &
                                   a33 = 0.0000001518_R64P, &
                                   a44 = -0.0000000069984_R64P, &
                                   a55 = -0.00000000036449_R64P, &
                                   a66 = -0.000000000007497_R64P
           ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false. .OR. &
             array3D_not_alloc(t3d)      ) then
               if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:2134, In->mod_wiparam/compute_psiw,: Unallocated allocatble array [t3d]")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_psiw:2134, Unallocated allocatable array [t3d])"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
         end if 
         if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_psiw: qpctimer_start failed to query performance frequency counter!"
              end if
          end if   
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
              do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                  do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                      do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                          do jj = j, DEFAULT_BLOCK_SIZE
                              do kk = k, DEFAULT_BLOCK_SIZE
                                  do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                            IF (USE_SOFT_PREFETCH .EQ. 1)
                                      call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                            ENDIF
                                      if(t3d(ii,kk,jj)>=t33 .AND. t3d(ii,kk,jj)<=t0) then
                                          tmp1 = t3d(ii,kk,jj)
                                          tmp1 = a00+tmp1*(a11+tmp1*(a22+tmp1*(a33+tmp1*(a44+tmp1*(a55+tmp1*(a66+tmp1))))))
                                          this%m_psiw(ii,kk,jj) = tmp1
                                      else if(t3d(ii,kk,jj)>=t0 .AND. t3d(ii,kk,jj)<t100) then
                                          tmp1 = 1._R64P/(1._R64P+Bpw*t3d(ii,kk,jj))
                                          tmp2 = t3d(ii,kk,jj)
                                          tmp2 = a0+tmp2*(a1+tmp2*(a2+tmp2*(a3+tmp2*(a4+tmp2*(a5+tmp2)))))
                                          this%m_psiw(ii,kk,jj) = tmp1*tmp2
                                      end if
                                  end do
                              end do
                          end do
                      end do
                  end do
              end do
!DIR$         ELSE
              do j = this%m_idx(17), this%m_idx(18)
                  do k = this%m_idx(15), this%m_idx(16)
                      do i = this%m_idx(13), this%m_idx(14)
!DIR$                 IF (USE_SOFT_PREFETCH .EQ. 1)
                          call MM_PREFETCH(t3d(i+2,k,j),1)
!DIR$                 ENDIF
                          if(t3d(i,k,j)>=t33 .AND. t3d(i,k,j)<=t0) then
                              tmp1 = t3d(i,k,j)
                              tmp1 = a00+tmp1*(a11+tmp1*(a22+tmp1*(a33+tmp1*(a44+tmp1*(a55+tmp1*(a66+tmp1))))))
                              this%m_psiw(i,k,j) = tmp1
                          else if(t3d(i,k,j)>=t0 .AND. t3d(i,k,j)<t100) then
                              tmp1 = 1._R64P/(1._R64P+Bpw*t3d(i,k,j))
                              tmp2 = t3d(i,k,j)
                              tmp2 = a0+tmp2*(a1+tmp2*(a2+tmp2*(a3+tmp2*(a4+tmp2*(a5+tmp2)))))
                              this%m_psiw(i,k,j) = tmp1*tmp2
                          end if 
                      end do
                  end do
              end do
              if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_psiw: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_psiw: Unable to read performance counter -- fatal!!"
                end if
              end if
              if(dbg) then
                  print*, "Fitting of psiw: ", this%m_psiw
              end if
    end subroutine
                            
    !========================================================62
    !  @subroutine:
    !               compute_psii
    !  @Purpose:
    !               Performs computation of ice density   
    !               
    !               Parameter array 't3d' i.e. reduced temperature
    !                of ice must conform to array m_psii.        
    !  @Warning:
    !               Upon detection of non-critical error
    !               integeral error indicator will be set
    !               to -1 and early exit (return) will be
    !               executed.
    !              
    !========================================================62  
    subroutine compute_psii(this,t3d,logging,filename,append, &
                            dbg,profiling,qpctimer,ierr  )
          implicit none
          class(water_ice_thermodynamic_t), intent(inout) :: this
          real(R64P), dimension(:,:,:),     intent(in)    :: t3d
!DIR$     ASSUME_ALIGNED t3d:32
          logical(I32P),                    intent(in)    :: logging
          character(len=*),                 intent(in)    :: filename
          logical(I32P),                    intent(in)    :: append,dbg,profiling
          type(QPCTimer_t),                 intent(inout) :: qpctimer
          integer(I32P),                    intent(inout) :: ierr
          ! llocals
          character(len=40) :: sdate,stime
          real(R64P)    :: Tr 
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(BOOL) :: ifail
          logical(I32P) :: bfail
          real(R64P), parameter :: pi0 = 0.9167_R64P
          real(R64P), parameter :: T0  = 0.00366099212886692293611568735127_R64P
          real(R64P), parameter :: av0 = 1._R64P, &
                                   av1 = -0.05294_R64P, &
                                   av2 = -0.05637_R64P, &
                                   av3 = -0.002913_R64P
          ! Start of executable statements
          if(ierr.LT.0) ierr = 0
          if(this%m_isbuilt .EQ. .false. .OR. &
             array3D_not_alloc(t3d)      ) then
               if(logging) then
                    call log_startup(filename,append)
                    call log_UsrMsg("logger:2264, In->mod_wiparam/compute_psii,: Unallocated allocatble array [t3d]")
                    call log_shutdown()
                else
                    call DATE_AND_TIME(date=sdate,time=stime)
                    write(stderr,*) "===========================NON-FATAL=========================="
                    write(sdterr,*) " ( mod_wiparam/compute_psii:2264, Unallocated allocatable array [t3d])"
                    write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                    write(stderr,*) "===========================NON-FATAL=========================="
              end if
              ierr = -1
             return
          end if 
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_wiparam/compute_psii: qpctimer_start failed to query performance frequency counter!"
              end if
          end if    
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
             do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE
                 do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE
                     do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE
                         do jj = j, DEFAULT_BLOCK_SIZE
                             do kk = k, DEFAULT_BLOCK_SIZE
                                 do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                            IF (USE_SOFT_PREFETCH .EQ. 1)
                                     call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                            ENDIF
                                 Tr =  (t3d(ii,kk,jj)-T0)*T0
                                 this%m_psii(ii,kk,jj) = &
                                                av0+Tr*(av1+Tr*(av2+Tr*(av3+Tr)))
                                 end do
                             end do
                         end do
                     end do
                 end do
             end do
!DIR$        ELSE
             do j = this%m_idx(17), this%m_idx(18)
                 do k = this%m_idx(15), this%m_idx(16)
                     do i = this%m_idx(13), this%m_idx(14)
!DIR$                IF (USE_SOFT_PREFETCH .EQ. 1)
                          call MM_PREFETCH(t3d(ii+2,kk,jj),1)
!DIR$                ENDIF
                           Tr =  (t3d(ii,kk,jj)-T0)*T0
                           this%m_psii(ii,kk,jj) = &
                                    av0+Tr*(av1+Tr*(av2+Tr*(av3+Tr)))   
                     end do
                 end do
             end do
!DIR$        ENDIF             
             if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_wiparam/compute_psii: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_wiparam/compute_psii: Unable to read performance counter -- fatal!!"
                end if
              end if                    
              if(dbg) then
                  print*, "Fitting of m_psii: ", this%m_psii
              end if
    end subroutine
                            
end module mod_wiparam