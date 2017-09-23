
module mod_hydro_mixratio

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_hydro_mixratio'
 !          
 !          Purpose:
 !                    Hydrometeors mixing ratio expressed in kg/kg
 !                   
 !                     
 !          History:
 !                        Date: 21-09-2017
 !                        Time: 09:23 GMT+2
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
    private ! everything is private beside variables and procedures deemed public
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT , &
                                stdout=>OUTPUT_UNIT
    use IFPORT, only : TRACEBACKQQ
    use module_class_error_check, only : array3D_not_alloc
                                         
    use module_kinds
    use mod_constants
    use module_logger
    use mod_code_timing 
    
    public :: assignment (=)
    public :: operator   (/=)
    public :: operator   (==)
    public :: operator   (>)
    public :: operator   (<)
    public :: operator   (.grad.)
    public :: operator   (.laplacian.)
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Version major
    integer(I32P), parameter, public :: MOD_HYDRO_MIXRATIO_MAJOR = 1
    
    ! Version minor
    integer(I32P), parameter, public :: MOD_HYDRO_MIXRATIO_MINOR = 0
    
    ! Version micro
    integer(I32P), parameter, public :: MOD_HYDRO_MIXRATIO_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_HYDRO_MIXRATIO_FULLVER = 1000*MOD_HYDRO_MIXRATIO_MAJOR+100*MOD_HYDRO_MIXRATIO_MINOR+ &
                                                                     10*MOD_HYDRO_MIXRATIO_MICRO
    
    ! Module creation date/time
    character(*),  parameter, public :: MOD_HYDRO_MIXRATIO_CREATE_DATE = "21-09-2017 09:30 +00200 (THR 21 SEP 2017 GMT+2)"
    
    ! Module build date/time (should be set after successfull compilation)
    character(*),  parameter, public :: MOD_HYDRO_MIXRATIO_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_HYDRO_MIXRATIO_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_HYDRO_MIXRATIO_INFO = "Hydrometeors mixing ratio representation."
    
    !============================================50
    !  Type: HydroMRatio
    !============================================50
    
    type, public :: HydroMRatio
        
         ! public by default
         ! Indices conforms to WRF implementation.
         ! Order of indices as follows:
         ! 1) Memory: ims,kms,jms,...  at m_idx(1)->m_idx(6)
         ! 2) Domain: ids,kds,jds,...  at m_idx(7)->m_idx(12)
         ! 3) Tile:   its,kts,jts,...  at m_idx(13)->m_idx(18)
         integer(I64P), dimension(A3DNidx) :: m_idx
         
         ! Hydrometeor name i.e. water vapour, ice, hail ..etc
         character(len=64) :: m_name
         
         real(R64P), allocatable, dimension(:,:,:) :: m_mratio
!DIR$    ATTRIBUTES ALIGN : 32 :: m_mratio
         
         ! Build indicator (logical)
         logical(I32P) :: m_isbuilt
         
         contains
         
        !===========================================55
        ! Constructors and Destructor procedures.
        !===========================================55
        
        procedure, pass(this), public :: default_init
        
        procedure, pass(this), public :: init
        
        procedure, pass(this), public :: copy
        
        procedure, pass(this), public :: destroy
        
        !============================================55
        ! Read/Write procedures
        !============================================55
        
        procedure, nopass, public :: read_hmratio
        
        procedure, nopass, public :: write_hmratio
         
    end type HydroMRatio
         
        !=================================================60
        !  Module operators
        !=================================================60  
    
        interface assignment  (=)
             module procedure assign_hmratio
        end interface
        
        interface operator    (/=)
             module procedure hmratio_neq_hmratio
        end interface
        
        interface operator    (==)
             module procedure hmratio_eq_hmratio
        end interface
        
        interface operator    (>)
             module procedure hmratio_gt_hmratio
        end interface
        
        interface operator    (<)
             module procedure hmratio_lt_hmratio
        end interface
        
        interface operator    (.grad.)
             module procedure hmratio_gradient
        end interface
        
        interface operator    (.laplacian.)
             module procedure hmratio_laplacian
        end interface
        
    contains
    
    !=================================================60
    !              Implementation
    !=================================================60
    
    
    !======================================================60
    ! subroutine: default_init
    ! Initialziation to default values in this case +INF.
    !   @Warning:
    !            Upon non-fatal early exit parameter
    !            'err' will be set to -1                      
    !======================================================60
    subroutine default_init(this,indices,name,logging,filename, &
                            append,dbg,err   )
          implicit none
          class(HydroMRatio),                intent(inout) :: this
          integer(I64P), dimension(A3DNidx), intent(in)    :: indices
          character(len=*),                  intent(in)    :: name
          logical(I32P),                     intent(in)    :: logging
          character(*),                      intent(in)    :: filename
          logical(I32P),                     intent(in)    :: append,dbg
          integer(I32P),                     intent(inout) :: err
          ! Locals
          character(len=40)  :: sdate,stime
          character(len=256) :: emsg
          integer(I64P)      :: j,k,i
          integer(I32P)      :: aerr
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:189, In->mod_hydro_mixratio/default_init: HydroMRatio already initiliazed!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_hydro_mixratio/default_init:189, HydroMRatio already initialized!!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          ! Begin construction
          this%m_idx = indices
          this%m_name = name
          associate(d1s=>this%m_idx(1), &
                    d1e=>this%m_idx(2), &
                    d2s=>this%m_idx(3), &
                    d2e=>this%m_idx(4), &
                    d3s=>this%m_idx(5), &
                    d3e=>this%m_idx(6) )
              allocate(this%m_mratio(d1s:d1e,d2s:d2e,d3s:d3e), &
                                     STAT=aerr,                &
                                     ERRMSG=emsg )
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:218, In->mod_hydro_mixratio/default_init: Failed to allocate: [m_mratio]")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_hydro_mixratio/default_init:218, Failed to allocate: [m_mratio])"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_hydro_mixratio/default_init:218 -> [FATAL-ERROR]: Terminating execution!!"
          end if
          ! Array initialzation
          do j = this%m_idx(17), this%m_idx(18)  ! m_jts, m_jte
              do k = this%m_idx(15), this%m_idx(16)  ! m_kts, m_kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                  
                  do i = this%m_idx(13), this%m_idx(14)   ! m_its, m_ite
                        this%m_mratio(i,k,j) = LAM_PINF
                  end do
              end do
          end do
          this%m_isbuilt = .true.
          if(dbg) then
              print*, "Hydrometeor=", this%m_name
              print*, this%m_mratio
          end if
    end subroutine
                            
    !======================================================60
    ! subroutine: init
    ! Initialziation by user(caller) passed array which
    ! usually contain computed scalar values of hydrometeor
    ! mixing ratios.
    !   @Warning:
    !            Upon non-fatal early exit parameter
    !            'err' will be set to -1                      
    !======================================================60                        
    subroutine init(this,indices,name,mratio,logging,profiling, &
                    filename,append,dbg,err,qpctimer    )
          implicit none
          class(HydroMRatio),                intent(inout) :: this
          integer(I64P), dimension(A3DNidx), intent(in)    :: indices
          character(len=*),                  intent(in)    :: name
          real(R64P), dimension(:,:,:),      intent(in)    :: mratio
!DIR$     ASSUME_ALIGNED mratio:32
          logical(I32P),                     intent(in)    :: logging,profiling
          character(len=*),                  intent(in)    :: filename
          logical(I32P),                     intent(in)    :: append,dbg
          integer(I32P),                     intent(inout) :: err
          type(QPCTimer_t),                  intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: sdate,stime
          character(len=256) :: emsg
          integer(I64P)      :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P)      :: jj,kk,ii
!DIR$     ENDIF
          integer(I32P)      :: aerr
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true. .OR. &
             array3D_not_alloc(mratio)) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:292, In->mod_hydro_mixratio/init: HydroMRatio already initialized, or invalid argument!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_hydro_mixratio/init:292, HydroMRatio already initialized!!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
             end if
             ! Begin construction
             this%m_idx = indices
             this%m_name = name
             associate(d1s=>this%m_idx(1), &
                       d1e=>this%m_idx(2), &
                       d2s=>this%m_idx(3), &
                       d2e=>this%m_idx(4), &
                       d3s=>this%m_idx(5), &
                       d3e=>this%m_idx(6) )
                 allocate(this%m_mratio(d1s:d1e,d2s:d2e,d3s:d3e), &
                           STAT=aerr,                             &
                           ERRMSG=emsg )
            end associate
            if(aerr.NE.0) then
               if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:323, In->mod_hydro_mixratio/init: Failed to allocate: [m_mratio]")
                  call log_shutdown()
               else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_hydro_mixratio/init:323, Failed to allocate: [m_mratio])"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_hydro_mixratio/default:323 -> [FATAL-ERROR]: Terminating execution!!"
            end if  
             ! Array initialization
            if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_hydro_mixratio/init: qpctimer_start failed to query performance frequency counter!"
              end if
            end if
!DIR$       IF (USE_LOOP_BLOCKING .EQ. 1)
            do j = this%m_idx(17), this%m_idx(18), DEFAULT_BLOCK_SIZE   ! m_jts, m_jte
                do k = this%m_idx(15), this%m_idx(16), DEFAULT_BLOCK_SIZE       ! m_kts, m_kte
                    do i = this%m_idx(13), this%m_idx(14), DEFAULT_BLOCK_SIZE     ! m_its, m_ite
                        do jj = j, DEFAULT_BLOCK_SIZE
                            do kk = k, DEFAULT_BLOCK_SIZE
!DIR$                           SIMD VECTORLENGTHFOR(REAL(KIND=8))                                
                                do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                           IF (USE_SOFT_PREFETCH .EQ. 1)
                                    call MM_PREFETCH(mratio(ii+2,kk,jj),1)
!DIR$                           ENDIF
                                    this%m_mratio(ii,kk,jj) = mratio(ii,kk,jj)
                                end do
                            end do
                        end do
                    end do
                end do
            end do
!DIR$       ELSE
            do j = this%m_idx(17), this%m_idx(18)
                do k = this%m_idx(15), this%m_idx(16)
!DIR$               SIMD VECTORLENGTHFOR(REAL(KIND=8))                    
                    do i = this%m_idx(13), this%m_idx(14)
!DIR$               IF (USE_SOFT_PREFETCH .EQ. 1)
                        call MM_PREFETCH(mratio(i+2,k,j),1)
!DIR$               ENDIF
                        this%m_mratio(i,k,j) = mratio(i,k,j)
                    end do
                end do
            end do
!DIR$       ENDIF
            if(profiling) then
                if(ifail.EQ.0) then
                    call qpctimer_stop(qpctimer,ifail)
                    call qpctimer_delta(qpctimer,bfail)
                    if(bfail .EQ. .false.) then
                         call qpctimer_print(qpctimer)
                    else
                         write(stderr,*) "mod_hydro_mixratio/init: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_hydro_mixratio/init: Unable to read performance counter -- fatal!!"
                end if
         end if
         this%m_isbuilt = .true.
         if(dbg) then
             print*, "Hydrometeor=", this%m_name
             print*, this%m_mratio
         end if
    end subroutine
                    
    !======================================================60 
    ! subroutine: copy 
    ! Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.                
    !======================================================60 
    subroutine copy(this,other,logging,filename,append,err)
          implicit none
          class(HydroMRatio),  intent(inout) :: this
          class(HydroMRatio),  intent(in)    :: other
          logical(I32P),       intent(in)    :: logging
          character(len=*),    intent(in)    :: filename
          logical(I32P),       intent(in)    :: append
          integer(I32P),       intent(inout) :: err
          ! Locals
          character(len=40) :: sdate,stime
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:420, In->mod_hydro_mixratio/copy: HydroMRatio already initialized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_hydro_mixratio/copy:420, HydroMRatio already initialized!!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          ! Begin construction
          this%m_idx(:) = other%m_idx(:)
          this%m_name = other%m_name
          this%m_mratio = other%m_ratio
          this%m_isbuilt = .true.
    end subroutine
    
    !======================================================60
    !  subroutine: destroy
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine destroy(this,logging,filename,append,err)
          implicit none
          class(HydroMRatio), intent(inout) :: this
          logical(I32P),      intent(in)    :: logging
          character(len=*),   intent(in)    :: filename
          logical(I32P),      intent(in)    :: append
          integer(I32P),      intent(inout) :: err
          ! Locals
          character(len=40)  :: sdate,stime
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:463, In->mod_hydro_mixratio/destroy: HydroMRatio already destroy!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_hydro_mixratio/copy:463, HydroMRatio already destroy!!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          this%m_idx(:) = 0
          this%m_name = " "
          if(ALLOCATED(this%m_mratio)) then
              deallocate(this%m_mratio, &
                         STAT=derr,     &
                         ERRMSG=emsg )
                 if(derr.NE.0) then
                    if(logging) then
                        call log_startup(filename,append)
                        call log_UsrMsg("logger:486, In->mod_hydro_mixratio/destroy: Failed to deallocate: [m_mratio]")
                        call log_shutdown()
                    else
                        call DATE_AND_TIME(date=sdate,time=stime)
                        write(ERROR_UNIT,*) "===========================FATAL========================="
                        write(ERROR_UNIT,*) "   (mod_hydro_mixratio/destroy:486, Failed deallocate: [m_mratio])"
                        write(ERROR_UNIT,*) "   (System message:)", emsg
                        write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                        write(ERROR_UNIT,*) "===========================FATAL=========================="
                   end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_hydro_mratio/destroy:486 -> [FATAL-ERROR]: Terminating execution!!"
              end if 
          end if
          this%m_isbuilt = .false.
    end subroutine
    
    !======================================================60
    !  subroutine: read_hmratio
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine read_hmratio(this,unit,ioerr,err)
          implicit none
          class(HydroMRatio), intent(in)    :: this
          integer,            intent(in)    :: unit
          integer(I32P),      intent(inout) :: ioerr,err
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .false.) then
              err = -1
              return
          end if
          READ(unit,iostat=ioerr) this
    end subroutine
    
    !======================================================60
    !  subroutine: write_hmratio
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine write_hmratio(this,unit,ioerr,err)
          implicit none
          class(HydroMRatio), intent(in)    :: this
          integer,            intent(in)    :: unit
          integer(I32P),      intent(inout) :: ioerr,err
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .false.) then
              err = -1
              return
          end if
          WRITE(unit,iostat=ioerr) this
    end subroutine
    
    !======================================================60
    !                   Module operators
    !======================================================60
    
    !=======================================================60
    !  subroutine: overloaded assignment (=)
    !=======================================================60
    subroutine assign_hmratio(this,other)
          implicit none
          type(HydroMRatio), intent(inout) :: this
          type(HydroMRatio), intent(in)    :: other
          ! Locals
          character(len=40) :: sdate,stime
          ! Start of executable statements
          if(LOC(this).EQ.LOC(other)) then
               write(stderr,*) "===========================NON-FATAL=========================="
               write(sdterr,*) " ( mod_hydro_mixratio/assignment(=):564, Attempted self_assignment!)"
               write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
               write(stderr,*) "===========================NON-FATAL=========================="
               return
          end if
          this%m_idx(:)  = other%m_idx(:)
          this%m_name    = other%m_name
          this%m_mratio  = other%m_mratio
          this%m_isbuilt = other%m_isbuilt
    end subroutine
    
    !======================================================60
    ! function: hmratio_neq_hmratio i.e. overloaded
    !           operator (/=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function hmratio_neq_hmratio(h1,h2) result(neq)
          implicit none
          type(HydroMRatio), intent(in) :: h1,h2
          ! Locals
          logical(I64P), allocatable, dimension(:,:,:) :: neq
!DIR$     ATTRIBUTES ALIGN : 32 :: neq
          ! Start of executable statements
          neq = h1%m_mratio /= h2%m_mratio
    end function
    
    !======================================================60
    ! function: hmratio_eq_hmratio i.e. overloaded
    !           operator (==)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function hmratio_eq_hmratio(h1,h2) result(eq)
          implicit none
          type(HydroMRatio), intent(in) :: h1,h2
          ! Locals
          logical(I64P), allocatable, dimension(:,:,:) :: eq
!DIR$     ATTRIBUTES ALIGN : 32 :: eq
          ! Start of executable statements
          eq = h1%m_mratio == h2%m_mratio
    end function
    
    !======================================================60
    ! function: hmratio_gt_hmratio i.e. overloaded
    !           operator (>)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function hmratio_gt_hmratio(h1,h2) result(gt)
          implicit none
          type(HydroMRatio), intent(in) :: h1,h2
          ! Locals
          logical(I64P), allocatable, dimension(:,:,:) :: gt
!DIR$     ATTRIBUTES ALIGN : 32 :: gt
          ! Start of executable statements
          gt = h1%m_mratio > h2%m_mratio
    end function
    
    !======================================================60
    ! function: hmratio_lt_hmratio i.e. overloaded
    !           operator (<)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function hmratio_lt_hmratio(h1,h2) result(lt)
          implicit none
          type(HydroMRatio), intent(in) :: h1,h2
          ! Locals
          logical(I64P), allocatable, dimension(:,:,:) :: lt
!DIR$     ATTRIBUTES ALIGN : 32 :: lt
          ! Start of executable statements
          lt = h1%m_mratio < h2%m_mratio
    end function
    
    !======================================================60
    ! function: hmratio_gradient i.e. 
    !           operator (.grad.)
    ! Warning:  No error checking is made!!
    ! TODO:
    !       CADNA for cancellation errors?
    !======================================================60 
    function hmratio_gradient(this) result(hgrad)
          implicit none
          type(HydroMRatio), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: hgrad
!DIR$     ATTRIBUTES ALIGN : 32 :: hgrad
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(I64P), parameter :: sf =  1000000000000000_I64P
          real(R64P)    :: eps,tmp,isf
          ! Start of executable statements
          associate(d1s=>this%m_ims, &
                    d1e=>this%m_ime, &
                    d2s=>this%m_kms, &
                    d2e=>this%m_kme, &
                    d3s=>this%m_jms, &
                    d3e=>this%m_jme )
              allocate(hgrad(d1s:d1e,d2s:d2e,d3s:d3e))
          end associate
          hgrad = LAM_PINF
          isf = 1._R64P/real(sf,R64P)
          eps = MACHEPSF64*0.333333333333333333333333_R64P
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_jts+1, this%m_jte-1, DEFAULT_BLOCK_SIZE
              do k = this%m_kts+1, this%m_kte-1, DEFAULT_BLOCK_SIZE
                  do i = this%m_its+1, this%m_ite-1, DEFAULT_BLOCK_SIZE
                      do jj = j, DEFAULT_BLOCK_SIZE
                          do kk = k, DEFAULT_BLOCK_SIZE
                           
                              do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                         IF (USE_SOFT_PREFETCH .EQ. 1)
                                  call MM_PREFETCH(this%m_mratio(ii+2,kk,jj),1)
!DIR$                         ENDIF
                                  tmp = this%m_mratio(ii+1,kk+1,jj+1)- &
                                        this%m_mratio(ii-1,kk-1,jj-1)
                                  hgrad(ii,kk,jj) = tmp/ &
                                      (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
                              end do
                          end do
                      end do
                  end do
              end do
          end do
!DIR$     ELSE
          do j = this%m_jts+1, this%m_jte-1
              do k = this%m_kts+1, this%m_kte-1
                  do i = this%m_its+1, this%m_ite-1
!DIR$             IF (USE_SOFT_PREFETCH .EQ. 1)
                      call MM_PREFETCH(this%m_mratio(i+2,k,j),1)
!DIR$             ENDIF
                      tmp = this%m_mratio(i+1,k+1,j+1)- &
                            this%m_mratio(i-1,k-1,j-1)
                      hgrad(i,k,j) = tmp/ &
                          (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
                  end do
              end do
          end do
!DIR$     ENDIF          
    end function   
    
    !======================================================60
    ! function: hmratio_laplacian i.e. 
    !           operator (.laplacian.)
    ! Warning:  No error checking is made!!
    ! TODO:
    !       CADNA for cancellation errors?
    !======================================================60 
    function hmratio_laplacian(this) result(hlap)
          implicit none
          type(HydroMRatio), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:,:) :: hlap
!DIR$     ATTRIBUTES ALIGN : 32 :: hlap
        
          integer(I64P) :: j,k,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,kk,ii
!DIR$     ENDIF
          integer(I64P), parameter :: sf =  1000000000000000_I64P
          real(R64P)    :: eps,tmp,isf
          ! Start of executable statements
          hlap = .grad.this
          isf = 1._R64P/real(sf,R64P)        
          eps = MACHEPSF64*0.333333333333333333333333_R64P 
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_jts+1, this%m_jte-1, DEFAULT_BLOCK_SIZE
              do k = this%m_kts+1, this%m_kte-1, DEFAULT_BLOCK_SIZE
                  do i = this%m_its+1, this%m_ite-1, DEFAULT_BLOCK_SIZE
                      do jj = j, DEFAULT_BLOCK_SIZE
                          do kk = k, DEFAULT_BLOCK_SIZE
                              do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                         IF (USE_SOFT_PREFETCH .EQ. 1)
                                  call MM_PREFETCH(hlap(ii+2,kk,jj),1)
!DIR$                         ENDIF
                                  tmp = hlap(ii+1,kk+1,jj+1)- &
                                        hlap(ii-1,kk-1,jj-1)
                                  hlap(ii,kk,jj) = tmp/ &
                                                 (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
                              end do
                          end do
                      end do
                  end do
              end do
          end do
!DIR$     ELSE
          do j = this%m_jts+1, this%m_jte-1
              do k = this%m_kts+1, this%m_kte-1
                  do i = this%m_its+1, this%m_ite-1
!DIR$             IF (USE_SOFT_PREFETCH .EQ. 1)
                      call MM_PREFETCH(hlap(i+2,k,j),1)
!DIR$             ENDIF
                      tmp = hlap(i+1,k+1,j+1)- &
                            hlap(i-1,k-1,j-1)
                      hlap(i,k,j) = tmp/ &
                          (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
                  end do
              end do
          end do
!DIR$     ENDIF
    end function
                    
end module mod_hydro_mixratio