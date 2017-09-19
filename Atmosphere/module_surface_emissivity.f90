
module mod_emissivity

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_emissivity'
 !          
 !          Purpose:
 !                    Surface emissivity measured in scalar values between 0.0 -> 1.0
 !                   
 !                     
 !          History:
 !                        Date: 19-09-2017
 !                        Time: 10:36 GMT+2
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
    use module_class_error_check, only : array2D_not_alloc
                                         
    use module_kinds
    use mod_constants
    use module_logger
    use mod_code_timing  
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Version major
    integer(I32P), parameter, public :: MOD_EMISSIVITY_MAJOR = 1
    
    ! Version minor
    integer(I32P), parameter, public :: MOD_EMISSIVITY_MINOR = 0
    
    ! Version micro
    integer(I32P), parameter, public :: MOD_EMISSIVITY_MICRO = 0
    
    ! Module/file full version
    integer(I32P), parameter, public :: MOD_EMISSIVITY_FULLVER = 1000*MOD_EMISSIVITY_MAJOR+100*MOD_EMISSIVITY_MINOR+ &
                                                                 10*MOD_EMISSIVITY_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_EMISSIVITY_CREATE_DATE = "19-09-2017 10:42 +00200 (TUE 19 SEP 2017 GMT+2)"
    
    ! Module build date  (should be set after successful build date/time)
    character(*),  parameter, public :: MOD_EMISSIVITY_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_EMISSIVITY_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com "
    
    ! Module short description
    character(*),  parameter, public :: MOD_EMISSIVITY_DESCRIPT = "Surface broadband emissivity representation."
    
    !======================================================60
    !   Type: Emissivity
    !======================================================60 
    
    type, public :: Emissivity
        
         ! public by default
         ! Indices
         integer(I64P) :: m_ims,m_ime,m_jms,m_jme, &
                          m_ids,m_ide,m_jds,m_jde, &
                          m_its,m_ite,m_jts,m_jte
         
         real(R64P), allocatable, dimension(:,:) :: m_emiss
!DIR$    ATTRIBUTES ALIGN : 32 :: m_emiss
         
         ! Built indicator
         logical(I32P) :: m_isbuilt
         
         contains
         
        !===========================================55
        ! Constructors and Destructor procedures.
        !===========================================55
        
        procedure, pass(this), public :: init
        
        procedure, pass(this), public :: copy
        
        procedure, pass(this), public :: destroy
        
        !============================================55
        ! Read/Write procedures
        !============================================55
        
        procedure, nopass, public :: read_emissivity
        
        procedure, nopass, public :: write_emissivity
        
    end type Emissivity
         
        !=================================================60
        !  Module operators
        !=================================================60 
         
        interface assignment (=)
              module procedure assign_emissivity
        end interface
        
        interface operator   (/=)
              module procedure emissivity_neq_emissivity
        end interface
        
        interface operator   (==)
              module procedure emissivity_eq_emissivity
        end interface
        
        interface operator   (>)
              module procedure emissivity_gt_emissivity
        end interface
        
        interface operator   (<)
              module procedure emissivite_lt_emissivity
        end interface
        
        interface operator   (.grad.)
              module procedure emissivity_gradient
        end interface
        
    contains
    
    !=================================================60
    !              Implementation
    !=================================================60
    !======================================================60
    ! subroutine: init
    ! Initialziation by user(caller) passed array which
    ! usually contain specific random scalar distribution.
    !   @Warning:
    !            Upon non-fatal early exit parameter
    !            'err' will be set to -1                      
    !======================================================60
    subroutine init(this,indices,emiss,logging,profiling, &
                    filename,append,dbg,err,qpctimer  )
          implicit none
          class(Emissivity),            intent(inout) :: this
          integer(I64P), dimension(12), intent(in)    :: indices
          real(R64P), dimension(:,:),   intent(in)    :: emiss
!DIR$     ASSUME_ALIGNED : 32 :: emiss
          logical(I32P),                intent(in)    :: logging,profiling
          character(len=*),             intent(in)    :: filename
          logical(I32P),                intent(in)    :: append,dbg
          integer(I32P),                intent(inout) :: err
          type(QPCTimer_t),             intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: sdate,stime
          character(len=256) :: emsg
          integer(I64P)      :: j,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P)      :: jj,ii
!DIR$     ENDIF
          integer(I32P)      :: aerr
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true. .OR.  &
             array2D_not_alloc(emiss)) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:186, In->mod_emissivity/init: Emissivity already initialized, or invalid argument!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_emissivity/init:186, Emissivity already initialized, or invalid argument!!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
           end if
          ! Begin construction
          this%m_ims=indices(1)
          this%m_ime=indices(2)
          this%m_jms=indices(3)
          this%m_jme=indices(4)
          this%m_ids=indices(5)
          this%m_ide=indices(6)
          this%m_jds=indices(7)
          this%m_jde=indices(8)
          this%m_its=indices(9)
          this%m_ite=indices(10)
          this%m_jts=indices(11)
          this%m_jte=indices(12)
          ! Allocate array
          associate(d1s=>this%m_ims, &
                    d1e=>this%m_ime, &
                    d2s=>this%m_jms, &
                    d2e=>this%m_jme )
              allocate(this%m_emiss(d1s:d1e,d2s:d2e), &
                       STAT=aerr,                     &
                       ERRMSG=emsg )
         end associate
         if(aerr.NE.0) then
             if(logging) then
                 call log_startup(filename,append)
                 call log_UsrMsg("logger:226, In->mod_emissivity/init: Failed to allocate: [m_emiss]")
                 call log_shutdown()
             else
                 call DATE_AND_TIME(date=sdate,time=stime)
                 write(ERROR_UNIT,*) "===========================FATAL========================="
                 write(ERROR_UNIT,*) "   (mod_emissivity/init:226, Failed to allocate: [m_emiss])"
                 write(ERROR_UNIT,*) "   (System message:)", emsg
                 write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                 write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_emissivity/init:226 -> [FATAL-ERROR]: Terminating execution!!"
         end if
          ! Array initialization
         if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_emissivity/init: qpctimer_start failed to query performance frequency counter!"
              end if
         end if
!DIR$    IF (USE_LOOP_BLOCKING .EQ. 1)
         do j = this%m_jts+1, this%m_jte-1, DEFAULT_BLOCK_SIZE
             do i = this%m_its+1, this%m_ite-1, DEFAULT_BLOCK_SIZE
                 do jj = j, DEFAULT_BLOCK_SIZE
!DIR$                SIMD VECTORLENGTHFOR(REAL(KIND=8))                     
                     do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                IF (USE_SOFT_PREFETCH .EQ. 1)
                         call MM_PREFETCH(emiss(ii+2,jj),1)
!DIR$                ENDIF
                            this%m_emiss(ii,jj) = emiss(ii,jj)
                     end do
                 end do
             end do
         end do
!DIR$    ELSE
         do j = this%m_jts, this%m_jte
!DIR$        SIMD VECTORLENGTHFOR(REAL(KIND=8))             
             do i = this%m_its, this%m_ite
!DIR$        IF (USE_SOFT_PREFETCH .EQ. 1)
                 call MM_PREFETCH(emiss(i+2,j),1)
!DIR$        ENDIF
                 this%m_emiss(i,j) = emiss(i,j)
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
                         write(stderr,*) "mod_emissivity/init: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_emissivity/init: Unable to read performance counter -- fatal!!"
                end if
         end if
         this%m_isbuilt = .true.
         if(dbg) then
             print*, " Surface emissivity: ", this%m_emiss
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
          class(Emissivity), intent(inout) :: this
          class(Emissivity), intent(in)    :: other
          logical(I32P),     intent(in)    :: logging
          character(len=*),  intent(in)    :: filename
          logical(I32P),     intent(in)    :: append
          integer(I32P),     intent(inout) :: err
          ! Locals
          character(len=40) :: sdate,stime
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:315, In->mod_emissivity/copy: Emissivity already initialized")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_emissivity/copy:315, Emissivity already initialized, or invalid argument!!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          ! Begin construction
          this%m_ims=other%m_ims
          this%m_ime=other%m_ime
          this%m_jms=other%m_jms
          this%m_jme=other%m_jme
          this%m_ids=other%m_ids
          this%m_ide=other%m_ide
          this%m_jds=other%m_jds
          this%m_jde=other%m_jde
          this%m_its=other%m_its
          this%m_ite=other%m_ite
          this%m_jts=other%m_jts
          this%m_jte=other%m_jte
          this%m_emiss=other%m_emiss
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
          class(Emissivity), intent(inout) :: this
          logical(I32P),     intent(in)    :: logging
          character(len=*),  intent(in)    :: filename
          logical(I32P),     intent(in)    :: append
          integer(I32P),     intent(inout) :: err
          ! Locals
          character(len=40)  :: sdate,stime
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:368, In->mod_emissivity/destroy: Emissivity already destroyed!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_emissivity/destroy:368, Emissivity already destroyed!!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          this%m_ims=0
          this%m_ime=0
          this%m_jms=0
          this%m_jme=0
          this%m_ids=0
          this%m_ide=0
          this%m_jds=0
          this%m_jde=0
          this%m_its=0
          this%m_ite=0
          this%m_jts=0
          this%m_jte=0
          if(ALLOCATED(this%m_emiss)) then
              deallocate(this%m_emiss, &
                         STAT=derr,    &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:400, In->mod_emissivity/destroy: Failed to deallocate: [m_emiss]")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=sdate,time=stime)
                      write(ERROR_UNIT,*) "===========================FATAL========================="
                      write(ERROR_UNIT,*) "   (mod_emissivity/destroy:40, Failed deallocate: [m_emiss])"
                      write(ERROR_UNIT,*) "   (System message:)", emsg
                      write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                      write(ERROR_UNIT,*) "===========================FATAL=========================="
                end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_emissivity/destroy:400 -> [FATAL-ERROR]: Terminating execution!!"
              end if 
          end if
          this%m_isbuilt = .false.
    end subroutine
    
    !======================================================60
    !  subroutine: read_emissivity
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine read_emissivity(this,unit,ioerr,err)
          implicit none
          class(Emissivity), intent(in)    :: this
          integer,           intent(in)    :: unit
          integer(I32P),     intent(inout) :: ioerr,err
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .false.) then
              err = -1
              return
          end if
          READ(unit,iostat=ioerr) this
    end subroutine
    
    !======================================================60
    !  subroutine: write_emissivity
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine write_emissivity(this,unit,ioerr,err)
          implicit none
          class(Emissivity), intent(in)    :: this
          integer,           intent(in)    :: unit
          integer(I32P),     intent(inout) :: ioerr,err
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
    subroutine assign_emissivity(this,other)
          implicit none
          type(Emissivity), intent(inout) :: this
          type(Emissivity), intent(in)    :: other
          ! Locals
          character(len=40)  :: sdate,stime
          ! Strat of executable statements
          if(LOC(this).EQ.LOC(other)) then
               write(stderr,*) "===========================NON-FATAL=========================="
               write(sdterr,*) " ( mod_emissivity/assignment(=):479, Attempted self_assignment!)"
               write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
               write(stderr,*) "===========================NON-FATAL=========================="
               return
          end if
          this%m_ims=other%m_ims
          this%m_ime=other%m_ime
          this%m_jms=other%m_jms
          this%m_jme=other%m_jme
          this%m_ids=other%m_ids
          this%m_ide=other%m_ide
          this%m_jds=other%m_jds
          this%m_jde=other%m_jde
          this%m_its=other%m_its
          this%m_ite=other%m_ite
          this%m_jts=other%m_jts
          this%m_jte=other%m_jte
          this%m_emiss=other%m_emiss
          this%m_isbuilt = .true.
    end subroutine
    
    !======================================================60
    ! function: emissivity_neq_emissivity i.e. overloaded
    !           operator (/=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function emissivity_neq_emissivity(e1,e2) result(neq)
          implicit none
          type(Emissivity), intent(in) :: e1,e2
          ! Locals
          logical(I64P), allocatable, dimension(:,:) :: neq
!DIR$     ATTRIBUTES ALIGN : 32 :: neq
          ! Start of executable statemetns
          neq = e1%m_emiss /= e2%m_emiss
    end function
    
    !======================================================60
    ! function: emissivity_eq_emissivity i.e. overloaded
    !           operator (==)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function emissivity_eq_emissivity(e1,e2) result(eq)
          implicit none
          type(Emissivity), intent(in) :: e1,e2
          ! Locals
          logical(I64P), allocatable, dimension(:,:) :: eq
!DIR$     ATTRIBUTES ALIGN : 32 :: neq
          ! Start of executable statemetns
          eq = e1%m_emiss == e2%m_emiss
    end function
    
    !======================================================60
    ! function: emissivity_gt_emissivity i.e. overloaded
    !           operator (>)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function emissivity_gt_emissivity(e1,e2) result(gt)
          implicit none
          type(Emissivity), intent(in) :: e1,e2
          ! Locals
          logical(I64P), allocatable, dimension(:,:) :: gt
!DIR$     ATTRIBUTES ALIGN : 32 :: gt
          ! Start of executable statements
          gt = e1%m_emiss > e2%m_emiss
    end function
    
    !======================================================60
    ! function: emissivity_lt_emissivity i.e. overloaded
    !           operator (<)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function emissivity_lt_emissivity(e1,e2) result(lt)
          implicit none
          type(Emissivity), intent(in) :: e1,e2
          ! Locals
          logical(I64P), allocatable, dimension(:,:) :: lt
!DIR$     ATTRIBUTES ALIGN : 32 :: lt
          ! Start of executable statements
          lt = e1%m_emiss < e2%m_emiss
    end function
    
    !======================================================60
    ! function: emissivity_gradient i.e. 
    !           operator (.grad.)
    ! Warning:  No error checking is made!!
    ! TODO:
    !       CADNA for cancellation errors?
    !======================================================60 
    function emissivity_gradient(this) result(egrad)
          implicit none
          type(Emissivity), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: egrad
!DIR$     ATTRIBUTES ALIGN : 32 :: egrad
          integer(I64P) :: j,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P) :: jj,ii
!DIR$     ENDIF
          integer(I64P), parameter :: sf =  1000000000000000_I64P
          real(R64P)    :: eps,tmp,isf
          ! Start of executable statements
          associate(d1s=>this%m_ims, &
                    d1e=>this%m_ime, &
                    d2s=>this%m_jms, &
                    d2e=>this%m_jme )
              allocate(egrad(d1s:d1e,d2s:d2e))
          end associate
          egrad = LAM_PINF
          isf = 1._R64P/real(sf,R64P)
          eps = MACHEPSF64*0.333333333333333333333333_R64P
!DIR$    IF (USE_LOOP_BLOCKING .EQ. 1)
         do j = this%m_jts+1, this%m_jte-1, DEFAULT_BLOCK_SIZE
             do i = this%m_its+1, this%m_its-1, DEFAULT_BLOCK_SIZE
                 do jj = j, DEFAULT_BLOCK_SIZE
                     do ii = i, DEFAULT_BLOCK_SIZE
!DIR$    IF (USE_SOFT_PREFETCH .EQ. 1)
                         call MM_PREFETCH(this%m_emiss(ii+2,jj),1)
!DIR$    ENDIF
                         tmp = this%m_alb(ii+1,jj+1)- &
                               this%m_alb(ii-1,jj-1)
                         egrad(ii,jj) = this%m_emiss(ii+1,jj+1)- &
                                        this%m_emiss(ii-1,jj-1)/ &
                             (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
                     end do
                 end do
             end do
         end do
!DIR$    ELSE
        do j = this%m_jts+1, this%m_jte-1
            do i = this%m_its+1, this%m_ite-1
!DIR$    IF (USE_SOFT_PREFETCH .EQ. 1)
                call MM_PREFETCH(this%m_emiss(i+2,j),1)
!DIR$    ENDIF 
                 tmp = this%m_emiss(i+1,j+1)- &
                       this%m_emiss(i-1,j-1)
                 egrad(i,j) = this%m_emiss(i+1,j+1)- &
                                this%m_emiss(i-1,j-1)/ &
                     (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
            end do
        end do
!DIR$  ENDIF          
    end function
    
end module mod_emissivity