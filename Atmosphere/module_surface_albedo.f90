
module mod_albedo

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_albedo'
 !          
 !          Purpose:
 !                    Surface albedo measured in scalar values between 0.0 -> 1.0
 !                   
 !                     
 !          History:
 !                        Date: 17-09-2017
 !                        Time: 10:49 GMT+2
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
    
    public :: assignment (=)
    public :: operator   (/=)
    public :: operator   (==)
    public :: operator   (>)
    public :: operator   (<)
    public :: operator   (.grad.)
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Version major
    integer(I32P), parameter, public :: MOD_ALBEDO_MAJOR = 1
    
    ! Version minor
    integer(I32P), parameter, public :: MOD_ALBEDO_MINOR = 0
    
    ! Version micro
    integer(I32P), parameter, public :: MOD_ALBEDO_MICRO = 0
    
    ! Module/file full version
    integer(I32P), parameter, public :: MOD_ALBEDO_FULLVER = 1000*MOD_ALBEDO_MAJOR+100*MOD_ALBEDO_MINOR+ &
                                                             10*MOD_ALBEDO_MICRO
    
    ! Module file crreation date/time
    character(*),  parameter, public :: MOD_ALBEDO_CREATE_DATE =  "17-09-2017 10:49 +00200 (SUN 17 SEP 2017 GMT+2)"
    
    ! Module build date (should set after successful build date/time)
    character(*),  parameter, public :: MOD_ALBEDO_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_ALBEDO_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_ALBEDO_DESCRIPT = "Surface albedo representation."
    
    !======================================================60
    !   Type: Albedo
    !======================================================60
    
    type, public :: Albedo
        
        ! public by default
        ! indices
        integer(I64P) :: m_ims,m_ime,m_jms,m_jme, &
                         m_ids,m_ide,m_jds,m_jde, &
                         m_its,m_ite,m_jts,m_jte
        
        real(R64P), allocatable, dimension(:,:) :: m_alb
!DIR$   ATTRIBUTES ALIGN : 32 :: m_alb
        
        ! Build indicator (logical)
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
        
        procedure, nopass, public :: read_albedo
        
        procedure, nopass, public :: write_albedo
        
    end type Albedo
        
        !=================================================60
        !  Module operators
        !=================================================60
        
        interface assignment (=)
              module procedure assign_albedo
        end interface
        
        interface operator   (/=)
              module procedure albedo_neq_albedo
        end interface
        
        interface operator   (==)
              module procedure albedo_eq_albedo
        end interface
        
        interface operator   (>)
              module procedure albedo_gt_albedo
        end interface
        
        interface operator   (<)
              module procedure albedo_lt_albedo
        end interface
        
        interface operator (.grad.)
              module procedure albedo_gradient
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
    subroutine init(this,indices,alb,logging,profiling, &
                    filename,append,dbg,err,qpctimer  )
          implicit none
          class(Albedo),                intent(inout) :: this
          integer(I64P), dimension(12), intent(in)    :: indices
          real(R64P), dimension(:,:),   intent(in)    :: alb
!DIR$     ASSUME_ALIGNED alb:32
          logical(I32P),                intent(in)    :: logging,profiling
          character(len=*),             intent(in)    :: filename
          logical(I32P),                intent(in)    :: append,dbg
          integer(I32P),                intent(inout) :: err
          type(QPCTimer_t),             intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I64P)      :: j,i
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P)      :: jj,ii
!DIR$     ENDIF
          integer(I32P)      :: aerr
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail
          ! Start of executable statemetns
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:187, In->mod_albedo/init: Albedo already initialized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_albedo/init:187, Albedo already initialized!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
              err = -1
              return
          end if
          if(array2D_not_alloc(alb)) then
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
              allocate(this%m_alb(d1s:d1e,d2s:d2e), &
                       STAT=aerr,                   &
                       ERRMSG=emsg)
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:230, In->mod_albedo/init: Failed to allocate: [m_alb]")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_albedo/init:230, Failed to allocate: [m_alb]!!)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_albedo/init:230 -> [FATAL-ERROR]: Terminating execution!!"
          end if
          ! Array initialization
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_albedo/init: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
!DIR$     IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_jts, this%m_jte, DEFAULT_BLOCK_SIZE
              do i = this%m_its, this%m_ite, DEFAULT_BLOCK_SIZE
                  do jj = j, DEFAULT_BLOCK_SIZE
!DIR$                 SIMD VECTORLENGTHFOR(REAL(KIND=8))                      
                      do ii = i, DEFAULT_BLOCK_SIZE
!DIR$                 IF (USE_SOFT_PREFETCH .EQ. 1)
                          call MM_PREFETCH(alb(ii+2,jj),1)
!DIR$                 ENDIF
                          this%m_alb(ii,jj) = alb(ii,jj)
                      end do
                  end do
              end do
          end do
!DIR$     ELSE
          do j = this%m_jts, this%m_jte
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))              
              do i = this%m_its, this%m_ite
!DIR$         IF (USE_SOFT_PREFETCH .EQ. 1)
                   call MM_PREFETCH(alb(i+2,j),1)
!DIR$         ENDIF
                   this%m_alb(i,j) = alb(i,j)
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
                         write(stderr,*) "mod_albedo/init: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_albedo/init: Unable to read performance counter -- fatal!!"
                end if
          end if
          this%m_isbuilt = .true.
          if(dbg) then
              print*, " Surface albedo: ", this%m_alb
          endif
    end subroutine
    
    !======================================================60 
    ! subroutine: copy 
    ! Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.                
    !======================================================60                 
    subroutine copy(this,other,logging,filename,append,err)
          implicit none
          class(Albedo),    intent(inout) :: this
          class(Albedo),    intent(in)    :: other
          logical(I32P),    intent(in)    :: logging
          character(len=*), intent(in)    :: filename
          logical(I32P),    intent(in)    :: append
          integer(I32P),    intent(inout) :: err
          ! Locals
          character(len=40) :: dstr,tstr
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:322, In->mod_albedo/copy: Albedo already initialized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_albedo/copy:322, Albedo already initialized!)"
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
          this%m_alb=other%m_alb
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
          class(Albedo),    intent(inout) :: this
          logical(I32P),    intent(in)    :: logging
          character(len=*), intent(in)    :: filename
          logical(I32P),    intent(in)    :: append
          integer(I32P),    intent(inout) :: err
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: derr
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:374, In->mod_albedo/destroy: Albedo is already destroyed!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_albedo/destroy:374, Albedo already initialized!)"
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
          if(ALLOCATED(this%m_alb)) then
              deallocate(this%m_alb, &
                         STAT=derr,  &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:407, In->mod_albedo/destroy: Failed to deallocate [m_alb]")
                      call log_shutdown()
                  else
                      call DATE_AND_TIME(date=dstr,time=tstr)
                      write(ERROR_UNIT,*) "===========================FATAL========================="
                      write(ERROR_UNIT,*) "   (mod_albedo/destroy:407, Failed deallocate: [m_alb]!!)"
                      write(ERROR_UNIT,*) "   (System message:)", emsg
                      write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                      write(ERROR_UNIT,*) "===========================FATAL=========================="
                end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_alb/destroy:407 -> [FATAL-ERROR]: Terminating execution!!"
              end if 
          end if
          this%m_isbuilt = .false.
    end subroutine
    
    !======================================================60
    !  subroutine: read_albedo
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine read_albedo(this,unit,ioerr,err)
          implicit none
          class(Albedo), intent(in)    :: this
          integer,       intent(in)    :: unit
          integer(I32P), intent(inout) :: ioerr,err
          ! Start of executable statemeemts
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .false.) then
              err = -1
              return
          end if
          READ(unit,iostat=ioerr) this
    end subroutine
    
    !======================================================60
    !  subroutine: write_albedo
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine write_albedo(this,unit,ioerr,err)
          implicit none
          class(Albedo), intent(in)    :: this
          integer,       intent(in)    :: unit
          integer(I32P), intent(inout) :: ioerr,err
          ! Start of executable statemeemts
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
    subroutine assign_albedo(this,other)
          implicit none
          type(Albedo), intent(inout) :: this
          type(Albedo), intent(in)    :: other
          ! Start of executable statemetns
          if(LOC(this).EQ.LOC(other)) then
               call DATE_AND_TIME(date=dstr,time=tstr)
               write(stderr,*) "===========================NON-FATAL=========================="
               write(sdterr,*) " ( mod_albedo/assignment(=):483, Attempted self_assignment!)"
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
          this%m_alb=other%m_alb
          this%m_isbuilt = .true.
    end subroutine
    
    !======================================================60
    ! function: albedo_neq_albedo i.e. overloaded
    !           operator (/=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function albedo_neq_albedo(a1,a2) result(neq)
          implicit none
          type(Albedo), intent(in) :: a1,a2
          ! Locals
          logical(I64P), allocatable, dimension(:,:) :: neq
!DIR$     ATTRIBUTES ALIGN : 32 :: neq          
          ! Start of executable statemetns
          neq = a1%m_alb /= a2%m_alb
    end function
    
    !======================================================60
    ! function: albedo_eq_albedo i.e. overloaded
    !           operator (==)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function albedo_eq_albedo(a1,a2) result(eq)
          implicit none
          type(Albedo), intent(in) :: a1,a2
          ! Locals
          logical(I64P), allocatable, dimension(:,:) :: eq
!DIR$     ATTRIBUTES ALIGN : 32 :: eq
          ! Start of executable statements
          eq = a1%m_alb == a2%m_alb
    end function
    
    !======================================================60
    ! function: albedo_gt_albedo i.e. overloaded
    !           operator (>)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function albedo_gt_albedo(a1,a2) result(gt)
          implicit none
          type(Albedo), intent(in) :: a1,a2
          ! Locals
          logical(I64P), allocatable, dimension(:,:) :: gt
!DIR$     ATTRIBUTES ALIGN : 32 :: gt
          ! Start of executable statemetns
          gt = a1%m_alb > a2%m_alb
    end function
    
    !======================================================60
    ! function: albedo_lt_albedo i.e. overloaded
    !           operator (<)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function albedo_lt_albedo(a1,a2) result(lt)
          implicit none
          type(Albedo), intent(in) :: a1,a2
          ! Locals
          logical(I64P), allocatable, dimension(:,:) :: lt
!DIR$     ATTRIBUTES ALIGN : 32 :: lt
          ! Start of exeecutable statemetns
          lt = a1%m_alb < a2%m_alb
    end function
    
    !======================================================60
    ! function: albedo_gradient i.e. 
    !           operator (.grad.)
    ! Warning:  No error checking is made!!
    ! TODO:
    !       CADNA for cancellation errors?
    !======================================================60 
    function albedo_gradient(this) result(agrad)
          implicit none
          type(Albedo), intent(in) :: this
          ! Locals
          real(R64P), allocatable, dimension(:,:) :: agrad
!DIR$     ATTRIBUTES ALIGN : 32 :: agrad
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
              allocate(agrad(d1s:d1e,d2s:d2e))
         end associate
         agrad = LAM_PINF
         isf = 1._R64P/real(sf,R64P)
         eps = MACHEPSF64*0.333333333333333333333333_R64P
!DIR$    IF (USE_LOOP_BLOCKING .EQ. 1)
         do j = this%m_jts+1, this%m_jte-1, DEFAULT_BLOCK_SIZE
             do i = this%m_its+1, this%m_its-1, DEFAULT_BLOCK_SIZE
                 do jj = j, DEFAULT_BLOCK_SIZE
                     do ii = i, DEFAULT_BLOCK_SIZE
!DIR$    IF (USE_SOFT_PREFETCH .EQ. 1)
                         call MM_PREFETCH(this%m_alb(ii+2,jj),1)
!DIR$    ENDIF
                         tmp = this%m_alb(ii+1,jj+1)- &
                               this%m_alb(ii-1,jj-1)
                         agrad(ii,jj) = this%m_alb(ii+1,jj+1)- &
                                        this%m_alb(ii-1,jj-1)/ &
                             (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
                     end do
                 end do
             end do
         end do
!DIR$    ELSE
        do j = this%m_jts+1, this%m_jte-1
            do i = this%m_its+1, this%m_ite-1
!DIR$    IF (USE_SOFT_PREFETCH .EQ. 1)
                call MM_PREFETCH(this%m_alb(i+2,j),1)
!DIR$    ENDIF 
                 tmp = this%m_alb(i+1,j+1)- &
                       this%m_alb(i-1,j-1)
                 agrad(i,j) = this%m_alb(i+1,j+1)- &
                                this%m_alb(i-1,j-1)/ &
                     (2._R64P*eps*DMAX1(DABS(tmp),isf)*DSIGN(tmp,tmp))
            end do
        end do
!DIR$  ENDIF
    end function
    
end module mod_albedo