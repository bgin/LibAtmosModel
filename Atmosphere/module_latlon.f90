
module mod_latlon

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_latlon'
 !          
 !          Purpose:
 !                    Latitude-longtitude geographical coordinates.
 !                    This class is simple wrapper which conveniently
 !                    handles creation of geographiv coordinates(lat-lon).
 !                   
 !                     
 !          History:
 !                        Date: 15-09-2017
 !                        Time: 09:36 GMT+2
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
    integer(I32P), parameter, public :: MOD_LATLON_MAJOR = 1
    
    ! Version minor
    integer(I32P), parameter, public :: MOD_LATLON_MINOR = 0
    
    ! Version micro
    integer(I32P), parameter, public :: MOD_LATLON_MICRO = 0
    
    ! Module/file full version
    integer(I32P), parameter, public :: MOD_LATLON_FULLVER = 1000*MOD_LATLON_MAJOR+100*MOD_LATLON_MINOR+ &
                                                             10*MOD_LATLON_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_LATLON_CREATE_DATE = "15-09-2017 14:09 +00200 (FRI 15 SEP 2017 GMT+2)"
    
    ! Module build date (should be set after successful build date/time)
    character(*),  parameter, public :: MOD_LATLON_BUILD_DATE = " "
    
    ! Module author info.
    character(*),  parameter, public :: MOD_LATLON_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description.
    character(*),  parameter, public :: MOD_LATLON_DESCRIPT = "Latitude-longtitude geographical coordinates."
    
    !======================================================60
    !  Type: LatLon
    !======================================================60
    
    type, public :: LatLon
        
         ! public by default.
         ! indices
         integer(I64P) :: m_ims,m_ime,m_jms,m_jme, &
                          m_ids,m_ide,m_jds,m_jde, &
                          m_its,m_ite,m_jts,m_jte
         
        ! Lat-lon coordinates
        ! m_lat --  latitude, south is negative (degree)
        ! m_long -- longitude, west is negative (degree)
        real(R64P), allocatable, dimension(:,:) :: m_lat,m_long
        
!DIR$   ATTRIBUTES ALIGN : 32 :: m_lat
!DIR$   ATTRIBUTES ALIGN : 32 :: m_long
        
        ! built indicator (logical)
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
        
        procedure, nopass, public :: read_latlon
        
        procedure, nopass, public :: write_latlon
    
    end type LatLon
        
       !=================================================60
       !  Module operators
       !=================================================60  
        
       interface assignment (=)
            module procedure assign_latlon
       end interface 
       
       interface operator (/=)
            module procedure latlon_neq_latlon
       end interface
       
       interface operator (==)
            module procedure latlon_eq_latlon
       end interface
       
       interface operator (>)
            module procedure latlon_gt_latlon
       end interface
       
       interface operator (<)
             module procedure latlon_lt_latlon
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
    subroutine init(this,indices,lat,long,logging,    &
                    profiling,filename,append,dbg,err,qpctimer)
          implicit none
          class(LatLon),                intent(inout) :: this
          integer(I64P), dimension(12), intent(in)    :: indices
          real(R64P), dimension(:,:),   intent(in)    :: lat,long
!DIR$     ASSUME_ALIGNED lat:32,long:32
          logical(I32P),                intent(in)    :: logging,profiling
          character(len=*),             intent(in)    :: filename
          logical(I32P),                intent(in)    :: append,dbg
          integer(I32P),                intent(inout) :: err
          type(QPCTimer_t),             intent(inout) :: qpctimer
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I64P)      :: j,i
          integer(BOOL)      :: ifail
          logical(I32P)      :: bfail
!DIR$  IF (USE_LOOP_BLOCKING .EQ. 1)
          integer(I64P)      :: jj,ii
!DIR$  ENDIF          
          integer(I32P)      :: aerr
          ! Start of executable statements
          if(err.LT.0) err = 0
          if(array2D_not_alloc(lat) .OR. &
             array2D_not_alloc(long)) then
              err = -1
              return
          end if
          if(this%m_isbuilt .EQ. .true.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:182, In->mod_latlon/init: LatLon already initialized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_latlon/init:182, LatLon already initialized!)"
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
          ! Array alocation
          associate(d1s=>this%m_ims, &
                    d1e=>this%m_ime, &
                    d2s=>this%m_jms, &
                    d2e=>this%m_jme )
              allocate(this%m_lat(d1s:d1e,d2s:d2e),   &  
                       this%m_long(d1s:d1e,d2s:d2e),  &
                       STAT=aerr,                     &
                       ERRMSG=emsg )
          end associate
          if(aerr.NE.0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:221, In->mod_latlon/init: Failed to allocate: [m_coord] ")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(ERROR_UNIT,*) "===========================FATAL========================="
                  write(ERROR_UNIT,*) "   (mod_latlon/init:221, Failed to allocate: [m_coord)"
                  write(ERROR_UNIT,*) "   (System message:)", emsg
                  write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(ERROR_UNIT,*) "===========================FATAL=========================="
              end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_latlon/init:221 -> [FATAL-ERROR]: Terminating execution!!"
          end if
          !Array initilaization
          if(profiling) then
              call qpctimer_start(qpctimer,ifail)
              if(ifail.EQ.0) then
                  write(stderr,*) "mod_latlon/init: qpctimer_start failed to query performance frequency counter!"
              end if
          end if
!DIR$  IF (USE_LOOP_BLOCKING .EQ. 1)
          do j = this%m_jms, this%m_jme, DEFAULT_BLOCK_SIZE
              do i = this%m_ims, this%m_kme, DEFAULT_BLOCK_SIZE
                  do jj = j, DEFAULT_BLOCK_SIZE
!DIR$                 SIMD VECTORLENGTHFOR(REAL(KIND=8))                      
                      do ii = i, DEFAULT_BLOCK_SIZE
!DIR$           IF (USE_SOFT_PREFETCH .EQ. 1)
                          call MM_PREFETCH(lat(ii+1,jj),1)
                          call MM_PREFETCH(long(ii+1,jj),1)
!DIR$           ENDIF
                          this%m_lat(ii,jj) = lat(ii,jj)
                          this%m_long(ii,jj) = long(ii,jj)
                      end do
                  end do
              end do
          end do
!DIR$ ELSE
          do j = this%m_jms, this%m_jme
!DIR$         SIMD VECTORLENGTHFOR(REAL(KIND=8))              
              do i = this%m_ims, this%m_ime
!DIR$         IF (USE_SOFT_PREFETCH .EQ. 1)
                  call MM_PREFETCH(lat(i+1,j),1)
                  call MM_PREFETCH(long(i+1,j),1)
!DIR$         ENDIF
                  this%m_lat(i,j) = lat(i,j)
                  this%m_long(i,j) = long(i,j)
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
                         write(stderr,*) "mod_latlon/init: qpctimer_delta: failed to compute delta measurement!!"
                    end if  
                else
                    write(stderr,*) "mod_latlon/init: Unable to read performance counter -- fatal!!"
                end if
          end if
          this%m_isbuilt = .true.
          if(dbg) then
              print*, " Latitude:  ", this%m_lat
              print*, " Longtitude:", this%m_long
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
          class(LatLon),    intent(inout) :: this
          class(LatLon),    intent(in)    :: other
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
                  call log_UsrMsg("logger:330, In->mod_latlon/copy: LatLon already initilaized!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_latlon/copy:330, LatLon already initialized!)"
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
          this%m_lat=other%m_lat
          this%m_long=other%m_long
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
          class(LatLon),    intent(inout) :: this
          logical(I32P),    intent(in)    :: logging
          character(len=*), intent(in)    :: filename
          logical(I32P),    intent(in)    :: append
          integer(I32P),    intent(inout) :: err
          ! Locals
          character(len=40)  :: dstr,tstr
          character(len=256) :: emsg
          integer(I32P)      :: derr
          ! Start of executable statemetns
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .false.) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:384, In->mod_latlon/destroy: LatLon already destroyed!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=dstr,time=tstr)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_latlon/destroy:330, LatLon already destroyed!)"
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
          ! Arrays deallocation
          if(ALLOCATED(this%m_lat)) then
              deallocate(this%m_lat, &
                         STAT=derr,  &
                         ERRMSG=emsg)
               if(derr.NE.0) then
                   if(logging) then
                       call log_startup(filename,append)
                       call log_UsrMsg("logger:418, In->mod_latlon/destroy: Failed to deallocate: [m_lat] !!")
                       call log_shutdown()
                   else
                       call DATE_AND_TIME(date=dstr,time=tstr)
                       write(ERROR_UNIT,*) "===========================FATAL========================="
                       write(ERROR_UNIT,*) "   (mod_latlon/destroy:418, Failed deallocate: [m_lat]!!)"
                       write(ERROR_UNIT,*) "   (System message:)", emsg
                       write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                       write(ERROR_UNIT,*) "===========================FATAL=========================="
                end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_latlon/destroy:418 -> [FATAL-ERROR]: Terminating execution!!"
              end if 
          end if
          if(ALLOCATED(this%m_long)) then
              deallocate(this%m_long, &
                         STAT=derr,   &
                         ERRMSG=emsg)
              if(derr.NE.0) then
                  if(logging) then
                      call log_startup(filename,append)
                      call log_UsrMsg("logger:443, In->mod_latlon/destroy: Failed to deallocate: [m_long] !!")
                      call log_shutdown()
                  else
                       call DATE_AND_TIME(date=dstr,time=tstr)
                       write(ERROR_UNIT,*) "===========================FATAL========================="
                       write(ERROR_UNIT,*) "   (mod_latlon/destroy:443, Failed deallocate: [m_long]!!)"
                       write(ERROR_UNIT,*) "   (System message:)", emsg
                       write(ERROR_UNIT,*) "   ( Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                       write(ERROR_UNIT,*) "===========================FATAL=========================="
                end if
!DIR$         IF (SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ(STRING="FATAL-ERROR", USER_EXIT_CODE= -1)
!DIR$         ENDIF
              ERROR STOP "mod_latlon/destroy:443 -> [FATAL-ERROR]: Terminating execution!!"
              end if 
          end if
          this%m_isbuilt = .false.
    end subroutine

    !======================================================60
    !  subroutine: read_latlon
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine read_latlon(this,unit,ioerr,err)
          implicit none
          class(LatLon), intent(in)    :: this
          integer,       intent(in)    :: unit
          integer(I32P), intent(inout) :: ioerr,err
          ! Strat of executable statements
          if(err.LT.0) err = 0
          if(this%m_isbuilt .EQ. .false.) then
              err = -1
              return
          end if
          READ(unit,iostat=ioerr) this
    end subroutine
    
    !======================================================60
    !  subroutine: write_latlon
    !  Remark:
    !           Upon detecting non-fatal error inout
    !           integer indicator 'err' will be set to -1.
    !======================================================60
    subroutine write_latlon(this,unit,ioerr,err)
          implicit none
          class(LatLon), intent(in)    :: this
          integer,       intent(in)    :: unit
          integer(I32P), intent(inout) :: ioerr,err
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
    subroutine assign_latlon(this,other)
          implicit none
          type(LatLon), intent(inout) :: this
          type(LatLon), intent(in)    :: other
          ! Start of executable statements
          if(LOC(this).EQ.LOC(other)) then
               call DATE_AND_TIME(date=dstr,time=tstr)
               write(stderr,*) "===========================NON-FATAL=========================="
               write(sdterr,*) " ( mod_latlon/assignment(=):519, Attempted self_assignment!)"
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
          this%m_lat=other%m_lat
          this%m_long=other%m_long
          this%m_isbuilt = .true.
    end subroutine
    
    !======================================================60
    ! function: latlon_neq_latlon i.e. overloaded
    !           operator (/=)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function latlon_neq_latlon(c1,c2) result(neq)
         use module_helper_fields_equality
         implicit none
         type(LatLon), intent(in) :: c1,c2
         type(BoolCompField2D)    :: neq
         ! Start of executable statemetns
         neq%e1   = c1%m_lat  /= c2%m_lat
         neq%e2 = c1%m_long /= c2%m_long
    end function
    
    !======================================================60
    ! function: latlon_eq_latlon i.e. overloaded
    !           operator (==)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function latlon_eq_latlon(c1,c2) result(eq)
         use module_helper_fields_equality
         implicit none
         type(LatLon), intent(in) :: c1,c2
         type(BoolCompField2D)    :: eq
         ! Start of executable statements
         eq%e1 = c1%m_lat  == c2%m_lat
         eq%e2 = c1%m_long == c2%m_long
    end function
    
    !======================================================60
    ! function: latlon_gt_latlon i.e. overloaded
    !           operator (>)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function latlon_gt_latlon(c1,c2) result(gt)
          use module_helper_fields_equality
          implicit none
          type(LatLon), intent(in) :: c1,c2
          type(BoolCompField2D)    :: eq
          ! Start of executable stateemtns
          gt%e1 = c1%m_lat  > c2%m_lat
          gt%e2 = c1%m_long > c2%m_long
    end function
    
    !======================================================60
    ! function: latlon_lt_latlon i.e. overloaded
    !           operator (<)
    ! Warning:  No error checking is made!!
    !           Both arguments should be the same i.e.
    !           Same arrays size.
    !======================================================60
    function latlon_gt_latlon(c1,c2) result(lt)
          use module_helper_fields_equality
          implicit none
          type(LatLon), intent(in) :: c1,c2
          type(BoolCompField2D)    :: eq
          ! Start of executable statements
          lt%eq1 = c1%m_lat  < c2%m_lat
          lt%eq2 = c1%m_long < c2%m_long
    end function
end module mod_latlon