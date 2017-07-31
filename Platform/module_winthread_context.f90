
module module_winthread_context

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'module_winthread_context'
 !          
 !          Purpose:
 !                         The module_winthread_context provides user encapsulation
 !                         of Windows raw thread CONTEXT data structure.
 !                         This structure represents CPU state(context) of currently
 !                         running thread.
 !          History:
 !                        Date: 27-06-2017
 !                        Time: 19:39 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                    
 !                      Programmer: Bernard Gingold
 !
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !
 !          
 !
 !                       
 !==================================================================================85   


 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

    use module_logger
    use kernel32
    implicit none
    
    !============================================50
    ! Hardcoded File/module information
    !============================================50

    ! File/module version major
    integer(I32P), parameter, public :: module_winthread_context_major = 1
    
    ! File/module version minor
    integer(I32P), parameter, public :: module_winthread_context_minor = 0
    
    ! File/module version micro
    integer(I32P), parameter, public :: module_winthread_context_micro = 0
    
    ! File/module version full
    integer(I32P), parameter, public :: module_winthread_context_fullver = 1000*module_winthread_context_major + &
                                        100*module_winthread_context_minor + 10*module_winthread_context_micro
    
    ! File/module creation date
    character(*),  parameter, public :: module_winthread_context_creation_date="27-06-2017 19:39 PM GMT+2 (Jun 27 2017 19:39 PM )"
    
    ! File/module build date
    character(*),  parameter, public :: module_winthread_context_build_date=" "
    
    ! File/module name
    character(*),  parameter, public :: module_winthread_context_name="module_winthread_context"
    
    ! File/module author info
    character(*),  parameter, public :: module_winthread_context_author="Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! File/module short description
    character(*),  parameter, public :: module_winthread_context_desciption="Wrapper around Win thread context structure."
    
    !============================================50
    !  Low level hierarchy and organization
    !  of Win thread hardware context
    !
    !============================================50
    
    type :: ThreadHWContext
        
        private
        
        ! Owning process ID
        integer(DWORD)  :: m_pid
        
        ! Owning process handle
        integer(HANDLE) :: m_phandle
        
        ! Thread ID
        integer(DWORD)  :: m_tid
        
        ! Thread handle
        integer(HANDLE) :: m_thandle
        
        ! Thread Priority
        integer(SINT)   :: m_tpriority
        
       
        
        ! Logical Processor information
        type(T_SYSTEM_LOGICAL_PROCESSOR_INFORMATION), dimension(*) :: m_logcpus
        
        ! Current thread HW Context (64-bit CPU)
        ! Project must be compiled as a 64-bit one,
        ! otherwise T_CONTEXT structure will be undefined!!
        type(T_CONTEXT) :: m_tcontext
        
        ! Is object successfully built
        logical(I32P)   :: m_Isbuilt
        
        contains
    
        !========================================50
        ! Type-bounds getters
        !========================================50
        
        procedure, pass(this), public :: get_pid
        
        procedure, pass(this), public :: get_phandle
        
        procedure, pass(this), public :: get_tid
        
        procedure, pass(this), public :: get_thandle
        
        procedure, pass(this), public :: get_tpriority
        
        procedure, pass(this), public :: get_logcpus
        
        procedure, pass(this), public :: get_tcontext
        
        procedure, pass(this), public :: get_tcontextptr
        
        procedure, pass(this), public :: get_processor_mask
        
        procedure, pass(this), public :: get_relationship ! LOGICAL_PROCESSOR_RELATIONSHIP
        
        procedure, pass(this), public :: get_flags
        
        procedure, pass(this), public :: get_cache
        
        procedure, pass(this), public :: get_node_number
        
       
        
        procedure, pass(this), public :: get_P1Home       !*****  Register parameter home addresses
        
        procedure, pass(this), public :: get_P2Home
        
        procedure, pass(this), public :: get_P3Home
        
        procedure, pass(this), public :: get_P4Home
        
        procedure, pass(this), public :: get_P5Home
        
        procedure, pass(this), public :: get_P6Home
        
        procedure, pass(this), public :: get_contextflags  !****** Control flags
        
        procedure, pass(this), public :: get_MxCsr
        
        procedure, pass(this), public :: get_SegCs         !****** Segment Registers
        
        procedure, pass(this), public :: get_SegDs
        
        procedure, pass(this), public :: get_SegEs
        
        procedure, pass(this), public :: get_SegGs
        
        procedure, pass(this), public :: get_SegFs
        
        procedure, pass(this), public :: get_SegSs
        
        procedure, pass(this), public :: get_EFlags
        
        procedure, pass(this), public :: get_Dr0          !****** Debug Registers
        
        procedure, pass(this), public :: get_Dr1
        
        procedure, pass(this), public :: get_Dr2
        
        procedure, pass(this), public :: get_Dr3
        
        procedure, pass(this), public :: get_Dr6
        
        procedure, pass(this), public :: get_Dr7
        
        procedure, pass(this), public :: get_Rax           !****** GP Registers
        
        procedure, pass(this), public :: get_Rcx
        
        procedure, pass(this), public :: get_Rdx
        
        procedure, pass(this), public :: get_Rbx
        
        procedure, pass(this), public :: get_Rsp
        
        procedure, pass(this), public :: get_Rbp
        
        procedure, pass(this), public :: get_Rsi
        
        procedure, pass(this), public :: get_Rdi
        
        procedure, pass(this), public :: get_R8
        
        procedure, pass(this), public :: get_R10
        
        procedure, pass(this), public :: get_R11
        
        procedure, pass(this), public :: get_R12
        
        procedure, pass(this), public :: get_R13
        
        procedure, pass(this), public :: get_R14
        
        procedure, pass(this), public :: get_R15
        
        procedure, pass(this), public :: get_Rip
        
        procedure, pass(this), public :: get_Xmm0      !****** FP-Vector registers: SIMD, SSE Arch. only
        
        procedure, pass(this), public :: get_Xmm1
        
        procedure, pass(this), public :: get_Xmm2
        
        procedure, pass(this), public :: get_Xmm3
        
        procedure, pass(this), public :: get_Xmm4
        
        procedure, pass(this), public :: get_Xmm5
        
        procedure, pass(this), public :: get_Xmm6
        
        procedure, pass(this), public :: get_Xmm7
        
        procedure, pass(this), public :: get_Xmm8
        
        procedure, pass(this), public :: get_Xmm9
        
        procedure, pass(this), public :: get_Xmm10
        
        procedure, pass(this), public :: get_Xmm11
        
        procedure, pass(this), public :: get_Xmm12
        
        procedure, pass(this), public :: get_Xmm13
        
        procedure, pass(this), public :: get_Xmm14
        
        procedure, pass(this), public :: get_Xmm15
        
        procedure, pass(this), public :: get_FltSave
        
        procedure, pass(this), public :: get_DebugControl
        
        procedure, pass(this), public :: get_LastBranchToRip
        
        procedure, pass(this), public :: get_LastBranchFromRip
        
        procedure, pass(this), public :: get_LastExceptionToRip
        
        procedure, pass(this), public :: get_LastExceptionFromRip
        
        procedure, pass(this), public :: get_VectorControl
        
        procedure, pass(this), public :: get_VectorRegisters
        
        
        
        procedure, nopass,     public :: print_toFile
        
        procedure, nopass,     public :: print_toScreen
        
       
    
    end type


       
    interface ThreadHWContext
      procedure constructor
      procedure copy_ctor
    end interface
    
    contains
    
    !============================================50
    ! Implementation of class ThreadHWContext
    !============================================50

    !============================================50
    !  function:
    !              Class constructor
    !   Purpose:
    !              Creates object of type:
    !              ThreadHWContext.
    !============================================50

    type(ThreadHWContext) function constructor(retlen,relationship,logerror, &
                                               append  )
          use ISO_FORTRAN_ENV,       only : ERROR_UNIT
          use ifcore,                only : TRACEBACKQQ
          use module_logging_events, only : createWinErrorEvent
          implicit none
          integer(DWORD), intent(inout)           :: retlen
          integer(DWORD), intent(in)              :: relationship
          logical(I32P),  intent(in), optional    :: logerror
          logical(I32P),  intent(in), optional    :: append
          
          
          ! Locals
!DIR$ IF ENABLE_LOGGING == 1
          character(len=256),parameter            :: logname="LoggerEvents\ThreadHWContext_Error.log"
          type(WinError_t)                        :: winerr1,winerr2
          character(*), parameter                 :: msg="[FATAL-ERROR]: in ThreadHWContext ctor!!"
          character(len=40)                       :: date_str,time_str
!DIR$ ENDIF         
          integer(BOOL)                           :: berr1,berr2
          ! Start of executable statements
          constructor%m_pid       = GetCurrentProcessId()
          constructor%m_phandle   = GetCurrentProcess()
          constructor%m_tid       = GetCurrentThreadId()
          constructor%m_thandle   = GetCurrentThread()
          constructor%m_tpriority = GetThreadPriority(constructor%m_thandle)
          berr1 = GetLogicalProcessorInformationEx(relationship, &
                                               constructor%m_logcpus,retlen)
          if(berr1 .EQ. .false.) then
!DIR$ IF (ENABLE_LOGGING .EQ. 1) 
             if(present(logerror) .AND. (logerror .EQ. .true.)) then
                 
                 
                 call createWinErrorEvent(winerr1,"Generic Windows Error Event", &
                                          msg,"module_winthread_context.f90",    &
                                          "module_winthread_context",            &
                                          "ThreadHWContext%constructor",332,347,1 )
                 call logger_run(log_fileunit,winerr1,logname,append, &
                                 "WinErrorEvent"," ")
                 ERROR STOP "[FATAL-EROR]: ThreadHWContext --> Failed to build object."
             end if
!DIR$ ENDIF
             write(ERROR_UNIT,*) "[FATAL-ERROR]: Failed to build: ThreadHWContex object." 
             write(ERROR_UNIT,*) "GetLogicalProcessorEx failed with an error: ", GetLastError()
!DIR$ IF (SHOW_CALLSTACK .EQ. 1)
             call TRACEBACKQQ("Dumping call stack", -1)
!DIR$ ENDIF
             ERROR STOP "[FATAL-EROR]: ThreadHWContext --> Failed to build object."
          end if
          berr2 = GetThreadContext(constructor%m_thandle, &
                                   constructor%m_tcontext)
          
          if(berr2 .EQ. .false.) then
!DIR$ IF (ENABLE_LOGGING .EQ. 1)
              if(present(logerror) .AND. (logerror .EQ. .true.)) then
                
                
                  call createWinErrorEvent(winerr2,"Generic Windows Error Event", &
                                           msg,"module_winthread_context.f90",    &
                                           "module_winthread_context",            &
                                           "ThreadHWContext%constructor",362,387,1 )
                                           
                  call logger_run( log_fileunit,winerr2,logname, &
                                  append,"WinErrorEvent","")
                 ERROR STOP "[FATAL-EROR]: ThreadHWContext --> Failed to build object."
              end if
!DIR$ ENDIF
             write(ERROR_UNIT,*) "[FATAL-ERROR]: Failed to build: ThreadHWContex object." 
             write(ERROR_UNIT,*) "GetThreadContext failed with an error: ", GetLastError()
!DIR$ IF (SHOW_CALLSTACK .EQ. 1)
             call TRACEBACKQQ("Dumping call stack", -1)
!DIR$ ENDIF
             ERROR STOP "[FATAL-EROR]: ThreadHWContext --> Failed to build object."
          end if
          constructor%m_Isbuilt = .true.
          
    end function
    
    type(ThreadHWContext) function copy_ctor(other)
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          use ifcore, only : TRACEBACKQQ
          implicit none
          class(ThreadHWContext), intent(in) :: other
          ! Start of executable statements
          ! Sanity checking of argument state
          if(other%m_Isbuilt  .EQ. .false.) then
              write(ERROR_UNIT,*) "============================================"
              write(ERROR_UNIT,*) "  ThreadHWContext%copy_ctor: [FATAL-ERROR]"
              write(ERROR_UNIT,*) "  Cannot copy from invalid state!!"
              write(ERROR_UNIT,*) "  other%m_Isbuilt=", other%m_Isbuilt
              write(ERROR_UNIT,*) "============================================"
!DIR$ IF(SHOW_CALLSTACK .EQ. 1)
              call TRACEBACKQQ("Dumping call stack:",-1)
!DIR$ ENDIF
              ERROR STOP "Terminating execution: [FATAL-ERROR]"
          end if
          copy_ctor%m_pid       = other%m_pid
          copy_ctor%m_phandle   = other%m_phandle
          copy_ctor%m_tid       = other%m_tid
          copy_ctor%m_thandle   = other%m_thandle
          copy_ctor%m_tpriority = other%m_tpriority
          copy_ctor%m_logcpus   = other%m_logcpus
          copy_ctor%m_tcontext  = other%m_tcontext
          copy_ctor%m_Isbuilt   = other%m_Isbuilt
    end function
    
    pure function get_pid(this) result(pid)
          implicit none
          class(ThreadHwContext), intent(in) :: this
          integer(DWORD) :: pid
          ! Start of executable statements
          pid = this%m_pid
    end function
    
    pure function get_phandle(this) result(phandle)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          integer(HANDLE) :: phandle
          ! Start of executable statements
          phandle = this%m_phandle
    end function
    
    pure function get_tid(this) result(tid)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          integer(DWORD) :: tid
          ! Start of executable statements
          tid = this%m_tid
    end function
    
    pure function get_thandle(this) result(thandle)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          integer(HANDLE) :: thandle
          ! Start of executable statements.
          thandle = this%m_thandle
    end function
    
    pure function get_tpriority(this) result(tpriority)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          integer(SINT) :: tpriority
          ! Start of executable statements
          tpriority = this%m_tpriority
    end function
    
    pure function get_logcpus(this) result(logcpus)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          type(T_SYSTEM_LOGICAL_PROCESSOR_INFORMATION), dimension(*) :: logcpus
          ! Start of executable statements
          logcpus = this%m_logcpus
    end function
    
    pure function get_tcontext(this) result(tcontext)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          type(T_CONTEXT) :: tcontext
          ! Start of executable statements
          tcontext = this%m_tcontext
    end function

    pure function get_tcontextptr(tcontext) result(tcontextptr)
          implicit none
          type(T_CONTEXT), target, intent(in) :: tcontext
          ! Locals
          type(T_CONTEXT), pointer :: tcontextptr => null()
          ! Start of executable statements
          tcontextptr => this%m_tcontext
    end function      
          
    pure function get_processor_mask(this,lenght) result(procmask)
          implicit none
          class(ThreadHWContext), intent(in)    :: this
          integer(DWORD)                        :: length
          ! Locals
          integer(ULONG_PTR), dimension(lenght) :: procmask
          integer(DWORD)                        :: i
          ! Start of executable statements
          do i = 1, length
              procmask(i) = this%m_logcpus(i)%ProcessorMask
          end do
    end function
    
    pure function get_relationship(this,length) result(relationships)
          implicit none
          class(ThreadHWContext), intent(in)  :: this
          integer(DWORD),         intent(in)  :: length
          ! Locals
          integer(DWORD), dimension(length)   :: relationships
          integer(DWORD)                      :: i
          ! Start of executable statements
          do i = 1, length
              relationships(i) = this%m_logcpus(i)%Relationship
          end do
    end function
    
    pure function get_flags(this,length) result(flags)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          integer(DWORD),         intent(in) :: length
          ! Locals
          integer(BYTE), dimension(length)   :: flags
          integer(DWORD)                     :: i
          ! Start of executable statements
          do i = 1, length
              flags(i) = this%m_logcpus(i)%Flags
          end do
    end function
    
    pure function get_cache(this,length) result(cache)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          integer(DWORD),         intent(in) :: length
          ! Locals
          type(T_CACHE_DESCRIPTOR), dimension(length) :: cache
          integer(DWORD)                              :: i
          ! Start of executable statements
          do i = 1, length
              cache(i) = this%m_logcpus(i)%Cache
          end do
    end function
    
    pure function get_node_number(this,length) result(nnumber)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          integer(DWORD),         intent(in) :: length
          ! Locals
          integer(DWORD), dimension(length)  :: nnumber
          integer(DWORD)                     :: i
          ! Start of executable statements
          do i = 1, length
              nnumber(i) = this%m_logcpus(i)%NodeNumber
          end do
    end function
    
    pure function get_P1Home(this) result(P1Home)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: P1Home
          ! Start of executable statements
          P1Home = this%m_tcontext%P1Home
    end function
    
    pure function get_P2Home(this) result(P2Home)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: P2Home
          ! Start of executble statements
          P2Home = this%m_tcontext%P2Home
    end function
    
    pure function get_P3Home(this) result(P3Home)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: P3Home
          ! Start of executable statements
          P3Home = this%m_tcontext%P3Home
    end function
    
    pure function get_P4Home(this) result(P4Home)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: P4Home
          ! Start of executable statements
          P4Home = this%m_tcontext%P4Home
    end function
    
    pure function get_P5Home(this) result(P5Home)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: P5Home
          ! Start of executable
          P5Home = this%m_tcontext%P5Home
    end function
    
    pure function get_P6Home(this) result(P6Home)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: P6Home
          ! Start of executable
          P6Home = this%m_tcontext%P6Home
    end function
    
    pure function get_ContextFlags(this) result(ContextFlags)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD)                     :: ContextFlags
          ! Start of executable statements
          ContextFlags = this%m_tcontext%ContextFlags
    end function
    
    pure function get_MxCsr(this) result(MxCsr)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD)                     :: MxCsr
          ! Start of executable statements
          MxCsr = this%m_tcontext%MxCsr
    end function
    
    pure function get_SegCs(this) result(SegCs)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(WORD)                      :: SegCs
          ! Start of executable statements
          SegCs = this%m_tcontext%SegCs
    end function
    
    pure function get_SegDs(this) result(SegDs)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(WORD)                      :: SegDs
          ! Start of executable statements
          SegDs = this%m_tcontext%SegDs
    end function
    
    pure function get_SegEs(this) result(SegEs)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(WORD)                      :: SegEs
          ! Start of excutable statements
          SegEs = this%m_tcontext%SegEs
    end function
    
    pure function get_SegFs(this) result(SegFs)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(WORD)                      :: SegFs
          ! Start of executable statements
          SegFs = this%m_tcontext%SegFs
    end function
    
    pure function get_SegGs(this) result(SegGs)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(WORD)                      :: SegGs
          ! Start of execuatble statements
          SegGs = this%m_tcontext%SegGs
    end function
    
    pure function get_EFlags(this) result(EFlags)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD)                     :: EFlags
          ! Start of executable statements
          EFlags = this%m_tcontext%EFlags
    end function
    
    pure function get_Dr0(this) result(Dr0)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Dr0
          ! Start of executable statements
          Dr0 = this%m_tcontext%Dr0
    end function
    
    pure function get_Dr1(this) result(Dr1)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Dr1
          ! Start of executable statements
          Dr1 = this%m_tcontext%Dr1
    end function
    
    pure function get_Dr2(this) result(Dr2)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Dr2
          ! Start of executable
          Dr2 = this%m_tcontext%Dr2
    end function
    
    pure function get_Dr3(this) result(Dr3)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Dr3
          ! Start of executable statements
          Dr3 = this%m_tcontext%Dr3
    end function
    
    pure function get_Dr6(this) result(Dr6)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Dr6
          ! Start of executable statements
          Dr6 = this%m_tcontext%Dr6
    end function
    
    pure function get_Dr7(this) result(Dr7)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Dr7
          ! Start of executable statements
          Dr7 = this%m_tcontext%Dr7
    end function
    
    pure function get_Rax(this) result(Rax)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Rax
          ! Start of executable
          Rax = this%m_tcontext%Rax
    end function
    
    pure function get_Rcx(this) result(Rcx)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Rcx
          ! Start of executable statements
          Rcx = this%m_tcontext%Rcx
    end function
    
    pure function get_Rdx(this) result(Rdx)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Rdx
          ! Start of executable statements
          Rdx = this%m_tcontext%Rdx
    end function
    
    pure function get_Rbx(this) result(Rbx)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Rbx
          ! Start of executable statements
          Rbx = this%m_tcontext%Rbx
    end function
    
    pure function get_Rsp(this) result(Rsp)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Rsp
          ! Start of executable statements
          Rsp = this%m_tcontext%Rsp
    end function
    
    pure function get_Rbp(this) result(Rbp)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Rbp
          ! Start of executable stataments
          Rbp = this%m_tcontext%Rbp
    end function
    
    pure function get_Rsi(this) result(Rsi)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Rsi
          ! Start of executable statements
          Rsi = this%m_tcontext%Rsi
    end function
    
    pure function get_Rdi(this) result(Rdi)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Rdi
          ! Start of executable statements
          Rdi = this%m_tcontext%Rdi
    end function
    
    pure function get_R8(this) result(R8)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: R8
          ! Start of executable statements
          R8 = this%m_tcontext%R8
    end function
    
    pure function get_R10(this) result(R10)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: R10
          ! Start of executable statememtns
          R10 = this%m_tcontext%R10
    end function
    
    pure function get_R11(this) result(R11)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: R11
          ! Start of executable statements
          R11 = this%m_tcontext%R11
    end function
    
    pure function get_R12(this) result(R12)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: R12
          ! Start of executable statements
          R12 = this%m_tcontext%R12
    end function
    
    pure function get_R13(this) result(R13)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: R13
          ! Start of executable statements
          R13 = this%m_tcontext%R13
    end function
    
    pure function get_R14(this) result(R14)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: R14
          ! Start of executable statements
          R14 = this%m_tcontext%R14
    end function
    
    pure function get_R15(this) result(R15)
          implicit none
          class(ThreadHWContext), intent(in) ::  this
          ! Locals
          integer(DWORD64)                   :: R15
          ! Start of executable statements
          R15 = this%m_tcontext%R15
    end function
    
    pure function get_Rip(this) result(Rip)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: Rip
          ! Start of executable statements
          Rip = this%m_tcontext%Rip
    end function
    
    pure function get_Xmm0(this) result(Xmm0)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm0
          ! Start of executable statements
          Xmm0 = this%m_tcontext%Xmm0
    end function
    
    pure function get_Xmm1(this) result(Xmm1)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm1
          ! Start of executable statements
          Xmm1 = this%m_tcontext%Xmm1
    end function
    
    pure function get_Xmm2(this) result(Xmm2)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm2
          ! Start of executable statements
          Xmm2 = this%m_tcontext%Xmm2
    end function
    
    pure function get_Xmm3(this) result(Xmm3)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm3
          ! Start of executable statements
          Xmm3 = this%m_tcontext%Xmm3
    end function
    
    pure function get_Xmm4(this) result(Xmm4)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm4
          ! Start of executable statements
          Xmm4 = this%m_tcontext%Xmm4
    end function
    
    pure function get_Xmm5(this) result(Xmm5)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm5
          ! Start of executable statements
          Xmm5 = this%m_tcontext%Xmm5
    end function
    
    pure function get_Xmm6(this) result(Xmm6)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm6
          ! Start of executable statements
          Xmm6 = this%m_tcontext%Xmm6
    end function
    
    pure function get_Xmm7(this) result(Xmm7)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm7
          ! Start of executable statements
          Xmm7 = this%m_tcontext%Xmm7
    end function
    
    pure function get_Xmm8(this) result(Xmm8)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm8
          ! Start of executable statements
          Xmm8 = this%m_tcontext%Xmm8
    end function
    
    pure function get_Xmm9(this) result(Xmm9)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm9
          ! Start of executable statements
          Xmm9 = this%m_tcontext%Xmm9
    end function
    
    pure function get_Xmm10(this) result(Xmm10)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm10
          ! Start of executable statements
          Xmm10 = this%m_tcontext%Xmm10
    end function
    
    pure function get_Xmm11(this) result(Xmm11)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm11
          ! Start of executable statements
          Xmm11 = this%m_tcontext%Xmm11
    end function
    
    pure function get_Xmm12(this) result(Xmm12)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm12
          ! Start of executable statements
          Xmm12 = this%m_tcontext%Xmm12
    end function
    
    pure function get_Xmm13(this) result(Xmm13)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm13
          ! Start of executable statements
          Xmm13 = this%m_tcontext%Xmm13
    end function
    
    pure function get_Xmm14(this) result(Xmm14)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm14
          ! Start of executable statements
          Xmm14 = this%m_tcontext%Xmm14
    end function
    
    pure function get_Xmm15(this) result(Xmm15)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128)                       :: Xmm15
          ! Start of executable statements
          Xmm15 = this%m_tcontext%Xmm15
    end function
    
    pure function get_FltSave(this) result(FltSave)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_LEGACY_SAVE_AREA)           :: FltSave
          ! Start of executable statements
          FltSave = this%m_tcontext%FltSave
    end function
    
    pure function get_DebugControl(this) result(DbgCtrl)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: DbgCtrl
          ! Start of executable sattements
          DbgCtrl = this%m_tcontext%DebugControl
    end function
    
    pure function get_LastBranchToRip(this) result(LBTR)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: LBTR
          ! Start of executable statements
          LBTR = this%m_tcontext%LastBranchToRip
    end function
    
    pure function get_LastBranchFromRip(this) result(LBFR)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: LBFR
          ! Start of executable statements
          LBFR = this%m_tcontext%LastBranchFromRip
    end function
    
    pure function get_LastExceptionToRip(this) result(LETR)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: LETR
          ! Start of executable statements
          LETR = this%m_tcontext%LastExceptionToRip
    end function
    
    pure function get_LastExceptionFromRip(this) result(LEFR)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: LEFR
          ! Start of executable statements
          LEFR = this%m_tcontext%LastExceptionFromRip
    end function
    
    pure function get_VectorControl(this) result(VecCtrl)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          integer(DWORD64)                   :: VecCtrl
          ! Start of executable statements
          VecCtrl = this%m_tcontext%VectorControl
    end function
    
    pure function get_VectorRegister(this) result(VecReg)
          implicit none
          class(ThreadHWContext), intent(in) :: this
          ! Locals
          type(T_M128), dimension(16)        :: VecReg
          ! Start of executable statements
          VecReg = this%m_tcontext%VectorRegister
    end function
    
    subroutine print_toFile(this,filename,unit, &
                            ioerr,errmsg     )
          use ISO_FORTRAN_ENV, only ERROR_UNIT
          implicit none
          class(ThreadHWContext), intent(inout) :: this
          character(len=256),     intent(in)    :: filename
          integer(I32P),          intent(in)    :: unit
          integer(I32P),          intent(inout) :: ioerr
          character(len=*),       intent(inout) :: errmsg
          ! Start of executable statements
          open(UNIT=unit,FILE=filename,STATUS="NEW",ACTION="WRITE", &
               IOSTAT=ioerr, IOMSG=errmsg)
          if(ioerr > 0) then
             write(ERROR_UNIT,*) "==========================================="
             write(ERROR_UNIT,*) "  An error occurred while opening the file"
             write(ERROR_UNIT,*) "  I/O status=", ioerr
             write(ERROR_UNIT,*) "  I/O message=", TRIM(errmsg)
             write(ERROR_UNIT,*) "==========================================="
             return
          else
             write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) this
          endif
          if(ioerr > 0) then
             write(ERROR_UNIT,*) "==========================================="
             write(ERROR_UNIT,*) "  An error occurred while writing to file"
             write(ERROR_UNIT,*) "  I/O status=", ioerr
             write(ERROR_UNIT,*) "  I/O message=", TRIM(errmsg)
             write(ERROR_UNIT,*) "===========================================" 
             return
          end if
          close(UNIT=unit)
    end subroutine
    
    subroutine print_toScreen(this,ioerr,iomsg)
          use ISO_FORTRAN_ENV, only : ERROR_UNIT,OUTPUT_UNIT
          implicit none
          class(ThreadHWContext), intent(inout) :: this
          integer(I32P),          intent(inout) :: ioerr
          character(len=*),       intent(inout) :: iomsg
          ! Start of executable statements
          write(OUTPUT_UNIT,*,IOSTAT=ioerr,IOMSG=iomsg) this
          if(ioerr > 0) then
             write(ERROR_UNIT,*) "==========================================="
             write(ERROR_UNIT,*) "  An error occurred while printing to screen"
             write(ERROR_UNIT,*) "  I/O status=", ioerr
             write(ERROR_UNIT,*) "  I/O message=", TRIM(iomsg)
             write(ERROR_UNIT,*) "==========================================="
          end if
    end subroutine
    
   
  
    
end module