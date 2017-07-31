
module module_logging_events
    
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'module_logging_events'
 !          
 !          Purpose:
 !                        Declaration of logging events derived types. 
 !                        Following derived types are declared: 
 !                        1) PerfTimerEvent_t  
 !                        2) InvArgEvent_t 
 !                        3) FailAllocEvent_t  
 !                        4) IndexOutBoundsEvent_t
 !                        5) FailDeallocEvent_t
 !                        6) FileIOEvent_t
 !                        7) DisassociatedPtrEvent_t
 !                        8) FPTrapUndEvent_t
 !                        9) FPTrapOvfEvent_t
 !                        10) FPTrapDiv0Event_t
 !                        11) FPAbruptUndEvent_t
 !                        12) FPAbruptOvfEvent_t
 !                        13) FPAbruptDiv0Event_t
 !                        14) FPAbruptInvEvent_t
 !                        15) FPAbruptDmzEvent_t
 !                        16) FPPoleEvent_t   // domain error
 !                        17) FPSingularEvent_t  // domain error
 !                        
 !          History:
 !                      Date: 13-06-2017
 !                      Time: 13:42 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                    
 !                      Bernard Gingold
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
    
     use module_kinds, only :  I32P , I64P, R32P, R64P
     use ifwinty,      only :  T_LARGE_INTEGERX , DWORD, LONG, HANDLE
     use kernel32,     only :  GetCurrentProcess, &
                               GetCurrentProcessId, &
                               GetCurrentThread,    &
                               GetCurrentThreadId
     implicit none
    !============================================50
    !        File/Module version info
    !============================================50
    
    ! File major version
    integer(I32P), public, parameter :: module_logging_events_major = 1
    
    ! File minor version
    integer(I32P), public, parameter :: module_logging_events_minor = 0
    
    ! File micro(patch) version
    integer(I32P), public, parameter :: module_logging_events_micro = 0
    
    ! File full version
    integer(I32P), public, parameter :: module_logging_events_version = 1000*module_logging_events_major+100*module_logging_events_minor + &
                                                                        10*module_logging_events_micro
    
    ! Creation date
    character(*), public, parameter  :: module_logging_events_creation_date="13-06-2017 13:42 +00200 (Tue 13 June 2017 GMT+2)"
    
    ! File build date/time , should be set manually after every recompilation
    character(*), public, parameter  :: module_logging_events_build_date=" "
    
    ! Module name
    character(*), public, parameter  :: module_logging_events_name="module_logging_events"
    
    ! Module Author
    character(*), public, parameter  :: module_logging_events_author="Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*), public, parameter  :: module_logging_events_description="Logging events derived types."
    
    !============================================50
    ! Declaration/Definition of derived type
    !============================================50
    
    
    !============================================50
    !  Type: PerfTimerEvent_t
    !============================================50
    
    type :: PerfTimerEvent_t
        
        private
        ! Event name
        character(len=11) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
        ! File name  of measured event (top of hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing timed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing timed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code(start,end) (bottom of hierarchy)
        integer(I32P)     :: m_line_st,m_line_en
        
        ! Date of event  as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event  as returned by date_and_time
        character(len=40) :: m_time
        
        ! Number of timing runs
        integer(I32P)     :: m_nruns
        
        ! Allocatable array of type real(kind=4)
        ! for storing time measurement returned by CPU_TIME
        ! m_timings(1,:) - stores starting values
        ! m_timings(2,:) - stores ending values
        ! m_timings(3,:) - stores delta values
        ! Notification:
        !               Caller is responsible for array allocation/deallocation
        !DIR$ ATTRIBUTES ALIGN : 32 :: m_ctimings
        real(R32P), allocatable, dimension(:,:) :: m_ctimings
        
        ! Allocatable array of type integer(kind=8)
        ! for storing time measurement returned by SYSTEM_CLOCK
        ! m_timings(1,:) - stores starting values
        ! m_timings(2,:) - stores ending values
        ! m_timings(3,:) - stores delta values
        ! Notification:
        !               Caller is responsible for array allocation/deallocation
        !DIR$ ATTRIBUTES ALIGN : 32 :: m_sctimings
        integer(I64P), allocatable, dimension(:,:) :: m_sctimings
        
        ! Allocatable array of type real(kind=8)
        ! for storing time measurement returned by DCLOCK
        ! m_timings(1,:) - stores starting values
        ! m_timings(2,:) - stores ending values
        ! m_timings(3,:) - stores delta values
        ! Notification:
        !               Caller is responsible for array allocation/deallocation
        !DIR$ ATTRIBUTES ALIGN : 32 :: m_dctimings
        real(R64P), allocatable, dimension(:,:) :: m_dctimings
        
        ! Allocatable array of type real(kind=8)
        ! for storing time measurement returned by QueryPerformanceCounter WIN API wrapper
        ! m_timings(1,:) - stores starting values
        ! m_timings(2,:) - stores ending values
        ! m_timings(3,:) - stores delta values
        ! Notification:
        !               Caller is responsible for array allocation/deallocation
        type(T_LARGE_INTEGERX), allocatable, dimension(:,:) :: m_qpctimings
        
        ! Timers results delta.
        !DIR$ ATTRIBUTES ALIGN : 32 :: m_timer_delta
        real(R64P), allocatable, dimension(:,:)  :: m_timers_delta
        
        ! Average value of starting measurements
        real(R64P)         :: m_actimings,m_asctimings, &
                              m_adctimings,m_aqpctimings
        
        ! Average value of stoping measurements
        real(R64P)         :: m_actiminge,m_asctiminge, &
                              m_adctiminge,m_aqpctiminge
        
        ! Average value of delta measurements
        real(R64P)         :: m_actimingd,m_actimingd, &
                              m_adctimingd,m_aqpctimingd
        
        ! Clock facility used to perform time measurement
        ! Position        1 - CPU_TIME * supported
        !   -||-          2 - SYSTEM_CLOCK  * supportd
        !   -||-          3 - C intrinsic like 'rdtsc' ..etc  (experimental)
        !   -||-          4 - DCLOCK * supported
        !   -||-          5 - QueryPerformanceCounter * supprted
        !   -||-          6 - OpenMP timing procedures (experimental)
        character(len=40), dimension(6)     :: m_timer_type
        
        ! Number of loop cycles needed to warm the CPU
        integer(I32P)     :: m_nwcycles
        
        ! Number of code hot-spots
        integer(I32P)     :: m_nhotspots
        
        ! Highest hotspot in term of CPU consumption
        real(R64P)        :: m_maxhotspot
        
        ! Number of GFLOPS per hotspot
        real(R64P)        :: m_gflops
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: InvArgEvent_t
    !============================================50
    
    type :: InvArgEvent_t
        
        private
        
        ! Event name
        character(len=13)   :: m_event_name
        
        ! Custom message
        character(len=80)   :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80)   :: m_file_name
        
        ! Module name
        character(len=80)   :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80)   :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)       :: m_loc_st,m_loc_ed
        
        ! Date of event  as returned by date_and_time
        character(len=40)   :: m_date
        
        ! Time of event as returned by_date_and_time
        character(len=40)   :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(I32P)       :: m_severity
        
        ! Invalid argument(s)  scalar members
        ! are initialized to invalid value(s)
        ! other members of floating-point type 
        ! are initilized to Nan values and integers
        ! are initilized to Huge(integer(kind=4)) or
        ! Huge(integer(kind=8)) values.
        integer(I32P)     :: m_invargi32,m_invargi64
        
        real(R32P)        :: m_invargf32,m_invargf64
        
        ! In case of invalid argument of derived type
        ! then string containing derived type name
        ! is initiliazed by the caller.
        character(len=256) :: m_descript
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FailAllocEvent_t
    !============================================50
    
    type :: FailAllocEvent_t
        
        private
        
        ! Name of event
        character(len=16)  :: m_event_name
        
        ! Custom message
        character(len=80)  :: m_msg
        
        ! File name (top of event hierrarchy)
        character(len=80)  :: m_file_name
        
        ! Module name
        character(len=80)  :: m_module_name
        
         ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)    :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)     :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)    :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)     :: m_tid
        
        ! Procedure name
        character(len=80)  :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)      :: m_loc_st,m_loc_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)  :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)  :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(I32P)       :: m_severity
        
        ! Allocation 'STAT' value
        integer(I32P)       :: m_astat
        
        ! ERRMSG
        character(len=80)   :: m_errmsg
        
         ! Is this event built(initilaized)
        logical(I32P)       :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: IndexOutBoundsEvent_t
    !============================================50
    
    type :: IndexOutBoundsEvent_t
        
        private
        
        ! Event name
        character(len=21)             :: m_event_name
        
        ! Custom message
        character(len=80)             :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80)             :: m_file_name
        
        ! Module name
        character(len=80)             :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)               :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)                :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)               :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)                :: m_tid
        
        ! Procedure name
        character(len=80)             :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)                 :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)             :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)             :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(I32P)                 :: m_severity
        
        ! Integer(kind=4) array which holds expected and actual values i.e.
        ! (array indices)
        ! 1st dimension - expected values
        ! 2nd dimension - actual   values
        integer(I32P), dimension(2,31) :: m_indices32
        
         
        
         ! Is this event built(initilaized)
        logical(I32P)                  :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FailDeallocEvent_t
    !============================================50
    
    type :: FailDeallocEvent_t
        
        private
        
        ! Event name
        character(len=18) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! STAT value
        integer(I32P)     :: m_dstat
        
        ! ERRMSG
        character(len=80) :: m_errmsg
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FileIOEvent_t
    ! Remark: This event describes a failed
    !          file I/O operation.
    !============================================50
    
    type :: FileIOEvent_t
        
        private
        
        ! Event name
        character(len=13) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure context) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Failed I/O operation - name
        character(len=24) :: m_fileop
        
        ! IOSTAT
        integer(I32P)     :: m_iostat
        
        ! IOMSG
        character(len=80) :: m_iomsg
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: DisassocPtrEvent_t
    !============================================50
    
    type :: DisassocPtrEvent_t
        
        private
        
        ! Event name
        character(len=24) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Pointer association status
        logical(I32P)     :: m_status
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FPTrapUndEvent_t
    !============================================50
    
    type :: FPTrapUndEvent_t
        
        private
        
        ! Event name
        character(len=16) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Value of fp variable encoded as a real(kind=8)
        real(R64P)        :: m_undefval
        
        ! Value of FP-Env status flags
        integer(I32P)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FPTrapOvfEvent_t
    !============================================50
    
    type :: FPTrapOvfEvent_t
        
        private
        
        ! Event name
        character(len=16) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Overflow value encoded as a real(kind=8) (its max value).
        real(R64P)        :: m_ovfval
        
        ! FP-Env status flag at exception moment
        integer(I32P)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FPTrapDiv0Event_t
    !============================================50
    
    type :: FPTrapDiv0Event_t
        
        private
        
        ! Event name
        character(len=17) :: m_event_name
        
        ! Custon message
        character(len=80) :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Result of calculation (if possible to obtain)
        real(R64P)        :: m_div0val
        
        ! FP-Env flags
        integer(I32P)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
     
    !============================================50
    ! Type: FPTrapInvEvent_t
    !============================================50
    
    type :: FPTrapInvEvent_t
        
        private
        
        ! Event name
        character(len=16) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
         ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Value of Inv variable (if possible to obtain)
        real(R64P)        :: m_inval
        
        ! FP-Env flags
        integer(I32P)     :: m_flags
        
        ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    
    !============================================50
    ! Type: FPAbruptUndEvent_t
    !============================================50
    
    type :: FPAbruptUndEvent_t
        
        private
        
        ! Event name
        character(len=18) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Value of UND variable (if possible to obtain)
        real(R64P)        :: m_undval
        
        ! FP-Env flags
        integer(I32P)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FPAbruptOvfEvent_t
    !============================================50
    
    type :: FPAbruptOvfEvent_t
        
        private
        
        ! Event name
        character(len=18) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Value of overflown variable 
        real(R64P)        :: m_ovfval
        
        ! FP-Env flags
        integer(I32P)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FPAbruptDiv0Event_t
    !============================================50
    
    type :: FPAbruptDiv0Event_t
        
        
        private
        
        ! Event name
        character(len=19) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Value of variable after attempted div by zero
        real(R64P)        :: m_div0val
        
        ! FP-Env flags
        integer(I32P)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FPAbruptInvEvent_t
    !============================================50
    
    type :: FPAbruptInvEvent_t
        
       
        
        ! Event name
        character(len=18) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Invalid fp value (if possible to obtain)
        real(R64P)        :: m_inval
        
        ! FP-Env flags
        integer(I32P)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FPAbruptDmzEvent_t
    !============================================50
    
    type :: FPAbruptDmzEvent_t
        
        private
        
        ! Event name
        character(len=18) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Denormal value of variable (encoded as REAL(kind=8)
        real(R64P)        :: m_denval
        
        ! FP-Env flags
        integer(I32P)     :: m_flags
        
        ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FPPoleEvent_t
    !============================================50
    
    type :: FPPoleEvent_t
        
       
        private
        
        ! Event name
        character(len=13) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Pole value
        real(R64P)        :: m_polval
        
        ! FP-Env flags 
        integer(I32P)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FPSingularEvent_t
    !============================================50
    
    type :: FPSingularEvent_t
        
         private
         
        ! Event name
         character(len=17) :: m_event_name
         
        ! Custom message
         character(len=80) :: m_msg
         
        ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Singularity value of variable
        real(R64P)        :: m_singval
        
        ! FP-Env flags
        integer(I32P)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
         
    end type
    
    !============================================50
    ! Type: FPDomErrEvent_t
    !============================================50
    
    type :: FPDomErrEvent_t
        
        private
        
        ! Event name
        character(len=15) :: m_event_name
        
        ! User custom message
        character(len=80) :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Domain error value
        real(R64P)        :: m_errval
        
        ! Domain specific value (upper/lower bound)
        real(R64P)        :: m_domval
        
        ! FP-Env flags
        integer(I32P)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: WinError_t
    !============================================50
    
    type :: WinError_t
        
        private
          ! Event name
        character(len=15) :: m_event_name
        
        ! User custom message
        character(len=80) :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)   :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)    :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)   :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)    :: m_tid
        
        ! Procedure name
        character(len=80) :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(I32P)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40) :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40) :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(I32P)     :: m_severity
        
        ! Windows Error, usually the one returned by the call to GetLastError
        integer(DWORD)    :: m_winerr
        
        ! Is this event built(initilaized)
        logical(I32P)     :: m_isbuilt
        
    end type
    
    !============================================50
    !  Implementation of event creation
    !  subroutines.
    !============================================50
    
   
    
   
    
    contains
    
    !====================================================59
    ! subroutine:
    !             createPerfTimerEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: PerfTimerEvent_t
    !  Notification:
    !                
    !=====================================================59
    subroutine createPerfTimerEvent(event,evname,umsg,fname,             &
                                    modname,procname,locs,loce,          &
                                    nruns,ctimings,sctimings,            &
                                    dctimings,qpctimings,timersd,        &
                                    actimings,asctimings,adctimings,     &
                                    aqpctimings,actiminge,asctiminge,    &
                                    adctiminge,aqpctiminge,actimingsd,   &
                                    asctimingsd,adctimingsd,aqpctimingsd,&
                                    timert,nwcycles,nhotspots,maxhotspot,&
                                    gflops,errstat                               )
          
         
          implicit none
          type(PerfTimerEvent_t),                  intent(inout) :: event
          character(len=*),                        intent(in)    :: evname,umsg,fname, &
                                                                    modname,procname
          integer(I32P),                           intent(in)    :: locs,loce,nruns
          real(R32P),              dimension(:,:), intent(in)    :: ctimings
          integer(I64P),           dimension(:,:), intent(in)    :: sctimings
          real(R64P),              dimension(:,:), intent(in)    :: dctimings
          type(T_LARGE_INTEGER_X), dimension(:,:), intent(in)    :: qpctimings
          real(R64P),              dimension(:,:), intent(in)    :: timersd
          real(R64P),                              intent(in)    :: actimings
          real(R64P),                              intent(in)    :: asctimings
          real(R64P),                              intent(in)    :: adctimings
          real(R64P),                              intent(in)    :: aqpctimings
          real(R64P),                              intent(in)    :: actiminge
          real(R64P),                              intent(in)    :: asctiminge
          real(R64P),                              intent(in)    :: adctiminge
          real(R64P),                              intent(in)    :: aqpctiminge
          real(R64P),                              intent(in)    :: actimingsd
          real(R64P),                              intent(in)    :: asctimingsd
          real(R64P),                              intent(in)    :: adctimingsd
          real(R64P),                              intent(in)    :: aqpctimingsd
          character(len=40), dimension(6),         intent(in)    :: timert
          integer(I32P),                           intent(in)    :: nwcycles,nhotspots
          real(R64P),                              intent(in)    :: maxhotspot,gflops
          logical(I32P),                           intent(in)    :: errstat
          ! Locals
          character(len=40)                                      :: dstr,tstr
          ! Start of executable statements
          ! Sanity checcking on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          if((allocated(ctimings)  .EQ.  .false.) .OR. &
             (allocated(sctimings) .EQ.  .false.) .OR. &
             (allocated(dctimings) .EQ.  .false.) .OR. &
             (allocated(qpctimings).EQ.  .false.) .OR. &
             (allocated(timersd)   .EQ,  .false.)     ) then
               ERROR STOP "createPerfTimeEvent: [FATAL-ERROR]: Unallocated array!!"
          end if
              
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_en     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_nruns       = nruns
             event%m_ctimings    = ctimings
             event%m_sctimings   = sctimings
             event%m_dctimings   = dctimings
             event%m_qpctimings  = qpctimings
             event%m_timers_delta= timersd
             event%m_actimings   = actimings
             event%m_asctimings  = asctimings
             event%m_adctimings  = adctimings
             event%m_aqpctimimgs = aqpctimings
             event%m_actiminge   = actiminge
             event%m_asctiminge  = asctiminge
             event%m_adctiminge  = adctiminge
             event%m_aqpctimimge = aqpctiminge
             event%m_actimingd   = actimingd
             event%m_asctimingd  = asctimingd
             event%m_adctimingd  = adctimingd
             event%m_aqpctimimgd = aqpctimingd
             event%m_timer_type  = timert
             event%m_nwcycles    = nwcycles
             event%m_nhotspots   = nhotspots
             event%m_maxhotspot  = maxhotspot
             event%m_gflops      = gflops
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createPerfTimerEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createInvArgEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: InvArgEvent_t
    !  Notification:
    !                Indicates passing of invalid scalar,
    !                derived type argument or array argument
    !=====================================================59
          
        
    !====================================================59
    ! subroutine:
    !             createInvArgEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: InvArgEvent_t
    !  Notification:
    !                Indicates passing of invalid scalar,
    !                derived type argument or array argument
    !=====================================================59
    subroutine createInvArgEvent(event,evname,umsg,fname, &
                                 modname,procname,locs,loce, &
                                 severity,invargi32,invargi64, &
                                 invargf32,invargf64, arg_type,errstat )
          use, intrinsic :: IEEE_ARITHMETIC
          implicit none
          type(InvArgEvent_t),  intent(inout)        :: event
          character(len=*),     intent(in)           :: evname,umsg,fname, &
                                                        modname,procname
          integer(I32P),        intent(in)           :: locs,loce,severity
          integer(I32P),        intent(in), optional :: invargi32
          integer(I64P),        intent(in), optional :: invargi64
          real(R32P),           intent(in), optional :: invargf32
          real(R64P),           intent(in), optional :: invargf64
          character(len=*),     intent(in), optional :: arg_type
          logical(I32P),        intent(inout)        :: errstat
          ! Locals
          character(len=40)                          :: dstr,tstr
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             if(present(invargi32)) then
                event%m_invargi32 = invargi32
             else
                event%m_invargi32 = HUGE(integer(kind=4))
             end if
             if(present(invargi64)) then
                event%m_invargi64 = invargi64
             else
                event%m_invargi64 = HUGE(integer(kind=8))
             end if
             if(present(invargf32)) then
                event%m_invargf32 = invargf32
             else
                event%m_invargf32 = IEEE_VALUE(invargf32,IEEE_QUIET_NAN)
             end if
             if(present(invargf64)) then
                event%m_invargf64 = invargf64
             else
                event%m_invargf64 = IEEE_VALUE(invargf64,IEEE_QUIET_NAN)
             end if
             if(present(arg_type)) then
                event%m_descript = arg_type
             else
                event%m_descript = "NONE"
             endif
             event%m_isbuilt     = .true.
          else
              errstat            = .true.
              write(ERROR_UNIT,*) "====================================================="
              write(ERROR_UNIT,*) "   createInvArgEvent: [FATAL-ERROR]"
              write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
              write(ERROR_UNIT,*) "====================================================="
              return
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createFailAllocEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FailAllocEvent_t
    !  Notification:
    !                Indicates failed array allocation event
    !=====================================================59
    subroutine createFailAllocEvent(event,evname,umsg,fname, &
                                    modname,procname,locs,loce, &
                                    severity,astat,errmsg,errstat)
          implicit none
          type(FailAllocEvent_t),  intent(inout) :: event
          character(len=*),        intent(in)    :: evname,umsg,fname, &
                                                    modname,procname
          integer(I32P),           intent(in)    :: locs,loce,severity,astat
          character(len=*),        intent(in)    :: errmsg
          logical(I32P),           intent(inout) :: errstat
          ! Locals
          character(len=40)                      :: dstr,tstr
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false.) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_astat       = astat
             event%m_errmsg      = errmsg
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFailAllocEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    
    !====================================================59
    ! subroutine:
    !             createIndexOutBoundsEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: IndexOutBoundsEvent_t
    !  Notification:
    !               Indicates writing/reading/passing 
    !               indices which are out of bounds.
    !=====================================================59
    subroutine createIndexOutBoundsEvent(event,evname,umsg,fname, &
                                         modname,procname,locs,loce, &
                                         severity,indices,errstat    )
          implicit none
          type(IndexOutBoundsEvent_t),    intent(inout) :: event
          character(len=*),               intent(in)    :: evname,umsg,fname, &
                                                          modname,procname
          integer(I32P),                  intent(in)    :: locs,loce,severity
          integer(I32P), dimension(2,31), intent(in)    :: indices
          logical(I32P),                  intent(inout) :: errstat
          ! Locals
          character(len=40)                             :: dstr,tstr
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false.) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_indices32   = indices
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createIndexOutBoundsEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    
    !====================================================59
    ! subroutine:
    !             createFailDeallocEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FailDeallocEvent_t
    !  Notification:
    !                Indicates failed array deallocation event
    !=====================================================59
    subroutine createFailDeallocEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,dstat,errmsg,errstat)
          implicit none
          type(FailDeallocEvent_t),  intent(inout) :: event
          character(len=*),          intent(in)    :: evname,umsg,fname, &
                                                      modname,procname
          integer(I32P),             intent(in)    :: locs,loce,severity,dstat
          character(len=*),          intent(in)    :: errmsg
          logical(I32P),             intent(inout) :: errstat
          ! Locals
          character(len=40)                        :: dstr,tstr
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_dstat       = dstat
             event%m_errmsg      = errmsg
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFailDeallocEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createFileIOEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FileIOEvent_t
    !  Notification:
    !                Indicates failed file I/O operation
    !=====================================================59
    subroutine createFileIOEvent(event,evname,umsg,fname, &
                                 modname,procname,locs,loce, &
                                 severity,fileop,iostat,iomsg,errstat )
          implicit none
          type(FileIOEvent_t),  intent(inout)  :: event
          character(len=*),     intent(in)     :: evname,umsg,fname, &
                                                  modname,procname
          integer(I32P),        intent(in)     :: locs,loce,severity
          character(len=*),     intent(in)     :: fileop
          integer(I32P),        intent(in)     :: iostat
          character(len=*),     intent(in)     :: iomsg
          logical(I32P),        intent(inout)  :: errstat
          ! Locals
          character(len=40)                    :: dstr,tstr
          ! Start of executable statements
          ! Sanity check of errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_fileop      = fileop
             event%m_iostat      = iostat
             event%m_iomsg       = iomsg
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFileIOEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createDisassocPtrEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: DisassocPtrEvent_t
    !  Notification:
    !                none
    !=====================================================59
    subroutine createDisassocPtrEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,status,errstat    )
          implicit none
          type(DisassocPtrEvent_t),  intent(inout) :: event
          character(len=*),          intent(in)    :: evname,umsg,fname, &
                                                      modname,procname
          integer(I32P),             intent(in)    :: locs,loce,severity
          logical(I32P),             intent(in)    :: status
          logical(I32P),             intent(inout) :: errstat
          ! Locals
          character(len=*)                         :: dstr,tstr
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_status      = status
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createDisassocPtrEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createFPTrapUndEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPTrapUndEvent_t
    !  Notification:
    !                Caller of 'createFPTrapUndEvent'
    !                must cause directly or indirectly
    !                to modification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPTrapUndEvent(event,evname,umsg,fname, &
                                    modname,procname,locs,loce, &
                                    severity,undval,errstat    )
          use ifcore
          implicit none
          type(FPTrapUndEvent_t),  intent(inout) :: event
          character(len=*),        intent(in)    :: evname,umsg, &
                                                    fname,modname, &
                                                    procname
          integer(I32P),           intent(in)    :: locs,loce,severity
          real(R64P),              intent(in)    :: undval
          logical(I32P),           intent(inout) :: errstat
          ! Locals
          character(len=40)                      :: dstr,tstr
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false.) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_undval      = undval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPTrapUndEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    
    !====================================================59
    ! subroutine:
    !             createFPTrapOvfEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPTrapOvfEvent_t
    !  Notification:
    !                Caller of 'createFPTrapOvfEvent'
    !                must cause directly or indirectly
    !                to modification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPTrapOvfEvent(event,evname,umsg,fname, &
                                    modname,procname,locs,loce, &
                                    severity,ovfval,errstat     )
          use ifcore
          implicit none
          type(FPTrapOvfEvent_t),  intent(inout) :: event
          character(len=*),        intent(in)    :: evname
          character(len=*),        intent(in)    :: umsg
          character(len=*),        intent(in)    :: fname
          character(len=*),        intent(in)    :: modname
          character(len=*),        intent(in)    :: procname
          integer(I32P),           intent(in)    :: locs,loce,severity
          real(R64P),              intent(in)    :: ovfval
          logical(I32P),           intent(inout) :: errstat
          ! Locals
          character(len=40)                      :: dstr,tstr
          ! Start of executable statements
          ! Sanity check of errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_ovfval      = ovfval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPTrapOvfEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createFPTrapDiv0Event
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPTrapDiv0Event_t
    !  Notification:
    !                Caller of 'createFPTrapDiv0Event'
    !                must cause directly or indirectly
    !                to modification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPTrapDiv0Event(event,evname,umsg,fname, &
                                     modname,procname,locs,loce, &
                                     severity,div0val,errstat           )
          use ifcore
          implicit none
          type(FPTrapDiv0Event_t),  intent(inout)  :: event
          character(len=*),          intent(in)    :: evname
          character(len=*),          intent(in)    :: umsg
          character(len=*),          intent(in)    :: fname
          character(len=*),          intent(in)    :: modname
          character(len=*),          intent(in)    :: procname
          integer(I32P),             intent(in)    :: locs,loce,severity
          real(R64P),                intent(in)    :: div0val
          logical(I32P),             intent(inout) :: errstat
          ! Locals
          character(len=40)                        :: dstr,tstr
          ! Start of executable statements
          ! Sanity check of errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .true.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_div0val     = div0val
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPTrapDiv0Event: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    
    
    !====================================================59
    ! subroutine:
    !             createFPTrapInvEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPTrapInvEvent_t
    !  Notification:
    !                Caller of 'createFPTrapInvEvent'
    !                must cause directly or indirectly
    !                to modification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPTrapInvEvent(event,evname,umsg,fname, &
                                    modname,procname,locs,loce, &
                                    severity,inval,errstat          )
          use ifcore
          implicit none
          type(FPTrapInvEvent_t),  intent(inout)   :: event
          character(len=*),          intent(in)    :: evname
          character(len=*),          intent(in)    :: umsg
          character(len=*),          intent(in)    :: fname
          character(len=*),          intent(in)    :: modname
          character(len=*),          intent(in)    :: procname
          integer(I32P),             intent(in)    :: locs,loce,severity
          real(R64P),                intent(in)    :: inval
          logical(I32P),             intent(inout) :: errstat
          ! locals
          character(len=40)                        :: dstr,tstr
          ! Start of executable statements
          ! Sanity check of errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_inval       = inval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPTrapInvEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createFPAbruptUndEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPAbruptUndEvent_t
    !  Notification:
    !                Caller of 'createFPAbruptUndEvent'
    !                must cause directly or indirectly
    !                to modification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPAbruptUndEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,undval,errstat           )
          use ifcore
          implicit none
          type(FPAbruptUndEvent_t),  intent(inout) :: event
          character(len=*),          intent(in)    :: evname
          character(len=*),          intent(in)    :: umsg
          character(len=*),          intent(in)    :: fname
          character(len=*),          intent(in)    :: procname
          integer(I32P),             intent(in)    :: locs,loce,severity
          real(R64P),                intent(in)    :: undval
          logical(I32P),             intent(inout) :: errstat
          ! Locals
          character(len=40)                        :: dstr,tstr
          ! Start of executable statements
          ! Sanity check of errstat arguments
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GerCurrentThreadId()
             event%m_pro_name    = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_undval      = undval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPAbruptUndEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createFPAbruptOvfEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPAbruptOvfEvent_t
    !  Notification:
    !                Caller of 'createFPAbruptOvfEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPAbruptOvfEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,ovfval,errstat           )
          use ifcore
          implicit none
          type(FPAbruptOvfEvent_t),  intent(inout) :: event
          character(len=*),          intent(in)    :: evname
          character(len=*),          intent(in)    :: umsg
          character(len=*),          intent(in)    :: fname
          character(len=*),          intent(in)    :: modname
          character(len=*),          intent(in)    :: procname
          integer(I32P),             intent(in)    :: locs,loce,severity
          real(R64P),                intent(in)    :: ovfval
          logical(I32P),             intent(inout) :: errstat
          ! locals
          character(len=40)                        :: dstr,tstr
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_ovfval      = ovfval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPAbruptOvfEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createFPAbruptDiv0Event
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPAbruptDiv0Event_t
    !  Notification:
    !                Caller of 'createFPAbruptDiv0Event'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPAbruptDiv0Event(event,evname,umsg,fname, &
                                       modname,procname,locs,loce, &
                                       severity,div0val,errstat          )
          use ifcore
          implicit none
          type(FPAbruptDiv0Event_t),  intent(inout) :: event
          character(len=*),           intent(in)    :: evname
          character(len=*),           intent(in)    :: umsg
          character(len=*),           intent(in)    :: fname
          character(len=*),           intent(in)    :: modname
          character(len=*),           intent(in)    :: procname
          integer(I32P),              intent(in)    :: locs,loce,severity
          real(R64P),                 intent(in)    :: div0val
          logical(I32P),              intent(inout) :: errstat
          ! Locals
          character(len=40)                         :: dstr,lstr
          ! Start of executable statements
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false.) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_div0val     = div0val
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPAbruptDiv0Event: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createFPAbruptInvEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPAbruptInvEvent_t
    !  Notification:
    !                Caller of 'createFPAbruptInvEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPAbruptInvEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,inval,errstat            )
          use ifcore
          implicit none
          type(FPAbruptInvEvent_t),  intent(inout) :: event
          character(len=*),          intent(in)    :: evname
          character(len=*),          intent(in)    :: umsg
          character(len=*),          intent(in)    :: fname
          character(len=*),          intent(in)    :: modname
          character(len=*),          intent(in)    :: procname
          integer(I32P),             intent(in)    :: locs,loce,severity
          real(R64P),                intent(in)    :: inval
          logical(I32P),             intent(inout) :: errstat
          ! Locals
          character(len=40)                        :: dstr,tstr
          ! Start of executable statements
          ! Sanity check of errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_inval       = inval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPAbruptInvEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createFPAbruptDmzEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPAbruptDmzEvent_t
    !  Notification:
    !                Caller of 'createFPAbruptDmzEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPAbruptDmzEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,denval,errstat           )
          use ifcore
          implicit none
          type(FPAbruptDmzEvent_t), intent(inout) :: event
          character(len=*),         intent(in)    :: evname
          character(len=*),         intent(in)    :: umsg
          character(len=*),         intent(in)    :: fname
          character(len=*),         intent(in)    :: modname
          character(len=*),         intent(in)    :: procname
          integer(I32P),            intent(in)    :: locs,loce,severity
          real(R64P),               intent(in)    :: denval
          logical(I32P),            intent(inout) :: errstat
          ! Locals
          character(len=40)                       :: dstr,tstr
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_denval      = denval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPAbruptDmzEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                      
    !====================================================59
    ! subroutine:
    !             createFPPoleEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPPoleEvent_t
    !  Notification:
    !                Caller of 'createFPPoleEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPPoleEvent(event,evname,umsg,fname, &
                                 modname,procname,locs,loce, &
                                 severity,poleval,errstat          )
          use ifcore
          implicit none
          type(FPPoleEvent_t), intent(inout) :: event
          character(len=*),    intent(in)    :: evname
          character(len=*),    intent(in)    :: umsg
          character(len=*),    intent(in)    :: fname
          character(len=*),    intent(in)    :: modname
          character(len=*),    intent(in)    :: procname
          integer(I32P),       intent(in)    :: locs,loce,severity
          real(R64P),          intent(in)    :: poleval
          logical(I32P),       intent(inout) :: errstat
          ! Locals
          character(len=40)                  :: dstr,tstr
          ! Start of executable statements
          ! Sanity check on errstat argument.
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_polval      = polval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPPoleEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return 
          end if
          
    end subroutine
    !====================================================59
    ! subroutine:
    !             createFPSingularEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPSingularEvent_t
    !  Notification:
    !                Caller of 'createFPSingularEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPSingularEvent(event,evname,umsg,fname,   &
                                     modname,procname,locs,loce,&
                                     severity,singularity,errstat       )
          use ifcore
          implicit none
          type(FPSingularEvent_t), intent(inout) :: event
          character(len=*),        intent(in)    :: evname
          character(len=*),        intent(in)    :: umsg
          character(len=*),        intent(in)    :: fname
          character(len=*),        intent(in)    :: modname
          character(len=*),        intent(in)    :: procname
          integer(I32P),           intent(in)    :: locs,loce,severity
          real(R64P),              intent(in)    :: singularity
          logical(I32P),           intent(inout) :: errstat
          ! Locals
          character(len=40)                      :: dstr,tstr
          ! Start of executable statements
          ! Sanity check of errstat argument.
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=dstr,time=tstr)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
             event%m_phandle     = GetCurrentProcess()
             event%m_pid         = GetCurrentProcessId()
             event%m_thandle     = GetCurrentThread()
             event%m_tid         = GetCurrentThreadId()
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = dstr
             event%m_time        = tstr
             event%m_severity    = severity
             event%m_singval     = singularity
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
              errstat = .true.
              write(ERROR_UNIT,*) "====================================================="
              write(ERROR_UNIT,*) "   createFPSingularEvent: [FATAL-ERROR]"
              write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
              write(ERROR_UNIT,*) "====================================================="
              return
          end if
          
    end subroutine
    !============================================50
    ! subroutine:
    !             createFPDomErrEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPDomErrEvent_t
    !  Notification:
    !                Caller of 'createFPSingularEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !============================================50
    subroutine createFPDomErrEvent(event,evname,umsg,fname,   &
                                   modname,procname,locs,loce,&
                                   severity,errval,domval,errstat     )
          use ifcore
          implicit none
          type(FPDomErrEvent_t), intent(inout) :: event
          character(len=*),      intent(in)    :: evname
          character(len=*),      intent(in)    :: umsg
          character(len=*),      intent(in)    :: fname
          character(len=*),      intent(in)    :: modname
          character(len=*),      intent(in)    :: procname
          integer(I32P),         intent(in)    :: locs,loce,severity
          real(R64P),            intent(in)    :: errval,domval
          logical(I32P),         intent(inout) :: errstat
          ! Locals
          character(len=40)                    :: dstr,tstr
          ! Start of executable statements
          ! Sanity check of errstat
          if(errstat .EQ. .true.) then 
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false.) then
              call DATE_AND_TIME(date=dstr,time=tstr)
              event%m_event_name  = evname
              event%m_msg         = umsg
              event%m_file_name   = fname
              event%m_module_name = modname
              event%m_phandle     = GetCurrentProcess()
              event%m_pid         = GetCurrentProcessId()
              event%m_thandle     = GetCurrentThread()
              event%m_tid         = GetCurrentThreadId()
              event%m_proc_name   = procname
              event%m_line_st     = locs
              event%m_line_ed     = loce
              event%m_date        = dstr
              event%m_time        = tstr
              event%m_severity    = severity
              event%m_errval      = errval
              event%m_domval      = domval
              event%m_flags       = FOR_GET_FPE()
              event%m_isbuilt     = .true.
          else
              errstat = .true.
              write(ERROR_UNIT,*) "====================================================="
              write(ERROR_UNIT,*) "   createFPDomErrorEvent: [FATAL-ERROR]"
              write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
              write(ERROR_UNIT,*) "====================================================="
              return
          end if
          
    end subroutine
    !============================================50
    ! subroutine:
    !             createWinErrorEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: WinError
    !============================================50
    
    subroutine createWinErrorEvent(event,evname,umsg,fname,    &
                                   modname,procname,locs,loce, &
                                   severity,errstat             )
          use ISO_FORTRAN_ENV,only : ERROR_UNIT
          implicit none
          type(WinError_t), intent(inout) :: event
          character(len=*), intent(in)    :: evname
          character(len=*), intent(in)    :: umsg
          character(len=*), intent(in)    :: fname
          character(len=*), intent(in)    :: modname
          character(len=*), intent(in)    :: procname
          integer(I32P),    intent(in)    :: locs,loce,severity
          logical(I32P),    intent(inout) :: errstat
          ! Locals
          character(len=40)               :: dstr,tstr
          ! Start of executable statements
          
          ! Sanity check of error status
          if(errstat .EQ. .true.) then 
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false.) then
              call DATE_AND_TIME(date=dstr,time=tstr)
              event%m_event_name  = evname
              event%m_msg         = umsg
              event%m_file_name   = fname
              event%m_module_name = modname
              event%m_phandle     = GetCurrentProcess()
              event%m_pid         = GetCurrentProcessId()
              event%m_thandle     = GetCurrentThread()
              event%m_tid         = GetCurrentThreadId()
              event%m_proc_name   = procname
              event%m_line_st     = locs
              event%m_line_ed     = loce
              event%m_date        = dstr
              event%m_time        = tstr
              event%m_severity    = severity
              event%m_winerr      = GetLastError()
              event%m_isbuilt     = .true.
          else
              errstat = .true.
              write(ERROR_UNIT,*) "====================================================="
              write(ERROR_UNIT,*) "   createWinErrorEvent: [FATAL-ERROR]"
              write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
              write(ERROR_UNIT,*) "====================================================="
              return
          end if
          
    end subroutine
    
end module