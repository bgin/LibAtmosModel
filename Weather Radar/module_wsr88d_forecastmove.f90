
module mod_wsr88d_forecastmove


  !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_forecastmove'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Forecast Movement product.
 !          History:
 !                        Date: 24-02-2018
 !                        Time: 10:08 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                  Bernard Gingold
 !                 
 !          References:
 !         
 !                 Document: 2620001W    
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use module_kinds,  only : I32P, R64P
   
    implicit none
    private
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTMOVE_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTMOVE_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTMOVE_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTMOVE_FULLVER = 1000_I32*MOD_WSR88D_FORECASTMOVE_MAJOR + &
                                                                          100_I32P*MOD_WSR88D_FORECASTMOVE_MINOR + &
                                                                          10_I32P*MOD_WSR88D_FORECASTMOVE_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_FORECASTMOVE_CREATE_DATE = "24-02-2018 10:19 +00200 (SAT 24 FEB 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_FORECASTMOVE_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_FORECASTMOVE_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSR88D_FORECASTMOVE_DESCRIPT = "WSR-88D Forecast Movement product."
    
    !=====================================
    ! Type: ForecastMov_t
    !=====================================
    type, public :: ForecastMov_t
        
          private
          
          integer(I32P) :: m_nsize ! nss - storm speed array length
                                   ! nsd - storm directory array length
          
          real(R64P), allocatable, dimension(:), public :: m_StormSpeed  ! Storm Speed, Kts, 0.0-999.0, 1.0
          
          real(R64P), allocatable, dimension(:), public :: m_StormDir    ! Storm Direction, deg, 0.0-359.0, 1.0
          
          
          
          contains
    
         
          
          !========================================
          !    Getter procedures
          !========================================
          
          procedure, pass(this), public :: get_nsize
          
          
          
          !========================================
          !    Read/write procedures
          !========================================
          
          procedure, nopass, public :: read_state
          
          procedure, nopass, public :: write_state
          
          !=======================================
          !  Class helper procedures
          !=======================================
          
          procedure, pass(this), private :: dbg_info
          
         !=======================================
         !   Generic operators
         !=======================================
         procedure, public :: copy_assign
         
         generic :: assignment(=) => copy_assign
        
    end type ForecastMov_t
          
    interface ForecastMov_t
        procedure constructor
    end interface ForecastMov_t
          
    contains
    
     !========================================!
     !    Implementation                      !
     !========================================!
    
    !=================================================!
    !  @subroutine: init                                          
    !  Initialization of object state.                                          
    !  Allocation and initialization to default values
    !  of real arrays
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already  or invalid argument
    !   2) -2 -- Invalid argument (any of them)  ! Not used here
    !=================================================!
    type(ForecastMov_t) function constructor(nsize,logging,verbose,dbg,append,fname)
          use mod_print_error, only : handle_fatal_memory_error
          use mod_constants,   only : LAM_PINF                        
          integer(I32P),          intent(in)    :: nsize
          logical(I32P),          intent(in)    :: logging, verbose, dbg, append
          character(len=*),       intent(in)    :: fname
          ! Locals
          character(len=256) :: emsg
          integer(I32P)      ::  aerr
          ! Start of executable statemetns
          constructor%m_nsize = nsize
          associate(n=>constructor%m_nsize)
                   
               allocate(constructor%m_StormSpeed(n),  &
                        constructor%m_StormDir(n),    &
                        STAT=aerr,                    &
                        ERRMSG=emsg  )
          end associate
          if(aerr /= 0)  then
            
              call handle_fatal_memory_error(logging,verbose,append,fname,   &
                                             "logger:162 --> mod_wsr88d_forecastmove/constructor: Memory Allocation Failure!!" , &
                                             "mod_wsr88d_forecastmove/constructor:162 -- Memory Allocation Failure!!" , &
                                             emsg,__LINE__ )
          end if
          constructor%m_StormSpeed = LAM_PINF
          constructor%m_StormDir   = LAM_PINF
   end function constructor      
       

   
    
  
    
    !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nsize
!DIR$ ENDIF
    pure function get_nsize(this) result(nsize)
          implicit none
          class(ForecastMov_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: nsize
          ! Start of executable statemetns
          nsize = this%m_nsize
    end function get_nsize
    

    
    !============================================
    !   Read/write procedures
    !============================================
    
     subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(ForecastMov_t), intent(in)    :: this
          character(len=*),     intent(in)    :: form
          integer(I32P),        intent(in)    :: unit
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
          implicit none
          class(ForecastMov_t), intent(in)    :: this
          character(len=*),     intent(in)    :: form
          integer(I32P),        intent(in)    :: unit
          integer(I32P),        intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
    subroutine dbg_info(this)
          implicit none
          class(ForecastMov_t), intent(in) :: this
          ! Start of executable statemetns
          print*, " Dump 'ForecastMov_t' state!! "
          print*, "==================================================="
          print*, "Collected at: ", __DATE__ , ":" , __TIME__
          print*, "==================================================="
          print*, "Length of  arrays         : ", this%m_nsize
          print*, "StormSpeed initialized to:  ", this%m_StormSpeed
          print*, "StormDir   initialized to:  ", this%m_StormDir
          print*, "===================================================="
    end subroutine dbg_info
    
    !====================================
    !  operator assignment (=)
    !====================================     
    subroutine copy_assign
          class(ForecastMov_t),   intent(inout) :: lhs
          class(ForecastMov_t),   intent(in)    :: rhs
          ! Start of executable ststements
          lhs%m_nsize      = rhs%m_nsize
          lhs%m_StormSpeed = rhs%m_StormSpeed
          lhs%m_StormDir   = rhs%m_StormDir
    end subroutine copy_assign

end module mod_wsr88d_forecastmove