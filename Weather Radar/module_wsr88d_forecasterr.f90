
module mod_wsr88d_forecasterr

  !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_forecasterr'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Forecast Error product.
 !          History:
 !                        Date: 25-02-2018
 !                        Time: 12:51 GMT+2
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
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTERR_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTERR_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTERR_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_FORECASTERR_FULLVER = 1000_I32P*MOD_WSR88D_FORECASTERR_MAJOR + &
                                                                         100_I32P*MOD_WSR88D_FORECASTERR_MINOR  + &
                                                                         10_I32P*MOD_WSR88D_FORECASTERR_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_FORECASTERR_CREATE_DATE = "25-02-2018 12:59 +00200 (SUN 25 FEB 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_FORECASTERR_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_FORECASTERR_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSR88D_FORECASTERR_DESCRIPT = "WSR-88D Forecast Error product"
    
    !=============================
    ! Type: ForecastErr_t
    !=============================
    type, public :: ForecastErr_t
        
          private
          
          integer(I32P) :: m_nsize ! size of array m_Err, size of array m_Mean
          
          ! Public access in order to eliminate  unnecesary copy operations
          
          real(R64P), allocatable, dimension(:), public :: m_Err  ! Error, nmi, 0.0-99.0, 0.1
          
          real(R64P), allocatable, dimension(:), public :: m_Mean ! Mean, nmi, 0.0-99.0, 0.1
          
          
          
          contains
    
         
          !========================================
          !    Getter procedures
          !========================================
          
          procedure, pass(this), public :: get_nsize
          
          
          
          !==========================================
          !   Read/write procedures
          !==========================================
          
          procedure, nopass, public :: read_state
          
          procedure, nopass, public :: write_state
          
          !============================================
          !  Class helper procedures
          !============================================
          
          procedure, pass(this), public :: dbg_info
          
         !=======================================
         !   Generic operators
         !=======================================
         procedure, public :: copy_assign
         
         generic :: assignment(=) => copy_assign
          
    end type ForecastErr_t
          
    interface ForecastErr_t
         procedure constructor
    end interface ForecastErr_t
          
    contains
    
     !========================================!
     !    Implementation                      !
     !========================================!
    
    !=================================================!
    !  @function: constructor                                          
    !  Initialization of object state.                                          
    !  Allocation and initialization to default values
    !  of real arrays
    !=================================================!
    type(ForecastErr_t) function constructor(nsize,logging,verbose,dbg,append,fname)
          use mod_print_error, only : handle_fatal_memory_error
          use mod_constants,   only : LAM_PINF                          
          integer(I32P),         intent(in)    :: nsize
          logical(I32P),         intent(in)    :: logging,verbose,dbg,append
          character(len=*),      intent(in)    :: fname
          ! Locals
          character(len=256) :: emsg
          integer(I32P)      ::  aerr
          ! Start of executable statements
          constructor%m_nsize = nsize
          associate(n=>constructor%m_nerr)
                   
               allocate(constructor%m_Err(n),  &
                        constructor%m_Mean(n), &
                        STAT=aerr,       &
                        ERRMSG=emsg )
          end associate
          if(aerr /= 0) then
             
              call handle_fatal_memory_error(logging,verbose,append,fname,   &
                                             "logger:161 --> mod_wsr88d_forecasterr/constructor: Memory Allocation Failure !!" , &
                                             "mod_wsr88d_forecasterr/constructor:161 Memory Allocation Failure !! " , &
                                             emsg,__LINE__ )
          end if
          constructor%m_Err  = LAM_PINF
          constructor%m_Mean = LAM_PINF
        
   end function constructor      
         
         
         
   
    
   
     
  
    !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nsize
!DIR$   ENDIF 
    pure function get_nsize(this) result(nsize)
          implicit none
          class(ForecastErr_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: nsize
          ! Start of executable statements
          nsize = this%m_nsize
    end function  get_nsize
    


    
    !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(ForecastErr_t), intent(in) :: this
          character(len=*),     intent(in) :: form
          integer(I32P),        intent(in) :: unit
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
          class(ForecastErr_t), intent(in) :: this
          character(len=*),     intent(in) :: form
          integer(I32P),        intent(in) :: unit
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
          class(ForecastErr_t),  intent(in) :: this
          ! Start of executable statements
          print*, "          ***Dump of 'ForecastErr_t' state*** !!"
          print*, "=========================================================="
          print*, "Collected on: ", __DATE__,":", __TIME__ ,  &
                  "in file: ", __FILE__, "at line: ", __LINE__
          print*, "=========================================================="
          print*, "Length  arrays:                  ", this%m_nsize
          print*, "Array m_Err initialized to INF:  ", this%m_Err
          print*, "Array m_Mean initialized to INF: ", this%m_Mean
          print*, "=========================================================="
    end subroutine dbg_info      
   
    !====================================
    !  operator assignment (=)
    !====================================
    subroutine copy_assig(lhs,rhs)
          class(ForecastErr_t),  intent(inout) :: lhs
          class(ForecastErr_t),  intent(in)    :: rhs
          ! Start of executable ststements
          lhs%m_nsize = rhs%m_nsize
          lhs%m_Err   = rhs%m_Err
          lhs%m_Mean  = rhs%m_Mean
    end subroutine copy_assign
    
    

end module mod_wsr88d_forecasterr