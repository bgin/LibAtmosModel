
module mod_wsr88d_mvelpos

  !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_mvelpos'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Maximum Velocity and Position product.
 !          History:
 !                        Date: 18-02-2018
 !                        Time: 09:39 GMT+2
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
     
     
     use module_kinds,    only : I32P, R64P
     
    implicit none
    private
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_WSR88D_MVELPOS_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_MVELPOS_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_MVELPOS_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_MVELPOS_FULLVER = 1000_I32P*MOD_WSR88D_MVELPOS_MAJOR + &
                                                                     100_I32P*MOD_WSR88D_MVELPOS_MINOR  + &
                                                                     10_I32P*MOD_WSR88D_MVELPOS_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_MVELPOS_CREATE_DATE = "18-02-2018 09:39 +00200 (SAT 18 FEB 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_MVELPOS_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_MVELPOS_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSR88D_MVELPOS_DESCRIPT = "Maximum Velocity and Position product."
    
    ! Module constants
    
    !============================
    ! Type: MaxVelPos_t
    !============================
    type, public :: MaxVelPos_t
        
          private
          
          integer(I32P) :: m_nsize ! Azimuth array size, and Range array size
          
          ! Public access in order to eliminate  unnecesary copy operations
          
          real(R64P), allocatable, dimension(:), public :: m_Azimuth ! Azimuth, deg, 0.0-359.0, 1.0
          
          real(R64P), allocatable, dimension(:), public :: m_Range   ! Range, nmi, 0.0-124.0, 1.0
          
          
          
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
          
          procedure, pass(this), private :: dbg_info
          
          !===================================
          !  Type-bound generic operators
          !===================================
          procedure, public :: copy_assign
          
          generic :: assignment(=) => copy_assign
          
          end type MaxVelPos_t
          
    interface MaxVelPos_t
        procedure constructor
    end interface MaxVelPos_t
          
    contains
    
     !========================================!
     !    Implementation                      !
     !========================================!
    
    !=================================================!
    !  @function: constructor                                          
    !  Initialization of object state.                                          
    !  Allocation and initialization to default values
    !  of real arrays
    !  
    !=================================================!
    type(MaxVelPos_t) function constructor(nsize,logging,verbose,append,fname)
          use mod_print_error, only : print_fatal_error
          use mod_constants,   only : LAM_PINF                            
          integer(I32P),      intent(in)    :: nsize
          integer(I32P),      intent(inout) :: ierr
          logical(I32P),      intent(in)    :: logging, verbose, append
          character(len=*),   intent(in)    :: fname
          ! Locals
          character(len=40)  :: sdate, stime
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          ! Start of executable statements
          constructor%m_nsize = nsize
          associate(n=>constructor%m_nsize)
                   
                    allocate(constructor%m_Azimuth(n),   &
                             constructor%m_Range(n),     &
                             STAT=aerr,            &
                             ERRMSG=emsg)
          end associate
          if(aerr /= 0)  then
             
              call handle_fatal_memory_error(logging,verbose,append,fname,  &
                                             "logger:180 --> mod_wsr88d_mvelpos/constructor: Memory Allocation Failure", &
                                             "mod_wsr88d_mvelpos/constructor: Memory Allocation Failure ",              &
                                             emsg,__LINE__)
          end if
          constructor%m_Azimuth = LAM_PINF
          constructor%m_Range   = LAM_PINF
    end function constructor      
         
         
    
    
   
  
    
   
           
    
    !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nsize
!DIR$   ENDIF 
    pure function get_nsize(this) result(nsize)
          
          class(MaxVelPos_t), intent(in) :: this
          ! Locals
          integer(I32P) :: na
          ! Start of executable statements
          nsize = this%m_nsize
    end function get_na 
    

   

    

    
    !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(MaxVelPos_t), intent(in) :: this
          character(len=*),   intent(in) :: form
          integer(I32P),      intent(in) :: unit
          integer(I32P),      intent(inout) :: ioerr
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
          class(MaxVelPos_t), intent(in) :: this
          character(len=*),   intent(in) :: form
          integer(I32P),      intent(in) :: unit
          integer(I32P),      intent(inout) :: ioerr
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
          class(MaxVelPos_t), intent(in) :: this
          ! Start of executable statements
           print*, " Dump 'MaxVelPos_t' state!!"
           print*, "============================================"
           print*, " Length of measurements: ", this%m_nsize
           print*, " Azimuth initialization: ", this%m_Azimuth
           print*, " Range   initialization: ", this%m_Range
           print*, "============================================="
          
  end subroutine dbg_info        
    
  
    
    !====================================
    !  operator assignment (=)
    !====================================
    subroutine copy_assign(lhs,rhs)
          class(MaxVelPos_t), intent(inout) :: lhs
          class(MaxVelPos_t), intent(in)    :: rhs
          ! Start of executable statements
          lhs%m_nsize = rhs%m_nsize
          lhs%m_Azimuth = rhs%m_Azimuth
          lhs%m_Range   = rhs%m_Range
    end subroutine copy_assign
    
end module mod_wsr88d_mvelpos