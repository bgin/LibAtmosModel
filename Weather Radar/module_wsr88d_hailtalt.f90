
module mod_wsr88d_hailtalt


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_hailtalt'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Hail Temperature and Altitude product.
 !          History:
 !                        Date: 24-02-2018
 !                        Time: 14:58 GMT+2
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
    ! Tab:10,11 col - Type , function and subroutine code blocks.     dd

    use module_kinds,  only : I32P, R64P
    
    implicit none
    private
    
     
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_WSR88D_HAILTALT_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_HAILTALT_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_HAILTALT_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_HAILTALT_FULLVER = 1000_I32P*MOD_WSR88D_HAILTALT_MAJOR + &
                                                                      100_I32P*MOD_WSR88D_HAILTALT_MINOR  + &
                                                                      10_I32P*MOD_WSR88D_HAILTALT_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_HAILTALT_CREATE_DATE = "24-02-2018 15:08 +00200 (SAT 24 FEB 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_HAILTALT_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_HAILTALT_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSR88D_HAILTALT_DESCRIPT = "WSR-88D Hail Temperature at Altitude"
    
    !===============================
    ! Type: HailTAlt_t
    !===============================
    type, public :: HailTAlt_t
        
          private
          
          integer(I32P) :: m_nsize ! ht0c size, ht20c size
          
            ! Access public in order to eliminate unnecessary copy operations.
          real(R64P), allocatable, dimension(:), public :: m_HT0C  !  Kft, 0.0-70.0 0.1   (0C)
          
          real(R64P), allocatable, dimension(:), public :: m_HT20C !  Kft, 0.0-70.0, 0.1  (-20.0C)
          
         
          
          contains
          
        
          !========================================
          !    Getter procedures
          !========================================
          
          procedure, pass(this), public :: get_nsize
          
        
          !==========================================
          !   Read/write procedurs
          !==========================================
          
          procedure, nopass, public :: read_state
          
          procedure, nopass, public :: write_state
          
          !==========================================
          !   Class helper procedures
          !==========================================
        
          procedure, pass(this), public :: dbg_info
          
         !=======================================
         !   Generic operators
         !=======================================
         procedure, public :: copy_assign
         
         generic :: assignment (=) => copy_assign
         
    end type HailTAlt_t
          
    interface HailTAlt_t
        procedure constructor
    end interface HailTAlt_t
          
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
    type(HailTAlt_t) function constructor(nsize,logging,verbose,dbg,append,fname)
          use mod_print_error,  only : handle_fatal_memory_error
          use mod_constants,    only : LAM_PINF                            
          integer(I32P),        intent(in)    :: nsize
          integer(I32P),        intent(inout) :: ierr
          logical(I32P),        intent(in)    :: logging, verbose, dbg, append
          character(len=*),     intent(in)    :: fname
          ! Locals
          character(len=256) ::  emsg
          integer(I32P)      ::  aerr
          ! Start of executable statements
          constructor%m_nsize = nsize
           associate(n=>constructor%m_nsize)
                     
                allocate(constructor%m_HT0C(n),   &
                         constructor%m_HT20C(n), &
                         STAT=aerr,          &
                         ERRMSG=emsg )
           end associate
           if(aerr /= 0) then
             
              call handle_fatal_memory_error(logging,verbose,append,fname,    &
                                             "logger:162 --> mod_wsr88d_hailtalt/constructor: -- Memory Allocation Failure !!",  &
                                             "mod_wsr88d_hailtalt/constructor:162 -- Memory Allocation Failure !!", &
                                             emsg,__LINE__ )
           end if
           constructor%m_HT0C  = LAM_PINF
           constructor%m_HT20C = LAM_PINF
    end function constructor      
          
    
    
  
  
      
    
    !==========================================
    !       Getter procedures
    !==========================================

!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nsize
!DIR$   ENDIF 
    pure function get_nsize(this) result(nsize)
          implicit none
          class(HailTAlt_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: nsize
          ! Start of executable statetmetns
          nsize = this%m_nsize
    end function get_nsize
    

    

    
    !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(HailTAlt_t), intent(in) :: this
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
          class(HailTAlt_t), intent(in) :: this
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
          class(HailTAlt_t),  intent(in) :: this
          ! Start of executable statemetns
          print*, "======================================================="
          print*, "           Dump of 'HailTAlt' state !!                 "
          print*, "======================================================="
          print*, "Collected at: ", __DATE__, ":", __TIME__ ,      &
                  "in file: ", __FILE__, "at line: ", __LINE__
          print*, "========================================================"
          print*, "1) Length of  arrays:              ", this%m_nsize
          print*, "2) Array HT0C initialized to INF:  ", this%m_HT0C
          print*, "3) Array HT20C initialized to INF: ", this%m_HT20C
          print*, "========================================================"
    end subroutine dbg_info     
         
    subroutine copy_assign(lhs,rhs)
          class(HailTAlt_t),  intent(inout) :: lhs
          class(HailTAlt_t),  intent(in)    :: rhs
          ! Start of executable ststements
          lhs%m_nsize = rhs%m_nsize
          lhs%m_HT0C  = rhs%m_HT0C
          lhs%m_HT20C = rhs%m_HT20C
    end subroutine copy_assign
    
end module mod_wsr88d_hailtalt