
module mod_wsr88d_stormpos

  !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_stormpos'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  Storm Position product.
 !          History:
 !                        Date: 22-02-2018
 !                        Time: 17:59 GMT+2
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
    integer(I32P), parameter, public :: MOD_WSR88D_STORMPOS_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_STORMPOS_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_STORMPOS_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_STORMPOS_FULLVER = 1000_I32P*MOD_WSR88D_STORMPOS_MAJOR + &
                                                                      100_I32P*MOD_WSR88D_STORMPOS_MINOR  + &
                                                                      10_I32P*MOD_WSR88D_STORMPOS_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_WSR88D_STORMPOS_CREATE_DATE = "22-02-2018 17:59 +00200 (THR 22 FEB 2018 GMT+2) "
    
    ! Module build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_WSR88D_STORMPOS_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_WSR88D_STORMPOS_AUTHOR_INFO = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_WSR88D_STORMPOS_DESCRIPT = "WSR-88D Storm Postion product"
    
    ! Module constants
    
    !==================================
    ! Type: StormPos_t
    !==================================
    type, public :: StormPos_t
        
          private
          
          ! azimuth size, range size
          integer(I32P) :: m_nsize
          
          ! Access public in order to eliminate unnecessary copy operations.
          real(R64P), allocatable, dimension(:), public :: m_Azimuth    ! Azimuth, deg, 0.0-359.0, 1.0
          
          real(R64P), allocatable, dimension(:), public :: m_Range      ! Range, nmi, 0.0-248.0, 1.0
          
          
          
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
          
           
          !============================================
          !  Class helper procedures
          !============================================
          
          procedure, pass(this), public :: dbg_info
          
         !=======================================
         !   Generic operators
         !=======================================
         procedure, public :: copy_assign
         
         generic :: assignment (=) => copy_assign
         
    end type StormPos_t
          
    interface StormPos_t
         procedure constructor
    end interface StormPos_t
          
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
    type(StormPos_t) function constructor(nsize,logging,verbose,dbg,append,fname)
        use mod_print_error, only : handle_fatal_memory_error
        use mod_constants,   only : LAM_PINF                           
        integer(I32P),      intent(in)    :: nsize
        logical(I32P),      intent(in)    :: logging, verbose, dbg, append
        character(len=*),   intent(in)    :: fname
        ! Locals
        character(len=256) :: emsg
        integer(I32P) ::  aerr
        ! Strat of eexecutable statements
        constructor%m_nsize = nsize
        associate(n=>constructor%m_nsize)
                  
            allocate(constructor%m_Azimuth(n),  &
                     constructor%m_Range(n),    &
                     STAT=aerr,           &
                     ERRMSG=emsg  )
        end associate
        if(aerr /= 0) then
           
              call handle_fatal_memory_error( logging,verbose,append,fname,    &
                                             "logger:162 --> mod_wsr88d_stormpos/constructor: Memory Allocation Failure!!", &
                                             "mod_wsr88d_stormpos/constructor:162 --> Memory Allocation Failure!!" , &
                                             emsg,__LINE__ )
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
          implicit none
          class(StormPos_t), intent(in) :: this
          ! Locals
          integer(I32P) :: nsize
          ! Start of executable statements
          nsize = this%m_nsize
    end function get_nsize 
    


    
    !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(StormPos_t), intent(in) :: this
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
          class(StormPos_t), intent(in) :: this
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
          class(StormPos_t),  intent(in) :: this
          ! Start of executable statements
           print*, "============================================"
           print*, "     *** Dump 'StormPos_t' state!! ***      " 
           print*, "============================================"
           print*, "Collected at: ", __DATE__,":",__TIME__, &
                  "in file: ", __FILE__, "at line: ",__LINE__
           print*, " Length of arrays:       ", this%m_nsize
           print*, " Azimuth initialization: ", this%m_Azimuth
           print*, " Range   initialization: ", this%m_Range
           print*, "============================================="
   end subroutine dbg_info      
          
    !====================================
    !  operator assignment (=)
    !====================================
    subroutine copy_assign(lhs,rhs)
          class(StormPos_t),  intent(inout) :: lhs
          class(StormPos_t),  intent(in)    :: rhs
          ! Start of executable ststements
          lhs%m_nsize = rhs%m_nsize
          lhs%m_Azimuth = rhs%m_Azimuth
          lhs%m_Range   = rhs%m_Range
    end subroutine copy_assign
    
    
end module mod_wsr88d_stormpos