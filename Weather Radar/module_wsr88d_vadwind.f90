
module mod_wsr88d_vadwind

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wsr88d_vadwind'
 !          
 !          Purpose:
 !                    This module contains description of
 !                    WSR-88D  VAD Wind Profile product.
 !          History:
 !                        Date: 28-02-2018
 !                        Time: 17:56 GMT+2
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
    integer(I32P), parameter, public :: MOD_WSR88D_VADWIND_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WSR88D_VADWIND_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WSR88D_VADWIND_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_WSR88D_VADWIND_FULLVER = 1000_I32P*MOD_WSR88D_VADWIND_MAJOR + &
                                                                     100_I32P*MOD_WSR88D_VADWIND_MINOR  + &
                                                                     10_I32P*MOD_WSR88D_VADWIND_MICRO
    
    ! Module creation date
    character(*), parameter, public :: MOD_WSR88D_VADWIND_CREATE_DATE = "28-02-2018 18:03 +00200 (WED 28 FEB 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation of this module)
    character(*), parameter, public :: MOD_WSR88D_VADWIND_BUILD_DATE = " "
    
    ! Module author info
    character(*), parameter, public :: MOD_WSR88D_VADWIND_AUTHOR = "Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*), parameter, public :: MOD_WSR88D_VADWIND_DESCRIPT = "WSR-88D VAD Wind profile product."
    
    !===================================
    ! Type: VADWind_t
    !===================================
    type, public :: VADWind_t
        
          private
          
          ! Indexing variables of wind fields conform to WRF memory indexing
          ! i.e. ims:ime,kms:kme,jms:jme
          integer(I32P) :: ims,ime,kms,kme,jms,jme
          
          integer(I32P) :: m_nsize
          
          ! Public array member fields in order to eliminate unnecessary copy operations
          
          real(R64P), allocatable, dimension(:), public     :: m_Alt !  ! Altitude,  100ft,  0.0-700.0,        1.0
          
          real(R64P), allocatable, dimension(:,:,:), public :: m_U !   U,        m/s,   -127.0-126.0,  0.1
          
          real(R64P), allocatable, dimension(:,:,:), public :: m_V !   V,        m/s,   -127.0-126.0,  0.1
          
          real(R64P), allocatable, dimension(:,:,:), public :: m_W !   W,     cm/s   -999.9-9999.9, 0.1   
          
          real(R64P), allocatable, dimension(:), public     :: m_Dir !  DIR       deg,     0-360,       1
          
          real(R64P), allocatable, dimension(:), public     :: m_SPD !  SPD       knots,   0-999,       1
          
          real(R64P), allocatable, dimension(:), public     :: m_RMS !  RMS       knots,   0-30.0,      0.1
          
          real(R64P), allocatable, dimension(:), public     :: m_SRNG ! SRNG    nm,      0-124.0,     0.01  
          
          real(R64P), allocatable, dimension(:), public     :: m_ELEV ! deg,     -1.0-45.0,   0.1
          
          
          
          contains
    
         
          
          !========================================
          !    Getter procedures  for scalar 
          !    members only.
          !========================================
          
          procedure, pass(this), public :: get_ims
          
          procedure, pass(this), public :: get_ime
          
          procedure, pass(this), public :: get_kms
          
          procedure, pass(this), public :: get_kme
          
          procedure, pass(this), public :: get_jms
          
          procedure, pass(this), public :: get_jme
          
          procedure, pass(this), public :: get_nsize
          
         
          
         
          
          !==========================================
          !   Read/write procedures
          !==========================================
          
          procedure, nopass, public :: read_state
          
          procedure, nopass, public ::  write_state
          
          !============================================
          !  Class helper procedures
          !============================================
          
          procedure, pass(this), public :: dbg_info
          
          !============================================
          !  Generic operators
          !============================================
          procedure, public :: copy_assign
          
          generic :: assignment (=) => copy_assign
          
          end type VADWind_t
          
    interface VADWind_t
         procedure :: constructor
    end interface VADWind_t
          
    contains
    
     !========================================!
     !    Implementation                      !
     !========================================!
    
    !=================================================!
    !  @subroutine: init                                          
    !  Initialization of object state.                                          
    !  Allocation and initialization to default values
    !  of real arrays
    !=================================================!
    type(VADWind_t) function constructor(ims,ime,kms,kme,jms,jme,nsize, &
                    logging, verbose,dbg,append,fname     )
                              
          use mod_print_error, only : handle_fatal_memory_error
          use mod_constants,   only : LAM_PINF                           
          integer(I32P),      intent(in)    :: ims,ime,kms,  &
                                               kme,jms,jme
          integer(I32P),      intent(in)    :: nsize
          logical(I32P),      intent(in)    :: logging,verbose,dbg,append
          character(len=*),   intent(in)    :: fname
          ! Locals
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          ! Start of executable statements
          ! Begin construction
          constructor%m_ims = ims
          constructor%m_ime = ime
          constructor%m_kms = kms
          constructor%m_kme = kme
          constructor%m_jms = jms
          constructor%m_jme = jme
          constructor%m_nsize = nsize
        associate(d1s=>constructor%m_ims,  &
                             d1e=>constructor%m_ime,  &
                             d2s=>constructor%m_kms,  &
                             d2e=>constructor%m_kme,  &
                             d3s=>constructor%m_jms,  &
                             d3e=>constructor%m_jme,  &
                             n=>constructor%m_nsize  )
              
               allocate(constructor%m_Alt(n),                     &
                        constructor%m_U(d1s:d1e,d2s:d2e,d3s:d3e), &
                        constructor%m_V(d1s:d1e,d2s:d2e,d3s:d3e), &
                        constructor%m_W(d1s:d1e,d2s:d2e,d3s:d3e), &
                        constructor%m_Dir(n),                     &
                        constructor%m_SPD(n),                     &
                        constructor%m_RMS(n),                     &
                        constructor%m_SRNG(n),                    &
                        constructor%m_ELEV(n),                    &
                        STAT=aerr,                                &
                        ERRMSG=emsg    )
        end associate 
        if(aerr /= 0) then
           
              call handle_fatal_memory_error(logging,verbose,append,fname,   &
                                             "logger:220 --> mod_wsr88d_vadwind/constructor: Memory Allocation Failure !!" ,   &
                                             "mod_wsr88d_vadwind/constructor:220 -- Memory Allocation Failure !!" ,           &
                                             emsg,__LINE__  )
                                             
        end if
        constructor%m_Alt  = LAM_PINF
        constructor%m_U    = LAM_PINF
        constructor%m_V    = LAM_PINF
        constructor%m_W    = LAM_PINF
        constructor%m_Dir  = LAM_PINF
        constructor%m_SPD  = LAM_PINF
        constructor%m_RMS  = LAM_PINF
        constructor%m_SRNG = LAM_PINF
        constructor%m_ELEV = LAM_PINF
    end function constructor
                    
   
    
    !==========================================
    !       Getter procedures
    !==========================================
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_ims
!DIR$   ENDIF 
    pure function get_ims(this) result(ims)
          
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: ims
          ! Start of executable statements
          ims = this%m_ims
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_ime
!DIR$   ENDIF 
    pure function get_ime(this) result(ime)
         
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: ime
          ! Start of executable statements
          ime = this%m_ime
    end function  
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_kms
!DIR$   ENDIF 
    pure function get_kms(this) result(kms)
          
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: kms
          ! Start of executable statements
          kms = this%m_kms
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_kme
!DIR$   ENDIF 
    pure function get_kme(this) result(kme)
          
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: kme
          ! Start of executable statements
          kme = this%m_kme
    end function
    
 !DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_jms
!DIR$   ENDIF 
    pure function get_jms(this) result(jms)
          
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: jms
          ! Start of executable statements
          jms = this%m_jms
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_jme
!DIR$   ENDIF 
    pure function get_jme(this) result(jme)
         
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: jme
          ! Start of executable statements
          jme = this%m_jme
    end function 
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nsize
!DIR$   ENDIF 
    pure function get_nsize(this) result(nsize)
          implicit none
          class(VADWind_t),  intent(in) :: this
          integer(I32P) :: nsize
          ! Start of executable statements
          nsize = this%m_nsize
    end function get_nsize
    
     !============================================
    !   Read/write procedures
    !============================================
    subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(VADWind_t),   intent(in) :: this
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
          class(VADWind_t),   intent(in) :: this
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
    
    subroutine dbg_info(this,verbose)
          implicit none
          class(VADWind_t),  intent(in) :: this
          logical(I32P),     intent(in) :: verbose
          ! Strat of executable statements
          print*, "==========================================================="
          print*, "        **** Dump of VADWind_t object state ***            "
          print*, "==========================================================="
          print*, "Collected at: ", __DATE__, ":", __TIME__, &
                  "in file: ", __FILE__, "at line: ", __LINE__
          print*, " m_ims:     ", this%m_ims, " m_ime: ", this%m_ime
          print*, " m_kms:     ", this%m_kms, " m_kme: ", this%m_kme
          print*, " m_jms:     ", this%m_jms, " m_jme: ", this%m_jme
          print*, " m_sdim1:   ", m_sdim1
          print*, " Wind altitude -- m_Alt:     ", this%m_Alt
          if(verbose == .true.) then
              print*, " Warning!! -- Printing possibly huge arrays!! "
              print*, " Wind V-component: ", this%m_V
              print*, " Wind U-component: ", this%m_U
              print*, " Wind W-component: ", this%m_W
          else
              print *, " Skipping over printing huge arrays."
          end if
          print*, "Wind direction:             "  this%m_Dir
          print*, " Wind speed -- m_SPD:       ", this%m_SPD 
          print*, " Wind speed (rms) -- m_RMS: ", this%m_RMS
          print*, " Wind SRNG:                 ", this%m_SRNG
          print*, " Antennae elevation:        ", this%m_ELEV
          
          print*, "==========================================================="
    end subroutine dbg_info
    
    !======================================
    !  Operator assignment (=)
    !======================================
    subroutine copy_assign(lhs,rhs)
          class(VADWind_t),  intent(inout) :: lhs
          class(VADWind_t),  intent(in)    :: rhs
          ! Start of executable ststements
          lhs%m_ims   = rhs%m_ims
          lhs%m_ime   = rhs%m_ime
          lhs%m_kms   = rhs%m_kms
          lhs%m_kme   = rhs%m_kme
          lhs%m_jms   = rhs%m_jms
          lhs%m_jme   = rhs%m_jme
          lhs%m_nsize = rhs%m_nsize
          lhs%m_Alt   = rhs%m_Alt
          lhs%m_U     = rhs%m_U
          lhs%m_V     = rhs%m_V
          lhs%m_W     = rhs%m_W
          lhs%m_Dir   = rhs%m_Dir
          lhs%m_SPD   = rhs%m_SPD
          lhs%m_RMS   = rhs%m_RMS
          lhs%m_SRNG  = rhs%m_SRNG
          lhs%m_ELEV  = rhs%m_ELEV
    end subroutine copy_assign
    
end module mod_wsr88d_vadwind