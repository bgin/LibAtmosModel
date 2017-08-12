
module mod_stokesvec


    !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_stokesvec'
 !          
 !          Purpose:
 !                   Implementation of 'Stokes Vector'
 !                   
 !                     
 !          History:
 !                        Date: 11-08-2017
 !                        Time: 14:31 GMT+2
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
    use module_kinds
    use module_logger
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_STOKESVEC_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_STOKESVEC_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_STOKESVEC_MICRO = 0

    ! Module full version
    integer(I32P), parameter, public :: MOD_STOKESVEC_FULLVER = 1000*MOD_STOKESVEC_MAJOR+100*MOD_STOKESVEC_MINOR + &
                                                                10*MOD_STOKESVEC_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_STOKESVEC_CREATE_DATE = "11-08-2017 15:02 +00200 (FRI 11 AUG 2017 GMT+2)"
    
    ! Module build date (should be set after every successful build)
    character(*),  parameter, public :: MOD_STOKESVEC_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_STOKESVEC_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_STOKESVEC_DESCRIPT = "Implememtation of Stokes Vector."
    
    !======================================================!
    !            Implementation                            !              
    !======================================================!
    
    type :: StokesVector_t
        
        private
        
        ! Component: I
        real(R64P) :: m_I
        
        ! Component: Q
        real(R64P) :: m_Q
        
        ! Compoenent: U
        real(R64P) :: m_U
        
        ! Component: V
        real(R64P) :: m_V
        
        ! logical built status indicator
        logical(I32P) :: m_isbuilt
        
        !============================================================70
        !    Type-bound procedures
        !    Decalaration
        !============================================================70
        
        contains
        
        ! Destructor
        procedure, pass(this), public :: destroy_svec
        
        ! Getters
        procedure, pass(this), public :: getI
        
        procedure, pass(this), public :: getQ
        
        procedure, pass(this), public :: getU
        
        procedure, pass(this), public :: getV
        
        procedure, pass(this), public :: get_status
        
        ! Computational procedures
        
        procedure, nopass, private :: su
        
        procedure, nopass, private :: sq
        
        procedure, nopass, private :: sv
        
        procedure, pass(this), public :: normQ
        
        procedure, pass(this), public :: normU
        
        procedure, pass(this), public :: normV
        
        procedure, pass(this), public :: poa ! Principle angle of polarization
        
        procedure, pass(this), public :: intensity
        
        procedure, pass(this), public :: dlp 
        
        procedure, pass(this), public :: dp
        
        procedure, pass(this), public :: dcp
        
        procedure, pass(this), public :: ellipticity
        
        !procedure, pass(this), public :: phase
        
       ! procedure, pass(this), public :: arct
        
        procedure, pass(this), public :: eccentricity
        
        procedure, pass(this), public :: totalpol ! total polarization
        
       
        
        procedure, pass(this), public :: print_svec
        
    end type StokesVector_t
        
    !============================================50
    ! Declaration of module operators
    !============================================50
    
    ! Constructor bound to name StokesVector_t 
    interface StokesVector_t
    
          procedure :: default_svec
          procedure :: create_svec
          procedure :: create_fbr ! fixed base representation
       
          procedure :: copy_svec
    
    end interface
    
    ! Operator assignment (=)
    interface assignment (=)
    
        module procedure :: assign_svec
    
    end interface
    
    ! Operator multiplication (*)
    interface operator (*)
    
        module procedure :: svec_mul_scalar
      
    end interface    
    
    ! Operator dot product (.dot.)
    interface operator (.dot.)
    
        module procedure :: dotp
    
    end interface
    
    ! Operator division (/)
    interface operator (/)
    
        module procedure :: svec_div_scalar
        
    end interface
    
    ! Operator addition (+)
    interface operator (+)
    
         module procedure :: svec_add_scalar
         module procedure :: svec_add_svec
         
    end interface
    
    ! Operator subtraction (-)
    interface operator (-)
    
        module procedure :: svec_sub_scalar
        module procedure :: svec_sub_svec
    
    end interface
    
    contains
    
    !======================================================60
    !  function: default_svec
    !            Creates StokesVector_t with its 4 fields
    !            set to zero.
    !======================================================60
    type(StokesVector_t) function default_svec()
          implicit none
          ! Start of executable statements
          default_svec%m_I = 0._R64P
          default_svec%m_Q = 0._R64P
          default_svec%m_U = 0._R64P
          default_svec%m_V = 0._R64P
          default_svec%m_isbuilt = .true.
    end function
    
    !======================================================60
    !  function: create_svec
    !            Creates StokesVector_t with its 4 fields
    !            set to specific polarization values.
    !======================================================60
    type(StokesVector_t) function create_svec(I,Q,U,V)
          implicit none
          real(R64P), intent(in) :: I,Q,U,V
          ! Start of executable statements
          create_svec%m_I = I
          create_svec%m_Q = Q
          create_svec%m_U = U
          create_svec%m_V = V
          create_svec%m_isbuilt = .true.
    end function
    
    !======================================================60
    !  function: create_fbr
    !            Creates StokesVector_t initialized from
    !            3 fixed base representation.
    !            1) Cartesian
    !            2) Cartesian rotated angle of 45 deg
    !            3) Cartesian circular
    !======================================================60
    type(StokesVector_t) function create_fbr(base,jvec)
          use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT
          use mod_jonesvec
          use mod_complex_arithm, only : cmag
          implicit none
          character(len=*),    intent(in) :: base
          type(JonesVector_t), intent(in) :: jvec
          ! Locals
          complex(R64P) :: tmp1
          ! Strat of executable statements
coord:    select case (base)
          case ("cartesian")
               tmp1 = jvec%m_h*DCONJG(jvec%m_v)
               create_fbr%m_I = cmag(jvec%m_h)*cmag(jvec%m_h) + &
                                cmag(jvec%m_v)*cmag(jvec%m_v)
               create_fbr%m_Q = cmag(jvec%m_h)*cmag(jvec%m_h) - &
                                cmag(jvec%m_v)*cmag(jvec%m_v)
               create_fbr%m_U = 2._R64P*DREAL(tmp1)
               create_fbr%m_V = -2._R64P*DIMAG(tmp1)
               create_fbr%m_isbuilt = .true.
          case ("rotated")
                 tmp1 = DCONJG(jvec%m_h)*jvec%m_v
                create_fbr%m_I = cmag(jvec%m_h)*cmag(jvec%m_h) + &
                                 cmag(jvec%m_v)*cmag(jvec%m_v)
                create_fbr%m_Q = -2._R64P*DREAL(tmp1)
                create_fbr%m_U = cmag(jvec%m_h)*cmag(jvec%m_h) - &
                                 cmag(jvec%m_v)*cmag(jvec%m_v)
                create_fbr%m_V = 2._R64P*DIMAG(tmp1)
                create_fbr%m_isbuilt = .true.
         case ("circular")
                tmp1 = DCONJG(jvec%m_h)*jvec%m_v
                create_fbr%m_I = cmag(jvec%m_h)*cmag(jvec%m_h) + &
                                 cmag(jvec%m_v)*cmag(jvec%m_v)
                create_fbr%m_Q = 2._R64P*DREAL(tmp1)
                create_fbr%m_U = -2._R64P*DIMAG(tmp1)
                create_fbr%m_V = cmag(jvec%m_h)*cmag(jvec%m_h) - &
                                 cmag(jvec%m_v)*cmag(jvec%m_v)
                create_fbr%m_isbuilt = .true.
         case default
                write(stderr,*) "mod_stokesvec/create_fbr:290, Invalid argument to select statement!!"
                create_fbr%m_isbuilt = .false.
                return
         end select coord
    end function
    
   
    !======================================================60
    !  function: copy_svec
    !            Creates StokesVector_t with its 4 fields
    !            set to copy of its argument.
    !======================================================60
    type(StokesVector_t) function copy_svec(other)
          use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT
          implicit none
          class(StokesVector_t), intent(in) :: other
          ! Start of executable statements
          if(other%m_isbuilt .EQ. .false.) then
              write(stderr,*) "mod_stokesvec/copy_svec:245, Argument in destroyed state!"
              return
          end if
          copy_svec%m_I = other%m_I
          copy_svec%m_Q = other%m_Q
          copy_svec%m_U = other%m_U
          copy_svec%m_V = other%m_V
          copy_svec%m_isbuilt = .true.
    end function
    
    !======================================================60
    !  subroutine: destroy_svec
    !              Destroys StokesVector_t by settings its
    !              members to invalid values.
    !======================================================60
    subroutine destroy_svec(this)
          use mod_constants, only : LAM_PINF
          use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT
          class(StokesVector_t), intent(inout) :: this
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              write(stderr,*) "mod_stokesvec/destroy_svec:266, StokesVector_t already destroyed!"
              return
          end if
          this%m_I = LAM_PINF
          this%m_Q = LAM_PINF
          this%m_U = LAM_PINF
          this%m_V = LAM_PINF
          this%m_isbuilt = .false.
    end subroutine
    
    
    !======================================================60
    !   Getter pure functions
    !======================================================60
    
    pure function getI(this) result(I)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: I
          ! Strat of executable statements
          I = this%m_I
    end function
    
    pure function getQ(this) result(Q)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: Q
          ! Start of executable statements
          Q = this%m_Q
    end function
    
    pure function getU(this) result(U)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: U
          ! Start of executable statements
          U = this%m_U
    end function
    
    pure function getV(this) result(V)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: V
          ! Start of executable statemetns
          V = this%m_V
    end function
    
    pure function get_status(this) result(isbuilt)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          logical(I32P) :: isbuilt
          ! Start of executable
          isbuilt = this%m_isbuilt
    end function
    
    !======================================================60
    !  private helper procedures
    !======================================================60
    
    pure function su(u) result(sqru)
          implicit none
          real(R64P), intent(in) :: u
          ! Locals
          real(R64P) :: sqru
          ! Start of executable statements
          sqru = u*u
    end function
    
    pure function sq(q) result(sqrq)
          implicit none
          real(R64P), intent(in) :: q
          ! Locals
          real(R64P) :: sqrq
          ! Start of executable sttements
          sqrq = q*q
    end function
    
    pure function sv(v) result(sqrv)
          implicit none
          real(R64P), intent(in) :: v
          ! Locals
          real(R64P) :: sqrv
          ! Strat of executable statements
          sqrv = v*v
    end function
    
    pure function normQ(this) result(nQ)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: nQ
          ! Start of executable statements
          nQ = this%getQ()/this%getI()
    end function
    
    pure function normU(this) result(nU)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: nU
          ! Start of executable statemetns
          nU = this%getU()/this%getI()
    end function
    
    pure function normV(this) result(nV)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: nV
          ! Start of executable statements
          nV = this%getV()/this%getI()
    end function
    
    pure function poa(this) result(pang)
          use mod_constants, only : LAM_HR64P
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: pang
          ! Start of executable statements
          pang = DATAN2(this%m_I,this%m_Q) * LAM_HR64P
    end function
    
    pure function intensity(this) result(intense)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: intense
          ! Start of executable statements
          intense = this%m_I
    end function
    
    pure function dlp(this) result(dlpol)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: dlpol
          ! Start of executable statements
          
          dlpol = DSQRT(su(this%m_U)+sq(this%m_Q))/this%m_I
    end function
    
    pure function dp(this) result(dpol)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: dpol ! degree of polarization
          ! Start of executable statements
          dpol = DSQRT(su(this%m_U)+sq(this%m_Q)+sv(this%m_V))/this%m_I
    end function
    
    pure function dcp(this) result(dcpol)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! locals
          real(R64P) :: dcpol
          ! Start of executable statements
          dcpol = this%m_V/this%m_I
    end function
    
    pure function ellipticity(this) result(ellip)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: ellip
          ! Start of executable statements
          ellip = this%m_V/(this%m_I+DSQRT(sq(this%m_Q)+su(this%m_U)))
    end function
    
    pure function eccentricity(this) result(eccentr)
          use mod_constants, only : LAM_ONER8
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: eccentr
          ! Strat of executable statements
          eccentr = DSQRT(LAM_ONER8-ellipticity()* &
                           ellipticity())
    end function
    
    pure function totalpol(this) result(tpol)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: tpol
          ! Start of executable statements
          tpol = sq(this%m_Q)+su(this%m_U)+sv(this%m_V)
    end function
    
    subroutine print_svec(this)
          implicit none
          class(StokesVector_t), intent(in) :: this
          ! Start of executable sattements
          print*, "========================================"
          print*, "   Component:     I=",this%m_I
          print*, "   Component:     Q=",this%m_Q
          print*, "   Component:     U=",this%m_U
          print*, "   Component:     V=",this%m_V
          print*, "   isbuilt:         ",this%m_isbuilt
          print*, "========================================"
    end subroutine
    
    !======================================================60
    ! subroutine: operator assignment (=)
    !======================================================60
    subroutine assign_svec(this,other)
          use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT
          class(StokesVector_t), intent(inout) :: this
          class(StokesVector_t), intent(in)    :: other
          ! Start of executable statements
          if(LOC(this).EQ.LOC(other)) then
              write(stderr,*) "mod_stokesvec/assign_svec:543, Attempted self-assignment!!"
              return
          end if
          this%m_I = other%m_I
          this%m_Q = other%m_Q
          this%m_U = other%m_U
          this%m_V = other%m_V
          this%m_isbuilt = other%m_isbuilt
    end subroutine
    
    !======================================================60
    ! function: svec_mul_scalar
    !======================================================60
    pure function svec_mul_scalar(this,s) result(nsvec)
          implicit none
          class(StokesVector_t), intent(in) :: this
          real(R64P),            intent(in) :: s
          ! Locals
          class(StokesVector_t) :: nsvec
          ! Start of executable statements
          nsvec%m_I = this%m_I*s
          nsvec%m_Q = this%m_Q*s
          nsvec%m_U = this%m_U*s
          nsvec%m_V = this%m_V*s
          nsvec%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: svec_mul_svec i.e dot product
    !======================================================60
    pure function dotp(this,other) result(dp)
          implicit none
          class(StokesVector_t), intent(in) :: this,other
          ! Locals
          real(R64P) :: dp
          ! Start of executable statemnts
          dp = (this%m_I*other%m_I)+(this%m_Q*other%m_Q)+ &
               (this%m_U*other%m_U)+(this%m_V*other%m_V)
    end function
    
     
    !======================================================60
    ! function: svec_div_scalar
    !======================================================60
    pure function svec_div_scalar(this,s) result(nsvec)
          implicit none
          class(StokesVector_t), intent(in) :: this
          real(R64P),            intent(in) :: s
          ! Locals
          class(StokesVector_t) :: nsvec
          ! Start of executable statements
          nsvec%m_I = this%m_I/s
          nsvec%m_Q = this%m_Q/s
          nsvec%m_U = this%m_U/s
          nsvec%m_V = this%m_V/s
          nsvec%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: svec_add_scalar
    !======================================================60
    pure function svec_add_scalar(this,s) result(nsvec)
          implicit none
          class(StokesVector_t), intent(in) :: this
          real(R64P),            intent(in) :: s
          ! Locals
          class(StokesVector_t) :: nsvec
          ! Start of executable statements
          nsvec%m_I = this%m_I+s
          nsvec%m_Q = this%m_Q+s
          nsvec%m_U = this%m_U+s
          nsvec%m_V = this%m_V+s
          nsvec%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: svec_add_svec
    !======================================================60
    pure function svec_add_svec(this,other) result(nsvec)
          implicit none
          class(StokesVector_t), intent(in) :: this,other
          ! Locals
          class(StokesVector_t) :: nsvec
          ! Strat of executable statements
          nsvec%m_I = this%m_I+other%m_I
          nsvec%m_Q = this%m_Q+other%m_Q
          nsvec%m_U = this%m_U+other%m_U
          nsvec%m_V = this%m_V+other%m_V
          nsvec%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: svec_sub_scalar
    !======================================================60
    pure function svec_sub_scalar(this,s) result(nsvec)
          implicit none
          class(StokesVector_t), intent(in) :: this
          real(R64P),            intent(in) :: s
          ! Locals
          class(StokesVector_t) :: nsvec
          ! Start of executable statements
          nsvec%m_I = this%m_I-s
          nsvec%m_Q = this%m_Q-s
          nsvec%m_U = this%m_U-s
          nsvec%m_V = this%m_V-s
          nsvec%m_isbuilt = .true.
    end function
    
    !======================================================60
    ! function: svec_sub_svec
    !======================================================60
    pure function svec_sub_svec(this,other) result(nsvec)
          implicit none
          class(StokesVector_t), intent(in) :: this,other
          ! Locals
          class(StokesVector_t) :: nsvec
          ! Strat of executable statements
          nsvec%m_I = this%m_I-other%m_I
          nsvec%m_Q = this%m_Q-other%m_Q
          nsvec%m_U = this%m_U-other%m_U
          nsvec%m_V = this%m_V-other%m_V
          nsvec%m_isbuilt = .true.
    end function
    
end module mod_stokesvec