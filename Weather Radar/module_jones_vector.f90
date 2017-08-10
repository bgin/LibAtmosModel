
module mod_jonesvec

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_jonesvec'
 !          
 !          Purpose:
 !                    Implements so called 'Jones Vector'
 !                   
 !                     
 !          History:
 !                        Date: 08-08-2017
 !                        Time: 12:22 GMT+2
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
    integer(I32P), parameter, public :: MOD_JONESVEC_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_JONESVEC_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_JONESVEC_MICRO = 0
    
    ! Module/file full version
    integer(I32P), parameter, public :: MOD_JONESVEC_FULLVER = 1000*MOD_JONESVEC_MAJOR+100*MOD_JONESVEC_MINOR + &
                                                               10*MOD_JONESVEC_MICRO
    
    ! Module/file creation date
    character(*),  parameter, public :: MOD_JONESVEC_CREATE_DATE = "08-08-2017 12:30 +00200 (TUE 08 AUG 2017 GMT+2)"
    
    ! Module/file build date/time (should be set to appropriate values after successful build date)
    character(*),  parameter, public :: MOD_JONESVEC_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_JONESVEC_AUTHOR = "Programmer: Bernard Gingold e-mail:beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_JONESVEC_DESCRIPT = "Implementation of Jones Vector"
    
    complex(R64P), parameter, private :: ZC = DCMPLX(0._R64P,0._R64P)
    
    !============================================50
    ! Type: JonesVector_t
    !============================================50
    
    type :: JonesVector_t
        
         private
         
         ! Horizontal component of Electric Field Hx
         complex(R64P) :: m_h
         
         ! Vertical component of Electric Field Hy
         complex(R64P) :: m_v
         
         ! Logical indicator set to true if 
         ! object is created
         logical(I32P) :: m_isbuilt
        !============================================================70
        !    Type-bound procedures
        !    Decalaration
        !============================================================70
         
        contains
        
        ! Destructor
         
        
        procedure, pass(this), public :: destroy_jvec
        
        ! Getters
        
        procedure, pass(this), public :: get_h
        
        procedure, pass(this), public :: get_v
        
        procedure, pass(this), public :: get_built_status
        
        ! Computational procedures
        
        procedure, pass(this), public :: field_intensity
        
        procedure, pass(this), public :: field_atan
        
        procedure, pass(this), public :: field_pdiff
        
        procedure, pass(this), public :: polarization_degree
        
        procedure, pass(this), public :: jvec_conj
        
        ! helpers
        procedure, nopass, public :: print_jvec
        
        end type JonesVector_t
    !============================================50
    ! Declaration of module operators
    !============================================50
    
    ! Constructor bound to name JonesVector_t
        
    interface  JonesVector_t
    
        procedure :: default_jvec
        
        procedure :: create_jvec
        
        procedure :: copy_jvec  
    
    end interface
        
    ! Operator assignment
        
    interface assignment (=)
    
          module procedure assign_jvec
    
    end interface
    
    ! Operator multiplication
    
    interface operator (*)
    
          module procedure jvec_mul_jvec
          module procedure jvec_mul_complex
         
          
   end interface       
   
    
    ! Operator division
    
    interface operator (/)
    
          module procedure jvec_div_complex
         
          
    end interface
    
    ! Operator addition
    
    interface operator (+)
    
          module procedure jvec_add_jvec
         
    
    end interface
    
    ! Operator subtraction
    
    interface operator (-)
    
          module procedure jvec_negate
          module procedure jvec_sub_jvec
         
    
    end interface
    
   
    
    contains
    
    !======================================================60
    !  function: default_jvec
    !            Creates JonesVector_t with its two fields
    !            set to zero.
    !======================================================60
    type(JonesVector_t) function default_jvec()
          implicit none
          ! Start of executable statements
          default_jvec%m_h = DCMPLX(0._R64P,0._R64P)
          default_jvec%m_v = DCMPLX(0._R64P,0._R64P)
          default_jvec%m_isbuilt = .true.
    end function
    
    !======================================================60
    !  function: create_jvec
    !            Creates JonesVector_t with its two fields
    !            set to specific polarization values.
    !======================================================60
    type(JonesVector_t) function create_jvec(h,v)
          implicit none
          complex(R64P), intent(in) :: h,v
          ! Start of executable statements
          create_jvec%m_h = h
          create_jvec%m_v = v
          create_jvec%m_isbuilt = .true.
    end function
    
    !======================================================60
    !  function: copy_jvec
    !            Creates JonesVector_t with its two fields
    !            set to copy of its argument.
    !======================================================60
    type(JonesVector_t) function copy_jvec(other)
          use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT
          implicit none
          class(JonesVector_t), intent(in) :: other
          ! Start of executable statements
          if(other%m_isbuilt .EQ. .false.) then
              write(stderr,*) "copy_jvec:236, Argument in destroyed state!"
              return
          end if
          copy_jvec%m_h = other%m_h
          copy_jvec%m_v = other%m_v
          copy_jvec%m_isbuilt = .true.
    end function

    !======================================================60
    !  subroutine: destroy_jvec
    !              Destroys JonesVector_t by settings its
    !              members to invalid values.
    !======================================================60
    subroutine destroy_jvec(this)
          use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT
          use IEEE_ARITHMETIC, only : IEEE_VALUE
          implicit none
          class(JonesVector_t), intent(inout) :: this
          ! Locals
          real(R64P) :: nan
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              write(stderr,*) "destroy_jvec:261, JonesVector_t already destroyed!"
              return
          end if
          nan = IEEE_VALUE(1._R64P,IEEE_QUIET_NAN)
          this%m_h = DCMPLX(nan,nan)
          this%m_v = DCMPLX(nan,nan)
          this%m_isbuilt = .false.
    end subroutine
    
    !======================================================60
    ! function: get_h
    !======================================================60
    pure function get_h(this) result(h)
          implicit none
          class(JonesVector_t), intent(in) :: this
          ! Locals
          complex(R64P) :: h
          ! Start of executable statements
          h = this%m_h
    end function
    
    !======================================================60
    ! function: get_v
    !======================================================60
    pure function get_v(this) result(v)
          implicit none
          class(JonesVector_t), intent(in) :: this
          ! Locals
          complex(R64P) :: v
          ! Start of executable statements
          v = this%m_v
    end function
    
    !======================================================60
    ! function: get_built_status
    !======================================================60
    pure function get_built_status(this) result(isbuilt)
          implicit none
          class(JonesVector_t), intent(in) :: this
          ! Locals
          logical(I32P) :: isbuilt
          ! Start of executable statements
          isbuilt = this%m_isbuilt
    end function
    
    !======================================================60
    ! function: field_intensity
    !======================================================60
    pure function field_intensity(this) result(norm)
          use mod_complex_arithm, only : cnorm
          implicit none
          class(JonesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: norm
          ! Start of executable statements
          norm = cnorm(this%m_h)+cnorm(this%m_v)
    end function
    
    !======================================================60
    ! function: field_atan
    !======================================================60
    pure function field_atan(this) result(ang)
          use mod_complex_arithm, only : cmag
          implicit none
          class(JonesVector_t), intent(in) :: this
          ! Locals
          real(R64P) :: ang,tmp
          ! Start of executable statements
          tmp = cmag(this%m_v)/cmag(this%m_h)
          ang = DATAN(tmp)
    end function
    
    !======================================================60
    ! function: field_pdiff
    !======================================================60
    pure function field_pdiff(this) result(pdiff)
         use mod_complex_arithm, only : carg
         implicit none
         class(JonesVector_t), intent(in) :: this
         ! Locals
         real(R64P) :: pdiff
         ! Start of executable statements
         pdiff = carg(this%m_v)-carg(this%m_h)
    end function
    
    !======================================================60
    ! function: polarization_degree
    !======================================================60
     pure function polarization_degree() return(val)
          implicit none
          ! Locals
          real(R64P) :: val
          val = 1._R64P
    end function
    
    !======================================================60
    ! function: jvec_conj
    !======================================================60
    pure function jvec_conj(jv1) result(njvec)
          implicit none
          class(JonesVector_t), intent(in) :: jv1
          ! Locals
          class(JonesVector_t) :: njvec
          ! Start of executable statements
          njvec = JonesVector_t(DCONJG(jv1%m_h),DCONJG(jv1%m_v))
    end function
    
    !======================================================60
    ! subroutine: print_jvec
    !======================================================60
    subroutine print_jvec(this)
          implicit none
          class(JonesVector_t), intent(in) :: this
          ! Start of executable statements
          print*, "==============================================="
          print*, " Vertical component   'v'   ", this%m_v
          print*, " Horizontal component 'h'   ", this%m_h
          print*, " JonesVector_t build status:", this%m_isbuilt
          print*, "================================================"
    end subroutine
    
    !======================================================60
    ! subroutine:  assignmemt (=)
    ! 
    !======================================================60
    subroutine assign_jvec(this,other)
          use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT
          implicit none
          class(JonesVector_t), intent(inout) :: this
          class(JonesVector_t), intent(in)    :: other
          ! Start of executable statements
          if(LOC(this).EQ.LOC(other)) then
              write(stderr,*) "assign_jvec:382, Attempted self-assignment!"
              return
          end if
          this%m_h = other%m_h
          this%m_v = other%m_v
          this%m_isbuilt = other%m_isbuilt
    end subroutine
    
    !======================================================60
    ! function: jvec_mul_jvec
    ! Remark: valid only for: conjg(this)*jv1
    !======================================================60
    pure function jvec_mul_jvec(this,jv1) result(c)
          implicit none
          class(JonesVector_t), intent(in) :: this
          class(JonesVector_t), intent(in) :: jv1
          ! Locals
          complex(R64P) :: c
          ! Start of executable statements
          c = this%m_h*jv1%m_h+this%m_v*jv1%m_v
    end function
    
    !======================================================60
    !  function: jvec_mul_complex
    !======================================================60
    pure function jvec_mul_complex(this,c) result(njvec)
          implicit none
          class(JonesVector_t), intent(in) :: this
          complex(R64P),        intent(in) :: c
          ! Locals
          class(JonesVector_t) :: njvec
          ! Start of executable statements
          njvec%m_h = this%m_h*c
          njvec%m_v = this%m_v*c
          njvec%m_isbuilt = .true.
    end function
    
    
    
    !======================================================60
    !  function: jvec_div_complex
    !======================================================60
    pure function jvec_div_complex(this,c) result(njvec)
          implicit none
          class(JonesVector_t), intent(in) :: this
          complex(R64P),        intent(in) :: c
          ! Start of executable statements
          if(c.EQ.ZC) then
              print*, "operator (/):446, Attempted division by (0.0,0.0)"
              return
         end if    
         njvec%m_h = this%m_h/c 
         njvec%m_v = this%m_v/c
         njvec%m_isbuilt = .true.
    end function
    
   
    
    !======================================================60
    ! function: jvec_add_jvec
    !======================================================60
    pure function jvec_add_jvec(this,other) result(njvec)
          implicit none
          class(JonesVector_t), intent(in) :: this,other
          ! Locals
          class(JonesVector_t) :: njvec
          ! Start of executable statements
          njvec = JonesVector_t(this%m_h+other%m_h, &
                                this%m_v+other%m_v )
    end function
    
   
    
    !======================================================60
    ! function: jvec_negate
    !======================================================60
    pure function jvec_negate(this) result(njvec)
          implicit none
          class(JonesVector_t), intent(inout) :: this
          ! Locals
          class(JonesVector_t) :: njvec
          ! Start of executable statements
          njvec = JonesVector_t(-this%m_h,-this%m_v)
    end function
    
    !======================================================60
    ! function: jvec_sub_jvec
    !======================================================60
    pure function jvec_sub_jvec(this,other) result(njvec)
          implicit none
          class(JonesVector_t), intent(in) :: this,other
          ! Lcals
          class(JonesVector) :: njvec
          ! Start of executable statements
          njvec = JonesVector_t(this%m_h-other%m_h, &
                                this%m_v-other%m_v)
    end function      
    
   
   
end module mod_jonesvec