
module mod_quaternion

#include "Config.hpp"

             
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_quaternion'
 !          
 !          Purpose:
 !                    Modeling of quaternions and their
 !                    corresponding mathematical operations.
 !          History:
 !                        Date: 02-12-2017
 !                        Time: 09:38 GMT+2
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
 !                          Mainly wikipedia.org
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
    private
    use module_kinds,    only : I32P,R64P
    
   
    ! public operators
    
    public :: assignment (=)
    public :: operator (+)
    public :: operator (-)
    public :: operator (*)
    public :: operator (/)
    public :: operator (==)
    public :: operator (/=)
    public :: operator (>)
    public :: operator (<)
    public :: operator (.idotp.)
    public :: operator (.icrossp.)
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(I32P), parameter, public :: MOD_QUATERNION_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_QUATERNION_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_QUATERNION_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_QUATERNION_FULLVER = 1000*MOD_QUATERNION_MAJOR+100*MOD_QUATERNION_MINOR+ &
                                                                 10*MOD_QUATERNION_MICRO
    
    ! Module/file creation date
    character(*),  parameter, public :: MOD_QUATERNION_CREATE_DATE = "02-12-2017 09:38 +00200 (SAT 02 DEC 2017 GMT+2)"
    
    ! Module build date (should be set after successfull compilation)
    character(*),  parameter, public :: MOD_QUATERNION_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_QUATERNION_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module description
    character(*),  parameter, public :: MOD_QUATERNION_DESCRIPT = " Implementation of Quaternions."
    
    !====================================
    !   Type: Quaternion_t
    !====================================
    
    type, public :: Quaternion_t
        
       
        
        ! derived type members
        
        real(R64P) :: m_x
        
        real(R64P) :: m_y
        
        real(R64P) :: m_z
        
        real(R64P) :: m_w
        
        contains
    
        ! Constructors
        
        ! default init to {0.0,0.0,0.0,0.0}
        procedure, pass(this), public :: default_init
        
        ! element-wise initialization
        procedure, pass(this), public :: elemwise_init
        
        ! initialize from complex number pair
        procedure, pass(this), public :: complex_init
        
        ! initialize from array of size 4
        procedure, pass(this), public :: array_init
        
        ! initialize scalar part only (vector part is set to 0.0)
        procedure, pass(this), public :: scpart_init
        
        ! initialize vector part only (scalar part is set to 0.0)
        procedure, pass(this), public :: vpart_init
        
        ! initialize by copy of rhs
        procedure, pass(this), public :: copy_init
        
        ! getters
        
        procedure, pass(this), public :: get_x
        
        procedure, pass(this), public :: get_y
        
        procedure, pass(this), public :: get_z
        
        procedure, pass(this), public :: get_w
        
        procedure, pass(this), public :: get_c1
        
        procedure, pass(this), public :: get_c2
        
        procedure, pass(this), public :: get_scpart
        
        procedure, pass(this), public :: get_vecpart
        
        ! read/write procedures
        
        procedure, nopass, public :: read_quaternion
        
        procedure, nopass, public :: write_quaternion
        
        ! Computational procedures
        
        procedure, pass(this), public :: conjugate
        
        procedure, pass(this), public :: norm
        
        procedure, pass(this), public :: vnorm
        
        procedure, pass(this), public :: distance
        
        procedure, pass(this), public :: unit
        
        procedure, pass(this), public :: polar_decomp
        
        procedure, pass(this), public :: reciprocal
        
        procedure, pass(this), public :: mat4x4
        
        procedure, pass(this), public :: q_exp
        
        procedure, pass(this), public :: q_ln
        
        
        
    end type :: Quaternion_t   
        
        
        !======================================
        !   Module operators
        !======================================
        
        interface assignment (=)
            module procedure q_assign_q
        end interface
        
        interface operator (+)
            module procedure q_add_q
            module procedure q_add_c
            module procedure q_add_s
            module procedure c_add_q
            module procedure s_add_q
        end interface
        
        interface operator (-)
            module procedure q_sub_q
            module procedure q_sub_c
            module procedure q_sub_s
            module procedure c_sub_q
            module procedure s_sub_q
        end interface
        
        interface operator (*)
            module procedure q_mul_q
            module procedure q_mul_c
            module procedure q_mul_s
            module procedure c_mul_q
            module procedure s_mul_q
        end interface
        
        interface operator (/)
            module procedure q_div_q
            module procedure q_div_c
            module procedure q_div_s
            module procedure c_div_q
            module procedure s_div_q
        end interface
        
        interface operator (==)
            module procedure q_eq_q
            module procedure q_eq_c
            module procedure q_eq_s
            module procedure c_eq_q
            module procedure s_eq_q
        end interface
        
        interface operator (/=)
            module procedure q_neq_q
            module procedure q_neq_c
            module procedure q_neq_s
            module procedure c_neq_q
            module procedure s_neq_q
        end interface
        
        interface operator (>)
            module procedure q_gt_q
            module procedure q_gt_c
            module procedure q_gt_s
            module procedure c_gt_q
            module procedure s_gt_q
        end interface
        
        interface operator (<)
            module procedure q_lt_q
            module procedure q_lt_c
            module procedure q_lt_s
            module procedure c_lt_q
            module procedure s_lt_q
        end interface
        
        interface operator (.idotp.)
            module procedure q_idotp_q
        end interface
        
        interface operator (.icrossp.)
            module procedure q_icrossp_q
        end interface
        
    contains
    
    !========================================!
    !    Implementation                      !
    !========================================!    
    
    !=================================================
    !  @subroutine: default_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its default
    !            initialization to {0.0,0.0,0.0,0.0}
    !=================================================
    subroutine default_init(this)
          implicit none
          class(Quaternion_t), intent(out) :: this
          ! Start of executable statements
          this%m_x = 0._R64P
          this%m_y = 0._R64P
          this%m_z = 0._R64P
          this%m_w = 0._R64P
    end subroutine
    
    !=================================================
    !  @subroutine: elemwise_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its element-wise
    !            initialization.
    !=================================================
    subroutine elemwise_init(this,x,y,z,w)
          implicit none
          class(Quaternion_t), intent(out) :: this
          real(R64P),          intent(in)  :: x,y,z,w
          ! Start of executable statements
          this%m_x = x
          this%m_y = y
          this%m_z = z
          this%m_w = w
    end subroutine
    
    !=================================================
    !  @subroutine: complex_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and member initialization
    !            by user passed a pair of complex numbers
    !=================================================
    subroutine complex_init(this,c1,c2)
          implicit none
          class(Quaternion_t), intent(out) :: this
          complex(R64P),       intent(in)  :: c1,c2
          ! Start of executable statements
          this%m_x = REAL(c1,KIND=8)
          this%m_y = AIMAG(c1,KIND=8)
          this%m_z = REAL(c2,KIND=8)
          this%m_w = AIMAG(c2,KIND=8)
    end subroutine
    
    !=================================================
    !  @subroutine: array_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its member initialization
    !            by user passed array of real numbers.
    !=================================================
    subroutine array_init(this,a)
          implicit none
          class(Quaternion_t),      intent(out) :: this
          real(R64P), dimension(4), intent(in)  :: a
          ! Start of executable statements
          this%m_x = a(1)
          this%m_y = a(2)
          this%m_z = a(3)
          this%m_w = a(4)
    end subroutine
    
    !=================================================
    !  @subroutine: scpart_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its member m_x
    !            initialzation by user passed scalar
    !================================================= 
    subroutine scpart_init(this,x)
          implicit none
          class(Quaternion_t), intent(out) :: this
          real(R64P),          intent(in)  :: x
          ! Start of executable statements
          this%m_x = x
          this%m_y = 0._R64P
          this%m_z = 0._R64P
          this%m_w = 0._R64P
    end subroutine
    
     
    !=================================================
    !  @subroutine: vpart_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its member m_x,m_y,m_z
    !            initialzation by user passed scalars.
    !================================================= 
    subroutine vpart_init(this,y,z,w)
          implicit none
          class(Quaternion_t), intent(out) :: this
          real(R64P),          intent(in)  :: y,z,w
          ! Start of executable statements
          this%m_x = 0._R64P
          this%m_y = y
          this%m_z = z
          this%m_w = w
    end subroutine
    
    !=================================================
    !  @subroutine: copy_init
    !  @Purpose: 
    !            Construction of object of type
    !            Quaternion_t and its member m_x,m_y,m_z,m_w
    !            initialization by user passed Quaternion_t
    !================================================= 
    subroutine copy_init(this,rhs)
          implicit none
          class(Quaternion_t), intent(out) :: this
          class(Quaternion_t), intent(in)  :: rhs
          ! Start of executable statements
          this%m_x = rhs%m_x
          this%m_y = rhs%m_y
          this%m_z = rhs%m_z
          this%m_w = rhs%m_w
    end subroutine
    
    !=============================================
    ! @function: 
    !               get_x
    ! @attributes: pure
    !=============================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_x
!DIR$ ENDIF    
    pure function get_x(this) result(x)
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          real(R64P) :: x
          ! Start of executable statements
          x = this%m_x
    end function
    
    !=============================================
    ! @function: 
    !               get_y
    ! @attributes: pure
    !=============================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_y
!DIR$ ENDIF    
    pure function get_y(this) result(y)
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          real(R64P) :: y
          ! Strat of executable statements
          y = this%m_y
    end function
    
    !=============================================
    ! @function: 
    !               get_z
    ! @attributes: pure
    !=============================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_z
!DIR$ ENDIF   
    pure function get_z(this) result(z)
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          real(R64P) :: z
          ! Start of executable satements
          z = this%m_z
    end function
    
    !=============================================
    ! @function: 
    !               get_w
    ! @attributes: pure
    !=============================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_w
!DIR$ ENDIF 
    pure function get_w(this) result(w)
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          real(R64P) :: w
          ! Start of executable statements
          w = this%m_w
    end function
    
    !=============================================
    ! @function: 
    !               get_c1
    ! @attributes: pure
    !=============================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_c1
!DIR$ ENDIF 
    pure function get_c1(this) result(c1)
          implicit none
          class(Quaternion_t), intent(in) :: c1
          ! Locals
          complex(R64P) :: c1
          ! Start of executable 
          c1 = DCMPLX(this%m_x,this%m_y)
    end function
    
    !=============================================
    ! @function: 
    !               get_c2
    ! @attributes: pure
    !=============================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_c2
!DIR$ ENDIF
    pure function get_c2(this) result(c2)
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          complex(R64P) :: c2
          ! Start of executable statements
          c2 = DCMPLX(this%m_z,this%m_w)
    end function
    
    !=============================================
    ! @function: 
    !               get_scpart
    ! @attributes: pure
    !=============================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_scpart
!DIR$ ENDIF
    pure function get_scpart(this) result(scpart)
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          real(R64P) :: scpart
          ! Start of executable statements
          scpart = this%m_x
    end function
    
     
    !=============================================
    ! @function: 
    !               get_vpart
    ! @attributes: pure
    !=============================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_vpart
!DIR$ ENDIF
    pure function get_vpart(this) result(vpart)
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          real(R64P), dimension(3) :: vpart
          ! Start of executable statemetns
          vpart(1) = this%m_y
          vpart(2) = this%m_z
          vpart(3) = this%m_w
    end function
    
    !=================================================!
    !  @subroutine: read_quaternion                                      
    !  @Purpose:
    !            read/write
    !=================================================! 
    subroutine read_quaternion(this,form,unit,ioerr)
          implicit none
          class(Quaternion_t), intent(in)     :: this
          character(len=*),     intent(in)    :: form
          integer(I32P),        intent(in)    :: unit
          integer(I32P),        intent(inout) :: ioerr
          ! Stat of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
    !=================================================!
    !  @subroutine: write_quaternion                                      
    !  @Purpose:
    !            read/write
    !=================================================! 
    subroutine write_quaternion(this,form,unit,ioerr)
          implicit none
          class(Quaternion_t), intent(in)     :: this
          character(len=*),     intent(in)    :: form
          integer(I32P),        intent(in)    :: unit
          integer(I32P),        intent(inout) :: ioerr
          ! Stat of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
    !=============================================
    ! @function: 
    !               conjugate
    ! @attributes: pure
    !=============================================
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: conjugate
!DIR$ ENDIF    
   pure function conjugate(this) result(q)
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          type(Quaternion_t) :: q
          ! Start of executable statements
          q%m_x =  this%m_x
          q%m_y = -this%m_y
          q%m_z = -this%m_z
          q%m_w = -this%m_w
    end function
    
    !=============================================
    ! @function: 
    !               norm
    ! @attributes: pure
    !=============================================
  pure  function norm(this) result(n)
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          real(R64P) :: n
          ! Start of executable statements
          n = SQRT(this%m_x*this%m_x+this%m_y*this%m_y+ &
                   this%m_z*this%m_z+this%m_w*this%m_w)
    end function
    
    !=============================================
    ! @function: 
    !               vnorm
    ! @attributes: pure
    !=============================================
  pure  function vnorm(this) result(n)
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          real(R64P) :: n
          ! Start of executable statements
          n = SQRT(this%m_y*this%m_y+this%m_z*this%m_z+ &
                   this%m_w*this%m_w)
  end function
  
  !=============================================
  ! @function: 
  !               distance
  ! @attributes: pure
  !=============================================
  pure function distance(this,other) result(dist)
          implicit none
          class(Quaternion_t), intent(in) :: this,other
          ! Locals
          real(R64P) :: dist
          ! Start of executable statements
          associate(x1=>this%m_x,y1=this%m_y,   &
                    z1=>this%m_z,w1=>this%m_w,  &
                    x2=>other%m_x,y2=>other%m_y,&
                    z2=>other%m_z,w2=>other%m_w)
              dist = SQRT((x1-x2)**2+(y1-y2)**2+  &
                          (z1-z2)**2+(w1-w2)**2)
          end associate
  end function 
  
  !=============================================
  ! @function: 
  !              unit
  ! @attributes: pure
  !=============================================
  pure function unit(this) result(uq)
          use mod_constants, only : LAM_PINF
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          type(Quaternion_t) :: uq
          real(R64P) :: t
          ! Start of executable statements
          if(this%m_x == 0._R64P .OR. &
             this%m_y == 0._R64P .OR. &
             this%m_z == 0._R64P .OR. &
             this%m_w == 0._R64P    ) then
                 uq%m_x = LAM_PINF
                 uq%m_y = LAM_PINF
                 uq%m_z = LAM_PINF
                 uq%m_w = LAM_PINF
          end if
          t = norm(this)
          uq%m_x = this%m_x / t
          uq%m_y = this%m_y / t
          uq%m_z = this%m_z / t
          uq%m_w = this%m_w / t
  end function
  
  !=============================================
  ! @function: 
  !              polar_decomp
  ! @attributes: pure
  !=============================================
  pure function polar_decomp(this) result(pd)
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          type(Quaternion_t) :: pd,q
          real(R64P) :: t
          ! Start of executable statements
          q = unit(this)
          t = norm(this)
          pd%m_x = t * q%m_x
          pd%m_y = t * q%m_y
          pd%m_z = t * q%m_z
          pd%m_w = t * q%m_w
  end function
  
  !=============================================
  ! @function: 
  !              reciprocal
  ! @attributes: pure
  !=============================================
  pure function reciprocal(this) result(r)
          implicit none
          class(Quaternion_t), intent(in)
          ! Locals
          type(Quaternion_t) :: r,q
          real(R64P) :: n
          ! Start of executable statemetns
          q = conjugate(this)
          n = norm(this)
          r%m_x = q%m_x / (n*n)
          r%m_y = q%m_y / (n*n)
          r%m_z = q%m_z / (n*n)
          r%m_w = q%m_w / (n*n)
  end function
  
  !=============================================
  ! @function: 
  !              mat4x4
  ! @attributes: pure
  !=============================================
  pure function mat4x4(this,mtype) result(m4x4)
          implicit none
          class(Quaternion_t), intent(in) :: this
          integer(I32P),       intent(in) :: mtype
          ! Locals
          real(R64P), dimension(4,4) :: m4x4
          ! Start of executable statements
          if(mtype == 0) then
              m4x4(1,1) =  this%m_x
              m4x4(1,2) = -this%m_y
              m4x4(1,3) = -this%m_z
              m4x4(1,4) = -this%m_w
              m4x4(2,1) =  this%m_y
              m4x4(2,2) =  this%m_x
              m4x4(2,3) = -this%m_w
              m4x4(2,4) =  this%m_z
              m4x4(3,1) =  this%m_z
              m4x4(3,2) =  this%m_w
              m4x4(3,3) =  this%m_x
              m4x4(3,4) = -this%m_y
              m4x4(4,1) =  this%m_w
              m4x4(4,2) = -this%m_z
              m4x4(4,3) =  this%m_y
              m4x4(4,4) =  this%m_x
          else if (mtype == 1) then
              m4x4(1,1) =  this%m_x
              m4x4(1,2) =  this%m_y
              m4x4(1,3) =  this%m_z
              m4x4(1,4) =  this%m_w
              m4x4(2,1) = -this%m_y
              m4x4(2,2) =  this%m_x
              m4x4(2,3) = -this%m_w
              m4x4(2,4) =  this%m_z
              m4x4(3,1) = -this%m_z
              m4x4(3,2) =  this%m_w
              m4x4(3,3) =  this%m_x
              m4x4(3,4) = -this%m_y
              m4x4(4,1) = -this%m_w
              m4x4(4,2) = -this%m_z
              m4x4(4,3) =  this%m_y
              m4x4(4,4) =  this%m_x
          end if
  end function        
              
  !=============================================
  ! @function: 
  !              q_exp
  ! @attributes: pure
  !=============================================
  pure function q_exp(this) result(qe)
          use mod_constants, only : LAM_MEPS8
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          type(Quaternion_t) :: qe
          real(R64P) :: t1,t2
          real(R64P), dimension(3) :: v
          ! Start of executable statemetns
          t1 = EXP(get_scpart(this))  ! e^a
          t2 = vnorm(get_vpart(this))
          if(t2 < LAM_MEPS8) then
              t2 = t2 + LAM_MEPS8
           end if
          v = get_vpart(this)
          v(1) = v(1) / t2 * SIN(t2)
          v(2) = v(2) / t2 * SIN(t2)
          v(3) = v(3) / t2 * SIN(t2)
          qe%m_x = t1 * COS(t2)
          qe%m_y = t1 * v(1)
          qe%m_z = t1 * v(2)
          qe%m_w = t1 * v(3)
  end function        
          
  !=============================================
  ! @function: 
  !              q_ln
  ! @attributes: pure
  !=============================================
  pure function q_ln(this) result(lq)
          use mod_constants, LAM_MEPS8
          implicit none
          class(Quaternion_t), intent(in) :: this
          ! Locals
          type(Quaternion_t) :: lq
          real(R64P) :: t1,t2,a
          real(R64P), dimension(3) :: v
          ! Start of executable statements
          t1 = LOG(norm(this))   ! ln||q||
          if(t1 < LAM_MEPS8) then
              t1 = t1 + LAM_MEPS8
          end if
          a = get_scpart(this)
          t2 = vnorm(get_vpart(this))  ! ||vecpart of q||
          v =  get_vpart(this)
          v(1) = v(1) / t2 * ACOS(a/t2)
          v(2) = v(2) / t2 * ACOS(a/t2)
          v(3) = v(3) / t2 * ACOS(a/t2)
          lq%m_x = t1
          lq%m_y = v(1)
          lq%m_z = v(2)
          lq%m_w = v(3)
  end function
  
  !=============================================
  ! @subroutine: 
  !             q_assign_q
  ! @attributes: 
  !=============================================
  subroutine q_assign_q(this,other)
          implicit none
          type(Quaternion_t), intent(out) :: this
          type(Quaternion_t), intent(in)  :: other
          ! Start of executable statements
          if(LOC(this) == LOC(other)) then
              WRITE(*,*) "assignment (=) -- Self assignment!!"
              return
          end if
          this%m_x = other%m_x
          this%m_y = other%m_y
          this%m_z = other%m_z
          this%m_w = other%m_w
  end subroutine
  
  !=============================================
  ! @function: 
  !              q_add_q (operator (+))
  ! @attributes: 
  !=============================================
  pure function q_add_a(lhs,rhs) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: lhs,rhs
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq%m_x = lhs%m_x + rhs%m_x
          nq%m_y = lhs%m_y + rhs%m_y
          nq%m_z = lhs%m_z + rhs%m_z
          nq%m_w = lhs%m_w + rhs%m_w
  end function
  
  !=============================================
  ! @function: 
  !              q_add_c (operator (+))
  ! @attributes: 
  !=============================================
  pure function q_add_c(lhs,rhs) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: lhs
          complex(R64P),      intent(in) :: rhs
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq%m_x = lhs%m_x + REAL(rhs,KIND=8)
          nq%m_y = lhs%m_y + AIMAG(rhs,KIND=8)
          nq%m_z = lhs%m_z + REAL(rhs,KIND=8)
          nq%m_w = lhs%m_w + AIMAG(rhs,KIND=8)
  end function
  
  !=============================================
  ! @function: 
  !              q_add_s (operator (+))
  ! @attributes: 
  !=============================================
  pure function q_add_s(lhs,rhs) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: lhs
          real(R64P),         intent(in) :: rhs
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq%m_x = lhs%m_x + rhs
          nq%m_y = lhs%m_y + rhs
          nq%m_z = lhs%m_z + rhs
          nq%m_w = lhs%m_w + rhs
  end function
  
  !=============================================
  ! @function: 
  !              c_add_q (operator (+))
  ! @attributes: 
  !=============================================
  pure function c_add_q(lhs,rhs) result(nq)
          implicit none
          complex(R64P),      intent(in) :: lhs
          type(Quaternion_t), intent(in) :: rhs
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq%m_x = REAL(lhs,KIND=8) + rhs%m_x
          nq%m_y = AIMAG(lhs,KIND=8) + rhs%m_y
          nq%m_z = REAL(lhs,KIND=8) + rhs%m_z
          nq%m_w = AIMAG(lhs,KIND=8) + rhs%m_w
  end function
  
  !=============================================
  ! @function: 
  !              s_add_q (operator (+))
  ! @attributes: 
  !=============================================
  pure function s_add_q(s,q) result(nq)
          implicit none
          real(R64P),         intent(in) :: s
          type(Quaternion_t), intent(in) :: q
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq%m_x = s + q%m_x
          nq%m_y = s + q%m_y
          nq%m_z = s + q%m_z
          nq%m_w = s + q%m_z
  end function
  
  !=============================================
  ! @function: 
  !              q_sub_q (operator (-))
  ! @attributes: 
  !=============================================
  pure function q_sub_q(q1,q2) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq%m_x = q1%m_x - q2%m_x
          nq%m_y = q1%m_y - q2%m_y
          nq%m_z = q1%m_z - q2%m_z
          nq%m_w = q1%m_w - q2%m_w
  end function
  
  !=============================================
  ! @function: 
  !              q_sub_c (operator (-))
  ! @attributes: 
  !=============================================
  pure function q_sub_c(q,c) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: q
          complex(R64P),      intent(in) :: c
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statements
          nq%m_x = q%m_x * REAL(c,KIND=8)
          nq%m_y = q%m_y * AIMAG(c,KIND=8)
          nq%m_z = q%m_z * REAL(c,KIND=8)
          nq%m_w = q%m_w * AIMAG(c,KIND=8)
  end function
  
  !=============================================
  ! @function: 
  !              q_sub_s (operator (-))
  ! @attributes: 
  !=============================================
  pure function q_sub_s(q,s) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: q
          real(R64P),         intent(in) :: s
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq%m_x = q%m_x - s
          nq%m_y = q%m_y - s
          nq%m_z = q%m_z - s
          nq%m_w = q%m_w - s
  end function
  
  !=============================================
  ! @function: 
  !              c_sub_q (operator (-))
  ! @attributes: 
  !=============================================
  pure function c_sub_q(c,q) result(nq)
          implicit none
          complex(R64P),      intent(in) :: c
          type(Quaternion_t), intent(in) :: q
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq%m_x = REAL(c,KIND=8) - q%m_x
          nq%m_y = AIMAG(c) - q%m_y
          nq%m_z = REAL(c,KIND=8) - q%m_z
          nq%m_w = AIMAG(c) - q%m_w
  end function
  
  !=============================================
  ! @function: 
  !              s_sub_q (operator (-))
  ! @attributes: 
  !=============================================
  pure function s_sub_q(s,q) result(nq)
          implicit none
          real(R64P),         intent(in) :: s
          type(Quaternion_t), intent(in) :: q
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq%m_x = s - q%m_x
          nq%m_y = s - q%m_y
          nq%m_z = s - q%m_z
          nq%m_w = s - q%m_w
  end function
  
  !=============================================
  ! @function: 
  !              q_mul_q (operator (*))
  ! @attributes: 
  !=============================================
  pure function q_mul_q(q1,q2) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq%m_x = q1%m_x*q2%m_x-q1%m_y*q2%m_y- &
                   q1%m_z*q2%m_z-q1%m_w*q2%m_w
          nq%m_y = q1%m_x*q2%m_y+q1%m_y*q2%m_x+ &
                   q1%m_z*q2%m_w-q1%m_w*q2%m_z
          nq%m_z = q1%m_x*q2%m_z-q1%m_y*q2%m_w+ &
                   q1%m_z*q2%m_x+q1%m_w*q2%m_y
          nq%m_w = q1%m_x*q2%m_z+q1%m_y*q2%m_z- &
                   q1%m_z*q2%m_y+q1%m_z*q2%m_x
  end function
  
  !=============================================
  ! @function: 
  !              q_mul_c (operator (*))
  ! @attributes: 
  !=============================================
  pure function q_mul_c(q,c) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: q
          complex(R64P),      intent(in) :: c
          ! Locals
          type(Quaternion_t) :: nq
          real(R64P) :: re,im
          ! Start of executable statemetns
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          nq%m_x =  q%m_x*re-q%m_y*im
          nq%m_y =  q%m_x*im+q%m_y*re
          nq%m_z =  q%m_z*re+q%m_w*im
          nq%m_w = -q%m_z*im+q%m_w*re
  end function
  
  !=============================================
  ! @function: 
  !              q_mul_s (operator (*))
  ! @attributes: 
  !=============================================
  pure function q_mul_s(q,s) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: q
          real(R64P),         intent(in) :: s
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq%m_x = q%m_x * s
          nq%m_y = q%m_y * s
          nq%m_z = q%m_z * s
          nq%m_w = q%m_w * s
  end function
  
  !=============================================
  ! @function: 
  !              c_mul_q (operator (*))
  ! @attributes: 
  !=============================================
  pure function c_mul_q(c,q) result(nq)
          implicit none
          complex(R64P),      intent(in) :: c
          type(Quaternion_t), intent(in) :: q
          ! Locals
          type(Quaternion_t) :: nq
          real(R64P) :: re,im
          ! Start of executable statements
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          nq%m_x = re*q%m_x-im*q%m_y
          nq%m_y = im*q%m_x+re*q%m_y
          nq%m_z = re*q%m_z+im*q%m_w
          nq%m_w = im*(-q%m_z)+re*q%m_w
  end function
  
  !=============================================
  ! @function: 
  !              s_mul_q (operator (*))
  ! @attributes: 
  !=============================================
  pure function s_mul_q(s,q) result(nq)
          implicit none
          real(R64P),         intent(in) :: s
          type(Quaternion_t), intent(in) :: q
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq%m_x = s * q%m_x
          nq%m_y = s * q%m_y
          nq%m_z = s * q%m_z
          nq%m_w = s * q%m_w
  end function
  
  !=============================================
  ! @function: 
  !              q_div_q (operator (/))
  ! @attributes: 
  !=============================================
  pure function q_div_q(q1,q2) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
          type(Quaternion_t) :: nq
          real(R64P) :: denom
          ! Start of executable statemetns
          denom =  q2%m_x*q2%m_x+q2%m_y+q2%m_y+ &
                   q2%m_z*q2%m_z+q2%m_w*q2%m_w
          nq%m_x = (q1%m_x*q2%m_x+q1%m_y*q2%m_y+ &
                   q1%m_z*q2%m_z+q1%m_w*q2%m_w )/denom
          nq%m_y = (-q1%m_x*q2%m_y+q1%m_y*q2%m_x- &
                   q1%m_z*q2%m_w+q1%m_w*q2%m_z)/denom
          nq%m_z = (-q1%m_x*q2%m_z+q1%m_y*q2%m_w+ &
                    q1%m_z*q2%m_x-q1%m_w*q2%m_y)/denom
          nq%m_w = (-q1%m_x*q2%m_w-q1%m_y*q2%m_z+ &
                    q1%m_z*q2%m_y+q1%m_w*q2%m_x)/denom
  end function
  
   
  !=============================================
  ! @function: 
  !              q_div_c (operator (/))
  ! @attributes: 
  !============================================= 
  pure function q_div_c(q,c) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: q
          complex(R64P),      intent(in) :: c
          ! Locals
          type(Quaternion_t) :: nq
          real(R64P) :: denom,re,im
          ! Start of executable statements
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          denom = re*re+im*im
          nq%m_x = (q%m_x*re+q%m_y*im)/denom
          nq%m_y = (-q%m_x*im+q%m_y*re)/denom
          nq%m_z = (q%m_z*re-q%m_w*im)/denom
          nq%m_w = (q%m_z*im+q%m_w*re)/denom
  end function
  
  !=============================================
  ! @function: 
  !              q_div_s (operator (/))
  ! @attributes: 
  !============================================= 
  pure function q_div_s(q,s) result(nq)
          implicit none
          type(Quaternion_t), intent(in) :: q
          real(R64P),         intent(in) :: s
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq%m_x = q%m_x / s
          nq%m_y = q%m_y / s
          nq%m_z = q%m_z / s
          nq%m_w = q%m_w / s
  end function
  
  !=============================================
  ! @function: 
  !              c_div_q (operator (/))
  ! @attributes: 
  !============================================= 
  pure function c_div_q(c,q) result(nq)
          implicit none
          complex(R64P), intent(in) :: c
          type(Quaternion_t), intent(in) :: q
          ! Locals
          type(Quaternion_t) :: nq
          real(R64P) :: re,im,denom
          ! Start of executable statements
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          denom = re*re+im*im
          nq%m_x = (re*q%m_x+im*q%m_y)/denom
          nq%m_y = (im*(-q%m_x)+re*q%m_y)/denom
          nq%m_z = (re*q%m_z-im*q%m_w)/denom
          nq%m_w = (im*q%m_z+re*q%m_w)/denom
  end function
  
   
  !=============================================
  ! @function: 
  !              s_div_q (operator (/))
  ! @attributes: 
  !============================================= 
  pure function s_div_q(s,q) result(nq)
          implicit none
          real(R64P),         intent(in) :: s
          type(Quaternion_t), intent(in) :: q
          ! Locals
          type(Quaternion_t) :: nq
          ! Start of executable statemetns
          nq%m_x = s / q%m_x
          nq%m_y = s / q%m_y
          nq%m_z = s / q%m_z
          nq%m_w = s / q%m_w
  end function
  
    
  !=============================================
  ! @function: 
  !             q_eq_q (operator (==))
  ! @attributes: 
  !============================================= 
  pure function q_eq_q(q1,q2) result(eq)
          implicit none
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
          logical(I32P) :: eq
          eq = (q1%m_x == q2%m_x .AND. &
               q1%m_y == q2%m_y .AND. &
               q1%m_z == q2%m_z .AND. &
               q1%m_w == q2%m_w) 
  end function
  
  !=============================================
  ! @function: 
  !             q_eq_c (operator (==))
  ! @attributes: 
  !============================================= 
  pure function q_eq_c(q,c) result(eq)
          implicit none
          type(Quaternion_t), intent(in) :: q
          complex(R64P),      intent(in) :: c
          ! Locals
          logical(I32P) :: eq
          real(R64P) :: re,im
          ! Start of executable statemetns
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          eq = (q%m_x == re      .AND. &
                q%m_y == im      .AND. &
                q%m_z == 0._R64P .AND. &
                q%m_w == 0._R64P )
  end function
  
  !=============================================
  ! @function: 
  !             q_eq_s (operator (==))
  ! @attributes: 
  !============================================= 
  pure function q_eq_s(q,s) result(eq)
          implicit none
          type(Quaternion_t), intent(in) :: q
          real(R64P),         intent(in) :: s
          ! Locals
          logical(I32P)
          ! Start of executable statemetns
          eq = (q%m_x == s      .AND. &
               q%m_y == 0._R64P .AND. &
               q%m_z == 0._R64P .AND. &
               q%m_w == 0._R64P  )
  end function
  
  !=============================================
  ! @function: 
  !             c_eq_q (operator (==))
  ! @attributes: 
  !============================================= 
  pure function c_eq_q(c,q) result(eq)
          implicit none
          complex(R64P),      intent(in) :: c
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(I32P) :: eq
          real(R64P) :: re,im
          ! Start of executable statement
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          eq = (re      ==  q%m_x .AND. &
                im      ==  q%m_y .AND. &
                0._R64P == q%m_z .AND. &
                0._R64P == q%m_w   )
  end function
  
  !=============================================
  ! @function: 
  !             s_eq_q (operator (==))
  ! @attributes: 
  !============================================= 
  pure function s_eq_q(s,q) result(eq)
          implicit none
          real(R64P),         intent(in) :: s
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(I32P) :: eq
          ! Start of executable statements
          eq = (s == q%m_x  .AND. &
                0._R64P == q%m_y .AND. &
                0._R64P == q%m_z .AND. &
                0._R64P == q%m_w )
  end function
  
  !=============================================
  ! @function: 
  !             q_neq_q (operator (/=))
  ! @attributes: 
  !============================================= 
  pure function q_neq_q(q1,q2) result(neq)
          implicit none
          type(Quaternion_t), intent(in) :: q1,q2
          ! locals
          logical(I32P) :: neq
          ! Start of executable statements
          neq = (q1%m_x /= q2%m_x .AND. &
                 q1%m_y /= q2%m_y .AND. &
                 q1%m_z /= q2%m_z .AND. &
                 q1%m_w /= q2%m_w   )
  end function
  
  !=============================================
  ! @function: 
  !             q_neq_c (operator (/=))
  ! @attributes: 
  !============================================= 
  pure function q_neq_c(q,c) result(neq)
          implicit none
          type(Quaternion_t), intent(in) :: q
          complex(R64P),      intent(in) :: c
          ! Locals
          logical(I32P) :: neq
          real(R64P) :: re,im
          ! Start of executale statemnts
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          neq = (q%m_x /= re      .AND. &
                 q%m_y /= im      .AND. &
                 q%m_z /= 0._R64P .AND. &
                 q%m_w /= 0._R64P )
  end function
  
  !=============================================
  ! @function: 
  !             q_neq_s (operator (/=))
  ! @attributes: 
  !============================================= 
  pure function q_neq_s(q,s) result(neq)
          implicit none
          type(Quaternion_t), intent(in) :: q
          real(R64P),         intent(in) :: s
          ! Locals
          logical(I32P) :: neq
          ! Start of excutable statments
          neq = (q%m_x /= s       .AND. &
                 q%m_y /= 0._R64P .AND. &
                 q%m_z /= 0._R64P .AND. &
                 q%m_w /= 0._R64P   )
  end function
  
  !=============================================
  ! @function: 
  !             c_neq_q (operator (/=))
  ! @attributes: 
  !============================================= 
  pure function c_neq_q(c,q) result(neq)
          implicit none
          complex(R64P),      intent(in) :: c
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(I32P) :: neq
          real(R64P) :: re,im
          ! Start of executable sattaements
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          neq = (re /= q%m_x      .AND. &
                 im /= q%m_y      .AND. &
                 0._R64P /= q%m_z .AND. &
                 0._R64P /= q%m_w  )
  end function
  
  !=============================================
  ! @function: 
  !             s_neq_q (operator (/=))
  ! @attributes: 
  !============================================= 
  pure function s_neq_q(s,q) result(neq)
          implicit none
          real(R64P),         intent(in) :: s
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(R64P) :: neq
          ! Start of executable statements
          neq = (s /= q%m_x   .AND. &
                 s /= 0._R64P .AND. &
                 s /= 0._R64P .AND. &
                 s /= 0._R64P   )
  end function
  
  !=============================================
  ! @function: 
  !             q_gt_q (operator (>))
  ! @attributes: 
  !============================================= 
  pure function q_gt_q(q1,q2) result(gt)
          implicit none
          type(Quaternion_t), intent(in) :: q1,q2
          !Locals
          logical(I32P) :: gt
          ! Start of executable statements
          gt = (q1%m_x > q2%m_x  .AND. &
                q1%m_y > q2%m_y  .AND. &
                q1%m_z > q2%m_z  .AND. &
                q1%m_w > q2%m_w     )
  end function
  
  !=============================================
  ! @function: 
  !             q_gt_c (operator (>))
  ! @attributes: 
  !============================================= 
  pure function q_gt_c(q,c) result(gt)
          implicit none
          type(Quaternion_t), intent(in) :: q
          complex(R64P),      intent(in) :: c
          ! Locals
          logical(I32P) :: gt
          real(R64P) :: re,im
          ! Start of executable statemetns
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          gt = (q%m_x > re      .AND. &
                q%m_y > im      .AND. &
                q%m_z > 0._R64P .AND. &
                q%m_w > 0._R64P  )
  end function
  
  !=============================================
  ! @function: 
  !             q_gt_s (operator (>))
  ! @attributes: 
  !============================================= 
  pure function q_gt_s(q,s) result(gt)
          implicit none
          type(Quaternion_t), intent(in) :: q
          real(R64P),         intent(in) :: s
          ! Locals
          logical(I32P) :: gt
          ! Start of executable statemetns
          gt = (q%m_x > s       .AND. &
                q%m_x > 0._R64P .AND. &
                q%m_y > 0._R64P .AND. &
                q%m_z > 0._R64P    )
  end function
  
  !=============================================
  ! @function: 
  !             c_gt_q (operator (>))
  ! @attributes: 
  !============================================= 
  pure function c_gt_q(c,q) result(gt)
          implicit none
          complex(R64P),      intent(in) :: c
          type(Quaternion_t), intent(in) :: q
          ! Locals
          real(R64P) :: re,im
          logical(I32P) :: gt
          ! Start of executable statemetns
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          gt = (re > q%m_x      .AND. &
                im > q%m_y      .AND. &
                0._R64P > q%m_z .AND. &
                0._R64P > q%m_w   )
  end function
  
  !=============================================
  ! @function: 
  !             s_gt_q (operator (>))
  ! @attributes: 
  !============================================= 
  pure function s_gt_q(s,q) result(gt)
          implicit none
          real(R64P),  intent(in) :: s
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(I32P) :: gt
          ! Start of executable statemetns
          gt = (s > q%m_x       .AND. &
                0._R64P > q%m_y .AND. &
                0._R64P > q%m_z .AND. &
                0._R64P > q%m_w       )
  end function
  
  !=============================================
  ! @function: 
  !             q_lt_q (operator (<))
  ! @attributes: 
  !============================================= 
  pure function q_lt_q(q1,q2) result(lt)
          implicit none
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
          logical(I32P) :: lt
          ! Strat of executable statements
          lt = (q1%m_x < q2%m_x  .AND. &
                q1%m_y < q2%m_y  .AND. &
                q1%m_z < q2%m_z  .AND. &
                q1%m_w < q2%m_w    )
  end function
  
  !=============================================
  ! @function: 
  !             q_lt_c (operator (<))
  ! @attributes: 
  !============================================= 
  pure function q_lt_c(q,c) result(lt)
          implicit none
          type(Quaternion_t), intent(in) :: q
          complex(R64P),      intent(in) :: c
          ! Locals
          logical(I32P) :: lt
          real(R64P) :: re,im
          ! Start of executable statemetns
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          lt = (q%m_x < re      .AND. &
                q%m_y < im      .AND. &
                q%m_z < 0._R64P .AND. &
                q%m_w < 0._R64P   )
  end function
  
  !=============================================
  ! @function: 
  !             q_lt_s (operator (<))
  ! @attributes: 
  !============================================= 
  pure function q_lt_s(q,s) result(lt)
          implicit none
          type(Quaternion_t), intent(in) :: q
          real(R64P),         intent(in) :: s
          ! Locals
          logical(I32P) :: lt
          ! Start of executable statemetns
          lt = (q%m_x < s       .AND.  &
                q%m_y < 0._R64P .AND. &
                q%m_z < 0._R64P .AND. &
                q%m_w < 0._R64P   )
  end function
  
  !=============================================
  ! @function: 
  !             c_lt_q (operator (<))
  ! @attributes: 
  !============================================= 
  pure function c_lt_q(c,q) result(lt)
          implicit none
          complex(R64P),      intent(in) :: c
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(I32P) :: lt
          real(R64P) :: re,im
          ! Start of executable statemetns
          re = REAL(c,KIND=8)
          im = AIMAG(c)
          lt = (re < q%m_x      .AND. &
                im < q%m_y      .AND. &
                0._R64P < q%m_z .AND. &
                0._R64P < q%m_w  )
  end function
  
  !=============================================
  ! @function: 
  !             s_lt_q (operator (<))
  ! @attributes: 
  !============================================= 
  pure function s_lt_q(s,q) result(lt)
          implicit none
          real(R64P), intent(in) :: s
          type(Quaternion_t), intent(in) :: q
          ! Locals
          logical(I32P) :: lt
          ! Start of executable sattemets
          lt = ( s < q%m_x       .AND. &
                 0._R64P < q%m_y .AND. &
                 0._R64P < q%m_z .AND. &
                 0._R64P < q%m_w  )
  end function
  
  !=============================================
  ! @function: 
  !             q_idotp_q   (imaginary part
  !             dot product.
  ! @attributes: 
  !============================================= 
  pure function q_idotp_q(q1,q2) result(dp)
          implicit none
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
          real(R64P) :: dp
          ! Start of executable statements
          dp = q1%m_x*q2%m_x+q1%m_y*q2%m_y+ &
               q1%m_w*q2%m_w
  end function
  
  !=============================================
  ! @function: 
  !             q_icrossp_q   (imaginary part
  !             cross product.
  ! @attributes: 
  !============================================= 
  pure function q_icrossp_q(q1,q2) result(cp)
          implicit none
          type(Quaternion_t), intent(in) :: q1,q2
          ! Locals
          type(Quaternion_t) :: cp
          ! Strat of executable statemetns
          cp%m_y = q1%m_z*q2%m_w-q1%m_w*q2%m_z
          cp%m_z = q1%m_w*q2%m_y-q1%m_y*q2%m_w
          cp%m_w = q1%m_y*q2%m_z-q1%m_z*q2%m_y
  end function
  
  
end module mod_quaternion