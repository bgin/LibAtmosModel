
module  module_concrete_class_pressure

#include "Config.hpp"
 !-----------------------------------------------------------------------------------85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'module_concrete_class_pressure'
 !          
 !          Purpose:
 !                          Concrete Data Types which implements a concept  of
 !                          an atmospheric pressure, which is measured in unit of Pascal (Pa).
 !                          'IAtmPressureField' - abstract class will contains deffered type
 !                          bound procedure and generic operators which later will be overriden
 !                          by subtype implementation.
 !                          Three subtypes will implemented.
 !                          1) Pressure scalar field 1D.
 !                          2) Pressure scalar field 2D.
 !                          3) Pressure scalar field 3D.
 !          History:
 !                      Date: 14-01-2017
 !                      Time: 14:33 GMT+2
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
 !          Copyright notice:
 !
 !                       Adapted from module_ra_goddard.f originally
 !                       part of WRF - physics folder.
 !----------------------------------------------------------------------------------85   


 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

 !==============================33
 !   USE DIRECTIVES
 !==============================33

use module_abstract_class_pressure
      
use module_class_error_check
      
use module_kinds

use module_helper_fields_equality

use IEEE_EXCEPTIONS

implicit none

    ! Module versioning information

    ! File version major
integer(I32P), parameter, public :: module_concrete_class_pressure_major = 1

    ! File version minor
integer(I32P), parameter, public :: module_concrete_class_pressure_minor = 0

    ! File version micro
integer(I32P), parameter, public :: module_concrete_class_pressure_micro = 0

    ! File full version, computed as follows: 1000*version_major + 100*version_minor + 10*version_micro
integer(I32P), parameter, public :: module_concrete_class_pressure_full_version = 1000*module_concrete_class_pressure_major + &
                                   100*module_concrete_class_pressure_minor + 10*module_concrete_class_pressure_micro

    ! File creation date
character*(*) CreateDate
parameter(CreateDate = '01-05-2017 15:48 +00200 (Mon, 5 May 2017 GMT+2)'   )
    
    ! File build date/time , should be set manually after every recompilation
character*(*) BuildDate
parameter(BuildDate = '' )

    ! Module name
character*(*) ModuleName
parameter(ModuleName = 'module_concrete_class_pressure')

    ! Module(file) author info
character*(*) ModuleAuthor
parameter(ModuleAuthor = 'Programmer: Bernard Gingold , contact: beniekg@gmail.com' )

    ! Module short description
character*(*) ModuleDescription
parameter(ModuleDescription = 'Concrete implementation of pure abstract type IAtmPressureField. ')

public :: CreateDate,BuildDate,ModuleName,ModuleAuthor,ModuleDescription
      
private
      
character*(*) file_path
parameter (file_path = 'C:\Users\Bernard\Documents\Visual Studio 2013\Projects\WRF_Goddard_Modification\WRF_Goddard_Modification\module_concrete_class_pressure.f90')

integer(I32P), parameter, private :: num_ctors = 5

real(R64P),    parameter, private :: KiB       = 1024_R64

real(R64P),    parameter, private :: MiB       = KiB * KiB

integer(I32P), parameter, private :: DP_SIZE   = 8

integer(I32P), parameter, private :: INT_SIZE  = 4

integer(I32P), parameter, private :: LONG_SIZE = 8
      
public :: CAtmPressureField1D, &
          CAtmPressureField2D, &
          CAtmPressureField3D      ! Public Constructors
    
!DIR$ DEFINE Declare_Destructor = 0
!DIR$ DEFINE Declare_Destructor_Field2D = 0
!DIR$ DEFINE Declare_Destructor_Field3D = 0
    !================================38
    ! Declaration of type: 
    !       CAtmPressureField1D
    !================================38
type, extends(IAtmPressureField) :: CAtmPressureField1D 
    
     
      
      integer(i64) :: m_ims  ! field1D start index
      
      integer(i64) :: m_ime  ! field1D end index
      
      integer(i64) :: m_ids  ! field1D domain start index
      
      integer(i64) :: m_ide  ! field1D domain end index
      
      integer(i64) :: m_its  ! field1D tile start index
      
      integer(i64) :: m_ite  ! field1D tile end index
      
      integer(i64) :: m_ms   ! mask of field1D start index  , m_ms >= m_ims
      
      integer(i64) :: m_me   ! mask of field1D end index    , m_me <= m_ime

!DIR$ IF (Declare_Destructor .EQ. 1)     
      logical(i32) :: m_is_initialized
!DIR$ ENDIF      
      logical, dimension(num_ctors) :: m_ctor_flags  
      
      character(len=17) :: m_rand_distr_type
    
      !DIR$ ATTRIBUTES ALIGN : 32 :: m_field1D
      real(R64), allocatable, dimension(:) ::  m_field1D  ! Pressure scalar field 1D
      
      !DIR$ ATTRIBUTES ALIGN : 32 :: m_field1D_full_levels
      real(R64), allocatable, dimension(:) ::  m_field1D_full_levels  ! Pressure scalar field1D at full levels
      
      !DIR$ ATTRIBUTES ALIGN : 32 :: m_mask_field
      logical(kind=8), allocatable, dimension(:) :: m_mask_field
      
contains

      !==============================38
      !   Type bound getters.
      !==============================38
      procedure, pass(this), public :: get_ims
      
      procedure, pass(this), public :: get_ime
      
      procedure, pass(this), public :: get_ms
      
      procedure, pass(this), public :: get_me
      
      procedure, pass(this), public :: is_init      => get_is_initialized
      
      procedure, pass(this), public :: get_ctor_flags
      
      procedure, pass(this), public :: rand_distr   => get_rand_distr_type
      
      procedure, pass(this), public :: get_field1D
      
      procedure, pass(this), public :: get_mask_field
      
      procedure, pass(this), public :: get_scalar   => get_scalar_from_field1D
      
      procedure, pass(this), public :: get_subarray => get_subarray_from_field1D
      
      procedure, pass(this), public :: get_logical_from_mask
      
      procedure, pass(this), public :: get_subarray_from_mask
      
      !==============================38
      !  Type bound setters
      !==============================38
      procedure, pass(this), public :: set_from_scal    => set_field1D_from_scalar
      
      procedure, pass(this), public :: set_from_subarr  => set_field1D_from_subarray
      
      procedure, pass(this), public :: set_mask_subarr   => set_mask_from_subarray
      
      !==============================38
      ! Helper procedures
      !==============================38
      procedure, nopass, private :: check_dim
      
      !==============================38
      ! Helper private procedure for
      ! handling allocation errors.
      ! Default action is call to 'STOP'
      !==============================38
      
      procedure, nopass, private :: alloc1d_error_handler
      
!DIR$ IF (Declare_Destructor .EQ. 1)      
      procedure, pass(this), public :: destroy
!DIR$ ENDIF      
      !==============================38
      !  Implementation of derived 
      !  procedures.
      !==============================38
      procedure, pass(this), public :: print_state      =>  print_state_field1D
      
      procedure, pass(this), public :: print_memory     =>  print_memory_field1D
      
      procedure, pass(this), public :: object_type      =>  object_type_field1D
      
      procedure, pass(this), public :: field_dimension  =>  field1D_dimension
      
      procedure, pass(this), public :: field_shape      =>  field1D_shape
      
      procedure, pass(this), public :: is_constructed   =>  is_field1D_constructed
      
      procedure, pass(this), public :: num_field_elems  =>  field1D_num_elems
      
      procedure, pass(this), public :: dirx_elems       =>  field1D_num_dirx_elems   ! only this procedure returns meaningful result.
      
      procedure, pass(this), public :: diry_elems       =>  field1D_num_diry_elems   ! this procedures returns -1 and must not be called.
      
      procedure, pass(this), public :: dirz_elems       =>  field1D_num_dirz_elems   ! this procedures returns -1 and must not be called
      
      procedure, pass(this), public :: max_pressure     =>  field1D_max_press
      
      procedure, pass(this), public :: max_loc_pressure =>  field1D_max_loc_press
      
      procedure, pass(this), public :: min_pressure     =>  field1D_min_press
      
      procedure, pass(this), public :: min_loc_pressure =>  field1D_min_loc_press
      
      !==============================38
      ! Type-bound deferred,
      ! operators implementation.
      !==============================38
      
      procedure, pass(this), public :: copy_assign   => copy_assign_field1D
      
      procedure, pass(this), public :: move_assign   => move_assign_field1D
      
      procedure, pass(this), public :: add           => field1D_add_field1D
      
      procedure, pass(this), public :: subtract      => field1D_sub_field1D
      
      procedure, pass(this), public :: multiply      => field1D_mul_field1D
      
      procedure, pass(this), public :: mul_real      => field1D_mul_real
      
      procedure, pass(this), public :: real_mul      => real_mul_field1D
      
      procedure, pass(this), public :: mul_int       => field1D_mul_int
      
      procedure, pass(this), public :: int_mul       => int_mul_field1D
      
      procedure, pass(this), public :: div           => field1D_div_field1D
      
      procedure, pass(this), public :: div_real      => field1D_div_real
      
      procedure, pass(this), public :: div_int       => field1D_div_int
      
      procedure, pass(this), public :: pow_real      => field1D_pow_real
      
      procedure, pass(this), public :: pow_int       => field1D_pow_int
      
      procedure, pass(this), public :: equal         => field1D_eq_field1D
      
      procedure, pass(this), public :: not_equal     => field1D_neq_field1D
      
      procedure, pass(this), public :: less_than     => field1D_lt_field1D
      
      procedure, pass(this), public :: great_than    => field1D_gt_field1D
      
      procedure, pass(this), public :: less_eq_than  => field1D_le_field1D
      
      procedure, pass(this), public :: great_eq_than => field1D_ge_field1D
    
end  type

     !===============================38
     !  Declaration of derived type:
     !    CAtmPressureField2D
     !===============================38

type, extends(IAtmPressureField) :: CAtmPressureField2D
    
    
    
    
    integer(i64) :: m_ims       !Field2D start index 'i' in memory

    integer(i64) :: m_ime       !Field2D end index 'i'   in memory
    
    integer(i64) :: m_jms       !Field2D start index 'j' in memory
    
    integer(i64) :: m_jme       !Field2D end index 'j'   in memory
    
    integer(i64) :: m_ids       !Field2D start index 'i' in domain
    
    integer(i64) :: m_ide       !Field2D end index 'i'  in domain
    
    integer(i64) :: m_jds       !Field2D start index 'j' in domain
    
    integer(i64) :: m_jde       !Field2D end index 'j' in domain
    
    integer(i64) :: m_its       !Field2D start index 'i' in tile
    
    integer(i64) :: m_ite       !Field2D end index 'i' in tile
    
    integer(i64) :: m_jts       !Field2D start index 'j' in tile
    
    integer(i64) :: m_jte       !Field2D end index 'j' in tile
    
    integer(i64) :: m_m1s       !Field2D mask 1st dim start index
    
    integer(i64) :: m_m1e       !Field2D mask 1st dim end index
    
    integer(i64) :: m_m2s       !Field2D mask 2nd dim start index
    
    integer(i64) :: m_m2e       !Field2D mask 2nd dim end index
    
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
    logical(i32) :: m_is_initialized
!DIR$ ENDIF 
    
    logical, dimension(num_ctors) :: m_ctor_flags
    
    character(len=*) :: m_rand_distr_t
    
    !DIR$ ATTRIBUTES ALIGN : 32 :: m_p2d
    real(R64), allocatable, dimension(:,:) :: m_p2d   ! Pressure field 2D (Pa)
    
    !DIR$ ATTRIBUTES ALIGN : 32 :: m_p8w2d
    real(R64), allocatable, dimension(:,:) :: m_p8w2d  ! Pressure field 2D at full levels (Pa)
    
    !DIR$ ATTRIBUTES ALIGN : 32 :: m_maskp2d
    logical(kind=8), allocatable, dimension(:,:) :: m_maskp2d    ! Pressure field difference 2D mask
    
contains
    
    !================================38
    !   Type-bound getters
    !================================38
    
    procedure, pass(this), public :: get_field2D_ims
    
    procedure, pass(this), public :: get_field2D_ime
    
    procedure, pass(this), public :: get_field2D_jms
    
    procedure, pass(this), public :: get_field2D_jme
    
    procedure, pass(this), public :: get_field2D_ids
    
    procedure, pass(this), public :: get_field2D_ide
    
    procedure, pass(this), public :: get_field2D_jds
    
    procedure, pass(this), public :: get_field2D_jde
    
    procedure, pass(this), public :: get_field2D_its
    
    procedure, pass(this), public :: get_field2D_ite
    
    procedure, pass(this), public :: get_field2D_jts
    
    procedure, pass(this), public :: get_field2D_jte
    
    procedure, pass(this), public :: get_field2D_m1s
    
    procedure, pass(this), public :: get_field2D_m1e
    
    procedure, pass(this), public :: get_field2D_m2s
    
    procedure, pass(this), public :: get_field2D_m2e
    
    procedure, pass(this), public :: get_field2D_init  
    
    procedure, pass(this), public :: get_field2D_ctor_flags
    
    procedure, pass(this), public :: get_rand_distr
    
    procedure, pass(this), public :: get_p2d
    
    procedure, pass(this), public :: get_p8w2d
    
    procedure, pass(this), public :: get_maskp2d
    
    procedure, pass(this), public :: scalar_from_p2d 
    
    procedure, pass(this), public :: scalar_from_p8w2d
    
    procedure, pass(this), public :: scalar_from_maskp2d
    
    procedure, pass(this), public :: subarray_from_p2d 
    
    procedure, pass(this), public :: subarray_from_p8w2d
    
    procedure, pass(this), public :: subarray_from_maskp2d
    
   !=================================38
   !    Type-bound setters
   !=================================38
    
   procedure, pass(this), public :: set_p2d_from_scalar 
   
   procedure, pass(this), public :: set_p8w2d_from_scalar
   
   procedure, pass(this), public :: set_maskp2d_from_scalar
   
   procedure, pass(this), public :: set_p2d_from_subarray 
   
   procedure, pass(this), public :: set_p8w2d_from_subarray
   
   procedure, pass(this), public :: set_maskp2d_from_subarray
   
   !=================================38
   !   Check member arrays dimensions
   !=================================38
   
   procedure, nopass, private :: check_dim2d
   
     
    
    !================================38
    ! Helper procedure for copying
    ! arrays. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    procedure, nopass, private :: omp_copy_arrays2d
    
    !================================38
    ! Helper procedure for adding 
    ! arrays. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_add_arrays2d
    
    !================================38
    ! Helper procedure for subtracting
    ! arrays. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_sub_arrays2d
    
    !================================38
    ! Helper procedure for multiplying
    ! arrays. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_mul_arrays2d
    
    !================================38
    ! Helper procedure for multiplying
    ! arrays by real. Using OMP 
    ! Workshare construct,
    !  which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_real_mul_arrays2d
    
    !================================38
    ! Helper procedure for multiplying
    ! real by arrays. Using OMP 
    ! Workshare construct,
    !  which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_mul_real
    
    !================================38
    ! Helper procedure for multiplying
    ! arrays by integer. Using OMP 
    ! Workshare construct,
    !  which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_int_mul_arrays2d
    
    !================================38
    ! Helper procedure for multiplying
    ! integer by arrays. Using OMP 
    ! Workshare construct,
    !  which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_mul_int
    
    !================================38
    ! Helper procedure for dividing
    ! arrays. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_div_arrays2d
    
    !================================38
    ! Helper procedure for dividing
    ! arrays by real. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_div_real
    
    !================================38
    ! Helper procedure for dividing
    ! arrays by integer.Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_div_int
    
    !================================38
    ! Helper procedure for exponentiation
    ! of arrays by real.Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_pow_real
    
    !================================38
    ! Helper procedure for exponentiation
    ! of arrays by integer.Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_pow_int
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for equality.
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types, operator .==.
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_eq
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for inequality.
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types , operator ./=.
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_neq
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for one being less than
    ! the other i.e. a < b
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types , operator .<.
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_lt
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for one being greater than
    ! the other i.e. a > b
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types , operator .>.
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_gt
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for one being less or equal than
    ! the other i.e. a <= b
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types , operator .=<.
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_le
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for one being greater or equal than
    ! the other i.e. a >= b
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types , operator .>=.
    !================================38
    
    procedure, nopass, private :: omp_arrays2d_ge
    
    !================================38
    ! Helper procedure for checking 
    ! existance of non-allowed very small
    ! values , which will be gradually sliding
    ! towards denormal range.
    ! In case of detection a value of TINY_PRESSURE
    ! will be inserted at specific location.
    ! This version checks 4 arrays.
    !================================38
    
    procedure, nopass, private :: check_arrays2d_denorm
    
    !================================38
    ! Helper procedure for checking 
    ! existance of non-allowed very small
    ! values , which will be gradually sliding
    ! towards denormal range.
    ! In case of detection a value of TINY_PRESSURE
    ! will be inserted at specific location
    ! This version checks 2 arrays.
    !================================38
    
    procedure, nopass, private :: check_arrays2d_denorm2
    
    !================================38
    ! Helper procedure for checking
    ! arrays conformance. This procedure
    ! will call 'STOP' if nonconformance
    ! is detected.
    !================================38
    
    procedure, pass(this), private :: nonconforming_arrays2d
    
    !================================38
    ! Helper procedure for handling
    ! failed allocation error
    ! Default action is call to 'STOP'
    !================================38
    
    procedure, nopass, private  ::  alloc2d_error_handler
    
   
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
   procedure, pass(this), public :: destroy_field2D
!DIR$ ENDIF
   
   !=================================38
   !  Declaration of derived pro-
   !  cedures
   !=================================38
   
   procedure, pass(this), public :: print_state      =>  print_state_field2D   
   
   procedure, pass(this), public :: print_memory     =>  print_memory_field2D
   
   procedure, pass(this), public :: object_type      =>  field2D_object_type
   
   procedure, pass(this), public :: field_dimension  =>  field2D_dimension
   
   procedure, pass(this), public :: field_shape      =>  field2D_shape
   
   procedure, pass(this), public :: is_constructed   =>  is_constructed_field2D
   
   procedure, pass(this), public :: field_elems      =>  field2D_num_elems
   
   procedure, pass(this), public :: dirx_elems       =>  field2D_num_dirx_elems        ! Number of field scalars along x direction
   
   procedure, pass(this), public :: diry_elems       =>  field2D_num_diry_elems        ! Number of field scalars along y direction
   
   procedure, pass(this), public :: dirz_elems       =>  field2D_num_dirz_elems        ! This value is set to -1  (direction 'z' does not exist)
   
   procedure, pass(this), public :: max_pressure     =>  field2D_max_press
   
   procedure, pass(this), public :: max_loc_pressure =>  field2D_max_loc_press
   
   procedure, pass(this), public :: min_pressure     =>  field2D_min_press
   
   procedure, pass(this), public :: min_loc_pressure =>  field2D_min_loc_press
   
   !=================================38
   !  Type-bound deffered operators
   !  declaration
   !=================================38
   
   procedure, pass(this), public :: copy_assign     => copy_assign_field2D
   
   procedure, pass(this), public :: move_assign     => move_assign_field2D
   
   procedure, pass(this), public :: add             => field2D_add_field2D
   
   procedure, pass(this), public :: subtract        => field2D_sub_field2D
   
   procedure, pass(this), public :: multiply        => field2D_mul_field2D
   
   procedure, pass(this), public :: mul_real        => field2D_mul_real
   
   procedure, pass(this), public :: real_mul        => real_mul_field2D
   
   procedure, pass(this), public :: mul_int         => field2D_mul_int
   
   procedure, pass(this), public :: int_mul         => int_mul_field2D
   
   procedure, pass(this), public :: div             => field2D_div_field2D
   
   procedure, pass(this), public :: div_real        => field2D_div_real
   
   procedure, pass(this), public :: div_int         => field2D_div_int
   
   procedure, pass(this), public :: pow_real        => field2D_pow_real
   
   procedure, pass(this), public :: pow_int         => field2D_pow_int
   
   procedure, pass(this), public :: equal           => field2D_eq_field2D
   
   procedure, pass(this), public :: not_equal       => field2D_neq_field2D
   
   procedure, pass(this), public :: less_than       => field2D_lt_field2D
   
   procedure, pass(this), public :: great_than      => field2D_gt_field2D
   
   procedure, pass(this), public :: less_eq_than    => field2D_lte_field2D
   
   procedure, pass(this), public :: great_eq_than   => field2D_gte_field2D
   
end  type

    !================================38
    !   Declaration of derived type:
    !   CAtmPressureField3D
    !================================38

type, extends(IAtmPressureField) :: CAtmPressureField3D
    
    public
    
    integer(I64P) ::  m_ims    ! Field3D start index 'i' in memory
   
    integer(I64P) ::  m_ime    ! Field3D end index 'i' in memory
   
    integer(I64P) ::  m_jms    ! Field3D start index 'j' in memory
   
    integer(I64P) ::  m_jme    ! Field3D end index 'j' in memory
   
    integer(I64P) ::  m_kms    ! Field3D start index 'k' in memory
   
    integer(I64P) ::  m_kme    ! Field3D end index 'k' in memory
   
    integer(I64P) ::  m_ids    ! Field3D start index 'i' in domain
    
    integer(I64P) ::  m_ide    ! Field3D end index 'i' in domain
    
    integer(I64P) ::  m_jds    ! Field3D start index 'j' in domain
    
    integer(I64P) ::  m_jde    ! Field3D end index 'j' in domain
    
    integer(I64P) ::  m_kds    ! Field3D start index 'k' in domain
    
    integer(I64P) ::  m_kde    ! Field3D end index 'k' in domain
    
    integer(I64P) ::  m_its    ! Field3D start index 'i' in tile
    
    integer(I64P) ::  m_ite    ! Field3D end index 'i' in tile
    
    integer(I64P) ::  m_jts    ! Field3D start index 'j' in tile
    
    integer(I64P) ::  m_jte    ! Field3D end index 'j' in tile
    
    integer(I64P) ::  m_kts    ! Field3D start index 'k' in tile
    
    integer(I64P) ::  m_kte    ! Field3D end index 'k' in tile
    
    integer(I64P) ::  m_m1s    ! Field3D logical mask start index in 1st dimension
    
    integer(I64P) ::  m_m1e    ! Field3D logical mask end index in 1st dimension
    
    integer(I64P) ::  m_m2s    ! Field3D logical mask start index in 2nd dimension
    
    integer(I64P) ::  m_m2e    ! Field3D logical mask end index in 2nd dimension
    
    integer(I64P) ::  m_m3s    ! Field3D logical mask start in 3rd dimension
    
    integer(I64P) ::  m_m3e    ! Field3D logical mask end in 3rd dimension
    

    
    logical(I32P), dimension(num_ctors) :: m_ctors_flags
    
    character(len=64) :: m_rand_distr
    
    !DIR$ ATTRIBUTES ALIGN : 32 :: m_p3d
    real(R64P), allocatable, dimension(:,:,:) :: m_p3d         ! Pressure field 3D (Pa)
    
    !DIR$ ATTRIBUTES ALIGN : 32 :: m_p8w3d
    real(R64P), allocatable, dimension(:,:,:) :: m_p8w3d        ! Pressure field 3D (Pa) at full levels
    
    !DIR$ ATTRIBUTES ALIGN : 32 :: m_maskp3d
    logical(I64P), allocatable, dimension(:,:,:) :: m_maskp3d  ! Logical mask field 3D denoting volumes of 
                                                                 ! pressure field3D and pressure field3D (full levels)
!DIR$ IF (Declare_Destructor_Field3D .EQ. 1)
    logical(I32P) ::  m_is_initialized
!DIR$ ENDIF
    contains

    !================================38
    !  Type-bound getters
    !================================38

    procedure, pass(this), public :: get_field3D_ims
    
    procedure, pass(this), public :: get_field3D_ime
    
    procedure, pass(this), public :: get_field3D_jms
    
    procedure, pass(this), public :: get_field3D_jme
    
    procedure, pass(this), public :: get_field3D_kms
    
    procedure, pass(this), public :: get_field3D_kme
    
    procedure, pass(this), public :: get_field3D_ids
    
    procedure, pass(this), public :: get_field3D_ide
    
    procedure, pass(this), public :: get_field3D_jds
    
    procedure, pass(this), public :: get_field3D_jde
    
    procedure, pass(this), public :: get_field3D_kds
    
    procedure, pass(this), public :: get_field3D_kde
    
    procedure, pass(this), public :: get_field3D_its
    
    procedure, pass(this), public :: get_field3D_ite
    
    procedure, pass(this), public :: get_field3D_jts
    
    procedure, pass(this), public :: get_field3D_jte
    
    procedure, pass(this), public :: get_field3D_kts
    
    procedure, pass(this), public :: get_field3D_kte
    
    procedure, pass(this), public :: get_field3D_m1s
    
    procedure, pass(this), public :: get_field3D_m1e
    
    procedure, pass(this), public :: get_field3D_m2s
    
    procedure, pass(this), public :: get_field3D_m2e
    
    procedure, pass(this), public :: get_field3D_m3s
    
    procedure, pass(this), public :: get_field3D_m3e
    
    procedure, pass(this), public :: get_field3D_init   
    
    procedure, pass(this), public :: get_field3D_ctor_flags
    
    procedure, pass(this), public :: get_field3D_rand_distr    
    
    procedure, pass(this), public :: get_p3d
    
    procedure, pass(this), public :: get_p8w3d
    
    procedure, pass(this), public :: get_maskp3d
    
    procedure, pass(this), public :: scalar_from_p3d
    
    procedure, pass(this), public :: scalar_from_p8w3d
    
    procedure, pass(this), public :: scalar_from_maskp3d
    
    procedure, pass(this), public :: subarray_from_p3d
    
    procedure, pass(this), public :: subarray_from_p8w3d
    
    procedure, pass(this), public :: subarrar_from_maskp3d
    
    !================================38
    ! Read/Write procedures
    !================================38
    
    !================================38
    !   Type-bound setters declaration
    !================================38
    
    procedure, pass(this), public ::  set_p3d_from_scalar 
    
    procedure, pass(this), public ::  set_p8w3d_from_scalar 
    
    procedure, pass(this), public ::  set_maskp3d_from_scalar
    
    procedure, pass(this), public ::  set_p3d_from_subarray 
    
    procedure, pass(this), public ::  set_p8w3d_from_subarray
    
    procedure, pass(this), public ::  set_maskp3d_from_subarray
    
    !================================38
    ! Read/Write procedures
    !================================38
    
    procedure, pass(this), public ::  read_press_field3D
    
    procedure, pass(this), public ::  write_press_field3D
    
    !================================38
    !  Check member arrays dimensions
    !================================38
    
    procedure, nopass, private  :: check_dim3d
    
    !================================38
    ! Helper procedure for copying
    ! arrays. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    procedure, nopass, private :: omp_copy_arrays3d
    
    !================================38
    ! Helper procedure for adding 
    ! arrays. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_add_arrays3d
    
    !================================38
    ! Helper procedure for subtracting
    ! arrays. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_sub_arrays3d
    
    !================================38
    ! Helper procedure for multiplying
    ! arrays. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_mul_arrays3d
    
    !================================38
    ! Helper procedure for multiplying
    ! arrays by real. Using OMP 
    ! Workshare construct,
    !  which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_real_mul_arrays3d
    
    !================================38
    ! Helper procedure for multiplying
    ! real by arrays. Using OMP 
    ! Workshare construct,
    !  which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_arrays3d_mul_real
    
    !================================38
    ! Helper procedure for multiplying
    ! arrays by integer. Using OMP 
    ! Workshare construct,
    !  which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_int_mul_arrays3d
    
    !================================38
    ! Helper procedure for multiplying
    ! integer by arrays. Using OMP 
    ! Workshare construct,
    !  which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_arrays3d_mul_int
    
    !================================38
    ! Helper procedure for dividing
    ! arrays. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_div_arrays3d
    
    !================================38
    ! Helper procedure for dividing
    ! arrays by real. Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_arrays3d_div_real
    
    !================================38
    ! Helper procedure for dividing
    ! arrays by integer.Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types
    !================================38
    
    procedure, nopass, private :: omp_arrays3d_div_int
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for equality.
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types, operator .==.
    !================================38
    
    procedure, nopass, private :: omp_arrays3d_eq
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for inequality.
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types , operator ./=.
    !================================38
    
    procedure, nopass, private :: omp_arrays3d_neq
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for one being less than
    ! the other i.e. a < b
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types , operator .<.
    !================================38
    
    procedure, nopass, private :: omp_arrays3d_lt
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for one being greater than
    ! the other i.e. a > b
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types , operator .>.
    !================================38
    
    procedure, nopass, private :: omp_arrays3d_gt
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for one being less or equal than
    ! the other i.e. a =< b
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types , operator .=<.
    !================================38
    
    procedure, nopass, private :: omp_arrays3d_lte
    
    !================================38
    ! Helper procedure for comparing
    ! arrays for one being greater or equal than
    ! the other i.e. a >= b
    ! Using OMP Workshare con-
    ! struct, which is used as a work-
    ! around for OpenMP lack of support
    ! of allocatable members of derived
    ! types , operator .>=.
    !================================38
    
    procedure, nopass, private :: omp_arrays3d_gte
    
    !================================38
    ! Helper procedure for checking 
    ! existance of non-allowed very small
    ! values , which will be gradually sliding
    ! towards denormal range.
    ! In case of detection a value of TINY_PRESSURE
    ! will be inserted at specific location
    !================================38
    
    procedure, nopass, private ::  check_arrays3d_denorm
    
    !================================38
    ! Helper procedure for checking
    ! arrays conformance. This procedure
    ! will call 'STOP' if nonconformance
    ! is detected.
    !================================38
    
    procedure, nopass, private ::  nonconforming_arrays3d
    
    !================================38
    ! Helper private procedure for
    ! handling allocation error.
    ! Default action is call to 'STOP'
    !================================38
    
    procedure, nopass, private :: alloc3d_error_handler
    
    
!DIR$ IF (Declare_Destructor_Field3D .EQ. 1)
    procedure, pass(this), public :: destroy_field3D
!DIR$ ENDIF
    
    !================================38
    ! Declaration of deffered type-
    ! bound procedures
    !================================38
    
    procedure, pass(this), public :: print_state      =>  print_state_field3D
    
    procedure, pass(this), public :: print_memory     =>  print_memory_field3D
    
    procedure, pass(this), public :: object_type      =>  field3D_object_type
    
    procedure, pass(this), public :: field_dimension  =>  field3D_dimension
    
    procedure, pass(this), public :: field_shape      =>  field3D_shape
    
    procedure, pass(this), public :: is_constructed   =>  is_field3D_constructed
    
    procedure, pass(this), public :: field_elems      =>  field3D_num_elems
    
    procedure, pass(this), public :: dirx_elems       =>  field3D_num_dirx_elems
    
    procedure, pass(this), public :: diry_elems       =>  field3D_num_diry_elems
    
    procedure, pass(this), public :: dirz_elems       =>  field3D_num_dirz_elems
    
    procedure, pass(this), public :: max_pressure     =>  field3D_max_press
    
    procedure, pass(this), public :: max_loc_pressure =>  field3D_max_loc_press
    
    procedure, pass(this), public :: min_pressure     =>  field3D_min_press
    
    procedure, pass(this), public :: min_loc_pressure =>  field3D_min_loc_press
    
   !=================================38
   !  Type-bound deffered operators
   !  declaration
   !=================================38
    
    procedure, pass(this), public :: copy_assign      =>  copy_assign_field3D
    
    procedure, pass(this), public :: move_assign      =>  move_assign_field3D
    
    procedure, pass(this), public :: add              =>  field3D_add_field3D
    
    procedure, pass(this), public :: subtract         =>  field3D_sub_field3D
    
    procedure, pass(this), public :: multiply         =>  field3D_mul_field3D
    
    procedure, pass(this), public :: mul_real         =>  field3D_mul_real
    
    procedure, pass(this), public :: real_mul         =>  real_mul_field3D
    
    procedure, pass(this), public :: mul_int          =>  field3D_mul_int
    
    procedure, pass(this), public :: int_mul          =>  int_mul_field3D
    
    procedure, pass(this), public :: div              =>  field3D_div_field3D
    
    procedure, pass(this), public :: div_real         =>  field3D_div_real
    
    procedure, pass(this), public :: div_int          =>  field3D_div_int
    
    procedure, pass(this), public :: pow_real         =>  field3D_pow_real
    
    procedure, pass(this), public :: pow_int          =>  field3D_pow_int
    
    procedure, pass(this), public :: equal            =>  field3D_eq_field3D
    
    procedure, pass(this), public :: not_equal        =>  field3D_neq_field3D
    
    procedure, pass(this), public :: less_than        =>  field3D_lt_field3D
    
    procedure, pass(this), public :: great_than       =>  field3D_gt_field3D
    
    procedure, pass(this), public :: less_eq_than     =>  field3D_lte_field3D
    
    procedure, pass(this), public :: great_eq_than    =>  field3D_gte_field3D
    
    
end  type

interface  CAtmPressureField1D

           procedure  def_ctor
           
           procedure  scalar_ctor
           
           procedure  field_ctor
           
           procedure  copy_ctor

end  interface

interface  CAtmPressureField2D

           procedure  def_ctor_field2D
           
           procedure  scalar_ctor_field2D
           
           procedure  field_ctor_field2D

           procedure  copy_ctor_field2D
           
           procedure  move_ctor_field2D     ! Simulated move-construction


end  interface

interface   CAtmPressureField3D

           procedure  def_ctor_field3D

           procedure  scalar_ctor_field3D
           
           procedure  field_ctor_field3D
           
           procedure  copy_ctor_field3D

           procedure  move_ctor_field3D     ! Simulated move-construction
           
end  interface




     !===============================38
     !  Implementation
     !===============================38

contains
    
     !===============================38
     !  Implementation of def_ctor
     !  \memberof CAtmPressureField1D
     !  \brief default (zero) initialization of this.
     !===============================38
    
     type(CAtmPressureField1D)  function  def_ctor(ims,ime,ids,ide,its,ite,ms,me)
     
          implicit none
          integer(i64), intent(in) :: ims
          integer(i64), intent(in) :: ime
          integer(i64), intent(in) :: ids
          integer(i64), intent(in) :: ide
          integer(i64), intent(in) :: its
          integer(i64), intent(in) :: ite
          integer(i64), intent(in) :: ms
          integer(i64), intent(in) :: me
          ! Local variables
          integer(i32)       :: ERR_ALLOC
          character(len=256) :: EMSG
          
          ! Start of executable statements:
        
          if(debug_flag)  call print_prologue('At prologue of function: CAtmPressureField1D%def_ctor')
         
          if((ime .EQ. 0) .OR. (me .GT. ime) .OR. (ms .LT. ims)) then
               call check_dim(ims,ime,ms,me,'CAtmPressureField1D%def_ctor',814)                                                 
          end if
          
          def_ctor%m_ims = ims
          def_ctor%m_ime = ime
          def_ctor%m_ids = ids
          def_ctor%m_ide = ide
          def_ctor%m_its = its
          def_ctor%m_ite = ite
          def_ctor%m_ms  = ms
          def_ctor%m_me  = me
          
          
           associate(ls => def_ctor%m_ims, le => def_ctor%m_ime)
                     
               
              allocate(def_ctor%m_field1D(ls:le), def_ctor%field1D_full_levels(ls:le), &
                       def_ctor%m_mask_field(ls:le), STAT=ERR_ALLOC,ERRMSG=ERMS)
          
          end associate
          
          if(ERR_ALLOC /= 0) then
              call alloc1d_error_handler(ERR_ALLOC,EMSG,file_path,"def_ctor",1359)
          end if    
         
          
          def_ctor%m_ctor_flags(1) = .true.  ; def_ctor%m_ctor_flags(2) = .false.
          def_ctor%m_ctor_flags(3) = .false. ; def_ctor%m_ctor_flags(4) = .false.
          def_ctor%m_rand_distr_type = 'NONE'
          def_ctor%m_field1D = ZeroInit
          def_ctor%m_field1D_full_levels = ZeroInit
          def_ctor%m_mask_field = .false.
!DIR$ IF (Declare_Destructor .EQ. 1)
          def_ctor%m_is_initialized = .true.
!DIR$ ENDIF
          if(debug_flag)  call print_epilogue('At epilogue of function: CAtmPressureField1D%def_ctor')
          
    end  function
    
     !=============================================52
     !  Implementation of scalar_ctor
     !  \memberof CAtmPressureField1D
     !  \brief single scalar (real) 
     !        initialization of CAtmPressureField1D
     !=============================================52
    
     type(CAtmPressureField1D)  function  scalar_ctor(ims,ime,ids,ide,its,ite, & 
                                                      ms,me,rnd_type,val,w,hp_flag)
     
          implicit none
          integer(i64), intent(in)      :: ims
          integer(i64), intent(in)      :: ime
          integer(i64), intent(in)      :: ids
          integer(i64), intent(in)      :: ide
          integer(i64), intent(in)      :: its
          integer(i64), intent(in)      :: ite
          integer(i64), intent(in)      :: ms
          integer(i64), intent(in)      :: me
          character(len=*), intent(in)  :: rnd_type
          real(r64),    intent(in)      :: val
          real(r64),    intent(in)      :: w
          logical,      intent(in)      :: hp_flag
          ! Local variables
          integer(i32) :: ERR_ALLOC
          character(len=*) :: EMSG
          ! Start of executable statements:
          
          if(debug_flag)  call print_prologue('At prologue of function: CAtmPressureField1D%scalar_ctor')
          
          if((ime .EQ. 0) .OR. (ms .LT. ims) .OR. (me .GT. ime)) then
              
               call check_dim(ims,ime,ms,me,'CAtmPressureField1D%scalar_ctor',882) 
          end if
    
          scalar_ctor%m_ims = ims
          scalar_ctor%m_ime = ime
          scalar_ctor%m_ids = ids
          scalar_ctor%m_ide = ide
          scalar_ctor%m_its = its
          scalar_ctor%m_ite = ite
          scalar_ctor%m_ms  = ms
          scalar_ctor%m_me  = me
          
          associate(ls => scalar_ctor%m_ims, le => scalar_ctor%m_ime)
          
          allocate(scalar_ctor%m_field1D(ls:le), scalar_ctor%m_field1D_full_levels(ls:le), &
                   scalar_ctor%m_mask_field(ls:le), STAT=ERR_ALLOC,ERRMSG=EMSG)
          
          end associate
          
          if(ERR_ALLOC /= 0) then
             call alloc1d_error_handler(ERR_ALLOC,EMSG,file_path,"scalar_ctor",1428) 
          end if    
         
    
          scalar_ctor%m_ctor_flags(1) = .false. ; scalar_ctor%m_ctor_flags(2) = .true.
          scalar_ctor%m_ctor_flags(3) = .false. ; scalar_ctor%m_ctor_flags(3) = .false.
          scalar_ctor%m_rand_distr_type = rnd_type
          scalar_ctor%m_field1D(:) = val
          scalar_ctor%m_field1D_full_levels(:) = w
          scalar_ctor%m_mask_field(scalar_ctor%m_ms:scalar_ctor%m_me) = hp_flag   ! Pressure at full level(w) = logical 1, pressure = logical 0
!DIR$ IF (Declare_Destructor .EQ. 1)
          scalar_ctor%m_is_initialized = .true.
!DIR$ ENDIF
          if(debug_flag)  call print_epilogue('At epilogue of function: CAtmPressureField1D%scalar_ctor')
          
    end  function
    
     !=============================================52
     !  Implementation of field_ctor
     !  \memberof CAtmPressureField1D
     !  \brief single scalar field 1D(array) 
     !        initialization of CAtmPressureField1D
     !=============================================52
     
     type(CAtmPressureField1D)  function  field_ctor(ims,ime,ids,ide,its,ite,ms,me,  &
                                                rnd_type,field1D,field1D_fl,mask_field)
     
          implicit none
          integer(i64),                   intent(in) :: ims  
          integer(i64),                   intent(in) :: ime
          integer(i64),                   intent(in) :: ids
          integer(i64),                   intent(in) :: ide
          integer(i64),                   intent(in) :: its
          integer(i64),                   intent(in) :: ite
          integer(i64),                   intent(in) :: ms
          integer(i64),                   intent(in) :: me
          character(len=32),              intent(in) :: rnd_type
          real(R64),        dimension(:), intent(in) :: field1D
          real(R64),        dimension(:), intent(in) :: field1D_fl
          logical(kind=8),  dimension(:), intent(in) :: mask_field
          ! Local variables
          integer(i32) :: ERR_ALLOC
          character(len=80) :: EMSG
          character(len=80), dimension(6) :: msg = [ '***** FATAL-ERROR ***** in: CAtmPressureField1D%field_ctor', &
                                                     'Non-allocated allocatable array(s) passed!!', &
                                                     '***** ERROR-DETAILS *****'  , &
                                                     'Allocation status: '  , &
                                                     '***** ERROR-DETAILS *****' , &
                                                     'TERMINATING EXECUTION WITH STOP STATEMENT'   ]
          
          ! Start of executable statements:
          
          if(debug_flag)  call print_prologue('At prologue of function: CAtmPressureField%field_ctor')
          
          if((ime .EQ. 0) .OR. (ms .LT. ims) .OR. (me .GT. ime)) then
              
                      call check_dim(ims,ime,ms,me,'CAtmPressureField1D%field_ctor',934)
          end if
    
           ! Check if dummy array args are allocated.
           call check_alloc1DR64_fail(field1D,msg,file_path,976)
           call check_alloc1DL64_fail(mask_field,msg,file_path,977)
           
           
    
          field_ctor%m_ims = ims
          field_ctor%m_ime = ime
          field_ctor%m_ids = ids
          field_ctor%m_ide = ide
          field_ctor%m_its = its
          field_ctor%m_ite = ite
          field_ctor%m_ms  = ms
          field_ctor%m_me  = me
          
          associate(ls => field_ctor%m_ims, le => field_ctor%m_ime)
          
          allocate(field_ctor%m_field1D(ls:le), field_ctor%m_field1D_full_levels(ls:le), &
                   field_ctor%m_mask_field(ls:le),STAT=ERR_ALLOC,ERRMSG=EMSG)
          
          end associate
          
          if(ERR_ALLOC /= 0) then
              call alloc1d_error_handler(ERR_ALLOC,EMSG,file_path,"field_ctor",1510)
          end if    
          
    
          
          field_ctor%m_ctor_flags(1) = .false. ; field_ctor%m_ctor_flags(2) = .false.
          field_ctor%m_ctor_flags(3) = .true.  ; field_ctor%m_ctor_flags(4) = .false.
          field_ctor%m_rand_distr_type = rnd_type
          field_ctor%m_field1D = field1D
          field_ctor%m_field1D_full_levels = field1D_fl
          field_ctor%m_mask_field = mask_field
!DIR$ IF (Declare_Destructor .EQ. 1) 
          field_ctor%m_is_initialized = .true.
!DIR$ ENDIF
          if(debug_flag)  call print_epilogue('At epilogue of function: CAtmPressureField1D%field_ctor')
         
     end  function
    
     type(CAtmPressureField1D)  function  copy_ctor(other)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: other
          ! Local variables
           
          integer(i32) :: ERR_ALLOC
          character(len=80) :: EMSG
          
          if(debug_flag)  call print_prologue('At prologue of function: CAtmPressureField1D%copy_ctor')
          
          copy_ctor%m_ims = other%m_ims
          copy_ctor%m_ime = other%m_ime
          copy_ctor%m_ids = other%m_ids
          copy_ctor%m_ide = other%m_ide
          copy_ctor%m_its = other%m_its
          copy_ctor%m_ite = other%m_ite
          copy_ctor%m_ms  = other%m_ms
          copy_ctor%m_me  = other%m_me
          
          associate (ls => copy_ctor%m_ims, le => copy_ctor%m_ime)
                       
              
                  allocate(copy_ctor%m_field1D(ls:le), copy_ctor%m_field1D_full_levels(ls:le), &
                           copy_ctor%m_mask_field(lx:ly), STAT=ERR_ALLOC,ERRMSG=EMSG)
                                                        
          end associate
    
          if(ERR_ALLOC /= 0) then
              call alloc1d_error_handler(ERR_ALLOC,EMSG,file_path,"copy_ctor",1557)
          end if
    
          copy_ctor%m_ctor_flags(1)   = .false. ; copy_ctor%m_ctor_flags(2) = .false.
          copy_ctor%m_ctor_flags(3)   = .false. ; copy_ctor%m_ctor_flags(4) = .true.
          copy_ctor%m_rand_distr_type = other%m_rand_distr_type
          copy_ctor%m_field1D         = other%m_field1D
          copy_ctor%m_field1D_full_levels = other%m_field1D_full_levels
          copy_ctor%m_mask_field      = other%m_mask_field
!DIR$ IF (Declare_Destructor .EQ. 1)
          copy_ctor%m_is_initialized = .true.
!DIR$ ENDIF
          if(debug_flag)  call print_epilogue('At epilogue of function: CAtmPressureField1D%copy_ctor' )
          
     end  function
    
!DIR$ IF (Declare_Destructor .EQ. 1)
    
     subroutine  destroy(this)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          ! Local variables
          integer(i32) :: ERR_DEALLOC
          character(len=*) :: EMSG
          ! Start of executable statements.
          
          if(debug_flag) call print_prologue('At prologue of subroutine: CAtmPressureField1D%destroy')
          
          if(.NOT. this%m_is_initialized) then
             print*, 'Attempted to destroy un-initialized object!!'
             print*, 'Status of this%m_is_initialized: ', this%m_is_initialized
             print*, 'Executing early exit!!'
             return
          end if
          
          this%m_ims = 0
          this%m_ime = 0
          this%m_ids = 0
          this%m_ide = 0
          this%m_its = 0
          this%m_ite = 0
          this%m_ms  = 0
          this%m_me  = 0
          this%m_ctor_flags(1) = .false. ; this%m_ctor_flags(2) = .false.
          this%m_ctor_flags(3) = .false. ; this%m_ctor_flags(4) = .false.
          this%m_rand_distr_type = ''
          if(allocated(this%m_field1D) .AND. allocated(this%m_mask_field) & 
             .AND. allocated(this%m_field1D_full_levels)) then
               deallocate(this%m_field1D,this%m_mask_field this%m_field1D_full_levels,STAT=ERR_DEALLOC,ERRMSG=EMSG)
               if(ERR_DEALLOC /= 0) then
                   print*, 'FATAL-ERROR in function: CAtmPressureField1D%destroy'
                   print*, 'Memory De-Allocation Failure!!.'
                   print*, 'ERR_ALLOC = ', ERR_DEALLOC
                   print*,  EMSG 
                   print*, 'Executing early exit!!'
                   return
               end if
               
          end if
          this%m_is_initialized = .false.
          if(debug_flag)  call print_epilogue('At epilogue of subroutine: CAtmPressureField1D%destroy' )
          
    end  subroutine
    
    
!DIR$ ENDIF

#if USE_INLINING == 0x1
       !DIR$ ATTRIBUTES INLINE :: get_ims
#endif
     pure  function  get_ims(this)  result(ret_ims)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          ! Local variables
          integer(i64) :: ret_ims
          ! Start executable statements
     
          ret_ims = this%m_ims
          
     end  function
    
#if USE_INLINING == 0x1
       !DIR$ ATTRIBUTES INLINE :: get_ime
#endif

     pure  function  get_ime(this)   result(ret_ime)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          ! Local variables
          integer(i64) :: ret_ime
          ! Start of executable statements
          
          ret_ime = this%m_ime
     
     end  function
    
#if USE_INLINING == 0x1
       !DIR$ ATTRIBUTES INLINE :: get_ms
#endif

     pure  function  get_ms(this)   result(ret_ms)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          ! Local variables
          integer(i64) :: ret_ms
          
          ret_ms = this%m_ms
     
     end  function
    
#if USE_INLINING == 0x1
       !DIR$ ATTRIBUTES INLINE :: get_me
#endif

     pure  function  get_me(this)   result(ret_me)
      
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          ! Local variables
          integer(i64) :: ret_me
          ! Start of executable statements
          
          ret_me = this%m_me
          
     end  function
    
#if  USE_INLINING == 0x1
        !DIR$ ATTRIBUTES INLINE :: get_is_initialized
#endif
     pure  function  get_is_initialized(this)  result(ret_is_init)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          ! Local variables
          logical(i32) :: ret_is_init
          ! Start of executable statements
          
          ret_is_init = this%m_is_initialized
          
     end  function
    
#if  USE_INLINING == 0x1
        !DIR$ ATTRIBUTES INLINE :: get_ctor_flags
#endif

     pure  function  get_ctor_flags(this)  result(ret_ctor_flags)
       
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          ! Local variables
          logical(i32), dimension(num_ctors) :: ret_ctor_flags
          ! Start of executable statements
          
          ret_ctor_flags = this%m_ctor_flags
          
     end  function
    
#if  USE_INLINING == 0x1
        !DIR$ ATTRIBUTES INLINE :: get_rand_distr_type
#endif
    
     pure  function  get_rand_distr_type(this)  result(ret_rnd_type)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          ! Local variables
          character(len=*) :: ret_rnd_type
          ! Start of executable statements
          
          ret_rnd_type = this%m_rand_distr_type
          
     end  function
    
#if  USE_INLINING == 0x1
        !DIR$ ATTRIBUTES INLINE :: get_field1D
#endif

     pure  function  get_field1D(this)  result(ret_field1D)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          ! Local variables
          real(r64), allocatable, dimension(:) :: ret_field1D
          ! Start of executable statements
          
          ret_field1D = this%m_field1D
          
     end  function
    
#if  USE_INLINING == 0x1
        !DIR$ ATTRIBUTES INLINE :: get_mask_field
#endif

     pure  function  get_mask_field(this)  result(ret_mask_field)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          ! Local variables
          logical(i32), allocatable, dimension(:) :: ret_mask_field
          ! Start of executables statements
          
          ret_mask_field = this%m_mask_field
          
     end  function
    
#if  USE_INLINING == 0x1
        !DIR$ ATTRIBUTES INLINE :: get_scalar_from_field1D
#endif

     function  get_scalar_from_field1D(this,idx)  result(ret_scalar)
     
          implicit none
          
          class(CAtmPressureField1D), intent(in) :: this
          integer(i64),               intent(in) :: idx
          
          ! Local variables
          real(R64) :: ret_scalar
          ! Start of executable statements
          if((idx .LT. this%m_ims) .OR. (idx .GT. this%m_ime)) then
                 print*, '***** FATAL-ERROR *****'
                 print*, 'Critical Error in function: CAtmPressureField1D%get_scalar_from_field1D'
                 print*, 'Argument: idx - out of boundaries'
                 print*, 'idx = ', idx
                 print*, 'lower range = ', this%m_ims
                 print*, 'upper range = ', this%m_ime
                 ERROR STOP 'TERMINATING PROGRAM - OUT-OF-BOUNDARIES'
          end if
          
          ret_scalar = this%m_field1D(idx)
     
     end  function
    
#if  USE_INLINING == 0x1
        !DIR$  ATTRIBUTES INLINE :: get_subarray_from_field1D
#endif

     function  get_subarray_from_field1D(this,idxs,idxe)   result(ret_subarray)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          integer(i64),               intent(in) :: idxs
          integer(i64),               intent(in) :: idxe
          ! Local variables
          real(i64), allocatable, dimension(:) :: ret_subarray
          ! Start of executable statements
          
          if((idxs .LT. this%m_ims) .OR. (idxe .GT. this%m_ime)) then
              
              print*, '***** FATAL-ERROR *****'
              print*, 'Critical Error in function: CAtmPressureField1D%get_subarray_from_field1D'
              print*, 'Argument: idxs or idxe - out of boundaries'
              print*, 'idxs = ', idxs
              print*, 'idxe = ', idxe
              print*, 'lower range = ', this%m_ims
              print*, 'upper range = ', this%m_ime
              ERROR STOP 'TERMINATING PROGRAM - OUT-OF-BOUNDARIES'
          end if
          
          ret_subarray = this%m_field1D[idxs:idxe]
          
    end  function

#if USE_INLINING == 0x1
       !DIR$ ATTRIBUTES INLINE :: get_logical_from_mask
#endif 
     function  get_logical_from_mask(this,idx)  result(ret_logical)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          integer(i64),               intent(in) :: idx
          ! Local variables
          logical(i32) :: ret_logical
          ! Start of executable statements
          
          if((idx .LT. this%m_ims) .OR. (idx .GT. this%m_ime)) then
              
                print*, '***** FATAL-ERROR *****'
                print*, 'Critical Error in function: CAtmPressureField1D%get_logical_from_mask'
                print*, 'Argument: idx - out of boundaries'
                print*,'idx = ', idx
                print*, 'lower range = ', this%m_ims
                print*, 'upper range = ', this%m_ime
                ERROR STOP 'TERMINATING PROGRAM - OUT-OF-BOUNDARIES'
          end if
          
          ret_logical = this%m_mask_field(idx)
     
     end  function
    
#if USE_INLINING == 0x1
      !DIR$ ATTRIBUTES INLINE :: get_subarray_from_mask
#endif

     function  get_subarray_from_mask(this,idxs,idxe)  result(ret_subarray)
     
          implicit none
          class(CAtmPressureField1D), intent(in) :: this
          integer(i64),               intent(in) :: idxs
          integer(i64),               intent(in) :: idxe
          ! Local variables
          logical(i32), allocatable, dimension(:) :: ret_subarray
          ! Start of executable statements
          
          if((idxs .LT. this%m_ims) .OR. (idxe .GT. this%m_ime)) then
              print*, '***** FATAL-ERROR *****'
              print*, 'Critical Error in function: CAtmPressureField1D%get_subarray_from_mask'
              print*, 'Argument: idxs or idxe - out of boundaries'
              print*, 'idxs = ', idxs
              print*, 'idxe = ', idxe
              print*, 'lower range = ', this%m_ims
              print*, 'upper range = ', this%m_ime
              ERROR STOP 'TERMINATING PROGRAM - OUT-OF-BOUNDARIES'
          end if
          
          ret_subarray = this%m_mask_field[idxs:idxe]
          
     end function
    
#if USE_INLINING == 0x1
       !DIR$ ATTRIBUTES INLINE :: set_field1D_from_scalar
#endif

     subroutine  set_field1D_from_scalar(this,idxs,idxe,val,set_all)
     
          implicit none
          class(CAtmPressureField1D), intent(in)            :: this
          integer(i64),               intent(in)            :: idxs
          integer(i64),               intent(in)            :: idxe
          real(i64),                  intent(in)            :: val
          logical(i32),               intent(in), optional  :: set_all
          ! Start of executable statements
          
          if((idxs .LT. this%m_ims) .OR. (idxe .GT. this%m_ime)) then
              print*, '***** FATAL-ERROR *****'
              print*, 'Critical Error in function: CAtmPressureField1D%set_field1D_from_scalar'
              print*, 'Argument: idxs or idxe - out of boundaries'
              print*, 'idxs = ', idxs
              print*, 'idxe = ', idxe
              print*, 'lower range = ', this%m_ims
              print*, 'upper range = ', this%m_ime
              ERROR STOP 'TERMINATING PROGRAM - OUT-OF-BOUNDARIES'
          end if
          
         if(present(set_all) .AND. (set_all .EQ. .true.)) then
             this%m_field1D(:) = val
         else
             this%m_field1D(idxs:idxe) = val
         end if
         
          
     end  subroutine
    
#if USE_INLINING == 0x1
       !DIR$ ATTRIBUTES INLINE :: 
#endif

     subroutine  set_field1D_from_subarray(this,idxs,idxe,array1D,set_all)
     
          implicit none
          class(CAtmPressureField1D),        intent(in)           :: this
          integer(i64),                      intent(in)           :: idxs
          integer(i64),                      intent(in)           :: idxe
          real(i64), dimension(:),           intent(in)           :: array1D
          logical(i32),                      intent(in), optional :: set_all
          
          if((idxs .LT. this%m_ims) .OR. (idxe .GT. this%m_ime)) then
              print*, '***** FATAL-ERROR *****'
              print*, 'Critical Error in function: CAtmPressureField1D%set_field1D_from_subarray'
              print*, 'Argument: idxs or idxe - out of boundaries'
              print*, 'idxs = ', idxs
              print*, 'idxe = ', idxe
              print*, 'lower range = ', this%m_ims
              print*, 'upper range = ', this%m_ime
              ERROR STOP 'TERMINATING PROGRAM - OUT-OF-BOUNDARIES'
              
          end if
          
          if((present(set_all)) .AND. (set_all .EQ. .true. )) then
              this%m_field1D(:) = array1D(:)
          else
              this%m_field1D(idxs:idxe) = array1D(idxs:idxe)
          endif
          
    end  subroutine
    
#if USE_INLINING == 0x1
       !DIR$ ATTRIBUTES INLINE ::  set_mask_from_subarray
#endif

     subroutine  set_mask_from_subarray(this,idxs,idxe,array1D,set_all)
     
          implicit none
          class(CAtmPressureField1D),   intent(in)           :: this
          integer(i64),                 intent(in)           :: idxs
          integer(i64),                 intent(in)           :: idxe
          logical(i32), dimension(:),   intent(in)           :: array1D
          logical(i32),                 intent(in), optional :: set_all
          ! Start of executable statements
          
          if((idxs .LT. this%m_ims) .OR. (idxe .GT. this%m_ime)) then
              print*, '***** FATAL-ERROR *****'
              print*, 'Critical Error in function: CAtmPressureField1D%set_mask_from_subarray'
              print*, 'Argument: idxs or idxe - out of boundaries'
              print*, 'idxs = ', idxs
              print*, 'idxe = ', idxe
              print*, 'lower range = ', this%m_ims
              print*, 'upper range = ', this%m_ime
          end if
          
          if((present(set_all)) .AND. (set_all .EQ. .true. )) then
              this%m_mask_field(:) = array1D(:)
          else
              this%m_mask_field(idxs:idxe) = array1D(idxs:idxe)
          end if
          
          
     end subroutine
     !===============================38
     !  Implementation of check_dim.
     !  \memberof CAtmPressureField1D
     !  \brief checks field1D and 
     !         mask_field dimension args.
     !         calls STOP upon detecting 
     !         invalid values.     
     !===============================38
      
       subroutine  check_dim(ims,ime,ms,me,msg,line)
     
           integer(i64), intent(in)      :: ims
           integer(i64), intent(in)      :: ime
           integer(i64), intent(in)      :: ms
           integer(i64), intent(in)      :: me
           character(len=*), intent(in) :: msg
           integer(i32),  intent(in)     :: line
           ! Local variables
           integer(i32), parameter :: clock_size = 3
           integer(i32), parameter :: dt_size    = 8
           integer(i64), dimension(dt_size) :: date_time
           character(len=12), dimension(clock_size) :: real_clock
           ! Start of executable statements:
           
           
           
           call DATE_AND_TIME(real_clock(1), real_clock(2), real_clock(3),  &
                        date_time )
          
           print*, '***** FATAL-ERROR *****'
           print*, 'Date of error: ', date_time(1), '-', date_time(2), '-', date_time(3)
           print*, 'UTC-DELTA: ', date_time(4)
           print*, 'Time of error: ', date_time(5), ':', date_time(6), ':', date_time(7), ':', date_time(8)
           print*, 'In procedure: ', msg
           print*, 'At line: ', line
           print*, 'In file: ', file_path
           print*, '***** ERROR-DETAILS *****'
           print*, 'Invalid dimension(s) argument(s)!!'
           print*, 'ime must be > 0 and    is: ', ime
           print*, 'me must be <= ime, and is: ', me
           print*, 'ms must be >= ims, and is: ', ms
           print*, '***** ERROR-DETAILS *****'
           print*, '***** FATAL-ERROR ***** '
           ERROR STOP 'TERMINATING EXECUTION - INVALID ARGUMENT(s)'
           
    end  subroutine
    
    
    subroutine  alloc1d_error_handler(error,msg,file,fname,line)
          implicit none
          use ISO_FORTRAN_ENV, only ERROR_UNIT
          integer(I32),     intent(in) :: error
          character(len=*), intent(in) :: msg
          character(len=*), intent(in) :: file
          character(len=*), intent(in) :: fname
          integer(I32),     intent(in) :: line
          ! Start of executable statements
          
          write(ERROR_UNIT,*) '----------------------------------------------'
          write(ERROR_UNIT,*) ' [FATAL-ERROR]: Memory Allocation Failure!!'
          write(ERROR_UNIT,*) '----------------------------------------------'
          write(ERROR_UNIT,*) ' In file: ', file
          write(ERROR_UNIT,*) ' In function: ', fname
          write(ERROR_UNIT,*) ' At line: ', line
          write(ERROR_UNIT,*) ' STAT = ', error
          write(ERROR_UNIT,*) ' ERRMSG: ', msg
          write(ERROR_UNIT,*) ' Terminating Execution!!'
          ERROR STOP ' FATAL --> Memory Allocation Failure!!'
    
    
    end  subroutine
    
    subroutine  check_dim2d(ime,jme,m1s,m1e,m2s,m2e,msg,line)
          implicit none
          
          integer(i64),     intent(in) :: ime
         
          integer(i64),     intent(in) :: jme
          integer(i64),     intent(in) :: m1s
          integer(i64),     intent(in) :: m1e
          integer(i64),     intent(in) :: m2s
          character(len=*), intent(in) :: msg
          integer(i32),     intent(in) :: line
          ! Local variables
          integer(i32), parameter :: clk_size = 3
          integer(i32), parameter :: dt_size  = 8
          character(len=12), dimension(clk_size) :: rclk
          integer(i64),      dimension(dt_size)  :: dt
          ! Start of executable statements
          call DATE_AND_TIME(rclk(1), rclk(2), rclk(3), dt)
           print*, '***** FATAL-ERROR *****'
           print*, 'Date of error: ', date_time(1), '-', date_time(2), '-', date_time(3)
           print*, 'UTC-DELTA: ', date_time(4)
           print*, 'Time of error: ', date_time(5), ':', date_time(6), ':', date_time(7), ':', date_time(8)
           print*, 'In procedure: ', msg
           print*, 'At line: ', line
           print*, 'In file: ', file_path
           print*, '***** ERROR-DETAILS *****'
           print*, 'm_ime must be > 0 and is: ', ime
           print*, 'm_jme must be > 0 and is: ', jme
           print*, 'm_m1e must be <= m_ime and is:', m1e
           print*, 'm_m2e must be <= m_jme and is:', m2e
           print*, 'm_m1s must be >= m_ims and is:', m1s
           print*, 'm_m2s must be >= m_jme and is:', m2s
           print*, '***** ERROR-DETAILS *****'
           ERROR STOP 'TERMINATING PROGRAM EXECUTION -- INVALID ARGUMENT(S)'
       
       
    end  subroutine
    
    
    subroutine  check_dim3d(ime,jme,kme,m1s,m1e, &
                            m2s,m2e,m3s,m3e,msg,line )
          implicit none
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          integer(I64),         intent(in)       :: ime,jme,kme, &
                                                    m1s,m1e,m2s, &
                                                    m2e,m3s,m3e
          character(len=*),     intent(in)       :: msg
          integer(I32),         intent(in)       :: line
          ! Locals
          integer(I32), parameter                :: clk_size = 3
          integer(I32), parameter                :: dt_size  = 8
          character(len=12), dimension(clk_size) :: clk
          integer(I64),      dimension(dt_size)  :: dt
          ! Start of executable statements
          
          call DATE_AND_TIME(clk(1),clk(2),clk(3),dt)
          write(ERROR_UNIT,*) '             ****[FATAL-ERROR]****                      '
          write(ERROR_UNIT,*) '--------------------------------------------------------'
          write(ERROR_UNIT,*) ' Date of Error: ', dt(1),'-',dt(2),'-',dt(3)
          write(ERROR_UNIT,*) ' UTC-Delta: ', dt(4)
          write(ERROR_UNIT,*) ' Time of Error: ', dt(5),':',dt(6),':',dt(7),':',dt(8)
          write(ERROR_UNIT,*) '--------------------------------------------------------'
          write(ERROR_UNIT,*) ' In file name: ',    file_path
          write(ERROR_UNIT,*) ' In procedure: ',    msg
          write(ERROR_UNIT,*) ' At line of code: ', line
          write(ERROR_UNIT,*) '--------------------------------------------------------'
          write(ERROR_UNIT,*) ' Index ime: ', ime,'Index kme:',kme,'Index jme:',jme
          write(ERROR_UNIT,*) ' Index m1s: ', m1s,'Index m1e:',m1e,'Index m2s:',m2s
          write(ERROR_UNIT,*) ' Index m2e: ', m2e,'Index m3s:',m3s,'Index m3e:',m3e
          write(ERROR_UNIT,*) '--------------------------------------------------------'
          ERROR STOP '[FATAL-ERROR] -- Invalid Indices Terminating Execution!! '
          
    end  subroutine
    
    
     subroutine  print_state_field1D(this) 
        implicit none
        class(CAtmPressureField1D), intent(in)  :: this
        ! Local variables
        integer(i32), parameter :: datetime_size = 8
        integer(i32), parameter :: clock_size = 3
        integer(i64), dimension(datetime_size) :: date_time
        character(len=12), dimension(clock_size) :: real_clock
        ! Start of executable statements
     
        call DATE_AND_TIME(real_clock(1), real_clock(2), real_clock(3), date_time)
        
        print*, '***** CAtmPressureField1D ***** -- STATE DUMP!!'
        print*, 'Date of collection: ', date_time(1), '-', date_time(2), '-', date_time(3)
        print*, 'UTC-DELTA: ', date_time(4)
        print*, 'Time of collection: ', date_time(5), ':', date_time(6), ':', date_time(7), ':', date_time(8)
        print*, 'Scalar members: '
        print*, 'm_ims:            ', this%m_ims
        print*, 'm_ime:            ', this%m_ime
        print*, 'm_ids:            ', this%m_ids
        print*, 'm_ide:            ', this%m_ide
        print*, 'm_its:            ', this%m_its
        print*, 'm_its:            ', this%m_ite
        print*, 'm_ms:             ', this%m_ms
        print*, 'm_me:             ', this%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
        print*, 'm_is_initialized: ', this%m_is_initialized
!DIR$ ENDIF
        print*, 'm_ctor_flags:     ', 'def_ctor:   ', this%m_ctor_flags(1),'scalar_ctor: ',this%m_ctor_flags(2), &
                                      'field_ctor: ', this%m_ctor_flags(3),'copy_ctor:   ',this%m_ctor_flags(3)
        print*, 'm_rand_distr_type: ', this%m_rand_distr_type
        print*, 'Array members: '
        print*, '----------------------------------------'
        print*, ' m_p1d -- pressure field in (Pa)
        print*, '----------------------------------------'
        print*,  this%m_field1D(:)
        print*, '----------------------------------------'
        print*, ' m_p8w1d -- pressure full levels in (Pa)'
        print*, '----------------------------------------'
        print*,  this%m_field1D_full_levels
        print*, '----------------------------------------'
        print*, ' m_mask_field -- boolean value (1,0)    '
        print*, '----------------------------------------'
        print*,   this%m_mask_field
        print*, '***** CAtmPressureField1D -- End of STATE DUMP!!'
        
        
    end  subroutine
    
    
     subroutine  print_memory_field1D(this) 
        implicit none
        use ISO_FORTRAN_ENV , only : OUTPUT_UNIT
        class(CAtmPressureField1D), intent(in) :: this
        ! Local variables
        integer(i32), parameter :: dt_size = 8
        integer(i32), parameter :: rc_size = 3
        integer(i64), dimension(dt_size) :: dt
        character(len=*), dimension(rc_size) :: rc
        
        ! Start of exceutable statements
        
        call DATE_AND_TIME(rc(1),rc(2),rc(3),dt)
        print*, '***** CAtmPressureField1D ***** -- MEMORY-MAP DUMP!!'
        print*, 'Date of collection: ', dt(1), '-', dt(2), '-', dt(3)
        print*, 'UTC-DELTA: ', dt(4)
        print*, 'Time of collection: ', dt(5), ':', dt(6), ':', dt(7), ':', dt(8)
        write(OUTPUT_UNIT,10) LOC(this)
10      format('this at:      ', Z15)
        write(OUTPUT_UNIT,20) LOC(this%m_ims)
20      format('this%m_ims at:', Z15)
        write(OUTPUT_UNIT,30) LOC(this%m_ime)
30      format('this%m_ime at:', Z15)
        write(OUTPUT_UNIT,40) LOC(this%m_ids)
40      format('this%m_ids at:', Z15)
        write(OUTPUT_UNIT,50) LOC(this%m_ide)
50      format('this%m_ide at:', Z15)
        write(OUTPUT_UNIT,60) LOC(this%m_its)
60      format('this%m_its at:', Z15)
        write(OUTPUT_UNIT,70) LOC(this%m_ite)
70      format('this%m_ite at:', Z15)
        write(OUTPUT_UNIT,80) LOC(this%m_ms)
80      format('this%m_ms  at:', Z15)
        write(OUTPUT_UNIT,90) LOC(this%m_me)
90      format('this%m_me  at:', Z15)
!DIR$ IF (Declare_Destructor .EQ. 1)
        write(OUTPUT_UNIT,100) LOC(this%m_is_initialized)
100     format('this%m_is_initialized at:', Z15)
!DIR$ ENDIF
        write(OUTPUT_UNIT,110) LOC(this%m_ctor_flags)
110     format('this%m_ctor_flags at:', Z15)
        write(OUTPUT_UNIT,120) LOC(this%m_rand_distr_type)
120     format('this%m_rand_distr_type at:', Z15)
        write(OUTPUT_UNIT,130) LOC(this%m_field1D)
130     format('this%m_field1D at:', Z15)
        write(OUTPUT_UNIT,140) LOC(this%m_field1D_full_levels)
140     format('this%m_field1D_full_levels at:', Z15)
        write(OUTPUT_UNIT,150) LOC(this%m_mask_field)
150     format('this%m_mask_field at:', Z15)
        write(OUTPUT_UNIT,160) LOC(this%m_mask_field) - LOC(this%m_ims)
160     format('Approx. address span of ', Z15, 1X, 'bytes')
        print*, ' Dumping short memory consumption stats!!'
        write(OUTPUT_UNIT,170) LOC(this%m_field1D(this%m_ime)) - LOC(this%m_field1D(1))
170     format('Address span of this%m_field1D = ', Z15, 1X, 'bytes')
        write(OUTPUT_UNIT,180) LOC(this%m_field1D_full_levels(this%m_ime)) - LOC( this%m_field1D_full_levels(1))
180     format('Address span of this%m_field1D_full_levels = ', Z15, 1X, 'bytes')
        write(OUTPUT_UNIT,190) LOC(this%m_mask_field(this%m_ime)) - LOC(this%m_mask_field(1))
190     format('Address span of this%m_mask_field1D = ', Z15, 1X, 'bytes')
        write(OUTPUT_UNIT,200) REAL(SIZE(this%m_field1D) * DP_SIZE) / KiB
200     format('Size of this%m_field1D = ', F10.6, 1X, 'bytes')
        write(OUTPUT_UNIT,210) REAL(SIZE(this%m_field1D_full_levels) * DP_SIZE) / KiB        
210     format('Size of this%m_field1D = ', F10.6, 1X, 'bytes')
        write(OUTPUT_UNIT,220) REAL(SIZE(this%m_mask_field) * DP_SIZE) / KiB
220     format('Size of this%m_mask_field = ', F10.6, 1X, 'bytes')
        write(OUTPUT_UNIT,230) REAL(64 * LONG_SIZE) / KiB
230     format('Size of 8 indices = ', F10.6, 1X, 'bytes')
        write(OUTPUT_UNIT,240) REAL(4 * INT_SIZE) / KiB
240     format('Size of this%m_ctor_flags = ', F10.6, 1X, 'bytes')
!DIR$ IF (Declare_Destructor .EQ. 1)
        write(OUTPUT_UNIT,250) REAL(INT_SIZE) / KiB
250     format('Size of this%m_is_initialized = ', F10.6, 1X, 'bytes')
!DIR$ ENDIF
        
        print*, '***** CAtmPressureField1D ***** -- MEMORY-MAP DUMP has finished successfully!!' 
        
    end  subroutine
    
    
     subroutine  object_type_field1D(this, base,description,is_derived)
        implicit none
        class(CAtmPressureField1D), intent(in) :: this
        class(IAtmPressureField),   intent(in) :: base
        character(len=*), intent(inout) :: description
        logical,          intent(inout) :: is_derived
        ! Start of exceutable statements.
        
        description = 'CAtmPressureField1D --> IAtmPressureField'
        is_derived =   EXTENDS_TYPE_OF(this,base)
        
    end  subroutine
    
#if USE_INLINING == 0x1
       !DIR$ ATTRIBUTES INLINE ::  field1D_dimension
#endif    
     pure  function  field1D_dimension()   result(ret_val)
        implicit none
        integer(i32), parameter :: arlen = 3
        integer(i32), dimension(arlen) :: ret_val
        ! Start of executable statements
        ret_val(1) = 1; ret_val(2) = 1; ret_val(3) = 1
    end  function
    
     subroutine  field1D_shape(this,sp1d,sp8w1d,smaskp1d) 
        implicit none
        integer(i32), parameter :: arlen = 1
        class(CAtmPressureField1D),     intent(in)    :: this
        integer(i32), dimension(arlen), intent(inout) :: sp1d
        integer(i32), dimension(arlen), intent(inout) :: sp8w1d
        integer(i32), dimension(arlen), intent(inout) :: smaskp1d
        ! Start of executable statements.
        
        sp1d     = SHAPE(this%m_field1D,i32)
        sp8w1d   = SHAPE(this%m_field1D_full_levels,i32)
        smaskp1d = SHAPE(this%m_mask_field,i32)
        
    end  subroutine

!DIR$ IF (Declare_Destructor .EQ. 1)
#if USE_INLINING == 0x1
       !DIR$ ATTRIBUTES INLINE ::  is_field1D_constructed
#endif 
     pure  function  is_field1D_constructed(this)    result(ret_b)
        implicit none
        class(CAtmPressureField1D),  intent(in) :: this
        logical(i32) :: ret_b
        ! Start of executable statements
        
        ret_b = this%m_is_initialized
        
    end  function
!DIR$ ENDIF
    
    
     function  field1D_num_elems(this)   result(ret_ar)
        implicit none
        class(CAtmPressureField1D),  intent(in) :: this
        ! Locals
        integer(i32), parameter :: arlen = 3
        integer(i32), dimension(arlen) :: ret_ar
        ! Start of executable statements
        
        ret_ar(1) = SIZE(this%m_field1D)
        ret_ar(2) = SIZE(this%m_field1D_full_levels)
        ret_ar(3) = SIZE(this%m_mask_field)
     
    end  function
    
    
     function  field1D_num_dirx_elems(this)   result(ret_ar)
        implicit none
        class(CAtmPressureField1D),  intent(in)  :: this
        ! Locals
        integer(i32), parameter                  :: arlen = 9
        integer(i32), dimension(arlen)           :: ret_ar
        ! Start of executable statements
    
        ret_ar(1) = SIZE(this%m_field1D)
        ret_ar(2) = -1 ! No direction(axis) Y
        ret_ar(3) = -1 ! No direction(axis) Z
        ret_ar(4) = SIZE(this%m_field1D_full_levels)
        ret_ar(5) = -1
        ret_ar(6) = -1
        ret_ar(7) = SIZE(this%m_mask_field)
        ret_ar(8) = -1
        ret_ar(9) = -1
        
    end  function
    
    
     pure  function  field1D_num_diry_elems()   result(ret_ar)
        implicit none
        
        ! Locals
        integer(i32), parameter                 :: arlen = 9
        integer(i32), dimension(arlen)          :: ret_ar
        integer(i32)                            :: i
        ! Start of executable statements
        
        ! No direction Y and Z is present,
        ! hence ar array is initialized to -1
        do i = 1, arlen
            ret_ar(i) = -1
        end do
        
     
    end  function
    
    
    pure function  field1D_num_dirz_elems()   result(ret_ar)
        implicit none
    
        ! Locals
        integer(i32), parameter         :: arlen = 9
        integer(i32), dimension(arlen)  :: ret_ar
        integer(i32)                    :: i
        ! Start of executable statements
        
        ! No direction Y and Z is present,
        ! hence ar array is initialized to -1
        do i = 1, arlen
            ret_ar(i) = -1
        end do
        
        
    end  function
    
    
     function  field1D_max_press(this)      result(ret_ar)
        implicit none
        class(CAtmPressureField1D),   intent(in)  :: this
        ! Locals
        integer(i32), parameter                   :: arlen = 2
        real(R64),    dimension(arlen)            :: ret_ar
        ! Start of executable statements
        
        ret_ar(1) = MAXVAL(this%m_field1D)
        ret_ar(2) = MAXVAL(this%m_field1D_full_levels)
        
        
    end  function
    
    
      subroutine  field1D_max_loc_press(this,loc1,loc2)   
        implicit none
        class(CAtmPressureField1D), intent(in)    :: this
        integer(i64), dimension(1), intent(inout) :: loc1
        integer(i64), dimension(1), intent(inout) :: loc2
        ! Start of executable statements
        
        loc1 = MAXLOC(this%m_field1D)
        loc2 = MAXLOC(this%m_field1D_full_levels)
        
    end  subroutine
    
    
    function  field1D_min_press(this)    result(ret_ar)
        implicit none
        class(CAtmPressureField1D),  intent(in)   :: this
        ! Locals
        integer(i32), parameter                   :: arlen = 2
        real(R64),    dimension(arlen)            :: ret_ar
        ! Start of executable statements
        
        ret_ar(1) = MINVAL(this%m_field1D)
        ret_ar(2) = MINVAL(this%m_field1D_full_levels)
        
    end  function
    
    
    subroutine  field1D_min_loc_press(this,loc1,loc2)
        implicit none
        class(CAtmPressureField1D),  intent(in)    :: this
        integer(i64), dimension(1),  intent(inout) :: loc1
        integer(i64), dimension(1),  intent(inout) :: loc2
        ! Start of executable statements
        
        loc1 = MINLOC(this%m_field1D)
        loc2 = MINLOC(this%m_field1D_full_levels)
        
    end  subroutine
    
     !===============================38
     !  Begin implementation of class
     !  CAtmPressureField1D deffered
     !  procedures.
     !===============================38
    
     subroutine  copy_assign_field1D(this,other)
          implicit none
        use ISO_FORTRAN_ENV, only ERROR_UNIT
        class(CAtmPressureField1D),  intent(inout) :: this
        class(IAtmPressureField),    intent(in)    :: other
        ! Start of executable statements
        
        ! Early exit on attempt to self-assign
        if(LOC(this) .EQ. LOC(other)) then
           write(*,*) 'Attempted Self-Assign  -- Detected!!'
           write(ERROR_UNIT,10) LOC(this)
10         format('this at  address: ', Z15)
           write(ERROR_UNIT,20) LOC(other)
20         format('other at address: ', Z15)
           write(*,*) 'Executing Early Return!!'
           return
        end if
        
        select type(other)
            class is(CAtmPressureField1D)
                this%m_ims = other%m_ims
                this%m_ime = other%m_ime
                this%m_ids = other%m_ids
                this%m_ide = other%m_ide
                this%m_its = other%m_its
                this%m_ite = other%m_ite
                this%m_ms  = other%m_ms
                this%m_me  = other%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
                this%m_is_initialized = other%m_is_initialized
!DIR$ ENDIF
                this%m_ctor_flags = other%m_ctor_flags
                this%m_rand_distr_type = other%m_rand_distr_type
                this%m_field1D = other%m_field1D
                this%m_field1D_full_levels = other%m_field1D_full_levels
                this%m_mask_field = other%m_mask_field
            class default
                    ERROR STOP 'copy_assign_field1D: Unsupported class detected!!'
        end  select
            
            
    end  subroutine
    
    
     subroutine  move_assign_field1D(this,other)
        implicit none
        use ISO_FORTRAN_ENV, only : ERROR_UNIT
        class(CAtmPressureField1D),  intent(inout) :: this
        class(IAtmPressureField),    intent(in)    :: other
        ! Start of executable statements
        
        ! Early exit on attempt to self-assign
        if(LOC(this) .EQ. LOC(other)) then
           write(*,*) 'Attempt to Self-Assign -- Detected!!' 
           write(ERROR_UNIT,10) LOC(this)
10         format('this  at address: ', Z15)
           write(ERROR_UNIT,20) LOC(other)
20         format('other at address: ', Z15)
           write(*,*) 'Executing Early Return'
           return 
        end if
        
        select type(other)
              class is(CAtmPressureField1D)
                this%m_ims = other%m_ims
                this%m_ime = other%m_ime
                this%m_ids = other%m_ids
                this%m_ide = other%m_ide
                this%m_its = other%m_its
                this%m_ite = other%m_ite
                this%m_ms  = other%m_ms
                this%m_me  = other%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
                this%m_is_initialized = other%m_is_initialized
!DIR$ ENDIF
                this%m_ctor_flags = other%m_ctor_flags
                this%m_rand_distr_type = other%m_rand_distr_type
                call move_alloc(other%m_field1D,this%m_field1D)
                call move_alloc(other%m_field1D_full_levels,this%m_field1D_full_levels)
                call move_alloc(other%m_mask_field,this%m_mask_field)
               class default
                 ERROR STOP 'move_assign_field1D: Unsupported class detected!!'
        end select
              
                 
        
     
    end  subroutine
    
    
     function  field1D_add_field1D(lhs,rhs)   result(obj)
        implicit none
        use ISO_FORTRAN_ENV, only :  ERROR_UNIT
        class(CAtmPressureField1D), intent(in) :: lhs
        class(IAtmPressureField),   intent(in) :: rhs
        ! Locals
        integer(i64) :: i
        class(IAtmPressureField), allocatable :: obj
        ! Start of executable statements
        
        select type(rhs)
          class is(CAtmPressureField1D)
              associate(lf  => lhs%m_field1D,rf => rhs%m_field1D,lfl => lhs%m_field1D_full_levels, &
                      rfl => rhs%m_field1D_full_levels,lmf => lhs%m_mask_field,rmf => rhs%m_mask_field)
                
                if((SIZE(lf) /= SIZE(rf)) .OR. (SIZE(lfl) /= SIZE(rfl)) .OR. (SIZE(lmf) /= SIZE(rmf))) then
                           write(*,*) '***** FATAL-ERROR *****'
                           write(*,*) 'Array mismatch size detected!!'
                           write(ERROR_UNIT,10) SIZE(lf) - SIZE(rf)
10                         format('Delta(lf,rf)   is:',I24.20, 'elements')
                           write(ERROR_UNIT,20) SIZE(lfl) - SIZE(rfl)
20                         format('Delta(lfl,rfl) is:',I24.20, 'elements')
                           write(ERROR_UNIT,30) SIZE(lmf) - SIZE(rmf)
30                         format('Delta(lmf,rmf) is:',I24.20, 'elememts')
                           ERROR STOP 'field1D_add_field1D: Array(s) Size Mismatch'
                end if
                
              end associate
            class default
                 ERROR STOP 'field1D_add_field1D: Unsupported class detected!!'
        end  select
        
        allocate(CAtmPressureField1D :: obj)
        select type(obj)
                class is(CAtmPressureField1D)
                    select type(rhs)
                    class is(CAtmPressureField1D)
                        obj%m_ims = lhs%m_ims
                        obj%m_ime = lhs%m_ime
                        obj%m_ids = lhs%m_ids
                        obj%m_ide = lhs%m_ide
                        obj%m_its = lhs%m_its
                        obj%m_ite = lhs%m_ite
                        obj%m_ms  = lhs%m_ms
                        obj%m_me  = lhs%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
                        obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
                        obj%m_ctor_flags          = lhs%m_ctor_flags
                        obj%m_rand_distr_type     = lhs%m_rand_distr_type
                        obj%m_field1D             = lhs%m_field1D + rhs%m_field1D
                        obj%m_field1D_full_levels = lhs%m_field1D_full_levels + rhs%m_field1D_full_levels
                        allocate(obj%m_mask_field(SIZE(lhs%m_mask_field)))  ! Must be 'allocated' before the use!!
                        do concurrent(i = obj%m_ims:obj%m_ime)
                            obj%m_mask_field(i) = IOR(lhs%m_mask_field(i),rhs%m_mask_field(i))
                        end do
                     class default 
                            ERROR STOP 'field1D_add_field1D: Unsupported class type'
                    end select
                end select
                
                                
                        
     
    end  function
    
    
     function  field1D_sub_field1D(lhs,rhs)   result(obj)
        implicit none
        use ISO_FORTRAN_ENV, only : ERROR_UNIT
        class(CAtmPressureField1D),  intent(in) :: lhs
        class(IAtmPressureField),    intent(in) :: rhs
        ! Locals
        integer(i64) :: i
        class(IAtmPressureField), allocatable :: obj
        ! Start of executable statements
        
         select type(rhs)
          class is(CAtmPressureField1D)
              associate(lf  => lhs%m_field1D,rf => rhs%m_field1D,lfl => lhs%m_field1D_full_levels, &
                      rfl => rhs%m_field1D_full_levels,lmf => lhs%m_mask_field,rmf => rhs%m_mask_field)
                
                if((SIZE(lf) /= SIZE(rf)) .OR. (SIZE(lfl) /= SIZE(rfl)) .OR. (SIZE(lmf) /= SIZE(rmf))) then
                           write(*,*) '***** FATAL-ERROR *****'
                           write(*,*) 'Array mismatch size detected!!'
                           write(ERROR_UNIT,10) SIZE(lf) - SIZE(rf)
10                         format('Delta(lf,rf)   is:',I24.20, 'elements')
                           write(ERROR_UNIT,20) SIZE(lfl) - SIZE(rfl)
20                         format('Delta(lfl,rfl) is:',I24.20, 'elements')
                           write(ERROR_UNIT,30) SIZE(lmf) - SIZE(rmf)
30                         format('Delta(lmf,rmf) is:',I24.20, 'elememts')
                           ERROR STOP 'field1D_sub_field1D: Array(s) Size Mismatch'
                end if
                
              end associate
            class default
                 ERROR STOP 'field1D_sub_field1D: Unsupported class detected!!'
          end  select
     
          allocate(CAtmPressureField1D :: obj)
          select type (obj)
          class is(CAtmPressureField1D)
              select type (rhs)
              class is (CAtmPressureField1D)
                        obj%m_ims             = lhs%m_ims
                        obj%m_ime             = lhs%m_ime
                        obj%m_ids             = lhs%m_ids
                        obj%m_ide             = lhs%m_ide
                        obj%m_its             = lhs%m_its
                        obj%m_ite             = lhs%m_ite
                        obj%m_ms              = lhs%m_ms
                        obj%m_me              = lhs%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
                        obj%m_is_initialized  = lhs%m_is_initialized
!DIR$ ENDIF
                        obj%m_ctor_flags      = lhs%m_ctor_flags
                        obj%m_rand_distr_type = lhs%m_rand_distr_type 
                        obj%m_field1D      = lhs%m_field1D - rhs%m_field1D
                        obj%m_field1D_full_levels = lhs%m_field1D_full_levels - rhs%m_field1D_full_levels
                        allocate(obj%m_mask_field(SIZE(lhs%m_mask_field)))
                        do concurrent(i = obj%m_ims:obj%m_ime)
                            obj%m_mask_field(i) = IOR(lhs%m_mask_field(i),rhs%m_mask_field(i))
                        end do
                class default
                       ERROR STOP 'field1D_sub_field1D: Unsupported class/object type' 
              end select
          end select
          
     
    end  function
    
    
     function  field1D_mul_field1D(lhs,rhs)     result(obj)
        implicit none
        use ISO_FORTRAN_ENV , only : ERROR_UNIT
        class(CAtmPressureField1D),  intent(in) :: lhs
        class(IAtmPressureField),    intent(in) :: rhs
        ! Locals
        integer(i64) :: i
        class(IAtmPressureField),   allocatable :: obj
        ! Start of executable statements
        
        select type(rhs)
        class is(CAtmPressureField1D)
            associate(lf  => lhs%m_field1D, rf => rhs%m_field1D, lfl => lhs%m_field1D_full_levels, &
                      rfl => rhs%m_field1D_full_levels, lmf => lhs%m_mask_field, rmf => rhs%m_mask_field)
                     if((SIZE(lf)  /= SIZE(rf))  .OR.  &
                        (SIZE(lfl) /= SIZE(rfl)) .OR.  &
                        (SIZE(lmf) /= SIZE(rmf))) then
                           write(*,*) '***** FATAL-ERROR *****'
                           write(*,*) 'Array mismatch size detected!!'
                           write(ERROR_UNIT,10) SIZE(lf) - SIZE(rf)
10                         format('Delta(lf,rf)   is:',I24.20, 'elements')
                           write(ERROR_UNIT,20) SIZE(lfl) - SIZE(rfl)
20                         format('Delta(lfl,rfl) is:',I24.20, 'elements')
                           write(ERROR_UNIT,30) SIZE(lmf) - SIZE(rmf)
30                         format('Delta(lmf,rmf) is:',I24.20, 'elememts')
                           ERROR STOP 'field1D_mul_field1D: Array(s) Size Mismatch'
                end if
                
            end  associate
            class default
                ERROR STOP 'field1D_mul_field1D: Unsupported class/objec type!!'
        end select
        
        allocate(CAtmPressureField1D :: obj)
        select type(obj)
        class is(CAtmPressureField1D)
            select type(rhs)
            class is(CAtmPressureField1D)
                obj%m_ims = lhs%m_ims
                obj%m_ime = lhs%m_ime
                obj%m_ids = lhs%m_ids
                obj%m_ide = lhs%m_ide
                obj%m_its = lhs%m_its
                obj%m_ite = lhs%m_ite
                obj%m_ms  = lhs%m_ms
                obj%m_me  = lhs%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
                obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
                obj%m_ctor_flags = lhs%m_ctor_flags
                obj%m_rand_distr_type = lhs%m_rand_distr_type
                obj%m_field1D = lhs%m_field1D * rhs%m_field1D
                obj%m_field1D_full_levels = lhs%m_field1D_full_levels * rhs%m_field1D_full_levels
                allocate(obj%m_mask_field(SIZE(lhs%m_mask_field)))
                do concurrent(i = obj%m_ims:obj%m_ime)
                    obj%m_mask_field(i) = IOR(lhs%m_mask_field(i),rhs%m_mask_field(i))
                end do
                class default
                    ERROR STOP 'field1D_mul_field1D: Unsupported class/object type!!'
            end select
        end select
        
     
     
    end  function
    
    
     pure  function  field1D_mul_real(lhs,rhs)    result(obj)
        implicit none
        class(CAtmPressureField1D),  intent(in) :: lhs
        real(R64),                   intent(in) :: rhs
        ! Locals
        class(IAtmPressureField), allocatable :: obj
        ! Start of executable statements
        
        allocate(CAtmPressureField1D :: obj)
        select type(obj)
        class is(CAtmPressureField1D)
            obj%m_ims = lhs%m_ims
            obj%m_ime = lhs%m_ime
            obj%m_ids = lhs%m_ids
            obj%m_ide = lhs%m_ide
            obj%m_its = lhs%m_its
            obj%m_ite = lhs%m_ite
            obj%m_ms  = lhs%m_ms
            obj%m_me  = lhs%m_me
!DIR$ IF (Declared_Destructor .EQ. 1)
            obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
            obj%m_ctor_flags = lhs%m_ctor_flags
            obj%m_rand_distr_type = lhs%m_rand_distr_type
            obj%m_field1D = lhs%m_field1D * rhs
            obj%m_field1D_full_levels = lhs%m_field1D_full_levels * rhs
            obj%m_mask_field = lhs%m_mask_field
           
        end select
        
     
    end  function
    
    
     pure  function  real_mul_field1D(lhs,rhs)     result(obj)
        implicit none
        real(R64),                   intent(in) :: lhs
        class(CAtmPressureField1D),  intent(in) :: rhs
        ! Locals
        class(IAtmPressureField), allocatable :: obj
        ! Start of executable statments
        
        allocate(CAtmPressureField1D :: obj)
        select type(obj)
        class is(CAtmPressureField1D)
             obj%m_ims = rhs%m_ims
             obj%m_ime = rhs%m_ime
             obj%m_ids = rhs%m_ids
             obj%m_ide = rhs%m_ide
             obj%m_its = rhs%m_its
             obj%m_ite = rhs%m_ite
             obj%m_ms  = rhs%m_ms
             obj%m_me  = rhs%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
             obj%m_is_initialized = rhs%m_is_initialized
!DIR$ ENDIF
             obj%m_ctor_flags = rhs%m_ctor_flags
             obj%m_rand_distr_type = rhs%m_rand_distr_type
             obj%m_field1D = lhs * rhs%m_field1D
             obj%m_field1D_full_levels = lhs * rhs%m_field1D_full_levels
             obj%m_mask_field = rhs%m_mask_field
        end select
        
     
    end  function
    
    
     pure  function  field1D_mul_int(lhs,rhs)    result(obj)
        implicit none
        class(CAtmPressureField1D),     intent(in) :: lhs
        integer(i32),                   intent(in) :: rhs
        ! Locals
        class(IAtmPressureField), allocatable :: obj
        ! Start of executable statements
     
        allocate(CAtmPressureField1D :: obj)
        select type(obj)
        class is(CAtmPressureField1D)
            obj%m_ims = lhs%m_ims
            obj%m_ime = lhs%m_ime
            obj%m_ids = lhs%m_ids
            obj%m_ide = lhs%m_ide
            obj%m_its = lhs%m_its
            obj%m_ite = lhs%m_ite
            obj%m_ms  = lhs%m_ms
            obj%m_me  = lhs%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
            obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
            obj%m_ctor_flags = lhs%m_ctor_flags
            obj%m_rand_distr_type = lhs%m_rand_distr_type
            obj%m_field1D  = lhs%m_field1D *  DFLOAT(rhs)
            obj%m_field1D_full_levels = lhs%m_field1D_full_levels * DFLOAT(rhs)
            obj%m_mask_field = lhs%m_mask_field
        end select
        
     
    end  function
    
    
     pure  function  int_mul_field1D(lhs,rhs)    result(obj)
        implicit none
        integer(i32),                intent(in)  :: lhs
        class(CAtmPressureField1D),  intent(in)  :: rhs
        ! Locals
        class(IAtmPressureField), allocatable :: obj
        ! Start of executable statements
        
        allocate(CAtmPressureField1D :: obj)
        select type(obj)
        class is(CAtmPressureField1D)
            obj%m_ims = rhs%m_ims
            obj%m_ime = rhs%m_ime
            obj%m_ids = rhs%m_ids
            obj%m_ide = rhs%m_ide
            obj%m_its = rhs%m_its
            obj%m_ite = rhs%m_ite
            obj%m_ms  = rhs%m_ms
            obj%m_me  = rhs%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
            obj%m_is_initialized = rhs%m_is_initialized
!DIR$ ENDIF
            obj%m_ctor_flags = rhs%m_ctor_flags
            obj%m_rand_distr_type = rhs%m_rand_distr_type
            obj%m_field1D = DFLOAT(lhs) * rhs%m_field1D
            obj%m_field1D_full_levels = DFLOAT(lhs) * rhs%m_field1D_full_levels
            obj%m_mask_field = rhs%m_mask_field
        end select
        
     
    end  function
    
    
     function  field1D_div_field1D(lhs,rhs)    result(obj)
        implicit none
        use ISO_FORTRAN_ENV , only : ERROR_UNIT
        class(CAtmPressureField1D),     intent(inout)    :: lhs
        class(IAtmPressureField),       intent(inout) :: rhs
        ! Locals
        integer(i64) :: i
        class(IAtmPressureField), allocatable :: obj
        ! Start of executable statements
        
         select type(rhs)
         class is(CAtmPressureField1D)
            associate(lf  => lhs%m_field1D, rf => rhs%m_field1D, lfl => lhs%m_field1D_full_levels, &
                      rfl => rhs%m_field1D_full_levels, lmf => lhs%m_mask_field, rmf => rhs%m_mask_field)
                     if((SIZE(lf) /= SIZE(rf)) .OR. (SIZE(lfl) /= SIZE(rfl)) .OR. (SIZE(lmf) /= SIZE(rmf))) then
                           write(*,*) '***** FATAL-ERROR *****'
                           write(*,*) 'Array mismatch size detected!!'
                           write(ERROR_UNIT,10) SIZE(lf) - SIZE(rf)
10                         format('Delta(lf,rf)   is:',I24.20, 'elements')
                           write(ERROR_UNIT,20) SIZE(lfl) - SIZE(rfl)
20                         format('Delta(lfl,rfl) is:',I24.20, 'elements')
                           write(ERROR_UNIT,30) SIZE(lmf) - SIZE(rmf)
30                         format('Delta(lmf,rmf) is:',I24.20, 'elememts')
                           ERROR STOP 'field1D_div_field1D: Array(s) Size Mismatch'
                end if
                
            end  associate
                      ! Attempt to scan for a value(s) <= TINY_PRESSURE and
                      ! correct them.
                      ! Hence both dummy objects are modifiable.
                     do concurrent(i = lhs%m_ims:lhs%m_ime)
                        if(ABS(lhs%m_field1D(i)) .LT. TINY_PRESSURE) then
                            lhs%m_field1D(i) = TINY_PRESSURE
                        else if(ABS(rhs%m_field1D(i)) .LT. TINY_PRESSURE) then
                            rhs%m_field1D(i) = TINY_PRESSURE
                        else if(ABS(lhs%m_field1D_full_levels(i)) .LT. TINY_PRESSURE) then
                            lhs%m_field1D_full_levels(i) = TINY_PRESSURE
                        else if(ABS(rhs%m_field1D_full_levels(i)) .LT. TINY_PRESSURE) then
                            rhs%m_field1D_full_levels(i) = TINY_PRESSURE
                        end if
                     end do
                     
                             
            class default
                ERROR STOP 'field1D_div_field1D: Unsupported class/objec type!!'
        end select
        
        allocate(CAtmPressureField1D :: obj)
        select type(obj)
        class is(CAtmPressureField1D)
            select type(rhs)
            class is(CAtmPressureField1D)
                 obj%m_ims = lhs%m_ims
                 obj%m_ime = lhs%m_ime
                 obj%m_ids = lhs%m_ids
                 obj%m_ide = lhs%m_ide
                 obj%m_its = lhs%m_its
                 obj%m_ite = lhs%m_ite
                 obj%m_ms  = lhs%m_ms
                 obj%m_me  = lhs%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
                 obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
                 obj%m_ctor_flags = lhs%m_ctor_flags
                 obj%m_rand_distr_type = lhs%m_rand_distr_type
                 obj%m_field1D = lhs%m_field1D / rhs%m_field1D
                 obj%m_field1D_full_levels = lhs%m_field1D_full_levels / rhs%m_field1D_full_levels
                 allocate(obj%m_mask_field(SIZE(lhs%m_mask_field)))
                 do concurrent(i = obj%m_ims:obj%m_ime)
                     obj%m_mask_field(i) = IOR(lhs%m_mask_field(i),rhs%m_mask_field(i))
                 end do
                 class default
                     ERROR STOP 'field1D_div_field1D: Unsupported class/object type!!'
            end select
        end select
        
                
                
    end  function
    
    
     pure  function  field1D_div_real(lhs,rhs)    result(obj)
        implicit none
        class(CAtmPressureField1D),    intent(inout) :: lhs
        real(R64),                     intent(inout) :: rhs
        ! Locals
        integer(i64)  :: i
        class(IAtmPressureField)                     ::  obj 
        ! Start of executable statements
        
        if(ABS(rhs) .LT. TINY_PRESSURE) then
            rhs = TINY_PRESSURE
        end if
        
        do concurrent(i = lhs%m_ims, lhs%m_ime)
            if(ABS(lhs%m_field1D(i)) .LT. TINY_PRESSURE) then
                lhs%m_field1D(i) = TINY_PRESSURE
            else if(ABS(lhs%m_field1D_full_levels(i)) .LT. TINY_PRESSURE) then
                lhs%m_field1D_full_levels(i) = TINY_PRESSURE
            end if
        end do
        
        allocate(CAtmPressureField1D :: obj)
           select type(obj)
           class is(CAtmPressureField1D)
               obj%m_ims = lhs%m_ims
               obj%m_ime = lhs%m_ime
               obj%m_ids = lhs%m_ids
               obj%m_ide = lhs%m_ide
               obj%m_its = lhs%m_its
               obj%m_ite = lhs%m_ite
               obj%m_ms  = lhs%m_ms
               obj%m_me  = lhs%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
               obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
               obj%m_ctor_flags = lhs%m_ctor_flags
               obj%m_rand_distr_type = lhs%m_rand_distr_type
               obj%m_field1D = lhs%m_field1D / rhs
               obj%m_field1D_full_levels = lhs%m_field1D_full_levels / rhs
               obj%m_mask_field = lhs%m_mask_field
           end select
           
        
     
    end  function
    
    
    pure  function  field1D_div_int(lhs,rhs)    result(obj)
        implicit none
        class(CAtmPressureField1D),     intent(inout) :: lhs
        integer(i32),                   intent(inout) :: rhs
        ! Locals
        integer(i64)                                  :: i
        
        class(IAtmPressureField), allocatable         :: obj
        ! Start of executable statements
        
        if(rhs .LE. 0) then
            rhs = 1
        end if
        
        do concurrent(i = lhs%m_ims:lhs%m_ime)
            if(ABS(lhs%m_field1D(i)) .LT. TINY_PRESSURE) then
                lhs%m_field1D(i) = TINY_PRESSURE
            else if(ABS(lhs%m_field1D_full_levels(i)) .LT. TINY_PRESSURE) then
                lhs%m_field1D_full_levels(i) = TINY_PRESSURE
            end if
        end do
        
        allocate(CAtmPressureField1D :: obj)
        select type(obj)
        class is(CAtmPressureField1D)
            obj%m_ims = lhs%m_ims
            obj%m_ime = lhs%m_ime
            obj%m_ids = lhs%m_ids
            obj%m_ide = lhs%m_ide
            obj%m_its = lhs%m_its
            obj%m_ite = lhs%m_ite
            obj%m_ms  = lhs%m_ms
            obj%m_me  = lhs%m_me
!DIR$ IF(Declare_Destructor .EQ. 1) 
            obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
            obj%m_ctor_flags = lhs%m_ctor_flags
            obj%m_rand_distr_type = lhs%m_rand_distr_type
            obj%m_field1D = lhs%m_field1D / DFLOAT(rhs)
            obj%m_field1D_full_levels = lhs%m_field1D_full_levels / DFLOAT(rhs)
            obj%m_mask_field = lhs%m_mask_field
        end select
        
            
    end  function
    
    
    pure  function  field1D_pow_real(lhs,rhs)    result(obj)
        implicit none
        class(CAtmPressureField1D),      intent(inout) :: lhs
        real(R64),                       intent(in)    :: rhs
        ! Locals
        integer(i64)                                   :: i
        class(IAtmPressureField), allocatable          ::  obj
        ! Start of execution statements
        
        ! Do not allow values less than TINY_PRESSURE threshold
        do concurrent(i = lhs%m_ims:lhs%m_ime
            if(ABS(lhs%m_field1D(i)) .LT. TINY_PRESSURE) then
                lhs%m_field1D(i) = TINY_PRESSURE
            else if(ABS(lhs%m_field1D_full_levels(i)) .LT. TINY_PRESSURE) then
                lhs%m_field1D_full_levels(i) = TINY_PRESSURE
            end if
        end do
        
        allocate(CAtmPressureField1D :: obj)
        select type(obj)
        class is(CAtmPressureField1D)
            obj%m_ims = lhs%m_ims
            obj%m_ime = lhs%m_ime
            obj%m_ids = lhs%m_ids
            obj%m_ide = lhs%m_ide
            obj%m_its = lhs%m_its
            obj%m_ite = lhs%m_ite
            obj%m_ms  = lhs%m_ms
            obj%m_me  = lhs%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
            obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
            obj%m_ctor_flags = lhs%m_ctor_flags
            obj%m_rand_distr_type = lhs%m_rand_distr_type
            obj%m_field1D = lhs%m_field1D ** rhs
            obj%m_field1D_full_levels = lhs%m_field1D_full_levels ** rhs
            obj%m_mask_field = lhs%m_mask_field
        end select
        
            
    end  function
    
    
     pure  function  field1D_pow_int(lhs,rhs)    result(obj)
        implicit none
        class(CAtmPressureField1D),     intent(inout) :: lhs
        integer(i32),                   intent(in)    :: rhs
        ! Locals
        integer(i64)                                  :: i
        class(IAtmPressureField), allocatable         :: obj
        ! Start of executable statements
        
        do concurrent(i = lhs%m_ims:lhs%m_ime)
            if(ABS(lhs%m_field1D(i)) .LT. TINY_PRESSURE) then
                lhs%m_field1D(i) = TINY_PRESSURE
            else if(ABS(lhs%m_field1D_full_levels(i)) .LT. TINY_PRESSURE) then
                lhs%m_field1D_full_levels(i) = TINY_PRESSURE
            end if
        end do
        
        allocate(CAtmPressureField1D :: obj)
        select type(obj)
        class is(CAtmPressureField1D)
             obj%m_ims = lhs%m_ims
             obj%m_ime = lhs%m_ime
             obj%m_ids = lhs%m_ids
             obj%m_ide = lhs%m_ide
             obj%m_its = lhs%m_its
             obj%m_ite = lhs%m_ite
             obj%m_ms  = lhs%m_ms
             obj%m_me  = lhs%m_me
!DIR$ IF (Declare_Destructor .EQ. 1)
             obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
             obj%m_ctor_flags = lhs%m_ctor_flags
             obj%m_rand_distr_type = lhs%m_rand_distr_type
             obj%m_field1D = lhs%m_field1D ** rhs
             obj%m_field1D_full_levels = lhs%m_field1D_full_levels ** rhs
             obj%m_mask_field = lhs%m_mask_field
        end select
        
        
     
     
    end  function
    
    
     function  field1D_eq_field1D(lhs,rhs)    result(eq_t)
        implicit none
        class(CAtmPressureField1D),     intent(in) :: lhs
        class(IAtmPressureField),       intent(in) :: rhs
        ! Locals
        type(BoolCompField1D), allocatable         :: eq_t
        integer(i32),                              :: ALLOC_ERR
        character(len=*),                          :: EMSG
        integer(i64)                               :: i
        ! Start of executable statements.
        
         select type(rhs)
          class is(CAtmPressureField1D)
            associate(lf  => lhs%m_field1D, rf => rhs%m_field1D, lfl => lhs%m_field1D_full_levels, &
                      rfl => rhs%m_field1D_full_levels, lmf => lhs%m_mask_field, rmf => rhs%m_mask_field)
                     if((SIZE(lf) /= SIZE(rf)) .OR. (SIZE(lfl) /= SIZE(rfl)) .OR. (SIZE(lmf) /= SIZE(rmf))) then
                           write(*,*) '***** FATAL-ERROR *****'
                           write(*,*) 'Array mismatch size detected!!'
                           write(ERROR_UNIT,10) SIZE(lf) - SIZE(rf)
10                         format('Delta(lf,rf)   is:',I24.20, 'elements')
                           write(ERROR_UNIT,20) SIZE(lfl) - SIZE(rfl)
20                         format('Delta(lfl,rfl) is:',I24.20, 'elements')
                           write(ERROR_UNIT,30) SIZE(lmf) - SIZE(rmf)
30                         format('Delta(lmf,rmf) is:',I24.20, 'elememts')
                           ERROR STOP 'field1D_eq_field1D: Array(s) Size Mismatch'
                end if
                
            end  associate
            class default
                ERROR STOP 'field1D_eq_field1D: Unsupported class/objec type!!'
          end select
        
          allocate(eq_t,STAT=ALLOC_ERR,ERRMSG=EMSG)
          if(ALLOC_ERR /= 0) then
             print*, '***** FATAL-ERROR *****' 
             print*, 'Failed to allocate derived type: BoolCompField1D in: field_eq_field!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ' ,EMSG
             ERROR STOP 'field1D_eq_field1D: MEMORY ALLOCATION FAILURE!!'
          end if
          
          allocate(eq_t%p1dres(SIZE(lhs%m_field1D)),eq_t%p8w1dres(SIZE(lhs%m_field1D_full_levels)), &
                   eq_t%p1dmaskres(SIZE(lhs%m_mask_field)), STAT=ALLOC_ERR,ERRMSG=EMSG)
          if(ALLOC_ERR /= 0) then
             print*, '***** FATAL-ERROR *****'
             print*, 'Failed to allocate dynamic arrays!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ', EMSG
             ERROR STOP 'field1D_eq_field1D: MEMORY ALLOCATION FAILURE!!'
          end if
          
          eq_t%p1dres     = .false. 
          eq_t%p8w1dres   = .false.
          eq_t%p1dmaskres = .false.
     
          select type(rhs)
          class is(CAtmPressureField1D)
              do concurrent(i = lhs%m_ims, lhs%m_ime)
                 eq_t%p1dres(i)     = lhs%m_field1D(i)             == rhs%m_field1D(i)
                 eq_t%p8w1dres(i)   = lhs%m_field1D_full_levels(i) == rhs%m_field1D_full_levels(i)
                 eq_t%p1dmaskres(i) = lhs%m_mask_field(i)          == rhs%m_mask_field(i)
              end do
              class default
                  ERROR STOP 'field1D_eq_field1D: Unsupported class/object type!!'
          end select
          
     
    end  function
    
    
     function  field1D_neq_field1D(lhs,rhs)    result(neq_t)
        implicit none
        use ISO_FORTRAN_ENV, only : ERROR_UNIT
        class(CAtmPressureField1D),     intent(in) :: lhs
        class(IAtmPressureField),       intent(in) :: rhs
        ! Locals
        type(BoolCompField1D), allocatable         :: neq_t
        integer(i32)                               :: ALLOC_ERR
        character(len=*)                           :: EMSG
        integer(i64),                              :: i
        ! Start of executable stataments
        
         select type(rhs)
          class is(CAtmPressureField1D)
            associate(lf  => lhs%m_field1D, rf => rhs%m_field1D, lfl => lhs%m_field1D_full_levels, &
                      rfl => rhs%m_field1D_full_levels, lmf => lhs%m_mask_field, rmf => rhs%m_mask_field)
                     if((SIZE(lf) /= SIZE(rf)) .OR. (SIZE(lfl) /= SIZE(rfl)) .OR. (SIZE(lmf) /= SIZE(rmf))) then
                           write(*,*) '***** FATAL-ERROR *****'
                           write(*,*) 'Array mismatch size detected!!'
                           write(ERROR_UNIT,10) SIZE(lf) - SIZE(rf)
10                         format('Delta(lf,rf)   is:',I24.20, 'elements')
                           write(ERROR_UNIT,20) SIZE(lfl) - SIZE(rfl)
20                         format('Delta(lfl,rfl) is:',I24.20, 'elements')
                           write(ERROR_UNIT,30) SIZE(lmf) - SIZE(rmf)
30                         format('Delta(lmf,rmf) is:',I24.20, 'elememts')
                           ERROR STOP 'field1D_neq_field1D: Array(s) Size Mismatch'
                end if
                
            end  associate
            class default
                ERROR STOP 'field1D_neq_field1D: Unsupported class/objec type!!'
          end select
          
          allocate(neq_t,STAT=ALLOC_ERR,ERR_MSG=EMSG)
          if(ALLOC_ERR /= 0) then
             print*, '***** FATAL-ERROR *****' 
             print*, 'Failed to allocate derived type: BoolCompField1D in: field_neq_field!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ' ,EMSG
             ERROR STOP 'field1D_neq_field1D: MEMORY ALLOCATION FAILURE!!'
          end if
          
          allocate(neq_t%p1dres(SIZE(lhs%m_field1D)),neq_t%p8w1dres(SIZE(lhs%m_field1D_full_levels)), &
                   neq_t%p1dmaskres(SIZE(lhs%m_mask_field)),STAT=ALLOC_ERR,ERR_MSG=EMSG)
          if(ALLOC_ERR /= 0) then
             print*, '***** FATAL-ERROR *****'
             print*, 'Failed to allocate dynamic arrays!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ', EMSG
             ERROR STOP 'field1D_neq_field1D: MEMORY ALLOCATION FAILURE!!'
          end if
          
          neq_t%p1dres(:)     = .false.
          neq_t%p8w1dres(:)   = .false.
          neq_t%p1dmaskres(:) = .false.
          
          select type(rhs)
          class is(CAtmPressureField1D)
              do concurrent(i = lhs%m_ims, lhs%m_ime)
                  neq_t%p1dres(i)     = lhs%m_field1D(i)             /= rhs%m_field1D(i)
                  neq_t%p8w1dres(i)   = lhs%m_field1D_full_levels(i) /= rhs%m_field1D_full_levels(i)
                  neq_t%p1dmaskres(i) = lhs%m_mask_field(i)          /= rhs%m_mask_field(i)
              end do
              class default
                  ERROR STOP 'field1D_neq_field1D: Unsupported derived type!!'
          end select
          
              
     
    end  function
    
    
     function  field1D_lt_field1D(lhs,rhs)    result(lt_t)
        implicit  none
        use ISO_FORTRAN_ENV, only : ERROR_UNIT
        class(CAtmPressureField1D),      intent(in) :: lhs
        class(IAtmPressureField),        intent(in) :: rhs
        ! Locals
        type(BoolCompField1D), allocatable          :: lt_t
        integer(i32)                                :: ALLOC_ERR
        character(len=*)                            :: EMSG
        integer(i64)                                :: i
        ! Start of executable statements.
        
         select type(rhs)
          class is(CAtmPressureField1D)
            associate(lf  => lhs%m_field1D, rf => rhs%m_field1D, lfl => lhs%m_field1D_full_levels, &
                      rfl => rhs%m_field1D_full_levels, lmf => lhs%m_mask_field, rmf => rhs%m_mask_field)
                     if((SIZE(lf) /= SIZE(rf)) .OR. (SIZE(lfl) /= SIZE(rfl)) .OR. (SIZE(lmf) /= SIZE(rmf))) then
                           write(*,*) '***** FATAL-ERROR *****'
                           write(*,*) 'Array mismatch size detected!!'
                           write(ERROR_UNIT,10) SIZE(lf) - SIZE(rf)
10                         format('Delta(lf,rf)   is:',I24.20, 'elements')
                           write(ERROR_UNIT,20) SIZE(lfl) - SIZE(rfl)
20                         format('Delta(lfl,rfl) is:',I24.20, 'elements')
                           write(ERROR_UNIT,30) SIZE(lmf) - SIZE(rmf)
30                         format('Delta(lmf,rmf) is:',I24.20, 'elememts')
                           ERROR STOP 'field1D_lt_field1D: Array(s) Size Mismatch'
                end if
                
            end  associate
            class default
                ERROR STOP 'field1D_lt_field1D: Unsupported class/objec type!!'
          end select
          
          allocate(lt_t,STAT_ALLOC_ERR,ERR_MSG=EMSG)
          if(ALLOC_ERR /= 0) then
             print*, '***** FATAL-ERROR *****' 
             print*, 'Failed to allocate derived type: BoolCompField1D in: field_lt_field!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ' ,EMSG
             ERROR STOP 'field1D_lt_field1D: MEMORY ALLOCATION FAILURE!!' 
          end if
          
          allocate(lt_t%p1dres(SIZE(lhs%m_field1D)),lt_t%p8w1dres(SIZE(lhs%m_field1D_full_levels)), &
                   lt_t%p1dmaskres(SIZE(lhs%m_mask_field)),STAT=ALLOC_ERR,ERR_MSG=EMSG)
          if(ALLOC_ERR /= 0)  then
             print*, '***** FATAL-ERROR *****'
             print*, 'Failed to allocate dynamic arrays!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ', EMSG
             ERROR STOP 'field1D_gt_field1D: MEMORY ALLOCATION FAILURE!!' 
          end if
          
          lt_t%p1dres(:)     = .false.
          lt_t%p8w1dres(:)   = .false.
          lt_t%p1dmaskres(:) = .false.
          
          select type(rhs)
          class is(CAtmPressureField1D)
              do concurrent(i = lhs%m_ims, lhs%m_ime)
                  lt_t%p1dres(i)     = lhs%m_field1D(i)             < rhs%m_field1D(i)
                  lt_t%p8w1dres(i)   = lhs%m_field1D_full_levels(i) < rhs%m_field1D_full_levels(i)
                  lt_t%p1dmaskres(i) = lhs%m_mask_field(i)          < rhs%m_mask_field(i)
              end do
              class default
                  ERROR STOP 'field1D_lt_field1D: Unsupported derived type!!'
          end select
          
     
    end  function
    
    
     function  field1D_gt_field1D(lhs,rhs)      result(gt_t)
        implicit none
        use ISO_FORTRAN_ENV, only : ERROR_UNIT
        class(CAtmPressureField1D),     intent(in) :: lhs
        class(IAtmPressureField),       intent(in) :: rhs
        ! Locals
        type(BoolCompField1D), allocatable         :: gt_t
        integer(i32)                               :: ALLOC_ERR
        character(len=*)                           :: EMSG
        integer(i64)                               :: i
        ! Start of executable statements
        
          select type(rhs)
          class is(CAtmPressureField1D)
            associate(lf  => lhs%m_field1D, rf => rhs%m_field1D, lfl => lhs%m_field1D_full_levels, &
                      rfl => rhs%m_field1D_full_levels, lmf => lhs%m_mask_field, rmf => rhs%m_mask_field)
                     if((SIZE(lf) /= SIZE(rf)) .OR. (SIZE(lfl) /= SIZE(rfl)) .OR. (SIZE(lmf) /= SIZE(rmf))) then
                           write(*,*) '***** FATAL-ERROR *****'
                           write(*,*) 'Array mismatch size detected!!'
                           write(ERROR_UNIT,10) SIZE(lf) - SIZE(rf)
10                         format('Delta(lf,rf)   is:',I24.20, 'elements')
                           write(ERROR_UNIT,20) SIZE(lfl) - SIZE(rfl)
20                         format('Delta(lfl,rfl) is:',I24.20, 'elements')
                           write(ERROR_UNIT,30) SIZE(lmf) - SIZE(rmf)
30                         format('Delta(lmf,rmf) is:',I24.20, 'elememts')
                           ERROR STOP 'field1D_gt_field1D: Array(s) Size Mismatch'
                end if
                
            end  associate
            class default
                ERROR STOP 'field1D_gt_field1D: Unsupported class/objec type!!'
          end select
          
          allocate(gt_t,STAT=ALLOC_ERR,ERR_MSG=EMSG)
          if(ALLOC_ERR /= 0) then
             print*, '***** FATAL-ERROR *****' 
             print*, 'Failed to allocate derived type: BoolCompField1D in: field_gt_field!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ' ,EMSG
             ERROR STOP 'field1D_gt_field1D: MEMORY ALLOCATION FAILURE!!'  
          end if
          
          allocate(gt_t%p1dres(SIZE(lhs%m_field1D)),gt_t%p8w1dres(SIZE(lhs%m_field1D_full_levels)), &
                   gt_t%p1dmaskres(SIZE(lhs%m_mask_field)),STAT=ALLOC_ERR,ERR_MSG=EMSG)
          if(ALLOC_ERR /= 0) then
             print*, '***** FATAL-ERROR *****'
             print*, 'Failed to allocate dynamic arrays!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ', EMSG
             ERROR STOP 'field1D_gt_field1D: MEMORY ALLOCATION FAILURE!!'  
          end if
          
          gt_t%p1dres     = .false.
          gt_t%p8w1dres   = .false.
          gt_t%p1dmaskres = .false.
          
          select type(rhs)
          class is(CAtmPressureField1D)
              do concurrent(i = lhs%m_ims, lhs%m_ime)
                  gt_t%p1dres(i)     = lhs%m_field1D(i)             > rhs%m_field1D(i)
                  gt_t%p8w1dres(i)   = lhs%m_field1D_full_levels(i) > rhs%m_field1D_full_levels(i)
                  gt_t%p1dmaskres(i) = lhs%m_mask_field(i)          > rhs%m_mask_field(i)
              end do
              class default
                  ERROR STOP 'field1D_gt_field1D: Unsupported polymorphic derived type!!'
          end select
          
     
    end  function
    
    
     function  field1D_le_field1D(lhs,rhs)      result(le_t)
        implicit none
        use ISO_FORTRAN_ENV, ONLY : ERROR_UNIT
        class(CAtmPressureField1D),     intent(in) :: lhs
        class(IAtmPressureField),       intent(in) :: rhs
        ! Locals
        type(BoolCompField1D), allocatable         :: le_t
        integer(i32)                               :: ALLOC_ERR
        character(len=*)                           :: EMSG
        integer(i64)                               :: i
        ! Start of executable statements
        
         select type(rhs)
          class is(CAtmPressureField1D)
            associate(lf  => lhs%m_field1D, rf => rhs%m_field1D, lfl => lhs%m_field1D_full_levels, &
                      rfl => rhs%m_field1D_full_levels, lmf => lhs%m_mask_field, rmf => rhs%m_mask_field)
                     if((SIZE(lf) /= SIZE(rf)) .OR. (SIZE(lfl) /= SIZE(rfl)) .OR. (SIZE(lmf) /= SIZE(rmf))) then
                           write(*,*) '***** FATAL-ERROR *****'
                           write(*,*) 'Array mismatch size detected!!'
                           write(ERROR_UNIT,10) SIZE(lf) - SIZE(rf)
10                         format('Delta(lf,rf)   is:',I24.20, 'elements')
                           write(ERROR_UNIT,20) SIZE(lfl) - SIZE(rfl)
20                         format('Delta(lfl,rfl) is:',I24.20, 'elements')
                           write(ERROR_UNIT,30) SIZE(lmf) - SIZE(rmf)
30                         format('Delta(lmf,rmf) is:',I24.20, 'elememts')
                           ERROR STOP 'field1D_le_field1D: Array(s) Size Mismatch'
                end if
                
            end  associate
            class default
                ERROR STOP 'field1D_le_field1D: Unsupported class/objec type!!'
          end select
          
          allocate(le_t,STAT=ALLOC_ERR,ERR_MSG=EMSG)
          if(ALLOC_ERR /= 0) then
             print*, '***** FATAL-ERROR *****' 
             print*, 'Failed to allocate derived type: BoolCompField1D in: field_le_field!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ' ,EMSG
             ERROR STOP 'field1D_le_field1D: MEMORY ALLOCATION FAILURE!!'  
          end if
          
          allocate(le_t%p1dres(SIZE(lhs%m_field1D)),le_t%p8w1dres(SIZE(lhs%m_field1D_full_levels)), &
                   le_t%p1dmaskres(SIZE(lhs%m_mask_field)),STAT=ALLOC_ERR,ERR_MSG=EMSG)
          if(ALLOC_ERR /= 0) then
             print*, '***** FATAL-ERROR *****'
             print*, 'Failed to allocate dynamic arrays!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ', EMSG
             ERROR STOP 'field1D_le_field1D: MEMORY ALLOCATION FAILURE!!'   
          end if
          
          le_t%p1dres(:)     = .false.
          le_t%p8w1dres(:)   = .false.
          le_t%p1dmaskres(:) = .false.
          
          select type(rhs)
          class is(CAtmPressureField1D)
             do concurrent(i = lhs%m_ims, lhs%m_ime)
                 le_t%p1dres(i)     = lhs%m_field1D(i)             <= rhs%m_field1D(i)
                 le_t%p8w1dres(i)   = lhs%m_field1D_full_levels(i) <= rhs%m_field1D_full_levels(i)
                 le_t%p1dmaskres(i) = lhs%m_mask_field(i)          <= rhs%m_mask_field1D(i)
             end do
          class default
                 ERROR STOP 'field1D_le_field1D: Unsupported polymorphic derived type!!'
          end select
          
     
    end  function
    
    
     function  field1D_ge_field1D(lhs,rhs)     result(ge_t)
        implicit none
        use ISO_FORTRAN_ENV, only : ERROR_UNIT
        class(CAtmPressureField1D),   intent(in) :: lhs
        class(IAtmPressureField),     intent(in) :: rhs
        ! Locals
        type(BoolCompField1D), allocatable       :: ge_t
        integer(i32)                             :: ALLOC_ERR
        character(len=*)                         :: EMSG
        integer(i64)                             :: i
        ! Start of executable statements
        
         select type(rhs)
          class is(CAtmPressureField1D)
            associate(lf  => lhs%m_field1D, rf => rhs%m_field1D, lfl => lhs%m_field1D_full_levels, &
                      rfl => rhs%m_field1D_full_levels, lmf => lhs%m_mask_field, rmf => rhs%m_mask_field)
                     if((SIZE(lf) /= SIZE(rf)) .OR. (SIZE(lfl) /= SIZE(rfl)) .OR. (SIZE(lmf) /= SIZE(rmf))) then
                           write(*,*) '***** FATAL-ERROR *****'
                           write(*,*) 'Array mismatch size detected!!'
                           write(ERROR_UNIT,10) SIZE(lf) - SIZE(rf)
10                         format('Delta(lf,rf)   is:',I24.20, 'elements')
                           write(ERROR_UNIT,20) SIZE(lfl) - SIZE(rfl)
20                         format('Delta(lfl,rfl) is:',I24.20, 'elements')
                           write(ERROR_UNIT,30) SIZE(lmf) - SIZE(rmf)
30                         format('Delta(lmf,rmf) is:',I24.20, 'elememts')
                           ERROR STOP 'field1D_ge_field1D: Array(s) Size Mismatch'
                end if
                
            end  associate
            class default
                ERROR STOP 'field1D_ge_field1D: Unsupported polymorphic derived type!!'
          end select
          
          allocate(ge_t,STAT=ALLOC_ERR,ERR_MSG=EMSG)
          if(ALLOC_ERR /= 0)  then
             print*, '***** FATAL-ERROR *****' 
             print*, 'Failed to allocate derived type: BoolCompField1D in: field_ge_field!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ' ,EMSG
             ERROR STOP 'field1D_le_field1D: MEMORY ALLOCATION FAILURE!!'   
          end if
          
          allocate(ge_t%p1dres(SIZE(lhs%m_field1D)),ge_t%p8w1dres(SIZE(lhs%m_field1D_full_levels)), &
                   ge_t%p1dmaskres(SIZE(lhs%m_mask_field)),STAT=ALLOC_ERR,ERR_MSG=EMSG)
          if(ALLOC_ERR /= 0)  then
             print*, '***** FATAL-ERROR *****'
             print*, 'Failed to allocate dynamic arrays!!'
             print*, 'Allocation error: ', ALLOC_ERR
             print*, 'Error message:    ', EMSG
             ERROR STOP 'field1D_ge_field1D: MEMORY ALLOCATION FAILURE!!'  
          end if
          
          ge_t%p1dres(:)     = .false.
          ge_t%p8w1dres(:)   = .false.
          ge_t%p1dmaskres(:) = .false.
          
          select type(rhs)
          class is(CAtmPressureField1D)
              do concurrent(i = lhs%m_ims, lhs%m_ime)
                  ge_t%p1dres(i)     = lhs%m_field1D(i)             >= rhs%m_field1D(i)
                  ge_t%p8w1dres(i)   = lhs%m_field1D_full_levels(i) >= rhs%m_field1D_full_levels(i)
                  ge_t%p1dmaskres(i) = lhs%m_mask_field(i)          >= rhs%m_mask_field(i)
              end do
              class default
                  ERROR STOP 'field1D_ge_field1D: Unsupported polymorphic derived type!!'
          end select
          
          
     
    end  function
    
    
    !================================38
    ! Implementation of concrete class
    ! 'CAtmPressureField2D'
    !================================38
    
    !================================38
    !  class Constructors
    !================================38
    
    type(CAtmPressureField2D)  function def_ctor_field2D(ims,ime,jms,jme,ids,ide, &
                                                         jds,jde,its,ite,jts,jte, &
                                                         m1s,m1e,m2s,m2e        )
    
            implicit none
            integer(i64),       intent(in) :: ims
            integer(i64),       intent(in) :: ime
            integer(i64),       intent(in) :: jms
            integer(i64),       intent(in) :: jme
            integer(i64),       intent(in) :: ids
            integer(i64),       intent(in) :: ide
            integer(i64),       intent(in) :: jds
            integer(i64),       intent(in) :: jde
            integer(i64),       intent(in) :: its
            integer(i64),       intent(in) :: ite
            integer(i64),       intent(in) :: jts
            integer(i64),       intent(in) :: jte
            integer(i64),       intent(in) :: m1s
            integer(i64),       intent(in) :: m1e
            integer(i64),       intent(in) :: m2s
            integer(i64),       intent(in) :: m2e
            ! Locals
            integer(i32)                   :: AllocErr
            character(len=*)               :: EMSG
            ! Start of executable statments
            
            if(debug_flag) call print_prologue('At prologue of: CAtmPressureField2D%def_ctor_field2D')
            
            if((ime .EQ. 0)   .OR. (jme .EQ. 0)     .OR. &
               (m1s .LT. ims) .OR. (m1e .GT. ime)   .OR. &
               (m2s .LT. jms) .OR. (m2e .GT. jme)     ) then
                
                   call check_dim2d(ime,jme,m1s,m1e,m2s,m2e,'CAtmPressureField2D%def_ctor_field2D',3512)
            end if
    
           associate(d1s => ims, d1e => ime, d2s => jms, d2e => jme)
               
               allocate(def_ctor_field2D%m_p2d(d1s:d1e,d2s:d2e),def_ctor_field2D%m_p8w2d(d1s:d1e,d2s:d2e), &
                        def_ctor_field2D%m_maskp2d(d1s:d1e,d2s:d2e),STAT=AllocErr,ERRMSG=EMSG)
          end associate
    
               if(AllocErr /= 0)  then
                  call alloc2d_error_handler(AllocErr,EMSG,file_path,"def_ctor_field2D",3589) 
              end if
    
              def_ctor_field2D%m_ims = ims
              def_ctor_field2D%m_ime = ime
              def_ctor_field2D%m_jms = jms
              def_ctor_field2D%m_jme = jme
              def_ctor_field2D%m_ids = ids
              def_ctor_field2D%m_ide = ide
              def_ctor_field2D%m_jds = jds
              def_ctor_field2D%m_jde = jde
              def_ctor_field2D%m_its = its
              def_ctor_field2D%m_ite = ite
              def_ctor_field2D%m_jts = jts
              def_ctor_field2D%m_jte = jte
              def_ctor_field2D%m_m1s = m1s
              def_ctor_field2D%m_m1e = m1e
              def_ctor_field2D%m_m2s = m2s
              def_ctor_field2D%m_m2e = m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
              def_ctor_field2D%m_is_initialized = .true.
!DIR$ ENDIF
              def_ctor_field2D%m_ctor_flags(1) = .true.
              def_ctor_field2D%m_ctor_flags(2) = .false.
              def_ctor_field2D%m_ctor_flags(3) = .false.
              def_ctor_field2D%m_ctor_flags(4) = .false.
              def_ctor_field2D%m_rand_distr_t  = 'NONE'
              def_ctor_field2D%m_p2d           = TINY_PRESSURE
              def_ctor_field2D%m_p8w2d         = TINY_PRESSURE
              def_ctor_field2D%m_maskp2d       = .false.
              
              if(debug_flag) call print_epilogue('At epilogue of: CAtmPressureField2D%def_ctor_field2D')
    
    end  function
    
    
     type(CAtmPressureField2D)  function scalar_ctor_field2D(ims,ime,jms,jme,ids,ide, &
                                                             jds,jde,its,ite,jts,jte, &
                                                             m1s,m1e,m2s,m2e,rndstr,  &
                                                             pval,pwval,dtflag        )
            implicit none
            integer(i64),       intent(in) :: ims
            integer(i64),       intent(in) :: ime
            integer(i64),       intent(in) :: jms
            integer(i64),       intent(in) :: jme
            integer(i64),       intent(in) :: ids
            integer(i64),       intent(in) :: ide
            integer(i64),       intent(in) :: jds
            integer(i64),       intent(in) :: jde
            integer(i64),       intent(in) :: its
            integer(i64),       intent(in) :: ite
            integer(i64),       intent(in) :: jts
            integer(i64),       intent(in) :: jte
            integer(i64),       intent(in) :: m1s
            integer(i64),       intent(in) :: m1e
            integer(i64),       intent(in) :: m2s
            integer(i64),       intent(in) :: m2e
            character(len=*),   intent(in) :: rndstr
            real(R64),          intent(in) :: pval
            real(R64),          intent(in) :: pwval
            logical(kind=8),    intent(in) :: dtflag
            ! Locals
            integer(i32)                   :: AllocErr
            character(len=256)             :: EMSG
            ! Start of executable statements
            
            if(debug_flag)  call print_prologue('At prologue of CAtmPressureField2D%scalar_ctor_field2D')
            
            if((ime .EQ. 0) .OR. (jme .EQ. 0) .OR. &
               (m1s .LT. ims) .OR. (m1e .GT. ime) .OR. &
               (m2s .LT. jms) .OR. (m2e .GT. jme)     ) then
                
                   call check_dim2d(ime,jme,m1s,m1em2s,m2e,'CAtmPressureField2D%scalar_ctor_field2D',3114)
            end if
    
            associate(d1s => ims, d1e => ime, d2s => jms, d2e => jme)
                
                 allocate(scalar_ctor_field2D%m_p2d(d1s:d1e,d2s:d2e),scalar_ctor_field2D%m_p8w2d(d1s:d1e,d2s:d2e), &
                          scalar_ctor_field2D%m_maskp2d(d1s:d1e,d2s:d2e),STAT=AllocErr,ERRMSG=EMSG)
                
            end associate
     
            if(AllocErr /= 0) then
               call alloc2d_error_handler(AllocErr,EMSG,file_path,"scalar_ctor_field2D",3652) 
            end if
    
            scalar_ctor_field2D%m_ims = ims
            scalar_ctor_field2D%m_ime = ime
            scalar_ctor_field2D%m_jms = jms
            scalar_ctor_field2D%m_jme = jme
            scalar_ctor_field2D%m_ids = ids
            scalar_ctor_field2D%m_ide = ide
            scalar_ctor_field2D%m_jds = jds
            scalar_ctor_field2D%m_jde = jde
            scalar_ctor_field2D%m_its = its
            scalar_ctor_field2D%m_ite = ite
            scalar_ctor_field2D%m_jts = jts
            scalar_ctor_field2D%m_jte = jte
            scalar_ctor_field2D%m_m1s = m1s
            scalar_ctor_field2D%m_m1e = m1e
            scalar_ctor_field2D%m_m2s = m2s
            scalar_ctor_field2D%m_m2e = m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
            scalar_ctor_field2D%m_is_initialized = .true.
!DIR$ ENDIF
            scalar_ctor_field2D%m_ctor_flags(1) = .false.
            scalar_ctor_field2D%m_ctor_flags(2) = .true.
            scalar_ctor_field2D%m_ctor_flags(3) = .false.
            scalar_ctor_field2D%m_ctor_flags(4) = .false.
            scalar_ctor_field2D%m_rand_distr_t  = rndstr
            scalar_ctor_field2D%m_p2d           = pval
            scalar_ctor_field2D%m_p8w2d         = pwval
            scalar_ctor_field2D%m_maskp2d       = dtflag
            
            if(debug_flag)  call print_epilogue('At epilogue of: CAtmPressureField2D%scalar_ctor_field2D')
    
    end  function
    
    
     type(CAtmPressureField2D)  function field_ctor_field2D(ims,ime,jms,jme,ids,ide, &
                                                            jds,jde,its,ite,jts,jte, &
                                                            m1s,m1e,m2s,m2e,rndstr,  &
                                                            p2d,p8w2d,maskp2d,use_omp        )
     
            implicit none
            integer(i64),                    intent(in) :: ims
            integer(i64),                    intent(in) :: ime
            integer(i64),                    intent(in) :: jms
            integer(i64),                    intent(in) :: jme
            integer(i64),                    intent(in) :: ids
            integer(i64),                    intent(in) :: ide
            integer(i64),                    intent(in) :: jds
            integer(i64),                    intent(in) :: jde
            integer(i64),                    intent(in) :: its
            integer(i64),                    intent(in) :: ite
            integer(i64),                    intent(in) :: jts
            integer(i64),                    intent(in) :: jte
            integer(i64),                    intent(in) :: m1s
            integer(i64),                    intent(in) :: m1e
            integer(i64),                    intent(in) :: m2s
            integer(i64),                    intent(in) :: m2e
            character(len=*),                intent(in) :: rndstr
            real(R64), dimension(:,:),       intent(in) :: p2d
            !DIR$ ASSUME_ALIGNED p2d:32
            real(R64), dimension(:,:),       intent(in) :: p8w2d
            !DIR$ ASSUME_ALIGNED p8w2d:32
            logical(kind=8), dimension(:,:), intent(in) :: maskp2d
            !DIR$ ASSUME_ALIGNED maskp2d:32
            logical(kind=4),                 intent(in) :: use_omp
            ! Locals
            integer(i32)                                :: AllocErr
            character(len=256)                          :: EMSG
            character(len=80), dimension(6) :: msg = ['FATAL-ERROR in: CAtmPressureField2D%field_ctor_field2D', &
                                                     'Non-allocated allocatable array(s) passed!!', &
                                                     '***** ERROR-DETAILS *****'  , &
                                                     'Allocation status: '  , &
                                                     '***** ERROR-DETAILS *****' , &
                                                     'TERMINATING EXECUTION WITH STOP STATEMENT'   ]
            ! Start of executable statements.
            
            if(debug_flag)  call print_prologue('At prologue of: CAtmPressureField2D%field_ctor_field2D')
            
            if((ime .EQ. 0) .OR. (jme .EQ. 0) .OR. &
               (m1s .LT. ims) .OR. (m1e .GT. ime) .OR. &
               (m2s .LT. jms) .OR. (m2e .GT. jme)     ) then
                
                   call check_dim2d(ime,jme,m1s,m1e,m2s,m2e,'CAtmPressureField2D%field_ctor_field2D',3732)
            end if
    
            call check_alloc2DR64_fail(p2d,msg,file,3739)
            call check_alloc2DR64_fail(p8w2d,msg,file,3740)
            call check_alloc2DL64_fail(maskp2d,msg,file,3741)
            
            associate(d1s => ims, d1e => ime, d2s => jms, d2e => jme)
                
                allocate(field_ctor_field2D%m_p2d(d1s:d1e,d2s:d2e),field_ctor_field2D%m_p8w2d(d1s:d1e,d2s:d2e),&
                         field_ctor_field2D%m_maskp2d(d1s:d1e,d2s:d2e),STAT=AllocErr,ERRMSG=EMSG)
                
            end associate
            
             if(AllocErr /= 0)  then
               call alloc2d_error_handler(AllocErr,EMSG,file_path,"field_ctor_field2D",3751)
            end if     
            
            field_ctor_field2D%m_ims = ims
            field_ctor_field2D%m_ime = ime
            field_ctor_field2D%m_jms = jme
            field_ctor_field2D%m_jme = jme
            field_ctor_field2D%m_ids = ids
            field_ctor_field2D%m_ide = ide
            field_ctor_field2D%m_jds = jds
            field_ctor_field2D%m_jde = jde
            field_ctor_field2D%m_its = its
            field_ctor_field2D%m_ite = ite
            field_ctor_field2D%m_jts = jts
            field_ctor_field2D%m_jte = jte
            field_ctor_field2D%m_m1s = m1s
            field_ctor_field2D%m_m1e = m1e
            field_ctor_field2D%m_m2s = m2s
            field_ctor_field2D%m_m2e = m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
            field_ctor_field2D%m_is_initialized = .true.
!DIR$ ENDIF
            field_ctor_field2D%m_ctor_flags(1) = .false.
            field_ctor_field2D%m_ctor_flags(2) = .false.
            field_ctor_field2D%m_ctor_flags(3) = .true.
            field_ctor_field2D%m_ctor_flags(4) = .false.
            field_ctor_field2D%m_rand_distr_t  =  rndstr
            
            if(use_omp .EQ. .true.) then
                call omp_copy_arrays2d(p2d,p8w2d,maskp2d,          &
                                       field_ctor_field2D%m_p2d,   &
                                       field_ctor_field2D%m_p8w2d, &
                                       field_ctor_field2D%m_maskp2d )
           else
        
               field_ctor_field2D%m_p2d           =  p2d
               field_ctor_field2D%m_p8w2d         =  p8w2d
               field_ctor_field2D%m_maskp2d       =  maskp2d
          end if
    
            if(debug_flag)  call print_epilogue('At epilogue of: CAtmPressureField2D%field_ctor_field2D')
            
    
    end  function
    
    
     type(CAtmPressureField2D)  function copy_ctor_field2D(other,use_omp)
     
          implicit none
          class(CAtmPressureField2D),   intent(in) :: other
          logical(I32),                 intent(in) :: use_omp
          ! Locals
          integer(I32)                             :: AllocErr
          character(len=*)                         :: EMSG
          ! Start of executable statements
          
          if(debug_flag)  call print_prologue('At prologue of: CAtmPressureField2D%copy_ctor_field2D')
          
          ! Assume that 'other' did not deallocate its arrays
          associate(d1s => other%m_ims, d1e => other%m_ime, d2s => other%m_jms, d2e => other%m_jme)
              
              allocate(copy_ctor_field2D%m_p2d(d1s:d1e,d2s:d2e),copy_ctor_field2D%m_p8w2d(d1s:d1e,d1s:d2e), &
                       copy_ctor_field2D%m_maskp2d(d1s:d1e,d2s:d2e),STAT=AllocErr,ERRMSG=EMSG)
              
          end  associate
    
          if(AllocErr /= 0) then
             call alloc2d_error_handler(AllocErr,EMSG,file_path,"copy_ctor_field2D",3817) 
          end if
    
          copy_ctor_field2D%m_ims  = other%m_ims
          copy_ctor_field2D%m_ime  = other%m_ime
          copy_ctor_field2D%m_jms  = other%m_jms
          copy_ctor_field2D%m_jme  = other%m_jme
          copy_ctor_field2D%m_ids  = other%m_ids
          copy_ctor_field2D%m_ide  = other%m_ide
          copy_ctor_field2D%m_jds  = other%m_jds
          copy_ctor_field2D%m_jde  = other%m_jde
          copy_ctor_field2D%m_its  = other%m_its
          copy_ctor_field2D%m_ite  = other%m_ite
          copy_ctor_field2D%m_jts  = other%m_jts
          copy_ctor_field2D%m_jte  = other%m_jte
          copy_ctor_field2D%m_m1s  = other%m_m1s
          copy_ctor_field2D%m_m1e  = other%m_m1e
          copy_ctor_field2D%m_m2s  = other%m_m2s
          copy_ctor_field2D%m_m2e  = other%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
          copy_ctor_field2D%m_is_initialized = other%m_is_initialized
!DIR$ ENDIF
          copy_ctor_field2D%m_ctor_flags     = other%m_ctor_flags
          copy_ctor_field2D%m_rand_distr_t   = other%m_rand_distr_t
          
          if(use_omp .EQ. .true.) then
              call omp_copy_arrays2d(other%m_p2d,other%m_p8w2d,other%m_maskp2d,         &
                                     copy_ctor_field2D%m_p2d,copy_ctor_field2D%m_p8w2d, &
                                     copy_ctor_field2D%m_maskp2d    )
          else
        
              copy_ctor_field2D%m_p2d            = other%m_p2d
              copy_ctor_field2D%m_p8w2d          = other%m_p8w2d
              copy_ctor_field2D%m_maskp2d        = other%m_maskp2d
          
          end if
    
          
          if(debug_flag)  call print_epilogue('At epilogue of: CAtmPressureField2D%copy_ctor_field2D')
          
     
    end  function
    
    
     type(CAtmPressureField2D)  function move_ctor_field2D(other)
          implicit none
          class(CAtmPressureField2D),    intent(inout) :: other
          ! No Locals
         
          ! Start of executable statements
          
          if(debug_flag)  call print_prologue('At prologue of: CAtmPressureField2D%move_ctor_field2D')
          
          move_ctor_field2D%m_ims = other%m_ims
          move_ctor_field2D%m_ime = other%m_ime
          move_ctor_field2D%m_jms = other%m_jms
          move_ctor_field2D%m_jme = other%m_jme
          move_ctor_field2D%m_ids = other%m_ids
          move_ctor_field2D%m_ide = other%m_ide
          move_ctor_field2D%m_jds = other%m_jds
          move_ctor_field2D%m_jde = other%m_jde
          move_ctor_field2D%m_its = other%m_its
          move_ctor_field2D%m_ite = other%m_ite
          move_ctor_field2D%m_jts = other%m_jts
          move_ctor_field2D%m_jte = other%m_jte
          move_ctor_field2D%m_m1s = other%m_m1s
          move_ctor_field2D%m_m1e = other%m_m1e
          move_ctor_field2D%m_m2s = other%m_m2s
          move_ctor_field2D%m_m2e = other%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
          move_ctor_field2D%m_is_initialized = other%m_is_initialized
!DIR$ ENDIF
          move_ctor_field2D%m_ctor_flags(1) = .false.
          move_ctor_field2D%m_ctor_flags(2) = .false.
          move_ctor_field2D%m_ctor_flags(3) = .false.
          move_ctor_field2D%m_ctor_flags(4) = .false.
          move_ctor_field2D%m_ctor_flags(5) = .true.
          move_ctor_field2D%m_rand_distr_t   = other%m_rand_distr_t
          call move_alloc(other%m_p2d,move_ctor_field2D%m_p2d)     ! other%m_p2d is deallocated here
          call move_alloc(other%m_p8w2d,move_ctor_field2D%m_p8w2d) ! other%m_p8w2d is deallocated here
          call move_alloc(other%m_maskp2d,move_ctor_field2D%m_maskp2d)  ! other%m_maskp2d is deallocated here
          ! Starting  nullification of  'other' indexing arguments
          other%m_ims = 0
          other%m_ime = 0
          other%m_jms = 0
          other%m_jme = 0
          other%m_ids = 0
          other%m_ide = 0
          other%m_jds = 0
          other%m_jde = 0
          other%m_its = 0
          other%m_ite = 0
          other%m_jts = 0
          other%m_jte = 0
          other%m_m1s = 0
          other%m_m1e = 0
          other%m_m2s = 0
          other%m_m2e = 0
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
          other%m_is_initialized = .false.
!DIR$ ENDIF
          other%m_ctor_flags = .false.
          other%m_rand_distr_t = ''
          
          if(debug_flag)  call print_epilogue('At prologue of: CAtmPressureField2D%move_ctor_field2D')
          
     
    end  function
    
    
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
    
     subroutine  destroy_field2D(this)
          implicit none
          class(CAtmPressureField2D),  intent(inout) :: this
          ! Locals
          integer(I32)                               :: DeallocErr
          character(len=*)                           :: EMSG
          ! Start of executable statements
          
          if(debug_flag) call  print_prologue('At prologue of: CAtmPressureField2D%destroy_field2D')
          
          if(.NOT. this%m_is_initialized) then
             print*, 'Attempted to destroy un-initialized object!!'
             print*, 'Status of this%m_is_initialized: ', this%m_is_initialized
             print*, 'Executing early exit!!'
             return
          end if
          
          this%m_ims = 0
          this%m_ime = 0
          this%m_jms = 0
          this%m_jme = 0
          this%m_ids = 0
          this%m_ide = 0
          this%m_jds = 0
          this%m_jde = 0
          this%m_its = 0
          this%m_ite = 0
          this%m_jts = 0
          this%m_jte = 0
          this%m_m1s = 0
          this%m_m1e = 0
          this%m_m2s = 0
          this%m_m2e = 0
          this%m_ctor_flags = .false.
          this%m_rand_distr_t = ''
          
          if(allocated(this%m_p2d) .AND. allocated(this%m_p8w2d) .AND. &
             allocated(this%m_maskp2d) ) then
             deallocate(this%m_p2d,this%m_p8w2d,this%m_maskp2d,STAT=DeallocErr,ERR_MSG=EMSG)
             if(DeallocErr /= 0) then
                call alloc2d_error_handler(DeallocErr,EMSG,file_path,"destroy_field2D",4004)
            end if    
         end if     
         
             
          this%m_is_initialized = .false.   
         
          if(debug_flag)  call print_epilogue('At epilogue of: CAtmPressureField2D%destroy_field2D')  
          
     
     
    end  subroutine
    
!DIR$ ENDIF
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_ims
#endif

     pure  function  get_field2D_ims(this)   result(ret_ims)
          implicit none
          class(CAtmPressureField2D),   intent(in) :: this
          ! Locals
          integer(i64)                             :: ret_ims
          ! Start of executable statements
          
          ret_ims = this%m_ims
     
     end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_ime
#endif

     pure  function get_field2D_ime(this)  result(ret_ime)
          implicit none
          class(CAtmPressureField2D),   intent(in) :: this
          ! Locals
          integer(i64)                             :: ret_ime
          ! Start of executable statements
          
          ret_ime = this%m_ime
     
     end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_jms
#endif

    pure  function get_field2D_jms(this)  result(ret_jms)
          implicit none
          class(CAtmPressureField2D),    intent(in) :: this
          ! Locals
          integer(i64)                              :: ret_jms
          ! Start of executable statements
          
          ret_jms = this%m_jms
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_jme
#endif

    pure  function get_field2D_jme(this)  result(ret_jme)
          implicit none
          class(CAtmPressureField2D),    intent(in) :: this
          ! Locals
          integer(i64)                              :: ret_jme
          ! Start of executable statments
          
          ret_jme = this%m_jme
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_ids
#endif

    pure  function  get_field2D_ids(this)  result(ret_ids)
          implicit none
          class(CAtmPressureField2D),    intent(in) :: this
          ! Locals
          integer(i64)                              :: ret_ids
          ! Start of executable statements
          
          ret_ids = this%m_ids
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_ide
#endif
    
    pure  function get_field2D_ide(this)   result(ret_ide)
          implicit none
          class(CAtmPressureField2D),     intent(in) :: this
          ! Locals
          integer(i64)                               :: ret_ide
          !  Start of executable statements
          
          ret_ide = this%m_ide
    
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_jds
#endif
    
    pure  function get_field2D_jds(this)    result(ret_jds)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: this
          ! Locals
          integer(i64)                                 :: ret_jds
          ! Start of executable statements
          
          ret_jds = this%m_jds
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_jde
#endif

    pure  function get_field2D_jde(this)    result(ret_jde)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: this
          ! Locals
          integer(i64)                                 :: ret_jde
          ! Start of executable statements
          
          ret_jde = this%m_jde
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_its
#endif

    pure  function get_field2D_its(this)    result(ret_its)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: this
          ! Locals
          integer(i64)                                 :: ret_its
          ! Start of executable statements
          
          ret_its = this%m_its
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_ite
#endif

    pure  function get_field2D_ite(this)    result(ret_ite)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: this
          ! Locals
          integer(i64)                                 :: ret_ite
          ! Start of executable statements
          
          ret_ite = this%m_ite
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_jts
#endif

    pure  function get_field2D_jts(this)    result(ret_jts)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: this
          ! Locals
          integer(i64)                                 :: ret_jts
          ! Start of executable statements
          
          ret_jts = this%m_jts
    
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_jte
#endif

    pure  function get_field2D_jte(this)    result(ret_jte)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: this
          ! Locals
          integer(i64)                                 :: ret_jte
          ! Start of executable statements
    
          ret_jte = this%m_jte
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_m1s
#endif

    pure  function get_field2D_m1s(this)    result(ret_m1s)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: this
          ! Locals
          integer(i64)                                 :: ret_m1s
          ! Start of executable statements
          
          ret_m1s = this%m_m1s
    
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_m1e
#endif

    pure  function get_field2D_m1e(this)    result(ret_m1e)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: this
          ! Locals
          integer(i64)                                 :: ret_m1e
          ! Start of executable statements
          
          ret_m1e = this%m_m1e
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_m2s
#endif

    pure  function get_field2D_m2s(this)    result(ret_m2s)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: this
          ! Locals
          integer(i64)                                 :: ret_m2s
          ! Start of executable statements
          
          ret_m2s = this%m_m2s
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_m2e
#endif

    pure  function get_field2D_m2e(this)    result(ret_m2e)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: this
          ! Locals
          integer(i64)                                 :: ret_m2e
          ! Start of executable statements
          
          ret_m2e = this%m_m2e
    
    end  function
    
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)

#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_init
#endif
    
    pure  function get_field2D_init(this)   result(ret_init)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: this
          ! Locals
          logical(i32)                                 :: ret_init
          ! Start of executable statememts
          
          ret_init = this%m_is_initialized
    
    end function
    
!DIR$ ENDIF
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field2D_ctor_flags
#endif

    pure  function get_field2D_ctor_flags(this)  result(ret_flags)
          implicit none
          class(CAtmPressureField2D),           intent(in) :: this
          ! Locals
          logical(i32), dimension(num_ctors)               :: ret_flags
          ! Start of executable statements
          
          ret_flags = this%m_ctors_flags
    
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_rand_distr
#endif

    pure  function get_rand_distr(this)   result(ret_rdistr)
          implicit none
          class(CAtmPressureField2D),     intent(in) :: this
          ! Locals
          character(len=*)                           :: ret_rdistr
          ! Start of executable statements
          
          ret_rdistr = this%m_rand_distr_t
    
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_p2d
#endif

    pure  function get_p2d(this)    result(ret_p2d)
          implicit none
          class(CAtmPressureField2D),   intent(in) :: this
          ! Locals
          real(R64), allocatable, dimension(:,:)   :: ret_p2d
          !DIR$ ATTRIBUTES_ALIGN : 32 :: ret_p2d
          ! Start of executable statements
          
          ret_p2d = this%m_p2d
    
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_p8w2d
#endif

    pure  function get_p8w2d(this)   result(ret_p8w2d)
          implicit none
          class(CAtmPressureField2D),   intent(in) :: this
          ! Locals
          real(R64), allocatable, dimension(:,:)   :: ret_p8w2d
          !DIR$ ATTRIBUTES_ALIGN : 32 :: ret_p8w2d
          ! Start of executable statements
          
          ret_p8w2d = this%m_p8w2d
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_maskp2d
#endif

    pure  function get_maskp2d(this)    result(ret_mskp2d)
          implicit none
          class(CAtmPressureField2D),   intent(in) :: this
          ! Locals
          logical(kind=8), allocatable, dimension(:,:) :: ret_mskp2d
          !DIR$ ATTRIBUTES_ALIGN : 32 :: ret_mskp2d
          ! Start of executable statements
          
          ret_mskp2d = this%m_maskp2d
    
    
    end  function
    
    
    function  scalar_from_p2d(this,idx,jdx)     result(ret_val)
          implicit none
         
          class(CAtmPressureField2D),   intent(in) :: this
          integer(i64),                 intent(in) :: idx
          integer(i64),                 intent(in) :: jdx
          ! Locals
          real(R64)                                :: ret_val
          character(len=80), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%scalar_from_p2d', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
                                                                
                                                                
          ! Start of executable statements
          
          if((idx .LT. this%m_ims .OR. idx .GT. this%m_ime) .OR. &
             (jdx .LT. this%m_jms .OR. jdx .GT. this%m_jme)      ) then
              
             call check_dim2Di64_eq(idx,jdx,this%m_ime,this%m_jme,msg_ar,file_path,3834)
         end  if
             
         ret_val = this%m_p2d(idx,jdx)
    
    end  function
    
    
    function  scalar_from_p8w2d(this,idx,jdx)   result(ret_val)
          implicit none
          
          class(CAtmPressureField2D),   intent(in) :: this
          integer(i64),                 intent(in) :: idx
          integer(i64),                 intent(in) :: jdx
          ! Locals
          real(R64)                                :: ret_val
          character(len=*), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%scalar_from_p8w2d', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
    
           if((idx .LT. this%m_ims .OR. idx .GT. this%m_ime) .OR. &
              (jdx .LT. this%m_jms .OR. jdx .GT. this%m_jme)      ) then
              
               call check_dim2Di64_eq(idx,jdx,this%m_ime,this%m_jme,msg_ar,file_path,3863)
           end  if
              
           ret_val = this%m_p8w2d(idx,jdx)   
          
    end  function
    
    
    function  scalar_from_maskp2d(this,idx,jdx)    result(ret_val)
          implicit none
          
          class(CAtmPressureField2D),   intent(in) :: this
          integer(i64),                 intent(in) :: idx
          integer(i64),                 intent(in) :: jdx
          ! Locals
          logical(kind=8)                          :: ret_val
          character(len=*), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%scalar_from_maskp2d', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of execuatble statements
          
          
         if((idx .LT. this%m_ims  .OR. idx .GT. this%m_ime) .OR. &
            (jdx .LT. this%m_jms  .OR. jdx .GT. this%m_jme)      ) then
              
               call check_dim2Di64_eq(idx,jdx,this%m_ime,this%m_jme,msg_ar,file_path,3893)
         end  if
    
         ret_val = this%m_maskp2d(idx,idy)
    
    end  function
    
    
    function  subarray_from_p2d(this,dim1s,dim1e,dim2s,dim2e)   result(ret_subarr)
          implicit none
          
          class(CAtmPressureField2D),   intent(in) :: this
          integer(i64),                 intent(in) :: dim1s
          integer(i64),                 intent(in) :: dim1e
          integer(i64),                 intent(in) :: dim2s
          integer(i64),                 intent(in) :: dim2e
          ! Locals
          real(R64), allocatable, dimension(:,:)   :: ret_subarr
          !DIR$ ATTRIBUTES_ALIGN : 32 :: ret_subarr
          character(len=*), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%subarray_from_p2d', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'dim1e      = ', &
                                                                'dim2e      = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          
          if((dim1s .LT. this%m_ims .OR. dim1e .GT. this%m_ime) .OR. &
             (dim2s .LT. this%m_jms .OR. dim2e .GT. this%m_jme)      ) then
              
              call check_dim2Di64_eq(dim1e,dim2e,this%m_ime,this%m_jme,msg_ar,file_path,3925)
          end if
             
         ret_subarr = this%m_p2d(dim1s:dim1e,dim2s:dim2e)
         
    end  function
    
    
    function  subarray_from_p8w2d(this,dim1s,dim1e,dim2s,dim2e)    result(ret_subarr)
          implicit none
          
          class(CAtmPressureField2D),   intent(in) :: this
          integer(i64),                 intent(in) :: dim1s
          integer(i64),                 intent(in) :: dim1e
          integer(i64),                 intent(in) :: dim2s
          integer(i64),                 intent(in) :: dim2e
          ! Locals
          real(R64), allocatable, dimension(:,:)   :: ret_subarr
          !DIR$ ATTRIBUTES_ALIGN : 32 :: ret_subarr
          character(len=*), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%subarray_from_p8w2d', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'dim1e       = ', &
                                                                'dim2e        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          
           if((dim1s .LT. this%m_ims .OR. dim1e .GT. this%m_ime) .OR. &
             (dim2s .LT. this%m_jms .OR. dim2e .GT. this%m_jme)      ) then
              
               call check_dim2Di64_eq(dim1e,dim2e,this%m_ime,this%m_jme,msg_ar,file_path,3957)
           end if
    
           ret_subarr = this%m_p8w2d(dim1s:dim1emdim2s:dim2e)
             
    end  function
    
    
    function  subarray_from_maskp2d(this,dim1s,dim1e,dim2s,dim2e)   result(ret_subarr)
          implicit none
         
          class(CAtmPressureField2D),   intent(in) :: this
          integer(i64),                 intent(in) :: dim1s
          integer(i64),                 intent(in) :: dim1e
          integer(i64),                 intent(in) :: dim2s
          integer(i64),                 intent(in) :: dim2e
          ! Locals
          logical(kind=8), allocatable, dimension(:,:) :: ret_subarr
          !DIR$ ATTRIBUTES_ALIGN : 32 :: ret_subarr
          character(len=*), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%subarray_from_maskp2d', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'dim1e      = ', &
                                                                'dim2e      = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          
           if((dim1s .LT. this%m_ims .OR. dim1e .GT. this%m_ime) .OR. &
              (dim2s .LT. this%m_jms .OR. dim2e .GT. this%m_jme)      ) then
              
               call check_dim2Di64_eq(dim1e,dim2e,this%m_ime,this%m_jme,msg_ar,file_path,3992)
           end if
             
           ret_subarr = this%m_maskp2d(dim1s:dim1e,dim2s:dim2e)  
    
    end  function
    
    
    subroutine  set_p2d_from_scalar(this,idx,jdx,val,set_all)
          implicit none
          class(CAtmPressureField2D),   intent(inout) :: this
          integer(i64),                 intent(in)    :: idx
          integer(i64),                 intent(in)    :: jdx
          real(R64),                    intent(in)    :: val
          logical(i32), optional,       intent(in)    :: set_all
          ! Local variables
          character(len=*), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%set_p2d_from_scalar', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable stataements
          
           if((idx .LT. this%m_ims  .OR. idx .GT. this%m_ime) .OR. &
              (jdx .LT. this%m_jms  .OR. jdx .GT. this%m_jme)      ) then
              
               call check_dim2Di64_eq(idx,jdx,this%m_ime,this%m_jme,msg_ar,file_path,4021)
           end  if
    
           if(present(set_all) .AND. (set_all .EQ. .true.)) then
              this%m_p2d = val
           else
               this%m_p2d(idx,jdx) = val
           end if
           
    
    end  subroutine
    
    
    subroutine  set_p8w2d_from_scalar(this,idx,jdx,val,set_all)
          implicit none
          class(CAtmPressureField2D),   intent(inout) :: this
          integer(i64),                 intent(in)    :: idx
          integer(i64),                 intent(in)    :: jdx
          real(R64),                    intent(in)    :: val
          logical(i32), optional,       intent(in)    :: set_all
          ! Locals
          character(len=*), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%set_p8w2d_from_scalar', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          
          if((idx .LT. this%m_ims  .OR. idx .GT. this%m_ime) .OR. &
             (jdx .LT. this%m_jms  .OR. jdx .GT. this%m_jme)      ) then
              
               call check_dim2Di64_eq(idx,jdx,this%m_ime,this%m_jme,msg_ar,file_path,4055)
          end  if
    
          if(present(set_all) .AND. (set_all .EQ. .true.)) then
             this%m_p8w2d = val
          else
              this%m_p8w2d(idx,jdx) = val
          end if
          
    end  subroutine
    
    
    subroutine  set_maskp2d_from_scalar(this,idx,jdx,val,set_all)
          implicit none
          class(CAtmPressureField2D),   intent(inout) :: this
          integer(i64),                 intent(in)    :: idx
          integer(i64),                 intent(in)    :: jdx
          logical(kind=8),              intent(in)    :: val
          logical(i32), optional,       intent(in)    :: set_all
          ! Local variables
          character(len=*), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%set_maskp2d_from_scalar', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statments
          
          if((idx .LT. this%m_ims  .OR. idx .GT. this%m_ime) .OR. &
             (jdx .LT. this%m_jms  .OR. jdx .GT. this%m_jme)      ) then
              
               call check_dim2Di64_eq(idx,jdx,this%m_ime,this%m_jme,msg_ar,file_path,4088)
          end  if
             
          if(present(set_all) .AND. (set_all .EQ. .true.)) then
              this%m_maskp2d = val
          else
              this%m_maskp2d(idx,jdx) = val
          end if
          
          
    end  subroutine
    
    
    subroutine  set_p2d_from_subarray(this,vdim1,vdim2,move_op)      ! Reimplement this!!
          implicit none
          class(CAtmPressureField2D),   intent(inout) :: this
          real(R64), dimension(:),      intent(in)    :: vdim1
          real(R64), dimension(:),      intent(in)    :: vdim2
          !DIR$ ASSUME_ALIGNED vdim1:32, vdim2:32
          logical(i32), optional,       intent(in)    :: move_op
          ! Locals
          real(R64), allocatable, dimension(:,:)      :: tmp
          !DIR$ ATTRIBUTES_ALIGN : 32 :: tmp
          character(len=*), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%set_p2d_from_subarray', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'size(vdim1)        = ', &
                                                                'size(vdim2)        = ', &
                                                                'size(m_p2d,dim=1)  = ', &
                                                                'size(m_p2d,dim=2)  = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          
          if((size(vdim1) .GT. size(this%m_p2d,dim=1)) .OR. (size(vdim2) .GT. size(this%m_p2d,dim=2))) then
                 call check_dim2Di64_eq(size(vdim1),size(vdim2),size(this%m_p2d,dim=1), &
                                        size(this%m_p2d,dim=2),msg_ar,file_path,4125)
          end if
    
         ! tmp = [vdim1(:),vdim2(:)]     error use reshape
          if(present(move_op) .AND. (move_op .EQ. .true.)) then
              call move_alloc(tmp,this%m_p2d)
          else
              this%m_p2d = tmp
          end if
    
    
    end  subroutine
    
    
    subroutine  set_p8w2d_from_subarray(this,vdim1,vdim2,move_op)
          implicit none
          class(CAtmPressureField2D),   intent(inout) :: this
          real(R64), dimension(:),      intent(in)    :: vdim1
          real(R64), dimension(:),      intent(in)    :: vdim2
          !DIR$ ASSUME_ALIGNED vdim1:32, vdim2:32
          logical(i32), optional,       intent(in)    :: move_op
          ! Local variables
          real(R64), allocatable, dimension(:,:)      :: tmp
          !DIR$ ATTRIBUTES_ALIGN : 32 :: tmp
          character(len=*), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%set_p8w2d_from_subarray', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'size(vdim1)        = ', &
                                                                'size(vdim2)        = ', &
                                                                'size(m_p2d,dim=1)  = ', &
                                                                'size(m_p2d,dim=2)  = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          
          if((size(vdim1) .GT. size(this%m_p8w2d,dim=1) .OR. (size(vdim2) .GT. size(this%m_p8w2d))) then
                  call check_dim2Di64_eq(size(vdim1),size(vdim2),size(this%m_p8w2d,dim=1), &
                                         size(this%m_p8w2d,dim=2),msg_ar,file_path,4164)
          end if
    
          tmp = [vdim1,vdim2]
          if(present(move_op) .AND. (move_op .EQ. .true.)) then
              call move_alloc(tmp,this%m_p8w2d)
          else
             this%m_p8w2d = tmp
          end if
    
    
    end  subroutine
    
    
    subroutine  set_maskp2d_from_subarray(this,vdim1,vdim2,move_op)
          implicit none
          class(CAtmPressureField2D),       intent(inout) :: this
          logical(kind=8),  dimension(:),   intent(in)    :: vdim1
          logical(kind=8),  dimension(:),   intent(in)    :: vdim2
          !DIR$ ASSUME_ALIGNED vdim1:32, vdim2:32
          logical(i32),     optional,       intent(in)    :: move_op
          ! Locals
          logical(kind=8),  allocatable, dimension(:,:)   :: tmp
          !DIR$ ATTRIBUTES_ALIGN : 32 :: tmp
          character(len=*), dimension(8) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField%set_maskp2d_from_subarray', &
                                                                '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'size(vdim1)        = ', &
                                                                'size(vdim2)        = ', &
                                                                'size(m_p2d,dim=1)  = ', &
                                                                'size(m_p2d,dim=2)  = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          
          if(size(vdim1) .GT. size(this%m_maskp2d,dim=1) .OR. (size(vdim2) .GT. size(this%m_maskp2d,dim=2))) then
                    call check_dim2Di64_eq(size(vdim1),size(vdim2),size(this%m_maskp2d,dim=1), &
                                           size(this%m_maskp2d,dim=2),msg_ar,file_path,4200)
          end if
          
          tmp = [vdim1,vdim2]
          if(present(move_op) .AND. (move_op .EQ. .true.)) then
              call move_alloc(tmp,this%m_maskp2d)
          else
              this%m_maskp2d = tmp
          end if
          
          
    end  subroutine
    
    
    pure  subroutine  omp_copy_arrays2d(p2din,p8w2din,maskp2din, &
                                        p2dout,p8w2dout,maskp2dout)
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:), intent(in)          :: p2din
          real(R64), contiguous, dimension(:,:), intent(in)          :: p8w2din
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: maskp2din
          real(R64), contiguous, dimension(:,:), intent(inout)       :: p2dout
          real(R64), contiguous, dimension(:,:), intent(inout)       :: p8w2dout
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: maskp2dout
          ! Start of executable statements
           
          !$OMP PARALLEL 
          !$OMP WORKSHARE
            p2dout     = p2din
            p8w2dout   = p8w2din
            maskp2dout = maskp2din
          !$OMP END  WORKSHARE
          !$OMP END  PARALLEL
          
    end  subroutine
     
    
    pure  subroutine  omp_add_arrays2d(p2din1,p8w2din1,maskp2din1, &
                                       p2din2,p8w2din2,maskp2din2, &
                                       p2dout,p8w2dout,maskp2dout   )
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din1
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: maskp2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din2
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din2
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: maskp2din2
          real(R64), contiguous, dimension(:,:),       intent(inout) :: p2dout
          real(R64), contiguous, dimension(:,:),       intent(inout) :: p8w2dout
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: maskp2dout
          ! Start of executable statements
          
          !$OMP PARALLEL
          !$OMP WORKSHARE
            p2dout     = p2din1   + p2din2
            p8w2dout   = p8w2din1 + p8w2din2
            maskp2dout = IOR(maskp2din1,maskp2din2)
          !$OMP END WORKSHARE
          !$OMP END PARALLEL
            
    end  subroutine
    
    
    pure  subroutine  omp_sub_arrays2d(p2din1,p8w2din1,mp2din1, &
                                       p2din2,p8w2din2,mp2din2, &
                                       p2dout.p8w2dout,mp2dout  )
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:), intent(in)          :: p2din1
          real(R64), contiguous, dimension(:,:), intent(in)          :: p8w2din1
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din1
          real(R64), contiguous, dimension(:,:), intent(in)          :: p2din2
          real(R64), contiguous, dimension(:,:), intent(in)          :: p8w2din2
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din2
          real(R64), contiguous, dimension(:,:), intent(inout)       :: p2dout
          real(R64), contiguous, dimension(:,:), intent(inout)       :: p8w2dout
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: mp2dout
          ! Start of executable statements
          
          !$OMP PARALLEL
          !$OMP WORKSHARE
            p2dout   =  p2din1   - p2din2
            p8w2dout =  p8w2din1 - p8w2din2
            mp2dout  =  IOR(mp2din1,mp2din2)
          !$OMP END WORKSHARE
          !$OMP END PARALLEL
    
    end  subroutine
    
    
    pure  subroutine  omp_mul_arrays2d(p2din1,p8w2din1,mp2din1, &
                                       p2din2,p8w2din2,mp2din2, &
                                       p2dout,p8w2dout,mp2dout   )
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:), intent(in)          :: p2din1
          real(R64), contiguous, dimension(:,:), intent(in)          :: p8w2din1
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din1
          real(R64), contiguous, dimension(:,:), intent(in)          :: p2din2
          real(R64), contiguous, dimension(:,:), intent(in)          :: p8w2din2
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din2
          real(R64), contiguous, dimension(:,:), intent(inout)       :: p2dout
          real(R64), contiguous, dimension(:,:), intent(inout)       :: p8w2dout
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: mp2dout
          ! Start of executable statements
    
          !$OMP PARALLEL
          !$OMP WORKSHARE
            p2dout   = p2din1   * p2din2
            p8w2dout = p8w2din1 * p8w2din2
            mp2dout  = IOR(mp2din1,mp2din2)
          !$OMP END WORKSHARE
          !$OMP END PARALLEL
            
            
    end  subroutine
    
    
    pure  subroutine  omp_real_mul_arrays2d(scalar,p2din,p8w2din,mp2din, &
                                            p2dout,p8w2dout,mp2dout      )
          implicit none
          use omp_lib
          real(R64),                                   intent(in)    :: scalar
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din
          real(R64), contiguous, dimension(:,:),       intent(inout) :: p2dout
          real(R64), contiguous, dimension(:,:),       intent(inout) :: p8w2dout
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: mp2dout
          ! Start of executable statements
          
          !$OMP PARALLEL
          !$OMP WORKSHARE
            p2dout   = scalar * p2din
            p8w2dout = scalar * p8w2din
            mp2dout  = mp2din
          !$OMP END WORKSHARE
          !$OMP END PARALLEL
            
    
    end  subroutine
    
    
    pure  subroutine  omp_arrays2d_mul_real(scalar,p2din,p8w2din,mp2din, &
                                            p2dout,p8w2dout,mp2dout      )
          implicit none
          use omp_lib
          real(R64),                                   intent(in)    :: scalar
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din
          real(R64), contiguous, dimension(:,:),       intent(inout) :: p2dout
          real(R64), contiguous, dimension(:,:),       intent(inout) :: p8w2dout
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: mp2dout
          ! Start of executable statements
          
          !$OMP PARALLEL
          !$OMP WORKSHARE
            p2dout   = p2din   * scalar
            p8w2dout = p8w2din * scalar
            mp2dout  = mp2din
          !$OMP END WORKSHARE
          !$OMP END PARALLEL
    
    
    end  subroutine
    
    
    pure  subroutine  omp_int_mul_arrays2d(scalar,p2din,p8w2din,mp2din, &
                                           p2dout,p8w2dout,mp2dout      )
          implicit none
          use omp_lib
          integer(i64),                                 intent(in)    :: scalar
          real(R64), contiguous, dimension(:,:),        intent(in)    :: p2din
          real(R64), contiguous, dimension(:,:),        intent(in)    :: p8w2din
          logical(kind=8), contiguous, dimension(:,:),  intent(in)    :: mp2din
          real(R64), contiguous, dimension(:,:),        intent(inout) :: p2dout
          real(R64), contiguous, dimension(:,:),        intent(inout) :: p8w2dout
          logical(kind=8), contiguous, dimension(:,:),  intent(inout) :: mp2dout
          ! Start of executable statements
          
          !$OMP PARALLEL
          !$OMP WORKSHARE
            p2dout   = REAL(scalar) * p2din
            p8w2dout = REAL(scalar) * p8w2din
            mp2dout  = mp2din
          !$OMP END WORKSHARE
          !$OMP END PARALLEL
    
    
    end  subroutine
    
    
    pure  subroutine  omp_arrays2d_mul_int(scalar,p2din,p8w2din,mp2din, &
                                           p2dout,p8w2dout,mp2dout      )
          implicit none
          use omp_lib
          integer(i64),                                 intent(in)    :: scalar
          real(R64), contiguous, dimension(:,:),        intent(in)    :: p2din
          real(R64), contiguous, dimension(:,:),        intent(in)    :: p8w2din
          logical(kind=8), contiguous, dimension(:,:),  intent(in)    :: mp2din
          real(R64), contiguous, dimension(:,:),        intent(inout) :: p2dout
          real(R64), contiguous, dimension(:,:),        intent(inout) :: p8w2dout
          logical(kind=8), contiguous, dimension(:,:),  intent(inout) :: mp2dout
          ! Start of executable statements
    
          !$OMP PARALLEL
          !$OMP WORKSHARE
            p2dout   = p2din   * DBLE(scalar)
            p8w2dout = p8w2din * DBLE(scalar)
            mp2dout  = mp2din
          !$OMP END WORKSHARE
          !$OMP END PARALLEL
            
            
    end  subroutine
    
    
    pure  subroutine  omp_div_arrays2d(p2din1,p8w2din1,mp2din1, &
                                       p2din2,p8w2din2,mp2din2, &
                                       p2dout,p8w2dout,mp2dout  )
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:), intent(in)          :: p2din1
          real(R64), contiguous, dimension(:,:), intent(in)          :: p8w2din1
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din1
          real(R64), contiguous, dimension(:,:), intent(in)          :: p2din2
          real(R64), contiguous, dimension(:,:), intent(in)          :: p8w2din2
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din2
          real(R64), contiguous, dimension(:,:), intent(inout)       :: p2dout
          real(R64), contiguous, dimension(:,:), intent(inout)       :: p8w2dout
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: mp2dout
          ! Start of executable statements
    
          !$OMP PARALLEL
          !$OMP WORKSHARE
            p2dout   = p2din1   / p2din2
            p8w2dout = p8w2din1 / p8w2din2
            mp2dout  = IOR(mp2din1,mp2din2)
          !$OMP END WORKSHARE
          !$OMP END PARALLEL
            
            
    end  subroutine
    
    
    pure  subroutine  omp_arrays2d_div_real(scalar,p2din,p8w2din,mp2din, &
                                            p2dout,p8w2dout,mp2dout      )
          implicit none
          use omp_lib
          real(R64),                                   intent(in)    :: scalar
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din
          real(R64), contiguous, dimension(:,:),       intent(inout) :: p2dout
          real(R64), contiguous, dimension(:,:),       intent(inout) :: p8w2dout
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: mp2dout
          ! Start of executable statements
                   
          !$OMP PARALLEL
          !$OMP WORKSHARE
            p2dout   = p2din   / scalar
            p8w2dout = p8w2din / scalar
            mp2dout  = mp2din
          !$OMP END WORKSHARE
          !$OMP END PARALLEL
    
    
    end  subroutine
    
    
    pure  subroutine  omp_arrays2d_div_int(scalar,p2din,p8w2din,mp2din, &
                                           p2dout,p8w2dout,mp2dout      )
          implicit none
          use omp_lib
          integer(i64),                                 intent(in)    :: scalar
          real(R64), contiguous, dimension(:,:),        intent(in)    :: p2din
          real(R64), contiguous, dimension(:,:),        intent(in)    :: p8w2din
          logical(kind=8), contiguous, dimension(:,:),  intent(in)    :: mp2din
          real(R64), contiguous, dimension(:,:),        intent(inout) :: p2dout
          real(R64), contiguous, dimension(:,:),        intent(inout) :: p8w2dout
          logical(kind=8), contiguous, dimension(:,:),  intent(inout) :: mp2dout
          ! Start of executable statements
    
          !$OMP PARALLEL
            !$OMP WORKSHARE
            p2dout   = p2din   / REAL(scalar,R64)
            p8w2dout = p8w2din / REAL(scalar,R64)
            mp2dout  = mp2din
            !$OMP END WORKSHARE
          !$OMP END PARALLEL
            
            
    end  subroutine
    
    
    pure  subroutine  omp_arrays2d_pow_real(scalar,p2din,p8w2din,mp2din, &
                                            p2dout,p8w2dout,mp2dout     )
          implicit none
          use omp_lib
          real(R64),                                    intent(in)    :: scalar
          real(R64), contiguous, dimension(:,:),        intent(in)    :: p2din
          real(R64), contiguous, dimension(:,:),        intent(in)    :: p8w2din
          logical(I64), contiguous, dimension(:,:),     intent(in)    :: mp2din
          real(R64), contiguous, dimension(:,:),        intent(inout) :: p2dout
          real(R64), contiguous, dimension(:,:),        intent(inout) :: p8w2dout
          logical(I64), contiguous, dimension(:,:),     intent(inout) :: mp2dout
          ! Start of executable statements
          
          !$OMP  PARALLEL
             !$OMP WORKSHARE
              p2dout   = p2din   ** scalar
              p8w2dout = p8w2din ** scalar
              mp2dout  = mp2din
              !$OMP END WORKSHARE
           !$OMP END PARALLEL
              
              
    end  subroutine
    
    
    pure  subroutine  omp_arrays2d_pow_int(scalar,p2din,p8w2din,mp2din, &
                                           p2dout,p8w2dout,mp2dout     )
          implicit none
          use omp_lib
          integer(I64),                             intent(in) :: scalar
          real(R64), contiguous, dimension(:,:),        intent(in)    :: p2din
          real(R64), contiguous, dimension(:,:),        intent(in)    :: p8w2din
          logical(I64), contiguous, dimension(:,:),     intent(in)    :: mp2din
          real(R64), contiguous, dimension(:,:),        intent(inout) :: p2dout
          real(R64), contiguous, dimension(:,:),        intent(inout) :: p8w2dout
          logical(I64), contiguous, dimension(:,:),     intent(inout) :: mp2dout
          ! Start of executable statements
    
          !$OMP  PARALLEL
              !$OMP WORKSHARE
                p2dout   = p2din   ** scalar
                p8w2dout = p8w2din ** scalar
                mp2dout  = mp2din
               !$OMP END WORKSHARE
          !$OMP END PARALLEL
                
          
    end  subroutine
    
    
    subroutine  omp_arrays2d_eq(p2din1,p8w2din1,mp2din1, &
                                      p2din2,p8w2din2,mp2din2, &
                                      bp2d,bp8w2d,bmp2d,fpcmp, &
                                      ulp,ims,jms        )
                                                       
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din1
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din2
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din2
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din2
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp8w2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bmp2d
          character(len=*),                            intent(in)    :: fpcmp
          integer(I32),                                intent(in)    :: ulp
          
          integer(I64),                                intent(in)    :: ims
          integer(I64),                                intent(in)    :: jms
          ! Locals
          integer(I64)                                               :: i,j
          real(R64)                                                  :: relative
          ! Start of executable statements
          
          FP_COMP_TYPE:   select case(fpcmp)
                          case ("NAIVE")
                              !$OMP PARALLEL
                                  !$OMP WORKSHARE
                                      bp2d   = p2din1   == p2din2
                                      bp8w2d = p8w2din1 == p8w2din2
                                      bmp2d  = mp2din1  == mp2din2
                                  !$OMP END WORKSHARE
                              !$OMP END PARALLEL 
                          case ("FLOAT_TOLERANCE")
                               !$OMP PARALLEL
                                   !$OMP DO PRIVATE(j,i) SHARED(bp2d,p2din1,p2din2, &
                                                    bp8w2d,p8w2din1,p8w2din2,bmp2d, &
                                                    mp2din1,mp2din2) SCHEDULE(STATIC)
                                do j = jms, SIZE(bp2d,dim=2)
                                   do i = ims, SIZE(bp2d,dim=1)
                                      bp2d(i,j)   = ABS(p2din1(i,j)-p2din2(i,j)) < SPACING(MAX(ABS(p2din1(i,j),      &
                                                                                             ABS(p2din2(i,j))))) 
                                      bp8w2d(i,j) = ABS(p8w2din1(i,j)-p8w2din2(i,j)) < SPACING(MAX(ABS(p8w2din1(i,j), &
                                                                                             ABS(p8w2din2(i,j)))))
                                      bmp2d(i,j)  = mp2din1(i,j) == mp2din2(i,j)
                                   end do
                                end do
                                   !$OMP END DO
                                !$OMP END PARALLEL
                          case ("ADJUST_PRECISION_TOLERANCE")
                                relative = DBLE(ABS(ulp))
                                !$OMP PARALLEL
                                   !$OMP DO PRIVATE(j,i) SHARED(bp2d,p2din1,p2din2, &
                                                    bp8w2d,p8w2din1,p8w2din2,bmp2d, &
                                                    mp2din1,mp2din2) SCHEDULE(STATIC)
                                   do j = jms, SIZE(bp2d,dim=2)
                                       do i = ims, SIZE(bp2d,dim=1)
                                           bp2d(i,j)   = ABS(p2din1(i,j)-p2din2(i,j)) < (relative*SPACING(MAX(ABS(p2din1(i,j),      &
                                                                                                            ABS(p2din2(i,j))))))
                                           bp8w2d(i,j) = ABS(p8w2din1(i,j)-p8w2din2(i,j)) < (relative*SPACING(MAX(ABS(p8w2din1(i,j), &
                                                                                                            ABS(p8w2din2(i,j))))))
                                           bmp2d(i,j) = mp2din1(i,j) == mp2din2(i,j)
                                       end do
                                   end do
                                   !$OMP END DO
                                !$OMP END PARALLEL
                          case default
                            ! Default case , return arrays set to .false.
                            bp2d   = .false.
                            bp8w2d = .false.
                            bmp2d  = .false.
                         
                          end select  FP_COMP_TYPE
                          
    
                                   
    end  subroutine
    
    
    subroutine  omp_arrays2d_neq(p2din1,p8w2din1,mp2din1, &
                                       p2din2,p8w2din2,mp2din2, &
                                       bp2d,bp8w2d,bmp2d,fpcmp, &
                                       ulp,ims,jms              )
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din1
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din2
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din2
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din2
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp8w2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bmp2d
          character(len=*),                            intent(in)    :: fpcmp
          integer(I32),                                intent(in)    :: ulp
          integer(I64),                                intent(in)    :: ims
          integer(I64),                                intent(in)    :: jms
          ! Locals
          integer(I64)                                               :: i,j
          real(R64)                                                  :: relative
          ! Start of executable statements
    
   FP_COMP_TYPE: select case (fpcmp)
                 case ("NAIVE")
                  !$OMP PARALLEL
                     !$OMP WORKSHARE
                        bp2d   = p2din1   /= p2din2
                        bp8w2d = p8w2din1 /= p8w2din2
                        bmp2d  = mp2din1  /= mp2din2
                     !$OMP END WORKSHARE
                  !$OMP END PARALLEL
                 case ("FLOAT_TOLERANCE")
                    !$OMP PARALLEL
                       !$OMP DO PRIVATE(j,i) SHARED(bp2d,p2din1,p2din2, &
                                                    bp8w2d,p8w2din1,p8w2din2,bmp2d, &
                                                    mp2din1,mp2din2) SCHEDULE(STATIC)
                                do j = jms, SIZE(bp2d,dim=2)
                                   do i = ims, SIZE(bp2d,dim=1)
                                      bp2d(i,j)   = ABS(p2din1(i,j)-p2din2(i,j)) > SPACING(MAX(ABS(p2din1(i,j),      &
                                                                                             ABS(p2din2(i,j))))) 
                                      bp8w2d(i,j) = ABS(p8w2din1(i,j)-p8w2din2(i,j)) > SPACING(MAX(ABS(p8w2din1(i,j), &
                                                                                             ABS(p8w2din2(i,j)))))
                                      bmp2d(i,j)  = mp2din1(i,j) /= mp2din2(i,j)
                                   end do
                                end do
                       !$OMP END DO
                    !$OMP END PARALLEL 
                 case ("ADJUST_PRECISION_TOLERANCE")
                      relative = DBLE(ABS(ulp))
                    !$OMP PARALLEL
                       !$OMP DO PRIVATE(j,i) SHARED(bp2d,p2din1,p2din2, &
                                                    bp8w2d,p8w2din1,p8w2din2,bmp2d, &
                                                    mp2din1,mp2din2) SCHEDULE(STATIC)
                                   do j = jms, SIZE(bp2d,dim=2)
                                       do i = ims, SIZE(bp2d,dim=1)
                                           bp2d(i,j)   = ABS(p2din1(i,j)-p2din2(i,j)) > (relative*SPACING(MAX(ABS(p2din1(i,j),      &
                                                                                                            ABS(p2din2(i,j))))))
                                           bp8w2d(i,j) = ABS(p8w2din1(i,j)-p8w2din2(i,j)) > (relative*SPACING(MAX(ABS(p8w2din1(i,j), &
                                                                                                            ABS(p8w2din2(i,j))))))
                                           bmp2d(i,j) = mp2din1(i,j) /= mp2din2(i,j)
                                       end do
                                   end do
                       !$OMP END DO
                    !$OMP END PARALLEL
                 case default
                   ! Default case return result arrays set to false
                   bp2d   = .false.
                   bp8w2d = .false.
                   bmp2d  = .false.
                 end select FP_COMP_TYPE
                 
            
    end  subroutine
    
    
      subroutine  omp_arrays2d_lt(p2din1,p8w2din1,mp2din1, &
                                      p2din2,p8w2din2,mp2din2, &
                                      bp2d,bp8w2d,bmp2d,fpcmp, &
                                      ims,jms                 )
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din1
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din2
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din2
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din2
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp8w2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bmp2d
          character(len=*),                            intent(in)    :: fpcmp
          integer(I64),                                intent(in)    :: ims
          integer(I64),                                intent(in)    :: jms
          ! Locals
          integer(I64)                                               :: i,j
          ! Start of executable statements
     FP_COMP_TYPE:  select case (fpcmp)
                    case ("NAIVE")
                      !$OMP PARALLEL
                         !$OMP WORKSHARE
                             bp2d   = p2din1   < p2din2
                             bp8w2d = p8w2din1 < p8w2din2
                             bmp2d  = mp2din1  < mp2din2
                         !$OMP END WORKSHARE
                      !$OMP END PARALLEL
                    case ("EPSILON_TOLERANCE")
                        !$OMP PARALLEL
                            !$OMP DO PRIVATE(j,i) SHARED(bp2d,p2din1,p2din2,bp8w2d, &
                                                  p8w2din1,p8w2din2,bmp2d,mp2din1,mp2din2) &
                                                  SCHEDULE(STATIC)
                                do j = jms, SIZE(bp2d,dim=2)
                                    do i = ims, SIZE(bp2d,dim=1)
                                        if((p2din2(i,j)-p2din1(i,j)) > SPACING(MAX(ABS(p2din1(i,j)),ABS(p2din2(i,j)))))) then
                                            bp2d(i,j) = .true.
                                        else
                                            bp2d(i,j) = .false.
                                        end if
                                        if((p8w2din2(i,j)-p8w2din1(i,j)) > SPACING(MAX(ABS(p8w2din1(i,j)),ABS(p8w2din2(i,j)))))) then
                                            bp8w2d(i,j) = .true.
                                        else
                                            bp8w2d(i,j) = .false.
                                        end if
                                        bmp2d(i,j) = mp2din1(i,j) < mp2din2(i,j)
                                     end do
                                end do
                            !$OMP END DO
                        !$OMP END PARALLEL
                    case default
                        bp2d   = .false.
                        bp8w2d = .false.
                        bmp2d  = .false.
                    end select FP_COMP_TYPE
                    
    
    end  subroutine
    
    
    subroutine  omp_arrays2d_gt(p2din1,p8w2din1,mp2din1, &
                                      p2din2,p8w2din2,mp2din2, &
                                      bp2d,bp8w2d,bmp2d,fpcmp, &
                                      ims,jms                   )
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din1
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din2
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din2
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din2
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp8w2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bmp2d
          character(len=*),                            intent(in)    :: fpcmp
          integer(I64),                                intent(in)    :: ims
          integer(I64),                                intent(in)    :: jms
          ! Locals
          integer(I64)                                               :: i,j
          ! Start of executable statements
     FP_COMP_TYPE:  select case (fpcmp)
                    case ("NAIVE")
                      !$OMP PARALLEL
                         !$OMP WORKSHARE
                             bp2d   = p2din1   > p2din2
                             bp8w2d = p8w2din1 > p8w2din2
                             bmp2d  = mp2din1  > mp2din2
                         !$OMP END WORKSHARE
                      !$OMP END PARALLEL
                    case ("EPSILON_TOLERANCE")
                        !$OMP PARALLEL
                            !$OMP DO PRIVATE(j,i) SHARED(bp2d,p2din1,p2din2,bp8w2d, &
                                                p8w2din1,p8w2din2,bmp2d,mp2din1,mp2din2) &
                                                SCHEDULE(STATIC)
                             do j = jms, SIZE(bp2d,dim=2)
                                 do i = ims, SIZE(bp2d,dim=1)
                                     if((p2din1(i,j)-p2din2(i,j)) > SPACING(MAX(ABS(p2din1(i,j)),ABS(p2din2(i,j))))) then
                                         bp2d(i,j) = .true.
                                     else
                                         bp2d(i,j) = .false.
                                     end if
                                     if((p8w2din1(i,j)-p8w2din2(i,j)) > SPACING(MAX(ABS(p8w2din1(i,j)),ABS(p8w2din2(i,j))))) then
                                         bp8w2d(i,j) = .true.
                                     else
                                         bp8w2d(i,j) = .false.
                                     end if
                                     bmp2d(i,j) = mp2din1(i,j) > mp2din2(i,j)
                                 end do
                             end do
                            !$OMP END DO
                        !$OMP END PARALLEL
                    case default
                      bp2d   = .false.
                      bp8w2d = .false.
                      bmp2d  = .false.
                    end select FP_COMP_TYPE
    
    end  subroutine
    
    
    pure  subroutine  omp_arrays2d_le(p2din1,p8w2din1,mp2din1, &
                                      p2din2,p8w2din2,mp2din2, &
                                      bp2d,bp8w2d,bmp2d,fpcmp, &
                                      ims,jms                   )
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din1
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din2
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din2
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din2
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp8w2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bmp2d
          character(len=*),                            intent(in)    :: fpcmp
          integer(I64),                                intent(in)    :: ims
          integer(I64),                                intent(in)    :: jms
          ! Locals
          integer(I64)                                               :: i,j
          ! Start of executable statements
          
    FP_COMP_TYPE:  select case (fpcmp)
                   case ("NAIVE")
                     !$OMP PARALLEL
                         !$OMP WORKSHARE
                            bp2d   = p2din1   <= p2din2
                            bp8w2d = p8w2din1 <= p8w2din2
                            bmp2d  = mp2din1  <= mp2din2
                         !$OMP END WORKSHARE
                     !$OMP END PARALLEL
                   case ("EPSILON_TOLERANCE")
                      !$OMP PARALLEL
                         !$OMP DO PRIVATE(j,i) SHARED(bp2d,p2din1,p2din2,bp8w2d,         &
                                                p8w2din1,p8w2din2,bmp2d,mp2din1,mp2din2) &
                                                SCHEDULE(STATIC)
                             do j = jms, SIZE(bp2d,dim=2)
                                 do i = ims, SIZE(bp2d,dim=1)
                                     if((p2din2(i,j)-p2din1(i,j)) >= SPACING(MAX(ABS(p2din1(i,j)),ABS(p2din1(i,j)))))  then
                                         bp2d(i,j) = .true.
                                     else
                                         bp2d(i,j) = .false.
                                     end if
                                     if((p8w2din2(i,j)-p8w2din1(i,j)) >= SPACING(MAX(ABS(p8w2din1(i,j)),ABS(p8w2din2(i,j))))) then
                                         bp8w2d(i,j) = .true.
                                     else
                                         bp8w2d(i,j) = .false.
                                     end if
                                     bmp2d(i,j) = mp2din1(i,j) <= mp2din2(i,j)
                                 end do
                             end do
                         !$OMP END DO
                      !$OMP END PARALLEL
                   case default
                        bp2d   = .false.
                        bp8w2d = .false.
                        bmp2d  = .false.
                   end select FP_COMP_TYPE
            
    
    end  subroutine
    
    
    pure  subroutine  omp_arrays2d_ge(p2din1,p8w2din1,mp2din1, &
                                      p2din2,p8w2din2,mp2din2, &
                                      bp2d,bp8w2d,bmp2d,fpcmp, &
                                      ims,jms               )
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din1
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din1
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p2din2
          real(R64), contiguous, dimension(:,:),       intent(in)    :: p8w2din2
          logical(kind=8), contiguous, dimension(:,:), intent(in)    :: mp2din2
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bp8w2d
          logical(kind=8), contiguous, dimension(:,:), intent(inout) :: bmp2d
          character(len=*),                            intent(in)    :: fpcmp
          integer(I64),                                intent(in)    :: ims
          integer(I64),                                intent(in)    :: jms
          ! Locals
          integer(I64)                                               :: i,j
          ! Start of executable statements
     
     FP_COMP_TYPE:  select case (fpcmp)
                    case ("NAIVE")
                     !$OMP PARALLEL
                        !$OMP WORKSHARE
                           bp2d   = p2din1   >= p2din2
                           bp8w2d = p8w2din1 >= p8w2din2
                           bmp2d  = mp2din1  >= mp2din1
                        !$OMP END WORKSHARE
                     !$OMP END PARALLEL
                    case ("EPSILON_TOLERANCE")
                      !$OMP PARALLEL
                        !$OMP DO PRIVATE(j,i) SHARED(bp2d,p2din1,p2din2,bp8w2d,          &
                                                p8w2din1,p8w2din2,bmp2d,mp2din1,mp2din2) &
                                              SCHEDULE(STATIC)
                            do j = jms, SIZE(bp2d,dim=2)
                                do i = ims, SIZE(bp2d,dim=1)
                                    if((p2din1(i,j)-p2din2(i,j)) >= SPACING(MAX(ABS(p2din1(i,j)),ABS(p2din2(i,j))))) then
                                        bp2d(i,j) = .true.
                                    else
                                        bp2d(i,j) = .false.
                                    end if
                                    if((p8w2din1(i,j)-p8w2din2(i,j)) >= SPACING(MAX(ABS(p8w2din1(i,j)),ABS(p8w2din2(i,j))))) then
                                        bp8w2d(i,j) = .true.
                                    else
                                        bp8w2d(i,j) = .false.
                                    end if
                                    bmp2d(i,j) = mp2din1(i,j) >= mp2din2(i,j)
                                end do
                            end do
                        !$OMP END DO
                      !$OMP END PARALLEL
                    case default
                        bp2d   = .false.
                        bp8w2d = .false.
                        bmp2d  = .false.
                    end select FP_COMP_TYPE
                    
    end  subroutine
    
    
    subroutine  check_arrays2d_denorm(p2din1,p8w2din1, &
                                      p2din2,p8w2din2, &
                                      ims,ime,jms,jme )
          implicit none
          use omp_lib
          real(R64), contiguous, dimension(:,:), intent(inout) :: p2din1
          real(R64), contiguous, dimension(:,:), intent(inout) :: p8w2din1
          real(R64), contiguous, dimension(:,:), intent(inout) :: p2din2
          real(R64), contiguous, dimension(:,:), intent(inout) :: p8w2din2
          integer(I64),                          intent(in)    :: ims
          integer(I64),                          intent(in)    :: ime
          integer(I64),                          intent(in)    :: jms
          integer(I64),                          intent(in)    :: jme
          ! Locals
          integer(I64)                                         :: i,j
          real(R64), parameter                                 :: press = TINY_PRESSURE
          ! Start of executable statements
          
          !$OMP PARALLEL SHARED(p2din1,p8w2din1,p2din2,p8w2din2,press)
             !$OMP DO PRIVATE(j,i)   SCHEDULE(STATIC)
                do j = jms, jme
                    do i = ims, ime
                        if(ABS(p2din1(i,j)) .LT. press) then
                            p2din1(i,j)   = press
                        else if(ABS(p8w2din1(i,j)) .LT. press) then
                            p8w2din1(i,j) = press
                        else if(ABS(p2din2(i,j)) .LT. press) then
                            p2din2(i,j)   = press
                        else if(ABS(p8w2din2(i,j)) .LT. press) then
                            p8w2din2(i,j)  = press
                        end if
                     end do
                end do
           !$OMP END DP
         !$OMP END PARALLEL
    
    end  subroutine
    
    
    subroutine  check_arrays2d_denorm2(p2din,p8w2din, &
                                       ims,ime,jms,jme )
          implicit none
          real(R64), contiguous, dimension(:,:), intent(inout) :: p2din
          real(R64), contiguous, dimension(:,:), intent(inout) :: p8w2din
          integer(I64),                          intent(in)    :: ims
          integer(I64),                          intent(in)    :: ime
          integer(I64),                          intent(in)    :: jms
          integer(I64),                          intent(in)    :: jme
          ! Locals
          integer(I64)                                         :: i,j
          real(R64), parameter                                 :: press = TINY_PRESSURE
    
          ! Start of executable statements
          
          !$OMP PARALLEL SHARED(p2din,p8w2din,press) 
                !$OMP DO PRIVATE(j,i) SCHEDULE(STATIC)
                   do j = jms, jme
                       do i = ims, ime
                           if(ABS(p2din(i,j)) .LT. press)  then
                               p2din(i,j) = press
                           else if(ABS(p8w2din(i,j)) .LT. press) then
                               p8w2din(i,j) = press
                           end if
                       end do
                   end do
                 !$OMP END DO
          !$OMP END PARALLEL
          
    end  subroutine
    
    
    subroutine  nonconforming_arrays2d(this,other,print_details,file,fname,line)
          implicit none
          use ISO_FORTRAN_ENV, only : ERROR_UNIT,OUTPUT_UNIT
          class(CAtmPressureField2D),   intent(in) :: this
          class(CAtmPressureField2D),   intent(in) :: other
          logical(I32), optional,       intent(in) :: print_details
          character(len=*),             intent(in) :: file
          character(len=*),             intent(in) :: fname
          integer(I32),                 intent(in) :: line
          ! Start of executable statements
    
          if(SIZE(this%m_p2d,dim=1)     /= SIZE(other%m_p2d,dim=1)     .OR. &
             SIZE(this%m_p2d,dim=2)     /= SIZE(other%m_p2d,dim=2)     .OR. &
             SIZE(this%m_p8w2d,dim=1)   /= SIZE(other%m_p8w2d,dim=1)   .OR. &
             SIZE(this%m_p8w2d.dim=2)   /= SIZE(other%m_p8w2d,dim=2)   .OR. &
             SIZE(this%m_maskp2d,dim=1) /= SIZE(other%m_maskp2d,dim=1) .OR. &
             SIZE(this%m_maskp2d,dim=2) /= SIZE(other%m_maskp2d,dim=2)     ) then
              
                  if(present(print_details) .AND. (print_details .EQ. .true.)) then
                      write(ERROR_UNIT,*) '[FATAL-ERROR] --> Nonconforming Arrays!!'
                      write(ERROR_UNIT,10) line
10                    format('At line: ', I24.20)
                      print *, 'In file: ', file
                      print *, 'In function:', fname
                      write(ERROR_UNIT,*) '--------------------------------------'
                      write(ERROR_UNIT,*) '  Checking dimension conformance.'
                      write(ERROR_UNIT,20) SIZE(this%m_p2d,dim=1)-SIZE(other%m_p2d,dim=1)
20                    format('m_p2d,dim=1 conform: ', I24.20)
                      write(ERROR_UNIT,30) SIZE(this%m_p2d,dim=2)-SIZE(other%m_p2d,dim=2)
30                    format('m_p2d,dim=2 conform:  ', I24.20)
                      write(ERROR_UNIT,40) SIZE(this%m_p8w2d,dim=1)-SIZE(other%m_p8w2d,dim=1)
40                    format('m_p8w2d,dim=1 conform: ',I24.20)
                      write(ERROR_UNIT,50) SIZE(this%m_p8w2d,dim=2)-SIZE(other%m_p8w2d,dim=2)
50                    format('m_p8w2d,dim=2 conform: ',I24.20)
                      write(ERROR_UNIT,60) SIZE(this%m_maskp2d,dim=1)-SIZE(this%m_maskp2d,dim=1)
60                    format('m_maskp2d,dim=1 conform:',I24.20)
                      write(ERROR_UNIT,70) SIZE(this%m_maskp2d,dim=2)-SIZE(this%m_maskp2d,dim=2)
70                    format('m_maskp2d,dim=2 conform:',I24.20)
                      write(ERROR_UNIT,*) ' End of check: Terminating Execution.'
                      write(ERROR_UNIT,*) '--------------------------------------'
                  end if
                  write(ERROR_UNIT,*) ' Caller decided on detail skipping-- Terminating Execution!!'
                  ERROR STOP '[FATAL-ERROR] --> Nonconforming Arrays!!'
             else
                  write(OUTPUT_UNIT,*) '-----------------------------------------------'
                  write(OUTPUT_UNIT,*)   'Finished check: Conforming Arrays Detected!!'
                  write(OUTPUT_UNIT,*) '-----------------------------------------------'
             end if
             
    
    end  subroutine
    
    
    subroutine  alloc2d_error_handler(error,msg,file,fname,line)
          implicit none
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          integer(I32),     intent(in) :: error
          character(len=*), intent(in) :: msg
          character(len=*), intent(in) :: file
          character(len=*), intent(in) :: fname
          integer(I32),     intent(in) :: line
          
          ! Start of executable statements
          
          write(ERROR_UNIT,*) '----------------------------------------------'
          write(ERROR_UNIT,*) ' [FATAL-ERROR]: Memory Allocation Failure!!'
          write(ERROR_UNIT,*) '----------------------------------------------'
          write(ERROR_UNIT,*) ' In file: ', file
          write(ERROR_UNIT,*) ' In function: ', fname
          write(ERROR_UNIT,*) ' At line: ', line
          write(ERROR_UNIT,*) ' STAT = ', error
          write(ERROR_UNIT,*) ' ERRMSG: ', msg
          write(ERROR_UNIT,*) ' Terminating Execution!!'
          ERROR STOP ' FATAL --> Memory Allocation Failure!!'
    
    end  subroutine
    
    
    subroutine  print_state_field2D(this,tf)
         
          use ISO_FORTRAN_ENV, only : OUTPUT_UNIT,ERROR_UNIT
          implicit none
          class(CAtmPressureField2D),    intent(in) :: this
          character(len=*),              intent(in) :: tf
          ! Locals
          integer(i32),      parameter              :: rc_len = 3
          integer(i32),      parameter              :: dt_len = 8
          character(len=12), dimension(rc_len)      :: rc
          integer(i32),      dimension(dt_len)      :: dt
          integer(i64)                              :: i
          integer(i64)                              :: j
          ! Start of executable statements
          
          if(.NOT. allocated(this%m_p2d)   .OR. &
             .NOT. allocated(this%m_p8w2d) .OR. &
             .NOT. allocated(this%m_maskp2d)   ) then
              
              write(ERROR_UNIT,*) '[FATAL-ERROR] in: print_state_field2D!!'
              write(ERROR_UNIT,*) 'Object in inconsistent state - deallocated arrays!!'
              write(ERROR_UNIT,*) ' Executing early return!!'
              return
         end if
             
          call DATE_AND_TIME(rc(1),rc(2),rc(3),dt)
          print*, '***** CAtmPressureField2D ***** -- STATE DUMP!!'
          select case(tf)
          case("string")
              print*, "Date of collection:", rc(1)
              print*, "Time of collection:", rc(2)
              print*, "UTC Delta:", rc(3)
          case("integral")
              print*, 'Date of collection: ', dt(1), '-', dt(2), '-', dt(3)
              print*, 'UTC-DELTA: ', dt(4)
              print*, 'Time of collection: ', dt(5), ':', dt(6), ':', dt(7), ':', dt(8)
          case default
              print*, "[ERROR]: Invalid argument has been passed to choose time format!!"
          end select
          print*, 'Scalar members: 
          write(OUTPUT_UNIT,10) this%m_ims
10        format('this%m_ims=', I24.20)
          write(OUTPUT_UNIT,20) this%m_ime
20        format('this%m_ime=', I24.20)
          write(OUTPUT_UNIT,30) this%m_jms
30        format('this%m_jms=', I24.20)
          write(OUTPUT_UNIT,40) this%m_jme
40        format('this%m_jme=', I24.20)
          write(OUTPUT_UNIT,50) this%m_ids
50        format('this%m_ids=', I24.20)
          write(OUTPUT_UNIT,60) this%m_ide
60        format('this%m_ide=', I24.20)
          write(OUTPUT_UNIT,70) this%m_jds
70        format('this%m_jds=', I24.20)
          write(OUTPUT_UNIT,80) this%m_jde
80        format('this%m_jde=', I24.20)
          write(OUTPUT_UNIT,90) this%m_its
90        format('this%m_its=', I24.20)
          write(OUTPUT_UNIT,100) this%m_ite
100       format('this%m_ite=', I24.20)
          write(OUTPUT_UNIT,110) this%m_jts
110       format('this%m_jts=', I24.20)
          write(OUTPUT_UNIT,120) this%m_jte
120       format('this%m_jte=', I24.20)
          write(OUTPUT_UNIT,130) this%m_m1s
130       format('this%m_m1s=', I24.20)
          write(OUTPUT_UNIT,140) this%m_m1e
140       format('this%m_m1e=', I24.20)
          write(OUTPUT_UNIT,150) this%m_m2s
150       format('this%m_m2s=', I24.20)
          write(OUTPUT_INIT,160) this%m_m2e
160       format('this%m_m2e=', I24.20)
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
          write(OUTPUT_UNIT,170) this%m_is_initialized
170       format('this%m_is_initialized=', L)
!DIR$ ENDIF
          write(OUTPUT_UNIT,180) this%m_ctor_flags(1)
180       format('this%m_ctor_flags(1)=', L)
          write(OUTPUT_UNIT,190) this%m_ctor_flags(2)
190       format('this%m_ctor_flags(2)=', L)
          write(OUTPUT_UNIT,200) this%m_ctor_flags(3)
200       format('this%m_ctor_flags(3)=', L)
          write(OUTPUT_UNIT,210) this%m_ctor_flags(4)
210       format('this%m_ctor_flags(4)=', L)
          write(OUTPUT_UNIT,220) this%m_rand_distr_t
220       format('this%m_rand_distr_t= ', 'c')
          write(OUTPUT_UNIT,230)
230       format('1', T15, ' Field2D Array Components ')
          write(OUTPUT_UNIT,240)
240       format('0', T2, 'm_p2d', T8, 'm_p8w2d', T16, 'm_maskp2d')
          write(OUTPUT_UNIT,250)
250       format(1X, T2, '==========', T8, '==========', T16, '=========='/)
          do j = this%m_jms, this%m_jme
              do i = this%m_ims, this%m_ime
                  write(OUTPUT_UNIT,260) this%m_p2d(i,j), this%m_p8w2d(i,j), this%m_maskp2d(i,j)
260               format(1X, T2, F10.15, T8, F10.15, T16, L)
              end do
          end do
          write(*,*)  'Finshed dumping CAtmPressureField2D object state!!'
          call DATE_AND_TIME(rc(1),rc(2),rc(3),dt)
          print*, 'Date of collection: ', dt(1), '-', dt(2), '-', dt(3)
          print*, 'UTC-DELTA: ', dt(4)
          print*, 'Time of collection: ', dt(5), ':', dt(6), ':', dt(7), ':', dt(8)
          
    end  subroutine
    
    
    subroutine  print_memory_field2D(this)
          implicit none
          use ISO_FORTRAN_ENV, ONLY : OUTPUT_UNIT
          class(CAtmPressureField2D),    intent(in) :: this
          ! Local variables
          integer(i32),      parameter              :: rc_len = 3
          integer(i32),      parameter              :: dt_len = 8
          character(len=12), dimension(rc_len)      :: rc
          integer(i32),      dimension(dt_len)      :: dt
          integer(i64)                              :: i
          integer(i64)                              :: j
          ! Start of executable statements
          
          if(.NOT. allocated(this%m_p2d)   .OR. &
             .NOT. allocated(this%m_p8w2d) .OR. &
             .NOT. allocated(this%m_maskp2d)    ) then
                 write(*,*) 'FATAL ERROR in: print_memory_field2D!!'
                 write(*,*) 'Object in inconsistent state - deallocated arrays!!'
                 write(*,*) 'Executing early exit!!'
                 return
          end if
         
          call DATE_AND_TIME(rc(1),rc(2),rc(3),dt)
          print*, '***** CAtmPressureField2D ***** -- STATE DUMP!!'
          print*, 'Date of collection: ', dt(1), '-', dt(2), '-', dt(3)
          print*, 'UTC-DELTA: ', dt(4)
          print*, 'Time of collection: ', dt(5), ':', dt(6), ':', dt(7), ':', dt(8)     
          print*, 'Printing CAtmPressureField2D memory layout. '
          write(OUTPUT_UNIT,10) LOC(this)
10        format('this at: ', Z15)
          write(OUTPUT_UNIT,20) LOC(this%m_ims)
20        format('this%m_ims at: ', Z15)
          write(OUTPUT_UNIT,30) LOC(this%m_ime)
30        format('this%m_ime at: ', Z15)
          write(OUTPUT_UNIT,40) LOC(this%m_jms)
40        format('this%m_jms at: ', Z15)
          write(OUTPUT_UNIT,50) LOC(this%m_jme)
50        format('this%m_jme at: ', Z15)
          write(OUTPUT_UNIT,60) LOC(this%m_ids)
60        format('this%m_ids at: ', Z15)
          write(OUTPUT_UNIT,70) LOC(this%m_ide)
70        format('this%m_ide at: ', Z15)
          write(OUTPUT_UNIT,80) LOC(this%m_jds)
80        format('this%m_jds at: ', Z15)
          write(OUTPUT_UNIT,90) LOC(this%m_jde)
90        format('this%m_jde at: ', Z15)
          write(OUTPUT_UNIT,100) LOC(this%m_its)
100       format('this%m_its at: ', Z15)
          write(OUTPUT_UNIT,110) LOC(this%m_ite)
110       format('this%m_ite at: ', Z15)
          write(OUTPUT_UNIT,120) LOC(this%m_jts)
120       format('this%m_jts at: ', Z15)
          write(OUTPUT_UNIT,130) LOC(this%m_jte)
130       format('this%m_jte at: ', Z15)
          write(OUTPUT_UNIT,140) LOC(this%m_m1s)
140       format('this%m_m1s at: ', Z15)
          write(OUTPUT_UNIT,150) LOC(this%m_m1e)
150       format('this%m_m1e at: ', Z15)
          write(OUTPUT_UNIT,160) LOC(this%m_m2s)
160       format('this%m_m2s at: ', Z15)
          write(OUTPUT_UNIT,170) LOC(this%m_m2e)
170       format('this%m_m2e at: ', Z15)
!DIR$ IF ( Declare_Destructor_Field2D .EQ. 1)
          write(OUTPUT_UNIT,180) LOC(this%m_is_initilaized)
180       format('this%m_is_initilaized at: ', Z15)
!DIR$ ENDIF
          write(OUTPUT_UNIT,190) LOC(this%m_ctor_flags)
190       format('this%m_ctor_flags at: ', Z15)
          write(OUTPUT_UNIT,200) LOC(this%m_rand_distr_t)
200       format('this%m_rand_distr_t at: ', Z15)
          write(OUTPUT_UNIT,210) LOC(this%m_p2d)
210       format('this%m_p2d at: ',Z15)
          write(OUTPUT_UNIT,220) LOC(this%m_p8w2d)
220       format('this%m_p8w2d at:', Z15)
          write(OUTPUT_UNIT,230) LOC(this%m_maskp2d)
230       format('this%m_maskp2d at: ', Z15)
          write(OUTPUT_UNIT,240)
240       format('1', T8, 'Sweeping member arrays address space.')
          write(OUTPUT_UNIT,250)
250       format('0', T2, 'm_p2d', T16, 'm_p8w2d', T30, 'm_maskp2d')
          write(OUTPUT_UNIT,260)
260       format(1X, T2, '==========', T8, '==========', T16, '=========='/)
          do j = this%m_jms, this%m_jme
              do i = this%m_ims, this%m_ime
                  write(OUTPUT_UNIT,270) LOC(this%m_p2d(i,j)), LOC(this%m_p8w2d(i,j)), LOC(this%m_maskp2d(i,j))
270               format(1X,T2, Z15, T8, Z15, T16, z15)
              end do
          end do
          write(OUTPUT_UNIT,280) REAL(SIZE(this%m_p2d) * DP_SIZE) / KiB
280       format('Size of this%m_p2d=', F10.6, 1X, 'KiB')
          write(OUTPUT_UNIT,290) REAL(SIZE(this%m_p8w2d) * DP_SIZE) / KiB
290       format('Size of this%m_p8w2d=', F10.6, 1X, 'KiB')
          write(OUTPUT_UNIT,300) REAL(SIZE(this%m_maskp2d) * DP_SIZE) / KiB
300       format('Size of this%m_maskp2d=', F10.6, 1X, 'KiB')
           write(*,*)  'Finshed dumping CAtmPressureField2D object state!!'
          call DATE_AND_TIME(rc(1),rc(2),rc(3),dt)
          print*, 'Date of collection: ', dt(1), '-', dt(2), '-', dt(3)
          print*, 'UTC-DELTA: ', dt(4)
          print*, 'Time of collection: ', dt(5), ':', dt(6), ':', dt(7), ':', dt(8)
    end  subroutine
    
    
    subroutine  field2D_object_type(this,base,description,is_derived)
          implicit none
          class(CAtmPressureField2D),   intent(in)    :: this
          class(IAtmPressureField),     intent(in)    :: base
          character(len=*),             intent(inout) :: description
          logical(i32),                 intent(inout) :: is_derived
          ! Start of executable statements
          description = '[Derived]: CAtmPressureField2D --> [Base]: IAtmPressureField'
          is_derived = EXTENDS_TYPE_OF(this,base)
    
    end  subroutine
    
    
#if USE_INLINING == 1
      !DIR$ ATTRIBUTES INLINE :: field2D_dimension
#endif

    pure  function field2D_dimension()   result(ret_ar)
          implicit none
          ! Local variables
          integer(i32), parameter         :: ar_len = 3
          integer(i32), dimension(ar_len) :: ret_ar
          ! Start of executable statements
          
          ret_ar(1) = 2; ret_ar(2) = 2, ret_ar(3) = 3
    
    end  function
    
    
    subroutine  field2D_shape(this,n,sp2d,sp8w2d,smaskp2d)
          implicit none
          class(CAtmPressureField2D),    intent(in)    :: this
          integer(i32),                  intent(in)    :: n
          integer(i32), dimension(n),    intent(inout) :: sp2d
          integer(i32), dimension(n),    intent(inout) :: sp8w2d
          integer(i32), dimension(n),    intent(inout) :: smaskp2d
          ! Start of executable statements
          
          sp2d     = SHAPE(this%m_p2d)
          sp8w2d   = SHAPE(this%m_p8w2d)
          smaskp2d = SHAPE(this%m_maskp2d)
          
    end  subroutine
    
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: is_constructed_field2D
#endif
    
    pure  function is_constructed_field2D(this)     result(b_ret)
          implicit none
          class(CAtmPressureField2D),   intent(in) :: this
          ! Locals
          logical(i32)                             :: b_ret
          ! Start of executable statements
    
          b_ret = this%m_is_initialized
    
    end  function
    
!DIR$ ENDIF 
    
    function  field2D_num_elems(this)   result(n_elems)
          implicit none
          class(CAtmPressureField2D),   intent(in) :: this
          ! Locals
          integer(i32), parameter                  :: arlen = 3
          integer(i64), dimension(arlen)           :: n_elems
          ! Start of executable stataments
          
          n_elems(1) = SIZE(this%m_p2d)
          n_elems(2) = SIZE(this%m_p8w2d)
          n_elems(3) = SIZE(this%m_maskp2d)
    
    end  function
    
    
    function  field2D_num_dirx_elems(this)      result(n_elems)
          implicit none
          class(CAtmPressureField2D),   intent(in) :: this
          ! Locals
          integer(i32), parameter                  :: arlen = 9
          integer(i64), dimension(arlen)           :: n_elems
          ! Start of executable statements
          
          n_elems(1) = SIZE(this%m_p2d,dim=1)
          n_elems(2) = 0
          n_elems(3) = 0
          n_elems(4) = SIZE(this%m_p8w2d,dim=1)
          n_elems(5) = 0
          n_elems(6) = 0
          n_elems(7) = SIZE(this%m_maskp2d,dim=1)
          n_elems(8) = 0
          n_elems(9) = 0
    
    
    end  function
    
    
    function  field2D_num_diry_elems(this)      result(n_elems)
          implicit none
          class(CAtmPressureField2D),    intent(in) :: this
          ! Locals
          integer(i32), parameter                   :: arlen = 9
          integer(i64), dimension(arlen)            :: n_elems
          ! Start of executable statements
    
          n_elems(1) = 0
          n_elems(2) = SIZE(this%m_p2d,dim=2)
          n_elems(3) = 0
          n_elems(4) = 0
          n_elems(5) = SIZE(this%m_p8w2d,dim=2)
          n_elems(6) = 0
          n_elems(7) = 0
          n_elems(8) = SIZE(this%m_maskp2d,dim=2)
          n_elems(9) = 0
          
    end  function
    
    
    pure  function  field2D_num_dirz_elems()    result(n_elems)
          implicit none
         
          ! Locals
          integer(i32), parameter                     :: arlen
          integer(i64), dimension(arlen)              :: n_elems
          ! Start of executable statements
          
          n_elems = 0
    
    end  function
    
    
    function  field2D_max_press(this)    result(max_vals)
          implicit none
          class(CAtmPressureField2D),   intent(in) :: this
          ! Locals
          integer(i32), parameter                  :: arlen = 2
          real(R64), dimension(arlen)              :: max_vals
          ! Start of executable statements
          
          max_vals(1) = MAXVAL(this%m_p2d)
          max_vals(2) = MAXVAL(this%m_p8w2d)
    
    
    end  function
    
    
    function  field2D_max_loc_press(this)   result(max_locs)
          implicit none
          class(CAtmPressureField2D),    intent(in) :: this
          ! Locals
          integer(i32), parameter                   :: arlen = 2
          integer(i64), dimension(arlen)            :: max_locs
          ! Start of executable statements
          
          max_locs(1) = MAXLOC(this%m_p2d)
          max_locs(2) = MAXLOC(this%m_p8w2d)
    
    end  function
    
    
    function  field2D_min_press(this)    result(min_vals)
          implicit none
          class(CAtmPressureField2D),   intent(in) :: this
          ! locals
          integer(i32), parameter                  :: arlen = 2
          real(R64), dimension(arlen)              :: min_vals
          ! Start of executable staememts
          
          min_vals(1) = MINVAL(this%m_p2d)
          min_vals(2) = MINVAL(this%m_p8w2d)
    
    end  function
    
    
    function  field2D_min_loc_press(this)    result(min_locs)
          implicit none
          class(CAtmPressureField2D),     intent(in) :: this
          ! Locals
          integer(i32), parameter                    :: arlen = 2
          integer(i64), dimension(arlen)             :: min_locs
          ! Start of executable statements
          
          min_locs(1) = MINLOC(this%m_p2d)
          min_locs(2) = MINLOC(this%m_p8w2d)
    
    end  function
    
    
     !===============================38
     !  Begin implementation of class
     !  CAtmPressureField2D deferred
     !  procedures.
     !===============================38
    
    subroutine  copy_assign_field2D(this,other,use_omp)
          implicit none
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          class(CAtmPressureField2D),   intent(inout) :: this
          class(IAtmPressureField),     intent(in)    :: other
          logical(i32),                 intent(in)    :: use_omp
          ! Start of executable statements
          
          if(LOC(this) .EQ. LOC(other)) then
             write(*,*) 'copy_assign_field2D: Attempted Self-Assign  -- Detected!!'
             write(ERROR_UNIT,10) LOC(this)
10           format('this at  address: ', Z15)
             write(ERROR_UNIT,20) LOC(other)
20           format('other at address: ', Z15)
             write(*,*) 'Executing Early Return!!'
             return 
          end if 
          
          select type(other)
          class is(CAtmPressureField2D)
              this%m_ims = other%m_ims
              this%m_ime = other%m_ime
              this%m_jms = other%m_jms
              this%m_jme = other%m_jme
              this%m_ids = other%m_ids
              this%m_ide = other%m_ide
              this%m_jds = other%m_jds
              this%m_jde = other%m_jde
              this%m_its = other%m_its
              this%m_ite = other%m_ite
              this%m_jts = other%m_jts
              this%m_jte = other%m_jte
              this%m_m1s = other%m_m1s
              this%m_m1e = other%m_m1e
              this%m_m2s = other%m_m2s
              this%m_m2e = other%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
              this%m_is_initialized = other%m_is_initialized
!DIR$ ENDIF
              this%m_ctor_flags = other%m_ctor_flags
              this%m_rand_distr_t = other%m_rand_distr_t
              if(use_omp .EQ. .true.) then
                 call omp_copy_arrays2d(other%m_p2d,other%m_p8w2d,other%m_maskp2d, &
                                        this%m_p2d,this%m_p8w2d,this%m_maskp2d    )
              else
                  
                 this%m_p2d = other%m_p2d
                 this%m_p8w2d = other%m_p8w2d
                 this%m_maskp2d = other%m_maskp2d
              end if
              
            class default
                  ERROR STOP 'copy_assign_field2D: Unsupported derived type!!'
          end select
          
    
    
    end  subroutine
    
    
    subroutine  move_assign_field2D(this,other)
          implicit none
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          class(CAtmPressureField2D),   intent(inout) :: this
          class(IAtmPressureField),     intent(in)    :: other
          ! Start of executable statments
          
          if(LOC(this) .EQ. LOC(other)) then
             write(*,*) 'move_assign_field2D: Attempted Self-Assign  -- Detected!!'
             write(ERROR_UNIT,10) LOC(this)
10           format('this at  address: ', Z15)
             write(ERROR_UNIT,20) LOC(other)
20           format('other at address: ', Z15)
             write(ERROR_UNIT,*) 'Executing Early Return!!'
             return  
              
          end if
          
          select type(other)
          class is(CAtmPressureField2D)
              this%m_ims = other%m_ims
              this%m_ime = other%m_ime
              this%m_jms = other%m_jms
              this%m_jme = other%m_jme
              this%m_ids = other%m_ids
              this%m_ide = other%m_ide
              this%m_jds = other%m_jds
              this%m_jde = other%m_jde
              this%m_its = other%m_its
              this%m_ite = other%m_ite
              this%m_jts = other%m_jts
              this%m_jte = other%m_jte
              this%m_m1s = other%m_m1s
              this%m_m1e = other%m_m1e
              this%m_m2s = other%m_m2s
              this%m_m2e = other%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
              this%m_is_initialized = other%m_is_initialized
!DIR$ ENDIF
              this%m_ctor_flags = other%m_ctor_flags
              this%m_rand_distr_t = other%m_rand_distr_t
              move_alloc(other%m_p2d,this%m_p2d)
              move_alloc(other%m_p8w2d,this%m_p8w2d)
              move_alloc(other%m_maskp2d,this%m_maskp2d)
              class default
                  ERROR STOP 'move_assign_field2D: Unsupported derived type'
          end select
          
            
    
    end  subroutine
    
    
    function  field2D_add_field2D(lhs,rhs,use_omp,print_details)      result(obj)
          implicit none
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          class(CAtmPressureField2D),   intent(in) :: lhs
          class(IAtmPressureField),     intent(in) :: rhs
          logical(i32),                 intent(in) :: use_omp
          logical(I32), optional,       intent(in) :: print_details
          ! Locals
          class(IAtmPressureField), allocatable    :: obj
          integer(i64)                             :: i
          integer(i64)                             :: j
          character*(*) fname
          parameter (fname='field2D_add_field2D')
          ! Start of executable statements
          
          select type(rhs)
          class is(CAtmPressureField2D)
             call nonconforming_arrays2d(lhs,rhs,print_details,file_path,fname,5852)
               class default
                   ERROR STOP 'field2D_add_field2D: Unsupported derived type!!'
          end select
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField2D)
              select type(rhs)
              class is(CAtmPressureField2D)
                  obj%m_ims = lhs%m_ims
                  obj%m_ime = lhs%m_ime
                  obj%m_jms = lhs%m_jms
                  obj%m_jme = lhs%m_jme
                  obj%m_ids = lhs%m_ids
                  obj%m_ide = lhs%m_ide
                  obj%m_jds = lhs%m_jds
                  obj%m_jde = lhs%m_jds
                  obj%m_its = lhs%m_its
                  obj%m_ite = lhs%m_ite
                  obj%m_jts = lhs%m_jts
                  obj%m_jte = lhs%m_jte
                  obj%m_m1s = lhs%m_m1s
                  obj%m_m1e = lhs%m_m1e
                  obj%m_m2s = lhs%m_m2s
                  obj%m_m2e = lhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
                  obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
                  obj%m_ctor_flags   = lhs%m_ctor_flags
                  obj%m_rand_distr_t = lhs%m_rand_distr_t
                  allocate(obj%m_maskp2d(SIZE(lhs%m_maskp2d,dim=1),SIZE(lhs%m_maskp2d,dim=2)))
                           
                  if(use_omp .EQ. .true.) then
                      call omp_add_arrays2d(lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                            rhs%m_p2d,rhs%m_p8w2d,rhs%m_maskp2d, &
                                            obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d   )
                  else
                      obj%m_p2d          = lhs%m_p2d   + rhs%m_p2d
                      obj%m_p8w2d        = lhs%m_p8w2d + rhs%m_p8w2d
                  do j = obj%m_jms, obj%m_jme
                  !DIR$ SIMD
                      do  i = obj%m_ims, obj%m_ime
                           obj%m_maskp2d(i,j) = IOR(lhs%m_maskp2d(i,j),rhs%m_maskp2d(i,j))
                        end do
                  end do   
                  end if   
                  class default
                      ERROR STOP 'field2D_add_field2D: Unsupported derived type!!'
              end select
          end select
          
          
    end  function
    
    
    function  field2D_sub_field2D(lhs,rhs,use_omp,print_details)    result(obj)
          implicit none
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          class(CAtmPressureField2D),   intent(in) :: lhs
          class(IAtmPressureField),     intent(in) :: rhs
          logical(I32),                 intent(in) :: use_omp
          logical(I32), optional,       intent(in) :: print_details
          ! Locals
          class(IAtmPressureField), allocatable    :: obj
          integer(i64)                             :: i
          integer(i64)                             :: j
          character*(*) fname
          parameter (fname='field2D_sub_field2D')
          ! Start of executable statements
          
          select type(rhs)
          class is(CAtmPressureField2D)
             call nonconforming_arrays2d(lhs,rhs,print_details,file_path,fname,5925)
             class default
                 ERROR STOP 'field2D_sub_field2D: Unsupported derived type!!'
          end select
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField2D)
              select type(rhs)
              class is(CAtmPressureField2D)
                  obj%m_ims = lhs%m_ims
                  obj%m_ime = lhs%m_ime
                  obj%m_jms = lhs%m_jms
                  obj%m_jme = lhs%m_jme
                  obj%m_ids = lhs%m_ids
                  obj%m_ide = lhs%m_ide
                  obj%m_jds = lhs%m_jds
                  obj%m_jde = lhs%m_jds
                  obj%m_its = lhs%m_its
                  obj%m_ite = lhs%m_ite
                  obj%m_jts = lhs%m_jts
                  obj%m_jte = lhs%m_jte
                  obj%m_m1s = lhs%m_m1s
                  obj%m_m1e = lhs%m_m1e
                  obj%m_m2s = lhs%m_m2s
                  obj%m_m2e = lhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
                  obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
                  obj%m_ctor_flags   = lhs%m_ctor_flags
                  obj%m_rand_distr_t = lhs%m_rand_distr_t
                  allocate(obj%m_maskp2d(SIZE(lhs%m_maskp2d,dim=1),SIZE(lhs%m_maskp2d,dim=2)))
                  if(use_omp .EQ. .true.) then
                      call  omp_sub_arrays2d(lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                             rhs%m_p2d,rhs%m_p8w2d,rhs%m_maskp2d, &
                                             obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d  )
                  else
                      obj%m_p2d          = lhs%m_p2d   - rhs%m_p2d
                      obj%m_p8w2d        = lhs%m_p8w2d - rhs%m_p8w2d
                  
                      do j = obj%m_jms, obj%m_jme
                          !DIR$ SIMD
                         do i = obj%m_ims, obj%m_ime
                              obj%m_maskp2d(i,j) = IOR(lhs%m_maskp2d(i,j),rhs%m_maskp2d(i,j))
                         end do
                      end do
                  end if
                  
                  
                  class default
                      ERROR STOP 'field2D_sub_field2D: Unsupported derived type!!'
              end select
          end select
          
                  
    end  function
    
    
    function  field2D_mul_field2D(lhs,rhs,use_omp,print_details)    result(obj)
          implicit none
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          class(CAtmPressureField2D),   intent(in) :: lhs
          class(IAtmPressureField),     intent(in) :: rhs
          logical(I32),                 intent(in) :: use_omp
          logical(I32), optional,       intent(in) :: print_details
          ! Locals
          class(IAtmPressureField), allocatable    :: obj
          integer(I64)                             :: i
          integer(I64)                             :: j
          character*(*) fname
          parameter (fname='field2D_mul_field2D')
          ! Start of executable statements
          
          select type(rhs)
          class is(CAtmPressureField2D)
              call nonconforming_arrays2d(lhs,rhs,print_details,file_path,fname,6000)
              class default
                  ERROR STOP 'Unsupported derived type!!'
          end select
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField2D)
              select type(rhs)
              class is(CAtmPressureField2D)
                  obj%m_ims = lhs%m_ims
                  obj%m_ime = lhs%m_ime
                  obj%m_jms = lhs%m_jms
                  obj%m_jme = lhs%m_jme
                  obj%m_ids = lhs%m_ids
                  obj%m_ide = lhs%m_ide
                  obj%m_jds = lhs%m_jds
                  obj%m_jde = lhs%m_jds
                  obj%m_its = lhs%m_its
                  obj%m_ite = lhs%m_ite
                  obj%m_jts = lhs%m_jts
                  obj%m_jte = lhs%m_jte
                  obj%m_m1s = lhs%m_m1s
                  obj%m_m1e = lhs%m_m1e
                  obj%m_m2s = lhs%m_m2s
                  obj%m_m2e = lhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
                  obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
                  obj%m_ctor_flags   = lhs%m_ctor_flags
                  obj%m_rand_distr_t = lhs%m_rand_distr_t
                  allocate(obj%m_maskp2d(SIZE(lhs%m_maskp2d,dim=1),SIZE(lhs%m_maskp2d,dim=2)))
                  if(use_omp .EQ. .true.) then
                      call omp_mul_arrays2d(lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                            rhs%m_p2d,rhs%m_p8w2d,rhs%m_maskp2d, &
                                            obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d  )
                  else
                      obj%m_p2d          = lhs%m_p2d   * rhs%m_p2d
                      obj%m_p8w2d        = lhs%m_p8w2d * rhs%m_p8w2d
                  
                     do j = obj%m_jms, obj%m_jme
                        do i = obj%m_ims, obj%m_ime
                          obj%m_maskp2d(i,j) = IOR(lhs%m_maskp2d(i,j),rhs%m_maskp2d(i,j))
                        end do
                     end do
                  end if
                  
                  class default
                      ERROR STOP 'Unsupported derived type!!'
              end select
          end select
          
          
    
    end  function
    
   
    function  field2D_mul_real(lhs,rhs,use_omp)    result(obj)
          implicit none
          class(CAtmPressureField2D),     intent(in) :: lhs
          real(R64),                      intent(in) :: rhs
          logical(i32),                   intent(in) :: use_omp
          ! Locals
          class(IAtmPressureField), allocatable      :: obj
          ! Start of executable statements
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField2D)
               obj%m_ims = lhs%m_ims
               obj%m_ime = lhs%m_ime
               obj%m_jms = lhs%m_jms
               obj%m_jme = lhs%m_jme
               obj%m_ids = lhs%m_ids
               obj%m_ide = lhs%m_ide
               obj%m_jds = lhs%m_jds
               obj%m_jde = lhs%m_jds
               obj%m_its = lhs%m_its
               obj%m_ite = lhs%m_ite
               obj%m_jts = lhs%m_jts
               obj%m_jte = lhs%m_jte
               obj%m_m1s = lhs%m_m1s
               obj%m_m1e = lhs%m_m1e
               obj%m_m2s = lhs%m_m2s
               obj%m_m2e = lhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
               obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
               obj%m_ctor_flags   = lhs%m_ctor_flags
               obj%m_rand_distr_t = lhs%m_rand_distr_t
              
               if(use_omp .EQ. .true.) then
                   call omp_arrays2d_mul_real(rhs,lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                              obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d    )
               else
                   obj%m_p2d     = lhs%m_p2d   * rhs
                   obj%m_p8w2d   = lhs%m_p8w2d * rhs
                   obj%m_maskp2d = lhs%m_maskp2d
               end if
          end select
          
    
    end  function
    
    
    function  real_mul_field2D(lhs,rhs,use_omp)    result(obj)
          implicit none
          real(R64),                    intent(in) :: lhs
          class(CAtmPressureField2D),   intent(in) :: rhs
          logical(i32),                 intent(in) :: use_omp
          ! Locals
          class(IAtmPressureField), allocatable    :: obj
          ! Start of executable statements
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField2D)
               obj%m_ims = rhs%m_ims
               obj%m_ime = rhs%m_ime
               obj%m_jms = rhs%m_jms
               obj%m_jme = rhs%m_jme
               obj%m_ids = rhs%m_ids
               obj%m_ide = rhs%m_ide
               obj%m_jds = rhs%m_jds
               obj%m_jde = rhs%m_jde
               obj%m_its = rhs%m_its
               obj%m_ite = rhs%m_ite
               obj%m_jts = rhs%m_jts
               obj%m_jte = rhs%m_jte
               obj%m_m1s = rhs%m_m1s
               obj%m_m1e = rhs%m_m1e
               obj%m_m2s = rhs%m_m2s
               obj%m_m2e = rhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
               obj%m_is_initialized = rhs%m_is_initialized
!DIR$ ENDIF
               obj%m_ctor_flags = rhs%m_ctor_flags
               obj%m_rand_distr_t = rhs%m_rand_distr_t
             
               if(use_omp .EQ. .true.) then
                   call omp_real_mul_arrays2d(lhs,rhs%m_p2d,rhs%m_p8w2d,rhs%m_maskp2d, &
                                              obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d     )
               else
                   obj%m_p2d     = lhs * rhs%m_p2d
                   obj%m_p8w2d   = lhs * rhs%m_p8w2d
                   obj%m_maskp2d = rhs%m_maskp2d
               end if
               
          end select
          
    
    end  function
    
    
    function  field2D_mul_int(lhs,rhs,use_omp)    result(obj)
          implicit none
          class(CAtmPressureField2D),    intent(in) :: lhs
          integer(I64),                  intent(in) :: rhs
          logical(I32),                  intent(in) :: use_omp
          ! Locals
          class(IAtmPressureField), allocatable     :: obj
          ! Start of executable statements
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField1D)
               obj%m_ims = lhs%m_ims
               obj%m_ime = lhs%m_ime
               obj%m_jms = lhs%m_jms
               obj%m_jme = lhs%m_jme
               obj%m_ids = lhs%m_ids
               obj%m_ide = lhs%m_ide
               obj%m_jds = lhs%m_jds
               obj%m_jde = lhs%m_jds
               obj%m_its = lhs%m_its
               obj%m_ite = lhs%m_ite
               obj%m_jts = lhs%m_jts
               obj%m_jte = lhs%m_jte
               obj%m_m1s = lhs%m_m1s
               obj%m_m1e = lhs%m_m1e
               obj%m_m2s = lhs%m_m2s
               obj%m_m2e = lhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
               obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
               obj%m_ctor_flags   = lhs%m_ctor_flags
               obj%m_rand_distr_t = lhs%m_rand_distr_t
              
               if(use_omp .EQ. .true.) then
                   call omp_arrays2d_mul_int(rhs,lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                             obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d      )
               else
                   obj%m_p2d     = lhs%m_p2d   * DBLE(rhs)
                   obj%m_p8w2d   = lhs%m_p8w2d * DBLE(rhs)
                   obj%m_maskp2d = lhs%m_maskp2d
               end if
               
          end select
          
    
    end  function
    
    
    function  int_mul_field2D(lhs,rhs,use_omp)    result(obj)
          implicit none
          integer(I64),               intent(in) :: lhs
          class(CAtmPressureField2D), intent(in) :: rhs
          ! Locals
          class(IAtmPressureField), allocatable  :: obj
          ! Start of executable statements
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField2D)
               obj%m_ims = rhs%m_ims
               obj%m_ime = rhs%m_ime
               obj%m_jms = rhs%m_jms
               obj%m_jme = rhs%m_jme
               obj%m_ids = rhs%m_ids
               obj%m_ide = rhs%m_ide
               obj%m_jds = rhs%m_jds
               obj%m_jde = rhs%m_jde
               obj%m_its = rhs%m_its
               obj%m_ite = rhs%m_ite
               obj%m_jts = rhs%m_jts
               obj%m_jte = rhs%m_jte
               obj%m_m1s = rhs%m_m1s
               obj%m_m1e = rhs%m_m1e
               obj%m_m2s = rhs%m_m2s
               obj%m_m2e = rhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
               obj%m_is_initialized = rhs%m_is_initialized
!DIR$ ENDIF
               obj%m_ctor_flags = rhs%m_ctor_flags
               obj%m_rand_distr_t = rhs%m_rand_distr_t
               
               if(use_omp .EQ. .true.) then
                   call  omp_int_mul_arrays2d(lhs,rhs%m_p2d,rhs%m_p8w2d,rhs%m_maskp2d, &
                                              obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d    )
               else
                   obj%m_p2d     = DBLE(lhs) * rhs%m_p2d
                   obj%m_p8w2d   = DBLE(lhs) * rhs%m_p8w2d
                   obj%m_maskp2d = rhs%m_maskp2d
               endif
               
          end select
          
    
    
    end  function
    
    
    function  field2D_div_field2D(lhs,rhs,use_omp,print_details)    result(obj)
          implicit none
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          class(CAtmPressureField2D),       intent(inout) :: lhs
          class(IAtmPressureField),         intent(inout) :: rhs
          logical(I32),                     intent(in)    :: use_omp
          logical(I32), optional,           intent(in)    :: print_details
          ! Locals
          class(IAtmPressureField), allocatable           :: use_omp
          integer(I64)                                    :: i
          integer(I64)                                    :: j
          character*(*) fname
          parameter (fname='field2D_div_field2D')
          ! Start of executable statements
          
          select type(rhs)
          class is(CAtmPressureField2D)
                  
                  ! Check for lhs vs. rhs arrays conformance
                  call nonconforming_arrays2d(lhs,rhs,print_details,file_path,fname,6305)     
                  ! Attempt to scan for a value(s) <= TINY_PRESSURE and
                  ! correct them.
                  ! Hence both dummy objects are modifiable. 
                  call check_arrays2d_denorm(lp,lp8,rp,rp8,       &
                                             lhs%m_ims,lhs%m_ime, &
                                             lhs%m_jms,lhs%m_jme   )
              class default
                  ERROR STOP 'Unsupported derived type!!'
          end select
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField2D)
              select type(rhs)
              class is(CAtmPressureField2D)
                    obj%m_ims = lhs%m_ims
                    obj%m_ime = lhs%m_ime
                    obj%m_jms = lhs%m_jms
                    obj%m_jme = lhs%m_jme
                    obj%m_ids = lhs%m_ids
                    obj%m_ide = lhs%m_ide
                    obj%m_jds = lhs%m_jds
                    obj%m_jde = lhs%m_jds
                    obj%m_its = lhs%m_its
                    obj%m_ite = lhs%m_ite
                    obj%m_jts = lhs%m_jts
                    obj%m_jte = lhs%m_jte
                    obj%m_m1s = lhs%m_m1s
                    obj%m_m1e = lhs%m_m1e
                    obj%m_m2s = lhs%m_m2s
                    obj%m_m2e = lhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
                    obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
                    obj%m_ctor_flags   = lhs%m_ctor_flags
                    obj%m_rand_distr_t = lhs%m_rand_distr_t
                    allocate(obj%m_maskp2d(SIZE(lhs%m_maskp2d,dim=1),SIZE(lhs%m_maskp2d,dim=2)))
                    if(use_omp .EQ. .true.) then
                        call omp_div_arrays2d(lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                              rhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                              obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d  )
                    else
                        obj%m_p2d   = lhs%m_p2d   / rhs%m_p2d
                        obj%m_p8w2d = lhs%m_p8w2d / rhs%m_p8w2d
                        do j = obj%m_jms, obj%m_jme
                            do i = obj%m_ims, obj%m_ime
                                obj%m_maskp2d = IOR(lhs%m_maskp2d(i,j),rhs%m_maskp2d(i,j)
                            end do
                        end do
                    end if
                class default
                    ERROR STOP 'field2D_div_field2D: Unsupported derived type!!'
              end select
          end select
          
                      
    
    end  function
    
    
    function  field2D_div_real(lhs,rhs,use_omp)     result(obj)
          implicit none
          class(CAtmPressureField2D),    intent(inout) :: lhs
          real(R64),                     intent(in)    :: rhs
          logical(I32),                  intent(in)    :: use_omp
          ! Locals
          class(IAtmPressureField), allocatable        :: obj
          
          ! Start of executable statements
          call check_arrays2d_denorm2(lhs%m_p2d,lhs%m_p8w2d, &
                                      lhs%m_ims,lhs%m_ims,lhs%m_jms,lhs%m_jme)
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField2D)
                    obj%m_ims = lhs%m_ims
                    obj%m_ime = lhs%m_ime
                    obj%m_jms = lhs%m_jms
                    obj%m_jme = lhs%m_jme
                    obj%m_ids = lhs%m_ids
                    obj%m_ide = lhs%m_ide
                    obj%m_jds = lhs%m_jds
                    obj%m_jde = lhs%m_jds
                    obj%m_its = lhs%m_its
                    obj%m_ite = lhs%m_ite
                    obj%m_jts = lhs%m_jts
                    obj%m_jte = lhs%m_jte
                    obj%m_m1s = lhs%m_m1s
                    obj%m_m1e = lhs%m_m1e
                    obj%m_m2s = lhs%m_m2s
                    obj%m_m2e = lhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
                    obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
                    obj%m_ctor_flags   = lhs%m_ctor_flags
                    obj%m_rand_distr_t = lhs%m_rand_distr_t
                    if(use_omp .EQ. .true.) then
                        call omp_arrays2d_div_real(rhs,lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                                   obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d    )
                    else
                        obj%m_p2d   = lhs%m_p2d   / rhs
                        obj%m_p8w2d = lhs%m_p8w2d / rhs
                        obj%m_maskp2d = lhs%m_maskp2d
                    end if
                    
          end select
    
    end  function
    
    
    function  field2D_div_int(lhs,rhs,use_omp)     result(obj)
          implicit none
          class(CAtmPressureField2D),   intent(in) :: lhs
          integer(I64),                 intent(in) :: rhs
          logical(I32),                 intent(in) :: use_omp
          ! Locals
          class(IAtmPressureField), allocatable    :: obj
          ! Start of executable statments
          
          call check_arrays2d_denorm2(lhs%m_p2d,lhs%m_p8w2d, &
                                      lhs%m_ims,lhs%m_ims,lhs%m_jms,lhs%m_jme)
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField2D)
                    obj%m_ims = lhs%m_ims
                    obj%m_ime = lhs%m_ime
                    obj%m_jms = lhs%m_jms
                    obj%m_jme = lhs%m_jme
                    obj%m_ids = lhs%m_ids
                    obj%m_ide = lhs%m_ide
                    obj%m_jds = lhs%m_jds
                    obj%m_jde = lhs%m_jds
                    obj%m_its = lhs%m_its
                    obj%m_ite = lhs%m_ite
                    obj%m_jts = lhs%m_jts
                    obj%m_jte = lhs%m_jte
                    obj%m_m1s = lhs%m_m1s
                    obj%m_m1e = lhs%m_m1e
                    obj%m_m2s = lhs%m_m2s
                    obj%m_m2e = lhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
                    obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
                    obj%m_ctor_flags   = lhs%m_ctor_flags
                    obj%m_rand_distr_t = lhs%m_rand_distr_t
                    if(use_omp .EQ. .true.) then
                        call omp_arrays2d_div_int(rhs,lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                                  obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d     )
                    else
                        obj%m_p2d   = lhs%m_p2d   / DBLE(rhs)
                        obj%m_p8w2d = lhs%m_p8w2d / DBLE(rhs)
                        obj%m_maskp2d = lhs%m_maskp2d
                    end if
                
          end select
          
    
    end  function
    
    
    function  field2D_pow_real(lhs,rhs,use_omp)    result(obj)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: lhs
          real(R64),                        intent(in) :: rhs
          logical(I32),                     intent(in) :: use_omp
          ! Locals
          class(IAtmPressureField), allocatable        :: obj
          
          ! Start of execuatble statments
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField2D)
                    obj%m_ims = lhs%m_ims
                    obj%m_ime = lhs%m_ime
                    obj%m_jms = lhs%m_jms
                    obj%m_jme = lhs%m_jme
                    obj%m_ids = lhs%m_ids
                    obj%m_ide = lhs%m_ide
                    obj%m_jds = lhs%m_jds
                    obj%m_jde = lhs%m_jds
                    obj%m_its = lhs%m_its
                    obj%m_ite = lhs%m_ite
                    obj%m_jts = lhs%m_jts
                    obj%m_jte = lhs%m_jte
                    obj%m_m1s = lhs%m_m1s
                    obj%m_m1e = lhs%m_m1e
                    obj%m_m2s = lhs%m_m2s
                    obj%m_m2e = lhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
                    obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
                    obj%m_ctor_flags   = lhs%m_ctor_flags
                    obj%m_rand_distr_t = lhs%m_rand_distr_t
                    if(use_omp .EQ. .true.) then
                        call omp_arrays2d_pow_real(rhs,lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                                   obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d      )
                    else
                        obj%m_p2d     = lhs%m_p2d   ** rhs
                        obj%m_p8w2d   = lhs%m_p8w2d ** rhs
                        obj%m_maskp2d = lhs%m_maskp2d
                    end if
                    
          end select
          
    
    end  function
    
    
    function  field2D_pow_int(lhs,rhs,use_omp)    result(obj)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: lhs
          integer(I64),                     intent(in) :: rhs
          logical(I32),                     intent(in) :: use_omp
          ! Locals
          class(IAtmPressureField), allocatable        :: obj
          ! Start of executable statments
          
          allocate(CAtmPressureField2D :: obj)
          select type(obj)
          class is(CAtmPressureField2D)
                    obj%m_ims = lhs%m_ims
                    obj%m_ime = lhs%m_ime
                    obj%m_jms = lhs%m_jms
                    obj%m_jme = lhs%m_jme
                    obj%m_ids = lhs%m_ids
                    obj%m_ide = lhs%m_ide
                    obj%m_jds = lhs%m_jds
                    obj%m_jde = lhs%m_jds
                    obj%m_its = lhs%m_its
                    obj%m_ite = lhs%m_ite
                    obj%m_jts = lhs%m_jts
                    obj%m_jte = lhs%m_jte
                    obj%m_m1s = lhs%m_m1s
                    obj%m_m1e = lhs%m_m1e
                    obj%m_m2s = lhs%m_m2s
                    obj%m_m2e = lhs%m_m2e
!DIR$ IF (Declare_Destructor_Field2D .EQ. 1)
                    obj%m_is_initialized = lhs%m_is_initialized
!DIR$ ENDIF
                    obj%m_ctor_flags   = lhs%m_ctor_flags
                    obj%m_rand_distr_t = lhs%m_rand_distr_t
                    if(use_omp .EQ. .true.) then
                        call omp_arrays2d_pow_int(rhs,lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                                  obj%m_p2d,obj%m_p8w2d,obj%m_maskp2d    )
                    else
                        obj%m_p2d      = lhs%m_p2d   ** rhs
                        obj%m_p8w2d    = lhs%m_p8w2d ** rhs
                        obj%m_maskp2d  = lhs%m_maskp2d
                    end if
            
          end select
          
    
    end  function
    
    
    function  field2D_eq_field2D(lhs,rhs,use_omp,print_details,   &
                                fpcmp,ulp                    )    result(beq)
          implicit none
          use ISO_FORTRAN_ENV, only ERROR_UNIT
          class(CAtmPressureField2D),   intent(in) :: lhs
          class(IAtmPressureField),     intent(in) :: rhs
          logical(I32),                 intent(in) :: use_omp
          logical(I32), optional,       intent(in) :: print_details
          character(len=*),             intent(in) :: fpcmp
          integer(I32),                 intent(in) :: ulp
          ! Locals
          type(BoolCompField2D), allocatable       :: beq
          
          integer(I32)                             :: AllocErr
          character(len=256)                       :: EMSG
          character(*), parameter :: fname = "field2D_eq_field2D"
         
          ! Start of executable statements
          
          select type(rhs)
          class is(CAtmPressureField2D)
                call nonconforming_arrays2d(lhs,rhs,print_details,file_path,fname,6581)
            class default
                    ERROR STOP 'field2D_eq_field2D:loc(6551) -- Unsupported derived type!!'
          end select
          
          allocate(BoolCompField2D::beq,STAT=AllocErr,ERRMSG=EMSG)
          if(AllocErr /= 0) then
             call alloc2d_error_handler(AllocErr,EMSG,file_path,"field2D_eq_field2D",6599)
          end if    
          
          
          allocate(beq%bp2d(SIZE(lhs%m_p2d,dim=1),SIZE(lhs%m_p2d,dim=2)),       &
                   beq%bp8w2d(SIZE(lhs%m_p8w2d,dim=1),SIZE(lhs%m_p8w2d,dim=2)), &
                   beq%bp2dmsk(SIZE(lhs%m_maskp2d,dim=1),SIZE(lhs%m_maskp2d,dim=2)),STAT=AllocErr,ERRMSG=EMSG)
           if(AllocErr /= 0) then
              call alloc2d_error_handler(AllocErr,EMSG,file_path,"field2D_eq_field2D",6607)
           end if   
            
           beq%bp2d    = .false.
           beq%bp8w2d  = .false.
           beq%bp2dmsk = .false.
           
           select type(rhs)
           class is(CAtmPressureField2D)
               if(use_omp .EQ. .true.) then
                   call omp_arrays2d_eq(lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                        rhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                        beq%bp2d,beq%bp8w2d,beq%bp2dmsk,     &
                                        fpcmp,ulp,lhs%m_ims,lhs%m_jms)
               else
                   beq%bp2d    = lhs%m_p2d     == rhs%m_p2d
                   beq%bp8w2d  = lhs%m_p8w2d   == rhs%m_p8w2d
                   beq%bp2dmsk = lhs%m_maskp2d == rhs%m_maskp2d
               end if
           class default
              ERROR STOP 'field2D_eq_field2D:loc(6604) -- Unsupported polymorphic type!!' 
           end select
           
           
    end  function
    
    
    function  field2D_neq_field2D(lhs,rhs,use_omp,print_details, &
                                  fpcmp,ulp                  )     result(bneq)
          implicit none
          
          class(CAtmPressureField2D),       intent(in) :: lhs
          class(IAtmPressureField),         intent(in) :: rhs
          logical(I32),                     intent(in) :: use_omp
          logical(I32),optional,            intent(in) :: print_details
          character(len=*),                 intent(in) :: fpcmp
          integer(I32),                     intent(in) :: ulp
          ! Locals
          type(BoolCompField2D), allocatable           :: bneq
          integer(I32)                                 :: AllocErr
          character(len=256)                           :: EMSG
          character(*), parameter :: fname = "field1D_neq_field2D"
          
          ! Start of executable statements
          
          select type(rhs)
          class is(CAtmPressureField2D)
                call nonconforming_arrays2d(lhs,rhs,print_details,file_path,fname,6615)     ! lhs-rhs arrays conformance
            class default            
                 ERROR STOP 'field2D_neq_field2D:loc(6635) -- Unsupported polymorphic type!!'
          end select
          
          allocate(BoolCompField2D::bneq,STAT=AllocErr,ERRMSG=EMSG)
          if(AllocErr /= 0) then
             call alloc2d_error_handler(AllocErr,EMSG,file_path,"field2D_neq_field2D",6657)
          end if
          
          allocate(bneq%bp2d(SIZE(lhs%m_p2d,dim=1),SIZE(lhs%m_p2d,dim=2)),       &
                   bneq%bp8w2d(SIZE(lhs%m_p8w2d,dim=1),SIZE(lhs%m_p8w2d,dim=2)), &
                   bneq%bp2dmsk(SIZE(lhs%m_maskp2d,dim=1),SIZE(lhs%m_maskp2d,dim=2)),STAT=AllocErr,ERRMSG=EMSG)
          if(AllocErr /= 0) then
              call alloc2d_error_handler(AllocErr,EMSG,file_path,"field2D_neq_field2D",6664)
          end if
          
          bneq%bp2d    = .false.
          bneq%bp8w2d  = .false.
          bneq%bp2dmsk = .false.
          
          select type(rhs)
          class is(CAtmPressureField2D)
              if(use_omp .EQ. .true.) then
                  call omp_arrays2d_neq(lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                        rhs%m_p2d,rhs%m_p8w2d,rhs%m_maskp2d, &
                                        bneq%bp2d,bneq%bp8w2d,bneq%bp2dmsk,  &
                                        fpcmp,ulp,lhs%m_ims,lhs%m_jms   )
              else
                  bneq%bp2d   = lhs%m_p2d      /= rhs%m_p2d
                  bneq%bp8w2d = lhs%m_p8w2d    /= rhs%m_p8w2d
                  bneq%bp2dmsk = lhs%m_maskp2d /= rhs%m_maskp2d
              end if
          class default
                  ERROR STOP 'field2D_neq_field2D:loc(6693) -- Unsupported polymorphic type!!'
          end select
          
    end  function
    
    
    function  field2D_lt_field2D(lhs,rhs,use_omp,print_details,fpcmp)    result(blt)
          implicit none
          
          class(CAtmPressureField2D),       intent(in) :: lhs
          class(IAtmPressureField),         intent(in) :: rhs
          logical(I32),                     intent(in) :: use_omp
          logical(I32), optional,           intent(in) :: print_details
          character(len=*),                 intent(in) :: fpcmp
          ! Locals
          type(BoolCompField2D), allocatable           :: blt
          integer(I32)                                 :: AllocErr
          character(len=256)                           :: EMSG
          character(*), parameter :: fname = "field2D_lt_field2D"
         
          ! Start of executable statements
    
          select type(rhs)
          class is(CAtmPressureField2D)
              call nonconforming_arrays2d(lhs,rhs,print_details,file_path,fname,6707) ! lhs<->rhs arrays conformance
            class default
                    ERROR STOP 'field2D_lt_field2D:loc(6723) -- Unsupported polymorphic type!!'
          end select
          
          allocate(BoolCompField2D::blt,STAT=AllocErr,ERRMSG=EMSG)
          if(AllocErr /= 0) then
               call alloc2d_error_handler(AllocErr,EMSG,file_path,fname,6713)
          end if
          
          allocate(blt%bp2d(SIZE(lhs%m_p2d,dim=1),SIZE(lhs%m_p2d,dim=2)),       &
                   blt%bp8w2d(SIZE(lhs%m_p8w2d,dim=1),SIZE(lhs%m_p8w2d,dim=2)), &
                   blt%bp2dmsk(SIZE(lhs%m_maskp2d,dim=1),SIZE(lhs%m_maskp2d,dim=2)),STAT=AllocErr,ERRMSG=EMSG)
          if(AllocErr /= 0) then
             call alloc2d_error_handler(AllocErr,EMSG,file_path,fname,6720)
          end if
          
          blt%bp2d    = .false.
          blt%bp8w2d  = .false.
          blt%bp2dmsk = .false.
          
          select type(rhs)
          class is(CAtmPressureField2D)
              if(use_omp .EQ. .true.) then
                  call omp_arrays2d_lt(lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                       rhs%m_p2d,rhs%m_p8w2d,rhs%m_maskp2d, &
                                       blt%bp2d,blt%bp8w2d,blt%bp2dmsk,     & 
                                       fpcmp,lhs%m_ims,lhs%m_jms        )
              else
                  blt%bp2d    = lhs%m_p2d     < rhs%m_p2d
                  blt%bp8w2d  = lhs%m_p8w2d   < rhs%m_p8w2d
                  blt%bp2dmsk = lhs%m_maskp2d < rhs%m_maskp2d
              end if
              class default
                  ERROR STOP 'field2D_lt_field2D:loc(6780) -- Unsupported polymorphic type!!'
          end select
          
    
    end  function
    
    
    function  field2D_gt_field2D(lhs,rhs,use_omp,print_details,fpcmp)     result(bgt)
          implicit none
          class(CAtmPressureField2D),       intent(in) :: lhs
          class(IAtmPressureField),         intent(in) :: rhs
          logical(I32),                     intent(in) :: use_omp
          logical(I32), optional,           intent(in) :: print_details
          ! Locals
          type(BoolCompField2D), allocatable           :: bgt
          integer(I32)                                 :: AllocErr
          character(len=256)                           :: EMSG
          character(*), parameter                      :: fname = "field2D_gt_field2D"
         
          ! Start of executable statements
          
          select type(rhs)
          class is(CAtmPressureField2D)
                call nonconforming_arrays2d(lhs,rhs,print_details,file_path,fname,7013)
                class default
                    ERROR STOP 'field2D_gt_field2D:loc(6813) -- Unsupported polymorphic type!!'
          end select
          
          allocate(BoolCompField2D::bgt,STAT=AllocErr,ERR_MSG=EMSG)
          if(AllocErr /= 0) then
             call alloc2d_error_handler(AllocErr,EMSG,file_path,fname,7020)
          end if
          
          allocate(bgt%bp2d(SIZE(lhs%m_p2d,dim=1),SIZE(lhs%m_p2d,dim=2)),       &
                   bgt%bp8w2d(SIZE(lhs%m_p8w2d,dim=1),SIZE(lhs%m_p8w2d,dim=2)), &
                   bgt%bp2dmsk(SIZE(lhs%m_maskp2d,dim=1),SIZE(lhs%m_maskp2d,dim=2)),STAT=AllocErr,ERRMSG=EMSG)
          if(AllocErr /= 0) then
              call alloc2d_error_handler(AllocErr,EMSG,file_path,fname,7027) 
          end if
          
          bgt%bp2d    = .false.
          bgt%bp8w2d  = .false.
          bgt%bp2dmsk = .false.
          
          select type(rhs)
          class is(CAtmPressureField2D)
              if(use_omp .EQ. .true.) then
                  call omp_arrays2d_gt(lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d,  &
                                       rhs%m_p2d,rhs%m_p8w2d,rhs%m_maskp2d,  &
                                       bgt%bp2d,bgt%bp8w2d,bgt%bp2dmsk,      &
                                       fpcmp,lhs%m_ims,lhs%m_jms            )
              else
                  bgt%bp2d    = lhs%m_p2d     > rhs%m_p2d
                  bgt%bp8w2d  = lhs%m_p8w2d   > rhs%m_p8w2d
                  bgt%bp2dmsk = lhs%m_maskp2d > rhs%m_maskp2d
              end if
              class default
                  ERROR STOP 'field2D_gt_field2D:loc(6867) -- Unsupported polymorphic type!!'
          end select
          
                        
    end  function
    
    
    function  field2D_lte_field2D(lhs,rhs,use_omp,print_details,fpcmp)    result(blte)
          implicit none
          
          class(CAtmPressureField2D),       intent(in) :: lhs
          class(IAtmPressureField),         intent(in) :: rhs
          logical(I32),                     intent(in) :: use_omp
          logical(I32), optional,           intent(in) :: print_details
          character(len=*),                 intent(in) :: fpcmp
          ! Locals
          type(BoolCompField2D), allocatable           :: blte
          integer(I32)                                 :: AllocErr
          character(len=256)                           :: EMSG
          character(*), parameter                      :: fname = "field2D_lte_field2D"
         
          ! Start of executable statements
          
          select type(rhs)
          class is(CAtmPressureField2D)
             call nonconforming_arrays2d(lhs,rhs,print_details,file_path,fname,7072)
             class default
                 ERROR STOP 'field2D_lte_field2D:loc(6897) -- Unsupported polymorphic type!!'
          end select
          
          allocate(BoolCompField2D::blte,STAT=AllocErr,ERRMSG=EMSG)
          if(AllocErr /= 0) then 
             call alloc2d_error_handler(AllocErr,EMSG,file_path,fname,7079)
          end if
          
          allocate(blte%bp2d(SIZE(lhs%m_p2d,dim=1),SIZE(lhs%m_p2d,dim=2)),       &
                   blte%bp8w2d(SIZE(lhs%m_p8w2d,dim=1),SIZE(lhs%m_p8w2d,dim=2)), &
                   blte%bp2dmsk(SIZE(lhs%m_maskp2d,dim=1),SIZE(lhs%m_maskp2d,dim=2)), STAT=AllocErr,ERRMSG=EMSG)
          if(AllocErr /= 0) then
             call alloc2d_error_handler(AllocErr,EMSG,file_path,fname,7086) 
          end if
          
          blte%bp2d    = .false.
          blte%bp8w2d  = .false.
          blte%bp2dmsk = .false.
          
          select type(rhs)
          class is(CAtmPressureField2D)
              if(use_omp .EQ. .true.) then
                  call omp_arrays_lte(lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                      rhs%m_p2d,rhs%m_p8w2d,rhs%m_maskp2d, &
                                      blte%bp2d,blte%bp8w2d,blte%bp2dmsk,  &
                                      fpcmp,lhs%m_ims,lhs%m_jms)
              else
                  blte%bp2d    = lhs%m_p2d     <= rhs%m_p2d
                  blte%bp8w2d  = lhs%m_p8w2d   <= rhs%m_p8w2d
                  blte%bp2dmsk = lhs%m_maskp2d <= rhs%m_maskp2d
              end if
              class default
                  ERROR STOP 'field2D_lte_field2D:loc(6954) -- Unspupported polymorphic type!!'
          end select
          
    
    end  function
    
    
    function  field2D_gte_field2D(lhs,rhs,use_omp,print_details,fpcmp)    result(bgte)
          implicit none
          
          class(CAtmPressureField2D),   intent(in) :: lhs
          class(IAtmPressureField),     intent(in) :: rhs
          logical(I32),                 intent(in) :: use_omp
          logical(I32), optional,       intent(in) :: print_details
          character(len=*),             intent(in) :: fpcmp
          ! Locals
          type(BoolCompField2D), allocatable       :: bgte
          integer(I32)                             :: AllocErr
          character(len=256)                       :: EMSG
          character(*), parameter                  :: fname = "field2D_gte_field2D"
          
          ! Start of executable statements
          
          select type(rhs)
          class is(CAtmPressureField2D)
             call nonconforming_arrays2d(lhs,rhs,print_details,file_path,fname,7131)  
             class default     
                   ERROR STOP 'field2D_gte_field2D:loc(6986) -- Unsupported polymorphic type!!'
          end select
          
          allocate(BoolCompField2D::bgte,STAT=AllocErr,ERRMSG=EMSG)
          if(AllocErr /= 0) then
             call alloc2d_error_handler(AllocErr,EMSG,file_path,fname,7138) 
          end if
          
          allocate(bgte%bp2d(SIZE(lhs%m_p2d,dim=1),SIZE(lhs%m_p2d,dim=2)),       &
                   bgte%bp8w2d(SIZE(lhs%m_p8w2d,dim=1),SIZE(lhs%m_p8w2d,dim=2)), &
                   bgte%bp2dmsk(SIZE(lhs%m_maskp2d,dim=1),SIZE(lhs%m_maskp2d,dim=2)), STAT=AllocErr,ERRMSG=EMSG)
          if(AllocErr /= 0) then
             call alloc2d_error_handler(AllocErr,EMSG,file_path,fname,7145) 
          end if
          
          bgte%bp2d    = .false.
          bgte%bp8w2d  = .false.
          bgte%bp2dmsk = .false.
          
          select type(rhs)
          class is(CAtmPressureField2D)
              if(use_omp .EQ. .true.) then
                  call omp_arrays2d_gte(lhs%m_p2d,lhs%m_p8w2d,lhs%m_maskp2d, &
                                        rhs%m_p2d,rhs%m_p8w2d,rhs%m_maskp2d, &
                                        bgte%bp2d,bgte%bp8w2d,bgte%bp2dmsk,  &
                                        fpcmp,lhs%m_ims,lhs%m_jms           )
              else
                  bgte%bp2d    = lhs%m_p2d     >= rhs%m_p2d
                  bgte%bp8w2d  = lhs%m_p8w2d   >= rhs%m_p8w2d
                  bgte%bp2dmsk = lhs%m_maskp2d >= rhs%m_maskp2d
              end if
           class default
               ERROR STOP 'field2D_gte_field2D:loc(7044) -- Unsupported polymorphic type!!'
          end select
          
          
    end  function
    
    
    !================================38
    ! Implementation of concrete class
    ! 'CAtmPressureField3D'
    !================================38
    
    !================================38
    !  class Constructors
    !================================38
    
    type(CAtmPressureField3D)  function  def_ctor_field3D(ims,ime,jms,jme,kms,kme, &
                                                          ids,ide,jds,jde,kds,kde, &
                                                          its,ite,jts,jte,kts,kte, &
                                                          m1s,m1e,m2s,m2e,m3s,m3e  )
          implicit none
          
          integer(I64),         intent(in) :: ims,ime,jms,jme,kms,kme, &
                                              ids,ide,jds,jde,kds,kde, &
                                              its,ite,jts,jte,kts,kte, &
                                              m1s,m1e,m2s,m2e,m3s,m3e 
          ! Locals
          integer(I32)                     :: AllocErr
          character(len=256)               :: EMSG
          ! Start of executable statements
          if(debug_flag) call print_prologue('At prologue of: def_ctor_field3D:loc(7083)')
          
          if((ime .LE. 0)   .OR. &
             (jme .LE. 0)   .OR. &
             (kme .LE. 0)   .OR. &
             (m1s .LT. ims) .OR. &
             (m2s .LT. jms) .OR. &
             (m3s .LT. kms) .OR. &
             (m1e .GT. ime) .OR. &
             (m2e .GT. jme) .OR. &
             (m3e .GT. kme)      ) then                                               
                call check_dim3d(ime,jme,kme,m1s,m1e,m2s,  &
                                 m2e,m3s,m3e,"def_ctor_field3D",7234)
          end if
          
          def_ctor_field3D%m_ims=ims; def_ctor_field3D%m_ime=ime
          def_ctor_field3D%m_jms=jms; def_ctor_field3D%m_jme=jme
          def_ctor_field3D%m_kms=kms; def_ctor_field3D%m_kme=kme
          
          def_ctor_field3D%m_ids=ids; def_ctor_field3D%m_ide=ide
          def_ctor_field3D%m_jds=jds; def_ctor_field3D%m_jde=jde
          def_ctor_field3D%m_kds=kds; def_ctor_field3D%m_kde=kde
          
          def_ctor_field3D%m_its=its; def_ctor_field3D%m_ite=ite
          def_ctor_field3D%m_jts=jts; def_ctor_field3D%m_jte=jte
          def_ctor_field3D%m_kts=kts; def_ctor_field3D%m_kte=kte
          
          def_ctor_field3D%m_m1s=m1s; def_ctor_field3D%m_m1e=m1e
          def_ctor_field3D%m_m2s=m2s; def_ctor_field3D%m_m2e=m2e
          def_ctor_field3D%m_m3s=m3s; def_ctor_field3D%m_m3e=m3e

          def_ctor_field3D%m_ctor_flags(1) = .true.
          def_ctor_flags3D%m_ctor_flags(2) = .false.
          def_ctor_flags3D%m_ctor_flags(3) = .false.
          def_ctor_flags3D%m_ctor_flags(4) = .false.
          def_ctor_flags3D%m_ctor_flags(5) = .false.
          def_ctor_flags3D%m_rand_distr    = 'NONE'
    
          associate(d1s => def_ctor_field3D%m_ims,d1e => def_ctor_field3D%m_ime, &
                    d2s => def_ctor_field3D%m_kms,d2e => def_ctor_field3D%m_kme, &
                    d3s => def_ctor_field3D%m_jms,d3e => def_ctor_field3D%m_jme   )
              
                   allocate(def_ctor_field3D%m_p3d(d1s:d1e,d2s:d2e,d3s:d3e),     &
                            def_ctor_field3D%m_p8w3d(d1s:d1e,d2s:d2e,d3s:d3e),   &
                            def_ctor_field3D%m_maskp3d(d1s:d1e,d2s:d2e,d3s:d3e), &
                            STAT=AllocErr,ERRMSG=EMSG )
              
          end associate
          if(AllocErr /= 0) then
             call alloc3d_error_handler(AllocErr,EMSG,file_path,"def_ctor_field3D",7282)
          end if
    
          def_ctor_field3D%m_p3d     = TINY_PRESSURE
          def_ctor_field3D%m_p8w3d   = TINY_PRESSURE
          def_ctor_field3D%m_maskp3d = .false.
!DIR$ IF (Declare_Destructor_Field3D .EQ. 1)
          def_ctor_field3D%m_is_initialized = .true.
!DIR$ ENDIF          
          if(debug_flag) call print_epilogue('At epilogue of: def_ctor_field3D:loc(7147)')
          
    end  function
    
    
    type(CAtmPressureField3D)  function  scalar_ctor_field3D(ims,ime,kms,kme,jms,jme, &
                                                             ids,ide,kds,kde,jds,jde, &
                                                             its,ite,kts,kte,jts,jte, &
                                                             m1s,m1e,m2s,m2e,m3s,m3e, &
                                                             rndstr,p3val,p8wval,dtflag)
          implicit none
          
          integer(I64),         intent(in) :: ims,ime,kms,kme,jms,jme, &
                                              ids,ide,kds,kde,jds,jde, &
                                              its,ite,kts,kte,jts,jte, &
                                              m1s,m1e,m2s,m2e,m3s,m3e
          character(len=*),     intent(in) :: rndstr
          real(R64),            intent(in) :: p3val,p8wval
          logical(I64),         intent(in) :: dtflag
          ! Locals
          integer(I32)                     :: AllocErr
          character(len=256)               :: EMSG
          ! Start of executable statements
          
          if(debug_flag) call print_prologue('At prologue of: scalar_ctor_field3D:loc(7171)')
          
          if((ime .LE. 0)   .OR. &
             (jme .LE. 0)   .OR. &
             (kme .LE. 0)   .OR. &
             (m1s .LT. ims) .OR. &
             (m2s .LT. jms) .OR. &
             (m3s .LT. kms) .OR. &
             (m1e .GT. ime) .OR. &
             (m2e .GT. jme) .OR. &
             (m3e .GT. kme)      ) then                                                  
                call check_dim3d(ime,kme,jme,m1s,m1e, &
                                 m2s,m2e,m3s,m3e,"scalar_ctor_field3D",7316)
          end if
    
          scalar_ctor_field3D%m_ims=ims; scalar_ctor_field3D%m_ime=ime
          scalar_ctor_field3D%m_jms=jms; scalar_ctor_field3D%m_jme=jme
          scalar_ctor_field3D%m_kms=kme; scalar_ctor_field3D%m_kme=kme
          
          scalar_ctor_field3D%m_ids=ids; scalar_ctor_field3D%m_ide=ide
          scalar_ctor_field3D%m_jds=jds; scalar_ctor_field3D%m_jde=jde
          scalar_ctor_field3D%m_kds=kds; scalar_ctor_field3D%m_kde=kde
          
          scalar_ctor_field3D%m_its=its; scalar_ctor_field3D%m_ite=ite
          scalar_ctor_field3D%m_jts=jts; scalar_ctor_field3D%m_jte=jte
          scalar_ctor_field3D%m_kts=kts; scalar_ctor_field3D%m_kte=kte
          
          scalar_ctor_field3D%m_m1s=m1s; scalar_ctor_field3D%m_m1e=m1e
          scalar_ctor_field3D%m_m2s=m2s; scalar_ctor_field3D%m_m2e=m2e
          scalar_ctor_field3D%m_m3s=m3s; scalar_ctor_field3D%m_m3e=m3e
!DIR$ IF (Declare_Destructor_Field3D .EQ. 1)
          scalar_ctor_field3D%m_is_initialized = .true.
!DIR$ ENDIF
          scalar_ctor_field3D%m_ctor_flags(1) = .false.
          scalar_ctor_field3D%m_ctor_flags(2) = .true.
          scalar_ctor_field3D%m_ctor_flags(3) = .false.
          scalar_ctor_field3D%m_ctor_flags(4) = .false.
          scalar_ctor_field3D%m_ctor_flags(5) = .false.
          scalar_ctor_field3D%m_rand_distr    = rndstr
          
          associate(d1s => scalar_ctor_field3D%m_ims, &
                    d1e => scalar_ctor_field3D%m_ime, &
                    d2s => scalar_ctor_field3D%m_kms, &
                    d2e => scalar_ctor_field3D%m_kme, &
                    d3s => scalar_ctor_field3D%m_jms, &
                    d3e => scalar_ctor_field3D%m_jme  )
              
              allocate(scalar_ctor_field3D%m_p3d(d1s:d1e,d2s:d2e,d3s:d3e),    &
                       scalar_ctor_field3D%m_p8w3d(d1s:d1e,d2s:d2e,d3s:d3e),  &
                       scalar_ctor_field3D%m_maskp3d(d1s:d1e,d2s:d2e,d3s:d3e),&
                       STAT=AllocErr,ERRMSG=EMSG)
              
          end associate
          if(AllocErr /= 0) then
              call alloc3d_error_handler(AllocErr,EMSG,file_path,scalar_ctor_field3D,7367)
          end if
          scalar_ctor_field3D%m_p3d     = p3val
          scalar_ctor_field3D%m_p8w3d   = p8wval
          scalar_ctor_field3D%m_maskp3d = dtflag
          
          if(debug_flag) call print_epilogue("At epilogue of: scalar_ctor_field3D:loc(7374)")
          
    end  function
    
    
    type(CAtmPressureField3D)  function  field_ctor_field3D(ims,ime,kms,kme,jms,jme, &
                                                            ids,ide,kds,kde,jds,jde, &
                                                            its,ite,kts,kte,jts,jte, &
                                                            m1s,m1e,m2s,m2e,m3s,m3e, &
                                                            rndstr,p3d,p8w3d,maskp3d,&
                                                            use_omp                   )
          implicit none
          integer(I64),                     intent(in) ::  ims,ime,kms,kme,jms,jme, &
                                                           ids,ide,kds,kde,jds,jde, &
                                                           its,ite,kts,kte,jts,jte, &
                                                           m1s,m1e,m2s,m2e,m3s,m3e  
          character(len=*),                 intent(in) ::  rndstr
          real(R64),    dimension(:,:,:),   intent(in) ::  p3d
          !DIR$ ASSUME_ALIGNED p3d:32
          real(R64),    dimension(:,:,:),   intent(in) ::  p8w3d
          !DIR$ ASSUME_ALIGNED p8w3d:32
          logical(I64), dimension(:,:,:),   intent(in) ::  maskp3d
          !DIR$ ASSUME_ALIGNED maskp3d:32
          logical(I32),                     intent(in) ::  use_omp
          ! Locals
          integer(I32)                                 ::  AllocErr
          character(len=256)                           ::  EMSG
          character(len=80), dimension(6) :: msg = ['FATAL-ERROR in: CAtmPressureField3D%field_ctor_field3D', &
                                                     'Non-allocated allocatable array(s) passed!!', &
                                                     '***** ERROR-DETAILS *****'  , &
                                                     'Allocation status: '  , &
                                                     '***** ERROR-DETAILS *****' , &
                                                     'TERMINATING EXECUTION WITH STOP STATEMENT'   ]
          ! Start of executable statements
          if(debug_flag) call print_prologue("At prologue of: field_ctor_field3D:loc(7408)")
          
          if((ime .LE. 0)   .OR. &
             (jme .LE. 0)   .OR. &
             (kme .LE. 0)   .OR. &
             (m1s .LT. ims) .OR. &
             (m2s .LT. jms) .OR. &
             (m3s .LT. kms) .OR. &
             (m1e .GT. ime) .OR. &
             (m2e .GT. jme) .OR. &
             (m3e .GT. kme)      ) then                                                  
                call check_dim3d(ime,kme,jme,m1s,m1e, &
                                 m2s,m2e,m3s,m3e,"field_ctor_field3D",7400)
          end if
          call check_alloc3DR64_fail(p3d,msg,file_path,7422)
          call check_alloc3DR64_fail(p8w3d,msg,file_path,7423)
          call check_alloc3DL64_fail(maskp3d,msg,file_path,7424)
          
          field_ctor_field3D%m_ims=ims; field_ctor_field3D%m_ime=ime
          field_ctor_field3D%m_kms=kms; field_ctor_field3D%m_kme=kme
          field_ctor_field3D%m_jms= ms; field_ctor_field3D%m_jme=jme
          
          field_ctor_field3D%m_ids=ids; field_ctor_field3D%m_ide=ide
          field_ctor_field3D%m_kds=kds; field_ctor_field3D%m_kde=kde
          field_ctor_field3D%m_jds=jds; field_ctor_field3D%m_jde=jde
          
          field_ctor_field3D%m_its=its; field_ctor_field3D%m_ite=ite
          field_ctor_field3D%m_kts=kts; field_ctor_field3D%m_kte=kte
          field_ctor_field3D%m_jts=jts; field_ctor_field3D%m_jte=jte
          
          field_ctor_field3D%m_m1s=m1s; field_ctor_field3D%m_m1e=m1e
          field_ctor_field3D%m_m2s=m2s; field_ctor_field3D%m_m2e=m2e
          field_ctor_field3D%m_m3s=m3s; field_ctor_field3D%m_m3e=m3e
!DIR$ IF (Declare_Destructor_Field3D .EQ. 1)
          field_ctor_field3D%m_is_initialized = .true.
!DIR$ ENDIF
          field_ctor_field3D%m_ctor_flags(1)  = .false.
          field_ctor_field3D%m_ctor_flags(2)  = .false.
          field_ctor_field3D%m_ctor_flags(3)  = .true.
          field_ctor_field3D%m_ctor_flags(4)  = .false.
          field_ctor_field3D%m_ctor_flags(5)  = .false.
          field_ctor_field3D%m_rand_distr     =  rndstr
          
          associate(d1s => field_ctor_field3D%m_ims, &
                    d1e => field_ctor_field3D%m_ime, &
                    d2s => field_ctor_field3D%m_kms, &
                    d2e => field_ctor_field3D%m_kme, &
                    d3s => field_ctor_field3D%m_jms, &
                    d3e => field_ctor_field3D%m_jme   )
              
                   allocate(field_ctor_field3D%m_p3d(d1s:d1e,d2s:d2e,d3s:d3e),     &
                            field_ctor_field3D%m_p8w3d(d1s:d1e,d2s:d2e,d3s:d3e),   &
                            field_ctor_field3D%m_maskp3d(d1s:d1e,d2s:d2e,d3s:d3e), &
                            STAT=AllocErr,ERRMSG=EMSG)
         end associate
         if(AllocErr /= 0) then
            call alloc3d_error_handler(AllocErr,EMSG,file_path,"field_ctor_field3D",7463)
         end if
         if(use_omp .EQ. .true.) then
             call omp_copy_arrays3d(p3d,p8w3d,maskp3d,           &
                                    field_ctor_field3D%m_p3d,    &
                                    field_ctor_field3D%m_p8w3d,  &
                                    field_ctor_field3D%m_maskp3d   )
         else
              field_ctor_field3D%m_p3d     = p3d
              field_ctor_field3D%m_p8w3d   = p8w3d
              field_ctor_field3D%m_maskp3d = maskp3d
         end if
    
         if(debug_flag) call print_epilogue("At epilogue of: field_ctor_field3D:loc(7476)")
    
    end  function
    
    
    type(CAtmPressureField3D)  function  copy_ctor_field3D(other,use_omp)
          implicit none
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          class(CAtmPressureField3D),       intent(in) :: other
          logical(I32),                     intent(in) :: use_omp
          ! Locals
          integer(I32)                                 :: AllocErr
          character(len=256)                           :: EMSG
          ! Start of executable statements
          if(debug_flag) call print_prologue("At prologue of: copy_ctor_field3D:loc(7490)")
          if((other%m_ctor_flags(5) .EQ. .false.)  .OR. & 
             (.NOT. allocated(other%m_p3d)         .OR. &
              .NOT. allocated(other%m_p8w3d)        .OR. &
              .NOT. allocated(other%m_maskp3d))           ) then
                   write(ERROR_UNIT,*) '  [WARNING]: -- Argument is in deallocated state!!'
                   write(ERROR_UNIT,*) '  Performing an early exit!!'
                   return
          end if
          copy_ctor_field3D%m_ims=other%m_ims; copy_ctor_field3D%m_ime=other%m_ime
          copy_ctor_field3D%m_kms=other%m_kms; copy_ctor_field3D%m_kme=other%m_kme
          copy_ctor_field3D%m_jms=other%m_jms; copy_ctor_field3D%m_jme=other%m_jme
          
          copy_ctor_field3D%m_ids=other%m_ids; copy_ctor_field3D%m_ide=other%m_ide
          copy_ctor_field3D%m_kds=other%m_kds; copy_ctor_field3D%m_kde=other%m_kde
          copy_ctor_field3D%m_jds=other%m_jds; copy_ctor_field3D%m_jde=other%m_jde
          
          copy_ctor_field3D%m_its=other%m_its; copy_ctor_field3D%m_ite=other%m_ite
          copy_ctor_field3D%m_kts=other%m_kts; copy_ctor_field3D%m_kte=other%m_kte
          copy_ctor_field3D%m_jts=other%m_jts; copy_ctor_field3D%m_jte=other%m_jte
          
          copy_ctor_field3D%m_m1s=other%m_m1s; copy_ctor_field3D%m_m1e=other%m_m1e
          copy_ctor_field3D%m_m2s=other%m_m2s; copy_ctor_field3D%m_m2e=other%m_m2e
          copy_ctor_field3D%m_m3s=other%m_m3s; copy_ctor_field3D%m_m3e=other%m_m3e
!DIR$ IF (Declare_Destructor_Field3D .EQ. 1)
          copy_ctor_field3D%m_is_initialized=other%m_is_initialized
!DIR$ ENDIF
          copy_ctor_field3D%m_ctor_flags(1) = .false.
          copy_ctor_field3D%m_ctor_flags(2) = .false.
          copy_ctor_field3D%m_ctor_flags(3) = .false.
          copy_ctor_field3D%m_ctor_flags(4) = .true.
          copy_ctor_field3D%m_ctor_flags(5) = .false.
          copy_ctor_field3D%m_rand_distr    = other%m_rand_distr
          associate(d1s => copy_ctor_field3D%m_ims,  &
                    d1e => copy_ctor_field3D%m_ime,  &
                    d2s => copy_ctor_field3D%m_kms,  &
                    d2e => copy_ctor_field3D%m_kme,  &
                    d3s => copy_ctor_field3D%m_jms,  &
                    d3e => copy_ctor_field3D%m_jme   )
              
                    allocate(copy_ctor_field3D%m_p3d(d1s:d1e,d2s:d2e,d3s:d3e),     &
                             copy_ctor_field3D%m_p8w3d(d1s:d1e,d2s:d2e,d3s:d3e),   &
                             copy_ctor_field3D%m_maskp3d(d1s:d1e,d2s:d2e,d3s:d3e), &
                             STAT=AllocErr,ERRMSG=EMSG)
          end associate
          if(AllocErr /= 0) then
              call alloc3d_error_handler(AllocErr,EMSG,file_path,"copy_ctor_field3D",7528)
          end if
          if(use_omp .EQ. .true.) then
              call omp_copy_arrays3d(other%m_p3d,other%m_p8w3d,other%m_maskp3d,         &
                                     copy_ctor_field3D%m_p3d,copy_ctor_field3D%m_p8w3d, &
                                     copy_ctor_field3D%m_maskp3d                        )
          else
               copy_ctor_field3D%m_p3d     = other%m_p3d
               copy_ctor_field3D%m_p8w3d   = other%m_p8w3d
               copy_ctor_field3D%m_maskp3d = other%m_maskp3d
          end if
          if(debug_flag) call print_epilogue("At epilogue of: copy_ctor_field3D:loc(7540)")
    
    end  function
    
    
    type(CAtmPressureField3D)  function  move_ctor_field3D(other)  
          implicit none
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          class(CAtmPressureField3D),   intent(inout) :: other
          ! Start of executable statements
          if(debug_flag) call print_prologue("At prologue of: move_ctor_field3D:loc(7549)")
          if(other%m_ctor_flags(5) .EQ. .false. .OR. & 
            (.NOT. allocated(other%m_p3d)       .OR. &
             .NOT. allocated(other%m_p8w3d)     .OR. &
             .NOT. allocated(other%m_maskp3d))       ) then    ! Object is in deallocated state.
                   write(ERROR_UNIT,*) '  [WARNING]: -- Argument is in deallocated state!!'
                   write(ERROR_UNIT,*) '  Performing an early exit!!'
                   return
          end if
          move_ctor_field3D%m_ims=other%m_ims; move_ctor_field3D%m_ime=other%m_ime
          move_ctor_field3D%m_kms=other%m_kms; move_ctor_field3D%m_kme=other%m_kme
          move_ctor_field3D%m_jms=other%m_jms; move_ctor_field3D%m_jme=other%m_jme
          
          move_ctor_field3D%m_ids=other%m_ids; move_ctor_field3D%m_ide=other%m_ide
          move_ctor_field3D%m_kds=other%m_kds; move_ctor_field3D%m_kde=other%m_kde
          move_ctor_field3D%m_jds=other%m_jds; move_ctor_field3D%m_jde=other%m_jde
          
          move_ctor_field3D%m_its=other%m_its; move_ctor_field3D%m_jts=other%m_jts
          move_ctor_field3D%m_kts=other%m_kts; move_ctor_field3D%m_kts=other%m_kts
          move_ctor_field3D%m_jts=other%m_jts; move_ctor_field3D%m_jte=other%m_jte
          
          move_ctor_field3D%m_m1s=other%m_m1s; move_ctor_field3D%m_m1e=other%m_m1e
          move_ctor_field3D%m_m2s=other%m_m2s; move_ctor_field3D%m_m2e=other%m_m2e
          move_ctor_field3D%m_m3s=other%m_m3s; move_ctor_field3D%m_m3e=other%m_m3e
!DIR$ IF (Declare_Destructor_Field3D .EQ. 1)
          move_ctor_field3D%m_is_initialized=other%m_is_initialized
!DIR$ ENDIF
          move_ctor_field3D%m_ctor_flags(1) = .false.
          move_ctor_field3D%m_ctor_flags(2) = .false.
          move_ctor_field3D%m_ctor_flags(3) = .false.
          move_ctor_field3D%m_ctor_flags(4) = .false.
          move_ctor_field3D%m_ctor_flags(5) = .true.
          move_ctor_field3D%m_rand_distr    = other%m_rand_distr
          call move_alloc(other%m_p3d,    move_ctor_field3D%m_p3d)
          call move_alloc(other%m_p8w3d,  move_ctor_field3D%m_p8w3d)
          call move_alloc(other%m_maskp3d,move_ctor_field3D%m_maskp3d)
          ! Begin nullyfication of 'other' members.
          other%m_ims=0; other%m_ime=0; other%m_kms=0; other%m_kme=0
          other%m_jms=0; other%m_jme=0;
          other%m_ids=0; other%m_ide=0; other%m_kds=0; other%m_kde=0
          other%m_jds=0; other%m_jde=0
          other%m_its=0; other%m_its=0; other%m_kts=0; other%m_kte=0
          other%m_jts=0; other%m_jte=0
          other%m_m1s=0; other%m_m1e=0; other%m_m2s=0; other%m_m2e=0
          other%m_m3s=0; other%m_m3e=0
!DIR$ IF (Declare_Destructor_Field3D .EQ. 1)
          other%m_is_initialized = .false.
!DIR$ ENDIF
          other%m_rand_distr = ''
          if(debug_flag) call print_epilogue("At epilogue of: move_ctor_field3D:loc(7611)")
          
    end  function
    
!DIR$ IF (Declare_Destructor_Field3D .EQ. 1)
    
    subroutine  destroy_field3D(this)
          implicit none
          use ISO_FORTRAN_ENV,only : ERROR_UNIT
          class(CAtmPressureField3D),   intent(inout) :: this
          ! Locals
          integer(I32)                                :: DeallocErr
          character(len=256)                          :: EMSG
          ! Start of executable statements
          if(debug_flag) call print_prologue("At prologue of: destroy_field3D:loc(7620)")
          if(.NOT. this%m_is_initialized) then
             write(ERROR_UNIT,*) ' [FATAL-ERROR]: Object is in uninitialized state!!'
             write(ERROR_UNIT,*) ' State of object: ', this%m_is_initialized
             write(ERROR_UNIT,*) ' Executing early exit!!'
             return
          end if
          this%m_ims=0; this%m_ime=0; this%m_kms=0; this%m_kme=0
          this%m_jms=0; this%m_jme=0
          this%m_ids=0; this%m_ide=0; this%m_kds=0; this%m_kde=0
          this%m_jds=0; this%m_jde=0
          this%m_its=0; this%m_ite=0; this%m_kts=0; this%m_kte=0
          this%m_jts=0; this%m_jte=0
          this%m_m1s=0; this%m_m1e=0; this%m_m2s=0; this%m_m2e=0
          this%m_m3s=0; this%m_m3e=0
          this%m_ctor_flags = .false.
          this%m_rand_distr = ''
          if(allocated(this%m_p3d)    .AND.  &
             allocated(this%m_p8w3d)  .AND.  &
             allocated(this%m_maskp3d)       ) then
             deallocate(this%m_p3d,this%m_p8w3d,this%m_maskp3d, &
                        STAT=DeallocErr,ERRMSG=EMSG)
             if(DeallocErr /= 0) then
                 call alloc3d_error_handler(DeallocErr,EMSG,file_path,"destroy_field3D",7644)
             end if
          end if
             
          this%m_is_initialized = .false.
          if(debug_flag) call print_epilogue("At epilogue of: destroy_field3D:loc(7648)")
          
    end subroutine      
    
!DIR$ ENDIF    
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_ims
#endif
    
    pure  function get_field3D_ims(this)  result(ret_ims)
          implicit none
          class(CAtmPressureField3D), intent(in) :: this
          ! Locals
          integer(I64)                           :: ret_ims
          ! Start of executable ststements
          ret_ims = this%m_ims
    
    end  function

#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_ime
#endif
    
    pure  function get_field3D_ime(this)  result(ret_ime)
          implicit none
          class(CAtmPressureField3D), intent(in) :: this
          ! Locals
          integer(I64)                           :: ret_ime
          ! Start of executable statements
          ret_ime = this%m_ime
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_jms
#endif
    
    pure  function get_field3D_jms(this)  result(ret_jms)
          implicit none
          class(CAtmPressureField3D), intent(in) :: this
          ! Locals
          integer(I64)                           :: ret_jms
          ! Start of executable statements
          ret_jms = this%m_jms
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_jme
#endif
    
    pure  function get_field3D_jme(this)  result(ret_jme)
          implicit none
          class(CAtmPressureField3D), intent(in) :: this
          ! Locals
          integer(I64)                           :: ret_jme
          ! Start of executable ststements
          ret_jme = this%m_jme
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_kms
#endif
    
    pure  function get_field3D_kms(this)  result(ret_kms)
          implicit none
          class(CAtmPressureField3D), intent(in) :: this
          ! Locals
          integer(I64)                           :: ret_kms
          ! Start of executable statements
          ret_kms = this%m_kms
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_kme
#endif
    
    pure  function get_field3D_kme(this)  result(ret_kme)
          implicit none
          class(CAtmPressureField3D), intent(in) :: this
          ! Locals
          integer(I64),                          :: ret_kme
          ! Start of executable statements
          ret_kme = this%m_kme
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_ids
#endif
    
    pure  function get_field3D_ids(this)  result(ret_ids)
          implicit none
          class(CAtmPressureField3D),  intent(in) :: this
          ! Locals
          integer(I64),                           :: ret_ids
          ! Start of executable statements
          ret_ids = this%m_ids
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_ide
#endif
    
    pure  function get_field3D_ide(this)  result(ret_ide)
          implicit none
          class(CAtmPressureField3D),  intent(in) :: this
          ! Locals
          integer(I64),                           :: ret_ide
          ! Start of executable statements
          ret_ide = this%m_ide
          
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_jds
#endif
    
    pure  function get_field3D_jds(this)  result(ret_jds)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_jds
          ! Start of executable statements
          ret_jds = this%m_jds
    
    end function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_jde
#endif
    
    pure  function get_field3D_jde(this)  result(ret_jde)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_jde
          ! Start of executable stataments
          ret_jde = this%m_jde
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_kds
#endif
    
    pure  function get_field3D_kds(this)  result(ret_kds)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_kds
          ! Start of executable ststements
          ret_kds = this%m_kds
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_kde
#endif
    
    pure  function get_field3D_kde(this)  result(ret_kde)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_kde
          ! Start of executable statement
          ret_kde = this%m_kde
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_its
#endif
    
    pure  function get_field3D_its(this)  result(ret_its)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_its
          ! Start of executable statements
          ret_its = this%m_its
    
    end  function
    
#if USE_INLINING == 1   
    !DIR$ ATTRIBUTES INLINE :: get_field3D_ite
#endif
    
    pure  function get_field3D_ite(this)  result(ret_ite)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_ite
          ! Start of executable statements
          ret_ite = this%m_ite
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_jts
#endif
    
    pure  function get_field3D_jts(this)  result(ret_jts)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_jts
          ! Start of executable
          ret_jts = this%m_jts
    
    end  function

#if USE_INLINING == 1   
    !DIR$ ATTRIBUTES INLINE :: get_field3D_jte
#endif
    
    pure  function get_field3D_jte(this)  result(ret_jte)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_jte
          ! Start of execuatble statements
          ret_jte = this%m_jte
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_kts
#endif
    
    pure  function get_field3D_kts(this)  result(ret_kts)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_kts
          ! Start of executable statements
          ret_kts = this%m_kts
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_kte
#endif
    
    pure  function get_field3D_kte(this)  result(ret_kte)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_kte
          ! Start of executable statements
          ret_kte = this%m_kte
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_m1s
#endif
    
    pure  function get_field3D_m1s(this)  result(ret_m1s)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_m1s
          ! Start of executable statements
          ret_m1s = this%m_m1s
    
    end  function

#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_m1e
#endif
    
    pure  function get_field3D_m1e(this)  result(ret_m1e)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_m1e
          ! Start of executable statements
          ret_m1e = this%m_m1e
    
    end  function

#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_m2s
#endif
    
    pure  function get_field3D_m2s(this)  result(ret_m2s)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_m2s
          ! Start of executable ststements
          ret_m2s = this%m_m2s
    
    end  function
    

#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_m2e
#endif
    
    pure function get_field3D_m2e(this)   result(ret_m2e)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_m2e
          ! Start of executable statements
          ret_m2e = this%m_m2e
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_m3s
#endif
    
    pure  function get_field3D_m3s(this)  result(ret_m3s)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_m3s
          ! Start of executable stataments
          ret_m3s = this%m_m3s
    
    end  function

#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_m3e
#endif
    
    pure  function get_field3D_m3e(this)  result(ret_m3e)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          integer(I64),                            :: ret_m3e
          ! Start of executable statements
          ret_m3e = this%m_m3e
    
    end  function

!DIR$ IF (Declare_Destructor_Field3D .EQ. 1)
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_init

    pure  function get_field3D_init(this)   result(ret_init)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          logical(I32),                            :: ret_init
          ! Start of executable statements
          ret_init = this%m_is_initialized
    
    end  function
!DIR$ ENDIF  
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_ctor_flags
#endif
    
    pure  function get_field3D_ctor_flags(this)   result(ret_flags)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          logical(I32), dimension(5)               :: ret_flags
          ! Start of executable statements
          ret_flags = this%m_ctor_flags
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_field3D_rand_distr
#endif
    
    pure  function get_field3D_rand_distr(this)   result(ret_rdstr)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          character(len=64),                       :: ret_rdstr
          ! Start of executable statements
          ret_rdstr = this%m_rand_distr
    
    end  function

#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_p3d
#endif
    
    pure  function get_p3d(this)   result(ret_p3d)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          real(R64), allocatable, dimension(:,:,:) :: ret_p3d
          !DIR$ ATTRIBUTES ALIGN ret_p3d:32
          ! Start of executable statements
          ret_p3d = this%m_p3d
    
    end  function
    
#if USE_INLINING == 1
    !DIR$ ATTRIBUTES INLINE :: get_p8w3d
#endif
    
    pure  function get_p8w3d(this)   result(ret_p8w3d)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          real(R64), allocatable, dimension(:,:,:) :: ret_p8w3d
          !DIR$ ATTRIBUTES ALIGN ret_p8w3d:32
          ! Start of executable stataments
          ret_p8w3d = this%m_p8w3d
    
    end  function
    
#if USE_INLINE == 1
    !DIR$ ATTRIBUTES INLINE :: get_maskp3d
#endif

    pure  function get_maskp3d(this)   result(ret_maskp3d)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          ! Locals
          logical(I64), allocatable, dimension(:,:,:) :: ret_maskp3d
          !DIR$ ATTRIBUTES ALIGN  ret_maskp3d:32
          ! Start of executable statements
          ret_maskp3d = this%m_maskp3d
    
    end  function
    
    
    function  scalar_from_p3d(this,idx,kdx,jdx)  result(ret_val)
          implicit none
          class(CAtmPressureField3D), intent(in) :: this
          integer(I64),               intent(in) :: idx,kdx,jdx
          ! Locals
          real(R64)                              :: ret_val
          character(len=80), dimension(10) :: msg_ar = [ 'FATAL-ERROR in: CAtmPressureField3D%scalar_from_p3d', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable stataments
          if((idx.LT.this%m_ims .OR. idx.GT.this%m_ime) .OR. &
             (kdx.LT.this%m_kms .OR. kdx.GT.this%m_kme) .OR. &
             (jdx.LT.this%m_jms .OR. jdx.GT.this%m_jme)      ) then
                call check_dim3Di64_eq(idx,kdx,jdx,this%m_ime,this%m_kme, &
                                       this%m_jme,msg_ar,file_path,8117)
          end if
          ret_val = this%m_p3d(idx,kdx,jdx)   
              
    end  function
    
    
    function  scalar_from_p8w3d(this,idx,kdx,jdx)    result(ret_val)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          integer(I64),                 intent(in) :: idx,kdx,jdx
          ! Locals
          real(R64)                                :: ret_val
          character(len=80), dimension(10) :: msg_ar = [ '[FATAL-ERROR] in: CAtmPressureField3D%scalar_from_p8w3d', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
           !Start of executable statements
           if((idx.LT.this%m_ims .OR. idx.GT.this%m_ime) .OR. &
              (kdx.LT.this%m_kms .OR. kdx.GT.this%m_kme) .OR. &
              (jdx.LT.this%m_jms .OR. jdx.GT.this%m_jme)      ) then
                call check_dim3Di64_eq(idx,kdx,jdx,this%m_ime,this%m_kme, &
                                       this%m_jme,msg_ar,file_path,8144)
           end if
           ret_val = this%m_p8w3d(idx,kdx,jdx)
           
    end  function
    
    
    function  scalar_from_maskp3d(this,idx,kdx,jdx)   result(ret_val)
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          integer(I64),                 intent(in) :: idx,kdx,jdx
          ! Locals
          logical(I64),                 intent(in) :: ret_val
          character(len=80), dimension(10) :: msg_ar = [ '[FATAL-ERROR] in: CAtmPressureField3D%scalar_from_maskp3d', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          if(( idx.LT.this%m_ims .OR. idx.GT.this%m_ime) .OR. &
              (kdx.LT.this%m_kms .OR. kdx.GT.this%m_kme) .OR. &
              (jdx.LT.this%m_jms .OR. jdx.GT.this%m_jme)      ) then
                call check_dim3Di64_eq(idx,kdx,jdx,this%m_ime,this%m_kme, &
                                       this%m_jme,msg_ar,file_path,8173)
          end if
          ret_val = this%m_maskp3d(idx,kdx,jdx)
    
    end  function
    
    
    function  subarray_from_p3d(this,dim1s,dim1e,dim2s, &
                                dim2e,dim3s,dim3e         )  result(ret_arr)
          implicit none
          class(CAtmPressureField3D),       intent(in) :: this
          integer(I64),                     intent(in) :: dim1s,dim1e, &
                                                          dim2s,dim2e, &
                                                          dim3s,dim3e
          ! Locals
          real(R64), allocatable, dimension(:,:,:)     :: ret_arr
          !DIR$ ATTRIBUTES ALIGN ret_arr:32
          character(len=80), dimension(10) :: msg_ar = [ '[FATAL-ERROR] in: CAtmPressureField3D%subarray_from_p3d', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable
          if((dim1s.LT.this%m_ims .OR. dim1e.GT.this%m_ime) .OR. &
             (dim2s.LT.this%m_kms .OR. dim2e.GT.this%m_kme) .OR. &
             (dim3s.LT.this%m_jms .OR. dim3e.GT.this%m_jme)     )   then
                call check_dim3Di64_eq(dim1e,dim2e,dim3e,this%m_ime, &
                                       this%m_kme,this%m_jme,msg_ar,file_path,8205)
          end if
    
          ret_arr = this%m_p3d(dim1s:dim1e,dim2s:dim2e,dim3s:dim3e)
          
    end  function
    
    
    function  subarray_from_p8w3d(this,dim1s,dim1e,dim2s, &
                                  dim2e,dim3s,dim3e        )   result(ret_arr)
          implicit none
          class(CAtmPressureField3D),       intent(in) :: this
          integer(I64),                     intent(in) :: dim1s,dim1e, &
                                                          dim2s,dim2e, &
                                                          dim3e,dim3e
          ! Locals
          real(R64), allocatable, dimension(:,:,:)     :: ret_arr
          !DIR$ ATTRIBUTES ALIGN ret_arr:32
          character(len=80), dimension(10) :: msg_ar = [ '[FATAL-ERROR] in: CAtmPressureField3D%subarray_from_p8w3d', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          if((dim1s.LT.this%m_ims .OR. dim1e.GT.this%m_ime) .OR. &
             (dim2s.LT.this%m_kms .OR. dim2e.GT.this%m_kme) .OR. &
             (dim3s.LT.this%m_jms .OR. dim3e.GT.this%m_jme)     )   then
                call check_dim3Di64_eq(dim1e,dim2e,dim3e,this%m_ime, &
                                       this%m_kme,this%m_jme,msg_ar,file_path,8238)
          end if
    
          ret_arr = this%m_p8w3d(dim1s:dim1e,dim2s:dim2e,dim3s:dim3e)
          
    end  function
    
    
    function  subarray_from_maskp3d(this,dim1s,dim1e,dim2s, &
                                    dim2e,dim3s,dim3e       )   result(ret_arr)
          implicit none
          class(CAtmPressureField3D),      intent(in) :: this
          integer(I64),                    intent(in) :: dim1s,dim1e, &
                                                         dim2s,dim2e, &
                                                         dim3s,dim3e  
          ! Locals
          logical(I64), allocatable, dimension(:,:,:) :: ret_arr
          !DIR$ ATTRIBUTES ALIGN ret_arr:32
          character(len=80), dimension(10) :: msg_ar = [ '[FATAL-ERROR] in: CAtmPressureField3D%subarray_from_maskp3d', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          if((dim1s.LT.this%m_ims .OR. dim1e.GT.this%m_ime)  .OR. &
             (dim2s.LT.this%m_kms .OR. dim2e.GT.this%m_kme)  .OR. &
             (dim3s.LT.this%m_jms .OR. dim3e.GT.this%m_jme)     )   then
                call check_dim3Di64_eq(dim1e,dim2e,dim3e,this%m_ime, &
                                       this%m_kme,this%m_jme,msg_ar,file_path,8271)
          end if
          
          ret_arr = this%m_maskp3d(dim1s:dim1e,dim2s:dim2e,dim3s:dim3e)
          
    end  function
    
    
    subroutine  set_p3d_from_scalar(this,idx,kdx,jdx,val,set_all)
          implicit none
          class(CAtmPressureField3D),   intent(inout) :: this
          integer(I64),                 intent(in)    :: idx,kdx,jdx
          real(R64),                    intent(in)    :: val
          logical(I32), optional,       intent(in)    :: set_all
          ! Locals
          character(len=80), dimension(10) :: msg_ar = [ '[FATAL-ERROR] in: CAtmPressureField3D%set_p3d_from_scalar', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          if(( idx.LT.this%m_ims .OR. idx.GT.this%m_ime) .OR. &
              (kdx.LT.this%m_kms .OR. kdx.GT.this%m_kme) .OR. &
              (jdx.LT.this%m_jms .OR. jdx.GT.this%m_jme)      ) then
                call check_dim3Di64_eq(idx,kdx,jdx,this%m_ime,this%m_kme, &
                                       this%m_jme,msg_ar,file_path,8301)
          end if 
          if(present(set_all) .AND. (set_all .EQ. .true)) then
             this%m_p3d = val
          else
              this%m_p3d(idx,kdx,jdx) = val
          end if
          
    end  subroutine
    
    
    subroutine  set_p8w3d_from_scalar(this,idx,kdx,jdx,val,set_all)
          implicit none
          class(CAtmPressureField3D),   intent(inout) :: this
          integer(I64),                 intent(in)    :: idx,kdx,jdx
          real(R64),                    intent(in)    :: val
          logical(I32), optional,       intent(in)    :: set_all
          ! Locals
          character(len=80), dimension(10) :: msg_ar = [ '[FATAL-ERROR] in: CAtmPressureField3D%set_p8w3d_from_scalar', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          if(( idx.LT.this%m_ims .OR. idx.GT.this%m_ime) .OR. &
              (kdx.LT.this%m_kms .OR. kdx.GT.this%m_kme) .OR. &
              (jdx.LT.this%m_jms .OR. jdx.GT.this%m_jme)      ) then
                call check_dim3Di64_eq(idx,kdx,jdx,this%m_ime,this%m_kme, &
                                       this%m_jme,msg_ar,file_path,8334)
          end if 
          if(present(set_all) .AND. (set_all .EQ. .true.)) then
              this%m_p8w3d = val
          else
              this%m_p8w3d(idx,kdx,jdx) = val
          end if
          
    end  subroutine
    
    
    subroutine  set_maskp3d_from_scalar(this,idx,kdx,jdx,val,set_all)
          implicit none
          class(CAtmPressureField3D),     intent(inout) :: this
          integer(I64),                   intent(in)    :: idx,kdx,jdx
          logical(I64),                   intent(in)    :: val
          logical(I32),optional,          intent(in)    :: set_all
          ! Locals
          character(len=80), dimension(10) :: msg_ar = [ '[FATAL-ERROR] in: CAtmPressureField3D%set_maskp3d_from_scalar', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          if(( idx.LT.this%m_ims .OR. idx.GT.this%m_ime) .OR. &
              (kdx.LT.this%m_kms .OR. kdx.GT.this%m_kme) .OR. &
              (jdx.LT.this%m_jms .OR. jdx.GT.this%m_jme)      ) then
                call check_dim3Di64_eq(idx,kdx,jdx,this%m_ime,this%m_kme, &
                                       this%m_jme,msg_ar,file_path,8367)
          end if 
          if(present(set_all) .AND. (set_all .EQ. .true.)) then
              this%m_maskp3d = val
          else
              this%m_maskp3d(idx,kdx,jdx) = val
          end if
          
    end  subroutine
    
    
    subroutine  set_p3d_from_subarray(this,a1d,b1d,c1d,d3d)
          
          implicit none
        
          class(CAtmPressureField3D),            intent(inout) :: this
          real(R64), optional, dimension(:),     intent(in)    :: a1d,b1d,c1d
          !DIR$ ASSUME_ALIGNED a1d:32, b1d:32, c1d:32 
          real(R64),           dimension(:,:,:), intent(in)    :: d3d
          !DIR$ ASSUME_ALIGNED d3d:32
          ! Locals
          integer(I64)                                         :: sp3d1,sp3d2,sp3d3, &
                                                                  sa1d,sb1d,sc1d ,   &
                                                                  sd3d1,sd3d2,sd3d3
          integer(I64)                                         :: i,k,j
          character(len=80), dimension(10) :: msg_ar = [ '[FATAL-ERROR] in: CAtmPressureField3D%set_p3d_from_subarray', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          sp3d1 = SIZE(this%m_p3d,dim=1)
          sp3d2 = SIZE(this%m_p3d,dim=2)
          sp3d3 = SIZE(this%m_p3d,dim=3)
          sd3d1 = SIZE(d3d,dim=1)
          sd3d2 = SIZE(d3d,dim=2)
          sd3d3 = SIZE(d3d,dim=3)
          if(present(a1d) .AND. &
             present(b1d) .AND. &
             present(c1d)       ) then
               sa1d = SIZE(a1d)
               sb1d = SIZE(b1d)
               sc1d = SIZE(c1d)
               if(sp3d1 /= sa1d .OR. &
                  sp3d2 /= sb1d .OR. &
                  sp3d3 /= sc1d     )  then
                   call check_dim3Di64_eq(sp3d1,sp3d2,sp3d3, &
                                          sa1d,sb1d,sc1d,msg_ar,file_path,8412)
               end if
                !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))  
               do i = this%m_ims, sp3d1
                  this%m_p3d(i,:,:) = a1d(i)
               end do
               !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
               do k = this%m_kms, sp3d2
                   this%m_p3d(:,k,:) = b1d(k)
               end do
               !DIR$ SIMD VECTORLENGTHFOR(KIND=8))
               do j = this%m_jms, sp3d3
                   this%m_p3d(:,:,j) = c1d(j)
               end do
          else
              if(sp3d1 /= sd3d1 .OR. &
                 sp3d2 /= sd3d2 .OR. &
                 sp3d3 /= sd3d3      ) then
                  call check_dim3Di64_eq(sp3d1,sp3d2,sp3d3,  &
                                         sd3d1,sd3d2,sd3d3,msg_ar,file_path,8438)
              end if
              do j = this%m_jms, sp3d3
                  do k = this%m_kms, sp3d2
                      !DIR$ SIMD VECTORLENGTHFOR(KIND=8))
                      do i = this%m_ims, sp3d3
                         this%m_p3d(i,k,j) = d3d(i,k,j)
                      end do
                  end do
              end do
              
          end if
             
    end  subroutine       
         
             
    subroutine  set_p8w3d_from_subarray(this,a1d,b1d,c1d,d3d)
          implicit none
          class(CAtmPressureField3D),             intent(inout) :: this
          real(R64), optional, dimension(:),      intent(in)    :: a1d,b1d,c1d
          !DIR$ ASSUME_ALIGN a1d:32, b1d:32, c1d:32
          real(R64), dimension(:,:,:),            intent(in)    :: d3d
          !DIR$ ASSUME_ALIGN d3d:32
          ! Locals
          integer(I64)                                          :: sa1d,sb1d,sc1d,    &
                                                                   sp8wd1,sp8wd2,sp8wd3, &
                                                                   sd3d1,sd3d2,sd3d3
          integer(I64)                                          :: i,j,k
          character(len=80), dimension(10) :: msg_ar = [ '[FATAL-ERROR] in: CAtmPressureField3D%set_p8w3d_from_subarray', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          sp8wd1 = SIZE(this%m_p8w3d,dim=1)
          sp8wd2 = SIZE(this%m_p8w3d,dim=2)
          sp8wd3 = SIZE(this%m_p8w3d,dim=3)
          sd3d1  = SIZE(d3d,dim=1)
          sd3d2  = SIZE(d3d,dim=2)
          sd3d3  = SIZE(d3d,dim=3)
          if(present(a1d) .AND. &
             present(b1d) .AND. &
             present(c1d)        ) then
               sa1d = SIZE(a1d)
               sb1d = SIZE(b1d)
               sc1d = SIZE(c1d)
               if(sp8wd1 /= sa1d .OR. &
                  sp8wd2 /= sb1d .OR. &
                  sp8wd3 /= sc1d      ) then
                     call check_dim3Di64_eq(sp8wd1,sp8wd2,sp8wd3,  &
                                            sa1d,sb1d,sc1d,msg_ar,file_path,8493)
               end if
               !DIR$ SIMD VECTORLENGTHFOR(KIND=8))
                  do i = this%m_ims, sp8wd1
                      this%m_p3d(i,:,:) = a1d(i)
                  end do
               !DIR$ SIMD VECTORLENGTHFOR(KIND=8))
                  do k = this%m_kms, sp8wd2
                      this%m_p3d(:,k,:) = b1d(k)
                  end do
               !DIR$ SIMD VECTORLENGTHFOR(KIND=8))
                  do j = this%m_jms, sp8wd3
                      this%m_p3d(:,:,j) = c1d(j)
                  end do
          else
                  if(sp8wd1 /= sd3d1 .OR. &
                     sp8wd2 /= sd3d2 .OR. &
                     sp8wd3 /= sd3d3      ) then
                        call check_dim3Di64_eq(sp8wd1,sp8wd2,sp8wd3, &
                                               sd3d1,sd3d2,sd3d3,msg_ar,file_path,8512)
                  end if
                  do j = this%m_jms, sp8wd3
                      do k = this%m_kms, sp8wd2
                          !DIR$ SIMD VECTORLENGTHFOR(KIND=8))
                          do i = this%m_ims, sp8wd1
                              this%m_p8wd3(i,k,j) = d3d(i,k,j)
                          end do
                      end do
                  end do
          end if
          
    end  subroutine
    
    
    subroutine  set_maskp3d_from_subarray(this,a1d,b1d,c1d,d3d)
          implicit none
          class(CAtmPressureField3D),           intent(inout) :: this
          logical(I64), optional, dimension(:), intent(in)    :: a1d,b1d,c1d
          !DIR$ ASSUME_ALIGNED a1d:32, b1d:32, c1d:32
          logical(I64),       dimension(:,:,:), intent(in)    :: d3d
          ! Locals
          integer(I64)                                        :: sm3d1,sm3d2,sm3d3, &
                                                                 sa1d,sb1d,sc1d,    &
                                                                 sd3d1,sd3d2,sd3d3
          integer(I64)                                        :: i,j,k
          character(len=80), dimension(10) :: msg_ar = [ '[FATAL-ERROR] in: CAtmPressureField3D%set_maskp3d_from_subarray', &
                                                                      '***** ERROR-DETAILS ***** ' ,  &
                                                                'Invalid Range of  Input Arguments!!' , &
                                                                'idx        = ', &
                                                                'kdx        = ', &
                                                                'jdx        = ', &
                                                                'this%m_ime = ', &
                                                                'this%m_kme = ', &
                                                                'this%m_jme = ', &
                                                                '***** ERROR-DETAILS *****'         ]
          ! Start of executable statements
          sm3d1 = SIZE(this%m_maskp3d,dim=1)
          sm3d2 = SIZE(this%m_maskp3d,dim=2)
          sm3d3 = SIZE(this%m_maskp3d,dim=3)
          sd3d1 = SIZE(d3d,dim=1)
          sd3d2 = SIZE(d3d,dim=2)
          sd3d3 = SIZE(d3d,dim=3)
          if(present(a1d) .AND. &
             present(b1d) .AND. &
             present(c1d)       ) then
               sa1d = SIZE(a1d)
               sb1d = SIZE(b1d)
               sc1d = SIZE(c1d)
               if(sm3d1 /= sa1d .OR. &
                  sm3d2 /= sb1d .OR. &
                  sm3d3 /= sc1d      ) then
                     call check_dim3Di64_eq(sm3d1,sm3d2,sm3d3, &
                                            sa1d,sb1d,sc1d,msg_ar,file_path,8564)
               end if
                !DIR$ SIMD VECTORLENGTHFOR(KIND=8))
                 do i = this%m_ims, sm3d1
                     this%m_maskp3d(i,:,:) = a1d(i)
                 end do
                !DIR$ SIMD VECTORLENGTHFOR(KIND=8))
                 do k = this%m_kms, sm3d2
                     this%m_maskp3d(:,k,:) = b1d(k)
                 end do
                !DIR$ SIMD VECTORLENGTHFOR(KIND=8))
                 do j = this%m_jms, sm3d3
                     this%m_maskp3d(:,:,j) = c1d(j)
                 end do
          else
                 if(sm3d1 /= sd3d1 .OR. &
                    sm3d2 /= sd3d2 .OR. &
                    sm3d3 /= sd3d3      ) then
                      call check_dim3Di64_eq(sm3d1,sm3d2,sm3d3,  &
                                             sd3d1,sd3d2,sd3d3,msg_ar,file_path,8583)
                 end if
                 do j = this%m_jms, sm3d3
                     do k = this%m_kms, sm3d2
                         !DIR$ SIMD VECTORLENGTHFOR(KIND=8))
                         do i = this%m_ims, sm3d1
                             this%m_maskp3d(i,k,j) = d3d(i,k,j)
                         end do
                     end do
                 end do
          end if
                 
    end  subroutine
   
    
    subroutine  read_press_field3D(this,format,unit,error)
          implicit none
          class(CAtmPressureField3D),  intent(in)    :: this
          character(len=*),            intent(in)    :: format
          integer(I32),                intent(in)    :: unit
          integer(I32),                intent(inout) :: error
          ! Start of executable statements
          select case(adjustl(trim(format)))
          case ("*")
              write(unit,*,iostat=error) this
          case default
              write(unit,adjustl(trim(format)),iostat=error) this
          end select
          
    end  subroutine
    
    
    subroutine  write_press_field3D(this,format,unit,error)
          implicit none
          class(CAtmPressureField3D),   intent(in)    :: this
          character(len=*),             intent(in)    :: format
          integer(I32),                 intent(in)    :: unit
          integer(I32),                 intent(inout) :: error
          ! Start of executable statements
          select case(adjustl(trim(format)))
          case ("*")
              read(unit,*,iostat=error) this
          case default
              read(unit,adjustl(trim(format)),iostat=error) this
          end select
          
    end  subroutine
    
    
    subroutine  print_state_field3D(this,tf)
          use ISO_FORTRAN_ENV, only : OUTPUT_UNIT
          implicit none
          class(CAtmPressureField3D),   intent(in) :: this
          character(len=*),             intent(in) :: tf
          ! Locals
          integer(I32), dimension(8)               :: dt
          character(len=12), dimension(3)          :: rc
          integer(I64)                             :: i,j,k
          character(len=80), dimension(6) :: msgs = [ '[FATAL-ERROR] in: CAtmPressureField3D%print_state_field3D', &
                                                                '       ***** ERROR-DETAILS ***** ' ,  &
                                                                '----------------------------------------' , &
                                                                '   Status of allocation:  ', &
                                                                '----------------------------------------' ,  &
                                                                '       ***** ERROR-DETAILS *****  '     ]
                                                                
                                                                        
          ! Start of executable statements
          call check_alloc3DR64_fail(this%m_p3d,msgs,file_path,8678)
          call check_alloc3DR64_fail(this%m_p8w3d,msgs,file_path,8679)
          call check_alloc3DL64_fail(this%m_maskp3d,msgs,file_path,8680)
          call DATE_AND_TIME(rc(1),rc(2),rc(3),dt)
          write(OUTPUT_UNIT,*) " Dump of CAtmPressureField3D - started."
          
    end  subroutine
    
    
    subroutine  alloc3d_error_handler(err,emsg,file,fname,line)
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          implicit none
          
          integer(I32),         intent(in) :: err
          character(len=*),     intent(in) :: emsg
          character(len=*),     intent(in) :: file
          character(len=*),     intent(in) :: fname
          integer(I32),         intent(in) :: line
          ! Start of executable statements
          
          write(ERROR_UNIT,*) '   ****[FATAL-ERROR]****     '
          write(ERROR_UNIT,*) '-----------------------------'
          write(ERROR_UNIT,*) ' Memory Allocation/Deallocation Failure  '
          write(ERROR_UNIT,*) ' STAT=    ', err
          write(ERROR_UNIT,*) ' ERRMSG:  ', emsg
          write(ERROR_UNIT,*) ' In File: ', file
          write(ERROR_UNIT,*) ' In proc: ', fname
          write(ERROR_UNIT,*) ' At line: ', line
          write(ERROR_UNIT,*) '-----------------------------'
          ERROR STOP ' [FATAL] - Terminating Execution - Failed to Allocate/Deallocate Memory!!'
          
    end  subroutine
    
    
    subroutine  nonconforming_arrays3d(this,other,details,file,funame,line)
          use ISO_FORTRAN_ENV,only : ERROR_UNIT,OUTPUT_UNIT
          implicit none
          class(CAtmPressureField3D),  intent(in) :: this,other
          logical(I32),                intent(in) :: details
          character(len=*),            intent(in) :: file
          character(len=*),            intent(in) :: funame
          integer(I32),                intent(in) :: line
          ! Locals
          integer(I64)                            :: ap3d1,ap3d2,ap3d3, &
                                                     bp3d1,bp3d2,bp3d3, &
                                                     ap8d1,ap8d2,ap8d3, &
                                                     bp8d1,bp8d2,bp8d3, &
                                                     am3d1,am3d2,am3d3, &
                                                     bm3d1,bm3d2,bm3d3
          ! Start of execuatble statements
          ap3d1=SIZE(this%m_p3d,dim=1);      ap3d2=SIZE(this%m_p3d,dim=2)
          ap3d3=SIZE(this%m_p3d,dim=3)
          bp3d1=SIZE(other%m_p3d,dim=1);     bp3d2=SIZE(other%m_p3d,dim=2)
          bp3d3=SIZE(other%m_p3d,dim=3)
          ap8d1=SIZE(this%m_p8w3d,dim=1);    ap8d2=SIZE(other%m_p8w3d,dim=2)
          ap8d3=SIZE(this%m_p8w3d,dim=3)
          bp8d1=SIZE(other%m_p8w3d,dim=1);   bp8d2=SIZE(other%m_p8w3d,dim=2)
          bp8d3=SIZE(other%m_p8w3d,dim=3)
          am3d1=SIZE(this%m_maskp3d,dim=1);  am3d2=SIZE(this%m_maskp3d,dim=2)
          am3d3=SIZE(this%m_maskp3d,dim=3)
          bm3d1=SIZE(other%m_maskp3d,dim=1); bm3d2=SIZE(other%m_maskp3d,dim=2)
          bm3d3=SIZE(other%m_maskp3d,dim=3)
          if(ap3d1 /= bp3d1 .OR. &
             ap3d2 /= bp3d2 .OR. &
             ap3d3 /= bp3d3 .OR. &
             ap8d1 /= bp8d1 .OR. &
             ap8d2 /= bp8d2 .OR. &
             ap8d3 /= bp8d3 .OR. &
             am3d1 /= bm3d1 .OR. &
             am3d2 /= bm3d2 .OR. &
             am3d3 /= bm3d3     )  then
             
             if(details .EQ. .true.) then
                  write(ERROR_UNIT,*) '[FATAL-ERROR] --> Nonconforming Arrays!!'
                  write(ERROR_UNIT,*) ' Gathering Error Details.'
                  write(ERROR_UNIT,10) line
10                format('At line: ', I24.20)
                  write(ERROR_UNIT,*) 'In file: ', file
                  write(ERROR_UNIT,*) 'In function:', funame
                  write(ERROR_UNIT,*) '--------------------------------------'
                  write(ERROR_UNIT,*) '  Computing dimension conformance.'
                  write(ERROR_UNIT,*) '--------------------------------------'
                  write(ERROR_UNIT,20) ap3d1-bp3d1
20                format('m_p3d dim=1 conformance:',I24.20)
                  write(ERROR_UNIT,30) ap3d2-bp3d2
30                format('m_p3d dim=2 conformance:',I24.20)
                  write(ERROR_UNIT,40) ap3d3-bp3d3
40                format('m_p3d dim=3 conformance:',I24.20)
                  write(ERROR_UNIT,50) ap8d1-bp8d1
50                format('m_p8w3d dim=1 conformance:',I24.20)
                  write(ERROR_UNIT,60) ap8d2-bp8d2
60                format('m_p8w3d dim=2 conformance:',I24.20)
                  write(ERROR_UNIT,70) ap8d3-bp8d3
70                format('m_p8w3d dim=3 conformance:',I24.20)
                  write(ERROR_UNIT,80) am3d1-bm3d1
80                format('m_maskp3d dim=1 conformance:',I24.20)
                  write(ERROR_UNIT,90) am3d2-bm3d2
90                format('m_maskp3d dim=2 conformance:',I24.20)
                  write(ERROR_UNIT,100) am3d3-bm3d3
100               format('m_maskp3d dim=3 conformance:',I24.20)
                  write(ERROR_UNIT,*) ' Terminating Execution with STOP!!'
            else
                  write(ERROR_UNIT,*) ' [FATAL-ERROR]: --> Nonconforming Arrays!!'
                  write(ERROR_UNIT,*) ' User decided to skip verbose details.'
                  write(ERROR_UNIT,*) ' Terminating Execution with STOP!!'
            end if 
                  ERROR STOP '[FATAL]: Non-Conforming Arrays Detected in: ' //  funame
         else
             write(OUTPUT_UNIT,*) ' Conforming Arrays Detected.'
         end if
    
                  
    end  subroutine
    
    
    subroutine  omp_copy_arrays3d(p3din,p8w3din,maskp3din, &
                                  p3dout,p8w3dout,maskp3dout  )
          implicit none
          real(R64),    contiguous,dimension(:,:,:), intent(in)    :: p3din, &
                                                                      p8w3din
          logical(I64), contiguous,dimension(:,:,:), intent(in)    :: maskp3din
          !DIR$ ASSUME_ALIGNED p3din:32,p8w3din:32,maskp3din:32
          real(R64),    contiguous,dimension(:,:,:), intent(inout) :: p3dout, &
                                                                      p8w3dout
          logical(I64), contiguous,dimension(:,:,:), intent(inout) :: maskp3dout
          ! Start of executable statements
            !$OMP PARALLEL
                !$OMP WORKSHARE
                    p3dout     = p3din
                    p8w3dout   = p8w3din
                    maskp3dout = maskp3din
                !$OMP END WORKSHARE
            !$OMP END PARALLEL
    
    end  subroutine 
    
end  module