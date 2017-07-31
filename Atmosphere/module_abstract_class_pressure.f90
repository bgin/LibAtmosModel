
module  module_abstract_class_pressure

  !-----------------------------------------------------------------------------------85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'module_abstract_class_pressure'
 !          
 !          Purpose:
 !                          Abstract Data Type which reperesent a concept  of
 !                          an atmospheric pressure which is measured in unit of Pascal (Pa).
 !                          'IAtmosPressure_t' - abstract class will contains deffered type
 !                          bound procedure and generic operators which later will be overriden
 !                          by subtype implementation.
 !                          Three subtypes will implemented.
 !                          1) Pressure scalar field 1D.
 !                          2) Pressure scalar field 2D.
 !                          3) Pressure scalar field 3D.
 !          History:
 !                      Date: 11-01-2017
 !                      Time: 19:06 GMT+2
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
 !                            Adapted from module_ra_goddard.f originally
 !                            part of WRF - physics folder.
 !----------------------------------------------------------------------------------85   


 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

 !====================================
 ! USE directives.
 !====================================
 
 use module_kinds
 
 
 implicit none
 
 private
 public :: IAtmPressureField
 
 character*(*) file_path
 parameter (file_path = 'C:\Users\Bernard\Documents\Visual Studio 2013\Projects\WRF_Goddard_Modification\WRF_Goddard_Modification\module_abstract_class_pressure.f90')

 type, abstract :: IAtmPressureField
        !========== DESCRIPTION ==========
        ! Abstract class IAtmosPressure.
        !========== DESCRIPTION ==========
        contains
        !================================================58
        ! Deffered type bound procedures and operators.
        !================================================58
        
        procedure(print_object_state),    pass(this),  deferred, public :: print_state                       ! Exhaustive printing of object state.
 
        procedure(print_memory_layout),   pass(this), deferred,  public :: print_memory                     ! Exhaustive printing of object memory layout.
 
        procedure(object_type_info),      pass(this), deferred,  public :: object_type                         ! Object type info.
 
        procedure(field_dimension_info),  pass(this), deferred,  public :: field_dimension                          ! Member field dimension.
        
        procedure(field_shape_info),      pass(this), deferred, public  :: field_shape                              ! Member array shape.
        
        
        
        procedure(is_object_constructed),    pass(this), deferred,  public   :: is_constructed                           ! Returns member array initilized by specific CTOR call.
        
        procedure(num_field_elems),          pass(this), deferred,  public   :: field_elems                          ! Number of elemnets in scalar field(total).
        
        procedure(num_dirx_elems),           pass(this), deferred,  public   :: dirx_elems                           ! Number of elements along x-field component.
        
        procedure(num_diry_elems),           pass(this), deferred,  public   :: diry_elems                           ! Number of elements along y-field component.
        
        procedure(num_dirz_elems),           pass(this), deferred,  public   :: dirz_elems                           ! Number of elements along z-field component.
        
        procedure(max_field_pressure),       pass(this), deferred,  public   :: max_pressure                             ! Maximum pressure in whole field.
        
        procedure(max_field_loc_pressure),   pass(this), deferred,  public   :: max_loc_pressure                         ! Index(location) of max field pressure.
        
        procedure(min_field_pressure),       pass(this), deferred,  public   :: min_pressure                             ! Minimum pressure in whole array.
        
        procedure(min_field_loc_pressure),   pass(this), deferred,  public   :: min_loc_pressure                         ! Index(location) of min field pressure.
 
        !================================================58
        ! Deferred type bound operators.
        !================================================58
        procedure(abstract_copy_assign),       pass(this), deferred :: copy_assign    ! Operation: Copy-assignment
        
        procedure(abstract_move_assign),       pass(this), deferred :: move_assign    ! Operation: Move-assignment
        
        procedure(abstract_field_add_field),   pass(this), deferred :: add            ! Operation: Fields addition
        
        procedure(abstract_field_sub_field),   pass(this), deferred :: subtract       ! Operation: Fields subtraction
        
        procedure(abstract_field_mul_field),   pass(this), deferred :: multiply       ! Operation: Fields multiplication
        
        procedure(abstract_field_mul_real),    pass(this), deferred :: mul_real       ! Operation: Field-real multiplication
        
        procedure(abstract_real_mul_field),    pass(this), deferred :: real_mul       ! Operation: Real-field multiplication.
        
        procedure(abstract_field_mul_int),     pass(this), deferred :: mul_int        ! Operation: Field-integer multiplication.
        
        procedure(abstract_int_mul_field),     pass(this), deferred :: int_mul        ! Operation: Integer-field multiplication.
        
        procedure(abstract_field_div_field),   pass(this), deferred :: div            ! Operation: Fields division.
        
        procedure(abstract_field_div_real),    pass(this), deferred :: div_real       ! Operation: Field-real division.
        
        procedure(abstract_field_div_int),     pass(this), deferred :: div_int        ! Operation: Field-integer division.
        
        procedure(abstract_field_pow_real),    pass(this), deferred :: pow_real       ! Operation: Field to real power.
        
        procedure(abstract_field_pow_int),     pass(this), deferred :: pow_int        ! Operation: Field to integer power.
        
        procedure(abstract_field_eq_field),    pass(this), deferred :: equal          ! Operation: Field-Field equality
        
        procedure(abstract_field_neq_field),   pass(this), deferred :: not_equal      ! Operation: Field-Field inequality.
        
        procedure(abstract_field_lt_field),    pass(this), deferred :: less_than      ! Operation: Filed-Field less-than.
        
        procedure(abstract_field_gt_field),    pass(this), deferred :: great_than     ! Operation: Field-Field greater-than.
        
        procedure(abstract_field_lte_field),   pass(this), deferred :: less_eq_than   ! Operation: Field-Field less-or-equal-than.
        
        procedure(abstract_field_gte_field),   pass(this), deferred :: great_eq_than  ! Operation: Field-Field great-orequal than.
        
        ! Operators-to-procedure names mapping.
        
        generic :: assignment(=) => copy_assign, move_assign
        
        generic :: operator(+)   => add
        
        generic :: operator(-)   => subtract
        
        generic :: operator(*)   => multiply, mul_real, real_mul, mul_int, int_mul
        
        generic :: operator(/)   => div, div_real, div_int
        
        generic :: operator(**)  => pow_real, pow_int
        
        generic :: operator(==)  => equal
        
        generic :: operator(/=)  => not_equal
        
        generic :: operator(<)  => less_than
        
        generic :: operator(>)  => great_than
        
        generic :: operator(.LE.) => less_eq_than
        
        generic :: operator(.GE.) => great_eq_than
        
        
end  type
        
        
        
        ! Operator procedures abstract interfaces.
        
abstract  interface


        ! abstract  subroutine print_object_state
        subroutine  print_object_state(this)
        
        import :: IAtmPressureField
        implicit none
               class(IAtmPressureField), intent(in) :: this
        
        end  subroutine
        
        ! abstract subroutine: print_memory_layout
        subroutine  print_memory_layout(this)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                
        end  subroutine
        
        ! abstract subroutine: object_type_info
        subroutine  object_type_info(this)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
        
        end  subroutine
        
        ! pure abstract function: field_dimension_info
        pure  function field_dimension_info(this)  result(dim_info)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                ! Local variables
                integer(i32) :: dim_info
                
        end  function
        
        ! pure abstract function: field_shape_info
        pure  function field_shape_info(this)  result(shape_info)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                ! Local variables
                integer(i32) :: shape_info
                
        end function
        
        ! pure abstract function: is_object_constructed
        pure  function is_object_constructed(this)  result(constructed)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                ! Local variables
                logical(i32) :: constructed
                
        end function
        
        ! pure abstract function: num_field_elems
        pure  function num_field_elems(this)  result(n_elems)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                ! Local variables
                integer(i64) :: n_elems
                
        end  function
        
        ! pure abstract function: num_dirx_elems
        pure  function num_dirx_elems(this)  result(n_xelems)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                ! Local variables
                integer(i64) :: n_xelems
                
        end  function
        
        ! pure abstract function: num_diry_elems
        pure  function num_diry_elems(this)  result(n_yelems)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                ! Local variables
                integer(i64) :: n_yelems
                
        end  function
        
        ! pure  abstract function: num_dirz_elems
        pure  function num_dirz_elems(this)  result(n_zelems)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                ! Local variables
                integer(i64) :: n_zelems
                
        end  function
        
        ! pure abstract function: max_field_pressure
        pure  function max_field_pressure(this)  result(max_press)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                ! Local variables
                real(R64) :: max_press
                
        end  function
        
        ! pure abstract function: max_field_loc_pressure
        pure  function max_field_loc_pressure(this)  result(mloc_press)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                ! Local variables
                integer(i64) :: mloc_press
                
        end  function
        
        ! pure abstract function: min_field_pressure
        pure  function min_field_pressure(this)  result(min_press)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                ! Local variables
                real(R64) :: min_press
                
        end  function
        
        ! pure abstract function: min_field_loc_pressure
        pure  function min_field_loc_pressure(this)  result(mloc_press)
        
         import :: IAtmPressureField
         implicit none
                class(IAtmPressureField), intent(in) :: this
                ! Local variables
                integer(i64) :: mloc_press
                
        end  function

        ! Overloading of assignment procedures
        
        ! abstract_copy_assign
        pure  subroutine  abstract_copy_assign(lhs,rhs)
        
        import :: IAtmPressureField
              class(IAtmPressureField), intent(inout) :: lhs
              class(IAtmPressureField), intent(in)    :: rhs
        end  subroutine
        
        ! abstract_move_assign
        pure  subroutine  abstract_move_assign(lhs,rhs)
        
         import :: IAtmPressureField
               class(IAtmPressureField), intent(inout) :: lhs
               class(IAtmPressureField), intent(in)    :: rhs
        end  subroutine
        
        ! abstract procedure: field-addition
        pure  function  abstract_field_add_field(lhs,rhs)  result(new_field)
        
        import :: IAtmPressureField
             class(IAtmPressureField), intent(in)  :: lhs
             class(IAtmPressureField), intent(in)  :: rhs
             class(IAtmPressureField), allocatable :: new_field
        end function
        
        ! abstract  procedure: field-subtraction
        pure  function  abstract_field_sub_field(lhs,rhs)  result(new_field)
        
         import :: IAtmPressureField
               class(IAtmPressureField), intent(in)  :: lhs
               class(IAtmPressureField), intent(in)  :: rhs
               class(IAtmPressureField), allocatable :: new_field
        end function
        
        ! abstract procedure: field-field multiplication.
        pure  function  abstract_field_mul_field(lhs,rhs)  result(new_field)
        
          import :: IAtmPressureField
                class(IAtmPressureField), intent(in)  :: lhs
                class(IAtmPressureField), intent(in)  :: rhs
                class(IAtmPressureField), allocatable :: new_field
        end function
        
        ! abstract procedure: filed-scalar multiplication , scalar is real
        pure  function  abstract_field_mul_real(lhs,rhs)   result(new_field)
        
            import :: IAtmPressureField, r64
                  class(IAtmPressureField), intent(in)  :: lhs
                  real(r64),                intent(in)  :: rhs
                  class(IAtmPressureField), allocatable :: new_field
        end  function
        
        ! abstract procedure: scalar-field multiplication , scalar is real
        pure  function  abstract_real_mul_field(lhs,rhs)   result(new_field)
        
             import :: IAtmPressureField, r64
                   real(r64),                      intent(in)  :: lhs
                   class(IAtmPressureField),       intent(in)  :: rhs
                   class(IAtmPressureField), allocatable       :: new_field
        end  function
        
        ! abstract procedure: field-scalar multiplicatin, scalar is integer
        pure  function  abstract_field_mul_int(lhs,rhs)   result(new_field)
        
              import :: IAtmPressureField, i64
                    class(IAtmPressureField), intent(in)  :: lhs
                    integer(i64),             intent(in)  :: rhs
                    class(IAtmPressureField), allocatable :: new_field
        end function
        
        ! abstract procedure: scalar-field multiplication, scalar is integer
        pure  function  abstract_int_mul_field(lhs,rhs)   result(new_field)
        
              import :: IAtmPressureField, i64
                    integer(i64),             intent(in)  :: lhs
                    class(IAtmPressureField), intent(in)  :: rhs
                    class(IAtmPressureField), allocatable :: new_field
        end  function
        
        ! abstract procedure: field-field division.
        pure  function  abstract_field_div_field(lhs,rhs)  result(new_field)
        
               import :: IAtmPressureField
                     class(IAtmPressureField), intent(in)  :: lhs
                     class(IAtmPressureField), intent(in)  :: rhs
                     class(IAtmPressureField), allocatable :: new_field
        end  function
        
        ! abstract procedure: field-scalar division, scalar is real
        pure  function  abstract_field_div_real(lhs,rhs)   result(new_field)
        
               import :: IAtmPressureField, r64
                     class(IAtmPressureField), intent(in)  :: lhs
                     real(r64),                intent(in)  :: rhs
                     class(IAtmPressureField), allocatable :: new_field
        end function
        
        ! abstract procedure: field-scalar division, scalar is integer.
        pure  function  abstract_field_div_int(lhs,rhs)   result(new_field)
        
               import :: IAtmPressureField, i64
                     class(IAtmPressureField), intent(in)  :: lhs
                     integer(i64),             intent(in)  :: rhs
                     class(IAtmPressureField), allocatable :: new_field
        end  function
        
        ! abstract  procedure: field to power(real).
        pure  function  abstract_field_pow_real(lhs,rhs)  result(new_field)
        
               import :: IAtmPressureField, r64
                     class(IAtmPressureField), intent(in)  :: lhs
                     real(r64),                intent(in)  :: rhs
                     class(IAtmPressureField), allocatable :: new_field
        end  function
        
        ! abstract procedure: field to power(integer).
        pure  function  abstract_field_pow_int(lhs,rhs)  result(new_field)
        
               import :: IAtmPressureField, i64
                     class(IAtmPressureField), intent(in)  :: lhs
                     integer(i64),             intent(in)  :: rhs
                     class(IAtmPressureField), allocatable :: new_field
        end function
        
        ! abstract procedure: field-field equality.
        pure  function abstract_field_eq_field(lhs,rhs)  result(eq_res)
        
               import :: IAtmPressureField
                     class(IAtmPressureField), intent(in)   :: lhs
                     class(IAtmPressureField), intent(in)   :: rhs
                     logical, allocatable, dimension(:,:,:) :: eq_res
        end  function
        
        ! abstract procedure: field-field inequality.
        pure  function  abstract_field_neq_field(lhs,rhs) result(neq_res)
        
                import :: IAtmPressureField
                       class(IAtmPressureField), intent(in)   :: lhs
                       class(IAtmPressureField), intent(in)   :: rhs
                       logical, allocatable, dimension(:,:,:) :: neq_res
        end  function
        
        ! abstract field-lt-field ,less-than
        pure  function  abstract_field_lt_field(lhs,rhs)  result(lt_res)
        
                import :: IAtmPressureField
                       class(IAtmPressureField), intent(in)   :: lhs
                       class(IAtmPressureField), intent(in)   :: rhs
                       logical, allocatable, dimension(:,:,:) :: lt_res
        end  function
        
        ! abstract  field-gt-field , greater-than
        pure  function  abstract_field_gt_field(lhs,rhs)  result(gt_res)
        
                import :: IAtmPressureField
                       class(IAtmPressureField), intent(in)   :: lhs
                       class(IAtmPressureField), intent(in)   :: rhs
                       logical, allocatable, dimension(:,:,:) :: gt_res
        end  function
        
        ! abstract field-lte-field , lte 
        pure  function  abstract_field_lte_field(lhs,rhs)  result(lte_res)
        
                 import :: IAtmPressureField
                        class(IAtmPressureField), intent(in)   :: lhs
                        class(IAtmPressureField), intent(in)   :: rhs
                        logical, allocatable, dimension(:,:,:) :: lte_res
        end  function
        
        ! abstract field-gte-field, gte
        pure  function  abstract_field_gte_field(lhs,rhs)  result(gte_res)
        
                 import :: IAtmPressureField
                        class(IAtmPressureField), intent(in)   :: lhs
                        class(IAtmPressureField), intent(in)   :: rhs
                        logical, allocatable, dimension(:,:,:) :: gte_res
        end  function
        
end  interface     


end  module