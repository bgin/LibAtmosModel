
module  module_class_error_check


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'module_class_error_check'
 !          
 !          Purpose:
 !                          Encapsulation of error checking type which contains
 !                          various error condition checking subroutines.
 !                          Upon discovering a fault ,error or exception from
 !                          correct code excution path specific subroutine will
 !                          be called and will act as an error handler.
 !          
 !          History:
 !                      Date: 03-01-2016
 !                      Time: 18:41 GMT+2
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
 !          
 !
 !                       
 !==================================================================================85   


 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

       use module_kinds, only: I32P , I64P, R32P, R64P, &
                               ZERO_I32P, ZERO_I64P
       
        implicit none
        
       ! Module versioning information

       ! File version major
        
       integer(I32P), public, parameter :: module_class_error_check_major = 1
       
       ! File version minor
        
       integer(I32P), public, parameter :: module_class_error_check_minor = 0
       
       ! File version micro
       
       integer(I32P), public, parameter :: module_class_error_check_micro = 0
       
       ! File full version, computed as follows: 1000*version_major + 100*version_minor + 10*version_micro
       
       integer(I32P), public, parameter :: module_class_error_check_version = 1000*module_class_error_check_major+100* &
                                                        module_class_error_check_minor+10*module_class_error_check_micro
       
       ! File creation date
       
       character(*), public, parameter :: CreationDate = "01-03-2017 18:41 +00200 (Wed, 1 March 2017 GMT+2)"
       
       ! File build date/time , should be set manually after every recompilation
       
       character(*), public, parameter :: BuildDate = ""
       
       ! Module name
       
       character(*), public, parameter :: ModuleName = "module_class_error_check"
       
       ! Module Author
       
       character(*), public, parameter :: ModuleAuthor = "Programmer: Bernard Gingold , contact: beniekg@gmail.com"
       
       ! Module Description
       
       character(*), public, parameter :: ModuleDescription = "Array Conformance/Allocation checking procedures."
       
       
       !=========================================50
       ! Derived types declarations
       !=========================================50
       
       
        type, public ::  ArrayConformVerifier
            
        contains
        
           !=====================================50
           ! Checking arrays conformance, by 
           ! their specific dimensions comparison.
           !=====================================50
        
          
            
          
            
           !=====================================50
           ! Conformance of integral arrays.
           !=====================================50
            generic,  public :: conform_dim1D_Int => is_conforming_dim1DI32, is_conforming_dim1DI64
            
            generic,  public :: conform_dim2D_Int => is_conforming_dim2DI32, is_conforming_dim2DI64
            
            generic,  public :: conform_dim3D_Int => is_conforming_dim3DI32, is_conforming_dim3DI64
            
            generic,  public :: conform_dim4D_Int => is_conforming_dim4DI32, is_conforming_dim4DI64
            
           !=====================================50
           ! Conformance of real arrays.
           !=====================================50
            generic,  public :: conform_dim1D_Real => is_conforming_dim1DR32, is_conforming_dim1DR64
            
            generic,  public :: conform_dim2D_Real => is_conforming_dim2DR32, is_conforming_dim2DR64
            
            generic,  public :: conform_dim3D_Real => is_conforming_dim3DR32, is_conforming_dim3DR64
            
            generic,  public :: conform_dim4D_Real => is_conforming_dim4DR32, is_conforming_dim4DR64
            
           !=====================================50
           ! Conformance of logical arrays.
           !=====================================50
            generic,  public :: conform_dim1D_Log  => is_conforming_dim1DL32, is_conforming_dim1DL64
            
            generic,  public :: conform_dim2D_Log  => is_conforming_dim2DL32, is_conforming_dim2DL64
            
            generic,  public :: conform_dim3D_Log  => is_conforming_dim3DL32, is_conforming_dim3DL64
            
            generic,  public :: conform_dim4D_Log  => is_conforming_dim4DL32, is_conforming_dim4DL64
            
           !=====================================50
           ! Generic procedures for checking if
           ! specific array size at dimensions
           ! dim=1,2,3,4 is not equal to 0.
           !=====================================50
            
           !=====================================50
           ! Arrays of dimension:  1.
           !=====================================50
            generic,  public :: array1D_size_zero  =>  is_len_array1DI32_eq_zero, is_len_array1DI64_eq_zero, &
                                                       is_len_array1DR32_eq_zero, is_len_array1DR64_eq_zero, &
                                                       is_len_array1DL32_eq_zero, is_len_array1DL64_eq_zero
            
           !=====================================50
           ! Arrays of dimension: 2.
           !=====================================50
            generic,  public :: array2D_size_zero  =>  is_len_array2DI32_eq_zero, is_len_array2DI64_eq_zero, &
                                                       is_len_array2DR32_eq_zero, is_len_array2DR64_eq_zero, &
                                                       is_len_array2DL32_eq_zero, is_len_array2DL64_eq_zero
            
           !=====================================50
           ! Arrays of dimension: 3.
           !=====================================50
            generic,  public :: array3D_size_zero  =>  is_len_array3DI32_eq_zero, is_len_array3DI64_eq_zero, &
                                                       is_len_array3DR32_eq_zero, is_len_array3DR64_eq_zero, &
                                                       is_len_array3DL32_eq_zero, is_len_array3DL64_eq_zero  
            
           !=====================================50
           ! Arrays of dimension: 4.
           !=====================================50
            generic,  public :: array4D_size_zero  =>  is_len_array4DI32_eq_zero, is_len_array4DI64_eq_zero, &
                                                       is_len_array4DR32_eq_zero, is_len_array4DR64_eq_zero, &
                                                       is_len_array4DL32_eq_zero, is_len_array4DL64_eq_zero 
            
            procedure, public, nopass :: is_conforming_dim1DI32
            
            procedure, public, nopass :: is_conforming_dim1DI64
            
            procedure, public, nopass :: is_confroming_dim2DI32
            
            procedure, public, nopass :: is_conforming_dim2DI64
            
            procedure, public, nopass :: is_conforming_dim3DI32
            
            procedure, public, nopass :: is_conforming_dim3DI64
            
            procedure, public, nopass :: is_conforming_dim4DI32
            
            procedure, public, nopass :: is_conforming_dim4DI64
            
            procedure, public, nopass :: is_conforming_dim1DR32
            
            procedure, public, nopass :: is_conforming_dim1DR64
            
            procedure, public, nopass :: is_conforming_dim2DR32
            
            procedure, public, nopass :: is_conforming_dim2DR64
            
            procedure, public, nopass :: is_conforming_dim3DR32
            
            procedure, public, nopass :: is_conforming_dim3DR64
            
            procedure, public, nopass :: is_conforming_dim4DR32
            
            procedure, public, nopass :: is_conforming_dim4DR64
            
            procedure, public, nopass :: is_conforming_dim1DL32
            
            procedure, public, nopass :: is_conforming_dim1DL64
            
            procedure, public, nopass :: is_conforming_dim2DL32
            
            procedure, public, nopass :: is_conforming_dim2DL64
            
            procedure, public, nopass :: is_conforming_dim3DL32
            
            procedure, public, nopass :: is_conforming_dim3DL64
            
            procedure, public, nopass :: is_conforming_dim4DL32
            
            procedure, public, nopass :: is_conforming_dim4DL64
            
            procedure, public, nopass :: is_len_array1DI32_eq_zero
                                         
            procedure, public, nopass :: is_len_array1DI64_eq_zero
            
            procedure, public, nopass :: is_len_array1DR32_eq_zero
            
            procedure, public, nopass :: is_len_array1DR64_eq_zero
            
            procedure, public, nopass :: is_len_array1DL32_eq_zero
            
            procedure, public, nopass :: is_len_array1DL64_eq_zero                            
           
            procedure, public, nopass :: is_len_array2DI32_eq_zero
            
            procedure, public, nopass :: is_len_array2DI64_eq_zero
            
            procedure, public, nopass :: is_len_array2DR32_eq_zero
            
            procedure, public, nopass :: is_len_array2DR64_eq_zero
            
            procedure, public, nopass :: is_len_array2DL32_eq_zero
            
            procedure, public, nopass :: is_len_array3DI32_eq_zero
            
            procedure, public, nopass :: is_len_array3DI64_eq_zero
            
            procedure, public, nopass :: is_len_array3DR32_eq_zero
            
            procedure, public, nopass :: is_len_array3DR64_eq_zero
            
            procedure, public, nopass :: is_len_array3DL32_eq_zero
            
            procedure, public, nopass :: is_len_array3DL64_eq_zero
            
            procedure, public, nopass :: is_len_array4DI32_eq_zero
            
            procedure, public, nopass :: is_len_array4DI64_eq_zero
            
            procedure, public, nopass :: is_len_array4DR32_eq_zero
            
            procedure, public, nopass :: is_len_array4DR64_eq_zero
            
            procedure, public, nopass :: is_len_array4DL32_eq_zero
            
            procedure, public, nopass :: is_len_array4DL64_eq_zero
            
        end type   
            
           
            
            
        
        
      
        
        type, public :: ArrayAllocVerifier
            
        contains
        
            !====================================50
            ! Declaration of generic procedures for
            ! checking allocation status of arrays 1D
            !====================================50
        
            generic, public :: array1D_not_alloc =>  is_not_alloc_array1DI32, is_not_alloc_array1DI64, &
                                                     is_not_alloc_array1DR32, is_not_alloc_array1DR64, &
                                                     is_not_alloc_array1DL32, is_not_alloc_array1DL64
            
            
            !====================================50
            ! Declaration of generic procedures for
            ! checking allocation status of arrays 2D
            !====================================50
            generic, public :: array2D_not_alloc =>  is_not_alloc_array2DI32, is_not_alloc_array2DI64, &
                                                     is_not_alloc_array2DR32, is_not_alloc_array2DR64, &
                                                     is_not_alloc_array2DL32, is_not_alloc_array2DL64
            
            
            !====================================50
            ! Declaration of generic procedures for
            ! checking allocation status of arrays 3D
            !====================================50
            generic, public :: array3D_not_alloc =>  is_not_alloc_array3DI32, is_not_alloc_array3DI64, &
                                                     is_not_alloc_array3DR32, is_not_alloc_array3DR64, &
                                                     is_not_alloc_array3DL32, is_not_alloc_array3DL64
            
            
            !====================================50
            ! Declaration of generic procedures for
            ! checking allocation status of arrays 4D
            !====================================50
            generic, public :: array4D_not_alloc =>  is_not_alloc_array4DI32, is_not_alloc_array4DI64, &
                                                     is_not_alloc_array4DR32, is_not_alloc_array4DR64, &
                                                     is_not_alloc_array4DL32, is_not_alloc_array4DL64
            
            
            
            procedure, public, nopass :: is_not_alloc_array1DI32
            
            procedure, public, nopass :: is_not_alloc_array1DI64
            
            procedure, public, nopass :: is_not_alloc_array1DR32
            
            procedure, public, nopass :: is_not_alloc_array1DR64
            
            procedure, public, nopass :: is_not_alloc_array1DL32
            
            procedure, public, nopass :: is_not_alloc_array1DL64
            
            procedure, public, nopass :: is_not_alloc_array2DI32
            
            procedure, public, nopass :: is_not_alloc_array2DI64
            
            procedure, public, nopass :: is_not_alloc_array2DR32
            
            procedure, public, nopass :: is_not_alloc_array2DR64
            
            procedure, public, nopass :: is_not_alloc_array2DL32
            
            procedure, public, nopass :: is_not_alloc_array2DL64
            
            procedure, public, nopass :: is_not_alloc_array3DI32
            
            procedure, public, nopass :: is_not_alloc_array3DI64
            
            procedure, public, nopass :: is_not_alloc_array3DR32
            
            procedure, public, nopass :: is_not_alloc_array3DR64
            
            procedure, public, nopass :: is_not_alloc_array3DL32
            
            procedure, public, nopass :: is_not_alloc_array3DL64
            
            procedure, public, nopass :: is_not_alloc_array4DI32
            
            procedure, public, nopass :: is_not_alloc_array4DI64
            
            procedure, public, nopass :: is_not_alloc_array4DR32
            
            procedure, public, nopass :: is_not_alloc_array4DR64
            
            procedure, public, nopass :: is_not_alloc_array4DL32
            
            procedure, public, nopass :: is_not_alloc_array4DL64
            
        end type   
            
          
          
           
            
           
            
            
          
            
            
        
        
        !========================================50
        ! Declaration of type-bound generic 
        ! interfaces. Type: ArrayConformVerifier
        !========================================50
        
        interface conform_dim1D_Int
             procedure is_conforming_dim1DI32
             procedure is_conforming_dim1DI64
        end  interface
        
        interface conform_dim2D_Int
             procedure is_conforming_dim2DI32
             procedure is_conforming_dim2DI64
        end  interface
        
        interface conform_dim3D_Int
             procedure is_conforming_dim3DI32
             procedure is_conforming_dim3DI64
        end  interface
        
        interface conform_dim4D_Int
             procedure is_conforming_dim4DI32
             procedure is_conforming_dim4DI64
        end  interface
        
        interface conform_dim1D_Real
             procedure is_conforming_dim1DR32
             procedure is_conforming_dim1DR64
        end  interface
        
        interface conform_dim2D_Real
             procedure is_conforming_dim2DR32
             procedure is_conforming_dim2DR64
        end interface
        
        interface conform_dim3D_Real
             procedure is_conforming_dim3DR32
             procedure is_conforming_dim3DR64
        end  interface
        
        interface conform_dim4D_Real
             procedure is_conforming_dim4DR32
             procedure is_conforming_dim4DR64
        end  interface
        
        interface conform_dim1D_Log
             procedure is_conforming_dim1DL32
             procedure is_conforming_dim1DL64
        end  interface
        
        interface conform_dim2D_Log
             procedure is_conforming_dim2DL32
             procedure is_conforming_dim2DL64
        end interface
        
        interface conform_dim3D_Log
             procedure is_conforming_dim3DL32
             procedure is_conforming_dim3DL64
        end  interface
        
        interface conform_dim4D_Log
             procedure is_conforming_dim4DL32
             procedure is_conforming_dim4DL64
        end  interface
        
        
        interface array1D_size_zero
             procedure is_len_array1DI32_eq_zero
             procedure is_len_array1DI64_eq_zero
             procedure is_len_array1DR32_eq_zero
             procedure is_len_array1DR64_eq_zero
             procedure is_len_array1DL32_eq_zero
             procedure is_len_array1DL64_eq_zero
        end interface
        
        interface array2D_size_zero
            procedure is_len_array2DI32_eq_zero
            procedure is_len_array2DI64_eq_zero
            procedure is_len_array2DR32_eq_zero
            procedure is_len_array2DR64_eq_zero
            procedure is_len_array2DL32_eq_zero
            procedure is_len_array2DL64_eq_zero
        end interface
        
        interface array3D_size_zero
            procedure is_len_array3DI32_eq_zero
            procedure is_len_array3DI64_eq_zero
            procedure is_len_array3DR32_eq_zero
            procedure is_len_array3DR64_eq_zero
            procedure is_len_array3DL32_eq_zero
            procedure is_len_array3DL64_eq_zero
        end interface
        
        interface array4D_size_zero
            procedure is_len_array4DI32_eq_zero
            procedure is_len_array4DI64_eq_zero
            procedure is_len_array4DR32_eq_zero
            procedure is_len_array4DR64_eq_zero
            procedure is_len_array4DL32_eq_zero
            procedure is_len_array4DL64_eq_zero
        
        end interface
        
        !========================================50
        ! Declaration of generic interfaces type-
        ! bound procedures.
        ! Type: ArrayAllocVerifier
        !========================================50
        
        interface array1D_not_alloc
            procedure is_not_alloc_array1DI32
            procedure is_not_alloc_array1DI64
            procedure is_not_alloc_array1DR32
            procedure is_not_alloc_array1DR64
            procedure is_not_alloc_array1DL32
            procedure is_not_alloc_array1DL64
        end interface
        
        interface array2D_not_alloc
            procedure is_not_alloc_array2DI32
            procedure is_not_alloc_array2DI64
            procedure is_not_alloc_array2DR32
            procedure is_not_alloc_array2DR64
            procedure is_not_alloc_array2DL32
            procedure is_not_alloc_array2DL64
        end interface
        
        interface array3D_not_alloc
            procedure is_not_alloc_array3DI32
            procedure is_not_alloc_array3DI64
            procedure is_not_alloc_array3DR32
            procedure is_not_alloc_array3DR64
            procedure is_not_alloc_array3DL32
            procedure is_not_alloc_array3DL64
        end interface
        
        interface array4D_not_alloc
            procedure is_not_alloc_array4DI32
            procedure is_not_alloc_array4DI64
            procedure is_not_alloc_array4DR32
            procedure is_not_alloc_array4DR64
            procedure is_not_alloc_array4DL32
            procedure is_not_alloc_array4DL64
        end interface
        
    contains
    
        
    
    !============================================50
    ! Implementation of generic overloaded 
    ! interfaces.
    ! Type: ArrayConformVerifier
    !============================================50
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type integer(kind=4).
    ! @Params: integer(I32), dimension(:) :: a1d,b1d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim1DI32(a1d,b1d)  result(b_res)
          implicit none
          integer(I32P), dimension(:), intent(in) :: a1d,b1d
          ! Locals
          logical(I32P)                           :: b_res
          ! Start of executable statements
          if(SIZE(a1d) /= SIZE(b1d)) then
             b_res = .true.
          else
             b_res = .false.
          end if
    end  function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type integer(kind=8).
    ! @Params: integer(I64P), dimension(:) :: a1d,b1d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim1DI64(a1d,b1d)  result(b_res)
          implicit none
          integer(I64P), dimension(:), intent(in) :: a1d,b1d
          ! Locals
          logical(I32P)                           :: b_res
          ! Start of executable statements
          if(SIZE(a1d) /= SIZE(b1d)) then
             b_res = .true.
          else
             b_res = .false.
          end if
    
    end  function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type integer(kind=4).
    ! @Params: integer(I32P), dimension(:,:) :: a1d,b1d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim2DI32(a2d,b2d)  result(b_res)
          implicit none
          integer(I32P), dimension(:,:), intent(in) :: a2d,b2d
          ! Locals
          logical(I32P)                             :: b_res
          ! Start of executable statements
          if((SIZE(a1d,dim=1) /= SIZE(b1d,dim=1)) .OR. &
             (SIZE(a1d,dim=2) /= SIZE(b2d,dim=2))     )  then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type integer(kind=8).
    ! @Params: integer(I64P), dimension(:,:) :: a1d,b1d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim2DI64(a2d,b2d)  result(b_res)
          implicit none
          integer(I64P), dimension(:,:), intent(in) :: a2d,b2d
          ! Locals
          logical(I32P)                             :: b_res
          ! Start of executable statements
          if((SIZE(a2d,dim=1) /= SIZE(b2d,dim=1)) .OR. &
             (SIZE(a2d,dim=2) /= SIZE(b2d,dim=2))      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type integer(kind=4).
    ! @Params: integer(I32P), dimension(:,:,:) :: a1d,b1d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim3DI32(a3d,b3d)  result(b_res)
          implicit none
          integer(I32P), dimension(:,:,:), intent(in) :: a3d,b3d
          ! Locals
          logical(I32P)                               :: b_res
          ! Start of executable statements
          if((SIZE(a3d,dim=1) /= SIZE(b3d,dim=1)) .OR. &
             (SIZE(a3d,dim=2) /= SIZE(b3d,dim=2)) .OR. &
             (SIZE(a3d,dim=3) /= SIZE(b3d,dim=3))      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type integer(kind=8).
    ! @Params: integer(I64P), dimension(:,:,:) :: a3d,b3d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim3DI64(a3d,b3d)  result(b_res)
          implicit none
          integer(I64P), dimension(:,:,:), intent(in) :: a3d,b3d
          ! Locals
          logical(I32P)                               :: b_res
          ! Start of executable statements
          if((SIZE(a3d,dim=1) /= SIZE(b3d,dim=1)) .OR. &
             (SIZE(a3d,dim=2) /= SIZE(b3d,dim=2)) .OR. &
             (SIZE(a3d,dim=3) /= SIZE(b3d,dim=3))      ) then
              b_res = .true.
          else
              b_res = .false.
          end if    
    end function 
   
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type integer(kind=4).
    ! @Params: integer(I32P), dimension(:,:,:,:) :: a4d,b4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim4DI32(a4d,b4d)  result(b_res)
          implicit none
          integer(I32P), dimension(:,:,:,:), intent(in) :: a4d,b4d
          ! Locals
          logical(I32P)                                 :: b_res
          ! Start of executable statements
          if((SIZE(a4d,dim=1) /= SIZE(b4d,dim=1)) .OR. &
             (SIZE(a4d,dim=2) /= SIZE(b4d,dim=2)) .OR. &
             (SIZE(a4d,dim=3) /= SIZE(b4d,dim=3)) .OR. &
             (SIZE(a4d,dim=4) /= SIZE(b4d,dim=4))      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type integer(kind=8).
    ! @Params: integer(I64P), dimension(:,:,:,:) :: a4d,b4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim4DI64(a4d,b4d)  result(b_res)
          implicit none
          integer(I64P), dimension(:,:,:,:), intent(in) :: a4d,b4d
          ! Locals
          logical(I32P)                                 :: b_res
          ! Start of executable statements
          if((SIZE(a4d,dim=1) /= SIZE(b4d,dim=1)) .OR. &
             (SIZE(a4d,dim=2) /= SIZE(b4d,dim=2)) .OR. &
             (SIZE(a4d,dim=3) /= SIZE(b4d,dim=3)) .OR. &
             (SIZE(a4d,dim=4) /= SIZE(b4d,dim=4))      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type real(kind=4).
    ! @Params: real(R32P), dimension(:) :: a1d,b1d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim1DR32(a1d,b1d)  result(b_res)
          implicit none
          real(R32P), dimension(:), intent(in) :: a1d,b1d
          ! Locals
          logical(I32P)                        :: b_res
          ! Start of executable statements
          if(SIZE(a1d) /= SIZE(b1d)) then
             b_res = .true.
          else
             b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type real(kind=8).
    ! @Params: real(R64P), dimension(:) :: a1d,b1d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim1DR64(a1d,b1d)  result(b_res)
          implicit none
          real(R64P), dimension(:), intent(in) :: a1d,b1d
          ! Locals
          logical(I32P)                        :: b_res
          ! Start of executable statements
          if(SIZE(a1d) /= SIZE(b1d)) then
             b_res = .true.
          else
             b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type real(kind=4).
    ! @Params: real(R32P), dimension(:,:) :: a2d,b2d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim2DR32(a2d,b2d)   result(b_res)
          implicit none
          real(R32P), dimension(:,:), intent(in) :: a2d,b2d
          ! Locals
          logical(I32P)                          :: b_res
          ! Start of executable statements
          if((SIZE(a2d,dim=1) /= SIZE(b2d,dim=1)) .OR. &
             (SIZE(a2d,dim=2) /= SIZE(b2d,dim=2))      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type real(kind=8).
    ! @Params: real(R64P), dimension(:,:) :: a2d,b2d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim2DR64(a2d,b2d)   result(b_res)
          implicit none
          real(R64P), dimension(:,:), intent(in) :: a2d,b2d
          ! Locals
          logical(I32P)                          :: b_res
          ! Start of executable stataments
           if((SIZE(a2d,dim=1) /= SIZE(b2d,dim=1)) .OR. &
              (SIZE(a2d,dim=2) /= SIZE(b2d,dim=2))      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type real(kind=4).
    ! @Params: real(R32P), dimension(:,:,:) :: a3d,b3d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim3DR32(a3d,b3d)   result(b_res)
          implicit none
          real(R32P), dimension(:,:,:), intent(in) :: a3d,b3d
          ! Locals
          logical(I32P)                            :: b_res
          ! Start of executable statements
          if((SIZE(a3d,dim=1) /= SIZE(b3d,dim=1)) .OR. &
             (SIZE(a3d,dim=2) /= SIZE(b3d,dim=2)) .OR. &
             (SIZE(a3d,dim=3) /= SIZE(b3d,dim=3))     )  then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type real(kind=8).
    ! @Params: real(R64P), dimension(:,:,:) :: a3d,b3d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim3DR64(a3d,b3d)   result(b_res)
          implicit none
          real(R64P), dimension(:,:,:), intent(in) :: a3d,b3d
          ! Locals
          logical(I32P)                            :: b_res
          ! Start of executable statements
          if((SIZE(a3d,dim=1) /= SIZE(b3d,dim=1)) .OR. &
             (SIZE(a3d,dim=2) /= SIZE(b3d,dim=2)) .OR. &
             (SIZE(a3d,dim=3) /= SIZE(b3d,dim=3))     )  then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function 
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type real(kind=4).
    ! @Params: real(R32P), dimension(:,:,:,:) :: a4d,b4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim4DR32(a4d,b4d)   result(b_res)
          implicit none
          real(R64P), dimension(:,:,:,:), intent(in) :: a4d,b4d
          ! Locals
          logical(I32P)                              :: b_res
          ! Start of executable statements
          if((SIZE(a4d,dim=1) /= SIZE(b4d,dim=1)) .OR. &
             (SIZE(a4d,dim=2) /= SIZE(b4d,dim=2)) .OR. &
             (SIZE(a4d,dim=3) /= SIZE(b4d,dim=3)) .OR. &
             (SIZE(a4d,dim=4) /= SIZE(b4d,dim=4))     )  then
              b_res = .true.
          else
              b_res = .false
          end if
    end  function
   
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type real(kind=8).
    ! @Params: real(R64P), dimension(:,:,:,:) :: a4d,b4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim4DR32(a4d,b4d)   result(b_res)
          implicit none
          real(R64P), dimension(:,:,:,:), intent(in) :: a4d,b4d
          ! Locals
          logical(I32P)                              :: b_res
          ! Start of executable statements
          if((SIZE(a4d,dim=1) /= SIZE(b4d,dim=1)) .OR. &
             (SIZE(a4d,dim=2) /= SIZE(b4d,dim=2)) .OR. &
             (SIZE(a4d,dim=3) /= SIZE(b4d,dim=3)) .OR. &
             (SIZE(a4d,dim=4) /= SIZE(b4d,dim=4))     )  then
              b_res = .true.
          else
              b_res = .false
          end if
    end function 
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type logical(kind=4).
    ! @Params: logical(I32P), dimension(:), intent(in) :: a1d,b1d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim1DL32(a1d,b1d)   result(b_res)
          implicit none
          logical(I32P), dimension(:), intent(in) :: a1d,b1d
          ! Locals
          logical(I32P)                           :: b_res
          ! Start of executable statements
          if(SIZE(a1d) /= SIZE(b1d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
   
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type logical(kind=8).
    ! @Params: logical(I64P), dimension(:), intent(in) :: a1d,b1d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim1DL64(a1d,b1d)   result(b_res)
          implicit none
          logical(I64P), dimension(:), intent(in) :: a1d,b1d
          ! Locals
          logical(I32P)                           :: b_res
          ! Start of executable statements
          if(SIZE(a1d) /= SIZE(b1d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type logical(kind=4).
    ! @Params: logical(I32P), dimension(:,:), intent(in) :: a2d,b2d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim2DL32(a2d,b2d)   result(b_res)
          implicit none
          logical(I32P), dimension(:,:), intent(in) :: a2d,b2d
          ! Locals
          logical(I32P)                             :: b_res
          ! Start of executable statements
          if((SIZE(a2d,dim=1) /= SIZE(b2d,dim=1)) .OR. &
             (SIZE(a2d,dim=2) /= SIZE(b2d,dim=2))      ) then 
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type logical(kind=8).
    ! @Params: logical(I64P), dimension(:,:), intent(in) :: a2d,b2d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim2DL64(a2d,b2d)   result(b_res)
          implicit none
          logical(I64P), dimension(:,:), intent(in) :: a2d,b2d
          ! Locals
          logical(I32P)                             :: b_res
          ! Start of executable statements.
          if((SIZE(a2d,dim=1) /= SIZE(b2d,dim=1)) .OR. &
             (SIZE(a2d,dim=2) /= SIZE(b2d,dim=2))      ) then 
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type logical(kind=4).
    ! @Params: logical(I32P), dimension(:,:,:), intent(in) :: a3d,b3d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim3DL32(a3d,b3d)   result(b_res)
          implicit none
          logical(I32P), dimension(:,:,:), intent(in) :: a3d,b3d
          ! Locals
          logical(I32P)                               :: b_res
          ! Start of executable statements
          if((SIZE(a3d,dim=1) /= SIZE(b3d,dim=1)) .OR. &
             (SIZE(a3d,dim=2) /= SIZE(b3d,dim=2)) .OR. &
             (SIZE(a3d,dim=3) /= SIZE(b3d,dim=3))      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type logical(kind=8).
    ! @Params: logical(I64P), dimension(:,:,:), intent(in) :: a3d,b3d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim3DL64(a3d,b3d)   result(b_res)
          implicit none
          logical(I64P), dimension(:,:,:), intent(in) :: a3d,b3d
          ! Locals
          logical(I32P)                               :: b_res
          ! Start of executable statements
          if((SIZE(a3d,dim=1) /= SIZE(b3d,dim=1)) .OR. &
             (SIZE(a3d,dim=2) /= SIZE(b3d,dim=2)) .OR. &
             (SIZE(a3d,dim=3) /= SIZE(b3d,dim=3))      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type logical(kind=4).
    ! @Params: logical(I32P), dimension(:,:,:,:), intent(in) :: a4d,b4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim4DL32(a4d,b4d)   result(b_res)
          implicit none
          logical(I32P), dimension(:,:,:,:), intent(in) :: a4d,b4d
          ! Locals
          logical(I32P)                                 :: b_res
          ! Start of executable statements
          if((SIZE(a4d,dim=1) /= SIZE(b4d,dim=1)) .OR. &
             (SIZE(a4d,dim=2) /= SIZE(b4d,dim=2)) .OR. &
             (SIZE(a4d,dim=3) /= SIZE(b4d,dim=3)) .OR. &
             (SIZE(a4d,dim=4) /= SIZE(b4d,dim=4))      )  then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conforming comparison of two
    !           arrays of type logical(kind=8).
    ! @Params: logical(I64P), dimension(:,:,:,:), intent(in) :: a4d,b4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff two args are not 
    !              conforming, otherwise .FALSE.
    !============================================50
    pure function is_conforming_dim4DL64(a4d,b4d)   result(b_res)
          implicit none
          logical(I64P), dimension(:,:,:,:), intent(in) :: a4d,b4d
          ! Locals
          logical(I32P)                                 :: b_res
          ! Start of executable statements
           if((SIZE(a4d,dim=1) /= SIZE(b4d,dim=1)) .OR. &
             (SIZE(a4d,dim=2) /= SIZE(b4d,dim=2)) .OR. &
             (SIZE(a4d,dim=3) /= SIZE(b4d,dim=3)) .OR. &
             (SIZE(a4d,dim=4) /= SIZE(b4d,dim=4))      )  then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function 
   
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) integral
    !           array of type integer(kind=4) to ZERO.
    !           Length of array in its 1st dimension is
    !           checked against zero length.
    ! @Params: integer(I32P), dimension(:), intent(in) :: a1d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff 1st dimension is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array1DI32_eq_zero(a1d)   result(b_res)
          implicit none
          integer(I32P), dimension(:), intent(in) :: a1d
          ! Locals
          logical(I32P)                           :: b_res
          ! Start of executable statements
          if(SIZE(a1d) .EQ. ZERO_I64P) then
             b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) integral
    !           array of type integer(kind=8) to ZERO.
    !           Length of array in its 1st dimension is
    !           checked against zero length.
    ! @Params: integer(I64P), dimension(:), intent(in) :: a1d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff 1st dimension is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array1DI64_eq_zero(a1d)   result(b_res)
          implicit none
          integer(I64P), dimension(:), intent(in) :: a1d
          ! Locals
          logical(I32P)                           :: b_res
          ! Start of executable statements
          if(SIZE(a1d) .EQ. ZERO_I64P) then
             b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) real
    !           array of type real(kind=4) to ZERO.
    !           Length of array in its 1st dimension is
    !           checked against zero length.
    ! @Params: real(R32P), dimension(:), intent(in) :: a1d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff 1st dimension is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array1DR32_eq_zero(a1d)   result(b_res)
          implicit none
          real(R32P), dimension(:), intent(in) :: a1d
          ! Locals
          logical(I32P)                        :: b_res
          ! Start of executable statements
          if(SIZE(a1d) .EQ. ZERO_I64P) then
             b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) real
    !           array of type real(kind=8) to ZERO.
    !           Length of array in its 1st dimension is
    !           checked against zero length.
    ! @Params: real(R64P), dimension(:), intent(in) :: a1d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff 1st dimension is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array1DR64_eq_zero(a1d)   result(b_res)
          implicit none
          real(R64P), dimension(:), intent(in) :: a1d
          ! Locals
          logical(I32P)                        :: b_res
          ! Start of executable statements
          if(SIZE(a1d) .EQ. ZERO_I64P) then
             b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) logical
    !           array of type logical(kind=4) to ZERO.
    !           Length of array in its 1st dimension is
    !           checked against zero length.
    ! @Params: logical(I32P), dimension(:), intent(in) :: a1d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff 1st dimension is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array1DL32_eq_zero(a1d)   result(b_res)
          implicit none
          logical(I32P), dimension(:), intent(in) :: a1d
          ! Local
          logical(I32P)                           :: b_res
          ! Start of executable statements
          if(SIZE(a1d) .EQ. ZERO_I64P) then
             b_res = .true.
          else
             b_res = .false.
          end if
    end function 
   
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) logical
    !           array of type logical(kind=8) to ZERO.
    !           Length of array in its 1st dimension is
    !           checked against zero length.
    ! @Params: logical(I64P), dimension(:), intent(in) :: a1d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff 1st dimension is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array1DL64_eq_zero(a1d)   result(b_res)
          implicit none
          logical(I64P), dimension(:), intent(in) :: a1d
          ! Locals
          logical(I32P)                           :: b_res
          ! Start of executable statements
          if(SIZE(a1d) .EQ. ZERO_I64P) then
             b_res = .true.
          else
             b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) integral
    !           array of type integer(kind=4) to ZERO.
    !           Length of array in its 1st,2nd dimensions is
    !           checked against zero length.
    ! @Params: integer(I32P), dimension(:,:), intent(in) :: a2d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff 1st dimension is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array2DI32_eq_zero(a2d)   result(b_res)
          implicit none
          integer(I32P), dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                           :: b_res
          ! Start of executable statements
          if((SIZE(a2d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a2d,dim=2) .EQ. ZERO_I64P)      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) integral
    !           array of type integer(kind=8) to ZERO.
    !           Length of array in its 1st,2nd dimensions is
    !           checked against zero length.
    ! @Params: integer(I64P), dimension(:,:), intent(in) :: a2d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff 1st dimension is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array2DI64_eq_zero(a2d)   result(b_res)
          implicit none
          integer(I64P), dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                             :: b_res
          ! Start of executable statements
          if((SIZE(a2d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a2d,dim=2) .EQ. ZERO_I64P)      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) real
    !           array of type real(kind=4) to ZERO.
    !           Length of array in its 1st,2nd dimensions is
    !           checked against zero length.
    ! @Params: real(R32P), dimension(:,:), intent(in) :: a2d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff 1st dimension is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array2DR32_eq_zero(a2d)   result(b_res)
          implicit none
          real(R32P), dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                          :: b_res
          ! Start of executable statements
          if((SIZE(a2d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a2d,dim=2) .EQ. ZERO_I64P)      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) real
    !           array of type real(kind=8) to ZERO.
    !           Length of array in its 1st,2nd dimensions is
    !           checked against zero length.
    ! @Params: real(R64P), dimension(:,:), intent(in) :: a2d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff 1st dimension is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array2DR64_eq_zero(a2d)   result(b_res)
          implicit none
          real(R64P), dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                          :: b_res
          ! Start of executable statements
          if((SIZE(a2d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a2d,dim=2) .EQ. ZERO_I64P)      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) logical
    !           array of type logical(kind=4) to ZERO.
    !           Length of array in its 1st,2nd dimensions is
    !           checked against zero length.
    ! @Params: logical(I32P), dimension(:,:), intent(in) :: a2d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 2 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array2DL32_eq_zero(a2d)   result(b_res)
          implicit none
          logical(I32P), dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                             :: b_res
          ! Start of executable statements
          if((SIZE(a2d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a2d,dim=2) .EQ. ZERO_I64P)      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) logical
    !           array of type logical(kind=8) to ZERO.
    !           Length of array in its 1st,2nd dimensions is
    !           checked against zero length.
    ! @Params: logical(I64P), dimension(:,:), intent(in) :: a2d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 2 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array2DL64_eq_zero(a2d)   result(b_res)
          implicit none
          logical(I64P), dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                             :: b_res
          ! Start of executable statements
          if((SIZE(a2d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a2d,dim=2) .EQ. ZERO_I64P)      ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) integral
    !           array of type integer(kind=4) to ZERO.
    !           Length of array in its 1st,2nd,3rd dimensions is
    !           checked against zero length.
    ! @Params: integer(I32P), dimension(:,:,:), intent(in) :: a3d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 3 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array3DI32_eq_zero(a3d)   result(b_res)
          implicit none
          integer(I32P), dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                               :: b_res
          ! Start of executable statements
          if((SIZE(a3d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=3) .EQ. ZERO_I64P)     ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) integral
    !           array of type integer(kind=8) to ZERO.
    !           Length of array in its 1st,2nd,3rd dimensions is
    !           checked against zero length.
    ! @Params: integer(I64P), dimension(:,:,:), intent(in) :: a3d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 3 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array3DI64_eq_zero(a3d)   result(b_res)
          implicit none
          integer(I64P), dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                               :: b_res
          ! Start of executable statements
          if((SIZE(a3d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=3) .EQ. ZERO_I64P)     ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) real
    !           array of type real(kind=4) to ZERO.
    !           Length of array in its 1st,2nd,3rd dimensions is
    !           checked against zero length.
    ! @Params: real(R32P), dimension(:,:,:), intent(in) :: a3d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 3 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array3DR32_eq_zero(a3d)   result(b_res)
          implicit none
          real(R32P), dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                            :: b_res
          ! Start of executable statements
          if((SIZE(a3d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=3) .EQ. ZERO_I64P)     ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
     
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) real
    !           array of type real(kind=8) to ZERO.
    !           Length of array in its 1st,2nd,3rd dimensions is
    !           checked against zero length.
    ! @Params: real(R32P), dimension(:,:,:), intent(in) :: a3d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 3 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array3DR64_eq_zero(a3d)   result(b_res)
          implicit none
          real(R64P), dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                            :: b_res
          ! Start of executable statements
          if((SIZE(a3d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=3) .EQ. ZERO_I64P)     ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) logical
    !           array of type logical(kind=4) to ZERO.
    !           Length of array in its 1st,2nd,3rd dimensions is
    !           checked against zero length.
    ! @Params: logical(I32P), dimension(:,:,:), intent(in) :: a3d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 3 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array3DL32_eq_zero(a3d)   result(b_res)
          implicit none
          logical(I32P), dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                               :: b_res
          ! Start of execuatble statements
          if((SIZE(a3d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=3) .EQ. ZERO_I64P)     ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) logical
    !           array of type logical(kind=8) to ZERO.
    !           Length of array in its 1st,2nd,3rd dimensions is
    !           checked against zero length.
    ! @Params: logical(I64P), dimension(:,:,:), intent(in) :: a3d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 3 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array3DL64_eq_zero(a3d)   result(b_res)
          implicit none
          logical(I64P), dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                               :: b_res
          ! Start of executable statements
          if((SIZE(a3d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a3d,dim=3) .EQ. ZERO_I64P)     ) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) integral
    !           array of type integer(kind=4) to ZERO.
    !           Length of array in its 1st,2nd,3rd,4th dimensions is
    !           checked against zero length.
    ! @Params: integer(I32P), dimension(:,:,:,:), intent(in) :: a4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 4 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array4DI32_eq_zero(a4d)   result(b_res)
          implicit none
          integer(I32P), dimension(:,:,:,:), intent(in) :: a4d
          ! Locals
          logical(I32P)                                 :: b_res
          ! Start of executable statements
          if((SIZE(a4d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=3) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=4) .EQ. ZERO_I64P)     )   then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) integral
    !           array of type integer(kind=8) to ZERO.
    !           Length of array in its 1st,2nd,3rd,4th dimensions is
    !           checked against zero length.
    ! @Params: integer(I64P), dimension(:,:,:,:), intent(in) :: a4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 4 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array4DI64_eq_zero(a4d)   result(b_res)
          implicit none
          integer(I64P), dimension(:,:,:,:), intent(in) :: a4d
          ! Locals
          logical(I32P)                                 :: b_res
          ! Start of executable statements
          if((SIZE(a4d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=3) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=4) .EQ. ZERO_I64P)     )   then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
     
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) real
    !           array of type real(kind=4) to ZERO.
    !           Length of array in its 1st,2nd,3rd,4th dimensions is
    !           checked against zero length.
    ! @Params: real(R32P), dimension(:,:,:,:), intent(in) :: a4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 4 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array4DR32_eq_zero(a4d)   result(b_res)
          implicit none
          real(R32P), dimension(:,:,:,:), intent(in) :: a4d
          ! Locals
          logical(I32P)                              :: b_res
          ! Start of executable statements
          if((SIZE(a4d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=3) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=4) .EQ. ZERO_I64P)     )   then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) real
    !           array of type real(kind=8) to ZERO.
    !           Length of array in its 1st,2nd,3rd,4th dimensions is
    !           checked against zero length.
    ! @Params: real(R64P), dimension(:,:,:,:), intent(in) :: a4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 4 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array4R64_eq_zero(a4d)   result(b_res)
          implicit none
          real(R64P), dimension(:,:,:,:), intent(in) :: a4d
          ! Locals
          logical(I32P)                              :: b_res
          ! Start of executable statements
          if((SIZE(a4d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=3) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=4) .EQ. ZERO_I64P)     )   then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) logical
    !           array of type logical(kind=4) to ZERO.
    !           Length of array in its 1st,2nd,3rd,4th dimensions is
    !           checked against zero length.
    ! @Params: logical(I32P), dimension(:,:,:,:), intent(in) :: a4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 4 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array4DL32_eq_zero(a4d)   result(b_res)
          implicit none
          logical(I32P), dimension(:,:,:,:), intent(in) :: a4d
          ! Local
          logical(I32P)                                 :: b_res
          ! Start of executable statements
          if((SIZE(a4d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=3) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=4) .EQ. ZERO_I64P)     )   then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Comparison of length of input (in) logical
    !           array of type logical(kind=8) to ZERO.
    !           Length of array in its 1st,2nd,3rd,4th dimensions is
    !           checked against zero length.
    ! @Params: logical(I64P), dimension(:,:,:,:), intent(in) :: a4d
    ! @Throws: Nothing          
    ! @Purity: True
    ! @Returnes: .TRUE. iff any of 4 dimensions is equal
    !              ZERO, otherwise .FALSE.
    !============================================50
    pure function is_len_array4DL64_eq_zero(a4d)   result(b_res)
          implicit none
          logical(I64P), dimension(:,:,:,:), intent(in) :: a4d
          ! Locals
          logical(I32P)                                 :: b_res
          ! Start of executable statements
          if((SIZE(a4d,dim=1) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=2) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=3) .EQ. ZERO_I64P) .OR. &
             (SIZE(a4d,dim=4) .EQ. ZERO_I64P)     )   then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! Implementation of generic interface procedures
    ! type-bound to ArrayAllocVerifier
    !============================================50
      
                        
    !============================================50
    ! @Purpose:
    !           Conducting check if integral 
    !           array 1D is successfully allocated
    !           
    ! @Params: integer(I32P), dimension(:) :: a1d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50                   
    pure function is_not_alloc_array1DI32(a1d)   result(b_res)        
          implicit none
          integer(I32P), allocatable, dimension(:), intent(in) :: a1d
          ! Locals
          logical(I32P)                                        :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a1d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
     
    
    !============================================50
    ! @Purpose:
    !           Conducting check if integral 
    !           array 1D is successfully allocated
    !           
    ! @Params: integer(I64P), dimension(:) :: a1d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50      
    pure function is_not_alloc_array1DI64(a1d)   result(b_res)
          implicit none
          integer(I64P), allocatable, dimension(:), intent(in) :: a1d
          ! Locals
          logical(I32P)                                        :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a1d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
     
    
    !============================================50
    ! @Purpose:
    !           Conducting check if real
    !           array 1D is successfully allocated
    !           
    ! @Params: real(R32P), dimension(:) :: a1d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50 
    pure function is_not_alloc_array1DR32(a1d)   result(b_res)
          implicit none
          real(R32P), allocatable, dimension(:), intent(in) :: a1d
          ! Locals
          logical(I32P)                                     :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a1d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function                                  
       
    
    !============================================50
    ! @Purpose:
    !           Conducting check if real
    !           array 1D is successfully allocated
    !           
    ! @Params: real(R64P), dimension(:) :: a1d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is not allocated, otherwise .FALSE.
    !             
    !               
    !============================================50
    pure function is_not_alloc_array1DR64(a1d)   result(b_res)
          implicit none                              
          real(R64P), allocatable, dimension(:), intent(in) :: a1d
          ! Locals
          logical(I32P)                                     :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a1d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function         
     
    
    !============================================50
    ! @Purpose:
    !           Conducting check if logical
    !           array 1D is successfully allocated
    !           
    ! @Params: logical(I32P), dimension(:) :: a1d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50    
    pure function is_not_alloc_array1DL32(a1d)   result(b_res)         
          implicit none
          logical(I32P), allocatable, dimension(:), intent(in) :: a1d
          ! Locals
          logical(I32P)                                        :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a1d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function     
     
    
    !============================================50
    ! @Purpose:
    !           Conducting check if logical
    !           array 1D is successfully allocated
    !           
    ! @Params: logical(I64P), dimension(:) :: a1d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50       
    pure function is_not_alloc_array1DL64(a1d)   result(b_res)    
          implicit none   
          logical(I64P), allocatable, dimension(:), intent(in) :: a1d
          ! Locals
          logical(I32P)                                        :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a1d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function        
        
    
    !============================================50
    ! @Purpose:
    !           Conducting check if integral 
    !           array 2D is successfully allocated
    !           
    ! @Params: integer(I32P), dimension(:,:) :: a2d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50       
    pure function is_not_alloc_array2DI32(a2d)   result(b_res)    
          implicit none
          integer(I32P), allocatable, dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                                          :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a2d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function   
              
     
    !============================================50
    ! @Purpose:
    !           Conducting check if integral 
    !           array 2D is successfully allocated
    !           
    ! @Params: integer(I64P), dimension(:,:) :: a2d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50           
    pure function is_not_alloc_array2DI64(a2d)   result(b_res)
          implicit none
          integer(I64P), allocatable, dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                                          :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a2d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Conducting check if real 
    !           array 2D is successfully allocated
    !           
    ! @Params: real(R32P), dimension(:,:) :: a2d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50  
    pure function is_not_alloc_array2DR32(a2d)   result(b_res)
          implicit none
          real(R32P), allocatable, dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                                       :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a2d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Conducting check if real 
    !           array 2D is successfully allocated
    !           
    ! @Params: real(R64P), dimension(:,:) :: a2d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50 
    pure function is_not_alloc_array2DR64(and)   result(b_res)
          implicit none
          real(R64P), allocatable, dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                                       :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a2d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Conducting check if logical 
    !           array 2D is successfully allocated
    !           
    ! @Params: logical(I32P), dimension(:,:) :: a2d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50 
    pure function is_not_alloc_array2DL32(a2d)    result(b_res)
          implicit none
          logical(I32P), allocatable, dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                                          :: b_res
          ! Start of execuatble statements
          if(.NOT. allocated(a2d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Conducting check if logical 
    !           array 2D is successfully allocated
    !           
    ! @Params: logical(I64P), dimension(:,:) :: a2d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50 
    pure function is_not_alloc_array2DL64(a2d)   result(b_res)
          implicit none
          logical(I64P), allocatable, dimension(:,:), intent(in) :: a2d
          ! Locals
          logical(I32P)                                          :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a2d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conducting check if integral 
    !           array 3D is successfully allocated
    !           
    ! @Params: integer(I32P), dimension(:,:,:) :: a3d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50   
    pure function is_not_alloc_array3DI32(a3d)    result(b_res)
          implicit none
          integer(I32P), allocatable, dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                                            :: b_res
          ! Start of execuatble statements
          if(.NOT. allocated(a3d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Conducting check if integral 
    !           array 3D is successfully allocated
    !           
    ! @Params: integer(I64P), dimension(:,:,:) :: a3d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50 
    pure function is_not_alloc_array3DI64(a3d)    result(b_res)
          implicit none
          integer(I64P), allocatable, dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                                            :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a3d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Conducting check if real 
    !           array 3D is successfully allocated
    !           
    ! @Params: real(R32P), dimension(:,:,:) :: a3d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50  
    pure function is_not_alloc_array3DR32(a3d)   result(b_res)
          implicit none
          real(R32P), allocatable, dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                                         :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a3d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Conducting check if real 
    !           array 3D is successfully allocated
    !           
    ! @Params: real(R64P), dimension(:,:,:) :: a3d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50  
    pure function is_not_alloc_array3DR64(a3d)    result(b_res)
          implicit none
          real(R64P), allocatable, dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                                         :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a3d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Conducting check if logical 
    !           array 3D is successfully allocated
    !           
    ! @Params: logical(I32P), dimension(:,:,:) :: a3d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50 
    pure function is_not_alloc_array3DL32(a3d)    result(b_res)
          implicit none
          logical(I32P), allocatable, dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                                            :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a3d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
     
    !============================================50
    ! @Purpose:
    !           Conducting check if logical 
    !           array 3D is successfully allocated
    !           
    ! @Params: logical(I64P), dimension(:,:,:) :: a3d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50 
    pure function is_not_alloc_array3DL64(a3d)    result(b_res)
          implicit none
          logical(I64P), allocatable, dimension(:,:,:), intent(in) :: a3d
          ! Locals
          logical(I32P)                                            :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a3d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conducting check if integral 
    !           array 4D is successfully allocated
    !           
    ! @Params: integer(I32P), dimension(:,:,:,:) :: a4d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50 
    pure function is_not_alloc_array4DI32(a4d)    result(b_res)
          implicit none
          integer(I32P), allocatable, dimension(:,:,:,:), intent(in) :: a4d
          ! Locals
          logical(I32P)                                              :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a4d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Conducting check if integral 
    !           array 4D is successfully allocated
    !           
    ! @Params: integer(I64P), dimension(:,:,:,:) :: a4d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50
    pure function is_not_alloc_array4DI64(a4d)    result(b_res)
          implicit none
          integer(I64P), allocatable, dimension(:,:,:,:), intent(in) :: a4d
          ! Locals
          logical(I32P)                                              :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a4d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conducting check if real 
    !           array 4D is successfully allocated
    !           
    ! @Params: real(R32P), dimension(:,:,:,:) :: a4d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50  
    pure function is_not_alloc_array4DR32(a4d)    result(b_res)
          implicit none
          real(R32P), allocatable, dimension(:,:,:,:), intent(in) :: a4d
          ! Locals
          logical(I32P)                                           :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a4d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Conducting check if real 
    !           array 4D is successfully allocated
    !           
    ! @Params: real(R64P), dimension(:,:,:,:) :: a4d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50 
    pure function is_not_alloc_array4DR64(a4d)    result(b_res)
          implicit none
          real(R64P), allocatable, dimension(:,:,:,:), intent(in) :: a4d
          ! Locals
          logical(I32P)                                           :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a4d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    !============================================50
    ! @Purpose:
    !           Conducting check if logical 
    !           array 4D is successfully allocated
    !           
    ! @Params: logical(I32P), dimension(:,:,:,:) :: a4d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50 
    pure function is_not_alloc_array4DL32(a4d)    result(b_res)
          implicit none
          logical(I32P), allocatable, dimension(:,:,:,:), intent(in) :: a4d
          ! Locals
          logical(I32P)                                              :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a4d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
    !============================================50
    ! @Purpose:
    !           Conducting check if logical 
    !           array 4D is successfully allocated
    !           
    ! @Params: logical(I64P), dimension(:,:,:,:) :: a4d
    ! @Throws: Nothing          
    ! @Pureness: True
    ! @Returnes: .TRUE. iff dummy argument (allocatable
    !             array) is allocated, otherwise .FALSE.
    !             
    !               
    !============================================50 
    pure function is_not_alloc_array4DL64(a4d)    result(b_res)
          implicit none
          logical(I64P), allocatable, dimension(:,:,:,:), intent(in) :: a4d
          ! Locals
          logical(I32P)                                              :: b_res
          ! Start of executable statements
          if(.NOT. allocated(a4d)) then
              b_res = .true.
          else
              b_res = .false.
          end if
    end function
    
    
        
        
end  module