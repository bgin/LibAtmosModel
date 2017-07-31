
module  module_multi_array_flattening
include "Config.fpp"


  !-----------------------------------------------------------------------------------85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'module_multi_array_flattening'
 !          
 !          Purpose:
 !                         This module "flattens" multi-dimensional allocatable
 !                         deffered-shape arrays to 1-D rank assumed-size arrays.
 !                         Flattened assumed size arrays are passed to C-CUDA
 !                         wrappers which in turn call CUDA kernels to perform
 !                         computation on behalf of Fortran interface.
 !          History:
 !                      Date: 22-01-2017
 !                      Time: 13:34 GMT+2
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
 !                       
 !----------------------------------------------------------------------------------85   


 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

    USE module_kinds
    USE module_class_error_check
   
!  File version parameters
!  Major
integer(i32), parameter, public :: module_multi_array_flattening_major = 1

!  Minor
integer(i32), parameter, public :: module_multi_array_flattening_minor = 0

!  Micro
integer(i32), parameter, public :: module_multi_array_flattening_micro = 0

!  File version
integer(i32), parameter, public :: module_multi_array_flattening_version = 1000*module_multi_array_flattening_major &
                                    + 100*module_multi_array_flattening_minor + 10*module_multi_array_flattening_micro            

!  Date Creation.
character(LEN=*) CreateDate
parameter (CreateDate = '24-01-2017 19:49 +0200 (Tue, 24 Jan 2014) GMT+2' )

!  Date of Build should be set after successful built of this file
character(LEN=*) BuildDate
parameter (BuildDate = '')

! Author information
character(LEN=*) AuthorInfo
parameter (AuthorInfo = 'Programmer: Bernard Gingold, contact: beniekg@gmail.com' )

! Module name
character(LEN=*) ModuleName
parameter (ModuleName = 'module_multi_array_flattening' )

public :: CreateDate, BuildDate, AuthorInfo, ModuleName


! Module definitions and parameters.

character(LEN=*) file_path
parameter (file_path = 'C:\Users\Bernard\Documents\Visual Studio 2013\Projects\WRF_Goddard_Modification\WRF_Goddard_Modification\module_multi_array_flattening.f90')

integer(i32), parameter :: def_num_threads = 4


 public  :: s_multiarray1D_to_flat1D,   &
            d_multiarray1D_to_flat1D,   &
            s_multiarray2D_to_flat1D,   &
            d_multiarray2D_to_flat1D,   &
            s_multiarray3D_to_flat1D,   &
            d_multiarray3D_to_flat1D,   &
            s_multiarray4D_to_flat1D,   &
            d_multiarray4D_to_flat1D,   &
            i32_multiarray1D_to_flat1D, &
            i64_multiarray1D_to_flat1D, &
            i32_multiarray2D_to_flat1D, &
            i64_multiarray2D_to_flat1D, &
            i32_multiarray3D_to_flat1D, &
            i64_multiarray3D_to_flat1D, &
            i32_multiarray4D_to_flat1D, &
            i64_multiarray4D_to_flat1D, &
            l32_multiarray1D_to_flat1D, &
            l64_multiarray1D_to_flat1D, &
            l32_multiarray2D_to_flat1D, &
            l64_multiarray2D_to_flat1D, &
            l32_multiarray3D_to_flat1D, &
            l64_multiarray3D_to_flat1D, &
            l32_multiarray4D_to_flat1D, &
            l64_multiarray4D_to_flat1D

!======================================40
! Flatten real deffered arrays of rank 1
!======================================40

   
    
    interface  FlattenReal1D
    
         module  procedure  s_multiarray1D_to_flat1D
         
         module  procedure  d_multiarray1D_to_flat1D
    
    end  interface
    
!======================================40
! Flatten real deffered arrays of rank 2.
!======================================40
    
    interface  FlattenReal2D
    
         module  procedure  s_multiarray2D_to_flat1D
         
         module  procedure  d_multiarray2D_to_flat1D
    
    end  interface
    
!======================================40
! Flatten real deffered arrays of rank 3.
!======================================40
    
    interface  FlattenReal3D
    
         module  procedure  s_multiarray3D_to_flat1D
         
         module  procedure  d_multiarray3D_to_flat1D
    
    end  interface
    
!======================================40
! Flatten real deffered arrays of rank 4.
!======================================40
    
    interface  FlattenReal4D
    
          module  procedure s_multiarray4D_to_flat1D
          
          module  procedure d_multiarray4D_to_flat1D
    
    end  interface
    
!============================================46
! Flatten integral deferred arrays of rank 1.
!============================================46
    
    interface  FlattenIntegral1D
    
         module  procedure  i32_multiarray1D_to_flat1D
         
         module  procedure  i64_multiarray1D_to_flat1D
         
    end  interface
    
!============================================46
! Flatten integral defferd arrays of rank 2.
!============================================46
    
    interface  FlattenIntegral2D
    
         module  procedure  i32_multiarray2D_to_flat1D
         
         module  procedure  i64_multiarray2D_to_flat1D
    
    end  interface
    
!============================================46
! Flatten integral deffered arrays of rank 3.
!============================================46
    
    interface  FlattenIntegral3D
    
         module  procedure  i32_multiarray3D_to_flat1D
         
         module  procedure  i64_multiarray3D_to_flat1D
    
    end  interface
    
!============================================46
! Flatten integral defferd arrays of rank 4.
!============================================46
    
    interface  FlattenIntegral4D
    
         module  procedure  i32_multiarray4D_to_flat1D
         
         module  procedure  i64_multiarray4D_to_flat1D
    
    end  interface
    
!============================================46
! Flatten  logical defferd arrays of rank 1.
!============================================46
    
    interface  FlattenLogical1D
         ! Logical arrays are emulated by int and long long int
         ! set to either 0 or 1.
         module  procedure  l32_multiarray1D_to_flat1D
         
         module  procedure  l64_multiarray1D_to_flat1D
    
    end  interface
    
!============================================46
! Flatten logical defferd arrays of rank 2.
!============================================46
    
    interface  FlattenLogical2D
         ! Logical arrays are emulated by int and long long int
         ! set to either 0 or 1.
         module  procedure  l32_multiarray2D_to_flat1D
         
         module  procedure  l64_multiarray2D_to_flat1D
    
    end  interface
    
!============================================46
! Flatten logical deffered arrays of rank 3.
!============================================46
    
    interface  FlattenLogical3D
         ! Logical arrays are emulated by int and long long int
         ! set to either 0 or 1.
         module  procedure  l32_multiarray3D_to_flat1D
         
         module  procedure  l64_multiarray3D_to_flat1D
    
    end  interface
    
!============================================46
! Flatten logical deffered arrays of rank 4.
!============================================46
    
    interface  FlattenLogic4D
         ! Logical arrays are emulated by int and long long int
         ! set to either 0 or 1.
         module  procedure  l32_multiarray4D_to_flat1D
         
         module  procedure  l64_multiarray4D_to_flat1D
    
    end  interface
    
    CONTAINS
    
    
        
    
         subroutine  s_multiarray1D_to_flat1D(multiarray1D,flat_array1D,lo_bound,hi_bound,length)
                                                
         
                     IMPLICIT NONE
                     real(R32), dimension(:), intent(in)            :: multiarray1D
                     real(R32), dimension(*), intent(inout)         :: flat_array1D
                     !DIR$ ASSUME_ALIGNED multiarray1D:32, flat_array1D:32
                     integer(i32),            intent(in),  optional :: lo_bound
                     integer(i32),            intent(in),  optional :: hi_bound
                     integer(i32),            intent(in)            :: length
                     
                    
                     ! Local variables
                     integer(i32) :: i
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: s_multiarray1D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT'   ]
                     
                     character(len=80), dimension(6) :: msg2 = [ '***** FATAL-ERROR ***** in subroutine: s_multiarraya1D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray1D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****' ]
                                                                 
                                                                
                                                                  
                     ! Start off executable statements.
                      
                     call check_alloc1DR32_fail(multiarray1D,msg1,file_path,235)
                     
                     if(size(multiarray1D) /= length) then
                         call check_dim1Di32_eq(size(multiarray1D),length,msg2,file_path,236 )
                     end if
                     
                     if((present(lo_bound)) .AND. (present(hi_bound))) then
                         !DIR$ SIMD
                         !DIR$ UNROLL (4)
                         do i = lo_bound, hi_bound
                             flat_array1D(i) = multiarray1D(i)
                         end do
                     else
                         !DIR$ SIMD
                         !DIR$ UNROLL (4)
                         do i = 1, length
                             flat_array1D(i) = multiarray1D(i)
                         end do
                     end if
                     
         end  subroutine
         
         
         subroutine  d_multiarray1D_to_flat1D(multiarray1D,flat_array1D,lo_bound,hi_bound,length)
         
                     IMPLICIT NONE
                     real(R64), dimension(:), intent(in)              :: multiarray1D
                     !DIR$ ASSUME_ALIGNED multiarray1D:32
                     real(R64), dimension(*), intent(inout)           :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),            intent(in), optional    :: lo_bound
                     integer(i32),            intent(in), optional    :: hi_bound
                     integer(i32),            intent(in)              :: length
                     ! Local variables
                     integer(i32) :: i
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: d_multiarray1D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT'  ]
                     
                     character(len=80), dimension(6) :: msg2 = [ '***** FATAL-ERROR ***** in subroutine: d_multiarraya1D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray1D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'         ]
                     ! Start of executable statements
                     
                     call check_alloc1DR32_fail(multiarray1D,msg1,file_path,287)
                    
                     if(size(multiarray1D) /= length) then
                            call check_dim1Di32_eq(size(multiarray1D),length,msg2,file_path,289)
                     end if
                     
                     if(present(lo_bound) .AND. present(hi_bound)) then
                         !DIR$ SIMD
                         !DIR$ UNROLL (4)
                         do i = lo_bound, hi_bound
                             flat_array1D(i) = multiarray1D(i)
                         end do
                     else
                         !DIR$ SIMD
                         !DIR$ UNROLL (4)
                         do i = 1, length
                             flat_array1D(i) = multiarray1D(i)
                         end do
                     end if
                     
                     
         end  subroutine
         
         
         
         subroutine  s_multiarray2D_to_flat1D(multiarray2D,flat_array1D,lo_dim1,hi_dim1, &
                                              lo_dim2,hi_dim2, tot_length,copy_subarrays , num_threads     )

                     use omp_lib
                     IMPLICIT NONE
                     real(R32), dimension(:,:), intent(in)             :: multiarray2D
                     !DIR$ ASSUME_ALIGNED multiarray2D:32
                     real(R32), dimension(*), intent(inout)            :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),            intent(in)               :: lo_dim1
                     integer(i32),            intent(in)               :: hi_dim1
                     integer(i32),            intent(in)               :: lo_dim2
                     integer(i32),            intent(in)               :: hi_dim2
                     integer(i32),            intent(out),optional     :: tot_length
                     logical(i32),            intent(in)               :: copy_subarrays
                     integer(i32),            intent(in)               :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,d1,d2
                     character(len=80), dimension(6) :: msg1 = [    '***** FATAL-ERROR ***** in subroutine: s_multiarray2D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: s_multiarraya2D_to_flat1D , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements
         
                     call check_alloc2DR32_fail(multiarray2D,msg1,file_path,342)
                     
                     if(size(multiarray2D) /= size(flat_array1D)) then
                        call check_dim1Di32_eq(size(multiarray2D),size(flat_array1D),msg2,file_path,344) 
                     end if
                     
                     if(num_threads .LE. 0) then
                         call omp_set_num_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                     end if
                     d1 = size(multiarray2D,dim=1)
                     d2 = size(multiarray2D,dim=2)
                     if(present(tot_length)) then
                        if((d1*d2) .LE. HUGE(tot_length)) then
                           tot_length = d1 * d2
                        else
                              print*, '*****FATAL-ERROR*****'
                              print*, 'SUBROUTINE: d_multiarray2D_to_flat1D'
                              print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                              print*, 'dimensions size , d1 * d2 = ', d1 * d2
                              print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                              ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                        end if
                        
                     end if
!DIR$ IF  (_OPENMP .GE. 201307)   

                     if(copy_subarrays .EQ. .true. ) then
                        !$OMP PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(j,i) 
                           do j = lo_dim1,hi_dim1
                               !DIR$ SIMD
                               !DIR$ UNROLL (4)
                               do i = lo_dim2,hi_dim2
                                   flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                               end do
                           end do
                        !$OMP END PARALLEL DO
                     else
                         !$OMP  PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(j,i)
                         do j = 1, d1
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)
                             do i = 1, d2
                                flat_array1D(j+d1*i) = multiarray2D(i,j)
                             end do
                         end do
                        !$OMP END PARALLEL DO
                     end if
!DIR$ ELSE
                    if(copy_subarrays .EQ. .true. ) then
                        do j = lo_dim1,hi_dim1
                            do i = lo_dim2,hi_dim2
                                flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                            end do
                        end do
                    else
                        do j = 1, d1
                            do i = 1, d2
                                flat_array1D(j+d1*i) = multiarray2D(i,j)
                            end do
                        end do
                    end if
                    
                     
                     
!DIR$ ENDIF
                     
         end  subroutine
                                              
                                              
         subroutine  d_multiarray2D_to_flat1D(multiarray2D,flat_array1D,lo_dim1,hi_dim1,lo_dim2, &
                                                hi_dim2,tot_length,copy_subarrays,num_threads   )
         
                     use omp_lib
                     implicit none
                     real(R64), dimension(:,:), intent(in)              :: multiarray2D
                     !DIR$ ASSUME_ALIGNED multiarray2D:32
                     real(R64), dimension(*), intent(inout)             :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),            intent(in)                :: lo_dim1
                     integer(i32),            intent(in)                :: hi_dim1
                     integer(i32),            intent(in)                :: lo_dim2
                     integer(i32),            intent(in)                :: hi_dim2
                     integer(i32),            intent(out),optional      :: tot_length
                     logical(i32),            intent(in)                :: copy_subarrays
                     integer(i32),            intent(in)                :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,d1,d2
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: d_multiarray2D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: d_multiarraya2D_to_flat1D , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements
         
                     call check_alloc2DR64_fail(multiarray2D,msg1,file_path,437)
                     if(size(multiarray2D) /= size(flat_array1D)) then
                            call check_dim1Di32_eq(size(multiarray2D),size(flat_array1D),msg2,file_path,438)
                     end if
                     
                     if(num_threads .LE. 0) then
                         call omp_set_num_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                     end if
                     
                     d1 = size(multiarray2D,dim=1)
                     d2 = size(multiarray2D,dim=2)
                     if(present(tot_length)) then
                         if((d1 * d2) .LE. HUGE(tot_length)) then
                              tot_length = d1 * d2
                         else
                              print*, '*****FATAL-ERROR*****'
                              print*, 'SUBROUTINE: d_multiarray2D_to_flat1D'
                              print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                              print*, 'dimensions size , d1 * d2 = ', d1 * d2
                              print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                              ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     
                     end if
                     
!DIR$ IF  (_OPENMP .GE. 201307)   
                     
                     if(copy_subarrays .EQ. .true. ) then
                         !$OMP PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(j,i)
                         do j = lo_dim1,hi_dim1
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)
                             do i = lo_dim2,hi_dim2
                                flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     else
                         !$OMP  PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(j,i)
                         do j = 1, d1
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)
                             do i = 1, d2
                                 flat_array1D(j+d1*i) = multiarray2D(i,j)
                             end do
                         end do
                         !$OMP END  PARALLEL DO
                     end if
!DIR$ ELSE
                     if(copy_subarray .EQ. .true. ) then
                         do j = lo_dim1,hi_dim1
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                             
                             do i = lo_dim2,hi_dim2
                                 flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                             end do
                         end do
                     else
                         do j = 1, d1
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                            
                             do i = 1, d2
                                 flat_array1D(j+d1*i) = multiarray2D(i,j)
                             end do
                         end do
                     end if
                     
                     
!DIR$ ENDIF
         
         end  subroutine
                                              
                                              
         subroutine  s_multiarray3D_to_flat1D(multiarray3D,flat_array1D,lo_dim1,hi_dim1,lo_dim2,hi_dim2, &
                                                lo_dim3,hi_dim3,tot_length,copy_subarrays,num_threads     )
         
                     use omp_lib
                     implicit none
                     real(R32), dimension(:,:,:), intent(in)             :: multiarray3D
                     !DIR$ ASSUME_ALIGNED multiarray3D:32
                     real(R32), dimension(*),     intent(inout)          :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                intent(in)             :: lo_dim1
                     integer(i32),                intent(in)             :: hi_dim1
                     integer(i32),                intent(in)             :: lo_dim2
                     integer(i32),                intent(in)             :: hi_dim2
                     integer(i32),                intent(in)             :: lo_dim3
                     integer(i32),                intent(in)             :: hi_dim3
                     integer(i32),                intent(out),optional    :: tot_length
                     logical(i32),                intent(in)             :: copy_subarrays
                     integer(i32),                intent(in)             :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,k,d1,d2,d3
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: s_multiarray3D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: s_multiarraya3D_to_flat1D , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements
                     
                     call check_alloc3DR32_fail(multiarray3D,msg1,file_path,553)
                     
                     if(size(multiarray3D) /= size(flat_array1D)) then
                           call check_dim1Di32_eq(size(multiarray3D),size(flat_array1D),msg2,file_path,555)
                     end if
                     
                     if(num_threads .LE. 0) then
                         call omp_set_num_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                     end if
                     
                     d1 = size(multiarray3D,dim=1)
                     d2 = size(multiarray3D,dim=2)
                     d3 = size(multiarray3D,dim=3)
                     if(present(tot_length)) then
                         if((d1*d2*d3) .LE. HUGE(tot_length)) then
                             tot_length = d1*d2*d3
                         else
                              print*, '*****FATAL-ERROR*****'
                              print*, 'SUBROUTINE: s_multiarray3D_to_flat1D'
                              print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                              print*, 'dimensions size , d1 * d2 * d3 = ', d1 * d2 * d3
                              print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                              ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     end if
                     
!DIR$  IF (_OPENMP .GE. 201307)
                     if(copy_subarrays .EQ. .true. ) then
                         !$OMP  PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                         do k = lo_dim1,hi_dim1
                             do j = lo_dim2,hi_dim2
                                 !DIR$ PREFETCH *:0:4
                                 !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                
                                 do i = lo_dim3,hi_dim3
                                     flat_array1D(k+hi_dim1*j+hi_dim2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     else
                         !$OMP  PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                         do k = 1, d1
                            do j = 1, d2
                                !DIR$ PREFETCH *:0:4
                                !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                             
                                do i = 1, d3
                                    flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                                end do
                            end do
                         end do
                         !$OMP END PARALLEL DO
                         end if
!DIR$ ELSE
                        if(copy_subarrays .EQ. true. ) then
                           do k = lo_dim1, hi_dim1
                               do j = lo_dim2, hi_dim2
                                   do i = lo_dim3, hi_dim3
                                       flat_array1D(k+hi_dim1*j+hi_dim2*i) = multiarray3D(i,j,k)
                                   end do
                               end do
                           end do
                        else
                            do k = 1, d1
                                do j = 1, d2
                                   do i = 1, d3
                                       flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                                   end do
                                end do
                            end do
                        end if
                        
                                   
!DIR$ ENDIF
         end  subroutine
                                                
                                                
         subroutine  d_multiarray3D_to_flat1D(multiarray3D,flat_array1D,lo_dim1,hi_dim1,lo_dim2, &
                                                hi_dim2,lo_dim3,hi_dim3,tot_length,copy_subarrays,num_threads )
         
                     use omp_lib
                     implicit none
                     real(R64), dimension(:,:,:), intent(in)              :: multiarray3D
                     !DIR$ ASSUME_ALIGNED multiarray3D:32
                     real(R64), dimension(*),     intent(inout)           :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                intent(in)              :: lo_dim1
                     integer(i32),                intent(in)              :: hi_dim1
                     integer(i32),                intent(in)              :: lo_dim2
                     integer(i32),                intent(in)              :: hi_dim2
                     integer(i32),                intent(in)              :: lo_dim3
                     integer(i32),                intent(in)              :: hi_dim3
                     integer(i32),                intent(out),optional    :: tot_length
                     logical(i32),                intent(in)              :: copy_subarrays
                     integer(i32),                intent(in)              :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,k,d1,d2,d3
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: d_multiarray3D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: d_multiarraya3D_to_flat1D , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
         
                     call check_alloc3DR64_fail(multiarray3D,msg1,file_path,657)
                     
                     if(size(multiarray3D) /= size(flat_array1D)) then
                            call  check_dim1Di32_eq(size(multiarray3D),size(flat_array1D),msg2,file_path,659)
                     end if
                     
                     if(num_threads .LE. 0) then
                         call omp_set_num_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                     end if
                     
                     d1 = size(multiarray3D,dim=1)
                     d2 = size(multiarray3D,dim=2)
                     d3 = size(multiarray3D,dim=3)
                     if(present(tot_length)) then
                         if((d1*d2*d3) .LE. HUGE(tot_length)) then
                             tot_length = d1*d2*d3
                         else
                              print*, '*****FATAL-ERROR*****'
                              print*, 'SUBROUTINE: d_multiarray3D_to_flat1D'
                              print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                              print*, 'dimensions size , d1 * d2 * d3 = ', d1 * d2 * d3
                              print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                              ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     end if
                     
!DIR$ IF (_OPENMP .GE. 201307)
                     if(copy_subarrays .EQ. .true. ) then
                        !$OMP PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                         do k = lo_dim1,hi_dim1
                             do j = lo_dim2,hi_dim2
                                 !DIR$  PREFETCH *:0:4
                                 !DIR$  SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                               
                                 do i = lo_dim3,hi_dim3
                                     flat_array1D(k+hi_dim1*j+hi_dim2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     else
                         !$OMP PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                         do k = 1,d1
                             do j = 1,d2
                                 !DIR$  PREFETCH *:0:4
                                 !DIR$  SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                
                                 do i = 1,d3
                                     flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                         !$OMP  END PARALLEL DO
                     end if
!DIR$  ELSE
                     if(copy_subarrays .EQ. .true. ) then
                         do k = lo_dim1,hi_dim1
                             do j = lo_dim2,hi_dim2
                                 do i = lo_dim3,hi_dim3
                                     flat_array1D(k+hi_dim1*j+hi_dim2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                     else
                         do k = 1,d1
                            do j = 1,d2
                                do i = 1,d3
                                    flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                                end do
                            end do
                         end do
                     end if
                     
                             
                     
!DIR$  ENDIF
                     
         end  subroutine
                                                
                                                
         subroutine  s_multiarray4D_to_flat1D(multiarray4D,flat_array1D,lo_dim1,hi_dim1,lo_dim2, &
                                            hi_dim2,lo_dim3,hi_dim3,lo_dim4,hi_dim4,tot_length,copy_subarrays,num_threads )
         
                     use omp_lib
                     implicit none
                     real(R32), dimension(:,:,:,:), intent(in)              :: multiarray4D
                     !DIR$ ASSUME_ALIGNED multiarray4D:32
                     real(R32), dimension(*),       intent(inout)           :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                  intent(in)              :: lo_dim1
                     integer(i32),                  intent(in)              :: hi_dim1
                     integer(i32),                  intent(in)              :: lo_dim2
                     integer(i32),                  intent(in)              :: hi_dim2
                     integer(i32),                  intent(in)              :: lo_dim3
                     integer(i32),                  intent(in)              :: hi_dim3
                     integer(i32),                  intent(in)              :: lo_dim4
                     integer(i32),                  intent(in)              :: hi_dim4
                     integer(i32),                  intent(out),optional    :: tot_length
                     logical(i32),                  intent(in)              :: copy_subarrays
                     integer(i32),                  intent(in)              :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,k,l,d1,d2,d3,d4
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: s_multiarray4D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: s_multiarraya4D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     
                     call  check_alloc4DI32_fail(multiarray4D,msg1,file_path,765)
                     
                     if(size(multiarray4D) /= size(flat_array1D)) then
                            call  check_dim1Di32_eq(size(multiarray4D),size(flat_array1D),msg2,file_path,767)
                     end if
                     
                     if(num_threads .LE. 0) then
                         call omp_set_num_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                     end if
                     
                     d1 = size(multiarray4D,dim=1)
                     d2 = size(multiarray4D,dim=2)
                     d3 = size(multiarray4D,dim=3)
                     d4 = size(multiarray4D,dim=4)
                     if(present(tot_length)) then
                         if((d1*d2*d3*d4) .LE. HUGE(tot_length)) then
                             tot_length = d1*d2*d3*d4
                         else
                              print*, '*****FATAL-ERROR*****'
                              print*, 'SUBROUTINE: s_multiarray4D_to_flat1D'
                              print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                              print*, 'dimensions size , d1 * d2 * d3 = ', d1 * d2 * d3
                              print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                              ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     end if
                     
!DIR$ IF (_OPENMP .GE. 201307)
                     if(copy_subarrays .EQ. .true. ) then
                         !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(l,k,j,i)
                         do l = lo_dim1,hi_dim1
                             do k = lo_dim2,hi_dim2
                                 do j = lo_dim3,hi_dim3
                                     !DIR$  PREFETCH *:0:4
                                     !DIR$  SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                    
                                     do i = lo_dim4,hi_dim4
                                         flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                     end do
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     else
                         !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(l,k,j,i)
                         do l = 1,d1
                             do k = 1,d2
                                 do j = 1,d3
                                     !DIR$  PREFETCH *:0:4
                                     !DIR$  SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                    
                                     do i = 1,dim4
                                         flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                     end do
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     end if
!DIR$  ELSE
                    if(copy_subarrays .EQ. .true. ) then
                        do l = lo_dim1,hi_dim1
                            do k = lo_dim2,hi_dim2
                                do j = lo_dim3,hi_dim3
                                    do i = lo_dim4,hi_dim4
                                        flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                    end do
                                end do
                            end do
                        end do
                    else
                        do l = 1,d1
                            do k = 1,d2
                                do j = 1,d3
                                    do i = 1,d4
                                        flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                    end do
                                end do
                            end do
                        end do
                    end if
                    
                     
!DIR$  ENDIF
                                     
                                     
                                     
                                     
                                     
         end  subroutine           
                          
                                            
         subroutine  d_multiarray4D_to_float1D(multiarray4D,flat_array1D,lo_dim1,hi_dim1,lo_dim2,hi_dim2, &
                                            lo_dim3,hi_dim3,lo_dim4,hi_dim4,tot_length,copy_subarrays,num_threads )
         
                     use omp_lib
                     implicit none
                     real(R64), dimension(:,:,:,:),  intent(in)              :: multiarray4D
                     !DIR$ ASSUME_ALIGNED multiarray4D:32
                     real(R64), dimension(*),        intent(inout)           :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                   intent(in)              :: lo_dim1
                     integer(i32),                   intent(in)              :: hi_dim1
                     integer(i32),                   intent(in)              :: lo_dim2
                     integer(i32),                   intent(in)              :: hi_dim2
                     integer(i32),                   intent(in)              :: lo_dim3
                     integer(i32),                   intent(in)              :: hi_dim3
                     integer(i32),                   intent(in)              :: lo_dim4
                     integer(i32),                   intent(in)              :: hi_dim4
                     integer(i32),                   intent(out),optional    :: tot_length
                     logical(i32),                   intent(in)              :: copy_subarrays
                     integer(i32),                   intent(in)              :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,k,l,d1,d2,d3,d4
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: d_multiarray4D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: d_multiarraya4D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                    
                    call  check_alloc4DI32_fail(multiarray4D,msg1,file_path,889)
                    
                    if(size(multiarray4D) /= size(flat_array1D)) then
                          call  check_dim1Di32_eq(size(multiarray4D),size(flat_array1D),msg2,file_path,892)
                    end if
                    
                    if(num_threads .LE. 0) then
                        call omp_set_num_threads(def_num_threads)
                    else
                        call omp_set_num_threads(num_threads)
                    end if
                    
                    d1 = size(multiarray4D,dim=1)
                    d2 = size(multiarray4D,dim=2)
                    d3 = size(multiarray4D,dim=3)
                    d4 = size(multiarray4D,dim=4)
                    if(present(tot_length)) then
                        if((d1*d2*d3*d4) .LE. HUGE(tot_length)) then
                            tot_length = d1*d2*d3*d4
                        else
                              print*, '*****FATAL-ERROR*****'
                              print*, 'SUBROUTINE: d_multiarray4D_to_flat1D'
                              print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                              print*, 'dimensions size , d1 * d2 * d3 = ', d1 * d2 * d3
                              print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                              ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                        end if
                    end if
                    
!DIR$  IF (_OPENMP .GE. 201307)
                    if(copy_subarrays .EQ. .true. ) then
                        !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(l,k,j,i)
                        do l = lo_dim1,hi_dim1
                            do k = lo_dim2,hi_dim2
                                do j = lo_dim3,hi_dim3
!DIR$  IF (USE_SOFT_PREFETCH .EQ. 1)                                    
                                    !DIR$ PREFETCH *:0:4
!DIR$  ENDIF                                    
                                    !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                  
                                    do i = lo_dim4,hi_dim4
                                        flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                    end do
                                end do
                            end do
                        end do
                        !$OMP END PARALLEL DO
                    else
                        !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(l,k,j,i)
                        do l = 1,d1
                            do k = 1,d2
                                do j = 1,d3
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)                                    
                                    !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                                   
                                    !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                   
                                    do i = 1,d4
                                        flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                    end do
                                end do
                            end do
                        end do
                        !$OMP END PARALLEL DO
                        
                    end if

!DIR$ ELSE
                    if(copy_subarrays .EQ. .true. ) then
                        
                        do l = lo_dim1,hi_dim1
                            do k = lo_dim2,hi_dim2
                                do j = lo_dim3,hi_dim3
                                    do i = lo_dim4,hi_dim4
                                        flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                    end do
                                end do
                            end do
                        end do
                        
                    else     
                        
                        do l = 1,d1
                            do k = 1,d2
                                do j = 1,d3
                                    do i = 1,d4
                                        flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                    end do
                                end do
                            end do
                        end do
                        
                        
                        
                        
                    end if
                    
                    
                    
!DIR$ ENDIF
                    
         end  subroutine
                                            
                                            
                                            
         subroutine  i32_multiarray1D_to_flat1D(multiarray1D,flat_array1D,lo_dim1,hi_dim1,tot_length, &
                                                copy_subarrays  )
         
                     implicit none
                     integer(i32), dimension(:), intent(in)             :: multiarray1D
                     !DIR$ ASSUME_ALIGNED multiarray1D:32
                     integer(i32), dimension(*),  intent(inout)         :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                intent(in)            :: lo_dim1
                     integer(i32),                intent(in)            :: hi_dim1
                     integer(i32),                intent(out), optional :: tot_length
                     logical(i32),                intent(in)            :: copy_subarrays
                     !Local variables
                     integer :: i,d1
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: i32_multiarray1D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: i32_multiarraya1D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements.
                     call check_alloc1DI32_fail(multiarray1D,msg1,file_path,1087)
                     
                     if(size(multiarray1D) /= size(flat_array1D)) then
                            call check_dim1Di32_eq(size(multiarray1D),size(flat_array1D),msg2,file_path,1089)
                     end if
                     
                     d1 = size(mutliarray1D)
                     if(present(tot_length)) then
                         if(d1 .LE. HUGE(tot_length)) then
                             tot_length = d1
                         else
                              print*, '*****FATAL-ERROR*****'
                              print*, 'SUBROUTINE: i32_multiarray1D_to_flat1D'
                              print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                              print*, 'dimensions size , d1 = ', d1
                              print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                              ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     end if

!DIR$ IF (USE_AUTO_VECTORIZATION .EQ. 1)
                     if(copy_subarrays .EQ. .true. ) then
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                         !DIR$ PREFETCH *:0:4
!DIR$ ENDIF
                         !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                     
                         do i = lo_dim1,hi_dim1
                             flat_array1D(i) = multiarray1D(i)
                         end do
                     else
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                         !DIR$ PREFETCH *:0:4
!DIR$ ENDIF
                         !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                              
                         do  i = 1, d1
                             flat_array1D(i) = multiarray1D(i)
                         end do
                         
                     end if
!DIR$ ELSE
                     if(copy_subarrays .EQ. .true. ) then
                       
                         do i = lo_dim1,hi_dim1
                             flat_array1D(i) = multiarray1D(i)
                         end do
                     else
                         
                         do i = 1, d1
                             flat_array1D(i) = multiarray1D(i)
                         end do
                     end if
                     
                     
                     
!DIR$ ENDIF
                             
                             
                             
                             
                             
         end  subroutine
                                                
         
         subroutine  i64_multiarray1D_to_flat1D(multiarray1D,flat_array1D,lo_dim1,hi_dim1,tot_length, &
                                                    copy_subarrays )
                                                
                     implicit none
                     integer(i64), dimension(:), intent(in)            :: multiarray1D
                     !DIR$ ASSUME_ALIGNED multiarray1D:32
                     integer(i64), dimension(*), intent(inout)         :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),               intent(in)            :: lo_dim1
                     integer(i32),               intent(in)            :: hi_dim1
                     integer(i32),               intent(out), optional :: tot_length
                     logical(i32),               intent(in)            :: copy_subarrays
                     ! Local variables
                     integer(i32) :: i,d1
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: i64_multiarray1D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: i64_multiarraya1D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements.
                     
                     call check_alloc1DI64_fail(multiarray1D,msg1,file_path,1178)
                     
                     if(size(multiarray1D) /= size(flat_array1D)) then
                            call check_dim1Di32_eq(size(multiarray1D),size(flat_array1D),msg2,file_path,1181)
                     end if
                     
                     d1 = size(multiarray1D)
                     if(present(tot_length)) then   ! Guard agains wrap around
                         if(d1 .LE. HUGE(tot_length)) then
                             tot_length = d1
                         else
                              print*, '*****FATAL-ERROR*****'
                              print*, 'SUBROUTINE: i64_multiarray1D_to_flat1D'
                              print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                              print*, 'dimensions size , d1 = ', d1
                              print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                              ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     end if
!DIR$  IF (USE_AUTO_VECTORIZATION .EQ. 1)
                     if(copy_subarrays .EQ. .true. ) then
!DIR$  IF (USE_SOFT_PREFETCH .EQ. 1)
                         !DIR$ PREFETCH *:0:4
!DIR$  ENDIF                        
                         !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                        
                         do i = lo_dim1,hi_dim1
                             flat_array1D(i) = multiarray1D(i)
                         end do
                     else
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                         !DIR$ PREFETCH *:0:4
!DIR$ ENDIF
                         !DIR$ SIMD
                         !DIR$ UNROLL (4)                         
                         do i = 1,d1
                             flat_array1D(i) = multiarray1D(i)
                         end do
                     end if
!DIR$ ELSE
                     if(copy_subarrays .EQ. .true. ) then    ! Scalar copy
                        
                         do i = lo_dim1,hi_dim1
                             flat_array1D(i) = multiarray1D(i)
                         end do
                     else
                        
                         do i = 1,d1
                             flat_array1D(i) = multiarray1D(i)
                         end do
                     end if
                     
!DIR$ ENDIF
                                                
         end  subroutine                                       
         
                                                    
         subroutine  i32_multiarray2D_to_flat1D(multiarray2D,flat_array1D,lo_dim1,hi_dim1, &
                                                lo_dim2,hi_dim2,tot_length,copy_subarrays,num_threads )
         
                     use omp_lib
                     implicit none
                     integer(i32), dimension(:,:), intent(in)    :: multiarray2D
                     !DIR$ ASSUME_ALIGNED multiarray2D:32
                     integer(i32), dimension(*),   intent(inout) :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                 intent(in)            :: lo_dim1
                     integer(i32),                 intent(in)            :: hi_dim1
                     integer(i32),                 intent(in)            :: lo_dim2
                     integer(i32),                 intent(in)            :: hi_dim2
                     integer(i32),                 intent(out), optional :: tot_length    
                     logical(i32),                 intent(in)            :: copy_subarrays
                     integer(i32),                 intent(in)            :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,d1,d2
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: i32_multiarray2D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: i32_multiarraya2D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements
                     
                     call check_alloc2DI32_fail(multiarray2D,msg1,file_path,1277)
                     
                     if(size(multiarray2D) /= size(flat_array1D)) then
                             call check_dim1Di32_eq(size(multiarray2D,size(flat_array1D),msg2,file_path,1279)
                     end if
                     
                     if(num_threads .LE. 0 ) then
                         call omp_set_num_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                     end if
                     
                     d1 = size(multiarray2D,dim=1)
                     d2 = size(multiarray2D,dim=2)
                     if(present(tot_length)) then
                         if((d1*d2) .LE. HUGE(tot_length)) then
                             tot_length = d1*d2
                         else
                              print*, '*****FATAL-ERROR*****'
                              print*, 'SUBROUTINE: i32_multiarray2D_to_flat1D'
                              print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                              print*, 'dimensions size , d1 * d2 = ', d1*d2
                              print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                              ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     end if
                     
!DIR$ IF (_OPENMP .GE. 201307)
                     if(copy_subarrays .EQ. .true. ) then
                        !$OMP PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(j,i)
                         do j = lo_dim1,hi_dim1
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                            
                             do i = lo_dim2,hi_dim2
                                 flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     else
                         !$OMP PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(j,i)
                         do j = 1,d1
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                           
                             do i = 1,d2
                                 flat_array1D(j+d1*i) = multiarray2D(i,j)
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     end if
!DIR$ ELSE
                     

                    if(copy_subarrays .EQ. .true. ) then
                        ! Vectorize only
                        do j = lo_dim1,hi_dim1
                            !DIR$ SIMD
                            !DIR$ UNROLL (4)
                            do i = lo_dim2,hi_dim2
                                flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                            end do
                        end do
                    else
                        
                        do j = 1,d1
                            !DIR$ SIMD
                            !DIR$ UNROLL (4)
                            do i = 1,d2
                                flat_array1D(j+d1*i) = multiarray2D(i,j)
                            end do
                        end do
                    end if
                    
!DIR$ ENDIF                        
                         
                         
                         
                         
                         
         end subroutine 
         
                                                
         subroutine  i64_multiarray2D_to_flat1D(multiarray2D,flat_array1D,lo_dim1,hi_dim1,lo_dim2, &
                                                    hi_dim2,tot_length,copy_subarrays,num_threads   )
         
                     use omp_lib
                     implicit none
                     integer(i64), dimension(:,:), intent(in)            :: multiarray2D
                     !DIR$ ASSUME_ALIGNED multiarray2D:32
                     integer(i64), dimension(*),   intent(inout)         :: flat_array1D
                     !DIR$ ASUME_ALIGNED flat_array1D:32
                     integer(i32),                 intent(in)            :: lo_dim1
                     integer(i32),                 intent(in)            :: hi_dim1
                     integer(i32),                 intent(in)            :: lo_dim2
                     integer(i32),                 intent(in)            :: hi_dim2
                     integer(i32),                 intent(out), optional :: tot_length
                     logical(i32),                 intent(in)            :: copy_subarrays
                     integer(i32),                 intent(in)            :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,d1,d2
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: i64_multiarray2D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: i64_multiarraya2D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                   ! Start of executable statements
                   
                   call check_alloc2DI32_fail(multiarray2D,msg1,file_path,1463)
                   
                   if(size(multiarray2D) /= size(flat_array1D)) then
                           call check_dim1Di32_eq(size(multiarray2D),size(flat_array1D),msg2,file_path,1465)
                   end if
                   
                   if(num_threads .LE. 0 ) then
                       call omp_set_num_threads(def_num_threads)
                   else
                       call omp_set_num_threads(num_threads)
                   end if
                   
                   d1 = size(multiarray2D,dim=1)
                   d2 = size(multiarray2D,dim=2)
                   if(present(tot_length)) then
                       if((d1*d2) .LE. HUGE(TOT_LENGTH)) then
                           tot_length = d1*d2
                       else
                            print*, '*****FATAL-ERROR*****'
                            print*, 'SUBROUTINE: i64_multiarray2D_to_flat1D'
                            print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                            print*, 'dimensions size , d1 * d2 = ', d1*d2
                            print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                            ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                       end if
                   end if

!DIR$ IF (_OPENMP .GE. 201307)
                   if(copy_subarrays .EQ. .true. ) then
                       !$OMP PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(j,i)
                       do j = lo_dim1,hi_dim1
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF 
                           do i = lo_dim2,hi_dim2
                               flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                           end do
                       end do
                       !$OMP END PARALLEL DO
                   else
                       !$OMP PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(j,i)
                       do j = 1,d1
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF 
                           do i = 1,d2
                               flat_array1D(j+d1*i) = multiarray2D(i,j)
                           end do
                       end do
                       !$OMP END PARALLEL DO
                   end if
!DIR$ ELSE
                   
                   if(copy_subarrays .EQ. .true. ) then
                      ! Vectorize only
                       do j = lo_dim1,hi_dim1
                           !DIR$ SIMD
                           !DIR$ UNROLL (4)
                           do i = lo_dim2,hi_dim2
                               flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                           end do
                       end do
                   else
                       do j = 1,d1
                           !DIR$ SIMD
                           !DIR$ UNROLL (4)
                           do i = 1,d2
                               flat_array1D(j+d1*i) = multiarray2D(i,j)
                           end do
                       end do
                   end if
                   
                   
!DIR$ ENDIF
                       
                   
         end  subroutine
                                                
         
         subroutine  i32_multiarray3D_to_flat1D(multiarray3D,flat_array1D,lo_dim1,hi_dim1,lo_dim2, &
                                                    hi_dim2,lo_dim3,hi_dim3,tot_length,copy_subarrays,num_threads )
         
                     use omp_lib
                     implicit none
                     integer(i32), dimension(:,:,:), intent(in) :: multiarray3D
                     !DIR$ ASSUME_ALIGNED multiarray3D:32
                     integer(i32), dimension(*),     intent(inout) :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                   intent(in)            :: lo_dim1
                     integer(i32),                   intent(in)            :: hi_dim1
                     integer(i32),                   intent(in)            :: lo_dim2
                     integer(i32),                   intent(in)            :: hi_dim2
                     integer(i32),                   intent(in)            :: lo_dim3
                     integer(i32),                   intent(in)            :: hi_dim3
                     integer(i32),                   intent(out), optional :: tot_length
                     logical(i32),                   intent(in)            :: copy_subarrays
                     integer(i32),                   intent(in)            :: num_threads
                     ! Local variables
                     integer(i32) :: i,k,j,d1,d2,d3
                     character(len=*) FunName
                     parameter(FunName = 'subroutine: i32_multiarray3D_to_flat1D' )
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: i32_multiarray3D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: i32_multiarraya3D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements
                     
                     call check_alloc3DI32_fail(multiarray3D,msg1,file_path,1589)
                     
                     if((hi_dim1 .LE. lo_dim1) .OR. (hi_dim2 .LE. lo_dim2) .OR. (hi_dim3 .LE. lo_dim3)) then
                             print*, '***** FATAL-ERROR *****'
                             print*, 'In file: ', file_path
                             print*, 'In function: ', FunName
                             print*, 'At line: ', 1620
                             print*, '***** ERROR-DETAILS *****'
                             print*, 'Invalid Bounds Size!!'
                             print*, 'lo_dim1=',lo_dim1, 'hi_dim1=',hi_dim1, 'lo_dim2=',lo_dim2, 'hi_dim2=',hi_dim2, &
                                     'lo_dim3=',lo_dim3,'hi_dim3=',hi_dim3
                             ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE!!'
                     end if
                     
                     if(size(multiarray3D) /= size(flat_array1D)) then
                            call check_dim1Di32_eq(size(multiarray3D),size(flat_array1D),msg2,file_path,1591)
                     end if
                     
                     if(num_threads .LE. 0) then
                         call omp_setnum_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                     end if
                     
                     d1 = size(multiarray3D,dim=1)
                     d2 = size(multiarray3D,dim=2)
                     d3 = size(multiarray3D,dim=3)
                     if(present(tot_length)) then
                         if((d1*d2*d3) .LE. HUGE(tot_length)) then
                             tot_length = d1*d2*d3
                         else
                            print*, '*****FATAL-ERROR*****'
                            print*, 'SUBROUTINE: i32_multiarray3D_to_flat1D'
                            print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                            print*, 'dimensions size , d1 * d2 * d3 = ', d1*d2*d3
                            print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                            ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     end if
                     
!DIR$  IF (_OPENMP .GE. 201307)
                     if(copy_subarrays .EQ. .true. ) then
                         !$OMP PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                         do k = lo_dim1,hi_dim1
                             do j = lo_dim2,hi_dim2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF 
                                 do i = lo_dim3,hi_dim3
                                     flat_array1D(k+hi_dim1*j+hi_dim2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     else
                         !$OMP PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                         do k = 1,d1
                             do j = 1, d2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                  
                                 do i = 1,d3
                                     flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     end if
!DIR$ ELSE
                     if(copy_subarrays .EQ. .true. ) then
                         ! Vectorize only
                         do k = lo_dim1,hi_dim1
                             do j = lo_dim2,hi_dim2
                                 !DIR$ SIMD
                                 !DIR$ UNROLL (4)
                                 do i = lo_dim3,hi_dim3
                                     flat_array1D(k+hi_dim3*j+hi_dim2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                     else
                         ! Vectorize only
                         do k = 1,d1
                             do j = 1,d2
                                 !DIR$ SIMD
                                 !DIR$ UNROLL (4)
                                 do i = 1,d3
                                     flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                     end if
                     
!DIR$ ENDIF
                     
                     
         end  subroutine
                                                    
                                                    
                                                    
         subroutine  i64_multiarray3D_to_flat1D(multiarray3D,flat_array1D,lo_dim1,hi_dim1,lo_dim2, &
                                                hi_dim2,lo_dim3,hi_dim3,tot_length,copy_subarrays,num_threads )
         
                     use omp_lib
                     implicit none
                     integer(i64),  dimension(:,:,:), intent(in)    :: multiarray3D
                     !DIR$ ASSUME_ALIGNED multiarray3D:32
                     integer(i64),  dimension(*),     intent(inout) :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                    intent(in)            :: lo_dim1
                     integer(i32),                    intent(in)            :: hi_dim1
                     integer(i32),                    intent(in)            :: lo_dim2
                     integer(i32),                    intent(in)            :: hi_dim2
                     integer(i32),                    intent(in)            :: lo_dim3
                     integer(i32),                    intent(in)            :: hi_dim3
                     integer(i32),                    intent(out), optional :: tot_length
                     logical(i32),                    intent(in)            :: copy_subarrays
                     integer(i32),                    intent(in)            :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,k,d1,d2,d3
                     character(len=*) FunName
                     parameter(FunName = 'subroutine: i64_multiarray3D_to_flat1D')
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: i64_multiarray3D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: i64_multiarraya3D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements.
                     
                     call check_alloc3DI64_fail(multiarray3D,msg1,file_path,1751)
                     
                     if((hi_dim1 .LE. lo_dim1) .OR. (hi_dim2 .LE. lo_dim2) .OR. (hi_dim3 .LE. lo_dim3)) then
                             print*, '***** FATAL-ERROR *****'
                             print*, 'In file: ', file_path
                             print*, 'In function: ', FunName
                             print*, 'At line: ', 1755
                             print*, '***** ERROR-DETAILS *****'
                             print*, 'Invalid Bounds Size!!'
                             print*, 'lo_dim1=',lo_dim1, 'hi_dim1=',hi_dim1, 'lo_dim2=',lo_dim2, 'hi_dim2=',hi_dim2, &
                                     'lo_dim3=',lo_dim3,'hi_dim3=',hi_dim3
                             ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE!!'
                     end if
                     
                     if(size(multiarray3D) /= size(flat_array1D)) then
                              call check_dim3Di32_eq(size(multiarray3D),size(flat_array1D),msg2,file_path,1767)
                     end if
                     
                       
                     if(num_threads .LE. 0) then
                         call omp_setnum_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                     end if
                     
                     d1 = size(multiarray3D,dim=1)
                     d2 = size(multiarray3D,dim=2)
                     d3 = size(multiarray3D,dim=3)
                     
                     if(present(tot_length)) then
                         if((d1*d2*d3) .LE. HUGE(tot_length)) then
                             tot_length = d1*d2*d3
                         else
                            print*, '*****FATAL-ERROR*****'
                            print*, FunName
                            print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                            print*, 'dimensions size , d1 * d2 * d3 = ', d1*d2*d3
                            print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                            ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!' 
                         end if
                     end if
 
!DIR$ IF (_OPENMP .GE. 201307)
                     if(copy_subarrays .EQ. .true. ) then
                         !$OMP PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                         do k = lo_dim1,hi_dim1
                             do j = lo_dim2,hi_dim2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                  
                                do i = lo_dim3,hi_dim3
                                    flat_array1D(k+hi_dim1*j+hi_dim2*i) = multiarrray3D(i,j,k)
                                end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     else
                         !$OMP PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                         do k = 1,d1
                             do j = 1,d2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                  
                                 do i = 1,d3
                                     flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     end if
!DIR$ ELSE
                     if(copy_subarrays .EQ. .true. ) then
                         ! Vectorize only
                         do k = lo_dim1,hi_dim1
                             do j = lo_dim2,hi_dim2
                                 !DIR$ SIMD
                                 !DIR$ UNROLL (4)
                                 do i = lo_dim3,hi_dim3
                                     flat_array1D(k+hi_dim1*j+hi_dim2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                     else
                         ! Vectorize only
                         do k = 1,d1
                             do j = 1,d2
                                 !DIR$ SIMD
                                 !DIR$ UNROLL (4)
                                 do i = 1,d3
                                     flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                     end if

!DIR$ ENDIF
                                 
                                 
                                 
                                 
                                 
         end  subroutine
                                                    
                                                    
         subroutine  i32_multiarray4D_to_flat1D(multiarray4D,flat_array1D,lo_dim1,hi_dim1,lo_dim2, &
                                            hi_dim2,lo_dim3,hi_dim3,lo_dim4,hi_dim4,tot_length,copy_subarrays,num_threads)
         
                     use omp_lib
                     implicit none
                     integer(i32), dimension(:,:,:,:),  intent(in)            :: multiarray4D
                     !DIR$ ASSUME_ALIGNED multiarray4D:32
                     integer(i32), dimension(*),        intent(inout)         :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                      intent(in)            :: lo_dim1
                     integer(i32),                      intent(in)            :: hi_dim1
                     integer(i32),                      intent(in)            :: lo_dim2
                     integer(i32),                      intent(in)            :: hi_dim2
                     integer(i32),                      intent(in)            :: lo_dim3
                     integer(i32),                      intent(in)            :: hi_dim3
                     integer(i32),                      intent(in)            :: lo_dim4
                     integer(i32),                      intent(in)            :: hi_dim4
                     integer(i32),                      intent(out), optional :: tot_length
                     logical(i32),                      intent(in)            :: copy_subarrays
                     integer(i32),                      intent(in)            :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,k,l,d1,d2,d3,d4
                     character(len=*) FunName
                     parameter(FunName = 'subroutine: i32_multiarray4D_to_flat1D' )
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: i32_multiarray4D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: i32_multiarraya4D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements.
                     
                     call  check_alloc4DI32_fail(multiarray4D,msg1,file_path,1914)
                     
                     if((hi_dim1 .LE. lo_dim1) .OR. (hi_dim2 .LE. lo_dim2) .OR. &
                        (hi_dim3 .LE. lo_dim3) .OR. (hi_dim4 .LE. lo_dim4))     then
                         
                           print*, '***** FATAL-ERROR *****'
                           print*, 'In file: ', file_path
                           print*, 'In function: ', FunName
                           print*, 'At line: ', 1916
                           print*, '***** ERROR-DETAILS *****'
                           print*, 'Invalid Bounds Size!!'
                           print*, 'lo_dim1=',lo_dim1, 'hi_dim1=',hi_dim1, 'lo_dim2=',lo_dim2, 'hi_dim2=',hi_dim2, &
                                   'lo_dim3=',lo_dim3,'hi_dim3=', hi_dim3, 'lo_dim4=',lo_dim4, 'hi_dim4=',hi_dim4
                             ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE!!'
                     end if
                       
                     if(size(multiarray4D) /= size(flat_array1D)) then
                             call check_dim1Di32_eq(size(multiarray4D),size(flat_array1D),msg2,file_path,1930)
                     end if
                     
                      if(num_threads .LE. 0) then
                         call omp_setnum_threads(def_num_threads)
                        else
                         call omp_set_num_threads(num_threads)
                     end if
                     
                     d1 = size(multiarray4D,dim=1)
                     d2 = size(multiarray4D,dim=2)
                     d3 = size(multiarray4D,dim=3)
                     d4 = size(multiarray4D,dim=4)
                     if(present(tot_length)) then
                         if((d1*d2*d3*d4) .LE. HUGE(tot_length)) then
                             tot_length = d1*d2*d3*d4
                         else
                            print*, '*****FATAL-ERROR*****'
                            print*, FunName
                            print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                            print*, 'dimensions size , d1 * d2 * d3 * d4 = ', d1*d2*d3*d4
                            print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                            ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!' 
                         end if
                     end if
                     
!DIR$ IF (_OPENMP .GE. 201307)
                     if(copy_subarrays .EQ. .true. ) then
                         !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(l,k,j,i)
                         do l = lo_dim1,hi_dim1
                             do k = lo_dim2,hi_dim2
                                 do j = lo_dim3,hi_dim3
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                     
                                     do i = lo_dim4,hi_dim4
                                         flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                     end do
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     else
                          !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(i,k,j,i)
                         do l = 1,d1
                             do k = 1,d2
                                 do j = 1,d3
 !DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                    
                                      do i = 1,d4
                                          flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                      end do
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     end if
!DIR$ ELSE
                     if(copy_subarrays .EQ. .true.) then
                         ! Vectorize only
                         do l = lo_dim1,hi_dim1
                             do k = lo_dim2,hi_dim2
                                 do j = lo_dim3,hi_dim3
                                     !DIR$ SIMD
                                     !DIR$ UNROLL (4)
                                     do i = lo_dim4,hi_dim4
                                         flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                     end do
                                 end do
                             end do
                         end do
                     else
                         ! Vectorize only
                         do l = 1,d1
                             do k = 1,d2
                                 do j = 1,d3
                                     !DIR$ SIMD
                                     !DIR$ UNROLL (4)
                                     do i = 1,d4
                                         flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                     end do
                                 end do
                             end do
                         end do
                     end if
                     
!DIR$ ENDIF                                     
                                     
         end  subroutine
                                                    
         
         subroutine  i64_multiarray4D_to_flat1D(multiarray4D,flat_array1D,lo_dim1,hi_dim1,lo_dim2, &
                                        hi_dim2,lo_dim3,hi_dim3,lo_dim4,hi_dim4,tot_length,copy_subarrays,num_threads)
         
                     use omp_lib
                     implicit none
                     integer(i64), dimension(:,:,:,:), intent(in)            :: multiarray4D
                     !DIR$ ASSUME_ALIGNED multiarray4D:32
                     integer(i64), dimension(*),       intent(inout)         :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                     intent(in)            :: lo_dim1
                     integer(i32),                     intent(in)            :: hi_dim1
                     integer(i32),                     intent(in)            :: lo_dim2
                     integer(i32),                     intent(in)            :: hi_dim2
                     integer(i32),                     intent(in)            :: lo_dim3
                     integer(i32),                     intent(in)            :: hi_dim3
                     integer(i32),                     intent(in)            :: lo_dim4
                     integer(i32),                     intent(in)            :: hi_dim4
                     integer(i32),                     intent(out), optional :: tot_length
                     logical(i32),                     intent(in)            :: copy_subarrays
                     integer(i32),                     intent(in)            :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,k,l,d1,d2,d3,d4
                     character(len=*) FunName
                     parameter(FunName = 'subroutine: i64_multiarray4D_to_flat1D' )
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: i64_multiarray4D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: i64_multiarraya4D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements.
         
                     call check_alloc4DI64_fail(multiarray4D,msg1,file_path,2082)
                     
                     if((hi_dim1 .LE. lo_dim1) .OR. (hi_dim2 .LE. lo_dim2) .OR. &
                        (hi_dim3 .LE. lo_dim3) .OR. (hi_dim4 .LE. lo_dim4))     then
                         
                           print*, '***** FATAL-ERROR *****'
                           print*, 'In file: ', file_path
                           print*, 'In function: ', FunName
                           print*, 'At line: ', 2084
                           print*, '***** ERROR-DETAILS *****'
                           print*, 'Invalid Bounds Size!!'
                           print*, 'lo_dim1=',lo_dim1, 'hi_dim1=',hi_dim1, 'lo_dim2=',lo_dim2, 'hi_dim2=',hi_dim2, &
                                   'lo_dim3=',lo_dim3,'hi_dim3=', hi_dim3, 'lo_dim4=',lo_dim4, 'hi_dim4=',hi_dim4
                             ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE!!'
                     end if
                     
                     if(size(multiarray4D) /= size(flat_array1D)) then
                             call check_dim1Di32_eq(size(multiarray4D,size(flat_array1D),msg2,file_path,2098)
                     end if
                     
                     if(num_threads .LE. 0 ) then
                         call omp_set_num_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                     end if
                     
                     d1 = size(multiarray4D,dim=1)
                     d2 = size(multiarray4D,dim=2)
                     d3 = size(multiarray4D,dim=3)
                     d4 = size(multiarray4D,dim=4)
                     if(present(tot_length)) then
                         if((d1*d2*d3*d4) .LE. HUGE(tot_length)) then
                             tot_length = d1*d2*d3*d4
                         else
                             print*, '*****FATAL-ERROR*****'
                             print*, FunName
                             print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                             print*, 'dimensions size , d1 * d2 * d3 * d4 = ', d1*d2*d3*d4
                             print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                             ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!' 
                         end if
                     end if
                     
!DIR$  IF (_OPENMP .GE. 201307)
                     if(copy_subarrays .EQ. .true. ) then
                         !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(l,k,j,i)
                         do l = lo_dim1,hi_dim1
                             do k = lo_dim2,hi_dim2
                                 do j = lo_dim3,hi_dim3
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF               
                                     do i = lo_dim4,hi_dim4
                                         flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                     end do
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     else
                         !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(l,k,j,i)
                         do l = 1,d1
                             do k = 1,d2
                                 do j = 1,d3
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF               
                                     do i = 1,d4
                                         flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                     end do
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     end if

!DIR$ ELSE
                     if(copy_subarrays .EQ. .true. ) then
                         ! Vectorize only.
                         do l = lo_dim1,hi_dim1
                             do k = lo_dim2,hi_dim2
                                 do j = lo_dim3,hi_dim3
                                     !DIR$ SIMD
                                     !DIR$ UNROLL (4)
                                     do i = lo_dim4,hi_dim4
                                         flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                     end do
                                 end do
                             end do
                         end do
                     else
                         ! Vectorize only.
                         do l = 1,d1
                             do k = 1,d2
                                 do j = 1,d3
                                     !DIR$ SIMD
                                     !DIR$ UNROLL (4)
                                     do i = 1,d4
                                         flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                     end do
                                 end do
                             end do
                         end do
                     end if
                     
                         
                     
!DIR$ ENDIF
                     
         end  subroutine
                                            
                                            
         subroutine  l32_multiarray1D_to_flat1D(multiarray1D,flat_array1D,lo_dim1,hi_dim1, &
                                                        tot_length, copy_subarrays        )
         
                     implicit none
                     ! Fortan logical types are emulated by integer(kind=4) types of
                     ! values either 1 or 0
                     integer(i32), dimension(:), intent(in)            :: multiarray1D
                     !DIR$ ASSUME_ALIGNED multiarray1D:32
                     integer(i32), dimension(*), intent(inout)         :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),               intent(in)            :: lo_dim1
                     integer(i32),               intent(in)            :: hi_dim1
                     integer(i32),               intent(out), optional :: tot_length
                     logical(i32),               intent(in)            :: copy_subarrays
                     ! Local variables
                     integer(i32) :: i,d1
                     character(len=*) FunName
                     parameter(FunName = 'subroutine: l32_multiarray1D_to_flat1D')
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: l32_multiarray1D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: l32_multiarraya1D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements.
                     
                     call check_alloc1DI32_fail(multiarray1D,msg1,file_path,2218)
                     
                     
                    
                     
                     
                     if(hi_dim1 .LE. lo_dim1) then
                         print*, '***** FATAL-ERROR *****'
                         print*, 'In file: ', file_path
                         print*, 'In function: ', FunName
                         print*, 'At line: ', 2220
                         print*, '***** ERROR-DETAILS *****'
                         print*, 'Invalid Bounds Size!!'
                         print*, 'lo_dim1=', lo_dim1 , 'hi_dim1=', hi_dim1
                         ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE'
                     end if
                     
                     if(size(multiarray1D) /= size(flat_array1D)) then
                          call check_dim1Di32_eq(size(multiarray1D),size(flat_array1D),msg2,file_path,2231)
                     end if
                     
                     d1 = size(multiarray1D,dim=1)
                     if(present(tot_length)) then
                         if(d1 .LE. HUGE(tot_length)) then
                             tot_length = d1
                         else
                             print*, '*****FATAL-ERROR*****'
                             print*, FunName
                             print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                             print*, 'dimensions size , d1 = ', d1
                             print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                             ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     end if
                     
                     ! Check for existence of values which are not 0,or 1.
                     ! Upon occurrence of value > 1 assign 1
                     ! Upon occurrence of value < 0 assign 0
                     
                                      
                             !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD

                             !DIR$ UNROLL (4)                     
                     do i = 1,d1
                         if(multiarray1D(i) < 0 ) then
                             multiarray1D(i) = 0
                         else if(multiarray1D(i) > 1) then
                             multiarray1D(i) = 1
                         end if
                     end do
                     
                             
                     ! No OPENMP parallelization here in use.
                     ! Copy loop will be vectorized.
                     if(copy_subarrays .EQ. .true. ) then
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                          
                               do i = lo_dim1,hi_dim1
                                   flat_array1D(i) = multiarray1D(i)
                               end do
                     else
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                          
                               do i = 1,d1
                                   flat_array1D(i) = multiarray1D(i)
                               end do
                     end if
                     
                         
                         
                         
         end  subroutine
                                                        
                                             
         subroutine  l64_multiarray1D_to_flat1D(multiarray1D,flat_array1D,lo_dim1,hi_dim1, &
                                                   tot_length, copy_subarrays    )
        
                     implicit none
                     ! Fortan logical types are emulated by integer(kind=8) types of
                     ! values either 1 or 0
                     integer(i64), dimension(:), intent(in)            :: multiarray1D
                     !DIR$ ASSUME_ALIGNED multiarray1D:32
                     integer(i64), dimension(*), intent(inout)         :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),               intent(in)            :: lo_dim1
                     integer(i32),               intent(in)            :: hi_dim1
                     integer(i32),               intent(out), optional :: tot_length
                     logical(i32),               intent(in)            :: copy_subarrays
                     ! Local variables
                     integer(i32) :: i,d1
                     character(len=*) FunName
                     parameter(FunName = 'subroutine: l64_multiarray1D_to_flat1D' )
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: l64_multiarray1D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: l64_multiarray1D_to_flat1D' , &
                                                                 '***** ERROR-DETAILS ***** ' ,  &
                                                                 'Invalid Range of  Input Arguments!!' , &
                                                                 'size(multiarray2D) = ', &
                                                                 'flat_array1D length  = ', &
                                                                 '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements.
                     
                     call check_alloc1DI64_fail(multiarray1D,msg1,file_path,2318)
                     
                     if(hi_dim1 .LE. lo_dim1) then
                         print*, '***** FATAL-ERROR *****'
                         print*, 'In file: ', file_path
                         print*, 'In function: ', FunName
                         print*, 'At line: ', 2324
                         print*, '***** ERROR-DETAILS *****'
                         print*, 'Invalid Bounds Size!!'
                         print*, 'lo_dim1=', lo_dim1 , 'hi_dim1=', hi_dim1
                         ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE'
                     end if
                     
                     if(size(multiarray1D) /= size(flat_array1D)) then
                            call check_dim1Di32_eq(size(multiarray1D),size(flat_array1D),msg2,file_path,2331)
                     end if
                     
                     d1 = size(multiarray1D,dim=1)
                     if(present(tot_length)) then
                         if(d1 .LE. HUGE(tot_length)) then
                             tot_length = d1
                         else
                             print*, '*****FATAL-ERROR*****'
                             print*, FunName
                             print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                             print*, 'dimensions size , d1 = ', d1
                             print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                             ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     end if
                       
                     ! Check for existence of values which are not 0,or 1.
                     ! Upon occurrence of value > 1 assign 1
                     ! Upon occurrence of value < 0 assign 0
                     !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD

                             !DIR$ UNROLL (4) 
                             do i = 1,d1
                                 if(multiarray1D(i) < ZERO) then
                                     multiarray1D(i) = ZERO
                                 else if(multiarray1D(i) > ONE) then
                                     multiarray1D(i) = ONE
                                 end if
                             end do
                             
                                     
                      ! No OPENMP parallelization here in use.
                      ! Copy loop will be vectorized.
                      if(copy_subarrays .EQ. .true. ) then
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                          
                                do i = lo_dim1,hi_dim1
                                    flat_array1D(i) = multiarray1D(i)
                                end do
                      else
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                          
                                do i = 1,d1
                                    flat_array1D(i) = multiarray1D(i)
                                end do
                      end if
                      
                     
         end subroutine 
         
                                                   
         subroutine  l32_multiarray2D_to_flat1D(multiarray2D,flat_array1D,lo_dim1,hi_dim1,lo_dim2, &
                                                    hi_dim2,tot_length,copy_subarrays,num_threads   )
         
                     use omp_lib
                     implicit none
                     ! Fortan logical types are emulated by integer(kind=4) types of
                     ! values either 1 or 0
                     integer(i32), dimension(:,:), intent(in)               :: multiarray2D
                     !DIR$ ASSUME_ALIGNED multiarray2D:32
                     integer(i32), dimension(*),   intent(inout)            :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                 intent(in)               :: lo_dim1
                     integer(i32),                 intent(in)               :: hi_dim1
                     integer(i32),                 intent(in)               :: lo_dim2
                     integer(i32),                 intent(in)               :: hi_dim2
                     integer(i32),                 intent(out), optional    :: tot_length
                     logical(i32),                 intent(in)               :: copy_subarrays
                     integer(i32),                 intent(in)               :: num_threads
                     ! Local variables
                     integer :: i,j,d1,d2
                     character(len=*) FunName
                     parameter(FunName = 'subroutine: l32_multiarray2D_to_flat1D')
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: l32_multiarray2D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                      character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: l32_multiarray2D_to_flat1D' , &
                                                                   '***** ERROR-DETAILS ***** ' ,  &
                                                                   'Invalid Range of  Input Arguments!!' , &
                                                                   'size(multiarray2D) = ', &
                                                                   'flat_array1D length  = ', &
                                                                   '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements.
                    
                      call check_alloc2DI32_fail(multiarray2D,msg1,file_path,2468)
                      
                      if((hi_dim1 .LE. lo_dim1) .OR. (hi_dim2 .LE. lo_dim2)) then
                         print*, '***** FATAL-ERROR *****'
                         print*, 'In file: ', file_path
                         print*, 'In function: ', FunName
                         print*, 'At line: ', 2470
                         print*, '***** ERROR-DETAILS *****'
                         print*, 'Invalid Bounds Size!!'
                         print*, 'lo_dim1=', lo_dim1, 'hi_dim1=', hi_dim1, 'lo_dim2=',lo_dim2,'hi_dim2=',hi_dim2
                         ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE'
                      end if
                      
                      if(size(multiarray2D) /= size(flat_array1D)) then
                             call check_dim1Di32_eq(size(multiarray2D),size(flat_array1D),msg2,file_path,2481)
                      end if
                      
                      if(num_threads .LE. 0) then
                          call omp_set_num_threads(def_num_threads)
                      else
                          call omp_set_num_threads(num_threads)
                      end if
                      
                      
                      d1 = size(multiarray2D,dim=1)
                      d2 = size(multiarray2D,dim=2)
                      if(present(tot_length)) then
                          if((d1*d2) .LE. HUGE(tot_length)) then
                              tot_length = d1*d2
                          else
                             print*, '*****FATAL-ERROR*****'
                             print*, FunName
                             print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                             print*, 'dimensions size , d1 * d2 = ', d1*d2
                             print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                             ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                          end if
                      end if
                     ! Check for existence of values which are not 0,or 1.
                     ! Upon occurrence of value > 1 assign 1
                     ! Upon occurrence of value < 0 assign 0
                        
                             do j = 1,d1
                             !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)                                 
                                 do i = 1,d2
                                     if(multiarray2D(i,j) < 0) then
                                         multiarray2D(i,j) = 0
                                     else if(multiarray2D(i,j) > 1) then
                                         multiarray2D(i,j) = 1
                                     end if
                                 end do
                             end do
                             ! OpenMP parallelization and innermost
                             ! loop vectorization.
!DIR$ IF DEFINED (_OPENMP) .OR. DEFINED (USE_OPENMP) 
                             if(copy_subarrays .EQ. .true. ) then
                                !$OMP PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(i,j)
                                do j = lo_dim1,hi_dim1
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                     
                                    do i = lo_dim2,hi_dim2
                                        flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                                    end do
                                end do
                                !$OMP END PARALLEL DO
                             else
                                !$OMP PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(i,j)
                                 do j = 1,d1
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                      
                                     do i = 1,d2
                                         flat_array1D(j+d1*i) = multiarray2D(i,j)
                                     end do
                                 end do
                                 !$OMP END PARALLEL DO
                             end if
!DIR$ ELSE
                             if(copy_subarrays .EQ. .true. ) then
                                 ! Vectorize only
                                 do j = lo_dim1,hi_dim1
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                       
                                     do i = lo_dim2,hi_dim2
                                         flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                                     end do
                                 end do
                             else
                                 ! Vectorize only
                                 do j = 1,d1
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                    
                                     do i = 1,d2
                                         flat_array1D(j+d1*i) = multiarray2D(i,j)
                                     end do
                                 end do
                             end if
                             
!DIR$ ENDIF
                      
         end  subroutine
         
                                                    
         subroutine  l64_multiarray2D_to_flat1D(multiarray2D,flat_array1D,lo_dim1,hi_dim1,lo_dim2, &
                                                  hi_dim2,tot_length,copy_subarrays,num_threads       )
         
                     use omp_lib
                     implicit none
                     ! Fortan logical types are emulated by integer(kind=8) types of
                     ! values either 1 or 0
                     integer(i64), dimension(:,:), intent(in)            :: multiarray2D
                     !DIR$ ASSUME_ALIGNED multiarray2D:32
                     integer(i64), dimension(*),   intent(inout)         :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                 intent(in)            :: lo_dim1
                     integer(i32),                 intent(in)            :: hi_dim1
                     integer(i32),                 intent(in)            :: lo_dim2
                     integer(i32),                 intent(in)            :: hi_dim2
                     integer(i32),                 intent(out), optional :: tot_length
                     logical(i32),                 intent(in)            :: copy_subarrays
                     integer(i32),                 intent(in)            :: num_threads
                     ! Local   variables
                     integer(i32) :: i,j,d1,d2
                     character(len=*) FunName
                     parameter(FunName = 'subroutine: l64_multiarray2D_to_flat1D')
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: l64_multiarray2D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: l64_multiarray2D_to_flat1D' , &
                                                                   '***** ERROR-DETAILS ***** ' ,  &
                                                                   'Invalid Range of  Input Arguments!!' , &
                                                                   'size(multiarray2D) = ', &
                                                                   'flat_array1D length  = ', &
                                                                   '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements.
                     
                     call check_alloc2DI64_fail(multiarray2D,msg1,file_path,2640)
                     
                     if((hi_dim1 .LE. lo_dim1) .OR. (hi_dim1 .LE. lo_dim1)) then
                         print*, '***** FATAL-ERROR *****'
                         print*, 'In file: ', file_path
                         print*, 'In function: ', FunName
                         print*, 'At line: ', 2642
                         print*, '***** ERROR-DETAILS *****'
                         print*, 'Invalid Bounds Size!!'
                         print*, 'lo_dim1=', lo_dim1, 'hi_dim1=', hi_dim1, 'lo_dim2=',lo_dim2,'hi_dim2=',hi_dim2
                         ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE'
                     end if
                     
                     if(size(multiarray2D) /= size(flat_array1D)) then
                        call check_dim1Di32_eq(size(multiarray2D),size(flat_array1D),msg2,file_path,2653) 
                     end if
                     
                    if(num_threads .LE. 0) then
                         call omp_set_num_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                    end if
                    
                    d1 = size(multiarray2D,dim=1)
                    d2 = size(multiarray2D,dim=2)
                    if(present(tot_length)) then
                        if((d1*d2) .LE. HUGE(tot_length)) then
                            tot_length = d1*d2
                        else
                             print*, '*****FATAL-ERROR*****'
                             print*, FunName
                             print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                             print*, 'dimensions size , d1 * d2 = ', d1*d2
                             print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                             ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                        end if
                    end if
                     ! Check for existence of values which are not 0,or 1.
                     ! Upon occurrence of value > 1 assign 1
                     ! Upon occurrence of value < 0 assign 0
                     do j = 1,d1
                     !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)                         
                          do i = 1,d2
                              if(multiarray2D(i,j) > ONE) then
                                  multiarray2D(i,j) = ONE
                              else if(multiarray2D(i,j) < ZERO) then
                                  multiarray2D(i,j) = ZERO
                              end if
                          end  do
                     end do
                     ! OpenMP parallelization and innermost
                     ! loop vectorization.
!DIR$ IF (_OPENMP .GE. 201307)
                     if(copy_subarrays .EQ. .true. ) then
                         !$OMP PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(j,i)
                         do j = lo_dim1,hi_dim1
 !DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                             
                               do i = lo_dim2,hi_dim2
                                   flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                               end do
                         end do
                         !$OMP END PARALLEL DO
                     else
                         !$OMP PARALLEL DO SHARED(multiarray2D,flat_array1D), PRIVATE(j,i)
                         do j = 1,d1
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF 
                             do i = 1,d2
                                 flat_array1D(j+d1*i) = multiarray2D(i,j)
                             end do
                         end do
                         !$OMP PARALLEL DO
                     end if
!DIR$ ELSE
                     if(copy_subarrays .EQ. .true.) then
                         ! Vectorize only
                         do j = lo_dim1,hi_dim1
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                               
                               do i = lo_dim2,hi_dim2
                                   flat_array1D(j+hi_dim1*i) = multiarray2D(i,j)
                               end do
                         end do
                     else
                         ! Vectorize only
                         do j = 1,d1
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF  
                             do i = 1,d2
                                 flat_array1D(j+d1*i) = multiarray2D(i,j)
                             end do
                         end do
                     end if
                     
!DIR$ ENDIF
                                                  
                     
                     
         end  subroutine
         
                                                  
         subroutine  l32_multiarray3D_to_flat1D(multiarray3D,flat_array1D,lo_dim1,hi_dim1,lo_dim2, &
                                                hi_dim2,lo_dim3,hi_dim3,tot_length,copy_subarrays,num_threads)
         
                     use omp_lib
                     implicit none
                     integer(i32), dimension(:,:,:), intent(in)            :: multiarray3D
                     !DIR$ ASSUME_ALIGNED multiarray3D:32
                     integer(i32), dimension(*),     intent(inout)         :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                   intent(in)            :: lo_dim1
                     integer(i32),                   intent(in)            :: hi_dim1
                     integer(i32),                   intent(in)            :: lo_dim2
                     integer(i32),                   intent(in)            :: hi_dim2
                     integer(i32),                   intent(in)            :: lo_dim3
                     integer(i32),                   intent(in)            :: hi_dim3
                     integer(i32),                   intent(out), optional :: tot_length
                     logical(i32),                   intent(in)            :: copy_subarrays
                     integer(i32),                   intent(in)            :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,k,d1,d2,d3
                     character(len=*) FunName
                     parameter(FunName = 'subroutine: l32_multiarray3D_to_flat1D' )
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: l32_multiarray3D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: l32_multiarray3D_to_flat1D' , &
                                                                   '***** ERROR-DETAILS ***** ' ,  &
                                                                   'Invalid Range of  Input Arguments!!' , &
                                                                   'size(multiarray2D) = ', &
                                                                   'flat_array1D length  = ', &
                                                                   '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements.
         
                     call check_alloc3DI64_fail(multiarray3D,msg1,file_path,2812)
                     
                     if((hi_dim1 .LE. lo_dim1) .OR. (hi_dim2 .LE. lo_dim2) .OR. (hi_dim3 .LE. lo_dim3)) then
                         print*, '***** FATAL-ERROR *****'
                         print*, 'In file: ', file_path
                         print*, 'In function: ', FunName
                         print*, 'At line: ', 2814
                         print*, '***** ERROR-DETAILS *****'
                         print*, 'Invalid Bounds Size!!'
                         print*, 'lo_dim1=', lo_dim1, 'hi_dim1=', hi_dim1, 'lo_dim2=',lo_dim2,'hi_dim2=',hi_dim2, &
                                  'lo_dim3=',lo_dim3, 'hi_dim3=',hi_dim3
                         ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE'
                     end if
                     
                     if(size(multiarray3D) /= size(flat_array1D)) then
                             call check_dim1Di32_eq(size(multiarray3D),size(flat_array1D),msg2,file_path,2826)
                     end if
                     
                     if(num_threads .LE. 0) then
                         call omp_set_num_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                     end if
                     
                     d1 = size(multiarray3D,dim=1)
                     d2 = size(multiarray3D,dim=2)
                     d3 = size(multiarray3D,dim=3)
                     if(present(tot_length)) then
                         if((d1*d2*d3) .LE. HUGE(tot_length)) then
                             tot_length = d1*d2*d3
                         else
                             print*, '*****FATAL-ERROR*****'
                             print*, FunName
                             print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                             print*, 'dimensions size , d1 * d2 * d3 = ', d1*d2*d3
                             print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                             ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     end if
                     ! Check for existence of values which are not 0,or 1.
                     ! Upon occurrence of value > 1 assign 1
                     ! Upon occurrence of value < 0 assign 0
!DIR$ IF (_OPENMP .GE. 201307)
                     !$OMP PARALLEL DO SHARED(multiarray1D), PRIVATE(k,j,i)
                     do k = 1,d1
                         do j = 1,d2
                             !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)  
                             do i = 1,d3
                                 if(multiarray3D(i,j,k) > 1) then
                                     multiarray3D(i,j,k) = 1
                                 else if(multiarray3D(i,j,k) < 0) then
                                     multiarray3D(i,j,k) = 0
                                 end if
                             end do
                         end do
                     end do
                     !$OMP END PARALLEL DO
!DIR$ ELSE
                     ! Vectorize only
                      do k = 1,d1
                         do j = 1,d2
                             !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)  
                             do i = 1,d3
                                 if(multiarray3D(i,j,k) > 1) then
                                     multiarray3D(i,j,k) = 1
                                 else if(multiarray3D(i,j,k) < 0) then
                                     multiarray3D(i,j,k) = 0
                                 end if
                             end do
                         end do
                     end do
!DIR$ ENDIF
                     ! OpenMP parallelization and innermost
                     ! loop vectorization.
!DIR$ IF DEFINED (_OPENMP) .OR. DEFINED (USE_OPENMP)
                      if(copy_subarrays .EQ. .true.) then
                          !$OMP PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                          do k = lo_dim1,hi_dim1
                              do j = lo_dim2,hi_dim2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                      
                                  do i = lo_dim3,hi_dim3
                                      flat_array1D(k+hi_dim1*j+hi_dim2*i) = multiarray3D(i,j,k)
                                  end do
                              end do
                          end do
                          !$OMP END PARALLEL DO
                      else
                          ! OpenMP parallelization and innermost
                          ! loop vectorization. 
                          !$OMP PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                          do k = 1,d1
                              do j = 1,d2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                   
                               do i = 1,d3
                                   flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                               end do
                              end do
                          end do
                          !$OMP END PARALLEL DO
                      end if
!DIR$ ELSE
                      if(copy_subarrays .EQ. .true. ) then
                          ! Vectorize only
                          do k = lo_dim1,hi_dim1
                              do j = lo_dim2,hi_dim2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF
                                  do i = lo_dim3,hi_dim3
                                       flat_array1D(k+hi_dim1*j+hi_dim2*i) = multiarray3D(i,j,k)
                                  end do
                              end do
                          end do
                      else
                          ! Vectorize only
                          do k = 1,d1
                              do j = 1,d2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF
                                  do i = 1,d3
                                      flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                                  end do
                              end do
                          end do
                      end if
                      
!DIR$ ENDIF
                     
         end  subroutine
                                                  
                                                  
         subroutine  l64_multiarray3D_to_flat1D(multiarray3D,        &
                                                flat_array1D,        &
                                                     lo_dim1,        &
                                                     hi_dim1,        &
                                                     lo_dim2,        &
                                                     hi_dim2,        &
                                                     lo_dim3,        &
                                                     hi_dim3,        &
                                                     tot_length,     &
                                                     copy_subarrays, &
                                                     num_threads )
         
                     use omp_lib
                     implicit none
                     integer(i64), dimension(:,:,:), intent(in)            :: multiarray3D
                     !DIR$ ASSUME_ALIGNED multiarray3D:32
                     integer(i64), dimension(*),     intent(inout)         :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                   intent(in)            :: lo_dim1
                     integer(i32),                   intent(in)            :: hi_dim1
                     integer(i32),                   intent(in)            :: lo_dim2
                     integer(i32),                   intent(in)            :: hi_dim2
                     integer(i32),                   intent(in)            :: lo_dim3
                     integer(i32),                   intent(in)            :: hi_dim3
                     integer(i32),                   intent(out), optional :: tot_length
                     logical(i32),                   intent(in)            :: copy_subarrays
                     integer(i32),                   intent(in)            :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,k,d1,d2,d3
                     character(len=*) FunName
                     parameter(FunName = 'subroutine: l64_multiarray3D_to_flat1D')
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: l64_multiarray3D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: l64_multiarray3D_to_flat1D', &
                                                                   '***** ERROR-DETAILS ***** ' ,  &
                                                                   'Invalid Range of  Input Arguments!!' , &
                                                                   'size(multiarray2D) = ', &
                                                                   'flat_array1D length  = ', &
                                                                   '***** ERROR-DETAILS *****'   ]
                     ! Start of executable statements.
                     
                     call check_alloc3DI64_fail(multiarray3D,msg1,file_path,2999)
                     
                     if((hi_dim1 .LE. lo_dim1) .OR. (hi_dim2 .LE. lo_dim2) .OR. (hi_dim3 .LE. lo_dim3)) then
                         print*, '***** FATAL-ERROR *****'
                         print*, 'In file: ', file_path
                         print*, 'In function: ', FunName
                         print*, 'At line: ', 3001
                         print*, '***** ERROR-DETAILS *****'
                         print*, 'Invalid Bounds Size!!'
                         print*, 'lo_dim1=', lo_dim1, 'hi_dim1=', hi_dim1, 'lo_dim2=',lo_dim2,'hi_dim2=',hi_dim2, &
                                 'lo_dim3=',lo_dim3, 'hi_dim3=',hi_dim3
                         ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE'
                     end if
                     
                     if(size(multiarray3D) /= size(flat_array1D)) then
                           call check_dim1Di32_eq(size(multiarray3D),size(flat_array1D),msg2,file_path,3013)
                     end if
                     
                     if(num_threads .LE. 0) then
                         call omp_set_num_threads(def_num_threads)
                     else
                         call omp_set_num_threads(num_threads)
                     end if
                     
                     d1 = size(multiarray3D,dim=1)
                     d2 = size(multiarray3D,dim=2)
                     d3 = size(multiarray3D,dim=3)
                     if(present(tot_length)) then
                         if((d1*d2*d3) .LE. HUGE(tot_length)) then
                             tot_length = d1*d2*d3
                         else
                             print*, '*****FATAL-ERROR*****'
                             print*, FunName
                             print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                             print*, 'dimensions size , d1 * d2 * d3 = ', d1*d2*d3
                             print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                             ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                         end if
                     end if
                     ! Check for existence of values which are not 0,or 1.
                     ! Upon occurrence of value > 1 assign 1
                     ! Upon occurrence of value < 0 assign 0
!DIR$ IF (_OPENMP .GE. 201307)
                     !$OMP PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                     do k = 1,d1
                         do j = 1,d2
                             !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)                              
                              do i = 1,d3
                                  if(multiarray3D(i,j,k) > ONE) then
                                      multiarray3D(i,j,k) = ONE
                                  else if(multiarray3D(i,j,k) < ZERO) then
                                      multiarray3D(i,j,k) = ZERO
                                  end if
                              end do
                         end do
                     end do
                     !$OMP END PARALLEL DO
!DIR$ ELSE
                    ! Vectorize only
                     do k = 1,d1
                         do j = 1,d2
                             !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)                              
                              do i = 1,d3
                                  if(multiarray3D(i,j,k) > ONE) then
                                      multiarray3D(i,j,k) = ONE
                                  else if(multiarray3D(i,j,k) < ZERO) then
                                      multiarray3D(i,j,k) = ZERO
                                  end if
                              end do
                         end do
                     end do 
                     
!DIR$ ENDIF
                     ! OpenMP parallelization and innermost
                     ! loop vectorization.
!DIR$ IF (_OPENMP .GE. 201307)
                     if(copy_subarrays .EQ. .true. ) then
                        !$OMP PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                         do k = lo_dim1,hi_dim1
                             do j = lo_dim2,hi_dim2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF       
                                 do i = lo_dim3,hi_dim3
                                     flat_array1D(k+hi_dim1*j+hi_dim2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     else
                         !$OMP PARALLEL DO SHARED(multiarray3D,flat_array1D), PRIVATE(k,j,i)
                         do k = 1,d1
                             do j = 1,d2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF 
                                 do i = 1,d3
                                     flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                         !$OMP END PARALLEL DO
                     end if
!DIR$ ELSE
                     if(copy_subarrays .EQ. .true. ) then
                         ! Vectorize only.
                         do k = lo_dim1,hi_dim1
                             do j = lo_dim2,hi_dim2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF
                                 do i = lo_dim3,hi_dim3
                                     flat_array1D(k+hi_dim1*j+hi_dim2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                     else
                         ! Vectorize only
                         do k = 1,d1
                             do j = 1,d2
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF
                                 do i = 1,d3
                                     flat_array1D(k+d1*j+d2*i) = multiarray3D(i,j,k)
                                 end do
                             end do
                         end do
                     end if
                     
                
                     
!DIR$ ENDIF                    
                
         end  subroutine
         
         
         subroutine  l32_multiarray4D_to_flat1D(multiarray4D, &  ! defferd shape array an argument
                                                flat_array1D, &  ! assumed size array
                                                     lo_dim1, &
                                                     hi_dim1, &
                                                     lo_dim2, &
                                                     hi_dim2, &
                                                     lo_dim3, &
                                                     hi_dim3, &
                                                     lo_dim4, &
                                                     hi_dim4, &
                                                  tot_length, &
                                              copy_subarrays, &
                                                 num_threads   )
         
                use omp_lib
                implicit none
                integer(i32), dimension(:,:,:,:), intent(in)            :: multiarray4D
                !DIR$ ASSUME_ALIGNED multiarray4D:32
                integer(i32), dimension(*),       intent(inout)         :: flat_array1D
                !DIR$ ASSUME_ALIGNED flat_array1D:32
                integer(i32),                     intent(in)            :: lo_dim1
                integer(i32),                     intent(in)            :: hi_dim1
                integer(i32),                     intent(in)            :: lo_dim2
                integer(i32),                     intent(in)            :: hi_dim2
                integer(i32),                     intent(in)            :: lo_dim3
                integer(i32),                     intent(in)            :: hi_dim3
                integer(i32),                     intent(in)            :: lo_dim4
                integer(i32),                     intent(in)            :: hi_dim4
                integer(i32),                     intent(out), optional :: tot_length
                logical(i32),                     intent(in)            :: copy_subarrays
                integer(i32),                     intent(in)            :: num_threads
                ! Local variables
                integer(i32) :: i,j,k,l,d1,d2,d3,d4
                character(len=*) FunName
                parameter(FunName = 'subroutine: l32_multiarray4D_to_flat1D')
                character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: l32_multiarray4D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: l32_multiarray4D_to_flat1D', &
                                                                   '***** ERROR-DETAILS ***** ' ,  &
                                                                   'Invalid Range of  Input Arguments!!' , &
                                                                   'size(multiarray2D) = ', &
                                                                   'flat_array1D length  = ', &
                                                                   '***** ERROR-DETAILS *****'   ]
               ! Start of executable statements.
               
                call check_alloc4DI32_fail(multiarray4D,msg1,file_path,3211)
                
                if((hi_dim1 .LE. lo_dim1) .OR. (hi_dim2 .LE. lo_dim2) .OR. &
                   (hi_dim3 .LE. lo_dim3) .OR. (hi_dim4 .LE. lo_dim4)      )      then
                     print*, '***** FATAL-ERROR *****'
                     print*, 'In file: ', file_path
                     print*, 'In function: ', FunName
                     print*, 'At line: ', 3213
                     print*, '***** ERROR-DETAILS *****'
                     print*, 'Invalid Bounds Size!!'
                     print*, 'lo_dim1=', lo_dim1, 'hi_dim1=', hi_dim1, 'lo_dim2=',lo_dim2,'hi_dim2=',hi_dim2, &
                             'lo_dim3=',lo_dim3, 'hi_dim3=',  hi_dim3, 'lo_dim4=',lo_dim4,'hi_dim4=',hi_dim4
                     ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE'
                end if
                
                if(size(multiarray4D) /= size(flat_array1D)) then
                        call check_dim1Di32_eq(size(multiarray4D),size(flat_array1D),msg2,file_path,3226)
                end if
                
                if(num_threads .LE. 0) then
                    call omp_set_num_threads(def_num_threads)
                else
                    call omp_set_num_threads(num_threads)
                end if
                
                d1 = size(multiarray4D,dim=1)
                d2 = size(multiarray4D,dim=2)
                d3 = size(multiarray4D,dim=3)
                d4 = size(multiarray4D,dim=4)
                if(present(tot_length)) then
                    if((d1*d2*d3*d4) .LE. HUGE(tot_length)) then
                        tot_length = d1*d2*d3*d4
                    else
                         print*, '*****FATAL-ERROR*****'
                         print*, FunName
                         print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                         print*, 'dimensions size , d1 * d2 * d3 *d4 = ', d1*d2*d3*d4
                         print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                         ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                    end if
                end if
                ! Check for existence of values which are not 0,or 1.
                ! Upon occurrence of value > 1 assign 1
                ! Upon occurrence of value < 0 assign 0
!DIR$ IF (_OPENMP .GE. 201307)
                !$OMP PARALLEL DO SHARED(multiarray4D), PRIVATE(l,k,j,i)
                do l = 1,d1
                    do k = 1,d2
                        do j = 1,d3
                             !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)      
                             do i = 1,d4
                                 if(multiarray4D(i,j,k,l) > 1) then
                                     multiarray4D(i,j,k,l) = 1
                                 else if(multiarray4D(i,j,k,l) < 0) then
                                     multiarray4D(i,j,k,l)
                                 end if
                             end do
                        end do
                    end do
                end do
                !$OMP END PARALLEL DO
!DIR$ ELSE
                ! Vectorize only.
                 do l = 1,d1
                    do k = 1,d2
                        do j = 1,d3
                             !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)      
                             do i = 1,d4
                                 if(multiarray4D(i,j,k,l) > 1) then
                                     multiarray4D(i,j,k,l) = 1
                                 else if(multiarray4D(i,j,k,l) < 0) then
                                     multiarray4D(i,j,k,l) = 0
                                 end if
                             end do
                        end do
                    end do
                 end do
!DIR$ ENDIF
         
!DIR$ IF (_OPENMP .GE. 201307)
                 if(copy_subarrays .EQ. .true. ) then
                     !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(l,k,j,i)
                     do l = lo_dim1,hi_dim1
                         do k = lo_dim2,hi_dim2
                             do j = lo_dim3,hi_dim3
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF    
                                do i = lo_dim4,hi_dim4
                                    flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                end do
                             end do
                         end do
                     end do
                     !$OMP END PARALLEL DO
                 else
                     !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(l,k,j,i)
                     do l = 1,d1
                         do k = 1,d2
                             do j = 1,d3
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                 
                                   do i = 1,d4
                                      flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                   end do
                             end do
                         end do
                     end do
                     !$OMP END PARALLEL DO
                 end if
!DIR$
                 if(copy_subarrays .EQ. .true. ) then
                     ! Vectorize only
                     do l = lo_dim1,hi_dim1
                         do k = lo_dim2,hi_dim2
                             do j = lo_dim3,hi_dim3
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                   
                                   do i = lo_dim4,hi_dim4
                                       flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                   end do
                             end do
                         end do
                     end do
                 else
                     ! Vectorize only
                     do l = 1,d1
                         do k = 1,d2
                             do j = 1,d3
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF  
                                 do i = 1,d4
                                     flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                 end do
                             end do
                         end do
                     end do
                 end if
                 
                     
!DIR$ ENDIF
                 
         end  subroutine
                                                
                                                
         subroutine  l64_multiarray4D_to_flat1D(multiarray4D, &
                                                flat_array1D, &
                                                    lo_dim1,  &
                                                    hi_dim1,  &
                                                    lo_dim2,  &
                                                    hi_dim2,  &
                                                    lo_dim3,  &
                                                    hi_dim3,  &
                                                    lo_dim4,  &
                                                    hi_dim4,  &
                                                 tot_length,  &
                                              copy_subarrays, &
                                                 num_threads   )
         
                     use omp_lib
                     implicit none
                     integer(i64), dimension(:,:,:,:), intent(in)            :: multiarray4D
                     !DIR$ ASSUME_ALIGNED multiarray4D:32
                     integer(i64), dimension(*),       intent(inout)         :: flat_array1D
                     !DIR$ ASSUME_ALIGNED flat_array1D:32
                     integer(i32),                     intent(in)            :: lo_dim1
                     integer(i32),                     intent(in)            :: hi_dim1
                     integer(i32),                     intent(in)            :: lo_dim2
                     integer(i32),                     intent(in)            :: hi_dim2
                     integer(i32),                     intent(in)            :: lo_dim3
                     integer(i32),                     intent(in)            :: hi_dim3
                     integer(i32),                     intent(in)            :: lo_dim4
                     integer(i32),                     intent(in)            :: hi_dim4
                     integer(i32),                     intent(out), optional :: tot_length
                     logical(i32),                     intent(in)            :: copy_subarrays
                     integer(i32),                     intent(in)            :: num_threads
                     ! Local variables
                     integer(i32) :: i,j,k,l,d1,d2,d3,d4
                     character(len=*) FunName
                     parameter(FunName = 'subroutine: l64_multiarray4D_to_flat1D')
                     character(len=80), dimension(6) :: msg1 = [  '***** FATAL-ERROR ***** in subroutine: l64_multiarray4D_to_flat1D', &
                                                                  'Non-allocated allocatable array(s) passed!!', &
                                                                  '***** ERROR-DETAILS *****'  , &
                                                                  'Allocation status: '  , &
                                                                  '***** ERROR-DETAILS *****' , &
                                                                  'TERMINATING EXECUTION WITH STOP STATEMENT' ]
                     
                     character(len=80), dimension(6) :: msg2 = [  '***** FATAL-ERROR ***** in subroutine: l64_multiarray4D_to_flat1D', &
                                                                   '***** ERROR-DETAILS ***** ' ,  &
                                                                   'Invalid Range of  Input Arguments!!' , &
                                                                   'size(multiarray2D) = ', &
                                                                   'flat_array1D length  = ', &
                                                                   '***** ERROR-DETAILS *****'   ]
               ! Start of executable statements.
               
               call check_alloc4DI64_fail(multiarray4D,msg1,file_path,3483)
               
               if((hi_dim1 .LE. lo_dim1) .OR. (hi_dim2 .LE. lo_dim2) .OR. &
                  (hi_dim3 .LE. lo_dim3) .OR. (hi_dim4 .LE. lo_dim4)    ) then
                     print*, '***** FATAL-ERROR *****'
                     print*, 'In file: ', file_path
                     print*, 'In function: ', FunName
                     print*, 'At line: ', 3485
                     print*, '***** ERROR-DETAILS *****'
                     print*, 'Invalid Bounds Size!!'
                     print*, 'lo_dim1=', lo_dim1, 'hi_dim1=', hi_dim1, 'lo_dim2=',lo_dim2,'hi_dim2=',hi_dim2, &
                             'lo_dim3=',lo_dim3, 'hi_dim3=',  hi_dim3, 'lo_dim4=',lo_dim4,'hi_dim4=',hi_dim4
                     ERROR STOP 'TERMINATING PROGRAM -- INVALID BOUNDS SIZE'
                  end if
                  
                  if(size(multiarray4D) /= size(flat_array1D)) then
                          call check_dim1Di32_eq(size(multiarray4D),size(flat_array1D),msg2,file_path,3498)
                  end if
                  
                  if(num_threads .LE. 0) then
                      call omp_set_num_threads(def_num_threads)
                  else
                      call omp_set_num_threads(num_threads)
                  end if
                  
                  d1 = size(multiarray4D,dim=1)
                  d2 = size(multiarray4D,dim=2)
                  d3 = size(multiarray4D,dim=3)
                  d4 = size(multiarray4D,dim=4)
                  if(present(tot_length)) then
                      if((d1*d2*d3*d4) .LE. HUGE(tot_length)) then
                          tot_length = d1*d2d*d3*d4
                      else
                         print*, '*****FATAL-ERROR*****'
                         print*, FunName
                         print*, 'HUGE(integer(kind=4)) -- EXCEEDED!!'
                         print*, 'dimensions size , d1 * d2 * d3 *d4 = ', d1*d2*d3*d4
                         print*, 'HUGE(integer(kind=4)) = ', HUGE(tot_length)
                         ERROR STOP 'TERMINATING EXECUTION - FATAL CONDITION!!'
                      end if
                  end if
                ! Check for existence of values which are not 0,or 1.
                ! Upon occurrence of value > 1 assign 1
                ! Upon occurrence of value < 0 assign 0
!DIR$ IF (_OPENMP .GE. 201307)
                   !$OMP PARALLEL DO SHARED(multiarray4D), PRIVATE(l,k,j,i)
                  do l = 1,d1
                      do k = 1,d2
                          do j = 1,d3
                             !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)                                
                               do i = 1,d4
                                   if(multiarray4D(i,j,k,l) > ONE) then
                                       multiarray4D(i,j,k,l) = ONE
                                   else if(multiarray4D(i,j,k,l) < ZERO) then
                                       multiarray4D(i,j,k,l) = ZERO
                                   end if
                               end do
                          end do
                      end do
                  end do
                  !$OMP END PARALLEL DO
!DIR$ ELSE
                  ! Vectorize only.
                   do l = 1,d1
                      do k = 1,d2
                          do j = 1,d3
                             !DIR$ IVDEP
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
                             !DIR$ UNROLL (4)                                
                               do i = 1,d4
                                   if(multiarray4D(i,j,k,l) > ONE) then
                                       multiarray4D(i,j,k,l) = ONE
                                   else if(multiarray4D(i,j,k,l) < ZERO) then
                                       multiarray4D(i,j,k,l) = ZERO
                                   end if
                               end do
                          end do
                      end do
                   end do
                  
!DIR$ ENDIF
                   
!DIR$ IF (_OPENMP .GE. 201307)                   
                  if(copy_subarrays .EQ. .true. ) then
                     !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(l,k,j,i)
                      do l = lo_dim1,hi_dim1
                          do k = lo_dim2,hi_dim2
                              do j = lo_dim3,hi_dim3
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                    
                                  do i = lo_dim4,hi_dim4
                                      flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                  end do
                              end do
                          end do
                      end do
                      !$OMP END PARALLEL DO
                  else
                      !$OMP PARALLEL DO SHARED(multiarray4D,flat_array1D), PRIVATE(l,k,j,i)
                      do l = 1,d1
                          do k = 1,d2
                              do j = 1,d3
 !DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF
                                  do i = 1,d4
                                      flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                  end do
                              end do
                          end do
                      end do
                      !$OMP END PARALLEL DO
                  end if
!DIR$ ELSE
                  if(copy_subarrays .EQ. .true. ) then
                      ! Vectorize only
                      do l = lo_dim1,hi_dim1
                          do k = lo_dim2,hi_dim2
                              do j = lo_dim3,hi_dim3
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF                                    
                  

                                  do i = lo_dim4,hi_dim4
                                      flat_array1D(l+hi_dim1*k+hi_dim2*j+hi_dim3*i) = multiarray4D(i,j,k,l)
                                  end do
                              end do
                          end do
                      end do
                  else
                      ! Vectorize only
                      do l = 1,d1
                          do k = 1,d2
                              do j = 1,d3
!DIR$ IF (USE_SOFT_PREFETCH .EQ. 1)
                             !DIR$ PREFETCH *:0:4
!DIR$ ENDIF                             
                             !DIR$ SIMD
!DIR$ IF (UNROLL_16 .EQ. 1)                             
                             !DIR$ UNROLL (16)
!DIR$ ELSE
                             !DIR$ UNROLL (4)
!DIR$ ENDIF  
                                  do i = 1,d4
                                      flat_array1D(l+d1*k+d2*j+d3*i) = multiarray4D(i,j,k,l)
                                  end do
                              end do
                          end do
                      end do
                  end if
!DIR$ ENDIF                  
                   
         end  subroutine 
             
             
             
             
end  module