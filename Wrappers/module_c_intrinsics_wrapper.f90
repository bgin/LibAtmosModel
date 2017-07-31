
module  module_c_intrinsics_wrapper

 !-----------------------------------------------------------------------------------85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'module_c_intrinsics_wrapper'
 !          
 !          Purpose:
 !                          This module declares Fortran interface or wrappers
 !                          to Intel C Intrinsic functions.
 !                          This module will be callable from higher abstraction
 !                          layer dispatch code.
 !                          
 !          History:
 !                      Date: 04-02-2017
 !                      Time: 17:21 GMT+2
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

  

 use module_kinds
 
 ! Module versioning information. 
 
 ! Module/File major version
 
 integer(i32), parameter, public :: module_c_intrinsics_wrapper_major = 1
 
 ! Module/File minor version
 
 integer(i32), parameter, public :: module_c_intrinsics_wrapper_minor = 0
 
 ! Module/File micro(patch) version
 
 integer(i32), parameter, public :: module_c_intrinsics_wrapper_micro = 0
 
 ! Module/File full version.
 
 integer(i32), parameter, public :: module_c_intrinsics_wrapper_version = 1000*module_c_intrinsics_wrapper_major+100*module_c_intrinsics_wrapper_minor + &
                                                                          10*module_c_intrinsics_wrapper_micro
 
 ! Date of build should be set after successfull build.
 
 character(len=*) BuildDate
 parameter(BuildDate = '')
 
 ! Module/File name
 
 character(len=*) ModuleName
 parameter(ModuleName = 'module_c_intrinsics_wrapper')
 
 ! Creation Date
 
 character(len=*) CreationDate
 parameter(CreationDate = '05-02-2017 17:21 +00200 (Sat, 5 Feb 2017 GMT+2)')
 
 ! Author
 
 character(Len=*) Author
 parameter(Author = 'Programmer: Bernard Gingold , contact: beniekg@gmail.com')
 
 public :: BuildDate,ModuleName,CreationDate,Author
 
 
  !===================================================56
  !  Fortran interface to Intel C Intrinsic functions
  !  Load-Store intrinsics interfaces.
  !===================================================56
 
    interface
    
         subroutine  uload_store_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='uload_store_avx256_ps')
            !**************************************************65
            ! Unaligned load-store wrapper for C intrinscis
            ! _mm256_loadu_ps and _mm256_storeu_ps
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*), intent(in)           :: src      ! source array
            integer(C_INT),              intent(in), value    :: src_len  ! length of source array
            real(C_FLOAT), dimension(*), intent(inout)        :: dst      ! destination array
            integer(C_INT),              intent(in), value    :: dst_len  ! length of destination array
         
         end  subroutine
         
    end  interface   
    
    
    interface
    
         subroutine  uload_store_avx256_ps2d(src,dst,dim1,dim2)    BIND(C, NAME='uload_store_avx256_ps2d')
            !**************************************************65
            ! Unaligned load-store wrapper for C intrinscis
            ! _mm256_loadu_ps and _mm256_storeu_ps.
            ! Load-store operates on linearized arrays 2D.
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),   intent(in)       :: src
            real(C_FLOAT),  dimension(*),   intent(inout)    :: dst
            integer(C_INT),                 intent(in),value :: dim1
            integer(C_INT),                 intent(in),value :: dim2
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  uload_store_avx256_ps3d(src,dst,dim1,dim2,dim3)   BIND(C, NAME='uload_store_avx256_ps3d')
            !**************************************************65
            ! Unaligned load-store wrapper for C intrinscis
            ! _mm256_loadu_ps and _mm256_storeu_ps.
            ! Load-store operates on linearized arrays 3D.
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),   intent(in)       :: src
            real(C_FLOAT),  dimension(*),   intent(inout)    :: dst
            integer(C_INT),                 intent(in),value :: dim1
            integer(C_INT),                 intent(in),value :: dim2
            integer(C_INT),                 intent(in),value :: dim3
            
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  uload_store_avx256_pd(src,src_len,dst_dst_len)    BIND(C, NAME='uload_store_avx256_pd')
            !**************************************************65
            ! Unaligned load-store wrapper for C intrinscis
            ! _mm256_loadu_pd and _mm256_storeu_pd
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*), intent(in)          :: src     ! source array
            integer(C_INT),               intent(in),value    :: src_len ! length of source array
            real(C_DOUBLE), dimension(*), intent(inout)       :: dst     ! destination array
            integer(C_INT),               intent(in),value    :: dst_len ! length of destination array
            
         end  subroutine
    
    
    
    end  interface
    
    
    interface
    
         subroutine  uload_store_avx256_pd2d(src,dst,dim1,dim2)   BIND(C,  NAME='uload_store_avx256_pd2d')
            !**************************************************65
            ! Unaligned load-store wrapper for C intrinscis
            ! _mm256_loadu_pd and _mm256_storeu_pd
            !   Load-store operates on linearized arrays 2D.
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),   intent(in)       ::  src
            real(C_DOUBLE),  dimension(*),   intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  uload_store_avx256_pd3d(src,dst,dim1,dim2,dim3)   BIND(C, NAME='uload_store_avx256_pd3d')
            !**************************************************65
            ! Unaligned load-store wrapper for C intrinscis
            ! _mm256_loadu_pd and _mm256_storeu_pd
            !   Load-store operates on linearized arrays 3D.
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),   intent(in)       :: src
            real(C_DOUBLE),  dimension(*),   intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            integer(C_INT),                  intent(in),value :: dim3
            
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  aload_store_avx256_ps(src,src_len,dst,dst_len)    BIND(C, NAME='aload_store_avx256_ps')
            !**************************************************65
            ! Aligned load-store wrapper for C intrinscis
            ! _mm256_loadu_ps and _mm256_storeu_ps (float type)
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src
            integer(C_INT),               intent(in),value    :: src_len
            real(C_FLOAT)  dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
            
         end  subroutine
    
    
    end  interface  
    
    
    interface 
    
         subroutine  aload_store_avx256_ps2d(src,dst,dim1,dim2)   BIND(C, NAME='aload_store_avx256_ps2d')
            !**************************************************65
            ! Aligned load-store wrapper for C intrinscis
            ! _mm256_loadu_ps and _mm256_storeu_ps (float type)
            !**************************************************65
            
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),  intent(in)       :: src
            real(C_FLOAT),  dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dim1
            integer(C_INT),                intent(in),value :: dim2
         
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  aload_store_avx256_ps3d(src,dst,dim1,dim2,dim3)   BIND(C, NAME='aload_store_avx256_ps3d')
            !**************************************************65
            ! Aligned load-store wrapper for C intrinscis
            ! _mm256_loadu_ps and _mm256_storeu_ps (float type)
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),    intent(in)       :: src
            real(C_FLOAT), dimension(*),    intent(inout)    :: dst
            integer(C_INT),                 intent(in),value :: dim1
            integer(C_INT),                 intent(in),value :: dim2
            integer(C_INT),                 intent(in),value :: dim3
            
            
         end  subroutine
    
    end  interface


    interface
    
         subroutine  aload_store_avx256_pd(src,src_len,dst,dst_len)    BIND(C, NAME='aload_store_avx256_pd')
            !**************************************************65
            ! Aligned load-store wrapper for C intrinscis
            ! _mm256_loadu_pd and _mm256_storeu_pd (double type)
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),   intent(in)          :: src
            integer(C_INT),                 intent(in),value    :: src_len
            real(C_DOUBLE), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  aload_store_avx256_pd2d(src,dst,dim1,dim2)    BIND(C, NAME='aload_store_avx256_pd2d')
            !**************************************************65
            ! Aligned load-store wrapper for C intrinscis
            ! _mm256_loadu_pd and _mm256_storeu_pd (double type)
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),   intent(in)       :: src
            real(C_DOUBLE),  dimension(*),   intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  aload_store_avx256_pd3d(src,dst,dim1,dim2,dim3)   BIND(C, NAME='aload_store_avx256_pd3d')
            !**************************************************65
            ! Aligned load-store wrapper for C intrinscis
            ! _mm256_loadu_pd and _mm256_storeu_pd (double type)
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),   intent(in)       :: src
            real(C_DOUBLE),  dimension(*),   intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            integer(C_INT),                  intent(in),value :: dim3
            
            
         end  subroutine 
    
    
    end  interface

    
    interface
    
         subroutine  broadcast_avx256_ss(src,src_len,dst,dst_len)    BIND(C, NAME='broadcast_avx256_ss')
            !**************************************************65
            ! Broadcast of single float value to 8 elements 
            ! of dst. 
            ! 
            ! src[val] -->  dst[val,val,val,val,val,val,val,val]
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src         ! src array
            integer(C_INT),               intent(in),value    :: src_len     ! length of src array
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst         ! dst array 
            integer(C_INT),               intent(in),value    :: dst_len     ! length of dst array
            
         end  subroutine
    
    
    end  interface


    interface
    
         subroutine  broadcast_avx256_sd(src,src_len,dst,dst_len)   BIND(C, NAME='broadcast_avx256_sd')
            !**************************************************65
            ! Broadcast of single double value to 4 elements 
            ! of dst. 
            ! 
            ! src[val] -->  dst[val,val,val,val]
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src       ! src array
            integer(C_INT),                intent(in),value    :: src_len   ! length of src array
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst       ! dst array 
            integer(C_INT),                intent(in),value    :: dst_len   ! length of dst array.
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  load2_store_avx256_m128(hiaddr,hiaddr_len,loaddr,loaddr_len,dst,dst_len)   BIND(C, NAME='load2_store_avx256_m128')
            !**************************************************65
            ! Loads 2-pairs of float values from hiaddr and  
            ! loaddr and stores then in dst array
            ! dst[loaddr1,loaddr2,hi_addr1,hiaddr2]
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*), intent(in)          :: hiaddr
            integer(C_INT),              intent(in),value    :: hiaddr_len
            real(C_FLOAT), dimension(*), intent(in)          :: loaddr
            integer(C_INT),              intent(in),value    :: loaddr_len
            real(C_FLOAT), dimension(*), intent(inout)       :: dst
            integer(C_INT),              intent(in),value    :: dst_len
            
         end  subroutine
    
    
    
    end  interface
    
    
    interface
    
         subroutine  load2_store_avx256_m128d(hiaddr,hiaddr_len,loaddr,loaddr_len,dst,dst_len)   BIND(C, NAME='load2_store_avx256_m128d')
            !**************************************************65
            ! Loads 2-pairs of double values from hiaddr and  
            ! loaddr and stores then in dst array
            ! dst[loaddr1,loaddr2,hi_addr1,hiaddr2]
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: hiaddr
            integer(C_INT),                intent(in),value    :: hiaddr_len
            real(C_DOUBLE), dimension(*),  intent(in)          :: loaddr
            integer(C_INT),                intent(in),value    :: loaddr_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
         
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  stream_load_avx256_si256(src,src_len,dst,dst_len)   BIND(C, NAME='stream_load_avx256_si256')
            !**************************************************65
            ! Non-temporal stores of integer 32-bit values   
            ! from src to dst.
            ! src must be aligned on 32-byte boundaries.
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT), dimension(*),  intent(in)          :: src
            integer(C_INT),                intent(in),value    :: src_len
            integer(C_INT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    
    
    end  interface
    
    
    interface
    
         subroutine  maskload_avx256_epi32(src,src_len,mask,dst,dst_len)   BIND(C, NAME='maskload_avx256_epi32')
            !**************************************************65
            ! Mask load from src to dst array  
            ! Using values from mask.
            ! if mask values(8) are true dst <-- src else dst <-- 0
            ! Notice: mask dummy argument is size 8 static array.
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT), dimension(*),  intent(in)          :: src
            integer(C_INT),                intent(in),value    :: src_len
            integer(C_INT), dimension(8),  intent(in)          :: mask
            integer(C_INT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
         
         end  subroutine
    
    
    
    end  interface
    
    
    interface
    
         subroutine  movedup_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='movedup_avx256_pd')
            !**************************************************65
            ! Duplicates even indexed double(64-bit) elements from 
            ! src and stores result in dst.
            ! 
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*), intent(in)          :: src
            integer(C_INT),               intent(in),value    :: src_len
            real(C_DOUBLE), dimension(*), intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
         
         end  subroutine
    
    
    
    
    end  interface
    
    
    interface
    
         subroutine  movehdup_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='movehdup_avx256_ps')
            !**************************************************65
            ! Duplicates odd-indexed float(32-bit) elements from 
            ! src and stores result in dst.
            ! 
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src
            integer(C_INT),               intent(in),value    :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
         
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  moveldup_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='moveldup_avx256_ps')
            !**************************************************65
            ! Duplicates even-indexed float(32-bit) elements from 
            ! src and stores result in dst.
            ! calls internaly: _mm256_moveldup_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src
            integer(C_INT),               intent(in),value    :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
            
         end  subroutine
    
    
    end  interface
    
    !*************************************************55
    ! Fortran interfaces for AVX arithemtic
    ! type of intrinsics.
    !*************************************************55
    
     interface
    
         subroutine  add_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)    BIND(C, NAME='add_avx256_ps')
            !**************************************************65
            ! Vertical vector addition of float (32-bit) 
            ! elements from src1 and src2 
            ! store result in dst.
            ! calls internaly: _mm256_add_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src1
            integer(C_INT),               intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),  intent(in)          :: src2
            integer(C_INT),               intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
         
         end  subroutine
    
    
    end  interface
    
    
     interface 
     
         subroutine  add_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)    BIND(C, NAME='add_avx256_ps2d')
            !**************************************************65
            ! Vertical vector 2D addition of float (32-bit) 
            ! elements from src1 and src2 
            ! store result in dst.
            ! calls internaly: _mm256_add_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src1
            real(C_FLOAT), dimension(*),  intent(in)       :: src2
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dim1
            integer(C_INT),               intent(in),value :: dim2
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
     
     
     end  interface
     
     
     interface
     
         subroutine  add_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)    BIND(C, NAME='add_avx256_ps3d')
            !**************************************************65
            ! Vertical vector 3D addition of float (32-bit) 
            ! elements from src1 and src2 
            ! store result in dst.
            ! calls internaly: _mm256_add_ps
            ! 
            !**************************************************65
     
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src1
            real(C_FLOAT), dimension(*),  intent(in)       :: src2
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dim1
            integer(C_INT),               intent(in),value :: dim2
            integer(C_INT),               intent(in),value :: dim3
            integer(C_INT),               intent(inout)    :: fperr
            
            
         end  subroutine
     
     end  interface
     
     
    interface
    
         subroutine  add_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='add_avx256_pd')
            !**************************************************65
            ! Vertical vector addition of double (64-bit) 
            ! elements from src1 and src2 
            ! store result in dst.
            ! calls internaly: _mm256_add_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),  intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
         
         end  subroutine
    
    
    
    end  interface
    
    
    interface
    
         subroutine  add_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='add_avx256_pd2d')
            !**************************************************65
            ! Vertical vector 2D addition of double (64-bit) 
            ! elements from src1 and src2 
            ! store result in dst.
            ! calls internaly: _mm256_add_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),   intent(in)       :: src1
            real(C_DOUBLE),  dimension(*),   intent(in)       :: src2
            real(C_DOUBLE),  dimension(*),   intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            integer(C_INT),                  intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface 
    
         subroutine  add_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='add_avx256_pd3d')
            !**************************************************65
            ! Vertical vector 3D addition of double (64-bit) 
            ! elements from src1 and src2 
            ! store result in dst.
            ! calls internaly: _mm256_add_pd
            ! 
            !**************************************************65
    
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),   intent(in)       :: src1
            real(C_DOUBLE),  dimension(*),   intent(in)       :: src2
            real(C_DOUBLE),  dimension(*),   intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            integer(C_INT),                  intent(in),value :: dim3
            integer(C_INT),                  intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  addsub_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='addsub_avx256_ps')
            !**************************************************65
            ! Vertical vector alternative addition  and sub- 
            ! traction of float(32-bit) elements from src1 and src2 
            ! storing result in dst.
            ! calls internaly: _mm256_addsub_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),   intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  addsub_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='addsub_avx256_ps2d')
            !**************************************************65
            ! Vertical vector 2D alternative addition  and sub- 
            ! traction of float(32-bit) elements from src1 and src2 
            ! storing result in dst.
            ! calls internaly: _mm256_addsub_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),    intent(in)       :: src1
            real(C_FLOAT),  dimension(*),    intent(in)       :: src2
            real(C_FLOAT),  dimension(*),    intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            integer(C_INT),                  intent(inout)    :: fperr
         
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  addsub_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='addsub_avx256_ps3d')
            !**************************************************65
            ! Vertical vector 3D alternative addition  and sub- 
            ! traction of float(32-bit) elements from src1 and src2 
            ! storing result in dst.
            ! calls internaly: _mm256_addsub_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),    intent(in)        :: src1
            real(C_FLOAT),  dimension(*),    intent(in)        :: src2
            real(C_FLOAT),  dimension(*),    intent(inout)     :: dst
            integer(C_INT),                  intent(in),value  :: dim1
            integer(C_INT),                  intent(in),value  :: dim2
            integer(C_INT),                  intent(in),value  :: dim3
            integer(C_INT),                  intent(inout)     :: fperr
    
         end  subroutine
         
         
    end  interface
    
    
    interface
    
         subroutine  addsub_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)    BIND(C, NAME='addsub_avx256_pd')
            !**************************************************65
            ! Vertical vector alternative addition  and sub- 
            ! traction of double(64-bit) elements from src1 and src2 
            ! storing result in dst.
            ! calls internaly: _mm256_addsub_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),   intent(in)          :: src1
            integer(C_INT),                 intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),   intent(in)          :: src2
            integer(C_INT),                 intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  addsub_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)    BIND(C, NAME='addsub_avx256_pd2d')
            !**************************************************65
            ! Vertical vector 2D alternative addition  and sub- 
            ! traction of double(64-bit) elements from src1 and src2 
            ! storing result in dst.
            ! calls internaly: _mm256_addsub_pd
            ! 
            !**************************************************65
            
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),   intent(in)        :: src1
            real(C_DOUBLE),  dimension(*),   intent(in)        :: src2
            real(C_DOUBLE),  dimension(*),   intent(inout)     :: dst
            integer(C_INT),                  intent(in),value  :: dim1
            integer(C_INT),                  intent(in),value  :: dim2
            integer(C_INT),                  intent(inout)     :: fperr
            
            
         end  subroutine
         
         
    end  interface
    
    
    interface
    
         subroutine  addsub_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)    BIND(C, NAME='addsub_avx256_pd3d') 
            !**************************************************65
            ! Vertical vector 3D alternative addition  and sub- 
            ! traction of double(64-bit) elements from src1 and src2 
            ! storing result in dst.
            ! calls internaly: _mm256_addsub_pd
            ! 
            !**************************************************65
    
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),   intent(in)       :: src1
            real(C_DOUBLE),  dimension(*),   intent(in)       :: src2
            real(C_DOUBLE),  dimension(*),   intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            integer(C_INT),                  intent(in),value :: dim3
            integer(C_INT),                  intent(inout)    :: fperr
            
            
         end  subroutine
         
         
    end  interface
    
    
    interface
    
         subroutine  div_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)    BIND(C, NAME='div_avx256_ps')
            !**************************************************65
            ! Vertical vector division of float (32-bit) elements
            ! from src1 and src2  and storing result in dst.
            ! 
            ! calls internaly: _mm256_div_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),   intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
         
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  div_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)    BIND(C, NAME='div_avx256_ps2d')
            !**************************************************65
            ! Vertical vector 2D division of float (32-bit) elements
            ! from src1 and src2  and storing result in dst.
            ! 
            ! calls internaly: _mm256_div_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),   intent(in)          :: src1
            real(C_FLOAT),  dimension(*),   intent(in)          :: src2
            real(C_FLOAT),  dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dim1
            integer(C_INT),                 intent(in),value    :: dim2
            integer(C_INT),                 intent(inout)       :: fperr
            
            
         end  subroutine
    
    end  interface
    
    
    interface  
    
         subroutine  div_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)    BIND(C, NAME='div_avx256_ps3d')
            !**************************************************65
            ! Vertical vector 3D division of float (32-bit) elements
            ! from src1 and src2  and storing result in dst.
            ! 
            ! calls internaly: _mm256_div_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),     intent(in)       ::  src1
            real(C_FLOAT),  dimension(*),     intent(in)       ::  src2
            real(C_FLOAT),  dimension(*),     intent(inout)    ::  dst
            integer(C_INT),                   intent(in),value ::  dim1
            integer(C_INT),                   intent(in),value ::  dim2
            integer(C_INT),                   intent(in),value ::  dim3
            integer(C_INT),                   intent(inout)    ::  fperr
            
         
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  div_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='div_avx256_pd')
            !**************************************************65
            ! Vertical vector division of double (64-bit) elements
            ! from src1 and src2  and storing result in dst.
            ! 
            ! calls internaly: _mm256_div_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),  intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  div_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)    BIND(C, NAME='div_avx256_pd2d')
            !**************************************************65
            ! Vertical vector 2D division of double (64-bit) elements
            ! from src1 and src2  and storing result in dst.
            ! 
            ! calls internaly: _mm256_div_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),       intent(in)        ::  src1
            real(C_DOUBLE),  dimension(*),       intent(in)        ::  src2
            real(C_DOUBLE),  dimension(*),       intent(inout)     ::  dst
            integer(C_INT),                      intent(in),value  ::  dim1
            integer(C_INT),                      intent(in),value  ::  dim2
            integer(C_INT),                      intent(inout)     ::  fperr
         
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  div_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='div_avx256_pd3d')
            !**************************************************65
            ! Vertical vector 3D division of double (64-bit) elements
            ! from src1 and src2  and storing result in dst.
            ! 
            ! calls internaly: _mm256_div_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),     dimension(*),     intent(in)       :: src1
            real(C_DOUBLE),     dimension(*),     intent(in)       :: src2
            real(C_DOUBLE),     dimension(*),     intent(inout)    :: dst
            integer(C_INT),                       intent(in),value :: dim1
            integer(C_INT),                       intent(in),value :: dim2
            integer(C_INT),                       intent(in),value :: dim3
            integer(C_INT),                       intent(inout)    :: fperr
         
         end  subroutine
    
    
    
    end  interface
    
    
    interface
    
         subroutine  mul_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='mul_avx256_ps')
            !**************************************************65
            ! Vertical vector multiplication of 
            ! float (32-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_mul_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src1
            integer(C_INT),               intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),  intent(in)          :: src2
            integer(C_INT),               intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  mul_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='mul_avx256_ps2d')
            !**************************************************65
            ! Vertical vector 2D multiplication of 
            ! float (32-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_mul_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),     intent(in)       :: src1
            real(C_FLOAT),  dimension(*),     intent(in)       :: src2
            real(C_FLOAT),  dimension(*),     intent(inout)    :: dst
            integer(C_INT),                   intent(in),value :: dim1
            integer(C_INT),                   intent(in),value :: dim2
            integer(C_INT),                   intent(inout)    :: fperr
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  mul_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)    BIND(C, NAME='mul_avx256_ps3d')
            !**************************************************65
            ! Vertical vector 3D multiplication of 
            ! float (32-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_mul_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),     intent(in)          :: src1
            real(C_FLOAT),  dimension(*),     intent(in)          :: src2
            real(C_FLOAT),  dimension(*),     intent(inout)       :: dst
            integer(C_INT),                   intent(in),value    :: dim1
            integer(C_INT),                   intent(in),value    :: dim2
            integer(C_INT),                   intent(in),value    :: dim3
            integer(C_INT),                   intent(inout)       :: fperr
         
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  mul_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='mul_avx256_pd')
            !**************************************************65
            ! Vertical vector multiplication of 
            ! double (64-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_mul_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),   intent(in)          :: src1
            integer(C_INT),                 intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),   intent(in)          :: src2
            integer(C_INT),                 intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine   mul_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='mul_avx256_pd2d')
            !**************************************************65
            ! Vertical vector multiplication of 
            ! double (64-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_mul_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),    intent(in)       :: src1
            real(C_DOUBLE),  dimension(*),    intent(in)       :: src2
            real(C_DOUBLE),  dimension(*),    intent(inout)    :: dst
            integer(C_INT),                   intent(in),value :: dim1
            integer(C_INT),                   intent(in),value :: dim2
            integer(C_INT),                   intent(inout)    :: fperr
            
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  mul_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)    BIND(C, NAME='mul_avx256_pd3d')
            !**************************************************65
            ! Vertical vector 3D multiplication of 
            ! double (64-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_mul_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),     intent(in)       :: src1
            real(C_DOUBLE),  dimension(*),     intent(in)       :: src2
            real(C_DOUBLE),  dimension(*),     intent(inout)    :: dst
            integer(C_INT),                    intent(in),value :: dim1
            integer(C_INT),                    intent(in),value :: dim2
            integer(C_INT),                    intent(in),value :: dim3
            integer(C_INT),                    intent(inout)    :: fperr
            
            
         end  subroutine
    
    
    end  interface
    
    
    interface 
    
         subroutine  sub_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)    BIND(C, NAME='sub_avx256_ps')
            !**************************************************65
            ! Vertical vector subtraction of 
            ! float (32-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_sub_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),   intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    
    end  interface  
    
    
    interface
    
         subroutine  sub_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='sub_avx256_ps2d')
            !**************************************************65
            ! Vertical vector 2D subtraction of 
            ! float (32-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_sub_ps
            ! 
            !**************************************************65
        
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),    intent(in)       ::  src1
            real(C_FLOAT),  dimension(*),    intent(in)       ::  src2
            real(C_FLOAT),  dimension(*),    intent(inout)    ::  dst
            integer(C_INT),                  intent(in),value ::  dim1
            integer(C_INT),                  intent(in),value ::  dim2
            integer(C_INT),                  intent(inout)    ::  fperr
            
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  sub_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='sub_avx256_ps3d')
            !**************************************************65
            ! Vertical vector 3D subtraction of 
            ! float (32-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_sub_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),     intent(in)       :: src1
            real(C_FLOAT),  dimension(*),     intent(in)       :: src2
            real(C_FLOAT),  dimension(*),     intent(inout)    :: dst
            integer(C_INT),                   intent(in),value :: dim1
            integer(C_INT),                   intent(in),value :: dim2
            integer(C_INT),                   intent(in),value :: dim3
            integer(C_INT),                   intent(inout)    :: fperr
            
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  sub_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)    BIND(C, NAME='sub_avx256_pd')
            !**************************************************65
            ! Vertical vector subtraction of 
            ! double (64-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_sub_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),    intent(in)          :: src1
            integer(C_INT),                  intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),    intent(in)          :: src2
            integer(C_INT),                  intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),    intent(inout)       :: dst
            integer(C_INT),                  intent(in),value    :: dst_len
            
         end  subroutine
    
    
    
    end  interface
    
    
    interface
    
         subroutine  sub_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='sub_avx256_pd2d')
            !**************************************************65
            ! Vertical vector 2D subtraction of 
            ! double (64-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_sub_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),     intent(in)       :: src1
            real(C_DOUBLE),  dimension(*),     intent(in)       :: src2
            real(C_DOUBLE),  dimension(*),     intent(inout)    :: dst
            integer(C_INT),                    intent(in),value :: dim1
            integer(C_INT),                    intent(in),value :: dim2
            integer(C_INT),                    intent(inout)    :: fperr
            
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  sub_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)    BIND(C, NAME='sub_avx256_pd3d')
            !**************************************************65
            ! Vertical vector 3D subtraction of 
            ! double (64-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_sub_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),       intent(in)       :: src1
            real(C_DOUBLE),  dimension(*),       intent(in)       :: src2
            real(C_DOUBLE),  dimension(*),       intent(inout)    :: dst
            integer(C_INT),                      intent(in),value :: dim1
            integer(C_INT),                      intent(in),value :: dim2
            integer(C_INT),                      intent(in),value :: dim3
            integer(C_INT),                      intent(inout)    :: fperr
            
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  hsub_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)    BIND(C, NAME='hsub_avx256_ps')
            !**************************************************65
            !  Horizontal vector subtraction of 
            !  float (32-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_hsub_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),   intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
         
         end  subroutine
    
    
    end  interface 
    
    
    
    interface
    
         subroutine  hsub_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='hsub_avx256_ps2d')
            !**************************************************65
            !  Horizontal vector 2D subtraction of 
            !  float (32-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_hsub_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),        intent(in)       :: src1
            real(C_FLOAT),  dimension(*),        intent(in)       :: src2
            real(C_FLOAT),  dimension(*),        intent(inout)    :: dst
            integer(C_INT),                      intent(in),value :: dim1
            integer(C_INT),                      intent(in),value :: dim2
            integer(C_INT),                      intent(inout)    :: fperr
            
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  hsub_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)    BIND(C, NAME='hsub_avx256_ps3d')
            !**************************************************65
            !  Horizontal vector 3D subtraction of 
            !  float (32-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_hsub_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),      intent(in)       :: src1
            real(C_FLOAT),  dimension(*),      intent(in)       :: src2
            real(C_FLOAT),  dimension(*),      intent(inout)    :: dst
            integer(C_INT),                    intent(in),value :: dim1
            integer(C_INT),                    intent(in),value :: dim2
            integer(C_INT),                    intent(in),value :: dim3
            integer(C_INT),                    intent(inout)    :: fperr
            
            
         end  subroutine 
    
    
    end  interface
    
    
    interface
    
         subroutine  hsub_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='hsub_avx256_pd')
            !**************************************************65
            !  Horizontal vector subtraction of 
            !  double (64-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_hsub_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),    intent(in)          :: src1
            integer(C_INT),                  intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),    intent(in)          :: src2
            integer(C_INT),                  intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),    intent(inout)       :: dst
            integer(C_INT),                  intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  hsub_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)    BIND(C, NAME='hsub_avx256_pd2d')
            !**************************************************65
            !  Horizontal vector 2D subtraction of 
            !  double (64-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_hsub_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),    intent(in)       :: src1
            real(C_DOUBLE),  dimension(*),    intent(in)       :: src2
            real(C_DOUBLE),  dimension(*),    intent(inout)    :: dst
            integer(C_INT),                   intent(in),value :: dim1
            integer(C_INT),                   intent(in),value :: dim2
            integer(C_INT),                   intent(inout)    :: fperr
            
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  hsub_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='hsub_avx256_pd3d')
            !**************************************************65
            !  Horizontal vector 3D subtraction of 
            !  double (64-bit) elements from src1 and src2
            ! and storing result in dst.
            ! 
            ! calls internaly: _mm256_hsub_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),     intent(in)       :: src1
            real(C_DOUBLE),  dimension(*),     intent(in)       :: src2
            real(C_DOUBLE),  dimension(*),     intent(inout)    :: dst
            integer(C_INT),                    intent(in),value :: dim1
            integer(C_INT),                    intent(in),value :: dim2
            integer(C_INT),                    intent(in),value :: dim3
            integer(C_INT),                    intent(inout)    :: fperr
            
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  dp_avx256_ps(src1,src1_len,src2,src2_len,imm,dst,dst_len)   BIND(C, NAME='dp_avx256_ps')
            !**************************************************65
            !  Vector dot product of float(32-bit) elements  
            !  from src1 and src2  and storing result in dst.
            ! 
            ! 
            ! calls internaly: _mm256_dp_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),    intent(in)          :: src1
            integer(C_INT),                 intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),    intent(in)          :: src2
            integer(C_INT),                 intent(in),value    :: src2_len
            integer(C_INT),                 intent(in),value    :: imm
            real(C_FLOAT), dimension(*),    intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  hadd_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='hadd_avx256_ps')
            !**************************************************65
            !  Vector horizontal addition of float(32-bit) 
            !  elements from src1 and src2  and
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_hadd_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),   intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    
    end  interface
    
    
    
    interface
    
         subroutine  hadd_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)    BIND(C, NAME='hadd_avx256_ps2d')
            !**************************************************65
            !  Vector 2D horizontal addition of float(32-bit) 
            !  elements from src1 and src2  and
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_hadd_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),    intent(in)       :: src1
            real(C_FLOAT),  dimension(*),    intent(in)       :: src2
            real(C_FLOAT),  dimension(*),    intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            integer(C_INT),                  intent(inout)    :: fperr
            
            
         end  subroutine
         
    
    end  interface
    
    
    
    interface
    
         subroutine  hadd_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='hadd_avx256_ps3d')
            !**************************************************65
            !  Vector 3D horizontal addition of float(32-bit) 
            !  elements from src1 and src2  and
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_hadd_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),     intent(in)       :: src1
            real(C_FLOAT),  dimension(*),     intent(in)       :: src2
            real(C_FLOAT),  dimension(*),     intent(inout)    :: dst 
            integer(C_INT),                   intent(in),value :: dim1
            integer(C_INT),                   intent(in),value :: dim2
            integer(C_INT),                   intent(in),value :: dim3
            integer(C_INT),                   intent(inout)    :: fperr
            
            
         end  subroutine
    
    
    end  interface
    
    
    
    interface
    
         subroutine  hadd_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='hadd_avx256_pd')
            !**************************************************65
            !  Vector horizontal addition of double(64-bit) 
            !  elements from src1 and src2  and
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_hadd_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),   intent(in)          :: src1
            integer(C_INT),                 intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),   intent(in)          :: src2
            integer(C_INT),                 intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
            
         end  subroutine
    
    
    end  interface
    
    
    
    interface
    
         subroutine  hadd_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='hadd_avx256_pd2d')
            !**************************************************65
            !  Vector 2D horizontal addition of double(64-bit) 
            !  elements from src1 and src2  and
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_hadd_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),     intent(in)       :: src1
            real(C_DOUBLE),  dimension(*),     intent(in)       :: src2
            real(C_DOUBLE),  dimension(*),     intent(inout)    :: dst
            integer(C_INT),                    intent(in),value :: dim1
            integer(C_INT),                    intent(in),value :: dim2
            integer(C_INT),                    intent(inout)    :: fperr
            
            
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  hadd_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='hadd_avx256_pd3d')
            !**************************************************65
            !  Vector 3D horizontal addition of double(64-bit) 
            !  elements from src1 and src2  and
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_hadd_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE),  dimension(*),   intent(in)       :: src1
            real(C_DOUBLE),  dimension(*),   intent(in)       :: src2
            real(C_DOUBLE),  dimension(*),   intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            integer(C_INT),                  intent(in),value :: dim3
            integer(C_INT),                  intent(inout)    :: fperr
            
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  add_avx256_epi32(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='add_avx256_epi32')
            !**************************************************65
            !  Vector vertical addition of int(32-bit) 
            !  elements from src1 and src2  and
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_add_epi32
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT), dimension(*),   intent(in)          :: src1
            integer(C_INT),                 intent(in),value    :: src1_len
            integer(C_INT), dimension(*),   intent(in)          :: src2
            integer(C_INT),                 intent(in),value    :: src2_len
            integer(C_INT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  add_avx256_epi64(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='add_avx256_epi64')
            !**************************************************65
            !  Vector vertical addition of long long(64-bit) 
            !  elements from src1 and src2  and
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_add_epi64
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_LONG_LONG), dimension(*),  intent(in)          :: src1
            integer(C_INT),                      intent(in),value    :: src1_len
            integer(C_LONG_LONG), dimension(*),  intent(in)          :: src2
            integer(C_INT),                      intent(in),value    :: src2_len
            integer(C_LONG_LONG), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                      intent(in),value    :: dst_len
         
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  sub_avx256_epi32(src1,src1_len,src2,src2_len,dst,dst_len)    BIND(C, NAME='sub_avx256_epi32')
            !**************************************************65
            !  Vector vertical subtraction of integer(32-bit) 
            !  elements from src1 and src2  and
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_sub_epi64
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT), dimension(*),   intent(in)          :: src1
            integer(C_INT),                 intent(in),value    :: src1_len
            integer(C_INT), dimension(*),   intent(in)          :: src2
            integer(C_INT),                 intent(in),value    :: src2_len
            integer(C_INT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
         
         end  subroutine
    
    
    
    end  interface
    
    
    interface
    
         subroutine  sub_avx256_epi64(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='sub_avx256_epi64')
            !**************************************************65
            !  Vector vertical subtraction of long long(64-bit) 
            !  elements from src1 and src2  and
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_sub_epi64
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_LONG_LONG), dimension(*), intent(in)          :: src1
            integer(C_INT),                     intent(in),value    :: src1_len
            integer(C_LONG_LONG), dimension(*), intent(in)          :: src2
            integer(C_INT),                     intent(in),value    :: src2_len
            integer(C_LONG_LONG), dimension(*), intent(inout)       :: dst
            integer(C_INT),                     intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  abs_avx256_epi32(src,src_len,dst,dst_len)   BIND(C, NAME='abs_avx256_epi32')
            !**************************************************65
            !  Vector abs value of integer(32-bit) 
            !  elements from src 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_abs_epi32
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            integer(C_INT), dimension(*),  intent(in)       :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  blend_avx256_epi32(src1,src1_len,src2,src2_len,imm,dst,dst_len)   BIND(C, NAME='blend_avx256_epi32')
            !**************************************************65
            !  Vector blend of   integer(32-bit) 
            !  elements from src1 and src2 and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_blend_epi32
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT), dimension(*),  intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            integer(C_INT), dimension(*),  intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            integer(C_INT),                intent(in),value    :: imm
            integer(C_INT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  rcp_avx256_ps(src,src_len,dst,dst_len)    BIND(C, NAME='rcp_avx256_ps')
            !**************************************************65
            !  Vector reciprocal of floating-point(32-bit) 
            !  elements from src1  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_rcp_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)          :: src
            integer(C_INT),                intent(in),value    :: src_len
            real(C_FLOAT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  rcp_avx256_ps2d(src1,dst,dim1,dim2,fperr)   BIND(C, NAME='rcp_avx256_ps2d')
            !**************************************************65
            !  Vector 2D reciprocal of floating-point(32-bit) 
            !  elements from src1  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_rcp_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),    intent(in)       ::  src1
            real(C_FLOAT),  dimension(*),    intent(inout)    ::  dst
            integer(C_INT),                  intent(in),value ::  dim1
            integer(C_INT),                  intent(in),value ::  dim2
            integer(C_INT),                  intent(inout)    ::  fperr
            
            
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  rcp_avx256_ps3d(src1,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='rcp_avx256_ps3d')
            !**************************************************65
            !  Vector 3D reciprocal of floating-point(32-bit) 
            !  elements from src1  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_rcp_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),    intent(in)       :: src1
            real(C_FLOAT),  dimension(*),    intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            integer(C_INT),                  intent(in),value :: dim3
            integer(C_INT),                  intent(inout)    :: fperr
            
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  rsqrt_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='rsqrt_avx256_ps')
            !**************************************************65
            !  Vector reciprocal sqrt of floating-point(32-bit) 
            !  elements from src1  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_rsqrt_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src
            integer(C_INT),               intent(in),value    :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  rsqrt_avx256_ps2d(src,dst,dim1,dim2,fperr)   BIND(C, NAME='rsqrt_avx256_ps2d')
            !**************************************************65
            !  Vector 2D reciprocal sqrt of floating-point(32-bit) 
            !  elements from src1  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_rsqrt_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),    intent(in)       :: src
            real(C_FLOAT),  dimension(*),    intent(inout)    :: dst
            integer(C_INT),                  intent(in),value :: dim1
            integer(C_INT),                  intent(in),value :: dim2
            integer(C_INT),                  intent(Inout)    :: fperr
            
         end  subroutine
    
    
    end  interface
    
    
    
    interface
    
         subroutine  rsqrt_avx256_ps3d(src,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='rsqrt_avx256_ps3d')
            !**************************************************65
            !  Vector 3D reciprocal sqrt of floating-point(32-bit) 
            !  elements from src1  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_rsqrt_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),       intent(in)       :: src
            real(C_FLOAT),  dimension(*),       intent(inout)    :: dst
            integer(C_INT),                     intent(in),value :: dim1
            integer(C_INT),                     intent(in),value :: dim2
            integer(C_INT),                     intent(in),value :: dim3
            integer(C_INT),                     intent(inout)    :: fperr
            
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  sqrt_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='sqrt_avx256_pd')
            !**************************************************65
            !  Vector  sqrt of floating-point(64-bit) 
            !  elements from src1  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_sqrt_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src
            integer(C_INT),                intent(in),value    :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  sqrt_avx256_pd2d(src,dst,dim1,dim2,fperr)   BIND(C, NAME='sqrt_avx256_pd2d')
            !**************************************************65
            !  Vector 2D sqrt of floating-point(64-bit) 
            !  elements from src  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_sqrt_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),       intent(in)       :: src
            real(C_DOUBLE), dimension(*),       intent(inout)    :: dst
            integer(C_INT),                     intent(in),value :: dim1
            integer(C_INT),                     intent(in),value :: dim2
            integer(C_INT),                     intent(inout)    :: fperr
            
            
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  sqrt_avx256_pd3d(src,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='sqrt_avx256_pd3d')
            !**************************************************65
            !  Vector 3D sqrt of floating-point(64-bit) 
            !  elements from src  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_sqrt_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),       intent(in)       :: src
            real(C_DOUBLE), dimension(*),       intent(inout)    :: dst
            integer(C_INT),                     intent(in),value :: dim1
            integer(C_INT),                     intent(in),value :: dim2
            integer(C_INT),                     intent(in),value :: dim3
            integer(C_INT),                     intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  sqrt_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='sqrt_avx256_ps')
            !**************************************************65
            !  Vector  sqrt of floating-point(32-bit) 
            !  elements from src1  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_sqrt_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src
            integer(C_INT),               intent(in),value    :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  sqrt_avx256_ps2d(src,dst,dim1,dim2,fperr)   BIND(C, NAME='sqrt_avx256_ps2d')
            !**************************************************65
            !  Vector 2D sqrt of floating-point(32-bit) 
            !  elements from src1  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_sqrt_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),       intent(in)       ::  src
            real(C_FLOAT),  dimension(*),       intent(inout)    ::  dst
            integer(C_INT),                     intent(in),value ::  dim1
            integer(C_INT),                     intent(in),value ::  dim2
            integer(C_INT),                     intent(inout)    ::  fperr
            
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  sqrt_avx256_ps3d(src,dst,dim1,dim2,dim3,fperr)    BIND(C, NAME='sqrt_avx256_ps3d')
            !**************************************************65
            !  Vector 3D sqrt of floating-point(32-bit) 
            !  elements from src1  and 
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_sqrt_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),       intent(in)       :: src
            real(C_FLOAT),  dimension(*),       intent(inout)    :: dst
            integer(C_INT),                     intent(in),value :: dim1
            integer(C_INT),                     intent(in),value :: dim2
            integer(C_INT),                     intent(in),value :: dim3
            integer(C_INT),                     intent(inout)    :: fperr
            
            
         end  subroutine 
    
    end  interface
    
    
    interface
    
         subroutine  and_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='and_avx256_pd')
            !**************************************************65
            !  Vector  'AND' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_and_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),  intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  and_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='and_avx256_pd2d')
            !**************************************************65
            !  Vector 2D 'AND' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_and_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),       intent(in)       :: src1
            real(C_DOUBLE), dimension(*),       intent(in)       :: src2
            real(C_DOUBLE), dimension(*),       intent(inout)    :: dst
            integer(C_INT),                     intent(in),value :: dim1
            integer(C_INT),                     intent(in),value :: dim2
          
            integer(C_INT),                     intent(inout)    :: fperr
            
            
         end  subroutine
    
    
    end  interface
    
    
    
    interface
    
         subroutine  and_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='and_avx256_pd3d')
            !**************************************************65
            !  Vector 3D 'AND' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_and_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),           intent(in)       :: src1
            real(C_DOUBLE), dimension(*),           intent(in)       :: src2
            real(C_DOUBLE), dimension(*),           intent(inout)    :: dst
            integer(C_INT),                         intent(in),value :: dim1
            integer(C_INT),                         intent(in),value :: dim2
            integer(C_INT),                         intent(in),value :: dim3
            integer(C_INT),                         intent(inout)    :: fperr
            
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  and_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='and_avx256_ps')
            !**************************************************65
            !  Vector  'AND' of floating-point(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_and_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src1
            integer(C_INT),               intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),  intent(in)          :: src2
            integer(C_INT),               intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  and_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='and_avx256_ps2d')
            !**************************************************65
            !  Vector  2D 'AND' of floating-point(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_and_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),        intent(in)       :: src1
            real(C_FLOAT), dimension(*),        intent(in)       :: src2
            real(C_FLOAT), dimension(*),        intent(inout)    :: dst
            integer(C_INT),                     intent(in),value :: dim1
            integer(C_INT),                     intent(in),value :: dim2
            integer(C_INT),                     intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  and_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='and_avx256_ps3d')
            !**************************************************65
            !  Vector  3D 'AND' of floating-point(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_and_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),        intent(in)       :: src1
            real(C_FLOAT), dimension(*),        intent(in)       :: src2
            real(C_FLOAT), dimension(*),        intent(inout)    :: dst
            integer(C_INT),                     intent(in),value :: dim1
            integer(C_INT),                     intent(in),value :: dim2
            integer(C_INT),                     intent(in),value :: dim3
            integer(C_INT),                     intent(inout)    :: fperr
            
            
         end subroutine
    
    end  interface
    
    
    interface
    
         subroutine  andnot_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='andnot_avx256_pd')
            !**************************************************65
            !  Vector  'ANDNOT' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_andnot_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),  intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    
    interface 
    
         subroutine  andnot_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='andnot_avx256_pd2d')
            !**************************************************65
            !  Vector 2D 'ANDNOT' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_andnot_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),           intent(in)       :: src1
            real(C_DOUBLE), dimension(*),           intent(in)       :: src2
            real(C_DOUBLE), dimension(*),           intent(inout)    :: dst
            integer(C_INT),                         intent(in),value :: dim1
            integer(C_INT),                         intent(in),value :: dim2
            integer(C_INT),                         intent(inout)    :: fperr
            
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  andnot_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='andnot_avx256_pd3d')
            !**************************************************65
            !  Vector 3D 'ANDNOT' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_andnot_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),           intent(in)       :: src1
            real(C_DOUBLE), dimension(*),           intent(in)       :: src2
            real(C_DOUBLE), dimension(*),           intent(inout)    :: dst
            integer(C_INT),                         intent(in),value :: dim1
            integer(C_INT),                         intent(in),value :: dim2
            integer(C_INT),                         intent(in),value :: dim3
            integer(C_INT),                         intent(inout)    :: fperr
            
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  andnot_avx256_si256(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='andnot_avx256_si256')
            !**************************************************65
            !  Vector  'ANDNOT' of integer(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_andnot_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT), dimension(*),  intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            integer(C_INT), dimension(*),  intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            integer(C_INT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  xor_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)     BIND(C, NAME='xor_avx256_ps')
            !**************************************************65
            !  Vector  'XOR' of floating-point(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_xor_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),   intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  xor_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='xor_avx256_ps2d')
            !**************************************************65
            !  Vector  'XOR' of floating-point(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            !  Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_xor_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),    intent(in)       :: src1
            real(C_FLOAT), dimension(*),    intent(in)       :: src2
            real(C_FLOAT), dimension(*),    intent(inout)    :: dst
            integer(C_INT),                 intent(in),value :: dim1
            integer(C_INT),                 intent(in),value :: dim2
            integer(C_INT),                 intent(inout)    :: fperr
            
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  xor_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)    BIND(C, NAME='xor_avx256_ps3d')
            !**************************************************65
            !  Vector  'XOR' of floating-point(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            !  Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_xor_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),    intent(in)       :: src1
            real(C_FLOAT), dimension(*),    intent(in)       :: src2
            real(C_FLOAT), dimension(*),    intent(inout)    :: dst
            integer(C_INT),                 intent(in),value :: dim1
            integer(C_INT),                 intent(in),value :: dim2
            integer(C_INT),                 intent(in),value :: dim3
            integer(C_INT),                 intent(inout)    :: fperr
            
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  xor_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='xor_avx256_pd')
            !**************************************************65
            !  Vector  'XOR' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_xor_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),   intent(in)          :: src1
            integer(C_INT),                 intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),   intent(in)          :: src2
            integer(C_INT),                 intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  xor_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)    BIND(C, NAME='xor_avx256_pd2d')
            !**************************************************65
            !  Vector  'XOR' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            !   Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_xor_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE), dimension(*),           intent(in)       :: src1
             real(C_DOUBLE), dimension(*),           intent(in)       :: src2
             real(C_DOUBLE), dimension(*),           intent(inout)    :: dst
             integer(C_INT),                         intent(in),value :: dim1
             integer(C_INT),                         intent(in),value :: dim2
             integer(C_INT),                         intent(inout)    :: fperr
             
             
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  xor_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)      BIND(C, NAME='xor_avx256_pd3d')
            !**************************************************65
            !  Vector  'XOR' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            !   Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_xor_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE), dimension(*),           intent(in)       :: src1
             real(C_DOUBLE), dimension(*),           intent(in)       :: src2
             real(C_DOUBLE), dimension(*),           intent(inout)    :: dst
             integer(C_INT),                         intent(in),value :: dim1
             integer(C_INT),                         intent(in),value :: dim2
             integer(C_INT),                         intent(in),value :: dim3
             integer(C_INT),                         intent(inout)    :: fperr
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  xor_avx256_si256(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='xor_avx256_si256')
            !**************************************************65
            !  Vector  'XOR' of integer(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_xor_si256
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT), dimension(*),   intent(in)          :: src1
            integer(C_INT),                 intent(in),value    :: src1_len
            integer(C_INT), dimension(*),   intent(in)          :: src2
            integer(C_INT),                 intent(in),value    :: src2_len
            integer(C_INT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  or_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)    BIND(C, NAME='or_avx256_ps')
            !**************************************************65
            !  Vector  'OR' of floating-point(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_or_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),   intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  or_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)    BIND(C, NAME='or_avx256_ps2d')
            !**************************************************65
            !  Vector  'OR' of floating-point(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            !    Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_or_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src1
             real(C_FLOAT),  dimension(*),            intent(in)       :: src2
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(inout)    :: fperr
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  or_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='or_avx256_ps3d')
            !**************************************************65
            !  Vector  'OR' of floating-point(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            !    Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_or_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT), dimension(*),            intent(in)       :: src1
             real(C_FLOAT), dimension(*),            intent(in)       :: src2
             real(C_FLOAT), dimension(*),            intent(inout)    :: dst
             integer(C_INT),                         intent(in),value :: dim1
             integer(C_INT),                         intent(in),value :: dim2
             integer(C_INT),                         intent(in),value :: dim3
             integer(C_INT),                         intent(inout)    :: fperr
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  or_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='or_avx256_pd')
            !**************************************************65
            !  Vector  'OR' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_or_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),   intent(in)          :: src1
            integer(C_INT),                 intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),   intent(in)          :: src2
            integer(C_INT),                 intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  or_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='or_avx256_pd2d')
            !**************************************************65
            !  Vector  'OR' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            !   Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_or_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE), dimension(*),           intent(in)       :: src1
             real(C_DOUBLE), dimension(*),           intent(in)       :: src2
             real(C_DOUBLE), dimension(*),           intent(inout)    :: dst
             integer(C_INT),                         intent(in),value :: dim1
             integer(C_INT),                         intent(in),value :: dim2
            
             integer(C_INT),                         intent(inout)    :: fperr
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  or_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='or_avx256_pd3d')
            !**************************************************65
            !  Vector  'OR' of floating-point(64-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            !   Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_or_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE), dimension(*),           intent(in)       :: src1
             real(C_DOUBLE), dimension(*),           intent(in)       :: src2
             real(C_DOUBLE), dimension(*),           intent(inout)    :: dst
             integer(C_INT),                         intent(in),value :: dim1
             integer(C_INT),                         intent(in),value :: dim2
             integer(C_INT),                         intent(in),value :: dim3
             integer(C_INT),                         intent(inout)    :: fperr
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  or_avx256_si256(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='or_avx256_si256')
            !**************************************************65
            !  Vector  'OR' of integer(32-bit) 
            !  elements from src1  and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_or_si256
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT), dimension(*),   intent(in)          :: src1
            integer(C_INT),                 intent(in),value    :: src1_len
            integer(C_INT), dimension(*),   intent(in)          :: src2
            integer(C_INT),                 intent(in),value    :: src2_len
            integer(C_INT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  ceil_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='ceil_avx256_ps')
            !**************************************************65
            !  Vector  ceil operation of floating-point(32-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_ceil_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src
            integer(C_INT),               intent(in),value    :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  ceil_avx256_ps2d(src,dst,dim1,dim2,fperr)   BIND(C, NAME='ceil_avx256_ps2d')
            !**************************************************65
            !  Vector  ceil operation of floating-point(32-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            !     Linearized (flat) array 2D
            ! 
            ! calls internaly: _mm256_ceil_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(inout)    :: fperr
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  ceil_avx256_ps3d(src,dst,dim1,dim2,dim3,fperr)    BIND(C, NAME='ceil_avx256_ps3d')
            !**************************************************65
            !  Vector  ceil operation of floating-point(32-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            !     Linearized (flat) array 3D
            ! 
            ! calls internaly: _mm256_ceil_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             integer(C_INT),                          intent(inout)    :: fperr
             
             
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  ceil_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='ceil_avx256_pd')
            !**************************************************65
            !  Vector  ceil operation of floating-point(64-bit) 
            !  on elements from src
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_ceil_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),   intent(in)          :: src
            integer(C_INT),                 intent(in),value    :: src_len
            real(C_DOUBLE), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
         
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  ceil_avx256_pd2d(src,dst,dim1,dim2,fperr)   BIND(C, NAME='ceil_avx256_pd2d')
            !**************************************************65
            !  Vector  ceil operation of floating-point(64-bit) 
            !  on elements from src
            !  storing result in dst.
            !   
            !    Linearized (flat)  array 2D.
            ! 
            ! calls internaly: _mm256_ceil_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(inout)    :: fperr
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  ceil_avx256_pd3d(src,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='ceil_avx256_pd2d')
            !**************************************************65
            !  Vector  ceil operation of floating-point(64-bit) 
            !  on elements from src
            !  storing result in dst.
            !   
            !    Linearized (flat)  array 3D.
            ! 
            ! calls internaly: _mm256_ceil_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             integer(C_INT),                          intent(inout)    :: fperr
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  floor_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='floor_avx256_ps')
            !**************************************************65
            !  Vector floor operation of floating-point(32-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_floor_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)          :: src
            integer(C_INT),                intent(in),value    :: src_len
            real(C_FLOAT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  floor_avx256_ps2d(src,dst,dim1,dim2,fperr)   BIND(C, NAME='floor_avx256_ps2d')
            !**************************************************65
            !  Vector floor operation of floating-point(32-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            !    Linearized (flat) array 2D
            ! 
            ! calls internaly: _mm256_floor_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(inout)    :: fperr
             
             
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  floor_avx256_ps3d(src,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='floor_avx256_ps3d')
            !**************************************************65
            !  Vector floor operation of floating-point(32-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            !    Linearized (flat) array 3D
            ! 
            ! calls internaly: _mm256_floor_ps
            ! 
            !**************************************************65
            
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             integer(C_INT),                          intent(inout)    :: fperr
         
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  floor_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='floor_avx256_pd')
            !**************************************************65
            !  Vector floor operation of floating-point(64-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_floor_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(in)       :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  floor_avx256_pd2d(src,dst,dim1,dim2,fperr)   BIND(C, NAME='floor_avx256_pd2d')
            !**************************************************65
            !  Vector floor operation of floating-point(64-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            !    Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_floor_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(inout)    :: fperr
             
             
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  floor_avx256_pd3d(src,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='floor_avx256_pd3d')
            !**************************************************65
            !  Vector floor operation of floating-point(64-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            !    Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_floor_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             integer(C_INT),                          intent(inout)    :: fperr
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  max_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='max_avx256_ps')
            !**************************************************65
            !  Vector max operation of floating-point(32-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_max_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src1
            integer(C_INT),               intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),  intent(in)          :: src2
            integer(C_INT),               intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  max_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='max_avx256_ps2d')
            !**************************************************65
            !  Vector max operation of floating-point(32-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            !     Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_max_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src1
             real(C_FLOAT),  dimension(*),            intent(in)       :: src2
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(inout)    :: fperr
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  max_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='max_avx256_ps3d')
            !**************************************************65
            !  Vector max operation of floating-point(32-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            !     Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_max_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src1
             real(C_FLOAT),  dimension(*),            intent(in)       :: src2
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             integer(C_INT),                          intent(inout)    :: fperr
            
         end  subroutine 
    
    
    end  interface
    
    
    interface
    
         subroutine  max_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='max_avx256_pd')
            !**************************************************65
            !  Vector max operation of floating-point(64-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_max_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),  intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  max_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='max_avx256_pd2d')
            !**************************************************65
            !  Vector max operation of floating-point(64-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            !     Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_max_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE), dimension(*),           intent(in)       :: src1
             real(C_DOUBLE), dimension(*),           intent(in)       :: src2
             real(C_DOUBLE), dimension(*),           intent(inout)    :: dst
             integer(C_INT),                         intent(in),value :: dim1
             integer(C_INT),                         intent(in),value :: dim2
            
             integer(C_INT),                         intent(inout)    :: fperr
             
             
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  max_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='max_avx256_pd3d')
            !**************************************************65
            !  Vector max operation of floating-point(64-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            !     Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_max_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE), dimension(*),           intent(in)       :: src1
             real(C_DOUBLE), dimension(*),           intent(in)       :: src2
             real(C_DOUBLE), dimension(*),           intent(inout)    :: dst
             integer(C_INT),                         intent(in),value :: dim1
             integer(C_INT),                         intent(in),value :: dim2
             integer(C_INT),                         intent(in),value :: dim3
             integer(C_INT),                         intent(inout)    :: fperr
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  min_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='min_avx256_ps')
            !**************************************************65
            !  Vector min operation of floating-point(32-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_min_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src1
            integer(C_INT),               intent(in),value    :: src1_len
            real(C_FLOAT), dimension(*),  intent(in)          :: src2
            integer(C_INT),               intent(in),value    :: src2_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  min_avx256_ps2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='min_avx256_ps2d')
            !**************************************************65
            !  Vector min operation of floating-point(32-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            !   Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_min_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src1
             real(C_FLOAT),  dimension(*),            intent(in)       :: src2
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(inout)    :: fperr
             
             
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  min_avx256_ps3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='min_avx256_ps3d')
            !**************************************************65
            !  Vector min operation of floating-point(32-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            !   Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_min_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src1
             real(C_FLOAT),  dimension(*),            intent(in)       :: src2
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             integer(C_INT),                          intent(inout)    :: fperr
             
             
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  min_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='min_avx256_pd')
            !**************************************************65
            !  Vector min operation of floating-point(64-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_min_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src1
            integer(C_INT),                intent(in),value    :: src1_len
            real(C_DOUBLE), dimension(*),  intent(in)          :: src2
            integer(C_INT),                intent(in),value    :: src2_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                integer(in),value   :: dst_len
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  min_avx256_pd2d(src1,src2,dst,dim1,dim2,fperr)   BIND(C, NAME='min_avx256_pd2d')
            !**************************************************65
            !  Vector min operation of floating-point(64-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            !  Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_min_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE), dimension(*),           intent(in)       :: src1
             real(C_DOUBLE), dimension(*),           intent(in)       :: src2
             real(C_DOUBLE), dimension(*),           intent(inout)    :: dst
             integer(C_INT),                         intent(in),value :: dim1
             integer(C_INT),                         intent(in),value :: dim2
            
             integer(C_INT),                         intent(inout)    :: fperr
         
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  min_avx256_pd3d(src1,src2,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='min_avx256_pd3d')
            !**************************************************65
            !  Vector min operation of floating-point(64-bit) 
            !  elements from src1 and src2
            !  storing result in dst.
            !   
            !  Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_min_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE), dimension(*),           intent(in)       :: src1
             real(C_DOUBLE), dimension(*),           intent(in)       :: src2
             real(C_DOUBLE), dimension(*),           intent(inout)    :: dst
             integer(C_INT),                         intent(in),value :: dim1
             integer(C_INT),                         intent(in),value :: dim2
             integer(C_INT),                         intent(in),value :: dim3
             integer(C_INT),                         intent(inout)    :: fperr
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  round_avx256_ps(src,src_len,mode,dst,dst_len)   BIND(C, NAME='round_avx256_ps')
            !**************************************************65
            !  Vector round operation of floating-point(32-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_round_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)          :: src
            integer(C_INT),                intent(in),value    :: src_len
            integer(C_INT),                intent(in),value    :: mode
            real(C_FLOAT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  round_avx256_ps2d(src,dst,dim1,dim2,fperr)     BIND(C, NAME='round_avx256_ps2d')
            !**************************************************65
            !  Vector round operation of floating-point(32-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            !   Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_round_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(inout)    :: fperr
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  round_avx256_ps3d(src,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='round_avx256_ps3d')
            !**************************************************65
            !  Vector round operation of floating-point(32-bit) 
            !   on elements from src
            !  storing result in dst.
            !   
            !   Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_round_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             integer(C_INT),                          intent(inout)    :: fperr
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  round_avx256_pd(src,src_len,mode,dst,dst_len)   BIND(C, NAME='round_avx256_pd')
            !**************************************************65
            !  Vector round operation of floating-point(64-bit) 
            !  on  elements from src
            !  storing result in dst.
            !   
            ! 
            ! 
            ! calls internaly: _mm256_round_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),   intent(in)          :: src
            integer(C_INT),                 intent(in),value    :: src_len
            integer(C_INT),                 intent(in),value    :: mode
            real(C_DOUBLE), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                 intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  round_avx256_pd2d(src,dst,dim1,dim2,fperr)   BIND(C, NAME='round_avx256_pd2d')
            !**************************************************65
            !  Vector round operation of floating-point(64-bit) 
            !  on  elements from src
            !  storing result in dst.
            !   
            !    Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_round_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(inout)    :: fperr
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  round_avx256_pd3d(src,dst,dim1,dim2,dim3,fperr)   BIND(C, NAME='round_avx256_pd3d')
            !**************************************************65
            !  Vector round operation of floating-point(64-bit) 
            !  on  elements from src
            !  storing result in dst.
            !   
            !    Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_round_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             integer(C_INT),                          intent(inout)    :: fperr
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  store_avx256_ps(src,src_len,dst,dst_len)    BIND(C, NAME='store_avx256_ps')
            !**************************************************65
            !  Vector memory store of floating-point(32-bit) 
            !  on elements from src  storing result in dst.
            !  
            !  Aligned data on 32-byte boundary 
            ! 
            ! 
            ! calls internaly: _mm256_store_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)          :: src
            integer(C_INT),                intent(in),value    :: src_len
            real(C_FLOAT), dimension(*),   intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  store_avx256_ps2d(src,dst,dim1,dim2)   BIND(C, NAME='store_avx256_ps2d')
            !**************************************************65
            !  Vector memory store of floating-point(32-bit) 
            !  on elements from src  storing result in dst.
            !  
            !  Aligned data on 32-byte boundary 
            !  Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_store_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  store_avx256_ps3d(src,dst,dim1,dim2,dim3)    BIND(C, NAME='store_avx256_ps3d')
            !**************************************************65
            !  Vector memory store of floating-point(32-bit) 
            !  on elements from src  storing result in dst.
            !  
            !  Aligned data on 32-byte boundary 
            !  Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_store_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             
             
         end  subroutine
    
    end  interface
    
    
    
    interface
    
         subroutine  store_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='store_avx256_pd')
            !**************************************************65
            !  Vector memory store of floating-point(64-bit) 
            !  elements from src  storing result in dst.
            !  
            !  Aligned data on 32-byte boundary. 
            ! 
            ! 
            ! calls internaly: _mm256_store_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src
            integer(C_INT),                intent(in),value    :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  store_avx256_pd2d(src,dst,dim1,dim2)   BIND(C, NAME='store_avx256_pd2d')
            !**************************************************65
            !  Vector memory store of floating-point(64-bit) 
            !  elements from src  storing result in dst.
            !  
            !  Aligned data on 32-byte boundary. 
            !  Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_store_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             
             
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  store_avx256_pd3d(src,dst,dim1,dim2,dim3)   BIND(C, NAME='store_avx256_pd3d')
            !**************************************************65
            !  Vector memory store of floating-point(64-bit) 
            !  elements from src  storing result in dst.
            !  
            !  Aligned data on 32-byte boundary. 
            !  Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_store_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  storeu_avx256_ps(src,src_len,dst,dst_len)    BIND(C, NAME='storeu_avx256_ps')
            !**************************************************65
            !  Vector memory store of floating-point(32-bit) 
            !  elements from src   storing result in dst.
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: _mm256_storeu_ps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src
            integer(C_INT),               intent(in),value    :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  storeu_avx256_ps2d(src,dst,dim1,dim2)   BIND(C, NAME='storeu_avx256_ps2d')
            !**************************************************65
            !  Vector memory store of floating-point(32-bit) 
            !  elements from src   storing result in dst.
            !  
            !  Unaligned data. 
            !  Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_storeu_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             
             
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  storeu_avx256_ps3d(src,dst,dim1,dim2,dim3)   BIND(C, NAME='storeu_avx256_ps3d')
            !**************************************************65
            !  Vector memory store of floating-point(32-bit) 
            !  elements from src   storing result in dst.
            !  
            !  Unaligned data. 
            !  Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_storeu_ps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  storeu_avx256_pd(src,src_len,dst,dst_len)    BIND(C, NAME='storeu_avx256_pd')
            !**************************************************65
            !  Vector memory store of floating-point(64-bit) 
            !  elements from src  storing result in dst.
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: _mm256_storeu_pd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src
            integer(C_INT),                intent(in),value    :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  storeu_avx256_pd2d(src,dst,dim1,dim2,dim3)   BIND(C, NAME='storeu_avx256_pd2d')
            !**************************************************65
            !  Vector memory store of floating-point(64-bit) 
            !  elements from src  storing result in dst.
            !  
            !  Unaligned data. 
            !  Linearized (flat) array 2D.
            ! 
            ! calls internaly: _mm256_storeu_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  storeu_avx256_pd3d(src,dst,dim1,dim2,dim3)   BIND(C, NAME='storeu_avx256_pd3d')
            !**************************************************65
            !  Vector memory store of floating-point(64-bit) 
            !  elements from src  storing result in dst.
            !  
            !  Unaligned data. 
            !  Linearized (flat) array 3D.
            ! 
            ! calls internaly: _mm256_storeu_pd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  storeu2_avx256_m128(src,src_len,dst1,dst1_len,dst2,dst2_len)   BIND(C, NAME='storeu2_avx256_m128')
            !**************************************************65
            !  Vector memory store of floating-point(32-bit) 
            !  elements from src  storing result in dst1 and dst2
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: unknow number of machine code instr.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src
            integer(C_INT),               intent(in),value    :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst1
            integer(C_INT),               intent(in),value    :: dst1_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst2
            integer(C_INT),               intent(in),value    :: dst2_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  storeu2_avx256_m128d(src,src_len,dst1,dst1_len,dst2,dst2_len)   BIND(C, NAME='storeu2_avx256_m128d')
            !**************************************************65
            !  Vector memory store of floating-point(64-bit) 
            !  elements from src  storing result in dst1 and dst2
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: unknow number of machine code instr.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src
            integer(C_INT),                intent(in),value    :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst1
            integer(C_INT),                intent(in),value    :: dst1_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst2
            integer(C_INT),                intent(in),value    :: dst2_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  stream_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='stream_avx256_ps')
            !**************************************************65
            !  Vector memory stream store of floating-point(32-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: vmovntps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)          :: src
            integer(C_INT),               intent(in),value    :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)       :: dst
            integer(C_INT),               intent(in),value    :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  stream_avx256_ps2d(src,dst,dim1,dim2)   BIND(C, NAME='stream_avx256_ps2d')
            !**************************************************65
            !  Vector memory stream store of floating-point(32-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            !  Linearized (flat) array 2D.
            ! 
            ! calls internaly: vmovntps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             
             
         end  subroutine 
    
    end  interface
    
    
    interface
    
         subroutine  stream_avx256_ps3d(src,dst,dim1,dim2,dim3)    BIND(C, NAME='stream_avx256_ps3d')
            !**************************************************65
            !  Vector memory stream store of floating-point(32-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            !  Linearized (flat) array 3D.
            ! 
            ! calls internaly: vmovntps
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_FLOAT),  dimension(*),            intent(in)       :: src
            
             real(C_FLOAT),  dimension(*),            intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             
             
         end  subroutine 
    
    
    
    end  interface
    
    
    interface
    
         subroutine  stream_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='stream_avx256_pd')
            !**************************************************65
            !  Vector memory stream store of floating-point(64-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: vmovntpd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)          :: src
            integer(C_INT),                intent(in),value    :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)       :: dst
            integer(C_INT),                intent(in),value    :: dst_len
            
         end  subroutine
    
    end interface
    
    
    interface
    
         subroutine  stream_avx256_pd2d(src,dst,dim1,dim2)   BIND(C, NAME='stream_avx256_pd2d')
            !**************************************************65
            !  Vector memory stream store of floating-point(64-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            !  Linearized (flat) array 2D.
            ! 
            ! calls internaly: vmovntpd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             
             
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  stream_avx256_pd3d(src,dst,dim1,dim2,dim3)   BIND(C, NAME='stream_avx256_pd3d')
            !**************************************************65
            !  Vector memory stream store of floating-point(64-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            !  Linearized (flat) array 3D.
            ! 
            ! calls internaly: vmovntpd
            ! 
            !**************************************************65
         
             use, intrinsic :: iso_c_binding
             implicit none
             real(C_DOUBLE),  dimension(*),           intent(in)       :: src
            
             real(C_DOUBLE),  dimension(*),           intent(inout)    :: dst
             integer(C_INT),                          intent(in),value :: dim1
             integer(C_INT),                          intent(in),value :: dim2
             integer(C_INT),                          intent(in),value :: dim3
             
             
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  maskstore_avx256_ps(src,src_len,mask,dst,dst_len)   BIND(C, NAME='maskstore_avx256_ps')
            !**************************************************65
            !  Vector memory maskstore of floating-point(32-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: vmaskmovps
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT),  dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            integer(C_INT), dimension(8),  intent(in)       :: mask
            real(C_FLOAT),  dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  maskstore_avx256_pd(src,src_len,mask,dst,dst_len)   BIND(C, NAME='maskstore_avx256_pd')
            !**************************************************65
            !  Vector memory maskstore of floating-point(64-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: vmaskmovpd
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),   intent(in)       :: src
            integer(C_INT),                 intent(in),value :: src_len
            integer(C_INT), dimension(8),   intent(in)       :: mask
            real(C_DOUBLE), dimension(*),   intent(inout)    :: dst
            integer(C_INT),                 intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  acos_avx256_ps(src,src_len,dst,dst_len)   BIND(C,NAME='acos_avx256_ps')
            !**************************************************65
            !  Vector 'ACOS' of floating-point(32-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  acos_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='acos_avx256_pd')
            !**************************************************65
            !  Vector 'ACOS' of floating-point(64-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  acosh_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='acosh_avx256_ps')
            !**************************************************65
            !  Vector 'ACOSH' of floating-point(32-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_FLOAT), dimension(*),   intent(in)       :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
         
    end  interface
    
    
    interface
    
         subroutine  acosh_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='acosh_avx256_pd')
            !**************************************************65
            !  Vector 'ACOSH' of floating-point(64-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  asin_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='asin_avx256_ps')
            !**************************************************65
            !  Vector 'ASIN' of floating-point(32-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  asin_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='asin_avx256_pd')
            !**************************************************65
            !  Vector 'ASIN' of floating-point(64-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  asinh_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='asinh_avx256_ps')
            !**************************************************65
            !  Vector 'ASINH' of floating-point(32-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
         
         
         
    end  interface
    
    
    interface
    
         subroutine  asinh_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='asinh_avx256_pd')
            !**************************************************65
            !  Vector 'ASINH' of floating-point(64-bit) 
            !  elements from src  storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  atan2_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='atan2_avx256_ps')
            !**************************************************65
            !  Vector 'ATAN2' of floating-point(32-bit) 
            !  elements from src1 and src2 storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),   intent(in)       :: src1
            integer(C_INT),                intent(in),value :: src1_len
            real(C_FLOAT), dimension(*),   intent(in)       :: src2
            integer(C_INT),                intent(in),value :: src2_len
            real(C_FLOAT), dimension(*),   intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  atan2_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='atan2_avx256_pd')
            !**************************************************65
            !  Vector 'ATAN2' of floating-point(64-bit) 
            !  elements from src1 and src2 storing result in dst
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src1
            integer(C_INT),                intent(in),value :: src1_len
            real(C_DOUBLE), dimension(*),  intent(in)       :: src2
            integer(C_INT),                intent(in),value :: src2_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  atanh_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='atanh_avx256_ps')
            !**************************************************65
            !  Vector 'ATANH' of floating-point(32-bit) 
            !  elements from src  storing result in dst.
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  atanh_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='atanh_avx256_pd')
            !**************************************************65
            !  Vector 'ATANH' of floating-point(64-bit) 
            !  elements from src  storing result in dst.
            !  
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),   intent(in)       :: src
            integer(C_INT),                 intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),   intent(inout)    :: dst
            integer(C_INT),                 intent(in),value :: dst_len
            integer(C_INT),                 intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cos_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='cos_avx256_ps')
            !**************************************************65
            !  Vector 'COS' of floating-point(32-bit) 
            !  elements from src  storing result in dst.
            !  Results returned in radians.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cos_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='cos_avx256_pd')
            !**************************************************65
            !  Vector 'COS' of floating-point(64-bit) 
            !  elements from src  storing result in dst.
            !  Results returned in radians.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cosd_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='cosd_avx256_ps')
            !**************************************************65
            !  Vector 'COS' of floating-point(32-bit) 
            !  elements from src  storing result in dst.
            !  Results returned in degrees.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cosd_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='cosd_avx256_pd')
            !**************************************************65
            !  Vector 'COS' of floating-point(64-bit) 
            !  elements from src  storing result in dst.
            !  Results returned in degrees.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cosh_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='cosh_avx256_ps')
            !**************************************************65
            !  Vector 'COSH' of floating-point(32-bit) 
            !  elements from src  storing result in dst.
            !  Results returned in radians.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cosh_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='cosh_avx256_pd')
            !**************************************************65
            !  Vector 'COSH' of floating-point(64-bit) 
            !  elements from src  storing result in dst.
            !  Results returned in radians.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
         
         
    end  interface
    
    
    interface
    
         subroutine  hypot_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='hypot_avx256_ps')
            !**************************************************65
            !  Vector 'HYPOTENOUSE' of floating-point(32-bit) 
            !  elements from src1 and src2 
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src1
            integer(C_INT),               intent(in),value :: src1_len
            real(C_FLOAT), dimension(*),  intent(in)       :: src2
            integer(C_INT),               intent(in),value :: src2_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  hypot_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)    BIND(C, NAME='hypot_avx256_pd')
            !**************************************************65
            !  Vector 'HYPOTENOUSE' of floating-point(64-bit) 
            !  elements from src1 and src2 
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src1
            integer(C_INT),                intent(in),value :: src1_len
            real(C_DOUBLE), dimension(*),  intent(in)       :: src2
            integer(C_INT),                intent(in),value :: src2_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
         
    
    end  interface
    
    
    interface
    
         subroutine  cbrt_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='cbrt_avx256_ps')
            !**************************************************65
            !  Vector 'Cubic Root' of floating-point(32-bit) 
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65 
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cbrt_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='cbrt_avx256_pd')
            !**************************************************65
            !  Vector 'Cubic Root' of floating-point(64-bit) 
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65 
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cdfnorm_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='cdfnorm_avx256_ps')
            !**************************************************65
            !  Vector 'CDF-Normal Distr.' of floating-point(32-bit) 
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65 
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cdfnorm_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='cdfnorm_avx256_pd')
            !**************************************************65
            !  Vector 'CDF-Normal Distr.' of floating-point(64-bit) 
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cdfnorminv_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='cdfnorminv_avx256_ps')
            !**************************************************65
            !  Vector 'CDF-Inverse Normal Distr.' of floating-point(32-bit) 
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_FLOAT),             intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cdfnorminv_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='cdfnorminv_avx256_pd')
            !**************************************************65
            !  Vector 'CDF-Inverse Normal Distr.' of floating-point(64-bit) 
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cexp_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='cexp_avx256_ps')
            !**************************************************65
            !  Vector 'Complex EXP' of floating-point(32-bit) 
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  cexp_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='cexp_avx256_pd')
            !**************************************************65
            !  Vector 'Complex EXP' of floating-point(64-bit) 
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  clog_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='clog_avx256_ps')
            !**************************************************65
            !  Vector 'Complex LN' of floating-point(32-bit) 
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  clog_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='clog_avx256_pd')
            !**************************************************65
            !  Vector 'Complex LN' of floating-point(64-bit) 
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  erf_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='erf_avx256_ps')
            !**************************************************65
            !  Vector 'Complex ERF' of floating-point(32-bit) 
            !  Error Function.
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  erf_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='erf_avx256_pd')
            !**************************************************65
            !  Vector 'Complex ERF' of floating-point(64-bit) 
            !  Error Function.
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  erfc_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='erfc_avx256_ps')
            !**************************************************65
            !  Vector 'Complex ERFC' of floating-point(32-bit) 
            !  Complementary Error Function.
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in)       :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  erfc_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME ='erfc_avx256_pd')
            !**************************************************65
            !  Vector 'Complex ERFC' of floating-point(64-bit) 
            !  Complementary Error Function.
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  erfcinv_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='erfcinv_avx256_ps')
            !**************************************************65
            !  Vector 'Complex INVERFC' of floating-point(32-bit) 
            !  Complementary Inverse Error Function.
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  erfcinv_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME= 'erfcinv_avx256_pd')
            !**************************************************65
            !  Vector 'Complex INVERFC' of floating-point(64-bit) 
            !  Complementary Inverse Error Function.
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  erfinv_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='erfinv_avx256_ps')
            !**************************************************65
            !  Vector 'Complex INVERF' of floating-point(32-bit) 
            !  Inverse Error Function.
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  erfinv_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='erfinv_avx256_pd')
            !**************************************************65
            !  Vector 'Complex INVERF' of floating-point(64-bit) 
            !  Inverse Error Function.
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  exp10_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='exp10_avx256_ps')
            !**************************************************65
            !  Vector 'Exp base 10' of floating-point(32-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  exp10_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='exp10_avx256_pd')
            !**************************************************65
            !  Vector 'Exp base 10' of floating-point(64-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  exp2_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='exp2_avx256_ps')
            !**************************************************65
            !  Vector 'Exp base 2' of floating-point(32-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  exp2_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='exp2_avx256_pd')
            !**************************************************65
            !  Vector 'Exp base 2' of floating-point(64-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  exp_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='exp_avx256_ps')
            !**************************************************65
            !  Vector 'Exp ' of floating-point(32-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  exp_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='exp_avx256_pd')
            !**************************************************65
            !  Vector 'Exp ' of floating-point(64-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),   intent(in)       :: src
            integer(C_INT),                 intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),   intent(inout)    :: dst
            integer(C_INT),                 intent(in),value :: dst_len
            integer(C_INT),                 intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  expm1_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='expm1_avx256_ps')
            !**************************************************65
            !  Vector 'Expm1 ' of floating-point(32-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  expm1_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='expm1_avx256_pd')
            !**************************************************65
            !  Vector 'Expm1 ' of floating-point(64-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(in)       :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  invcbrt_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='invcbrt_avx256_ps')
            !**************************************************65
            !  Vector 'Inverse Cubic Root ' of floating-point(32-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  invcbrt_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='invcbrt_avx256_pd')
            !**************************************************65
            !  Vector 'Inverse Cubic Root ' of floating-point(64-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  invsqrt_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='invsqrt_avx256_ps')
            !**************************************************65
            !  Vector 'Inverse Square Root ' of floating-point(32-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
         
    end  interface
    
    
    interface
    
         subroutine  invsqrt_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='invsqrt_avx256_pd')
            !**************************************************65
            !  Vector 'Inverse Square Root ' of floating-point(32-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  log10_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='log10_avx256_ps')
            !**************************************************65
            !  Vector 'Logarithm base-10 ' of floating-point(32-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  log10_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='log10_avx256_pd')
            !**************************************************65
            !  Vector 'Logarithm base-10 ' of floating-point(64-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  log1p_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='log1p_avx256_ps')
            !**************************************************65
            !  Vector 'Logarithm of 1 ' of floating-point(64-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine

    end  interface
    
    
    interface
    
         subroutine  log1p_avx256_pd(src,src_len,dst,dst_len)  BIND(C, NAME='log1p_avx256_pd')
            !**************************************************65
            !  Vector 'Logarithm of 1 ' of floating-point(64-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  idiv_avx256_epi32(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='idiv_avx256_epi32')
            !**************************************************65
            !  Vector 'integral division ' of integers (32-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT), dimension(*),  intent(in)       :: src1
            integer(C_INT),                intent(in),value :: src1_len
            integer(C_INT), dimension(*),  intent(in)       :: src2
            integer(C_INT),                intent(in),value :: src2_len
            integer(C_INT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  log2_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='log2_avx256_ps')
            !**************************************************65
            !  Vector 'Logarithm 2' of floating-point(32-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  log2_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='log2_avx256_pd')
            !**************************************************65
            !  Vector 'Logarithm  2' of floating-point(64-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in) :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
         
         subroutine  logb_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='logb_avx256_ps')
            !**************************************************65
            !  Vector 'floor(log2(x))' of floating-point(32-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  logb_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='logb_avx256_pd')
            !**************************************************65
            !  Vector 'floor(log2(x))' of floating-point(64-bit) 
            !  
            !  elements from src array
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  pow_avx256_ps(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='pow_avx256_ps')
            !**************************************************65
            !  Vector 'POW' of floating-point(32-bit) 
            !  
            !  elements from src1 array and src2 array, 
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src1
            integer(C_INT),               intent(in),value :: src1_len
            real(C_FLOAT), dimension(*),  intent(in)       :: src2
            integer(C_INT),               intent(in),value :: src2_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  pow_avx256_pd(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='pow_avx256_pd')
            !**************************************************65
            !  Vector 'POW' of floating-point(64-bit) 
            !  
            !  elements from src1 array  and src2 array,
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src1
            integer(C_INT),                intent(in),value :: src1_len
            real(C_DOUBLE), dimension(*),  intent(in)       :: src2
            integer(C_INT),                intent(in),value :: src2_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  rem_avx256_epi32(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='rem_avx256_epi32')
            !**************************************************65
            !  Vector 'Division Remainder' of integer(32-bit) 
            !  
            !  elements from src1 array  and src2 array,
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_INT), dimension(*),  intent(in)       :: src1
            integer(C_INT),                intent(in),value :: src1_len
            integer(C_INT), dimension(*),  intent(in)       :: src2
            integer(C_INT),                intent(in),value :: src2_len
            integer(C_INT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  rem_avx256_epi64(src1,src1_len,src2,src2_len,dst,dst_len)   BIND(C, NAME='rem_avx256_epi64')
            !**************************************************65
            !  Vector 'Division Remainder' of integer(64-bit) 
            !  
            !  elements from src1 array  and src2 array,
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            integer(C_LONG_LONG), dimension(*),  intent(in)       :: src1
            integer(C_LONG_LONG),                intent(in),value :: src1_len
            integer(C_LONG_LONG), dimension(*),  intent(in)       :: src2
            integer(C_LONG_LONG),                intent(in),value :: src2_len
            integer(C_LONG_LONG), dimension(*),  intent(inout)    :: dst
            integer(C_LONG_LONG),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  sind_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='sind_avx256_ps')
            !**************************************************65
            !  Vector 'Sine' of floating-point(32-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  sind_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='sind_avx256_pd')
            !**************************************************65
            !  Vector 'Sine' of floating-point(64-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  sinh_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='sinh_avx256_ps')
            !**************************************************65
            !  Vector 'Hyperbolic Sine' of floating-point(32-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    
    end  interface
    
    
    interface
    
         subroutine  sinh_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='sinh_avx256_pd')
            !**************************************************65
            !  Vector 'Hyperbolic Sine' of floating-point(64-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)        :: src
            integer(C_INT),                intent(in),value  :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)     :: dst
            integer(C_INT),                intent(in),value  :: dst_len
            integer(C_INT),                intent(inout)     :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  tan_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='tan_avx256_ps')
            !**************************************************65
            !  Vector 'Tan' of floating-point(32-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  tan_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='tan_avx256_pd')
            !**************************************************65
            !  Vector 'Tan' of floating-point(64-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            ! 
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  tand_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='tand_avx256_ps')
            !**************************************************65
            !  Vector 'Tan' of floating-point(32-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            integer(C_INT),               intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  tand_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='tand_avx256_pd')
            !**************************************************65
            !  Vector 'Tan' of floating-point(64-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*), intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_DOUBLE), dimension(*), intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  tanh_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='tanh_avx256_ps')
            !**************************************************65
            !  Vector 'Tanh' of floating-point(32-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  tanh_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='tanh_avx256_pd')
            !**************************************************65
            !  Vector 'Tanh' of floating-point(64-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in) :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            integer(C_INT),                intent(inout)    :: fperr
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  trunc_avx256_ps(src,src_len,dst,dst_len)   BIND(C, NAME='trunc_avx256_ps')
            !***************************************************65
            !  Vector truncation of floating-point(32-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  trunc_avx256_pd(src,src_len,dst,dst_len)   BIND(C, NAME='trunc_avx256_pd')
            !***************************************************65
            !  Vector truncation of floating-point(64-bit) 
            !  
            !  elements from src array,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly: decomposed in many machine 
            ! code instructions.
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  fmadd_avx256_ps(src1,src2,src3,src_len,dst,dst_len)   BIND(C, NAME='fmadd_avx256_ps')
            !***************************************************65
            !  Vector 'FMADD' of floating-point(32-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfmaddxxxps, where xxx stands
            ! for 1,2,3 for instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src1
            real(C_FLOAT), dimension(*),  intent(in)       :: src2
            real(C_FLOAT), dimension(*),  intent(in)       :: src3
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  fmadd_avx256_pd(src1,src2,src3,src_len,dst,dst_len)   BIND(C, NAME='fmadd_avx256_pd')
            !***************************************************65
            !  Vector 'FMADD' of floating-point(64-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfmaddsubxxxpd, where xxx stands
            ! for 1,2,3 instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*), intent(in)       :: src1
            real(C_DOUBLE), dimension(*), intent(in)       :: src2
            real(C_DOUBLE), dimension(*), intent(in)       :: src3
            integer(C_CINT),              intent(in),value :: src_len
            real(C_DOUBLE), dimension(*), intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  fmaddsub_avx256_ps(src1,src2,src3,src_len,dst,dst_len)  BIND(C, NAME='fmaddsub_avx256_ps')
            !***************************************************65
            !  Vector 'FMADDSUB' of floating-point(32-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfmaddsubxxxpd, where xxx stands
            ! for 1,2,3 instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*), intent(in)       :: src1
            real(C_FLOAT), dimension(*), intent(in)       :: src2
            real(C_FLOAT), dimension(*), intent(in)       :: src3
            integer(C_INT),              intent(in),value :: src_len
            real(C_FLOAT), dimension(*), intent(inout)    :: dst
            integer(C_INT),              intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  fmaddsub_avx256_pd(src1,src2,src3,src_len,dst,dst_len)  BIND(C, NAME='fmaddsub_avx256_pd')
            !***************************************************65
            !  Vector 'FMADDSUB' of floating-point(64-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfmaddsubxxxpd, where xxx stands
            ! for 1,2,3 instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src1
            real(C_DOUBLE), dimension(*),  intent(in)       :: src2
            real(C_DOUBLE), dimension(*),  intent(in)       :: src3
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  fmsub_avx256_ps(src1,src2,src3,src_len,dst,dst_len)   BIND(C, NAME='fmsub_avx256_ps')
            !***************************************************65
            !  Vector 'FMADDSUB' of floating-point(32-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfmsubxxxpd, where xxx stands
            ! for 1,2,3 instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*), intent(in)       :: src1
            real(C_FLOAT), dimension(*), intent(in)       :: src2
            real(C_FLOAT), dimension(*), intent(in)       :: src3
            integer(C_INT),              intent(in),value :: src_len
            real(C_FLOAT), dimension(*), intent(inout)    :: dst
            integer(C_INT),              intent(in),value :: dst_len
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  fmsub_avx256_pd(src1,src2,src3,src_len,dst,dst_len)   BIND(C, NAME='fmsub_avx256_pd')
            !***************************************************65
            !  Vector 'FMADDSUB' of floating-point(64-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfmsubxxxpd, where xxx stands
            ! for 1,2,3 instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*),  intent(in)       :: src1
            real(C_DOUBLE), dimension(*),  intent(in)       :: src2
            real(C_DOUBLE), dimension(*),  intent(in)       :: src3
            integer(C_INT),                intent(in),value :: src_len
            real(C_DOUBLE), dimension(*),  intent(inout)    :: dst
            integer(C_INT),                intent(in),value :: dst_len
         end  subroutine 
    
    end  interface
    
    
    interface
    
         subroutine  fmsubadd_avx256_ps(src1,src2,src3,src_len,dst,dst_len)  BIND(C, NAME='fmsubadd_avx256_ps')
            !***************************************************65
            !  Vector 'FMSUBADD' of floating-point(32-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfmsubxxxpd, where xxx stands
            ! for 1,2,3 instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*), intent(in)       :: src1
            real(C_FLOAT), dimension(*), intent(in)       :: src2
            real(C_FLOAT), dimension(*), intent(in)       :: src3
            integer(C_INT),              intent(in),value :: src_len
            real(C_FLOAT), dimension(*), intent(inout)    :: dst
            integer(C_INT),              intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  fmsubadd_avx256_pd(src1,src2,src3,src_len,dst,dst_len)   BIND(C, NAME='fmsubadd_avx256_pd')
            !***************************************************65
            !  Vector 'FMSUBADD' of floating-point(64-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfmsubxxxpd, where xxx stands
            ! for 1,2,3 instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*), intent(in)       :: src1
            real(C_DOUBLE), dimension(*), intent(in)       :: src2
            real(C_DOUBLE), dimension(*), intent(in)       :: src3
            integer(C_INT),               intent(in),value :: src_len
            real(C_DOUBLE), dimension(*), intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  fnmadd_avx256_ps(src1,src2,src3,src_len,dst,dst_len)   BIND(C, NAME='fnmadd_avx256_ps')
            !***************************************************65
            !  Vector 'FNMADD' of floating-point(32-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfnmaddxxxpd, where xxx stands
            ! for 1,2,3 instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*), intent(in)       :: src1
            real(C_FLOAT), dimension(*), intent(in)       :: src2
            real(C_FLOAT), dimension(*), intent(in)       :: src3
            integer(C_INT),              intent(in),value :: src_len
            real(C_FLOAT), dimension(*), intent(inout)    :: dst
            integer(C_INT),              intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  fnmadd_avx256_pd(src1,src2,src3,src_len,dst,dst_len)   BIND(C, NAME='fnmadd_avx256_pd')
            !***************************************************65
            !  Vector 'FNMADD' of floating-point(64-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfnmaddxxxpd, where xxx stands
            ! for 1,2,3 instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*), intent(in)       :: src1
            real(C_DOUBLE), dimension(*), intent(in)       :: src2
            real(C_DOUBLE), dimension(*), intent(in)       :: src3
            integer(C_INT),               intent(in),value :: src_len
            real(C_DOUBLE), dimension(*), intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  fnmsub_avx256_ps(src1,src2,src3,src_len,dst,dst_len)   BIND(C, NAME='fnmsub_avx256_ps')
            !***************************************************65
            !  Vector 'FNMSUB' of floating-point(64-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfnmaddxxxpd, where xxx stands
            ! for 1,2,3 instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_FLOAT), dimension(*),  intent(in)       :: src1
            real(C_FLOAT), dimension(*),  intent(in)       :: src2
            real(C_FLOAT), dimension(*),  intent(in)       :: src3
            integer(C_INT),               intent(in),value :: src_len
            real(C_FLOAT), dimension(*),  intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
    
    interface
    
         subroutine  fnmsub_avx256_pd(src1,src2,src3,src_len,dst,dst_len)   BIND(C, NAME='fnmsub_avx256_pd')
            !***************************************************65
            !  Vector 'FNMSUB' of floating-point(64-bit) 
            !  
            !  elements from src1,src2,src3 arrays,
            !  storing result in dst.
            !  Unaligned data. 
            !  Expressed in degrees.
            ! 
            ! calls internaly:   vfnmaddxxxpd, where xxx stands
            ! for 1,2,3 instruction name.
            !
            ! 
            !**************************************************65
         
            use, intrinsic :: iso_c_binding
            implicit none
            real(C_DOUBLE), dimension(*), intent(in)       :: src1
            real(C_DOUBLE), dimension(*), intent(in)       :: src2
            real(C_DOUBLE), dimension(*), intent(in)       :: src3
            integer(C_INT),               intent(in),value :: src_len
            real(C_DOUBLE), dimension(*), intent(inout)    :: dst
            integer(C_INT),               intent(in),value :: dst_len
            
         end  subroutine
    
    end  interface
    
end  module