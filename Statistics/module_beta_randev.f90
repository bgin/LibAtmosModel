
module mod_beta_randev

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_beta_randev'
 !          
 !          Purpose:
 !                        Helper subroutines for initializing array fields
 !                        to random deviation from Beta Distribution.
 !
 !          History:
 !                        Date: 18-07-2017
 !                        Time: 09:51 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !           
 !                 Bernard Gingold 
 !
 !         
 !          
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
    use module_kinds
    use module_class_error_check, only : array1D_not_alloc,array2D_not_alloc,   &
                                         array3D_not_alloc,array4D_not_alloc,   &
                                         conform_dim1D_Real,conform_dim2D_Real, &
                                         conform_dim3D_Real,conform_dim4D_Real
    
    use module_statistics, only : rdbeta,rduni
    use ifcore
    implicit none
    
    public ::  init_field1D_rdevbeta, &
               init_field2D_rdevbeta, &
               init_field3D_rdevbeta, &
               init_field4D_rdevbeta
 
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_BETA_RANDEV_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_BETA_RANDEV_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_BETA_RANDEV_MICRO = 0
    
    ! Full version
    integer(I32P), parameter, public :: MOD_BETA_RANDEV_FULLVER = 1000*MOD_BETA_RANDEV_MAJOR + 100*MOD_BETA_RANDEV_MINOR + &
                                                          10*MOD_BETA_RANDEV_MICRO
    
    ! File creation date
    character(*), parameter,  public :: MOD_BETA_RANDEV_CREATION_DATE="15-07-2017 10:31 AM GMT+2 (15 SAT JULY 2017 -00200)"
    
    ! File build date should be initialized after every successful build
    character(*), parameter,  public :: MOD_BETA_RANDEV_BUILD_DATE=" "
    
    ! File/module name
    character(*), parameter,  public :: MOD_BETA_RANDEV_FMODNAME="module_beta_randev.f90"
    
    ! File author info
    character(*), parameter,  public :: MOD_BETA_RANDEV_AUTHOR="Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! File description
    character(*), parameter,  public :: MOD_BETA_RANDEV_DESCRIPTION="Helper subroutines for user fields initialization"
    
    contains
    
    
    
    !==========================================================================80
    ! subroutine:
    !               init_field1D_rdevbeta
    !
    !   
    !==========================================================================80
    subroutine init_field1D_rdevbeta(f1d,p,q,status,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          
          implicit none
          real(R64P), dimension(:),    intent(out)   :: f1d
!DIR$     ASSUME_ALIGNED f1d:32
          real(R64P), dimension(:),    intent(in)    :: p,q
!DIR$     ASSUME_ALIGNED p:32, q:32
          integer(I32P),               intent(inout) :: status
          logical(I32P), dimension(5), intent(inout) :: fp_flags
         
          ! Locals
          integer(I64P)                              :: i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF         
          ! Start of executable statements
          ! Sanity checking of input arguments
         
          if(array1D_not_alloc(f1d) .OR. &
             array1D_not_alloc(p)   .OR. &
             array1D_not_alloc(q)         ) then
               status = -1
               return
          end if
          if(conform_dim1D_real(f1d,p) .OR. &
             conform_dim1D_real(f1d,q)       ) then
                status = -2
                return
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF         
          do i = 1, SIZE(f1d)
              f1d(i) = rdbeta(p(i),q(i))
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
            status = -3
             write(ERROR_UNIT,*) "================================================================="
             write(ERROR_UNIT,*) " init_field1D_rdevbeta: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF            
    
    end subroutine              
    
    !==========================================================================80
    !  subroutine:
    !              init_field2D_rdevbeta
    !==========================================================================80
    subroutine init_field2D_rdevbeta(f2d,p,q,status,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          real(R64P), dimension(:,:),  intent(out)    :: f2d
!DIR$     ASSUME_ALIGNED f2d:32
          real(R64P), dimension(:,:),  intent(in)     :: p,q
!DIR$     ASSUME_ALIGNED p:32, q:32
          integer(I32P),               intent(inout)  :: status
          logical(I32P), dimension(5), intent(inout)  :: fp_flags
          ! Locals
          integer(I64P)                               :: i,j
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF          
          ! Start of executable statements
          ! Sanity checking of input arguments
          if(array2D_not_alloc(f2d) .OR. &
             array2D_not_alloc(p)   .OR. &
             array2D_not_alloc(q)        ) then
              status = -1
              return
          end if
          if(conform_dim2D_Real(f1d,p) .OR. &
             conform_dim2D_Real(f1d,q)      ) then
              status = -2
              return
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          do j = 1, SIZE(f2d,dim=1)
              do i = 1, SIZE(f2d,dim=2)
                  f2d(i,j) = rdbeta(p(i,j),q(i,j))
              end do
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
            status = -3
             write(ERROR_UNIT,*) "================================================================="
             write(ERROR_UNIT,*) " init_field2D_rdevbeta: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
    
    !==========================================================================80
    ! subroutine:
    !              init_field3D_rdevbeta
    !==========================================================================80
    subroutine init_field3D_rdevbeta(f3d,p,q,status,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          real(R64P), dimension(:,:,:), intent(out)   :: f3d
!DIR$     ASSUME_ALIGNED f3d:32
          real(R64P), dimension(:,:,:), intent(in)    :: p,q
!DIR$     ASSUME_ALIGNED p:32, q:32
          integer(I32P),                intent(inout) :: status
          logical(I32P), dimension(5),  intent(inout) :: fp_flags
          ! Locals
          integer(I64P)                               :: k,j,i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF 
          ! Start of executable statements
          ! Sanity check on input arguments
          if(array3D_not_alloc(f3d) .OR. &
             array3D_not_alloc(p)   .OR. &
             array3D_not_alloc(q)        )   then
                status = -1
                return
          end if
          if(conform_dim3D_Real(f3d,p) .OR. &
             conform_dim3D_Real(f3d,q)       ) then
              status = -2
              return
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          do j = 1, SIZE(f3d,dim=1)
              do k = 1, SIZE(f3d,dim=2)
                  do i = 1, SIZE(f3d,dim=3)
                      f3d(i,k,j) = rdbeta(p(i,k,j),q(i,k,j))
                  end do
              end do
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
            status = -3
             write(ERROR_UNIT,*) "================================================================="
             write(ERROR_UNIT,*) " init_field3D_rdevbeta: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
    
    !==========================================================================80
    !  subroutine:
    !               init_field4D_rdevbeta
    !==========================================================================80
    subroutine init_field4D_rdevbeta(f4d,p,q,status,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          real(R64P), dimension(:,:,:,:), intent(out)   :: f4d
!DIR$     ASSUME_ALIGNED f4d:32
          real(R64P), dimension(:,:,:,:), intent(in)    :: p,q
!DIR$     ASSUME_ALIGNED p:32, q:32
          integer(I32P),                  intent(inout) :: status
          logical(I32P), dimension(5),    intent(inout) :: fp_flags
          ! Locals
          integer(I64P)                                 :: l,j,k,i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of input arguments
          if(array4D_not_alloc(f4d) .OR. &
             array4D_not_alloc(p)   .OR. &
             array4D_not_alloc(q)        ) then
              status = -1
              return
          end if
          if(conform_dim4D_Real(f4d,p) .OR. &
             conform_dim4D_Real(f4d,q)     ) then
              status = -2
              return
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          do l = 1, SIZE(f4d,dim=1)
              do k = 1, SIZE(f4d,dim=2)
                  do j = 1, SIZE(f4d,dim=3)
                      do i = 1, SIZE(f4d,dim=4)
                          f4d(i,j,k,l) = rdbeta(p(i,j,k,l),q(i,j,k,l))
                      end do
                  end do
              end do
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
            status = -3
             write(ERROR_UNIT,*) "================================================================="
             write(ERROR_UNIT,*) " init_field4D_rdevbeta: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF            
    end subroutine
    
end module