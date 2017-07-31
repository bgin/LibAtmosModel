
module mod_wrap_mt95


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_wrap_mt95'
 !          
 !          Purpose:
 !                        Helper subroutines for initializing array fields
 !                        by means of calls to Mersenne-Twister random
 !                        number generators.
 !                        This is wrapper around module 'mt95'
 !
 !          History:
 !                        Date: 18-07-2017
 !                        Time: 15:06 GMT+2
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
    implicit none
    use mt95, only : genrand_init,genrand_real1,  &
                     genrand_real2,genrand_real3, &
                     genrand_res53
    use module_kinds
    use module_class_error_check, only : array1D_not_alloc,array2D_not_alloc,   &
                                         array3D_not_alloc,array4D_not_alloc
                                        
   
    use  ifcore
    
     public :: init_field1D_mt1,  &
               init_field2D_mt1,  &
               init_field3D_mt1,  &
               init_field1D_mt2,  &
               init_field2D_mt2,  &
               init_field3D_mt2,  &
               init_field1D_mt3,  &
               init_field2D_mt3,  &
               init_field3D_mt3,  &
               init_field1D_mt53, &
               init_field2D_mt53, &
               init_field3D_mt53
     
   
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(I32P), parameter, public :: MOD_WRAP_MT95_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_WRAP_MT95_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_WRAP_MT95_MICRO = 0
    
    ! Full version
    integer(I32P) ,parameter, public :: MOD_WRAP_MT95_FULLVER = 1000*MOD_WRAP_MT95_MAJOR + 100*MOD_WRAP_MT95_MINOR + &
                                                                10*MOD_WRAP_MT95_MICRO
    
    ! Creation date
    character(*), parameter, public :: MOD_WRAP_MT95_CREATION_DATE="15-07-2017 10:31 AM GMT+2 (15 SAT JULY 2017 -00200)"
    
    ! Build date , should be initialized to latest
    ! build date
    character(*), parameter, public :: MOD_WRAP_MT95_BUILD_DATE=" "
    
    ! File name
    character(*), parameter, public :: MOD_WRAP_MT95_FNAME="module_mt.f90"
    
    ! File author
    character(*), parameter, public :: MOD_WRAP_MT95_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail"
    
    ! File description
    character(*), parameter, public :: MOD_WRAP_MT95_DESCRIPT="Module wrapper around MT95"
    
    real(R64P), parameter, private :: sfactor = 1.25_R64P
    
    contains
    
    !==========================================================================80
    ! subroutine:
    !              init_field1D_mt1 
    !==========================================================================80
    subroutine init_field1D_mt1(f1d,put,puta,ss,ssa,its,ite,   &
                                option,status,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:),    intent(out)            :: f1d
!DIR$     ASSUME_ALIGNED f1d:32
          integer(I32P),               intent(in)             :: put
          integer(I32P), dimension(:), intent(in)             :: puta               
          real(R64P),                  intent(inout),optional :: ss
          real(R64P), dimension(:),    intent(inout),optional :: ssa
          integer(I64P),               intent(in)             :: its,ite
          integer(I32P),               intent(inout)          :: status
          character(len=*),            intent(in)             :: option
          logical(I32P), dimension(5), intent(inout)          :: fp_flags
          ! Locals
          integer(I64P)                           :: i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check on input arguments
          if(array1D_not_alloc(f1d) .OR.
             array1D_not_alloc(puta)) then
              status = -1
              return
          end if
          if(present(ss)) then
             if(DABS(ss) .EQ. 0.0_R64P .OR. &
                DABS(ss) .EQ. 1.0_R64P    ) then
                ss = sfactor
             end if
          else if(present(ssa)) then
              do i = its, ite
                  if(DABS(ssa(i)) .EQ. 0.0_R64P .OR. &
                     DABS(ssa(i)) .EQ. 1.0_R64P )  then
                      ssa(i) = sfactor
                  end if
              end do
          end if
          
          if(ANY(fp_flags)) then
              fp_flags = .false._I32P
          end if
init:     select case (option)
          case ("scalar")
              call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
              call genrand_real1(f1d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field1D_mt1s: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF
              if(present(ss)) then
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                  
                  do i = its, ite
                      f1d(i) = f1d(i) * ss
                  end do
              else if(present(ssa)) then
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                  do i = its, ite
                      f1d(i) = f1d(i) * ssa(i)
                  end do
              end if
          case ("array")
              call genrand_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
              call genrad_real1(f1d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field1D_mt1s: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF
              if(present(ss)) then
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                  do i = its, ite
                      f1d(i) = f1d(i) * ss
                  end do
              else if(present(ssa)) then
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                  do i = its, ite
                      f1d(i) = f1d(i) * ssa(i)
                  end do
              end if
           case default
              status = -3
              return
           end select init
    end subroutine      
    
    !==========================================================================80
    ! subroutine:
    !              init_field2D_mt1
    !==========================================================================80
    subroutine init_field2D_mt1(f2d,put,puta,option,its,ite,&
                                kts,kte,ss,ssa,status,fp_flags   )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:),  intent(out)             :: f2d
!DIR$     ASSUME_ALIGNED f2d:32
          integer(I32P),               intent(in)              :: put
          integer(I32P), dimension(:), intent(in)              :: puta
          character(len=*),            intent(in)              :: option
          integer(I64P),               intent(in)              :: its,ite, &
                                                                  kts,kte
          real(R64P),                  intent(inout), optional :: ss
          real(R64P), dimension(:,:),  intent(inout), optional :: ssa
          integer(I32P),               intent(inout)           :: status
          logical(I32P), dimension(5), intent(inout)           :: fp_flags
          ! Locals
          integer(I64P)                         :: i,k
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable  statements
          ! Sanity check of input arguments
          if(array2D_not_alloc(f2d) .OR.
             array1D_not_alloc(puta)) then
              status = -1
              return
          end if
          if(present(ss)) then
              if(DABS(ss) .EQ. 0.0_R64P .OR. &
                 DABS(ss) .EQ. 1.0_R64P     ) then
                  ss = sfactor
              end if
          else if(present(ssa)) then
               do k = kts, kte
                   do i = its, ite
                       if(DABS(ssa(i,k)) .EQ. 0.0_R64P .OR. &
                          DABS(ssa(i,k)) .EQ. 1.0_R64P  ) then
                           ssa(i,k) = sfactor
                      end if
                   end do
               end do
          end if
    
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
init:     select case (option)
          case ("scalar")
               call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF    
               call genrand_real1(f2d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field2D_mt1: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF    
               if(present(ss)) then
                   do k = kts, kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ss
                       end do
                   end do
               else if(present(ssa)) then
                   do k = kts, kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                            
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ssa(i,k)
                       end do
                   end do
               end if
               
          case ("array")
              call genrad_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
              call genrad_real1(f2d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field2D_mt1: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF    
               if(present(ss)) then
                   do k = kts, kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ss
                       end do
                   end do
               else if(present(ssa)) then
                   do k = kts, kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                            
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ssa(i,k)
                       end do
                   end do
               end if
               
          case default      
               status = -3
               return
          end select init     
    end subroutine
    
    !==========================================================================80
    ! subroutine:
    !             init_field3D_mt1
    !==========================================================================80    
    subroutine init_field3D_mt1(f3d,put,puta,option,its,ite, &
                                kts,kte,jts,jte,ss,ssa,status,fp_flags   )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:,:), intent(out)             :: f3d
          integer(I32P),                intent(in)              :: put
          integer(I32P), dimension(:),  intent(in)              :: puta
          character(len=*),             intent(in)              :: option
          integer(I64P),                intent(in)              :: its,ite, &
                                                                  kts,kte, &
                                                                  jts,jte
          real(R64P),                   intent(inout),optional  :: ss
          real(R64P), dimension(:,:,:), intent(inout),optional  :: ssa
          integer(I32P),                intent(inout)           :: status
          logical(I32P), dimension(5),  intent(inout)           :: fp_flags
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          integer(I64P)                           :: j,k,i
          ! Start of executable statements
          ! Sanity check of input arguments
          if(array3D_not_alloc(f3d) .OR. &
             array1D_not_alloc(puta) )  then
               status = -1
               return
          end if
          if(present(ss)) then
              if(DABS(ss) .EQ. 0.0_R64P .OR. &
                 DABS(ss) .EQ. 1.0_R64P ) then
                  ss = sfactor
              end if
          else if(present(ssa)) then
              do j = jts, jte
                  do k = kts, kte
                      do i = its, ite
                          if(DABS(ssa(i,k,j)) .EQ. 0.0_R64P .OR. &
                             DABS(ssa(i,k,j)) .EQ. 1.0_R64P ) then
                              ssa(i,k,j) = sfactor
                          end if
                      end do
                  end do
              end do
          end if
          
              
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
init:     select case (option)
          case ("scalar")
               call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF     
               call genrand_real1(f3d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field3D_mt1: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF    
               if(present(ss)) then
                   do j = jts, jte   
                       do k = kts, kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                                
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ss
                           end do
                       end do
                   end do
               else if(present(ssa)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                                
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ssa(i,k,j)
                           end do
                       end do
                   end do
               end if
               
          case ("array")
              call genrad_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF    
               call genrad_real1(f3d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field3D_mt1: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF    
               if(present(ss)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                                
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ss
                           end do
                       end do
                   end do
               else if(present(ssa)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                                
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ssa(i,k,j)
                           end do
                       end do
                   end do
               end if
          case default
               status = -3
               return
          end select init
    end subroutine
    
    !==========================================================================80
    ! subroutine:
    !             init_field1D_mt2
    ! Calls:
    !              Overloaded genrand_real2,
    !              Overloaded genrand_init
    !==========================================================================80
    subroutine init_field1D_mt2(f1d,put,puta,option,its,ite, &
                                 ss,ssa,status,fp_flags        )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:),    intent(out)            :: f1d
!DIR$     ASSUME_ALIGNED f1d:32     
          integer(I32P),               intent(in)             :: put
          integer(I32P), dimension(:), intent(in)             :: puta
          character(len=*),            intent(in)             :: option
          integer(I64P),               intent(in)             :: its,ite
          real(R64P),                  intent(inout),optional :: ss
          real(R64P), dimension(:),    intent(inout),optional :: ssa
!DIR$     ASSUME_ALIGNED ssa:32          
          integer(I32P),               intent(inout)          :: status
          logical(I32P), dimension(5), intent(inout)          :: fp_flags
          ! Locals
          integer(I32P)                :: i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity chaeck of input arguments
          status = 0
          if(array1D_not_alloc(f1d)) then
              status = -1
              return
          end if
          if(present(ss)) then
              if(DABS(ss) .EQ. 0.0_R64P .OR. &
                 DABS(ss) .EQ. 1.0_R64P  ) then
                  ss = sfactor
              end if
          else if(present(ssa)) then
              do i = its, ite
                  if(DABS(ssa(i)) .EQ. 0.0_R64P .OR. &
                     DABS(ssa(i)) .EQ. 1.0_R64P ) then
                      ssa(i) = sfactor
                  end if
              end do
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
init:     select case (option)
          case ("scalar")
               call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF    
               call genrand_real2(f1d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field1D_mt2: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF 
               if(present(ss)) then
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                   do i = its, ite
                       f1d(i) = f1d(i) * ss
                   end do
               else if(present(ssa)) then
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                        
                   do i = its, ite
                       f1d(i) = f1d(i) * ssa(i)
                   end do
               end if
          case ("array")
              call genrand_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
               call genrand_real2(f1d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field1D_mt2: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF 
               if(present(ss)) then
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                        
                   do i = its, ite
                       f1d(i) = f1d(i) * ss
                   end do
               else if(present(ssa)) then
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                        
                   do i = its, ite
                       f1d(i) = f1d(i) * ssa(i)
                   end do
               end if
          case default
               status = -3
               return
          end select init
    end subroutine
    
    !==========================================================================80
    ! subroutine:
    !               init_field2D_mt2
    ! Calls:
    !               Overloaded genrand_init
    !               Overloaded genrand_real2
    !==========================================================================80
    subroutine init_field2D_mt2(f2d,put,puta,option,its,ite, &
                                kts,kte,ss,ssa,status,fp_flags   )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:),  intent(out)             :: f2d
!DIR$     ASSUME_ALIGNED f2d:32          
          integer(I32P),               intent(in)              :: put
          integer(I32P), dimension(:), intent(in)              :: puta
          character(len=*),            intent(in)              :: option
          integer(I64P),               intent(in)              :: its,ite, &
                                                                  kts,kte
          real(R64P),                  intent(inout),optional  :: ss
          real(R64P), dimension(:,:),  intent(inout),optional  :: ssa
!DIR$     ASSUME_ALIGNED ssa:32          
          integer(I32P),               intent(inout)           :: status
          logical(I32P), dimension(5), intent(inout)           :: fp_flags
          ! Locals
          integer(I64P)                           :: k,i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of input arguments
          status = 0
          if(array2D_not_alloc(f2d) .OR. &
             array2D_not_alloc(ssa)    ) then
              status = -1
              return
          end if
          if(present(ss)) then
              if(DABS(ss) .EQ. 0.0_R64P .OR. &
                 DABS(ss) .EQ. 1.0_R64P  ) then
                  ss = sfactor
              end if
           
           else if(present(ssa)) then
              do k = kts, kte
                  do i = its, ite
                      if(DABS(ssa(i,k)) .EQ. 0.0_R64P .OR. &
                         DABS(ssa(i,k)) .EQ. 1.0_R64P  ) then
                          ssa(i,k) = sfactor
                      end if
                 end do
             end do
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
init:     select case (option)
          case ("scalar")
               call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF    
               call genrand_real2(f2d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field2D_mt2: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF     
               if(present(ss)) then
                   do k = kts, kte
!DIR$              SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ss
                       end do
                   end do
               
               else if(present(ssa)) then
                   do k = kts, kte
!DIR$              SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ssa(i,k)
                       end do
                   end do
               end if
          case ("array")
               call genrand_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF    
               call genrand_real2(f2d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field2D_mt2: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF     
               if(present(ss)) then
                   do k = kts, kte
!DIR$              SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ss
                       end do
                   end do
               else if(present(ssa)) then
                   do k = kts, kte
!DIR$               SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ssa(i,k)
                       end do
                   end do
               end if
          case default
               status = -3
               return
          end select init
    end subroutine
    
    !==========================================================================80
    !  subroutine:    
    !               init_field3D_mt2
    !  Calls:
    !               Overloaded genrand_init
    !               Overloaded genrand_real2
    !==========================================================================80
    subroutine init_field3D_mt2(f3d,put,puta,option,its,ite, &
                                kts,kte,jts,jte,ss,ssa,status,fp_flags )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:,:), intent(out) :: f3d
!DIR$     ASSUME_ALIGNED f3d:32
          integer(I32P),                intent(in)            :: put
          integer(I32P), dimension(:),  intent(in)            :: puta
          character(len=*),             intent(in)            :: option
          integer(I64P),                intent(in)            ::   its,ite, &
                                                                   kts,kte, &
                                                                   jts,jte
          real(R64P),                   intent(inout),optional :: ss
          real(R64P), dimension(:,:,:), intent(inout),optional :: ssa
!DIR$     ASSUME_ALIGNED ssa:32          
          integer(I32P),                intent(inout)          :: status
          logical(I32P), dimension(5),  intent(inout)          :: fp_flags
          ! Locals
          integer(I64P)                 :: j,k,i
          
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)        :: status_value
!DIR$ ENDIF
          ! Start of executable statememts
          ! Sanity checking of input arguments
          status = 0
          if(array3D_not_alloc(f3d) .OR. &
             array3D_not_alloc(ssa)     ) then
              status = -1
              return
          end if
          if(present(ss)) then
              if(DABS(ss) .EQ. 0.0_R64P .OR. &
                 DABS(ss) .EQ. 1.0_R64P    ) then
                  ss = sfactor
              end if
          else if(present(ssa)) then
              do j = jts, jte
                  do k = kts, kte
                      do i = its, ite
                          if(DABS(ssa(i,k,j)) .EQ. 0.0_R64P .OR. &
                             DABS(ssa(i,k,j)) .EQ. 1.0_R64P   ) then
                                 ssa(i,k,j) = sfactor
                          end if
                      end do
                  end do
              end do
          end if  
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
init:     select case (option)
          case ("scalar")
               call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF    
               call genrand_real2(f3d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field3D_mt2: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF    
               if(present(ss)) then
                  
                   do j = jts, jte
                       do k = kts, kte
!DIR$                   SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ss
                           end do
                       end do
                   end do
               else if(present(ssa)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$                  SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ssa(i,k,j)
                           end do
                       end do
                   end do
               end if
          case ("array")
              call genrand_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
               call genrand_real2(f3d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field3D_mt2: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF 
               if(present(ss)) then
                  
                   do j = jts, jte
                       do k = kts, kte
 !DIR$                 SIMD VECTORLENGTHFOR(REAL(KIND=8))         
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ss
                           end do
                       end do
                   end do
               else if(present(ssa)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$                   SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ssa(i,k,j)
                           end do
                       end do
                   end do
               end if
          case default
              status = -3
              return
          end select init
    end subroutine
    
    !==========================================================================80
    ! subroutine:
    !              init_field1D_mt3
    !  Calls:
    !              Overloaded genrand_init
    !              Ovreloaded genrand_real3 
    !==========================================================================80
    subroutine init_field1D_mt3(f1d,put,puta,option,its,ite, &
                                ss,ssa,status,fp_flags       )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:),    intent(out) :: f1d
!DIR$     ASSUME_ALIGNED f1d:32
          integer(I32P),               intent(in)             :: put
          integer(I32P), dimension(:), intent(in)             :: puta
          character(len=*),            intent(in)             :: option
          integer(I32P),               intent(in)             :: its,ite
          real(R64P),                  intent(inout),optional :: ss
          real(R64P), dimension(:),    intent(inout),optional :: ssa
          integer(I32P),               intent(inout)          :: status
          logical(I32P), dimension(5), intent(inout)          :: fp_flags
          ! Locals
          integer(I64P)                :: i
         
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)       :: status_value
!DIR$ ENDIF 
          ! Start of executable statements
          ! Sanity check of input arguments
          status = 0
          if(array1D_not_alloc(f1d) .OR. &
             array1D_not_alloc(ssa)     ) then
              status = -1
              return
          end if
          if(present(ss)) then
              if(DABS(ss) .EQ. 0.0_R64P .OR. &
                 DABS(ss) .EQ. 1.0_R64P   ) then
                  ss = sfactor
              end if
          else if(present(ssa)) then
              do i = its, ite
                  if(DABS(ssa(i)) .EQ. 0.0_R64P .OR. &
                     DABS(ssa(i)) .EQ. 1.0_R64P  ) then
                      ssa(i) = sfactor
                  end if
              end do
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
init:     select case (option)
          case ("scalar")
               call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
               call genrand_real3(f1d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field1D_mt3: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF     
               if(present(ss)) then
                   
!DIR$              SIMD VECTORLENGTHFOR(REAL(KIND=8))                   
                   do i = its, ite
                       f1d(i) = f1d(i) * ss
                   end do
               else if(present(ssa)) then
!DIR$              SIMD VECTORLENGTHFOR(REAL(KIND=8))                   
                   do i = its, ite
                       f1d(i) = f1d(i) * ssa(i)
                   end do
               end if
          case ("array")
              call genrand_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
               call genrand_real3(f1d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field1D_mt3: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF 
               if(present(ss)) then
                   
                   do i = its, ite
!DIR$              SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                       f1d(i) = f1d(i) * ss
                   end do
               else if(present(ssa)) then
!DIR$              SIMD VECTORLENGTHFOR(REAL(KIND=8))                   
                   do i = its, ite
                       f1d(i) = f1d(i) * ssa(i)
                   end do
               end if
          case default
               status = -3
               return
          end select init
    end subroutine
    
    !==========================================================================80
    ! subroutine: 
    !               init_field2D_mt3
    ! Calls:
    !              Overloaded genrand_init
    !              Overloaded genrand_real3
    !==========================================================================80
    subroutine init_field2D_mt3(f2d,put,puta,option,its,ite,kts, &
                                kte,ss,ssa,status,fp_flags      )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:), intent(inout) :: f2d
!DIR$     ASSUME_ALIGNED f2d:32
          integer(I32P),              intent(in)             :: put
          integer(I32P), dimension(:),intent(in)             :: puta
          character(len=*),           intent(in)             :: option
          integer(I32P),              intent(in)             :: its,ite, &
                                                                 kts,kte
          real(R64P),                 intent(inout),optional :: ss
          real(R64P), dimension(:,:), intent(inout),optional :: ssa
          integer(I32P),              intent(inout)          :: status
          logical(I32P), dimension(5),intent(inout)          :: fp_flags
          ! Locals
          integer(I64P)               :: k,j
         
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)       :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of input arguments
          status = 0
          if(array2D_not_alloc(f2d) .OR. &
             array2D_not_alloc(ssa)    ) then
              status = -1
              return
          end if
          if(present(ss)) then
              if(DABS(ss) .EQ. 0.0_R64P .OR. &
                 DABS(ss) .EQ. 1.0_R64P  ) then
                  ss = sfactor
              end if
          else if(present(ssa)) then
              do k = kts, kte
                  do i = its, ite
                      if(DABS(ssa(i,k)) .EQ. 0.0_R64 .OR. &
                         DABS(ssa(i,k)) .EQ. 1.0_R64P ) then
                              ssa(i,k) = sfactor
                      end if
                  end do
              end do
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
init:     select case (option)
          case ("scalar")
               call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
               call genrand_real3(f2d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field2D_mt3: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF     
               if(present(ss)) then
                    do k = kts, kte
!DIR$                   SIMD VECTORLENGTHFOR(REAL(KIND=8))                        
                         do i = its, ite
                             f2d(i,k) = f2d(i,k) * ss
                         end do
                    end do
               else if(present(ssa)) then
                   do k = kts, kte
!DIR$                  SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ssa(i,k)
                       end do
                   end do
               end if
          case("array")
              call genrand_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF              
               call genrand_real3(f2d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field2D_mt3: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF 
               if(present(ss)) then
                   do k = kts, kte
!DIR$               SIMD  VECTORLENGTHFOR(REAL(KIND=8))                       
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ss
                       end do
                   end do
               else if(present(ssa)) then
                   do k = kts, kte
!DIR$                  SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ssa(i,k)
                       end do
                   end do
               end if
          case default     
               status = -3             
               return
          end select init
    end subroutine
    
    !==========================================================================80
    !  subroutine:
    !               init_field3D_mt3
    !  Calls:
    !               Overloaded genrand_init
    !               Overloaded genrand_real3
    !==========================================================================80
    subroutine init_field3D_mt3(f3d,put,puta,option,its,ite,kts, &
                                kte,jts,jte,ss,ssa,status,fp_flags )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:,:), intent(out) :: f3d
!DIR$     ASSUME_ALIGNED f3d:32
          integer(I32P),                intent(in)            :: put
          integer(I32P), dimension(:),  intent(in)            :: puta
          character(len=*),             intent(in)            :: option
          integer(I64P),                intent(in)            :: its,ite, &
                                                                 kts,kte, &
                                                                 jts,jte
          real(R64P),                   intent(inout),optional :: ss
          real(R64P), dimension(:,:,:), intent(inout),optional :: ssa
!DIR$     ASSUME_ALIGNED ssa:32          
          integer(I32P),                intent(inout)          :: status
          logical(I32P), dimension(5),  intent(inout)          :: fp_flags
          ! Locals
          integer(I64P)                 :: j,k,i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)       :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of input arguments
          status = 0
          if(array3D_not_alloc(f3d) .OR. &
             array3D_not_alloc(ssa)   ) then
              status = -1
              return
          end if
          if(present(ss)) then
              if(DABS(ss) .EQ. 0.0_R64P .OR. &
                 DABS(ss) .EQ. 1.0_R64P    ) then
                  ss = sfactor
              end if
          else if(present(ssa)) then
              do j = jts, jte
                  do k = kts, kte
                      do i = its, ite
                          if(DABS(ssa(i,k,j)) .EQ. 0.0_R64P .OR. &
                             DABS(ssa(i,k,j)) .EQ. 1.0_R64P  ) then
                              ssa(i,k,j) = sfactor
                          end if
                      end do
                  end do
              end do
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
init:     select case (option)
          case ("scalar")
          call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
               call genrand_real3(f3d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field3D_mt3: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF 
               if(present(ss)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$                   SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ss
                           end do
                       end do
                   end do
               else if(present(ssa)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$                 SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ssa(i,k,j)
                           end do
                       end do
                   end do
               end if
          case ("array")
              call genrand_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
               call genrand_real3(f3d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field3D_mt3: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF     
               if(present(ss)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$                   SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ss
                           end do
                       end do
                   end do
               else if(present(ssa)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$                   SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ssa(i,k,j)
                           end do
                       end do
                   end do
               end if
          case default
               status = -3
               return
          end select init
    end subroutine  
    
    !==========================================================================80
    ! subroutine:
    !              init_field1D_mt53
    !  Calls:
    !              Overloaded genrand_init
    !              Overloaded genrand_res53
    !==========================================================================80
    subroutine init_field1D_mt53(f1d,put,puta,option,its,ite,  &
                                 ss,ssa,status,fp_flags    )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:),   intent(out)             :: f1d
!DIR$     ASSUME_ALIGNED f1d:32          
          integer(I32P),              intent(in)              :: put
          integer(I32P), dimension(:),intent(in)              :: puta
          integer(I32P),              intent(in)              :: its,ite
          real(R64P),                 intent(inout), optional :: ss
          real(R64P), dimension(:),   intent(inout), optional :: ssa
!DIR$     ASSUME_ALIGNED ssa:32          
          integer(I32P),              intent(inout)           :: status
          logical(I32P), dimension(5),intent(inout)           :: fp_status
          ! Locals
          integer(I64P)               :: i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)       :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of input arguments
          status = 0
          if(array1D_not_alloc(f1d) .OR. &
             array1D_not_alloc(ssa)   ) then
              status = -1
              return
          end if
          if(present(ss)) then
              if(DABS(ss) .EQ. 0.0_R64P .OR. &
                 DABS(ss) .EQ. 1.0_R64P )  then
                  ss = sfactor  ! scale by 1.25
              end if
          else if(present(ssa)) then
              do i = its, ite
                  if(DABS(ssa(i)) .EQ. 0.0_R64P .OR. &
                     DABS(ssa(i)) .EQ. 1.0_R64P ) then
                       ssa(i) = sfactor ! scale by 1.25
                  end if
              end do
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
init:     select case (option)
          case ("scalar")
               call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF    
               call genrand_res53(f1d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field1D_mt53: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF    
               if(present(ss)) then
!DIR$              SIMD VECTORLENGTHFOR(REAL(KIND=8))                   
                   do i = its, ite
                       f1d(i) = f1d(i) * ss
                   end do
               else if(present(ssa)) then
!DIR$               SIMD VECTORLENGTHFOR(REAL(KIND=8))                   
                   do i = its, ite
                       f1d(i) = f1d(i) * ssa(i)
                   end do
               end if
          case ("array")
              call genrand_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
               call genrand_real53(f3d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field1D_mt53: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF     
               if(present(ss)) then
!DIR$             SIMD VECTORLENGTHFOR(REAL(KIND=8))                   
                   do i = its, ite
                       f1d(i) = f1d(i) * ss
                   end do
               else if(present(ssa)) then
!DIR$              SIMD VECTORLENGTHFOR(REAL(KIND=8))                   
                   do i = its, ite
                       f1d(i) = f1d(i) * ssa(i)
                   end do
               end if
          case default
                status = -3
                return
          end select init
    end subroutine
    
    !==========================================================================80
    ! subroutine:
    !               init_field2D_mt53
    ! Calls:
    !               Overloaded genrand_init
    !               Overloaded genrand_real53
    !==========================================================================80
    subroutine init_field2D_mt53(f2d,put,puta,option,its,ite, &
                                 kts,kte,ss,ssa,status,fp_flags )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:),  intent(out)            :: f2d
!DIR$     ASSUME_ALIGNED f2d:32
          integer(I32P),               intent(in)             :: put
          integer(I32P), dimension(:), intent(in)             :: puta
          character(len=*),            intent(in)             :: option
          integer(I64P),               intent(in)             :: its,ite,&
                                                                 kts,kte
          real(R64P),                  intent(inout),optional :: ss
          real(R64P), dimension(:,:),  intent(inout),optional :: ssa
!DIR$     ASSUME_ALIGNED ssa:32          
          integer(I32P),               intent(inout)          :: status
          logical(I32P), dimension(5), intent(inout)          :: fp_flags
          ! Locals
          integer(I64P)                :: k,j
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)       :: status_value
!DIR$ ENDIF 
          ! Start of executable statements
          ! Sanity check of input arguments
          status = 0
          if(array2D_not_alloc(f2d) .OR. &
             array2D_not_alloc(ssa)    ) then
              status = -1
              return
          end if
          if(present(ss)) then
              if(DABS(ss) .EQ. 0.0_R64P &
                 DABS(ss) .EQ. 1.0_R64P ) then
                  ss = sfactor ! scale by 1.25
                 end if
          else if(present(ssa)) then
              do k = kts, kte
                  do i = its, ite
                      if(DABS(ssa(i,k)) .EQ. 0.0_R64P .OR. &
                          DABS(ssa(i,k)) .EQ. 1.0_R64P  ) then
                            ssa(i,k) = sfactor ! scale by 1.25
                      end if
                  end do
              end do
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
init:     select case (option)
          case ("scalar")
              call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
               call genrand_real53(f2d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field2D_mt53: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF    
               if(present(ss)) then
                   do k = kts, kte
!DIR$                   SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ss
                       end do
                   end do
               else if(present(ssa)) then
                   do k = kts, kte
!DIR$                   SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ssa(i,k)
                       end do
                   end do
               end if
          case ("array")
              call getrand_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
               call getrand_real53(f2d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field2D_mt53: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF 
               if(present(ss)) then
                   do k = kts, kte
!DIR$               SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ss
                       end do
                   end do
               else if(present(ssa)) then
                   do k = kts, kte
!DIR$               SIMD VECTORLENGTHFOR(REAL(KIND=8))                       
                       do i = its, ite
                           f2d(i,k) = f2d(i,k) * ssa(i,k)
                       end do
                   end do
               end if
          case default
               status = -3
               return
          end select init
    end subroutine
    
    !==========================================================================80
    ! subroutine:
    !               init_field3D_mt53
    !  Calls:
    !               Overloaded genrand_init
    !               Overloaded genrand_real53
    !==========================================================================80
    subroutine init_field3D_mt53(f3d,put,puta,option,its,ite,kts,kte, &
                                 jts,jte,ss,ssa,status,fp_flags       )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:,:), intent(out) :: f3d
!DIR$     ASSUME_ALIGNED f3d:32          
          integer(I32P),                intent(in)             :: put
          integer(I32P), dimension(:),  intent(in)             :: puta
          character(len=*),             intent(in)             :: option
          integer(I64P),                intent(in)             :: its,ite, &
                                                                  kts,kte, &
                                                                  jts,jte
          real(R64P),                   intent(inout),optional :: ss
          real(R64P), dimension(:,:,:), intent(inout),optional :: ssa
!DIR$     ASSUME_ALIGNED ssa:32
          integer(I32P),                intent(inout)          :: status
          logical(I32P), dimension(5),  intent(inout)          :: fp_flags
          ! Locals
          integer(I64P)                 :: j,k,i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)       :: status_value
!DIR$ ENDIF 
          ! Start of executable statements
          ! Sanity checking of input arguemts
          status = 0
          if(array3D_not_alloc(f3d) .OR. &
             array3D_not_alloc(ssa)  )  then
              status = -1
              return
          end if
          if(present(ss)) then
              if(DABS(ss) .EQ. 0.0_R64P .OR. &
                  DABS(ss) .EQ. 1.0_R64P ) then
                    ss = sfactor ! scale by 1.25
              end if
          else if(present(ssa)) then
              do j = jts, jte
                  do k = kts, kte
                      do i = its, ite
                          if(DABS(ssa(i,k,j)) .EQ. 0.0_R64P .OR. &
                              DABS(ssa(i,k,j)) .EQ. 1.0_R64P ) then
                                    ssa(i,k,j) = sfactor  ! scale by 1.25
                          end if
                      end do
                  end do
              end do
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
init:     select case (option)
          case ("scalar")
               call genrand_init(put)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
               call genrand_real53(f3d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field3D_mt53: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF 
               if(present(ss)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$                       SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ss
                           end do
                       end do
                   end do
               else if(present(ssa)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$                       SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ssa(i,k,j)
                           end do
                       end do
                   end do
               end if
          case ("array")
              call genrand_init(puta)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_status(status_value)
               call ieee_set_halting_mode(ieee_all,.false.)
               call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
               call genrand_real53(f3d)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
               call ieee_get_flag(ieee_all,fp_flags)
               if(ANY(fp_flags)) then
                  status = -2
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " init_field3D_mt53: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
               end if
               call ieee_set_status(status_value)
!DIR$ ENDIF    
                if(present(ss)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$                       SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ss
                           end do
                       end do
                   end do
               else if(present(ssa)) then
                   do j = jts, jte
                       do k = kts, kte
!DIR$                       SIMD VECTORLENGTHFOR(REAL(KIND=8))                           
                           do i = its, ite
                               f3d(i,k,j) = f3d(i,k,j) * ssa(i,k,j)
                           end do
                       end do
                   end do
               end if
          case default
               status = -3
               return
          end select init
    end subroutine
end module