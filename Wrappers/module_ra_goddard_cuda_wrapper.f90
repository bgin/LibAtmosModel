
module  module_ra_goddard_cuda_wrapper


  !-----------------------------------------------------------------------------------85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'module_ra_goddard_cuda_wrapper'
 !          
 !          Purpose:
 !                          This module declares Fortran interface or wrapper
 !                          to C CUDA implementation of computational expensive
 !                          Fortran subroutines.
 !                          This module will be callable from higher abstraction
 !                          layer dispatch code.
 !          History:
 !                      Date: 20-01-2017
 !                      Time: 20:27 GMT+2
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

 
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'column' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50

    use module_kinds
    
    ! File version info
    
    ! Major version
    integer(i32), parameter, public :: module_ra_goddard_cuda_wrapper_major = 1
    
    ! Minor version
    integer(i32), parameter, public :: module_ra_goddard_cuda_wrapper_minor = 0
    
    ! Micro(patch) version
    integer(i32), parameter, public :: module_ra_goddard_cuda_wrapper_micro = 0
    
    ! File version
    integer(i32), parameter, public :: module_ra_goddard_cuda_wrapper_version = 1000*module_ra_goddard_cuda_wrapper_major &
        + 100*module_ra_goddard_cuda_wrapper_minor + 10*module_ra_goddard_cuda_wrapper_micro
    
    ! Date of build should be set after suucessful build of this file
    character(len=*) BuildDate
    parameter(BuildDate = '')
    
    ! Creation Date
    character(len=*) CreateDate
    parameter(CreateDate = '20-01-2017 20:27 +200 (Fri, 20 Jan 2017 GMT+2)')
    
    ! Author
    character(len=*) Author
    parameter(Author = 'Programmer: Bernard Gingold , conatct: beniekg@gmail.com')
    
    public :: BuildDate,CreateDate,Author
        
    
    
     ! Static array lenghts
    integer(kind=4), parameter :: st_ar_len = 9
    integer(kind=4), parameter :: st_ar_len2 = 6
    integer(kind=4), parameter :: st_ar_len3 = 3
    
     interface
     
         subroutine  column(m,np,pa,dt,sabs0,sabs,spre,stem)  BIND(C, NAME='column')
         
!***********************************************************************
!-----compute column-integrated (from top of the model atmosphere)
!     absorber amount (sabs), absorber-weighted pressure (spre) and
!     temperature (stem).
!     computations follow eqs. (8.24) - (8.26).
!
!--- input parameters
!   number of soundings (m)
!   number of atmospheric layers (np)
!   layer pressure (pa)
!   layer temperature minus 250k (dt)
!   layer absorber amount (sabs0)
!
!--- output parameters
!   column-integrated absorber amount (sabs)
!   column absorber-weighted pressure (spre)
!   column absorber-weighted temperature (stem)
!
!--- units of pa and dt are mb and k, respectively.
!    units of sabs are g/cm**2 for water vapor and (cm-atm)stp
!    for co2 and o3
!***********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),               intent(in)  :: m
                integer(C_INT),               intent(in)  :: np
                real(C_DOUBLE), dimension(*), intent(in)  :: pa
                real(C_DOUBLE), dimension(*), intent(in)  :: dt
                real(C_DOUBLE), dimension(*), intent(in)  :: sabs0
                real(C_DOUBLE), dimension(*), intent(out) :: sabs
                real(C_DOUBLE), dimension(*), intent(out) :: spre
                real(C_DOUBLE), dimension(*), intent(out) :: stem
                
         end  subroutine

       
                                
     end  interface
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'h2oexps' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
        
         subroutine  h2oexps(ib,m,np,dh2o,pa,dt,xkw,aw,bw,pm,mw,h2oexp)   BIND(C, NAME='h2oexps')
         
!**********************************************************************
!   compute exponentials for water vapor line absorption
!   in individual layers using eqs. (8.18) and (8.19).
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  layer water vapor amount for line absorption (dh2o)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!  absorption coefficients for the first k-distribution
!     function due to h2o line absorption (xkw)
!  coefficients for the temperature and pressure scaling (aw,bw,pm)
!  ratios between neighboring absorption coefficients for
!     h2o line absorption (mw)
!
!---- output parameters
!  6 exponentials for each layer  (h2oexp)
!**********************************************************************
         
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),                           intent(in)  :: ib
                integer(C_INT),                           intent(in)  :: m
                integer(C_INT),                           intent(in)  :: np
                real(C_DOUBLE), dimension(*),             intent(in)  :: dh2o
                real(C_DOUBLE), dimension(*),             intent(in)  :: pa
                real(C_DOUBLE), dimension(*),             intent(in)  :: dt
                real(C_DOUBLE), dimension(st_ar_len),     intent(in)  :: xkw
                real(C_DOUBLE), dimension(st_ar_len),     intent(in)  :: aw
                real(C_DOUBLE), dimension(st_ar_len),     intent(in)  :: bw
                real(C_DOUBLE), dimension(st_ar_len),     intent(in)  :: pm
                integer(C_INT), dimension(st_ar_len),     intent(in)  :: mw
                real(C_DOUBLE), dimension(*),             intent(out) :: h2oexp
                
         end  subroutine
     
     end  interface

     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'conexps' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
     
         subroutine  conexps(ib,m,np,dcont,xke,conexp)   BIND(C, NAME='conexps')
         
!**********************************************************************
!   compute exponentials for continuum absorption in individual layers.
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  layer scaled water vapor amount for continuum absorption (dcont)
!  absorption coefficients for the first k-distribution function
!     due to water vapor continuum absorption (xke)
!
!---- output parameters
!  1 or 3 exponentials for each layer (conexp)
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),                   intent(in)  :: ib
                integer(C_INT),                   intent(in)  :: m
                integer(C_INT),                   intent(in)  :: np
                real(C_DOUBLE), dimension(*),         intent(in)  :: dcont
                real(C_DOUBLE), dimension(st_ar_len), intent(in)  :: xke
                real(C_DOUBLE), dimension(*),         intent(out) :: conexp
                
         end  subroutine 
         
     end interface
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'co2exps' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
     
         subroutine  co2exps(m,np,dco2,pa,dt,co2exp)   BIND(C, NAME='co2exps')
         
!**********************************************************************
!   compute co2 exponentials for individual layers.
!
!---- input parameters
!  number of grid intervals (m)
!  number of layers (np)
!  layer co2 amount (dco2)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  6 exponentials for each layer (co2exp)
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),           intent(in)  :: m
                integer(C_INT),           intent(in)  :: np
                real(C_DOUBLE), dimension(*), intent(in)  :: dco2
                real(C_DOUBLE), dimension(*), intent(in)  :: pa
                real(C_DOUBLE), dimension(*), intent(in)  :: dt
                real(C_DOUBLE), dimension(*), intent(out) :: co2exp
                
         end  subroutine
     
     end  interface
     
     interface 
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'n2oexps' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         
         subroutine  n2oexps(ib,m,np,d2no,pa,dt,n2oexp)    BIND(C, NAME='n2oexps')
         
!**********************************************************************
!   compute n2o exponentials for individual layers
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  layer n2o amount (dn2o)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  2 or 4 exponentials for each layer (n2oexp)
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),           intent(in)  :: ib
                integer(C_INT),           intent(in)  :: m
                integer(C_INT),           intent(in)  :: np
                real(C_DOUBLE), dimension(*), intent(in)  :: d2no
                real(C_DOUBLE), dimension(*), intent(in)  :: pa
                real(C_DOUBLE), dimension(*), intent(in)  :: dt
                real(C_DOUBLE), dimension(*), intent(out) :: n2oexp
         
         end  subroutine
         
     end  interface
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'ch4exps' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
     
         subroutine  ch4exps(ib,m,np,dch4,pa,dt,ch4exp)  BIND(C, NAME='ch4exps')
         
!**********************************************************************
!   compute ch4 exponentials for individual layers
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  layer ch4 amount (dch4)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  1 or 4 exponentials for each layer (ch4exp)
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),           intent(in)  :: ib
                integer(C_INT),           intent(in)  :: m
                integer(C_INT),           intent(in)  :: np
                real(C_DOUBLE), dimension(*), intent(in)  :: dch4
                real(C_DOUBLE), dimension(*), intent(in)  :: pa
                real(C_DOUBLE), dimension(*), intent(in)  :: dt
                real(C_DOUBLE), dimension(*), intent(out) :: ch4exp
                
         end subroutine
     
     
     end  interface
     
     
     interface  
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'comexps' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
     
         subroutine  comexps(ib,m,np,dcom,dt,comexp)   BIND(C, NAME='comexp')
         
!**********************************************************************
!   compute co2-minor exponentials for individual layers using
!   eqs. (8.18) and (8.19).
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  layer co2 amount (dcom)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  6 exponentials for each layer (comexp)
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),           intent(in)  :: ib
                integer(C_INT),           intent(in)  :: m
                integer(C_INT),           intent(in)  :: np
                real(C_DOUBLE), dimension(*), intent(in)  :: dcom
                real(C_DOUBLE), dimension(*), intent(in)  :: dt
                real(C_DOUBLE), dimension(*), intent(out) :: comexp
                
         end  subroutine
     
     end  interface
     
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'cfcexps' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  cfcexps(ib,m,np,a1,b1,fk1,a2,b2,fk2,dcfc,dt,cfcexp)   BIND(C, NAME='cfcexps')
         
!**********************************************************************
!   compute cfc(-11, -12, -22) exponentials for individual layers.
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  parameters for computing the scaled cfc amounts
!             for temperature scaling (a1,b1,a2,b2)
!  the absorption coefficients for the
!     first k-distribution function due to cfcs (fk1,fk2)
!  layer cfc amounts (dcfc)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  1 exponential for each layer (cfcexp)
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),           intent(in) :: ib
                integer(C_INT),           intent(in) :: m
                integer(C_INT),           intent(in) :: np
                real(C_DOUBLE),               intent(in) :: a1
                real(C_DOUBLE),               intent(in) :: b1
                real(C_DOUBLE),               intent(in) :: fk1
                real(C_DOUBLE),               intent(in) :: a2
                real(C_DOUBLE),               intent(in) :: b2
                real(C_DOUBLE),               intent(in) :: fk2
                real(C_DOUBLE), dimension(*), intent(in) :: dcfc
                real(C_DOUBLE), dimension(*), intent(in) :: dt
                real(C_DOUBLE), dimension(*), intent(out) :: cfcexp
                
         end  subroutine
     
     end  interface 
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'b10exps' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  b10exps(m,np,dh2o,dcont,dco2,dn2o,pa,dt &
                            ,h2oexp,conexp,co2exp,n2oexp     )   BIND(C, NAME='b10exps')
         
!**********************************************************************
!   compute band3a exponentials for individual layers
!
!---- input parameters
!  number of grid intervals (m)
!  number of layers (np)
!  layer h2o amount for line absorption (dh2o)
!  layer h2o amount for continuum absorption (dcont)
!  layer co2 amount (dco2)
!  layer n2o amount (dn2o)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!
!  exponentials for each layer (h2oexp,conexp,co2exp,n2oexp)
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),             intent(in)  :: m
                integer(C_INT),             intent(in)  :: np
                real(C_DOUBLE), dimension(*),   intent(in)  :: dh2o
                real(C_DOUBLE), dimension(*),   intent(in)  :: dcont
                real(C_DOUBLE), dimension(*),   intent(in)  :: dco2
                real(C_DOUBLE), dimension(*),   intent(in)  :: dn2o
                real(C_DOUBLE), dimension(*),   intent(in)  :: pa
                real(C_DOUBLE), dimension(*),   intent(in)  :: dt
                real(C_DOUBLE), dimension(*),   intent(out) :: h2oexp
                real(C_DOUBLE), dimension(*),   intent(out) :: conexp
                real(C_DOUBLE), dimension(*),   intent(out) :: co2exp
                real(C_DOUBLE), dimension(*),   intent(out) :: n2oexp
         
         end  subroutine
     
     
     end  interface 
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'tablup' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine   tablup(k1,k2,m,np,nx,nh,sabs,spre,stem,w1,p1 &
                            ,dwe,dpe,coef1,coef2,coef3, tran   )       BIND(C, NAME='tablup')
         
                                      
!**********************************************************************
!   compute water vapor, co2 and o3 transmittances between level
!   k1 and and level k2 for m soundings, using table look-up.
!
!   calculations follow eq. (4.16).
!
!---- input ---------------------
!  indices for layer (k1) and level (k2)
!  number of grid intervals (m)
!  number of atmospheric layers (np)
!  number of pressure intervals in the table (nx)
!  number of absorber amount intervals in the table (nh)
!  column-integrated absorber amount (sabs)
!  column absorber amount-weighted pressure (spre)
!  column absorber amount-weighted temperature (stem)
!  first value of absorber amount (log10) in the table (w1)
!  first value of pressure (log10) in the table (p1)
!  size of the interval of absorber amount (log10) in the table (dwe)
!  size of the interval of pressure (log10) in the table (dpe)
!  pre-computed coefficients (coef1, coef2, and coef3)
!
!---- updated ---------------------
!  transmittance (tran)
!
!  note:
!   (1) units of sabs are g/cm**2 for water vapor and
!       (cm-atm)stp for co2 and o3.
!   (2) units of spre and stem are, respectively, mb and k.
!
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),             intent(in)  :: k1
                integer(C_INT),             intent(in)  :: k2
                integer(C_INT),             intent(in)  :: m
                integer(C_INT),             intent(in)  :: np
                integer(C_INT),             intent(in)  :: nx
                integer(C_INT),             intent(in)  :: nh
                real(C_DOUBLE), dimension(*),   intent(in)  :: sabs
                real(C_DOUBLE), dimension(*),   intent(in)  :: spre
                real(C_DOUBLE), dimension(*),   intent(in)  :: stem
                real(C_DOUBLE),                 intent(in)  :: w1
                real(C_DOUBLE),                 intent(in)  :: p1
                real(C_DOUBLE),                 intent(in)  :: dwe
                real(C_DOUBLE),                 intent(in)  :: dpe
                real(C_DOUBLE), dimension(*),   intent(in)  :: coef1
                real(C_DOUBLE), dimension(*),   intent(in)  :: coef2
                real(C_DOUBLE), dimension(*),   intent(in)  :: coef3
                real(C_DOUBLE), dimension(*),   intent(out) :: tran
                
         end  subroutine
     
     
     end  interface
     
     
     interface
     
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'h2okdis' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  h2okdis(ib,m,np,k,fkw,gkw,ne,h2oexp,conexp, &
                             th2o,tcon,tran                     )    BIND(C, NAME='h2okdis')
         
!**********************************************************************
!   compute water vapor transmittance between levels k1 and k2 for
!   m soundings, using the k-distribution method.
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of levels (np)
!  current level (k)
!  planck-weighted k-distribution function due to
!    h2o line absorption (fkw)
!  planck-weighted k-distribution function due to
!    h2o continuum absorption (gkw)
!  number of terms used in each band to compute water vapor
!     continuum transmittance (ne)
!  exponentials for line absorption (h2oexp)
!  exponentials for continuum absorption (conexp)
!
!---- updated parameters
!  transmittance between levels k1 and k2 due to
!    water vapor line absorption (th2o)
!  transmittance between levels k1 and k2 due to
!    water vapor continuum absorption (tcon)
!  total transmittance (tran)
!
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),                                   intent(in)  :: ib
                integer(C_INT),                                   intent(in)  :: m
                integer(C_INT),                                   intent(in)  :: np
                integer(C_INT),                                   intent(in)  :: k
                real(C_DOUBLE),     dimension(st_ar_len2,st_ar_len),  intent(in)  :: fkw
                real(C_DOUBLE),     dimension(st_ar_len2,st_ar_len3), intent(in)  :: gkw
                integer(C_INT), dimension(st_ar_len),             intent(in)  :: ne
                real(C_DOUBLE),     dimension(*),                     intent(in)  :: h2oexp
                real(C_DOUBLE),     dimension(*),                     intent(in)  :: conexp
                real(C_DOUBLE),     dimension(*),                     intent(out) :: th2o
                real(C_DOUBLE),     dimension(*),                     intent(out) :: tcon
                real(C_DOUBLE),     dimension(*),                     intent(out) :: tran
         end  subroutine
     
     end  interface
     
     
     interface
     
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'co2kdis' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  co2kdis(m,np,k,co2exp,tco2,tran)   BIND(C, NAME='co2kdis')
         
!**********************************************************************
!   compute co2 transmittances between levels k1 and k2 for
!    m soundings, using the k-distribution method with linear
!    pressure scaling.
!
!---- input parameters
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for co2 absorption (co2exp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to co2 absorption
!     for the various values of the absorption coefficient (tco2)
!   total transmittance (tran)
!
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),           intent(in) :: m
                integer(C_INT),           intent(in) :: np
                integer(C_INT),           intent(in) :: k
                real(C_DOUBLE), dimension(*), intent(in) :: co2exp
                real(C_DOUBLE), dimension(*), intent(out) :: tco2
                real(C_DOUBLE), dimension(*), intent(out) :: tran
         
         end  subroutine
     
     end  interface
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'n2okdis' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  n2okdis(ib,m,np,k,n2oexp,tn2o,tran)   BIND(C, NAME='n2okdis') 
         
!**********************************************************************
!   compute n2o transmittances between levels k1 and k2 for
!    m soundings, using the k-distribution method with linear
!    pressure scaling.
!
!---- input parameters
!   spectral band (ib)
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for n2o absorption (n2oexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to n2o absorption
!     for the various values of the absorption coefficient (tn2o)
!   total transmittance (tran)
!
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),           intent(in) :: ib
                integer(C_INT),           intent(in) :: m
                integer(C_INT),           intent(in) :: np
                integer(C_INT),           intent(in) :: k
                real(C_DOUBLE), dimension(*), intent(in) :: n2oexp
                real(C_DOUBLE), dimension(*), intent(out) :: tn2o
                real(C_DOUBLE), dimension(*), intent(out) :: tran
                
         end  subroutine
     
     end  interface
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'ch4kdis' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  ch4kdis(ib,m,np,k,ch4exp,tch4,tran)    BIND(C, NAME='ch4kdis')
         
!**********************************************************************
!   compute ch4 transmittances between levels k1 and k2 for
!    m soundings, using the k-distribution method with
!    linear pressure scaling.
!
!---- input parameters
!   spectral band (ib)
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for ch4 absorption (ch4exp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to ch4 absorption
!     for the various values of the absorption coefficient (tch4)
!   total transmittance (tran)
!
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),           intent(in)  :: ib
                integer(C_INT),           intent(in)  :: m
                integer(C_INT),           intent(in)  :: np
                integer(C_INT),           intent(in)  :: k
                real(C_DOUBLE), dimension(*), intent(in)  :: ch4exp
                real(C_DOUBLE), dimension(*), intent(out) :: tch4
                real(C_DOUBLE), dimension(*), intent(out) :: tran
         end  subroutine
     
     end  interface
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'comkdis' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  comkdis(ib,m,np,k,comexp,tcom,tran)   BIND(C, NAME='comkdis')
         
!**********************************************************************
!  compute co2-minor transmittances between levels k1 and k2
!   for m soundings, using the k-distribution method
!   with linear pressure scaling.
!
!---- input parameters
!   spectral band (ib)
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for co2-minor absorption (comexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to co2-minor absorption
!     for the various values of the absorption coefficient (tcom)
!   total transmittance (tran)
!
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),           intent(in)  :: ib
                integer(C_INT),           intent(in)  :: m
                integer(C_INT),           intent(in)  :: np
                integer(C_INT),           intent(in)  :: k
                real(C_DOUBLE), dimension(*), intent(in)  :: comexp
                real(C_DOUBLE), dimension(*), intent(out) :: tcom
                real(C_DOUBLE), dimension(*), intent(out) :: tran
         end  subroutine
     
     end  interface
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'cfckdis' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  cfckdis(m,np,k,cfcexp,tcfc,tran)   BIND(C, NAME='cfckdis')
         
!**********************************************************************
!  compute cfc-(11,12,22) transmittances between levels k1 and k2
!   for m soundings, using the k-distribution method with
!   linear pressure scaling.
!
!---- input parameters
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for cfc absorption (cfcexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to cfc absorption
!     for the various values of the absorption coefficient (tcfc)
!   total transmittance (tran)
!
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),           intent(in)  :: m
                integer(C_INT),           intent(in)  :: np
                integer(C_INT),           intent(in)  :: k
                real(C_DOUBLE), dimension(*), intent(in)  :: cfcexp
                real(C_DOUBLE), dimension(*), intent(out) :: tcfc
                real(C_DOUBLE), dimension(*), intent(out) :: tran
         end  subroutine
     
     end  interface
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'b10kdis' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  b10kdis(m,np,k,h2oexp,conexp,co2exp,n2oexp, &
                            th2o,tcon,tco2,tn2o,tran        )         BIND(C, NAME='b10kdis')
         
!**********************************************************************
!
!   compute h2o (line and continuum),co2,n2o transmittances between
!   levels k1 and k2 for m soundings, using the k-distribution
!   method with linear pressure scaling.
!
!---- input parameters
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for h2o line absorption (h2oexp)
!   exponentials for h2o continuum absorption (conexp)
!   exponentials for co2 absorption (co2exp)
!   exponentials for n2o absorption (n2oexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to h2o line absorption
!     for the various values of the absorption coefficient (th2o)
!   transmittance between levels k1 and k2 due to h2o continuum
!     absorption for the various values of the absorption
!     coefficient (tcon)
!   transmittance between levels k1 and k2 due to co2 absorption
!     for the various values of the absorption coefficient (tco2)
!   transmittance between levels k1 and k2 due to n2o absorption
!     for the various values of the absorption coefficient (tn2o)
!   total transmittance (tran)
!
!**********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),        intent(in) :: m
                integer(C_INT),        intent(in) :: np
                integer(C_INT),        intent(in) :: k
                real(C_DOUBLE), dimension(*), intent(in) :: h2oexp
                real(C_DOUBLE), dimension(*), intent(in) :: conexp
                real(C_DOUBLE), dimension(*), intent(in) :: co2exp
                real(C_DOUBLE), dimension(*), intent(in) :: n2oexp
                real(C_DOUBLE), dimension(*), intent(out) :: th2o
                real(C_DOUBLE), dimension(*), intent(out) :: tcon
                real(C_DOUBLE), dimension(*), intent(out) :: tco2
                real(C_DOUBLE), dimension(*), intent(out) :: tn2o
                real(C_DOUBLE), dimension(*), intent(out) :: tran
         
         end  subroutine
     
     
     end  interface
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'cldovlp' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  cldovlp(m,np,k2,ict,icb,it,im,ib, &
                            cldhi,cldmd,cldlw,fcld,tcldlyr,fclr )    BIND(C, NAME='cldovlp')
         
!***********************************************************************
!     compute the fractional clear line-of-sight between levels k1
!     and k2
!
! input parameters
!
!  m:       number of soundings
!  np:      number of layers
!  k2:      index for the level
!  ict:     the level separating high and middle clouds
!  icb:     the level separating middle and low clouds
!  it:      number of cloudy layers in the high-cloud group
!  im:      number of cloudy layers in the middle-cloud group
!  ib:      number of cloudy layers in the low-cloud group
!  fcld:    fractional cloud cover of a layer
!  tcldlyr: transmittance of a cloud layer
!
! output parameter
!
!  fclr:    clear line-of-sight between levels k1 and k2
!***********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT64_T),          intent(in) :: m
                integer(C_INT64_T),          intent(in) :: np
                integer(C_INT64_T),          intent(in) :: k2
                integer(C_INT64_T), dimension(*), intent(in) :: ict
                integer(C_INT64_T), dimension(*), intent(in) :: icb
                integer(C_INT64_T), dimension(*), intent(in) :: it
                integer(C_INT64_T), dimension(*), intent(in) :: im
                integer(C_INT64_T), dimension(*), intent(in) :: ib
                real(C_DOUBLE),     dimension(*), intent(inout) :: cldhi
                real(C_DOUBLE),     dimension(*), intent(inout) :: cldmd
                real(C_DOUBLE),     dimension(*), intent(inout) :: cldlw
                real(C_DOUBLE),     dimension(*), intent(in)    :: fcld
                real(C_DOUBLE),     dimension(*), intent(in)    :: tcldlyr
                real(C_DOUBLE),     dimension(*), intent(out)   :: fclr
                
         
         end  subroutine
     
     end  interface
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'cloud_scale' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  cloud_scale(m,np,cosz,fcld,taucld,ict,icb, &
                                cc,tauclb, tauclf  )                BIND(C, NAME='cloud_scale')
         
         
!********************************************************************
!
!   this subroutine computes the high, middle, and low cloud
!    amounts and scales the cloud optical thickness (section 7)
!
!   to simplify calculations in a cloudy atmosphere, clouds are
!    grouped into high, middle and low clouds separated by the levels
!    ict and icb (level 1 is the top of the model atmosphere).
!
!   within each of the three groups, clouds are assumed maximally
!    overlapped, and the cloud cover (cc) of a group is the maximum
!    cloud cover of all the layers in the group.  the optical thickness
!    (taucld) of a given layer is then scaled to new values (tauclb and
!    tauclf) so that the layer reflectance corresponding to the cloud
!    cover cc is the same as the original reflectance with optical
!    thickness taucld and cloud cover fcld.
!
!---input parameters
!
!    number of atmospheric soundings (m)
!    number of atmospheric layers (np)
!    cosine of the solar zenith angle (cosz)
!    fractional cloud cover (fcld)
!    cloud optical thickness (taucld)
!    index separating high and middle clouds (ict)
!    index separating middle and low clouds (icb)
!
!---output parameters
!
!    fractional cover of high, middle, and low cloud groups (cc)
!    scaled cloud optical thickness for direct  radiation (tauclb)
!    scaled cloud optical thickness for diffuse radiation (tauclf)
!
!********************************************************************
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),               intent(in)  :: m
                integer(C_INT),               intent(in)  :: np
                real(C_DOUBLE), dimension(*), intent(in)  :: cosz
                real(C_DOUBLE), dimension(*), intent(in)  :: fcld
                real(C_DOUBLE), dimension(*), intent(in)  :: taucld
                integer(C_INT), dimension(*), intent(in)  :: ict
                integer(C_INT), dimension(*), intent(in)  :: icb
                real(C_DOUBLE), dimension(*), intent(out) :: cc
                real(C_DOUBLE), dimension(*), intent(out) :: tauclb
                real(C_DOUBLE), dimension(*), intent(out) :: tauclf
                
         end  subroutine
     
     
     end  interface
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'twostream_adding' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  twostream_adding(m,np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                                     cc,rr,tt,td,rs,ts,fclr,fall,falld,fsdir,fsdif)   BIND(C, NAME='twostream_adding')
         
!*******************************************************************
!  compute upward and downward fluxes using a two-stream adding method
!  following equations (6.9)-(6.16).
!
!  clouds are grouped into high, middle, and low clouds which are assumed
!  randomly overlapped. it involves a maximum of 8 sets of calculations.
!  in each set of calculations, each atmospheric layer is homogeneous,
!  either totally filled with clouds or without clouds.
!  input parameters:
!
!   m:   number of soundings
!   np:  number of atmospheric layers
!   ict: the level separating high and middle clouds
!   icb: the level separating middle and low clouds
!   ih1,ih2,im1,im2,is1,is2: indices for three group of clouds
!   cc:  effective cloud covers for high, middle and low clouds
!   rr:  reflection of a layer illuminated by beam radiation
!   tt:  total (direct+diffuse) transmission of a layer illuminated
!        by beam radiation
!   td:  direct beam transmission
!   rs:  reflection of a layer illuminated by diffuse radiation
!   ts:  transmission of a layer illuminated by diffuse radiation
!
!  output parameters:
!
!     fclr:  clear-sky flux divergence (downward minus upward)
!     fall:  all-sky flux divergence (downward minus upward)
!     fsdir: surface direct downward flux
!     fsdif: surface diffuse downward flux
!
!*********************************************************************c
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),                 intent(in) :: m
                integer(C_INT),                 intent(in) :: np
                integer(C_INT), dimension(*),   intent(in) :: ict
                integer(C_INT), dimension(*),   intent(in) :: icb
                integer(C_INT),                 intent(in) :: ih1
                integer(C_INT),                 intent(in) :: ih2
                integer(C_INT),                 intent(in) :: im1
                integer(C_INT),                 intent(in) :: im2
                integer(C_INT),                 intent(in) :: is1
                integer(C_INT),                 intent(in) :: is2
                real(C_DOUBLE), dimension(*),   intent(in) :: cc
                real(C_DOUBLE), dimension(*),   intent(in) :: rr
                real(C_DOUBLE), dimension(*),   intent(in) :: tt
                real(C_DOUBLE), dimension(*),   intent(in) :: td
                real(C_DOUBLE), dimension(*),   intent(in) :: rs
                real(C_DOUBLE), dimension(*),   intent(in) :: ts
                real(C_DOUBLE), dimension(*),   intent(out) :: fclr
                real(C_DOUBLE), dimension(*),   intent(out) :: fall
                real(C_DOUBLE), dimension(*),   intent(out) :: falld
                real(C_DOUBLE), dimension(*),   intent(out) :: fsdir
                real(C_DOUBLE), dimension(*),   intent(out) :: fsdif
                
         end  subroutine
     
     end  interface
     
     
     interface
     
   !=============================================50
   !  Fortran interface to corresponding C-CUDA
   !  implementation of 'reduce_flux' subroutine
   !  from 'module_ra_goddard'.
   !=============================================50
         subroutine  reduce_flux(m,np,swc,u1,du,nu,swh,w1,dw,nw,tbl,df)   BIND(C, NAME='reduce_flux')
         
!*****************************************************************
!-----compute the reduction of clear-sky downward solar flux
!     due to co2 absorption.
         
                USE, INTRINSIC :: ISO_C_BINDING
                IMPLICIT NONE
                integer(C_INT),               intent(in)  :: m
                integer(C_INT),               intent(in)  :: np
                real(C_DOUBLE), dimension(*), intent(in)  :: swc
                real(C_DOUBLE),               intent(in)  :: u1
                real(C_DOUBLE),               intent(in)  :: du
                real(C_INT),                  intent(in)  :: nu
                real(C_DOUBLE), dimension(*), intent(in)  :: swh
                real(C_DOUBLE),               intent(in)  :: w1
                real(C_DOUBLE),               intent(in)  :: dw
                integer(C_INT),               intent(in)  :: nw
                real(C_DOUBLE), dimension(*), intent(in)  :: tbl
                real(C_DOUBLE), dimension(*), intent(out) :: df
                
         
         
         end subroutine
     
     
     end  interface

end  module