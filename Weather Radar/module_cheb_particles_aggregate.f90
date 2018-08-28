
#include "Config.fpp"


module mod_cheb_particles_aggregate

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_cheb_particles_aggregate'
 !          
 !          Purpose:
 !                      This module models 'Chebyshev Particles' aggregated ensemble
 !                       
 !          History:
 !                        Date: 04-08-2018
 !                        Time: 11:51 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !          
 !                 
 !          References:
 !         
 !                           'Scattering of Radiation by Moderately Non-Spherical Particles' 
 !                            By A. Mugnai and W.J. Wiscombe (1986)
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    implicit none
    use module_kinds,    only : I32P, R64P
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT, &
                                stdout=>OUTPUT_UNIT

     
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P),  parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P),  parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P),  parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P),  parameter, public :: MOD_CHEB_PARTICLES_AGGREAGTE_FULLVER = 1000_I32P*MOD_CHEB_PARTICLES_AGGREGATE_MAJOR + &
                                                                                100_I32P*MOD_CHEB_PARTICLES_AGGREGATE_MINOR  + &
                                                                                10_I32P*MOD_CHEB_PARTICLES_AGGREGATE_MICRO
    ! Module creation date
    character(*),   parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_CREATE_DATE = "04-08-2018 11:41 +00200 (SAT 04 AUG 2018 GMT+2) "
    
    ! Module build date (  should be set after successful compilation)
    character(*),   parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_CHEB_PARTICLES_AGGREGATE_DESCRIPT = "Model of aggregated chebyshev particles (hydrometeors)."
    
    ! Constants
    
    real(R64P),    parameter, private :: Deg90toRad = 1.5708_R64P
    
    !============================================
    !  type: Chebyshev_paggregate_t
    !============================================
    type, public :: Chebyshev_paggregate_t
        
          private
          
         
          
           ! Number of particles  in aggregated emsemble
          integer(I32P)              :: m_np 
          
          ! Particles aggregate ID number
          integer(I32P)              :: m_ID
          
          ! Time evolution steps
          integer(I32P)              :: m_nt
          
          ! Maximal number of parametric equation points
          integer(I32P)              :: m_maxx, m_maxy, m_maxz
          
          
          
          ! Total volume of particles per ensemble
          real(R64P)                 :: m_tpv
          
          ! Total particles surface area per ensemble
          real(R64P)                 :: m_tpsa
          
          ! Total particles mass per ensemble
          real(R64P)                 :: m_tpm
          
           ! Hydrometeor type
          character(len=32)          :: m_htype
          
           ! Ensemble shape  only 2 types supported for now: -- ( spheroid, chebyshev-particle)
           character(len=64)         :: m_esh
           
          ! Chebyshev particles shape in aggregated assembly ( (r = r0[1+eTn(cos(theta))])
          ! [r,np], where r = parametric shape (cross-section), np =  n-th particle
          real(R64P), allocatable, dimension(:,:), public :: m_pcs
!DIR$     ATTRIBUTES ALIGN : 64 :: m_pcs
          
          ! Chebyshev particles radii in aggregate ensemble
          real(R64P), allocatable, dimension(:),   public :: m_pradii
!DIR$     ATTRIBUTES ALIGN : 64 :: m_pradii
          
          ! Trajectory of Chebyshev particles ensemble, radial distance component (spherical coordinate system)
          ! [r,np]
          real(R64P), allocatable, dimension(:,:), public :: m_prdist
!DIR$     ATTRIBUTES ALIGN : 64 :: m_prdist
          
           ! Trajectory of Chebyshev particles ensemble, theta angle component (spherical coordinate system)
           ! [theta,np]
          real(R64P), allocatable, dimension(:,:), public :: m_ptheta
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ptheta
          
          ! Trajectory of Chebyshev particles ensemble, phi angle component (spherical coordinate system)
          ! [phi,np]
          real(R64P), allocatable, dimension(:,:), public :: m_pphi
!DIR$     ATTRIBUTES ALIGN : 64 :: m_pphi
          
          ! Chebyshev particles aggregate shape approximated by 3D parametric equations.
          ! Components form location of the particle in the ensemble.
          ! [3,np], where first dimension represents coordinate components
          ! second dimension represent number of particles.
          real(R64P), allocatable, dimension(:,:), public :: m_pes
!DIR$     ATTRIBUTES ALIGN : 64 :: m_pes
          
          ! Chebyshev particles ensemble( per each particle) parametric equation in x - dimension (non-dimensional)
          ! [paramx,np]
          real(R64P), allocatable, dimension(:,:), public :: m_ppx
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ppx
          
          ! Chebyshev particles ensemble (per each particle)  parametric equation in y  - dimension (non-dimensional)
          real(R64P), allocatable, dimension(:,:), public :: m_ppy
!DIR$     ATTRIBUTES ALIGN : 64 :: m_ppy
          
         ! Chebyshev particles ensemble (per each particle)  parametric equation in z  - dimension (non-dimensional)
          real(R64P), allocatable, dimension(:,:), public :: m_ppz
!DIR$    ATTRIBUTES ALIGN : 64 :: m_ppz
          
          ! Chebyshev particles ensemble fall speed 
          ![nt]
          real(R64P), allocatable, dimension(:),   public :: m_pfv
!DIR$     ATTRIBUTES ALIGN : 64 :: m_pvf
          
          contains
    
          !========================================================
          !         Class getters
          !========================================================
    
          procedure, pass(this), public :: get_np
          
          procedure, pass(this), public :: get_ID
          
          procedure, pass(this), public :: get_nt
          
          procedure, pass(this), public :: get_maxx
          
          procedure, pass(this), public :: get_maxy
          
          procedure, pass(this), public :: get_maxz
          
          procedure, pass(this), public :: get_htype
          
          procedure, pass(this), public :: get_esh
          
          procedure, pass(this), public :: get_tpv
          
          procedure, pass(this), public :: get_tpsa
          
          procedure, pass(this), public :: get_tpm
          
          !==============================================56
          ! write/read subroutines
          !==============================================56
          
          procedure, public :: read_state
          
          procedure, public :: write_state
          
          !===============================================
          !  Class helper procedures
          !===============================================
          procedure, public :: dbg_info
          
          !===============================================
          !   Generic operators
          !===============================================
          procedure, public :: copy_assign
        
          generic :: assignment (=) => copy_assign
          
          !================================================
          !  Computational procedures
          !================================================
          
          procedure, public :: compute_cross_section
          
          procedure, public :: compute_ensemble_shape
          
          procedure, public :: compute_x_param
          
          procedure, public :: compute_y_param
          
          procedure, public :: compute_z_param
          
          procedure, public :: compute_ensemble_vol
          
          procedure, public :: compute_ensemble_surf
          
          procedure, public :: compute_vfall
          
         
          
    end type Chebyshev_paggregate_t
          
          
    interface Chebyshev_paggregate_t
        procedure :: constructor
    end interface Chebyshev_paggregate_t
    
    contains
    
    !================================================================
    !  @function:  constructor
    !             Memory allocation and
    !             default initialization of array and scalar
    !             member variables.
    !=================================================================
    pure type(Chebyshev_paggregate_t)  function constructor(np,ID,nt,htype,esh,maxx,maxy,maxz,  &
                                                         msval,verbose,logging,filename,append    )
          use mod_print_error,  only : handle_fatal_memory_error
          use mod_constants,    only : LAM_PINF
          integer(I32P),        intent(in) :: np
          integer(I32P),        intent(in) :: ID
          integer(I32P),        intent(in) :: nt
          character(len=32),    intent(in) :: htype
          character(len=64),    intent(in) :: esh
          integer(I32P),        intent(in) :: maxx
          integer(I32P),        intent(in) :: maxy
          integer(I32P),        intent(in) :: maxz
          integer(I32P),        intent(in) :: msval   ! max values per particle's shape
          logical(I32P),        intent(in) :: verbose
          logical(I32P),        intent(in) :: logging
          character(len=*),     intent(in) :: filename
          logical(I32P),        intent(in) :: append
          ! Locals
          integer(I32P)      :: aerr
          character(len=256) :: emsg
          ! Exec code ......
          constructor%m_np    = np
          constructor%m_ID    = ID
          constructor%m_nt    = nt
          constructor%m_maxx  = maxx
          constructor%m_maxy  = maxy
          constructor%m_maxz  = maxz
          constructor%m_tpv   = LAM_PINF
          constructor%m_tpsa  = LAM_PINF
          constructor%m_tpm   = LAM_PINF
          constructor%m_htype = htype
          constructor%m_esh   = esh
         
          
          associate(mnp=>constructor%m_np,  &
                    mnt=>constructor%m_nt,  &
                    mmx=>constructor%m_mmx, &
                    mmy=>constructor%m_mmy, &
                    mmz=>constructor%m_mmz   )
              
              allocate(  constructor%m_pcs(msval,mnp),  &
                         constructor%m_pradii(mnp),     &
                         constructor%m_rdist(mnt,mnp),  &
                         constructor%m_ptheta(mnt,mnp), &
                         constructor%m_pphi(mnt,mnp),   &
                         constructor%m_pes(3,mnp),      &
                         constructor%m_ppx(mmx,mnp),    &
                         constructor%m_ppy(mmy,mnp),    &
                         constructor%m_ppz(mmz,mnp),    &
                         constructor%m_pfv(mnt),        &
                         STAT=aerr,                     &
                         ERRMSG=emsg             )
              
         end associate
         if(aerr /= 0_I32P) then
             call handle_fatal_memory_error(  logging,verbose,append,filename,   &
                                   "logger:303 -- Module: [mod_cheb_particles_aggregate], subroutine: [Chebyshev_paggregate_t%constructor]" ,  &
                                   "------ !!! FATAL-ERROR !!! ------ Memory Allocation Failure !!", &
                                    emsg,__LINE__)
         end if
         constructor%m_pcs    = LAM_PINF
         constructor%m_pradii = LAM_PINF
         constructor%m_rdist  = LAM_PINF
         constructor%m_ptheta = LAM_PINF
         constructor%m_pphi   = LAM_PINF
         constructor%m_pes    = LAM_PINF
         constructor%m_ppx    = LAM_PINF
         constructor%m_ppy    = LAM_PINF
         constructor%m_ppz    = LAM_PINF
         constructor%m_pfv    = LAM_PINF
    end function constructor
    
    !==========================================
    !       Class getters
    !==========================================

!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_np
!DIR$ ENDIF
    pure function get_np(this) result(np)
          class(Chebyshev_paggregate_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: np
          ! Exec code ....
          np = this%m_np
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_ID
!DIR$ ENDIF 
    pure function get_ID(this) result(ID)
          class(Chebyshev_paggregate_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: ID
          ! Exec code ....
          ID = this%m_ID
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nt
!DIR$ ENDIF 
    pure function get_nt(this)  result(nt)
          class(Chebyshev_paggregate_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: nt
          ! Exec code ....
          nt = this%m_nt
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_maxx
!DIR$ ENDIF 
    pure function get_maxx(this) result(maxx)
          class(Chebyshev_paggregate_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: maxx
          ! Exec code ...
          maxx = this%m_maxx
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_maxy
!DIR$ ENDIF 
    pure function get_maxy(this) result(maxy)
          class(Chebyshev_paggregate_t), intent(in) :: this
          ! Locals
          integer(I32P) :: maxy
          ! Exec code ...
          maxy = this%m_maxy
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_maxz
!DIR$ ENDIF 
    pure function get_maxz(this) result(maxz)
          class(Chebyshev_paggregate_t), intent(in) :: this
          ! Locals
          integer(I32P) :: maxz
          ! Exec code ...
          maxz = this%m_maxz
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_htype
!DIR$ ENDIF    
    pure function get_htype(this) result(htype)
          class(Chebyshev_paggregate_t),  intent(in) :: this
          ! Locals
          character(len=32) :: htype
          ! Exec code ....
          htype = this%m_htype
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_esh
!DIR$ ENDIF  
    pure function get_esh(this)  result(esh)
          class(Chebyshev_paggregate_t),  intent(in) :: this
          ! Locals
          character(len=64) :: esh
          ! Exec code ...
          esh = this%m_esh
    end function
    
 !DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_tpv
!DIR$ ENDIF 
    pure function get_tpv(this) result(tpv)
          class(Chebyshev_paggregate_t),  intent(in) :: this
          ! Locals
          real(R64P) :: tpv
          ! Exec code ...
          tpv = this%m_tpv
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_tpsa
!DIR$ ENDIF 
    pure function get_tpsa(this) result(tpsa)
          class(Chebyshev_paggregate_t), intent(in) :: this
          ! Locals
          real(R64P) ::  tpsa
          ! Exec code ....
          tpsa = this%m_tpsa
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_tpm
!DIR$ ENDIF 
    pure function get_tpm(this) result(tpm)
          class(Chebyshev_paggregate_t), intent(in) :: this
          ! Locals
          real(R64P) :: tpm
          ! Exec code ...
          tpm = this%m_tpm
    end function
    
     !==================================================
    !       Read write procedures
    !==================================================
    subroutine read_state(this,form,unit,ioerr)
        
          class(Chebyshev_paggregate_t),   intent(in)    :: this
          character(len=*),                intent(in)    :: form
          integer(I32P),                   intent(in)    :: unit
          integer(I32P),                   intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine read_state
    
    subroutine write_state(this,form,unit,ioerr)
         
          class(Chebyshev_paggregate_t),   intent(in)    :: this
          character(len=*),                intent(in)    :: form
          integer(I32P),                   intent(in)    :: unit
          integer(I32P),                   intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine write_state
    
    subroutine dbg_info(this,show_arrays)
          class(Chebyshev_paggregate_t),  intent(in) :: this
          logical(I32P),                  intent(in) :: show_arrays
          ! Exec code ....
          print*, "=================================================================="
          print*, "         Dumping: -- Chebyshev_paggregate_t -- object state        "
          print*, "=================================================================="
          print*, "         Chebyshev_paggregate_t: -- Memory layout                  "
          write(stdout,10) LOC(this)
10        format('addressof(this):                   -- ',Z15)
          write(stdout,20) LOC(this%m_np)
20        format('addressof(this%m_np):              -- ',Z15)
          write(stdout,30) LOC(this%m_ID)
30        format('addressof(this%m_ID):              -- ',Z15)
          write(stdout,40) LOC(this%m_nt)
40        format('addressof(this%m_nt):              -- ',Z15)
          write(stdout,50) LOC(this%m_maxx)
50        format('addressof(this%m_maxx):            -- ',Z15)
          write(stdout,60) LOC(this%m_maxy)
60        format('addressof(this%m_maxy):            -- ',Z15)
          write(stdout,70) LOC(this%m_maxz)
70        format('addressof(this%m_maxz):            -- ',Z15)
          write(stdout,80) LOC(this%m_htype)
80        format('addressof(this%m_htype):           -- ',Z15)
          write(stdout,90) LOC(this%m_esh)
90        format('addressof(this%m_esh):             -- ',Z15)
          write(stdout,100) LOC(this%m_tpv)
100       format('addressof(this%m_tpv):             -- ',Z15)
          write(stdout,110) LOC(this%m_tpsa)
110       format('addressof(this%m_tpsa):            -- ',Z15)
          write(stdout,120) LOC(this%m_tpm)
120       format('addressof(this%m_tpm):             -- ',Z15)
          write(stdout,130) LOC(this%m_pcs)
130       format('addressof(this%m_pcs):             -- ',Z15)
          write(stdout,140) LOC(this%m_pradii)
140       format('addressof(this%m_pradii):          -- ',Z15)
          write(stdout,150) LOC(this%m_rdist)
150       format('addressof(this%m_rdist):           -- ',Z15)
          write(stdout,160) LOC(this%m_ptheta)
160       format('addressof(this%m_ptheta):          -- ',Z15)
          write(stdout,170) LOC(this%m_pphi)
170       format('addressof(this%m_pphi):            -- ',Z15)
          write(stdout,180) LOC(this%m_pes)
180       format('addressof(this%m_pes):             -- ',Z15)
          write(stdout,190) LOC(this%m_ppx)
190       format('addressof(this%m_ppx):             -- ',Z15)
          write(stdout,200) LOC(this%m_ppy)
200       format('addressof(this%m_ppy):             -- ',Z15)
          write(stdout,210) LOC(this%m_ppz)
210       format('addressof(this%m_ppz):             -- ',Z15) 
          write(stdout,220) LOC(this%m_pfv)
220       format('addressof(this%m_pfv):             -- ',Z15)
          print*, "==========================================================="
          print*, "        Dumping scalar and array member values             "
          print*, "==========================================================="
          print*, "  m_np:    ",  this%m_np,    &
                  "  m_ID:    ",  this%m_ID,    &
                  "  m_nt:    ",  this%m_nt,    &
                  "  m_maxx:  ",  this%m_maxx,  &
                  "  m_maxy:  ",  this%m_maxy,  &
                  "  m_maxz:  ",  this%m_maxz,  &
                  "  m_tpv:   ",  this%m_tpv,   &
                  "  m_tpsa:  ",  this%m_tpsa,  &
                  "  m_tpm:   ",  this%m_tpm,   &
                  "  m_htype: ",  this%m_htype, &
                  "  m_esh:   ",  this%m_esh
          if( show_arrays == .true. )  then  
                   print*, "  Caller decided to print array members content.            "
                   print*, "============================================================"
                   print*, " m_pcs:     ", this%m_pcs
                   print*, " m_pradii:  ", this%m_pradii
                   print*, " m_rdist:   ", this%m_rdist
                   print*, " m_ptheta:  ", this%m_ptheta
                   print*, " m_pphi:    ", this%m_pphi
                   print*, " m_pes:     ", this%m_pes
                   print*, " m_ppx:     ", this%m_ppx
                   print*, " m_ppy:     ", this%m_ppy
                   print*, " m_ppz:     ", this%m_ppz
                   print*, " m_pfv:     ", this%m_pfv
          else
                     print*, " Caller decided to skip over array content printing."
          end if
          print*, "==============================================================="
          print*, "     End of: -- Chebyshev_paggregate_t object state dump        "
          print*, "==============================================================="
    end subroutine
    
    subroutine copy_assign(this,other)
          class(Chebyshev_paggregate_t),    intent(inout) :: this
          class(Chebyshev_paggregate_t),    intent(in)    :: other
          ! Exec code ...
          this%m_np     = other%m_np
          this%m_ID     = other%m_ID
          this%m_nt     = other%m_nt
          this%m_maxx   = other%m_maxx
          this%m_maxy   = other%m_maxy
          this%m_maxz   = other%m_maxz
          this%m_tpv    = other%m_tpv
          this%m_tpsa   = other%m_tpsa
          this%m_tpm    = other%m_tpm
          this%m_htype  = other%m_htype
          this%m_esh    = other%m_esh
          this%m_pcs    = other%m_pcs
          this%m_pradii = other%m_pradii
          this%m_rdist  = other%m_rdist
          this%m_ptheta = other%m_ptheta
          this%m_pphi   = other%m_pphi
          this%m_pes    = other%m_pes
          this%m_ppx    = other%m_ppx
          this%m_ppy    = other%m_ppy
          this%m_ppz    = other%m_ppz
          this%m_pfv    = other%m_pfv
    end subroutine
    
    !===================================================================
    !                   Computational procedures 
    !  These procedures  type bound, but in order to facilitate potential
    !  OpenMP parallelization (to be done later) derived type is not
    !  passed explicitly as an argument.
    !  Only Chebyshev Particles of type T2 and T4 should be used, at
    !  least in accordance to Wycombe paper.
    !===================================================================
    
    !===============================================================================
    ! Compute particles shape as a cross-section
    ! This code relies on compuation of radial distance as function of un-perturbed
    ! sphere radius, deformity coefficient and T(n) curve.
    !===============================================================================
    subroutine compute_cross_section(pcs,np,nrval,sphrad,chebn,cdeform,inith,incth,ierr,  &
                                     verbose,ierr,fp_flags                  )
 !DIR$  IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$   ENDIF
          use mod_print_error,  only : print_non_fatal_error
          use mod_constants,    only : LAM_dtcoeff
          real(R64P),    dimension(nrval,np),       intent(out)   :: pcs
          integer(I32P),                            intent(in)    :: np
          integer(I32P),                            intent(in)    :: nrval
          real(R64P),    dimension(np),             intent(in)    :: sphrad
          real(R64P),    dimension(np),             intent(in)    :: chebn
          real(R64P),    dimension(np),             intent(in)    :: cdeform
          real(R64P),                               intent(in)    :: inith
          real(R64P),                               intent(in)    :: incth
          integer(I32P),                            intent(inout) :: ierr
          logical(I32P),                            intent(in)    :: verbose
          logical(I32P), dimension(5),              intent(inout) :: fp_flags
          ! Locals
          integer(I32P)     :: j,i
          real(R64P)        :: theta,term,r
          character(len=40) :: sdate,stime
!DIR$     IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)    :: status_value
!DIR$     ENDIF
          ! Exec code .....
          ! Input arguments checking
          if(ierr < 0_I32P) ierr = 0_I32P
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          if( inith < 0._R64P  ) then
                  if(verbose == .true.)  then
                       call print_non_fatal_error( "=================== Non-Fatal Error =====================",  &
                          " Module name: -- [mod_cheb_particles_aggregate],  subroutine name: [compute_cross_section]: -- Invalid Argument!!", &
                             sdate,stime,__LINE__     )
                  end if
                  ierr = -1_I32P
                  return
          end if
          do i = 1_I32P, size(LAM_dtcoeff)
              if(incth /= LAM_dtcoeff(i)) then
                  if(verbose == .true.) then
                         call print_non_fatal_error( "============================ Non-Fatal Error ==========================", &
                             " Module name: -- [mod_cheb_particles_aggregate],  subroutine name: [compute_cross_section]: -- Invalid: 'incth' or 'incphi' value.", &
                             sdate,stime,__LINE__  )
                  end if
                  ierr = -2_I32P
                  return
              end if
          end do
          do i = 1_I32P,    mxp
             if((chebn(i)       < 0.0_R64P ) .OR.  &
                (abs(cdeform(i) > 1.0_R64P)    ) then
                    if(verbose == .true. ) then
                           call print_non_fatal_error( "========================== Non-Fatal Error =========================", &
                             " Module name: -- [mod_cheb_particles_aggregate],  subroutine name: [compute_cross_section]: -- Invalid: T(n) order ,or deform coeff.", &
                             sdate,stime,__LINE__)
                    end if
                    ierr = -3_I32P
                    return
             end if
          end do
          term = 0.0_R64P
          theta = 0.0_R64P
          r     = 0.0_R64P
          ! Computational loop
          ! IEEE exception cheking if enabled is perfromed per each interation 
          ! of 'j' (outer) loop.
          do j = 1_I32P,   np
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                 call ieee_get_status(status_value)
                 call ieee_set_halting_mode(ieee_all,.false.)
                 call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
                 do i = 1_I32P, nrval
                     theta = theta + incth
                     term = 1.0_R64P + cdeform(j) * cos(chebn(j) * theta)
                     pcs(i,j) = sphrad(j) * term
                 end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                call ieee_get_flag(ieee_all,fp_flags)
                if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " compute_cross_section: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags, "at iteration: ", j
                        print "====================================================================================================="
                end if
                call ieee_set_status(status_value)
!DIR$ ENDIF     
          end do
          ierr = 0_I32P
    end subroutine
    
    !=================================================================71
    !     Computation of ensemble shape.
    !     As for now only 3 shapes are supported:
    !     1) Cylindrical
    !     2) Pure spherical
    !     3) Chebyshev particle like
    !=================================================================71
    subroutine compute_ensemble_shape( pes,np,inz,incz,shape_name, &
                                       r,inphi,inth,incphi,incth,sphrad,chebn,cdeform,verbose,ierr,fp_flags)
!DIR$  IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$   ENDIF
          use mod_print_error,  only : print_non_fatal_error
          real(R64P),   dimension(3,np),    intent(out)           :: pes
          integer(I32P),                    intent(in)            :: np
          real(R64P),                       intent(in), optional  :: inz
          real(R64P),                       intent(in), optional  :: incz
          character(len=64),                intent(in)            :: shape_name
          real(R64P),                       intent(in), optional  :: r
          real(R64P),                       intent(in), optional  :: inphi
          real(R64P),                       intent(in), optional  :: inth
          real(R64P),                       intent(in), optional  :: incphi
          real(R64P),                       intent(in), optional  :: incth
          real(R64P),                       intent(in), optional  :: sphrad
          real(R64P),                       intent(in), optional  :: chebn
          real(R64P),                       intent(in), optional  :: cdeform
          logical(I32P),                    intent(in)            :: verbose
          integer(I32P),                    intent(inout)         :: ierr
          logical(I32P),dimension(5),       intent(inout)         :: fp_flags
        
         
          ! Locals
          integer(I32P)     :: i
          real(R64P)        :: term1,phi,theta,x,y,z,u
          character(len=40) :: sdate,stime
!DIR$     IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)    :: status_value
!DIR$     ENDIF
          ! Exec code .....
          ! Input arguments checking    
          if(ierr = 0_I32P) ierr = 0_I32P
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          
          ! Begin computation
          select case (trim(shape_name))
          case("Cylindrical")
              if(  present(inz)  .AND.  &
                   present(incz) .AND.  &
                   present(r)    .AND.  &
                   present(inth) .AND.  &
                   present(incth)       ) then
                 
                    ! Parametric equation 
                    ! Begin computation
                  
                    z = inz
                    theta = inth
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                    call ieee_get_status(status_value)
                    call ieee_set_halting_mode(ieee_all,.false.)
                    call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF                     
                    do i = 1_I32P,    np
                        theta = theta + incth
                        z = z + incz
                        pes(1,i) = r * cos(theta)
                        pes(2,i) = r * sin(theta)
                        pes(3,i) = z
                    end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                    call ieee_get_flag(ieee_all,fp_flags)
                    if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " compute_ensemble_shape: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                        print "====================================================================================================="
                    end if
                   call ieee_set_status(status_value)
!DIR$ ENDIF                       
              else
                   if(verbose == .true.) then
                          call print_non_fatal_error( "========================== Non-Fatal Error =========================", &
                             " Module name: -- [mod_cheb_particles_aggregate],  subroutine name: [compute_ensemble_shape]: -- Optional argument(s) non present", &
                               sdate,stime,__LINE__)
                   end if
                   ierr = -1_I32P
                   return
              end if
          case("Spheroidal")
              if(present(r)      .AND. &
                 present(inth)   .AND. &
                 present(incth)  .AND. &
                 present(inphi)  .AND. &
                 present(incphi)        )  then
                    ! Parametric equation 
                    ! Begin computation
                    theta = inth
                    phi   = inphi
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                    call ieee_get_status(status_value)
                    call ieee_set_halting_mode(ieee_all,.false.)
                    call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
                    do i = 1_I32P,  np
                        theta = theta + incth
                        phi   = phi   + incphi
                        u = r * cos(phi)
                        pes(1,i) = sqrt(r**2 - u**2) * cos(theta)
                        pes(2,i) = sqrt(r**2 - u**2) * sin(theta)
                        pes(3,i) = u
                    end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                    call ieee_get_flag(ieee_all,fp_flags)
                    if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " compute_ensemble_shape: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                        print "====================================================================================================="
                    end if
                    call ieee_set_status(status_value)
!DIR$ ENDIF         
              else
                  if(verbose == .true.) then
                      call print_non_fatal_error( "========================== Non-Fatal Error =========================", &
                             " Module name: -- [mod_cheb_particles_aggregate],  subroutine name: [compute_ensemble_shape]: -- Optional argument(s) non present", &
                               sdate,stime,__LINE__)
                  end if
                  ierr = -1_I32P
                  return
              end if
          case("ChebyshevParticle")
               if(present(inth)   .AND.  &
                  present(incth)  .AND.  &
                  present(inphi)  .AND.  &
                  present(incphi) .AND.  &
                  present(sphrad) .AND.  &
                  present(chebn)  .AND.  &
                  present(cdeform)              ) then
                    ! Parametric equation 
                    ! Begin computation
                     theta = inth
                     phi   = inphi
                     x = 0.0_R64P
                     y = 0.0_R64P
                     z = 0.0_R64P
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                    call ieee_get_status(status_value)
                    call ieee_set_halting_mode(ieee_all,.false.)
                    call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
                    do i = 1_I32P,   np
                        theta = theta + incth
                        phi   = phi   + incphi
                        term1 = sphrad * (1.0_R64P + cdeform * cos(chebn * theta)
                        x = term1 * sin(theta) * cos(phi)
                        pes(1,i) = x
                        y = term1 * sin(theta) * sin(phi)
                        pes(2,i) = y
                        z = term1 * cos(theta)
                        pes(3,i) = z
                    end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                    call ieee_get_flag(ieee_all,fp_flags)
                    if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " compute_ensemble_shape: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                        print "====================================================================================================="
                    end if
                    call ieee_set_status(status_value)
!DIR$ ENDIF          
              else
                  if(verbose == .true.) then
                        call print_non_fatal_error( "========================== Non-Fatal Error =========================", &
                             " Module name: -- [mod_cheb_particles_aggregate],  subroutine name: [compute_ensemble_shape]: -- Optional argument(s) non present", &
                               sdate,stime,__LINE__)
                  end if
                  ierr = -1_I32P
                  return
              end if
          case default
                if(verbose == .true.) then
                     call print_non_fatal_error( "========================== Non-Fatal Error =========================", &
                             " Module name: -- [mod_cheb_particles_aggregate],  subroutine name: [compute_ensemble_shape]: -- Invalid switch argument", &
                               sdate,stime,__LINE__)
                end if
                ierr = -2_I32P
                return
          end select
          ierr = 0_I32P
    end subroutine
    
    !=================================================================71
    !   
    !     Chebyshev Particles parametric equation in parameter 'x'
    !     x = r0[1+- eTn(cos(theta))]sin(theta) cos(phi)
    !
    !=================================================================71
    subroutine compute_x_param(ppx,maxx,np,sphrad,chebn,cdeform,inith,incth,  &
                               iniphi,incphi,verbose,ierr,fp_flags        )
!DIR$   IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$   ENDIF
          use mod_print_error, only : print_non_fatal_error
          use mod_constants,   only : LAM_dtcoeff   
          real(R64P),       dimension(maxx,np),     intent(out)    :: ppx
          integer(I32P),                            intent(in)    :: maxx
          integer(I32P),                            intent(in)    :: np
          real(R64P),       dimension(np),          intent(in)    :: sphrad
          real(R64P),       dimension(np),          intent(in)    :: chebn
          real(R64P),       dimension(np),          intent(in)    :: cdeform
          real(R64P),                               intent(in)    :: inith
          real(R64P),                               intent(in)    :: incth
          real(R64P),                               intent(in)    :: iniphi
          real(R64P),                               intent(in)    :: incphi
          logical(I32P),                            intent(in)    :: verbose
          integer(I32P),                            intent(inout) :: ierr
          logical(I32P),  dimension(5),             intent(inout) :: fp_flags
          ! Locals
          integer(I32P)     :: j,i
          real(R64P)        :: term,theta,phi,x
          character(len=40) :: sdate,stime
!DIR$     IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)    :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity check
          if(ierr = 0_I32P) ierr = 0_I32P
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          if( iniphi < 0.0_R64P .OR. iniphi > 4.0*Deg90toRad ) then
              if(verbose == .true. ) then
                  call print_non_fatal_error(  " =================== Non-Fatal Error ===================== " , &
                         " Module name: -- [mod_cheb_particles_aggregate],  subroutine name: [compute_x_param]: -- Invalid Argument!! " , &
                                 sdate,stime,__LINE__  )
              end if
              ierr = -1_I32P
              return
          end if
          do j = 1_I32P,  size(LAM_dtcoeff)
              if(incphi  /= LAM_dtcoeff(i)) then
                  if(verbose == .true. ) then
                      call print_non_fatal_error(  " =================== Non-Fatal Error ===================== ", &
                             " Module name: -- [mod_cheb_particles_aggregate],  subroutine name: [compute_x_param]: -- Invalid incth or incphi value!!", &
                                sdate,stime, __LINE__ )
                  end if
                  ierr = -2_I32P
                  return
              end if
          end do
          term  = 0.0_R64P
          theta = inith
          phi   = iniphi
          x     = 0.0_R64P
          ! Begin computational loop
          do j = 1_I32P,     np
               ! Check ieee fp-exception register status per each iteration of 'j' loop
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
              do i  = 1_I32P,  maxx
                  theta = theta + incth
                  phi   = phi   + incphi
                  term = sphrad(j) * (1.0_R64P + cdeform(j) * cos(chebn(j) * theta)
                  x    = term * sin(theta) * cos(phi)
                  ppx(i,j) = x
              end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                call ieee_get_flag(ieee_all,fp_flags)
                if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " compute_x_param: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags, "at iteration: ", j
                        print "====================================================================================================="
                end if
                call ieee_set_status(status_value)
!DIR$ ENDIF        
          end do
          ierr = 0_I32P
    end subroutine
    
     
    !==========================================================================80
    !     Chebyshev Particles parametric equation in parameter 'y'
    !     x = r0[1+- eTn(cos(theta))]sin(theta) sin(phi)
    !==========================================================================80
    subroutine compute_y_param(ppy,maxy,np,sphrad,chebn,cdeform,inith,incth,   &
                                iniphi,incphi,verbose,ierr,fp_flags         )
!DIR$   IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$   ENDIF
          use mod_print_error, only : print_non_fatal_error
          use mod_constants,   only : LAM_dtcoeff 
          real(R64P),       dimension(maxy,np),     intent(out) :: ppy
          integer(I32P),                            intent(in)  :: maxy
          integer(I32P),                            intent(in)  :: np
          real(R64P),       dimension(np),          intent(in)    :: sphrad
          real(R64P),       dimension(np),          intent(in)    :: chebn
          real(R64P),       dimension(np),          intent(in)    :: cdeform
          real(R64P),                               intent(in)    :: inith
          real(R64P),                               intent(in)    :: incth
          real(R64P),                               intent(in)    :: iniphi
          real(R64P),                               intent(in)    :: incphi
          logical(I32P),                            intent(in)    :: verbose
          integer(I32P),                            intent(inout) :: ierr
          logical(I32P),  dimension(5),             intent(inout) :: fp_flags
          ! Locals
          integer(I32P)     :: j,i
          real(R64P)        :: term,theta,phi,y
          character(len=40) :: sdate,stime
!DIR$     IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)    :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity check
            ! Exec code .....
          ! Sanity checks
          if(ierr < 0_I32P) ierr = 0_I32P
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          if( iniphi < 0.0_R64P .OR. iniphi > 4.0_R64P*Deg90toRad  ) then
              if(verbose == .true. ) then
                  call print_non_fatal_error(   " =================== Non-Fatal Error ===================== " , &
                                " Module name: -- [mod_cheb_particles_aggregate],  subroutine name: [compute_y_param]: -- Invalid Argument!! " , &
                                 sdate,stime,__LINE__  )
              end if
              ierr = -1_I32P
              return
          end if
          do i = 1_I32P, size(LAM_dtcoeff)
              if( incphi /= LAM_dtcoeff(i)) then
                  if(verbose == .true.) then
                       call print_non_fatal_error(  " =================== Non-Fatal Error ===================== ", &
                                " Module name: -- [mod_cheb_particles_aggregate],  subroutine name: [compute_y_param]: -- Invalid incth or incphi value!!", &
                                sdate,stime, __LINE__ )
                  end if
                  ierr = -2_I32P
                  return
              end if
          end do
          term  = 0.0_R64P
          theta = inith
          phi   = iniphi
          y     = 0.0_R64P
          ! Begin a computation loop
          do j = 1_I32P,  np
             ! Check ieee fp-exception register status per each iteration of 'j' loop
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
              do i = 1_I32P, maxy
                  theta = theta + incth
                  phi   = phi   + incphi
                  term = sphrad(j) * 1.0_R64P + cdeform(j) * cos(chebn(j) * theta)
                  y = term * sin(theta) * sin(phi)
                  ppy(i,j) = y
              end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                call ieee_get_flag(ieee_all,fp_flags)
                if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " compute_y_param: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags, "at iteration: ", j
                        print "====================================================================================================="
                end if
                call ieee_set_status(status_value)
!DIR$ ENDIF           
          end do
          ierr = 0_I32P
    end subroutine
    
    !==========================================================================80
    !     Chebyshev Particles parametric equation in parameter 'z'
    !     x = r0[1+- eTn(cos(theta))]sin(theta)
    !==========================================================================80
    subroutine compute_z_param(ppz,maxz,np,sphrad,chebn,cdeform,inith,incth,verbose,ierr,fp_flags)
!DIR$  IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$  ENDIF
          use mod_print_error, only : print_non_fatal_error
          use mod_constants,   only : LAM_dtcoeff 
          real(R64P),       dimension(maxz,np),     intent(out)   :: ppz
          integer(I32P),                            intent(in)    :: maxz
          integer(I32P),                            intent(in)    :: np
          real(R64P),       dimension(np),          intent(in)    :: sphrad
          real(R64P),       dimension(np),          intent(in)    :: chebn
          real(R64P),       dimension(np),          intent(in)    :: cdeform
          real(R64P),                               intent(in)    :: inith
          real(R64P),                               intent(in)    :: incth
          logical(I32P),                            intent(in)    :: verbose
          integer(I32P),                            intent(inout) :: ierr
          logical(I32P),    dimension(5),           intent(inout) :: fp_flags
          ! Locals
          integer(I32P) :: j,i
          real(R64P)    :: term,theta,z
          character(len=40) :: sdate,stime
!DIR$    IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)    :: status_value
!DIR$    ENDIF
          ! Exec code ....
          ! Sanity check
          if(ierr < 0_I32P) ierr = 0_I32P
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          term = 0.0_R64P
          theta = 0.0_R64P
          z = 0.0_R64P
          ! Begin computational loop
          do j = 1_I32P,   np
               ! Check ieee fp-exception register status per each iteration of 'j' loop
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
              do i = 1_I32P,   maxz
                  theta = theta + incth
                
                        term = sphrad(j) * (1.0_R64P + cdeform(j) + cos(chebn(j) * theta))
                        z = term * cos(theta)
                        ppz(i,j) = z
                 
              end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                call ieee_get_flag(ieee_all,fp_flags)
                if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " compute_z_param: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags, "at iteration: ", j
                        print "====================================================================================================="
                end if
                call ieee_set_status(status_value)
!DIR$ ENDIF    
          end do
          ierr = 0_I32P
              
    end subroutine
    
    !==========================================================================80
    !           Computing Chebyshev ensemble particles volume.
    !           No argument verification is performed on array arguments like:
    !           sphrad,chebn and cdeform (these argument are verified by compute
    !                                     cross_section subroutine)
    !==========================================================================80
    subroutine  compute_ensemble_vol(tpv,np,sphrad,chebn,cdeform,fp_flags)
!DIR$  IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$  ENDIF
          use mod_constants,  only : LAM_PI
          real(R64P),                     intent(out)   ::  tpv
          integer(I32P),                  intent(in)    ::  np
          real(R64P),     dimension(np),  intent(in)    ::  sphrad
          real(R64P),     dimension(np),  intent(in)    ::  chebn
          real(R64P),     dimension(np),  intent(in)    ::  cdeform
          logical(I32P),  dimension(5),   intent(inout) ::  fp_flags
          ! Locals
          integer(I32P) :: i
          real(R64P)    :: term1,term1a,term2,term3,term4
!DIR$     IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity checks 
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          term1 = 0.0_R64P
          term1a = 0.0_R64P
          term2 = 0.0_R64P
          term3 = 0.0_R64P
          term4 = 0.0_R64P
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          do i = 1_I32P,   np
                        term1 =   0.3333333333333333333333333_R64P*4.0_R64P*LAM_PI*sphrad(i)**3
                        term1a =  1.0_R64P + 1.5_R64P * cdeform(i)**2 * &
                                  (4.0_R64P*chebn(i)**2-2.0_R64P/4.0_R64P*chebn(i)**2-1.0_R64P)
                        if(iand(int(chebn(i),kind=4),1_I32P) == 0_I32P) then
                                term2 = 3.0_R64P * cdeform(i) * (1.0_R64P + cdeform(i)**2*0.25_R64P) / &
                                        (chebn(i)**2-1.0_R64P)
                                term3 = 0.25_R64P*cdeform(i)**3 / &
                                        (9.0_R64P*chebn(i)**2-1.0_R64P)
                                term4 = term1 * (term1a - term2 - term3)
                                tpv = tpv + term4
                        else
                                term2 = term1 * term1a
                                tpv = tpv + term2
                        end if
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print "             compute_ensemble_vol: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                        print "====================================================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF                          
    end subroutine
    
    !==========================================================================80
    !            Computes Chebyshev particles surface area (total per ensemble)  
    !            No argument verification is performed on array arguments like:
    !            sphrad,chebn and cdeform (these argument are verified by compute
    !                                     cross_section subroutine)
    !==========================================================================80
    subroutine compute_ensemble_surf(tps,np,sphrad,chebn,cdeform,fp_flags)
!DIR$  IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1
          use, intrinsic :: ieee_exceptions
!DIR$  ENDIF
          use mod_constants, only : LAM_PI
          real(R64P),                      intent(out)   :: tps
          integer(I32P),                   intent(in)    :: np
          real(R64P),       dimension(np), intent(in)    :: sphrad
          real(R64P),       dimension(np), intent(in)    :: chebn
          real(R64P),       dimension(np), intent(in)    :: cdeform
          logical(I32P),    dimension(5),  intent(inout) :: fp_flags
          ! Locals
          integer(I32P) :: i
          real(R64P)    :: term1,term2,term3,term4,term5,term5a,tmp
!DIR$     IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF
          ! Exec code ...
          ! Sanity check
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          term1  = 0.0_R64P
          term2  = 0.0_R64P
          term3  = 0.0_R64P
          term4  = 0.0_R64P
          term5  = 0.0_R64P
          term5a = 0.0_R64P
          tmp    = 0.0_R64P
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
          do i = 1_I32P,    np
              term1 = 4.0_R64P * LAM_PI * sphrad(i)**2
              
              if(iand(int(chebn(i),kind=4),1_I32P) == 0_I32P) then
                    term2 = 1.0_R64P - 2.0_R64P * cdeform(i)/(chebn(i)**2-1.0_R64P)
                    term3 = cdeform(i)**2*(chebn(i)**4+2.0_R64P*chebn(i)**2-1.0_R64P) / &
                            (4.0_R64P*chebn(i)**2-1.0_R64P)
                    term4 =  3.0_R64P*cdeform(k)**4*chebn(i)**8 / &
                             (64.0_R64P*chebn(i)**4-12.0_R64P*chebn(i)**2+1.0_R64P)
                    term5 =  -6.0_R64P*cdeform(k)**5*chebn(i)**8
                    term5a =             1.0_R64P /        &
                             ((chebn(i)**2-1.0_R64P)*(9.0_R64P*chebn(i)**2-1.0_R64P)*(25.0_R64P*chebn(i)**2-1.0_R64P))
                    tmp = term1 * (term2 + term3 - term4 - term5 * term5a)
                    tps = tps + tmp
              else
                    term2 = 1.0_R64P + cdeform(k)**2*(chebn(i)**4+2.0_R64P*chebn(i)**2-1.0_R64P) / &
                                    (4.0_R64P*chebn(i)**2-1.0_R64P)
                    term3 = 3.0_R64P*cdeform(k)**4*chebn(i)**4*0.015625_R64P
                    term4 = 1.0_R64P + 20.0_R64P*chebn(i)**2-1.0_R64P / &
                                    ((16.0_R64P*chebn(i)**2-1.0_R64P)*(4.0_R64P*chebn(i)**2-1.0_R64P))
                    tmp = term1 * (term2 - term3 * term4)
                    tps = tps + tmp
              end if
              
          end do
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print "            compute_ensemble_surf: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                        print "====================================================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF               
          
    end subroutine
    
    !============================================================================
    !      Vertical falling speed computation based on
    !     "Fall Velocities of Hydrometeors in the Atmosphere: 
    !      Refinements to Continous Analytical Power Law    "
    !      by Vitaliy I. Khvorostyanov and Judith A. Curry
    !      
    !      Adding as an option turbulence correction coefficients.
    !============================================================================
    subroutine compute_vfall(pfv,nt,aRe,bRe,vb,kvisc,nx,ny,nz,A,rho_b,rho_f,mD, &
                             Re,bRet,aRet,fp_flags                          )
 !DIR$  IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$  ENDIF
          real(R64P),       dimension(nt),       intent(out)    :: pfv
          integer(I32P),                         intent(in)     :: nt
          real(R64P),       dimension(nt),       intent(in)     :: aRe
          real(R64P),       dimension(nt),       intent(in)     :: bRe
          real(R64P),                            intent(in)     :: vb
          real(R64P),       dimension(nx,ny,nz), intent(in)     :: kvisc
          integer(I32P),                         intent(in)     :: nx
          integer(I32P),                         intent(in)     :: ny
          integer(I32P),                         intent(in)     :: nz
          real(R64P),                            intent(in)     :: A
          real(R64P),                            intent(in)     :: rho_b
          real(R64P),      dimension(nx,ny,nz),  intent(in)     :: rho_f
          real(R64P),                            intent(in)     :: mD
          real(R64P),                            intent(in)     :: Re
          real(R64P),      dimension(nt),        intent(in)     :: bRet  ! Turbulence correction term
          real(R64P),      dimension(nt),        intent(in)     :: aRet  ! Turbulence correction term
          logical(I32P),   dimension(5),         intent(inout)  :: fp_flags
          ! Locals
          integer(I32P) :: i,ix,iy,iz
          real(R64P)    :: term1,term2,term2a,term3
!DIR$  IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) ::
!DIR$  ENDIF
          ! Exec code ....
          ! Sanity checking
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          term1  = 0.0_R64P
          term2  = 0.0_R64P
          term2a = 0.0_R64P
          term3  = 0.0_R64P
          if((abs(Re) - 999.0_R64P) <= EPSILON(0.0_R64P) ) then
             ! Obtain ieee floating-point register state 
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
              do i = 1_I32P,  nt
                  
                  do iz = 1_I32P,  nz
                      do iy = 1_I32P,  ny
                          do ix = 1_I32P,  nx
                              term1 = aRet(i) * kvisc(ix,iy,iz)**1.0_R64P-2.0_R64P*aRet(i)
                              term2 = 2.0_R64P*vb*9.81_R64P / A
                              if( rho_b > rho_f(ix,iy,iz) ) then
                                    term2a = abs(rho_b / rho_f(ix,iy,iz) )
                              else
                                    term2a = abs(rho_b / rho_f(ix,iy,iz) - 1.0_R64P )
                              end if
                              term3 = mD**2.0_R64P*bRet(i) - 1.0_R64P
                              pfv(i) = term1 * (term2 * term2a)**bRet(i) * term3
                          end do
                      end do
                  end do
                  
              end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " compute_vfall: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                        print "====================================================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF  
          else
                   ! Obtain ieee floating-point register state 
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF    
              do i = 1_I32P,  nt
                  
                  do iz = 1_I32P,  nz
                      do iy = 1_I32P,  ny
                          do ix = 1_I32P,  nx
                              term1 = aRe(i) * kvisc(ix,iy,iz)**1.0_R64P-2.0_R64P*aRe(i)
                              term2 = 2.0_R64P*vb*9.81_R64P / A
                              if( rho_b > rho_f(ix,iy,iz) ) then
                                    term2a = abs(rho_b / rho_f(ix,iy,iz) )
                              else
                                    term2a = abs(rho_b / rho_f(ix,iy,iz) - 1.0_R64P )
                              end if
                              term3 = mD**2.0_R64P*bRe(i) - 1.0_R64P
                              pfv(i) = term1 * (term2 * term2a)**bRe(i) * term3
                          end do
                      end do
                  end do
                  
              end do
            
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " compute_vfall: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                        print "====================================================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF 
          end if
    
    end subroutine
    
    
    
end module mod_cheb_particles_aggregate