

#include "Config.fpp"

module mod_cheb_particles_single 

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_cheb_particles_single'
 !          
 !          Purpose:
 !                      This module models 'Chebyshev Particles' as single entities (not size
 !                      distributed per bin)
 !                       
 !          History:
 !                        Date: 29-07-2018
 !                        Time: 17:17 GMT+2
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
    
    public ::  constructor
    public ::  cps_compute_xsection,   &
               cps_compute_volume,     &
               cps_compute_surface,    &
               cps_compute_paramx,     &
               cps_compute_paramy,     &
               cps_compute_paramz,     &
               cps_compute_vfall_v1,   &
               cps_compute_vfall_v2

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P),  parameter, public :: MOD_CHEB_PARTICLES_SINGLE_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P),  parameter, public :: MOD_CHEB_PARTICLES_SINGLE_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P),  parameter, public :: MOD_CHEB_PARTICLES_SINGLE_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P),  parameter, public :: MOD_CHEB_PARTICLES_SINGLE_FULLVER = 1000_I32P*MOD_CHEB_PARTICLES_SINGLE_MAJOR + &
                                                                             100_I32P*MOD_CHEB_PARTICLES_SINGLE_MINOR  + &
                                                                             10_I32P*MOD_CHEB_PARTICLES_SINGLE_MICRO 
    
    ! Module creation date
    character(*),   parameter, public :: MOD_CHEB_PARTICLES_SINGLE_CREATE_DATE = "29-07-2018 17:17 +00200 (SUN 29 JUL 2018 GMT+2) "
    
    ! Module build date  (  should be set after successful compilation)
    character(*),   parameter, public :: MOD_CHEB_PARTICLES_SINGLE_BUILD_DATE = " "
    
    ! Module author info
    character(*),   parameter, public :: MOD_CHEB_PARTICLES_SINGLE_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),   parameter, public :: MOD_CHEB_PARTICLES_SINGLE_DESCRIPT = "Modeling of Chebyshev particles as  single entities."
    
    ! Constants
    
    real(R64P), parameter, private :: Deg90toRad = 1.5708_R64P
    
    !=========================================================63
    !   type: Chebyshev_particles_single_t
    !=========================================================63
    ! Particles are a single entities.
    type, public :: Chebyshev_psingle_t
        
          private
          
          ! Dimensioning indices
          integer(I32P)                                        :: m_nx,m_ny,m_nz
          
          ! Total number of particles per volume  (scanning domain)
          integer(I32P)                                        :: m_maxp
          
          ! Time evolution number of steps
          integer(I32P)                                        :: m_nt
          
          ! Particles type (  (dust-like,grail,snow,hail)
          character(len=32), dimension(m_maxp), public         :: m_particles_type
          
          ! Particles bounding-box (scannings volume domain) dimension values (based on
          ! geographical coordinates)
          real(R64P),   dimension(3,8)                         :: m_scan_domain
          
          ! Chebyshev particles shape (cross-section) per domain i.e.  (r = r0[1+eTn(cos(theta))])
          real(R64P), allocatable, dimension(:,:), public      :: m_xsection
!DIR$     ATTRIBUTES ALIGN : 64  :: m_particles_xsection  
          
          ! Chebyshev particles radii (per scanning domain)
          real(R64P), allocatable, dimension(:),   public      :: m_radii
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_radii   
          
          ! Trajectory of Chebyshev particles (as a single entities) radial distance time evolution 
          ! (spherical coordinate system) per scanning domain, [time evolution points,number of particles]
          real(R64P), allocatable, dimension(:,:),  public    :: m_rdist
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_range
          
          ! Trajectory of Chebyshev particles (as a single entities) theta spherical coordinate per scanning domain
          ! [time evolution points,number of particles]
          real(R64P), allocatable, dimension(:,:),  public    :: m_theta
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_theta
          
          ! Trajectory of Chebyshev particles (as a single entities) phi spherical coordinate per scanning domain
          ! [time evolution points,number of particles]
          real(R64P), allocatable, dimension(:,:),  public    :: m_phi
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_phi
          
          ! Chebyshev particles (single entities) volume per scanning domain
          real(R64P), allocatable, dimension(:),    public    :: m_volume
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_vol
          
          ! Chebyshev particles (single entities) surface area per scanning domain
          real(R64P), allocatable, dimension(:),    public    :: m_surface
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_area
          
          ! Chebyshev particles (single enities) parametric equation : parameter 'x' per scanning domain.
          real(R64P), allocatable, dimension(:,:),  public    :: m_paramx
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_paramx
          
          ! Chebyshev particles (single entities) parametric equation: parameter 'y' per scanning domain.
          real(R64P), allocatable, dimension(:,:),  public    :: m_paramy
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_paramy
          
          ! Chebyshev particles (single entities) parametric equation: parameter 'z' per scanning domain.
          real(R64P), allocatable, dimension(:,:),  public    :: m_paramz
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_paramz
          
          ! Chebyshev particles (single entities) vertical falling speed per scanning domain.
          ! [number of evaluation time steps,number of particles per domain]
          real(R64P), allocatable, dimension(:,:),  public    :: m_vfall
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_vfall
          
          ! Chebyshev particles (single entities) surface temperature (time-invariant) (units of Celsius)
          ! per scanning domain
          real(R64P), allocatable, dimension(:),   public     :: m_stemp
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_stemp
          
          ! Chebyshev particles (single entities) mass (time invariant) (units of miligram)
          real(R64P), allocatable, dimension(:),   public      :: m_mass
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_mass
          
          ! Chebyshev particles mixing ratio per scanning domain  (units of kg/kg)
          real(R64P), allocatable, dimension(:,:,:), public    :: m_mixratio
!DIR$     ATTRIBUTES ALIGN : 64 :: m_particles_mratio
          
          contains
    
          !========================================================
          !         Class getters
          !========================================================
    
          procedure, pass(this), public :: get_nx
          
          procedure, pass(this), public :: get_ny
          
          procedure, pass(this), public :: get_nz
          
          procedure, pass(this), public :: get_maxp
          
          procedure, pass(this), public :: get_nt
          
          procedure, pass(this), public :: get_sdomain
          
          !==============================================56
          ! write/read subroutines
          !==============================================56
          
          procedure, nopass,    public :: read_state
          
          procedure, nopass,    public :: write_state
          
          !===============================================
          !  Class helper procedures
          !===============================================
          
          procedure, pass(this), public :: dbg_info
          
          !===============================================
          !   Generic operators
          !===============================================
          
          procedure, public :: copy_assign
          
          generic :: assignment (=) => copy_assign
    
    end type Chebyshev_psingle_t
          
          
    interface     Chebyshev_psingle_t
        
        procedure :: constructor
        
    end interface Chebyshev_psingle_t
    
    contains
    
     
    !================================================================
    !  @function:  constructor
    !             Memory allocation and
    !             default initialization of array and scalar
    !             member variables.
    !=================================================================
    type(Chebyshev_psingle_t) function constructor(nx,ny,nz,maxp,nt,particles_type,scan_domain,  &
                                                 maxx,maxy,maxz,maxshv,verbose,logging,filename,append  )
          use mod_print_error,  only : handle_fatal_memory_error
          use mod_constants,    only : LAM_PINF
          integer(I32P),                        intent(in) :: nx
          integer(I32P),                        intent(in) :: ny
          integer(I32P),                        intent(in) :: nz
          integer(I32P),                        intent(in) :: maxp
          integer(I32P),                        intent(in) :: nt
          character(len=32),   dimension(maxp), intent(in) :: particles_type
          real(R64P),          dimension(3,8),  intent(in) ::  domain_coords
          integer(I32P),                        intent(in) :: maxx
          integer(I32P),                        intent(in) :: maxy
          integer(I32P),                        intent(in) :: maxz
          integer(I32P),                        intent(in) :: maxshv
          logical(I32P),                        intent(in) :: verbose
          logical(I32P),                        intent(in) :: logging
          character(len=*),                     intent(in) :: filename
          logical(I32P),                        intent(in) :: append
          ! Locals
          integer(I32P)      :: aerr
          character(len=256) :: emsg
          ! Exec code ....
          constructor%m_nx              = nx
          constructor%m_ny              = ny
          constructor%m_nz              = nz
          constructor%m_maxp            = maxp
          constructor%m_nt              = nt
          constructor%m_particles_type  = particles_type
          constructor%m_scan_domain     = scan_domain
          
          associate(mnx=>constructor%m_nx,      &
                    mny=>constructor%m_ny,      &
                    mnz=>constructor%m_nz,      &
                    mxp=>constructor%m_maxp,    &
                    mnt=>constructor%m_nt       )
              
                  allocate(   constructor%m_xsection(maxshv,mxp),        &
                              constructor%m_radii(mxp),                  &
                              constructor%m_rdist(mnt,mxp),              &
                              constructor%m_theta(mnt,mxp),              &
                              constructor%m_phi(mnt,mxp),                &
                              constructor%m_volume(mxp),                    &
                              constructor%m_surface(mxp),                   &
                              constructor%m_paramx(maxx,mxp),            &
                              constructor%m_paramy(maxy,mxp),            &
                              constructor%m_paramz(maxz,mxp),            &
                              constructor%m_vfall(mnt,mxp),              &
                              constructor%m_stemp(mxp),                  &
                              constructor%m_mass(mxp),                   &
                              constructor%m_mixratio(mnx,mny,mnz),         &
                              STAT=aerr,                                           &
                              ERRMSG=emsg                                  )
              
          end associate
          if(aerr /= 0_I32P) then
               call handle_fatal_memory_error( logging,verbose,append,filename,     &
                     "logger:282 -- Module name: [mod_cheb_particles_single], subroutine name: [constructor] -- Memory Allocation Failure!!", &
                     "logger:282 -- FATAL-ERROR -- Memory Allocation Failure !! ", &
                     emsg, __LINE__  )
          end if
          
          constructor%m_xsection = LAM_PINF
          constructor%m_radii    = LAM_PINF          
          constructor%m_rdist    = LAM_PINF
          constructor%m_theta    = LAM_PINF
          constructor%m_phi      = LAM_PINF
          constructor%m_volume   = LAM_PINF
          constructor%m_surface  = LAM_PINF
          constructor%m_paramx   = LAM_PINF
          constructor%m_paramy   = LAM_PINF
          constructor%m_paramz   = LAM_PINF
          constructor%m_vfall    = LAM_PINF
          constructor%m_stemp    = LAM_PINF
          constructor%m_mass     = LAM_PINF
          constructor%m_mixratio = LAM_PINF
          
    end function  constructor
    
    
    !==========================================
    !       Class getters
    !==========================================
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nx
!DIR$ ENDIF  
    pure function get_nx(this) result(nx)
          class(Chebyshev_psingle_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: nx
          ! Exec code ...
          nx = this%m_nx
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_ny
!DIR$ ENDIF  
    pure function get_ny(this) result(ny)
          class(Chebyshev_psingle_t),  intent(in) ::  this
          ! Locals
          integer(I32P) :: ny
          ! Exec code ...
          ny = this%m_y
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nz
!DIR$ ENDIF 
    pure function get_nz(this) result(nz)
          class(Chebyshev_psingle_t),   intent(in) :: this
          ! Locals
          integer(I32P) :: nz
          ! Exec code ....
          nz = this%m_nz
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_maxp
!DIR$ ENDIF 
    pure function get_maxp(this) result(maxp)
          class(Chebyshev_psingle_t),  intent(in) :: this
          ! Locals
          integer(I32P) :: maxp
          ! Exec code ...
          maxp = this%m_maxp
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_nt
!DIR$ ENDIF 
    pure function get_nt(this) result(nt)
          class(Chebyshev_psingle_t),   intent(in) :: this
          ! Locals
          integer(I32P) :: nt
          ! Exec code ...
          nt = this%m_nt
    end function
    
!DIR$ IF (USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_sdomain
!DIR$ ENDIF 
    pure function get_sdomain(this) result(sdomain)
          class(Chebyshev_psingle_t),  intent(in) :: this
          ! Locals
          real(R64P), dimension(3,8) :: sdomain
          ! Exec code ....
          sdomain = this%m_scanning_domain
    end function
    
     !==================================================
    !       Read write procedures
    !==================================================
    subroutine read_state(this,form,unit,ioerr)
        
          class(Chebyshev_psingle_t),   intent(in)    :: this
          character(len=*),             intent(in)    :: form
          integer(I32P),                intent(in)    :: unit
          integer(I32P),                intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine read_state
    
    subroutine write_state(this,form,unit,ioerr)
         
          class(Chebyshev_psingle_t),   intent(in)    :: this
          character(len=*),             intent(in)    :: form
          integer(I32P),                intent(in)    :: unit
          integer(I32P),                intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine write_state
    
    subroutine dbg_info(this,show_arrays)
          class(Chebyshev_psingle_t),  intent(in) :: this
          logical(I32P),               intent(in) :: show_arrays
          ! Exec code ....
          print*,   "============================================================================"
          print*,   "               Dumping: Chebyshev_psingle_t object state                    "
          print*,   "============================================================================"
          print*,   "               Chebyshev_psingle_t: -- memory layout                        "
          print*,   "============================================================================"
          write(stdout,10)  LOC(this)
10        format( 'addressof(this):                    --',Z15)
          write(stdout,20)  LOC(this%m_nx)
20        format( 'addressof(this%m_nx):               --',Z15)
          write(stdout,30)  LOC(this%m_ny)
30        format( 'addressof(this%m_ny):               --',Z15)
          write(stdout,40)  LOC(this%m_nz)
40        format( 'addressof(this%m_nz):               --',Z15)
          write(stdout,50)  LOC(this%m_maxp)
50        format( 'addressof(this%m_maxp):             --',Z15)
          write(stdout,60)  LOC(this%m_nt)
60        format( 'addressof(this%m_nt):               --',Z15)
          write(stdout,70)  LOC(this%m_particles_type)
70        format( 'addressof(this%m_particles_type):   --',Z15)
          write(stdout,80)  LOC(this%m_scan_domain)
80        format( 'addressof(this%m_scan_domain):      --',Z15)
          write(stdout,90)  LOC(this%m_xsection)
90        format( 'addressof(this%m_xsection):         --',Z15)
          write(stdout,100) LOC(this%m_radii)
100       format( 'addressof(this%m_radii):            --',Z15)
          write(stdout,110) LOC(this%m_rdist)
110       format( 'addressof(this%m_rdist):            --',Z15)
          write(stdout,120) LOC(this%m_theta)
120       format( 'addressof(this%m_theta):            --',Z15)
          write(stdout,130) LOC(this%m_phi)
130       format( 'addressof(this%m_phi):              --',Z15)
          write(stdout,140) LOC(this%m_volume)
140       format( 'addressof(this%m_volume):           --',Z15)
          write(stdout,150) LOC(this%m_surface)
150       format( 'addressof(this%m_surface):          --',Z15)
          write(stdout,160) LOC(this%m_paramx)
160       format( 'addressof(this%m_paramx):           --',Z15)
          write(stdout,170) LOC(this%m_paramy)
170       format( 'addressof(this%m_paramy):           --',Z15)
          write(stdout,180) LOC(this%m_paramz)
180       format( 'addressof(this%m_paramz):           --',Z15)
          write(stdout,190) LOC(this%m_vfall)
190       format( 'addressof(this%m_vfall):            --',Z15)
          write(stdout,200) LOC(this%m_stemp)
200       format( 'addressof(this%m_stemp):            --',Z15)
          write(stdout,210) LOC(this%m_mass)
210       format( 'addressof(this%m_mass):             --',Z15)
          write(stdout,220) LOC(this%m_mixratio)
220       format( 'addressof(this%m_mixratio):         --',Z15)
          print*, "=========================================================="
          print*, "     Dumping scalar and array member values               "
          print*, "=========================================================="
          print*, " m_nx:          ", this%m_nx,          &
                  " m_ny:          ", this%m_ny,          &
                  " m_nz:          ", this%m_nz,          &
                  " m_maxp:        ", this%m_maxp,        &
                  " m_nt:          ", this%m_nt,          &
                  " m_scan_domain: ", this%m_scan_domain
          if(show_arrays == .true. )  then
                   print*, "  Caller decided to print array members content.            "
                   print*, "============================================================"
                   print*, " m_particles_type:  ", this%m_particles_type
                   print*, " m_xsection:        ", this%m_xsection
                   print*, " m_radii:           ", this%m_radii
                   print*, " m_rdist:           ", this%m_rdist
                   print*, " m_theta:           ", this%m_theta
                   print*, " m_phi:             ", this%m_phi
                   print*, " m_volume:          ", this%m_volume
                   print*, " m_surface:         ", this%m_surface
                   print*, " m_paramx:          ", this%m_paramx
                   print*, " m_paramy:          ", this%m_paramy
                   print*, " m_paramz:          ", this%m_paramz
                   print*, " m_vfall:           ", this%m_vfall
                   print*, " m_stemp:           ", this%m_stemp
                   print*, " m_mass:            ", this%m_mass
                   print*, " m_mixratio:        ", this%m_mixratio
                   print*, "==========================================================="
          else
                   print*, "    Caller decided to skip over array content printing     "
          end if
          print*, "===================================================================="
          print*, "         End of: -- Chebyshev_psingle_t object state dump           "
          print*, "===================================================================="
    end subroutine
    
    !======================================================60
    !  subroutine: copy_assign
    !              Overloaded assignment (=)
    !======================================================60
    subroutine copy_assign(this,other)
          class(Chebyshev_psingle_t),   intent(inout) :: this
          class(Chebyshev_psingle_t),   intent(in)    :: other
          ! Exec code ....
          this%m_nx             = other%m_nx
          this%m_ny             = other%m_ny
          this%m_nz             = other%m_nz
          this%m_maxp           = other%m_maxp
          this%m_nt             = other%m_nt
          this%m_particles_type = other%m_particles_type
          this%m_scan_domain    = other%m_scan_domain
          this%m_xsection       = other%m_xsection
          this%m_radii          = other%m_radii
          this%m_rdist          = other%m_rdist
          this%m_theta          = other%m_theta
          this%m_phi            = other%m_phi
          this%m_volume         = other%m_volume
          this%m_surface        = other%m_surface
          this%m_paramx         = other%m_paramx
          this%m_paramy         = other%m_paramy
          this%m_paramz         = other%m_paramz
          this%m_vfall          = other%m_vfall
          this%m_stemp          = other%m_stemp
          this%m_mass           = other%m_mass
          this%m_mixratio       = other%m_mixratio
    end subroutine

    !===================================================================
    !                   Computational procedures 
    !  These procedures are not type bound in order to facilitate potential
    !  OpenMP parallelization (to be done later).
    !  Only Chebyshev Particles of type T2 and T4 should be used, at
    !  least in accordance to Wycombe paper.
    !===================================================================
    
    !===============================================================================
    ! Compute particles shape as a cross-section
    ! This code relies on compuation of radial distance as function of un-perturbed
    ! sphere radius, deformity coefficient and T(n) curve.
    !===============================================================================
    subroutine cps_compute_xsection(xsection,maxshv,mxp,chebn,sphrad,cdeform,inith,incth,  &
                                     verbose, ierr, fp_flags                                         )
!DIR$   IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$   ENDIF
          use mod_print_error,  only : print_non_fatal_error
          use mod_constants,    only : LAM_dtcoeff
          real(R64P),       dimension(maxshv,mxp),      intent(inout) :: xsection
          integer(I32P),                                intent(in)    :: maxshv
          integer(I32P),                                intent(in)    :: mxp
          real(R64P),       dimension(mxp),             intent(in)    :: chebn
          real(R64P),       dimension(mxp),             intent(in)    :: sphrad
          real(R64P),       dimension(mxp),             intent(in)    :: cdeform
          real(R64P),                                   intent(in)    :: inith
          real(R64P),                                   intent(in)    :: incth
          logical(I32P),                                intent(in)    :: verbose
          integer(I32P),                                intent(inout) :: ierr
          logical(I32P),    dimension(5),               intent(inout) :: fp_flags
          ! Locals
          integer(I32P)     :: j,i
          real(R64P)        :: term,theta
          character(len=40) :: sdate,stime
!DIR$     IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_value) :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity checking
          if(ierr < 0_I32P) ierr = 0_I32P
          if(any(fp_flags)) then
              fp_flags = .true.
          end if
          if( inith < 0._R64P  ) then
                  if(verbose == .true.)  then
                       call print_non_fatal_error( "=================== Non-Fatal Error =====================",  &
                          " Module name: -- [mod_cheb_particles_single],  subroutine name: [compute_cross_section]: -- Invalid Argument!!", &
                             sdate,stime,__LINE__     )
                  end if
                  ierr = -1_I32P
                  return
          end if
          do i = 1_I32P, size(LAM_dtcoeff)
              if(incth /= LAM_dtcoeff(i)) then
                  if(verbose == .true.) then
                         call print_non_fatal_error( "============================ Non-Fatal Error ==========================", &
                             " Module name: -- [mod_cheb_particles_single],  subroutine name: [compute_cross_section]: -- Invalid: 'incth' or 'incphi' value.", &
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
                             " Module name: -- [mod_cheb_particles_single],  subroutine name: [compute_cross_section]: -- Invalid: T(n) order ,or deform coeff.", &
                             sdate,stime,__LINE__)
                    end if
                    ierr = -3_I32P
                    return
             end if
          end do
          term = 0.0_R64P
          theta = inith
          ! Computational loop
          ! IEEE exception cheking if enabled is perfromed per each interation 
          ! of 'k' (outer) loop.
          do k = 1_I32P,    maxp
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                 call ieee_get_status(status_value)
                 call ieee_set_halting_mode(ieee_all,.false.)
                 call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
                 do i = 1_I32P, maxshv
                     theta = theta + incth
                     term = 1.0_R64P + cdeform(k) * cos(chebn(k) * theta)
                     xsection(i,k) = sphrad(k) * term
                 end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                call ieee_get_flag(ieee_all,fp_flags)
                if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " compute_cross_section: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags, "at iteration: ", k
                        print "====================================================================================================="
                end if
                call ieee_set_status(status_value)
!DIR$ ENDIF      
          end do
          ierr = 0_I32P
    end subroutine
    
     
    !==========================================================================80
    !           Computing Chebyshev particles volume per scanning domain.
    !           No argument verification is performed on array arguments like:
    !           sphrad,chebn and cdeform (these argument are verified by compute
    !                                     cross_section subroutine)
    !==========================================================================80
    subroutine cps_compute_volume(volume,maxp,sphrad,chebn,cdeform,fp_flags)
          use mod_contants,  only : LAM_PI
!DIR$  IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$  ENDIF
          real(R64P),       dimension(maxp),        intent(inout) :: volume
          integer(I32P),                            intent(in)    :: maxp
          real(R64P),       dimension(maxp),        intent(in)    :: sphrad
          real(R64P),       dimension(maxp),        intent(in)    :: chebn
          real(R64P),       dimension(maxp),        intent(in)    :: cdeform
          logical(I32P),    dimension(5),           intent(in)    :: fp_flags
          ! Locals
          integer(I32P) :: i
          real(R64P)    :: term1a,term1,term2,term3,term4
!DIR$     IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity checks
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          term1a = 0.0_R64P
          
          term1  = 0.0_R64P
          term2  = 0.0_R64P
          term3  = 0.0_R64P
          term4  = 0.0_R64P
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                 call ieee_get_status(status_value)
                 call ieee_set_halting_mode(ieee_all,.false.)
                 call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
                 do i = 1_I32P,  maxp
                        term1 =   0.3333333333333333333333333_R64P*4.0_R64P*LAM_PI*sphrad(i)**3
                        term1a =  1.0_R64P + 1.5_R64P * cdeform(i)**2 * &
                                  (4.0_R64P*chebn(i)**2-2.0_R64P/4.0_R64P*chebn(i)**2-1.0_R64P)
                        
                        if(iand(int(chebn(i),kind=4),1_I32P) == 0_I32P) then
                             term2 = 3.0_R64P * cdeform(i) * (1.0_R64P + cdeform(i)**2*0.25_R64P) / &
                                     (chebn(i)**2-1.0_R64P)
                             term3 = 0.25_R64P*cdeform(i)**3 / &
                                     (9.0_R64P*chebn(i)**2-1.0_R64P)
                             term4 = term1 * (term1a - term2 - term3)
                             volume(i) = term4
                        else
                            term2 = term1 * term1a
                            volume(i) = term2
                        end if
                        
                 end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print "             compute_volume: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                        print "====================================================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF                   
                        
    end subroutine
    
     
    !==========================================================================80
    !            Computes Chebyshev particles surface area   
    !            No argument verification is performed on array arguments like:
    !            sphrad,chebn and cdeform (these argument are verified by compute
    !                                     cross_section subroutine)
    !==========================================================================80
    subroutine cps_compute_surface(surface,maxp,sphrad,chebn,cdeform,fp_flags)
          use mod_constants,  only : LAM_PI    
!DIR$     IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$     ENDIF
          real(R64P),     dimension(maxp),   intent(inout) :: surface
          integer(I32P),                     intent(in)    :: maxp
          real(R64P),     dimension(maxp),   intent(in)    :: sphrad
          real(R64P),     dimension(maxp),   intent(in)    :: chebn
          real(R64P),     dimension(maxp),   intent(in)    :: cdeform
          logical(I32P),  dimension(5),      intent(inout) :: fp_flags
          ! Locals
          integer(I32P) :: i
          real(R64P)    :: term1,term2,term3,term4,term5,term5a
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
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
          do i = 1_I32P,  maxp
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
                    surface(i) = term1 * (term2 + term3 - term4 - term5 * term5a)
              else
                     term2 = 1.0_R64P + cdeform(k)**2*(chebn(i)**4+2.0_R64P*chebn(i)**2-1.0_R64P) / &
                                    (4.0_R64P*chebn(i)**2-1.0_R64P)
                     term3 = 3.0_R64P*cdeform(k)**4*chebn(i)**4*0.015625_R64P
                     term4 = 1.0_R64P + 20.0_R64P*chebn(i)**2-1.0_R64P / &
                                    ((16.0_R64P*chebn(i)**2-1.0_R64P)*(4.0_R64P*chebn(i)**2-1.0_R64P))
                     surface(i) = term1 * (term2 - term3 * term4)
              end if
              
          end do
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print "            cps_compute_surface: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                        print "====================================================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF             
    end subroutine
    
    !==========================================================================80
    !     Chebyshev Particles parametric equation in parameter 'x'
    !     x = r0[1+- eTn(cos(theta))]sin(theta) cos(phi)
    !==========================================================================80
    subroutine cps_compute_paramx(x_param,maxx,mxp,sphrad,cdeform,chebn,inith,    &
                                  incth,iniphi,incphi,verbose,ierr,fp_flags    )
!DIR$   IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$   ENDIF
          use mod_print_error, only : print_non_fatal_error
          use mod_constants,   only : LAM_dtcoeff    
          real(R64P),       dimension(maxx,mxp),     intent(inout) :: x_param
          integer(I32P),                             intent(in)    :: maxx
          integer(I32P),                             intent(in)    :: mxp
          real(R64P),       dimension(mxp),          intent(in)    :: sphrad
          real(R664P),      dimension(mxp),          intent(in)    :: cdeform
          real(R64P),       dimension(mxp),          intent(in)    :: chebn
          real(R64P),                                intent(in)    :: inith
          real(R64P),                                intent(in)    :: incth
          real(R64P),                                intent(in)    :: iniphi
          real(R64P),                                intent(in)    :: incphi
          logical(I32P),                             intent(in)    :: verbose
          integer(I32P),                             intent(inout) :: ierr
          logical(I32P),    dimension(5),            intent(inout) :: fp_flags
          ! Locals
          integer(I32P)     :: j,i
          real(R64P)        :: term,theta,phi,x
          character(len=40) :: sdate,stime
!DIR$   IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$   ENDIF
        ! Exec code ....
        ! Sanity checks 
          if(ierr < 0_I32P) ierr = 0_I32P
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          if( iniphi < 0.0_R64P .OR. iniphi > 4.0*Deg90toRad ) then
              if(verbose == .true. ) then
                  call print_non_fatal_error(  " =================== Non-Fatal Error ===================== " , &
                                " Module name: -- [mod_cheb_particles_single],  subroutine name: [cps_compute_paramx]: -- Invalid Argument!! " , &
                                 sdate,stime,__LINE__  )
              end if
              ierr = -1_I32P
              return
          end if
          do j = 1_I32P,  size(LAM_dtcoeff)
              if(incphi  /= LAM_dtcoeff(i)) then
                  if(verbose == .true. ) then
                      call print_non_fatal_error(  " =================== Non-Fatal Error ===================== ", &
                                " Module name: -- [mod_cheb_particles_single],  subroutine name: [cps_compute_paramx]: -- Invalid incth or incphi value!!", &
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
          do j = 1_I32P,    mxp
              ! Check ieee fp-exception register status per each iteration of 'j' loop
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
              do i = 1_I32P,  maxx
                  theta = theta + incth
                  phi   = phi   + incphi
                  term = sphrad(j) * (1.0_R64P + cdeform(j) * cos(chebn(j) * theta)
                  x    = term * sin(theta) * cos(phi)
                  x_param(i,j) = x
              end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                call ieee_get_flag(ieee_all,fp_flags)
                if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " cps_compute_paramx: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags, "at iteration: ", j
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
    subroutine cps_compute_paramy(y_param,maxy,mxp,sphrad,chebn,cdeform,inith,    &
                                  incth,iniphi,incphi,verbose,ierr,fp_flags    )
!DIR$  IF(USE_IEEE_EXCPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$  ENDIF    
          use mod_print_error, only : print_non_fatal_error
          use mod_constants,   only : LAM_dtcoeff
          real(R64P),   dimension(maxy,mxp),    intent(inout) :: y_param
          integer(I32P),                        intent(in)    :: maxy
          integer(I32P),                        intent(in)    :: mxp
          real(R64P),   dimension(mxp),         intent(in)    :: sphrad
          real(R64P),   dimension(mxp),         intent(in)    :: chebn
          real(R64P),   dimension(mxp),         intent(in)    :: cdeform
          real(R64P),                           intent(in)    :: inith
          real(R64P),                           intent(in)    :: incth
          real(R64P),                           intent(in)    :: iniphi
          real(R64P),                           intent(in)    :: incphi
          logical(I32P),                        intent(in)    :: verbose
          integer(I32P),                        intent(inout) :: ierr
          logical(I32P), dimension(5),          intent(inout) :: fp_flags
          ! Locals
          integer(I32P)     :: j,i
          real(R64P)        :: term,theta,phi,y
          character(len=40) :: sdate,stime
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$     ENDIF
          ! Exec code .....
          ! Sanity checks
          if(ierr < 0_I32P) ierr = 0_I32P
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          if( iniphi < 0.0_R64P .OR. iniphi > 4.0_R64P*Deg90toRad  ) then
              if(verbose == .true. ) then
                  call print_non_fatal_error(   " =================== Non-Fatal Error ===================== " , &
                                " Module name: -- [mod_cheb_particles_single],  subroutine name: [cps_compute_paramy]: -- Invalid Argument!! " , &
                                 sdate,stime,__LINE__  )
              end if
              ierr = -1_I32P
              return
          end if
          do i = 1_I32P, size(LAM_dtcoeff)
              if( incphi /= LAM_dtcoeff(i)) then
                  if(verbose == .true.) then
                      call print_non_fatal_error(  " =================== Non-Fatal Error ===================== ", &
                                " Module name: -- [mod_cheb_particles_single],  subroutine name: [cps_compute_paramy]: -- Invalid incth or incphi value!!", &
                                sdate,stime, __LINE__ )
                  end if
                  ierr = -2_I32P
                  return
              end if
          end do
          term = 0.0_R64P
          theta = inith
          phi   = iniphi
          y     = 0.0_R64P
          ! Begin a computation loop
          do j = 1_I32P,  mxp
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
                  y_param(i,j) = y
              end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                call ieee_get_flag(ieee_all,fp_flags)
                if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " cps_compute_paramy: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags, "at iteration: ", j
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
    subroutine cps_compute_paramz(z_param,maxz,mpx,sphrad,chebn,cdeform,inith,incth,  &
                                  ierr,fp_flags                 )
!DIR$  IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$  ENDIF
          use mod_print_error,  only : print_non_fatal_error
          use mod_constants,    only : LAM_dtcoeff
          real(R64P),       dimension(maxz,mpx),  intent(inout) :: z_param
          integer(I32P),                          intent(in)    :: maxz
          integer(I32P),                          intent(in)    :: mpx
          real(R64P),       dimension(mpx),       intent(in)    :: sphrad
          real(R64P),       dimension(mpx),       intent(in)    :: chebn
          real(R64P),       dimension(mpx),       intent(in)    :: cdeform
          real(R64P),                             intent(in)    :: inith
          real(R64P),                             intent(in)    :: incth
          integer(I32P),                          intent(inout) :: ierr
          logical(I32P),    dimension(5),         intent(inout) :: fp_flags
          
          ! Locals
          integer(I32P)     :: j,i
          real(R64P)        :: term,theta,z
         
!DIR$    IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)    :: status_value
!DIR$    ENDIF
          ! Exec code ....
          ! Sanity check
          if(ierr < 0_I32P) ierr = 0_I32P
          if(any(fp_flags)) then
              fp_flags = .false.
          end if
          term  = 0.0_R64P
          theta = inith
          z     = 0.0_R64P
          ! Begin computational loop
          do j = 1_I32P,  mxp
                  ! Check ieee fp-exception register status per each iteration of 'j' loop
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
              do i = 1_I32P,  maxz
                  theta = theta + incth
                  term = sphrad(j) * (1.0_R64P + cdeform(j) * cos(chebn(j) * theta))
                  z    = term * cos(theta)
                  z_param(i,j) = z
              end do
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
                call ieee_get_flag(ieee_all,fp_flags)
                if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " cps_compute_paramy: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags, "at iteration: ", j
                        print "====================================================================================================="
                end if
                call ieee_set_status(status_value)
!DIR$ ENDIF
          end do
          ierr = 0_I32P
    end subroutine
    
    !============================================================================
    !      Vertical falling speed computation based on
    !     "Fall Velocities of Hydrometeors in the Atmosphere: 
    !      Refinements to Continous Analytical Power Law    "
    !      by Vitaliy I. Khvorostyanov and Judith A. Curry
    !      General form version 1.
    !============================================================================
     subroutine cps_compute_vfall_v1(Vt,nt,np,Cd,mass,vb,A,rho_b,rho_f,nx,ny,nz,fp_flags)
!DIR$  IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
       use, intrinsic :: ieee_exceptions
!DIR$  ENDIF
       real(R64P),     dimension(nt,np),    intent(inout) :: Vt
       integer(I32P),                       intent(in)    :: nt
       integer(I32P),                       intent(in)    :: np
       real(R64P),     dimension(nt,np),    intent(in)    :: Cd
       real(R64P),     dimension(np),       intent(in)    :: mass
       real(R64P),     dimension(np),       intent(in)    :: vb
       real(R64P),     dimension(np),       intent(in)    :: A
       real(R64P),     dimension(np),       intent(in)    :: rho_b
       real(R64P),     dimension(nx,ny,nz), intent(in)    :: rho_f
       integer(I32P),                       intent(in)    :: nx,ny,nz
       logical(I32P),  dimension(5),        intent(inout) :: fp_flags
       ! Locals
       integer(I32P) :: j,i,ix,iy,iz
       real(R64P)    :: term1,term2
!DIR$  IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
       type(ieee_status_type) :: status_value
!DIR$ ENDIF
       ! Exec code ....
       ! Sanity check
       if(any(fp_flags)) then
           fp_flags = .false.
       end if
       term1 = 0.0_R64P
       term2 = 0.0_R64P
       ! Begin computational loop for general case of fall speed
       do j = 1_I32P,  np
              ! Obtain ieee floating-point register state per each 'j' iteration
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
             
              
              do i = 1_I32P,  nt
                  
                do iz = 1_I32P,  nx
                  do iy = 1_I32P, ny
                      do ix = 1_I32P, nz
                            term1 = 2.0_R64P * 9.81_R64P * vb(np) / &
                                    (Cd(i,j) * A(j))
                            if(rho_b(j) > rho_f(x,y,z)  then
                                  term2 = abs(rho_b(j) / rho_f(ix,iy,iz)
                            else
                                  term2 = abs(rho_b(j) / rho_f(ix,iy,iz) - 1.0_R64P)
                            end if
                            Vt(i,j) = sqrt(term1*term2)
                      end do
                  end do
              end do
                   
                    
                    
              end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " cps_compute_vfall_v1: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags, "at iteration: ", j
                        print "====================================================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF         
       end do
       
    end subroutine
    
    !============================================================================
    !      Vertical falling speed computation based on
    !     "Fall Velocities of Hydrometeors in the Atmosphere: 
    !      Refinements to Continous Analytical Power Law    "
    !      by Vitaliy I. Khvorostyanov and Judith A. Curry
    !      Modified form version 2.
    !============================================================================
    subroutine cps_compute_vfall_v2(Vt,nt,np,aRe,bRe,vb,kvisc,nx,ny,nz,A,rho_b,  &
                                     rho_f,mD, fp_flags          )
 !DIR$  IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
       use, intrinsic :: ieee_exceptions
!DIR$  ENDIF
       real(R64P),      dimension(nt,np),    intent(inout) :: Vt
       integer(I32P),                        intent(in)    :: nt
       integer(I32P),                        intent(in)    :: np
       real(R64P),      dimension(nt,np),    intent(in)    :: aRe
       real(R64P),      dimension(nt,np),    intent(in)    :: bRe
       real(R64P),      dimension(np),       intent(in)    :: vb
       real(R64P),      dimension(nx,ny,nz), intent(in)    :: kvisc
       integer(I32P),                        intent(in)    :: nx
       integer(I32P),                        intent(in)    :: ny
       integer(I32P),                        intent(in)    :: nz
       real(R64P),      dimension(np),       intent(in)    :: A
       real(R64P),      dimension(np),       intent(in)    :: rho_b
       real(R64P),      dimension(nx,ny,nz), intent(in)    :: rho_f
       real(R64P),      dimension(np),       intent(in)    :: mD
       logical(I32P),   dimension(5),        intent(inout) :: fp_flags
       ! Locals
       integer(I32P) :: j,i,ix,iy,iz
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
      
       ! Begin computational loop
       do j = 1_I32P,   np
            !    ! Obtain ieee floating-point register state per each 'j' iteration
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
              do i = 1_I32P,  nt
                  
                  do iz = 1_I32P,   nz
                      do iy = 1_I32P,  ny
                          do ix = 1_I32P,  nz
                              
                              term1 = aRe(i,j) * kvisc(ix,iy,iz)**1.0_R64P-2.0_R64P*aRe(i,j)
                              term2 = 2.0_R64P*vb(j)*9.81_R64P / A(j)
                              if( rho_b(j) > rho_f(ix,iy,iz) ) then
                                    term2a = abs(rho_b(j) / rho_f(ix,iy,iz) )
                              else
                                    term2a = abs(rho_b(j) / rho_f(ix,iy,iz) - 1.0_R64P )
                              end if
                              term3 = mD(j)**2.0_R64P*bRe(i,j) - 1.0_R64P
                              Vt(i,j) = term1 * (term2 * term2a)**bRe(i,j) * term3
                         
                          end do
                      end do
                  end do
                  
              end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                        print "====================================================================================================="
                        print " cps_compute_vfall_v2: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags, "at iteration: ", j
                        print "====================================================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF      
       end do
       
    end subroutine
    
    
    
end module mod_cheb_particles_single