
#include "Config.fpp"

module mod_kmeans

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_kmeans'
 !          
 !          Purpose:
 !                    This module is based on John Burkardt 'KMEANS' implementation
 !          History:
 !                        Date: 12-05-2018
 !                        Time: 16:06 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                  Bernard Gingold
 !                 
 !          References:
 !         
 !                John Burkardt webpage
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    
    use module_kinds, only : I32P, R64P
    implicit none
    private
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_KMEANS_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_KMEANS_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_KMEANS_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_KMEANS_FULLVER = 1000_I32P*MOD_KMEANS_MAJOR + &
                                                             100_I32P*MOD_KMEANS_MINOR  + &
                                                             10_I32P*MOD_KMEANS_MICRO
    ! Module creation date
    character(*),  parameter, public :: MOD_KMEANS_CREATE_DATE = "12-05-2018 16:19 +00200 (SAT 12 MAY 2018 GMT+2)"
    
    ! Module build date ( should be set after successful compilation date/time)
    character(*),  parameter, public :: MOD_KMEANS_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_KMEANS_AUTHOR = "Original author: John Burkardt, modified by: Bernard Gingold, e-mail: beniekg@gmail.com "
    
    ! Module short description
    character(*),  parameter, public :: MOD_KMEANS_DESCRIPT = "Fortran 2003 version of John Burkardt 'KMEANS' library."
    
    !================================
    ! type: Kmeans_t
    !================================
    type, public :: Kmeans_t
        
          private
          !  the number of spatial dimensions.
          integer(I32P) :: m_dim_num
          !  the number of signal samples (re,im) components
          integer(I32P) :: m_samples_num
          ! the number of clusters.
          integer(I32P) :: m_cluster_num
          !  the maximum number of iterations.
          integer(I32P) :: m_iter_max
          !  the number of iterations taken.
          integer(I32P) :: m_iter_num
          !  a seed for the random number generator.
          integer(I32P) :: m_seed
          
          !     On input, the user
		  !		may specify an initial cluster for each point, or leave all entrie of
		  !		CLUSTER set to 0.  On output, CLUSTER contains the index of the
		  !		cluster to which each data point belongs.
          
          integer(I32P), allocatable, dimension(:),   public :: m_cluster
          !   the coordinates of the cluster centers.
          real(R64P),    allocatable, dimension(:,:), public :: m_cluster_center
          !   the number of
          !	  points assigned to each cluster.
          integer(I32P), allocatable, dimension(:),   public :: m_cluster_population
          !   the energy of   the clusters.
		  real(R64P),    allocatable, dimension(:),   public :: m_cluster_energy
          
          contains
    
          !========================================
          !    Getter procedures
          !========================================
    
          procedure, pass(this), public :: get_dim_num
          
          procedure, pass(this), public :: get_samples_num
          
          procedure, pass(this), public :: get_cluster_num
          
          procedure, pass(this), public :: get_iter_max
          
          procedure, pass(this), public :: get_iter_num
          
          procedure, pass(this), public :: get_seed
          
          !========================================
          !    Read/write procedures
          !========================================
          
          procedure, nopass, public :: write_state
          
          procedure, nopass, public :: read_state
          
          !=======================================
          !  Class helper procedures
          !=======================================
          
          procedure, pass(this), public ::  dbg_info
          
          !============================================
          !  Computational procedures
          !============================================
          
          procedure, pass(this), public :: compute_hmeans01
          
          procedure, pass(this), public :: compute_hmeans02
          
          procedure, pass(this), public :: compute_kmeans01
          
          procedure, pass(this), public :: compute_kmeans02
          
          procedure, pass(this), public :: compute_kmeans03
          
          procedure, pass(this), public :: compute_hmeans01_kmeans01
          
          procedure, pass(this), public :: compute_hmeans01_kmeans02
          
          procedure, pass(this), public :: compute_hmeans01_kmeans03
          
        
            
    end type Kmeans_t
          
          interface Kmeans_t
            
            procedure :: constructor
            
          end interface Kmeans_t
          
    contains
    
    type(Kmeans_t) function constructor(dim_num,samples_num,cluster_num,iter_max,seed, &
                                        logging,verbose,fname,append  )
          use mod_print_error, only : handle_fatal_memory_error
          use mod_constants,   only : LAM_PINF
          integer(I32P),    intent(in) :: dim_num,samples_num,  &
                                          cluster_num,iter_max, &
                                          seed
          logical(I32P),    intent(in) :: logging,verbose
          character(len=*), intent(in) :: fname
          logical(I32P),    intent(in) :: append
          ! Locals
          character(len=256) :: emsg
          integer(I32P)      :: aerr
          ! Executable statemetns
          constructor%m_dim_num     = dim_num
          constructor%m_samples_num = samples_num
          constructor%m_cluster_num = cluster_num
          constructor%m_iter_max    = iter_max
          constructor%m_seedd       = seed
          associate(sn=>constructor%m_samples_num,   &
                    dn=>constructor%m_dim_num,       &
                    cn=>constructor%m_cluster_num )
                allocate( constructor%m_cluster(sn),            &
                          constructor%m_cluster_center(dn,cn),  &
                          constructor%m_cluster_population(cn), &
                          constructor%m_cluster_energy(cn),     &
                          STAT=aerr,                            &
                          ERRMSG=emsg   )
         end associate
         if(aerr /= 0) then
             call handle_fatal_memory_error(logging,verbose,append,fname, &
                                            "logger:205 --> mod_kmeans/constructor: -- Memory Allocation Failure !!", &
                                            "mod_kmeans/constructor:205 -- Memory Allocation Failure !!", &
                                            emsg,__LINE__ )
         end if
         constructor%m_cluster            = 0
         constructor%m_cluster_center     = LAM_PINF
         constructor%m_cluster_population = 0
         constructor%m_cluster_energy     = LAM_PINF
    end function
    
    !==========================================
    !       Getter procedures
    !==========================================
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_dim_num
!DIR$ ENDIF
    pure function get_dim_num(this) result(dim_num)
          class(Kmeans_t),  intent(in) :: this
          integer(I32P) :: dim_num
          dim_num = this%m_dim_num
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_samples_num
!DIR$ ENDIF
    pure function get_samples_num(this) result(samples_num)
          class(Kmeans_t),  intent(in) :: this
          integer(I32P) :: samples_num
          samples_num = this%m_samples_num
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_cluster_num
!DIR$ ENDIF
    pure function get_cluster_num(this) result(cluster_num)
          class(Kmeans_t), intent(in) :: this
          integer(I32P) :: cluster_num
          cluster_num = this%m_cluster_num
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_iter_max
!DIR$ ENDIF
    pure function get_iter_max(this) result(iter_max)
          class(Kmeans_t), intent(in) :: this
          integer(I32P) :: iter_max
          iter_max = this%m_iter_max
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_iter_num
!DIR$ ENDIF
    pure function get_iter_num(this) result(iter_num)
          class(Kmeans_t), intent(in) :: this
          integer(I32P) :: iter_num
          iter_num = this%m_iter_num
    end function
    
!DIR$ IF(USE_INLINING .EQ. 1)
    !DIR$ ATTRIBUTES INLINE :: get_seed
!DIR$ ENDIF
    pure function get_seed(this) result(seed)
          class(Kmeans_t), intent(in) :: this
          integer(I32P) :: seed
          seed = this%m_seed
    end function
    
    !============================================
    !   Read/write procedures
    !============================================
    
     subroutine read_state(this,form,unit,ioerr)
          !implicit none
          class(Kmeans_t),         intent(in)    :: this
          character(len=*),        intent(in)    :: form
          integer(I32P),           intent(in)    :: unit
          integer(I32P),           intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine read_state
    
    subroutine write_state(this,form,unit,ioerr)
         ! implicit none
          class(Kmeans_t),         intent(in)    :: this
          character(len=*),        intent(in)    :: form
          integer(I32P),           intent(in)    :: unit
          integer(I32P),           intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
    subroutine dbg_info(this)
          class(Kmeans_t), intent(in) :: this
          ! Exec code
          print*, "====================================================="
          print*, "         Dump of Kmeans_t object state               "
          print*, "====================================================="
          print*, " Collected on: ",__DATE__,":",__TIME__
          print*, "====================================================="
          print*, " m_dim_num:            ", this%m_dim_num
          print*, " m_samples_num:        ", this%m_samples_num
          print*, " m_cluster_num:        ", this%m_cluster_num
          print*, " m_iter_max:           ", this%m_iter_max
          print*, " m_iter_num:           ", this%m_iter_num
          print*, " m_seed:               ", this%m_seed
          print*, " m_cluster:            ", this%m_cluster
          print*, " m_cluster_center:     ", this%m_cluster_center
          print*, " m_cluster_population: ", this%m_cluster_population
          print*, " m_cluster_energy:     ", this%m_cluster_energy
          print*, "======================================================="
    end subroutine
    
    !
    subroutine compute_hmeans01(this,sig_samples,cluster_variance,fp_flags, &
                                samples_num,dim_num,cluster_num,verbose         )
          use kmeans_lib, only : cluster_initialize_5,     &
                                 hmeans_01,                &
                                 cluster_variance_compute, &
                                 cluster_print_summary
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          use ifcore
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          class(Kmeans_t),  intent(inout) :: this
          real(R64P), dimension(dim_num,samples_num), intent(inout) :: sig_samples
          !DIR$ ASSUME_ALIGNED sig_samples:64
          real(R64P), dimension(cluster_num),         intent(inout) :: cluster_variance
          !DIR$ ASSUME_ALIGNED cluster_variance:64
          logical(I32P), dimension(5),                intent(inout) :: fp_flags
          integer(I32P),                              intent(in)    :: samples_num, &
                                                                       dim_num,     &
                                                                       cluster_num
          logical(I32P),                              intent(in)    :: verbose
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
          
!DIR$ ENDIF
          ! Exec code
          if(ANY(fp_flags) == .true.) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,.false.)
!DIR$ ENDIF   
          call cluster_initialize_5(this%m_dim_num,this%m_samples_num, &
                                    this%m_cluster_num,sig_samples,    &
                                    this%m_seed,this%m_cluster_center  )
          
          call hmeans_01(this%m_dim_num,this%m_samples_num,this%m_cluster_num, &
                         this%m_iter_max,this%m_iter_num,sig_samples,          &
                         this%m_cluster,this%m_cluster_center,                 &
                         this%m_cluster_population,this%m_cluster_energy  )
          
          call cluster_variance_compute(this%m_dim_num,this%m_samples_num,     &
                                        this%m_cluster_num,sig_samples,        &
                                        this%m_cluster,this%m_cluster_center,  &
                                        cluster_variance                    )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                write(ERROR_UNIT,*) "====================================================================="
                write(ERROR_UNIT,*) " Kmeans_t%compute_hmeans01: FLOATING-POINT EXECEPTION(S) OCCURRED"
                write(ERROR_UNIT,*) "====================================================================="
          end if    
          call ieee_set_status(status_value)
!DIR$ ENDIF           
          if(verbose == .true.) then
              call cluster_print_summary(this%m_samples_num,this%m_cluster_num,  &
                                this%m_cluster_population,this%m_cluster_energy, &
                                cluster_variance                                )
          end if
    end subroutine  
          
    subroutine compute_hmeans02(this,sig_samples,cluster_variance,fp_flags,    &
                                samples_num,dim_num,cluster_num,verbose     )
          use kmeans_lib, only : cluster_initialize_5,      &
                                 hmeans_02,                 &
                                 cluster_variance_compute,  &
                                 cluster_print_summary
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          use ifcore,
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          class(Kmeans_t),  intent(inout) :: this
          real(R64P), dimension(dim_num,samples_num), intent(inout) :: sig_samples
          !DIR$ ASSUME_ALIGNED sig_samples:64
          real(R64P), dimension(cluster_num),         intent(out)   :: cluster_variance
          !DIR$ ASSUME_ALIGNED cluster_variance:64
          logical(I32P), dimension(5),                intent(inout) :: fp_flags
          integer(I32P),                              intent(in)    :: samples_num, &
                                                                       dim_num,     &
                                                                       cluster_num
          logical(I32P),                              intent(in)    :: verbose
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
          
!DIR$ ENDIF
          ! Exec code
          if(ANY(fp_flags) == .true.) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,.false.)
!DIR$ ENDIF
          call cluster_initialize_5(this%m_dim_num,this%m_samples_num,this%m_cluster_num, &
                                    sig_samples,this%m_seed,this%m_cluster_center   )
          
          call hmeans_02(this%m_dim_num,this%m_samples_num,this%m_cluster_num,this%m_iter_max, &
                         this%m_iter_num,sig_samples,this%m_cluster,this%m_cluster_center,     &
                         this%m_cluster_population,this%m_cluster_energy,this%m_seed        )
          
          call cluster_variance_compute(this%m_dim_num,this%m_samples_num,this%m_cluster_num, &
                                        sig_samples,this%m_cluster_center,cluster_variance  )
          
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                write(ERROR_UNIT,*) "====================================================================="
                write(ERROR_UNIT,*) " Kmeans_t%compute_hmeans02: FLOATING-POINT EXECEPTION(S) OCCURRED"
                write(ERROR_UNIT,*) "====================================================================="
          end if    
          call ieee_set_status(status_value)
!DIR$ ENDIF  
          if(verbose == .true.) then
              call cluster_print_summary(this%m_samples_num,this%m_cluster_num,   &
                               this%m_cluster_population,this%m_cluster_energy,   &
                               cluster_variance                     )
         end if
          
    end subroutine
    
    subroutine compute_kmeans01(this,sig_samples,cluster_variance,fp_flags, &
                                samples_num,dim_num,cluster_num,verbose  )
          use kmeans_lib, only : cluster_initialize_5, &
                                 kmeans_01,            &
                                 cluster_variance_compute, &
                                 cluster_print_summary
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          use ifcore,
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          class(Kmeans_t),      intent(inout) :: this
          real(R64P), dimension(dim_num,samples_num), intent(inout) :: sig_samples
          !DIR$ ASSUME_ALIGNED sig_samples:64
          real(R64P), dimension(cluster_num),         intent(out)   :: cluster_variance
          !DIR$ ASSUME_ALIGNED cluster_variance:64
          logical(I32P), dimension(5),                intent(inout) :: fp_flags
          integer(I32P),                              intent(in)    :: samples_num, &
                                                                       dim_num,     &
                                                                       cluster_num
          logical(I32P),                              intent(in)    :: verbose
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
          
!DIR$ ENDIF
          ! Exec code
          if(ANY(fp_flags) == .true) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,.false.)
!DIR$ ENDIF
          call cluster_initialize_5(this%m_dim_num,this%m_samples_num,this%m_cluster_num, &
                                    sig_samples,this%m_seed,this%m_cluster_center   )
          
          call kmeans_01(this%m_dim_num,this%m_samples_num,this%m_cluster_num,  &
                         this%m_iter_max,this%m_iter_num,sig_samples,           &
                         this%m_cluster,this%m_cluster_center,                  &
                         this%m_cluster_population,this%m_cluster_energy   )
          
          call cluster_variance_compute(this%m_dim_num,this%m_samples_num,this%m_cluster_num, &
                                        sig_samples,this%m_cluster,this%m_cluster_center,     &
                                        cluster_variance                                )
          
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                write(ERROR_UNIT,*) "====================================================================="
                write(ERROR_UNIT,*) " Kmeans_t%compute_kmeans01: FLOATING-POINT EXECEPTION(S) OCCURRED"
                write(ERROR_UNIT,*) "====================================================================="
          end if    
          call ieee_set_status(status_value)
!DIR$ ENDIF 
          if(verbose == .true.) then
              call cluster_print_summary(this%m_samples_num,this%m_cluster_num,   &
                                    this%m_cluster_population,this%m_cluster_energy, &
                                    cluster_variance                                )
          end if
          
    end subroutine
    
    subroutine compute_kmeans02(this,sig_samples,cluster_variance,fp_flags,  &
                                samples_num,dim_num,cluster_num,verbose   )
          use kmeans_lib, only : cluster_initialize_1, &
                                 kmeans_02,            &
                                 cluster_variance_compute, &
                                 cluster_print_summary
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          use ifcore,
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          class(Kmeans_t),                            intent(inout) :: this
          real(R64P), dimension(dim_num,samples_num), intent(inout) :: sig_samples
          !DIR$ ASSUME_ALIGNED sig_samples:64
          real(R64P), dimension(cluster_num),         intent(out)   :: cluster_variance
          !DIR$ ASSUME_ALIGNED cluster_variance:64
          integer(I32P),                              intent(in)    :: samples_num, &
                                                                       dim_num,     &
                                                                       cluster_num
          logical(I32P),                              intent(in)    :: verbose
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
          
!DIR$ ENDIF
          ! Exec code
          if(ANY(fp_flags) == .true.) then
              fp_flags = .false. 
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,.false.)
!DIR$ ENDIF
          call cluster_initialize_1(this%m_dim_num,this%m_samples_num,this%m_cluster_num, &
                                    sig_samples,this%m_cluster_center           )
          
          call kmeans_02(this%m_dim_num,this%m_samples_num,this%m_cluster_num,  &
                         this%m_iter_max,this%m_iter_max,sig_samples,this%m_cluster, &
                         this%m_cluster_center,this%m_cluster_population,this%m_cluster_energy )
          
          call cluster_variance_compute(this%m_dim_num,this%m_samples_num,this%m_cluster_num, &
                                        sig_samples,this%m_cluster,this%m_cluster_center,cluster_variance     )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                write(ERROR_UNIT,*) "====================================================================="
                write(ERROR_UNIT,*) " Kmeans_t%compute_kmeans02: FLOATING-POINT EXECEPTION(S) OCCURRED"
                write(ERROR_UNIT,*) "====================================================================="
          end if    
          call ieee_set_status(status_value)
!DIR$ ENDIF    
          if(verbose == .true.) then
              call cluster_print_summary(this%m_samples_num,this%m_cluster_num,   &
                                this%m_cluster_population,this%m_cluster_energy,  &
                                cluster_variance                            )
          end if
          
    end subroutine
    
    subroutine compute_kmeans03(this,sig_samples,cluster_variance,fp_flags,  &
                                samples_num,dim_num,cluster_num,verbose  )
          use kmeans_lib, only : cluster_initialize_1, &
                                 kmeans_03,            &
                                 cluster_variance_compute, &
                                 cluster_print_summary
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          use ifcore,
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          class(Kmeans_t),      intent(inout) :: this
          real(R64P), dimension(dim_num,samples_num), intent(inout) :: sig_samples
          !DIR$ ASSUME_ALIGNED sig_samples:64
          real(R64P), dimension(cluster_num),         intent(out)   :: cluster_variance
          !DIR$ ASSUME_ALIGNED cluster_variance:64
          logical(I32P), dimension(5),                intent(inout) :: fp_flags
          integer(I32P),                              intent(in)    :: samples_num, &
                                                                       dim_num,     &
                                                                       cluster_num
          logical(I32P),                              intent(in)    :: verbose
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
          
!DIR$ ENDIF
          ! Exec code
          if(ANY(fp_flags) == .true.) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,.false.)
!DIR$ ENDIF
          call cluster_initialize_1(this%m_dim_num,         &
                                    this%m_samples_num,     &
                                    this%m_cluster_num,     &
                                    sig_samples,            &
                                    this%m_cluster_center   )
          
          call kmeans_03(this%m_dim_num,        &
                         this%m_samples_num,    &
                         this%m_cluster_num,    &
                         this%m_iter_max,       &
                         this%m_iter_num,       &
                         sig_samples,           &
                         this%m_cluster,        &
                         this%m_cluster_center, &
                         this%m_cluster_population, &
                         this%m_cluster_energy    )
          
          call cluster_variance_compute(this%m_dim_num,     &
                                        this%m_samples_num, &
                                        this%m_cluster_num, &
                                        sig_samples,        &
                                        this%m_cluster,     &
                                        cluster_variance  )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                write(ERROR_UNIT,*) "====================================================================="
                write(ERROR_UNIT,*) " Kmeans_t%compute_kmeans03: FLOATING-POINT EXECEPTION(S) OCCURRED"
                write(ERROR_UNIT,*) "====================================================================="
          end if    
          call ieee_set_status(status_value)
         
          
!DIR$ ENDIF
             if(verbose == .true.) then
              call cluster_print_summary(this%m_samples_num,  &
                                         this%m_cluster_num,  &
                                         this%m_cluster_population, &
                                         this%m_cluster_energy,     &
                                         cluster_variance           )  
             end if                           
              
    end subroutine
    
    subroutine compute_hmeans01_kmeans01(this,sig_samples,cluster_variance,fp_flags,  &
                                         samples_num,dim_num,cluster_num,verbose   )
          use kmeans_lib, only : cluster_initialize_5,     &
                                 hmeans_01,                &
                                 cluster_variance_compute, &
                                 cluster_print_summary,    &
                                 kmeans_01
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          use ifcore,
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          class(Kmeans_t),                              intent(inout) :: this
          real(R64P), dimension(dim_num,samples_num),   intent(inout) :: sig_samples
          !DIR$ ASSUME_ALIGNED sig_samples:64
          real(R64P), dimension(cluster_num),           intent(out)   :: cluster_variance
          !DIR$ ASSUME_ALIGNED cluster_variance:64
          logical(I32P), dimension(5),                  intent(inout) :: fp_flags
          integer(I32P),                                intent(in)    :: samples_num,dim_num, &
                                                                         cluster_num
          logical(I32P),                                intent(in)    :: verbose
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
          
!DIR$ ENDIF
          ! Exec code
          if(ANY(fp_flags) == .true.) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,.false.)
!DIR$ ENDIF
          call cluster_initialize_5(this%m_dim_num,         &
                                    this%m_samples_num,     &
                                    this%m_cluster_num,     &
                                    sig_samples,            &
                                    this%m_seed,            &
                                    this%m_cluster_center   )
          
          call hmeans_01( this%m_dim_num,         &
                          this%m_samples_num,     &
                          this%m_cluster_num,     &
                          this%m_iter_max,        &
                          this%m_iter_num,        &
                          sig_samples,            &
                          this%m_cluster,         &
                          this%m_cluster_center,  &
                          this%m_cluster_population, &
                          this%m_cluster_energy    )
          
          call cluster_variance_compute( this%m_dim_num,         &
                                         this%m_samples_num,     &
                                         this%m_cluster_num,     &
                                         sig_samples,            &
                                         this%m_cluster,         &
                                         this%m_cluster_center,  &
                                         cluster_variance        )
          
           call kmeans_01(this%m_dim_num,         &
                          this%m_samples_num,     &
                          this%m_cluster_num,     &
                          this%m_iter_max,        &
                          this%m_iter_num,        &
                          sig_samples,            &
                          this%m_cluster,         &
                          this%m_cluster_center,  &
                          this%m_cluster_population, &
                          this%m_cluster_energy    )
           
           call cluster_variance_compute(this%m_dim_num,         &
                                         this%m_samples_num,     &
                                         this%m_cluster_num,     &
                                         sig_samples,            &
                                         this%m_cluster,         &
                                         this%m_cluster_center,  &
                                         cluster_variance        )
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                write(ERROR_UNIT,*) "====================================================================="
                write(ERROR_UNIT,*) " Kmeans_t%compute_hmeans01_kmeans01: FLOATING-POINT EXECEPTION(S) OCCURRED"
                write(ERROR_UNIT,*) "====================================================================="
          end if    
          call ieee_set_status(status_value)
         
          
!DIR$ ENDIF  
          if(verbose == .true.) then
              call cluster_print_summary( this%m_samples_num,  &
                                         this%m_cluster_num,  &
                                         this%m_cluster_population, &
                                         this%m_cluster_energy,     &
                                         cluster_variance           ) 
          end if
           
    end subroutine
    
    subroutine  compute_hmeans01_kmeans02(this,sig_samples,cluster_variance,fp_flags, &
                                          samples_num,dim_num,cluster_num,verbose   )
          use kmeans_lib, only : cluster_initialize_5,  &
                                 hmeans_01,             &
                                 cluster_variance_compute, &
                                 kmeans_02 , &
                                 cluster_print_summary
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          use ifcore,
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF 
          class(Kmeans_t),                              intent(inout) :: this
          real(R64P), dimension(dim_num,samples_num),   intent(inout) :: sig_samples
          !DIR$ ASSUME_ALIGNED sig_samples:64
          real(R64P), dimension(cluster_num),           intent(out)   :: cluster_variance
          !DIR$ ASSUME_ALIGNED cluster_variance:64
          logical(I32P), dimension(5),                  intent(inout) :: fp_flags
          integer(I32P),                                intent(in)    :: samples_num,dim_num, &
                                                                         cluster_num
          logical(I32P),                                intent(in)    :: verbose
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
          
!DIR$ ENDIF
          ! Exec code
          if(ANY(fp_flags) == .true.) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,.false.)
!DIR$ ENDIF
          call cluster_initialize_5(this%m_dim_num,         &
                                    this%m_samples_num,     &
                                    this%m_cluster_num,     &
                                    sig_samples,            &
                                    this%m_seed,            &
                                    this%m_cluster_center   )
          
          call hmeans_01( this%m_dim_num,         &
                          this%m_samples_num,     &
                          this%m_cluster_num,     &
                          this%m_iter_max,        &
                          this%m_iter_num,        &
                          sig_samples,            &
                          this%m_cluster,         &
                          this%m_cluster_center,  &
                          this%m_cluster_population, &
                          this%m_cluster_energy    )
          
          call cluster_variance_compute( this%m_dim_num,         &
                                         this%m_samples_num,     &
                                         this%m_cluster_num,     &
                                         sig_samples,            &
                                         this%m_cluster,         &
                                         this%m_cluster_center,  &
                                         cluster_variance        )
          
           call kmeans_02(this%m_dim_num,         &
                          this%m_samples_num,     &
                          this%m_cluster_num,     &
                          this%m_iter_max,        &
                          this%m_iter_num,        &
                          sig_samples,            &
                          this%m_cluster,         &
                          this%m_cluster_center,  &
                          this%m_cluster_population, &
                          this%m_cluster_energy    )
           
           call cluster_variance_compute(this%m_dim_num,         &
                                         this%m_samples_num,     &
                                         this%m_cluster_num,     &
                                         sig_samples,            &
                                         this%m_cluster,         &
                                         this%m_cluster_center,  &
                                         cluster_variance        )
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                write(ERROR_UNIT,*) "====================================================================="
                write(ERROR_UNIT,*) " Kmeans_t%compute_hmeans01_kmeans02: FLOATING-POINT EXECEPTION(S) OCCURRED"
                write(ERROR_UNIT,*) "====================================================================="
          end if    
          call ieee_set_status(status_value)
         
          
!DIR$ ENDIF  
          if(verbose == .true.) then
              call cluster_print_summary( this%m_samples_num,  &
                                         this%m_cluster_num,  &
                                         this%m_cluster_population, &
                                         this%m_cluster_energy,     &
                                         cluster_variance           ) 
          end if
    end subroutine
    
    subroutine compute_hmeans01_kmeans03(this,sig_samples,cluster_variance,fp_flags, &
                                          samples_num,dim_num,cluster_num,verbose   )
          use kmeans_lib, only : cluster_initialize_5,  &
                                 hmeans_01,             &
                                 cluster_variance_compute, &
                                 kmeans_03,                &
                                 cluster_print_summary
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          use ifcore,
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF 
          class(Kmeans_t),                              intent(inout) :: this
          real(R64P), dimension(dim_num,samples_num),   intent(inout) :: sig_samples
          !DIR$ ASSUME_ALIGNED sig_samples:64
          real(R64P), dimension(cluster_num),           intent(out)   :: cluster_variance
          !DIR$ ASSUME_ALIGNED cluster_variance:64
          logical(I32P), dimension(5),                  intent(inout) :: fp_flags
          integer(I32P),                                intent(in)    :: samples_num,dim_num, &
                                                                         cluster_num
          logical(I32P),                                intent(in)    :: verbose
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
          
!DIR$ ENDIF
          ! Exec code
          if(ANY(fp_flags) == .true.) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,.false.)
!DIR$ ENDIF
          call cluster_initialize_5(this%m_dim_num,         &
                                    this%m_samples_num,     &
                                    this%m_cluster_num,     &
                                    sig_samples,            &
                                    this%m_seed,            &
                                    this%m_cluster_center   )
          
          call hmeans_01( this%m_dim_num,         &
                          this%m_samples_num,     &
                          this%m_cluster_num,     &
                          this%m_iter_max,        &
                          this%m_iter_num,        &
                          sig_samples,            &
                          this%m_cluster,         &
                          this%m_cluster_center,  &
                          this%m_cluster_population, &
                          this%m_cluster_energy    )
          
          call cluster_variance_compute( this%m_dim_num,         &
                                         this%m_samples_num,     &
                                         this%m_cluster_num,     &
                                         sig_samples,            &
                                         this%m_cluster,         &
                                         this%m_cluster_center,  &
                                         cluster_variance        )
          
           call kmeans_03(this%m_dim_num,         &
                          this%m_samples_num,     &
                          this%m_cluster_num,     &
                          this%m_iter_max,        &
                          this%m_iter_num,        &
                          sig_samples,            &
                          this%m_cluster,         &
                          this%m_cluster_center,  &
                          this%m_cluster_population, &
                          this%m_cluster_energy    )
           
           call cluster_variance_compute(this%m_dim_num,         &
                                         this%m_samples_num,     &
                                         this%m_cluster_num,     &
                                         sig_samples,            &
                                         this%m_cluster,         &
                                         this%m_cluster_center,  &
                                         cluster_variance        )
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                write(ERROR_UNIT,*) "====================================================================="
                write(ERROR_UNIT,*) " Kmeans_t%compute_hmeans01_kmeans03: FLOATING-POINT EXECEPTION(S) OCCURRED"
                write(ERROR_UNIT,*) "====================================================================="
          end if    
          call ieee_set_status(status_value)
         
          
!DIR$ ENDIF  
          if(verbose == .true.) then
              call cluster_print_summary( this%m_samples_num,  &
                                         this%m_cluster_num,  &
                                         this%m_cluster_population, &
                                         this%m_cluster_energy,     &
                                         cluster_variance           ) 
          end if
       end subroutine 
    
end module mod_kmeans