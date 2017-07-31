
module mod_abstract_SRF

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_abstract_SRF'
 !          
 !          Purpose:
 !                      Abstract reprsentation of Spatio-temporal
 !                      Random Field.
 !                      Concrete representations will derive and 
 !                      implement functionality interface provided
 !                      by this ADT.
 !                      Following concrete implementations are planned:
 !                      1) Pressure SRF
 !                      2) Humidity SRF
 !                      3) Tmperature SRF
 !                      4) Wind [u,v] speed SRF
 !                     
 !          History:
 !                        Date: 25-07-2017
 !                        Time: 08:12 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
 !          
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

    implicit none
    use module_kinds
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_ABSTRACT_SRF_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_ABSTRACT_SRF_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_ABSTRACT_SRF_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_ABSTRACT_SRF_FULLVER = 1000*MOD_ABSTRACT_SRF_MAJOR + 100*MOD_ABSTRACT_SRF_MINOR + &
                                                                   10*MOD_ABSTRACT_SRF_MINOR
    
    ! Module creation date
    character(*),  parameter, public :: MOD_ABSTRACT_SRF_CREATE_DATE = "25-07-2017 08:23 AM GMT+2 (TUE 25 JULY 2017 08:23 -00200)"
    
    ! Module build date -- must be set to latest build date/time
    character(*),  parameter, public :: MOD_ABSTRACT_SRF_BUILD_DATE = " "
    
    ! Module author
    character(*),  parameter, public :: MOD_ABSTRACT_SRF_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short synopsis
    character(*),  parameter, public :: MOD_ABSTRACT_SRF_DESCRIPT = "Abstract derived type representation of SRF"
    
    private
    
    public
    
    !================================================================70
    ! Declaration of abstract derived type: SRandomField_t
    ! Spatio-temporal Random Field.
    !================================================================70
    type, abstract :: SRandomField_t
        
        contains
    
        !================================================58
        ! Deffered type bound procedures and operators.
        !================================================58
    
        ! Print whole object state
        procedure(print_srf_state),          pass(this), deferred, public :: srf_print
        
        ! Write to file whole object state
        procedure(write_srf_tofile),         pass(this), deferred, public :: srf_tofile
        
        ! Write to file memory layout
        procedure(write_mem_tofile),         pass(this), deferred, public :: srf_mem_tofile
        
        ! Print memory layout of the object
        procedure(print_memory_layout),      pass(this), deferred, public :: srf_print_mem
        
        ! Print object type
        procedure(print_object_type),         pass(this), deferred, public :: srf_print_type
        
        
        ! SRF random realization description list(array) e.g. list(0) = gaussian,list(1) = beta, etc...
        procedure(srf_ran_distr_list),        pass(this), deferred, public :: srf_randistr_list
        
        ! SRF random realization state(content)
        procedure(srf_stoch_realization),      pass(this), deferred, public :: srf_stoch_real
        
        ! SRF random initialization by random distributions e.g. normal, chisquare, gamma, etc...
        procedure(srf_randistr_initialization),pass(this), deferred, public :: srf_rand_init
        
        ! SRF time step between two single field realizations
        ! procedure(srf_realization_time_step),  pass(this), deferred, public :: srf_time_step   passed to local getters
        
        ! SRF Type i.e homogenous or inhomogenous,vector or scalar, isotropic or anisotropic
        procedure(srf_field_description),      pass(this), deferred, public :: srf_description
        
        ! SRF Spectral density function  [formula (3) p.50]
        procedure(srf_spectral_density_func),  pass(this), deferred, public :: srf_sdf
        
        ! SRF 1D FFT
        procedure(srf_transform_fft1D),        pass(this), deferred, public :: srf_fft1D
        
        ! SRF 2D FFT
        procedure(srf_transform_fft2D),        pass(this), deferred, public :: srf_fft2D
        
        ! SRF 3D FFT
        procedure(srf_transform_fft3D),        pass(this), deferred, public :: srf_fft3D
        
        ! SRF standard deviation per rank 1 , for all realizations, requires array 2D
        procedure(srf_stdev_all_rank1),        pass(this), deferred, public :: srf_stdev_rank1
        
        ! SRF standard deviation per rank 2, for all realizations   requires array 2D
        procedure(srf_stdev_all_rank2),        pass(this), deferred, public :: srf_stdev_rank2
        
        ! SRF  standard deviation per rank 3, for all realizations  requires array 2D
        procedure(srf_stdev_all_rank3),        pass(this), deferred, public :: srf_stdev_rank3
        
        ! SRF moments per rank 1, for all realizations(events) requires array 2D
        procedure(srf_moments_all_rank1),      pass(this), deferred, public :: srf_moments_rank1
        
        ! SRF moments per rank 2,  for all realizations(events),  requires array 2D
        procedure(srf_moments_all_rank2),      pass(this), deferred, public :: srf_moments_rank2
        
        ! SRF moments per rank 3,  for all realizations(events), requires array 2D
        procedure(srf_moments_all_rank3),      pass(this), deferred, public :: srf_moments_rank3
        
        ! SRF Total moment per all rank , for all realizations, requires array 1D
        procedure(srf_total_moment),           pass(this), deferred, public :: srf_tot_moment
        
        ! SRF skewness per rank 1, for all realizations, requires array 2D
        procedure(srf_skewness_all_rank1),     pass(this), deferred, public :: srf_skew_rank1
        
        ! SRF skeweness per rank 2, for all realizations, requires array 2D
        procedure(srf_skewness_all_rank2),     pass(this), deferred, public :: srf_skew_rank2
        
        ! SRF skewness per rank 3, for all realizations, requires array 2D
        procedure(srf_skewness_all_rank3),     pass(this), deferred, public :: srf_skew_rank3
        
        ! SRF total skewness per all ranks , for all realizations requires array 1D
        procedure(srf_total_skewness),         pass(this), deferred, public :: srf_tot_skewness
        
        ! SRF Total standard deviation , for all realizations, requires array 1D
        procedure(srf_total_stdev),            pass(this), deferred, public :: srf_tot_stdev
        
        ! SRF Total Median absolute deviation, for all stochastic realizations
        ! requires array 1D
        procedure(srf_median_abs_devition),    pass(this), deferred, public :: srf_mad
        
        ! SRF Median Absolute Deviation per rank 1, for all stochastic realizations
        ! requires array 2D.
        procedure(srf_mad_all_rank1),          pass(this), deferred, public :: srf_mad_rank1
        
        ! SRF Median Absolute Deviation per rank 2, for all stochastic realizations
        ! requires array 2D.
        procedure(srf_mad_all_rank2),          pass(this), deferred, public :: srf_mad_rank2
        
        ! SRF Median Absolute Deviation per rank 3, for all stochastic realizations
        ! requires array 2D.
        procedure(srf_mad_all_rank3),          pass(this), deferred, public :: srf_mad_rank3
        
        ! SRF Variogram
        procedure(srf_compute_variogram),      pass(this), deferred, public :: srf_variogram
        
        ! SRF Semivariogram
        procedure(srf_compute_semivariogram),  pass(this), deferred, public :: srf_semivariogram
        
        ! SRF Variance-to-mean ratio
        procedure(srf_variance_mean_ratio),    pass(this), deferred, public :: srf_vmr
        
        ! SRF Sum of squared variables per rank 1(dim=1), for all realizations  
        !, requires array 2D
        procedure(srf_sum_all_rank1),          pass(this), deferred, public :: srf_sall_rank1
        
        ! SRF Sum of squared variables per rank 2(dim=2), for all realizations 
        ! , requires array 2D
        procedure(srf_sum_all_rank2),          pass(this), deferred, public :: srf_sall_rank2
        
        ! SRF sum of squared variables per rank 3(dim=3), 
        ! for all realizations, requires array 2D
        procedure(srf_sum_all_rank3),          pass(this), deferred, public :: srf_sall_rank3
        
        ! SRF Correlation function
        procedure(srf_corelation_function),  pass(this), deferred, public :: srf_corel_func
        
        ! SRF Amplitude-modulating function Theta(s,w)         [formula (27), p.54]
        procedure(srf_am_function),          pass(this), deferred, public :: srf_theta
        
        ! SRF Evolutionary mean spectral power density function [formula (28), p. 54]
        procedure(srf_evolution_spdf),       pass(this), deferred, public :: srf_spdf
        
        ! SRF Second-order spectral moment [formula (14), p.52[
        procedure(srf_2order_spec_moment),   pass(this), deferred, public :: srf_2order_sm
        
        ! SRF Spectral distribution function
        procedure(srf_spectral_distr_func),  pass(this), deferred, public :: srf_spectral_df
        
        ! SRF Spatial cross-covariance function [formula (19), p.36]
        procedure(srf_cross_covar_function), pass(this), deferred, public :: srf_xcovar_func
        
        ! SRF Coefficient of spatial cross-corelation coefficient
        procedure(srf_cross_corel_coef),     pass(this), deferred, public :: srf_xcorel_coef
        
        ! SRF Mean value (first moment) [formula (1), p.32]
        procedure(srf_first_moment),          pass(this), deferred, public :: srf_mean
        
        ! SRF Covariance [formula (2), p.32]
        procedure(srf_covariance),            pass(this), deferred, public :: srf_covar
        
        ! SRF Variance(total) [formula (4), p.32] computed for all field dimensions
        ! and for all stochastic realizations.
        procedure(srf_total_variance),        pass(this), deferred, public :: srf_tot_var
        
        ! SRF Variance [formula (4), p.32] computed for dim=1 and for all stochastic
        ! realizations, requires array 2D.
        procedure(srf_variance_rank1),        pass(this), deferred, public :: srf_var_rank1
        
        ! SRF Variance [formula (4), p.32] computed for dim=2 and for all stochastic
        ! realizations, requires array 2D.
        procedure(srf_variance_rank2),        pass(this), deferred, public :: srf_var_rank2
        
        ! SRF Variance [formula (4), p.32] computed for dim=3 and for all stochastic
        ! realizations, requires array 2D.
        procedure(srf_variance_rank3),          pass(this), deferred, public :: srf_var_rank3
        
        ! SRF Spatial corelation(total) function [formula (5), p.32]
        ! computed for all 3 dimensions and for all stochastic realizations
        ! requires array 1D.
        procedure(srf_total_spatcorel),  pass(this), deferred, public :: srf_tot_scorel
        
        ! SRF Spatial corelation function [formula (5), p.32] computed for
        ! dim=1 and for all stochastic realizations, requires array 2D.
        procedure(srf_spatcorel_rank1),  pass(this), deferred, public :: srf_scorel_rank1
        
        ! SRF Spatial corelation function [formula (5), p.32[ computed for
        ! dim=2 and for all stochastic realizations, requires array 2D.
        procedure(srf_spatcorel_rank2),  pass(this), deferred, public :: srf_scorel_rank2
        
        ! SRF Spatial corelation function [formula (5), p.32[ computed for
        ! dim=3 and for all stochastic realizations, requires array 2D.
        procedure(srf_spatcorel_rank3),  pass(this), deferred, public :: srf_scorel_rank3
        
        
        ! SRF Covariance of stochastic partial derivative [formula (21), p.60,62]
        procedure(srf_covariance_spderiv),    pass(this), deferred, public :: srf_covar_spderiv
        
        ! SRF Spatial covariance  [formula (33), p.62]
        procedure(srf_spatial_covariance),    pass(this), deferred, public :: srf_spat_covar
        
        ! SRF Nonegative-definite condition [formula (38), p.64]
        procedure(srf_noneg_definite_cond),   pass(this), deferred, public :: srf_ndef_cond
        
        ! SRF Homogenous cross-covariance [formula (48), p.66]
        procedure(srf_homogenous_cross_covar),pass(this), deferred, public :: srf_homo_xcovar
        
        ! SRF Cross-spectral density function [formula (56), p.68
        procedure(srf_cross_spect_dens_func), pass(this), deferred, public :: srf_xspectral_dfunc
        
        ! SRF - is isotropic [formula (1), p.69]
        procedure(srf_is_isotropic),          pass(this), deferred, public :: is_isotropic
        
        ! SRF Isotropic gaussian covariance [formula (10), p.71]
        procedure(srf_gaussian_covariance),   pass(this), deferred, public :: srf_gauss_covar
        
        ! SRF Isotropic exponential covariance [formula (11), p.71]
        procedure(srf_exponent_covariance),   pass(this), deferred, public :: srf_exponent_covar
        
        ! SRF Isotropic spherical covariance  [formula (12), p.71]
        procedure(srf_spherical_covariance),  pass(this), deferred, public :: srf_spheric_covar
        
        ! SRF is isotropic field continous    [chapter 8.2, p.71]
        procedure(srf_is_field_continous),    pass(this), deferred, public :: srf_is_continous
        
        ! SRF Isotropic correlation radius    [formula (1), p.76]
        procedure(srf_isotrop_corel_radius),  pass(this), deferred, public :: srf_corel_radius
        
        !SRF Entropy field                    [formula (1), p.105]
        procedure(srf_field_entropy),         pass(this), deferred, public :: srf_entropy
        
        ! SRF Gaussian random vector entropy  [formula (3), p.104]
        procedure(srf_gauss_randvec_entropy), pass(this), deferred, public :: srf_grandvec_entropy
        
        ! SRF Conditional entropy             [formula (5), p.105]
        procedure(srf_conditional_entropy),   pass(this), deferred, public :: srf_cond_entropy
        
        ! SRF compute an amount of information [formula (10), p.105]
        procedure(srf_randvar_info_amount),   pass(this), deferred, public :: srf_info_amount
        
        ! SRF Sample covariance                [formula (21), p.259]
        procedure(srf_sample_covariance),     pass(this), deferred, public :: srf_sample_covar
        
        ! SRF Sample mean                      [formula (22), p.259]
        procedure(srf_compute_sample_mean),   pass(this), deferred, public :: srf_sample_mean
        
        ! SRF Sample semivariogram             [formula (23), p.259]
        procedure(srf_sample_semivariogram),  pass(this), deferred, public :: srf_sample_semivario
        
        ! SRF Sample covariance on 2D grid     [formula (24), p.259]
        procedure(srf_sample_covariance2D),   pass(this), deferred, public :: srf_sample_covar2D
        
        ! SRF Sample semivariogram on 2D grid [formula (25), p.259]
        procedure(srf_sample_semivariogram2D),pass(this), deferred, public :: srf_sample_semivario2D
        
        !============================================================70
        !  Type-bound generic operators
        !
        !============================================================70
        
        ! Copy-Assignment Operator
        procedure(abstract_operator_copy),      pass(this), deferred, public :: operator_copy
        
        ! Mathematical Operators
        
        procedure(abstract_operator_gradient),   pass(this), deferred, public :: operator_gradient
        
        procedure(abstract_operator_divergence), pass(this), deferred, public :: operator_divergence
        
        procedure(abstract_operator_laplacian),  pass(this), deferred, public :: operator_laplacian
        
        procedure(abstract_operator_curl),       pass(this), deferred, public :: operator_curl
        
        ! Operators-to-procedure names mapping.
        
        generic :: assignment(=)         =>  operator_copy
        
        generic :: operator(.grad.)      =>  operator_gradient
        
        generic :: operator(.div.)       =>  operator_divergence
        
        generic :: operator(.laplacian.) =>  operator_laplacian
        
        generic :: operator(.curl.)      =>  operator_curl
        
        
    end type SRandomField_t
        
    abstract interface
    
          subroutine print_srf_state(this)
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t), intent(in) :: this
          
          end subroutine
    
    end interface
    
    abstract interface 
    
          subroutine write_srf_tofile(this,unit,fname,append,iostat,errmsg)
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t), intent(in)    :: this
                    integer(I32P),         intent(in)    :: unit
                    character(len=*),      intent(in)    :: fname
                    character(len=*),      intent(in)    :: append
                    integer(I32P),         intent(inout) :: iostat
                    character(len=256),    intent(inout) :: errmsg
          end subroutine
          
    end interface
    
    abstract interface
    
          subroutine write_mem_tofile(this,unit,fname,append,iostat,errmsg)
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t), intent(in)    :: this
                    integer(I32P),         intent(in)    :: unit
                    character(len=*),      intent(in)    :: fname
                    character(len=*),      intent(in)    :: append
                    integer(I32P),         intent(inout) :: iostat
                    character(len=256),    intent(inout) :: errmsg
          end subroutine
    
    end interface
    
    abstract interface
    
          subroutine print_memory_layout(this)
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t), intent(in) :: this
                    
          end subroutine
          
    end interface
    
    abstract interface
    
          subroutine print_object_type(this)
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t), intent(in) :: this
                    
          end subroutine
          
    end interface
    
    abstract interface
    
          pure function srf_ran_distr_list(this,nelems) result(list)
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t), intent(in) :: this
                    integer(I32P),         intent(in) :: nelems
                    ! Locals return
                    character(len=32), dimension(nelems) :: list
          end function
    
    end interface
    
    abstract interface
    
          subroutine srf_stoch_realization(this,nevents,randgen,   &
                                           stoch_copy,put1D,      &
                                           put2D,option1D,ssa4D,  &
                                           fp_flags1D )
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t),              intent(inout) :: this
                    integer(I32P),                      intent(in)    :: nevents
                    character(len=*),                   intent(in)    :: randgen
                    real(R64P), dimension(:,:,:,:),     intent(out)   :: stoch_copy
                    integer(I32P), dimension(:),        intent(in)    :: put1D
                    integer(I32P), dimension(:,:),      intent(in)    :: put2D
                    character(len=:), dimension(:),     intent(in)    :: option1D
                    real(R64P), dimension(:,:,:,:),     intent(in)    :: ssa4D
                    logical(I32P), dimension(ntimes*5), intent(inout) :: fp_flags
          end subroutine
    
    end interface
    
    abstract interface
    
          subroutine srf_randistr_initialization(this,distribution,nevents,beta_args,   &  ! beta args = (2,nargs)
                                                 binom_args,nbinom_args,dfchisq_args,   &  ! nbinom args (nargs) ,integer
                                                 rexp_args,                             &  ! binom args = (nargs), rexp_args = (nargs)
                                                 randf_args,gamma_args,nchisq_args,     &  ! randf args = (2,nargs), gamma args = (2,nargs)
                                                 ncf_args, normal_args,rpoiss_args,     &  ! nchisq_args = (2,nargs), ncf args = (3,nargs)
                                                 rstdgam_args,uniform_args,nelems       )   ! normal_args = (2,nargs),rpoiss args = (nargs)
                                                                                            ! rstdgamma args = (nargs),uniform args = (2,nargs)
                                                                                            ! nelems == nevents
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t),          intent(inout) :: this
                    character(len=*),               intent(in)    :: distribution
                    real(R64P), dimension(2,:),     intent(in)    :: beta_args,randf_args,    &
                                                                     gamma_args,nchisq_args,  &
                                                                     normal_args,uniform_args
                    real(R64P), dimension(:),       intent(in)    :: binom_args,dfchisq_args, &
                                                                     rexp_args,rpoiss_args,   &
                                                                     rstdgam_args
                    real(R64P), dimension(3,:),     intent(in)    :: ncf_args
                    integer(I32P), dimension(:),    intent(in)    :: nbinom_args
                    integer(I32P),                  intent(in)    :: nelems,nevents
          
          end subroutine
                                                 
    end interface
    
    
    
    abstract interface
    
          pure function srf_field_description(this) result(descript)
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t), intent(in) :: this
                    ! Locals/return
                    character(len=256)                :: descript
          
          end function
    
    end interface
    
    abstract interface
    
          subroutine srf_spectral_density_func(this,Xw,Xw_conj,pdf,ids,ide,jds,jde, &
                                               kds,kde,ims,ime,jms,jme,kms,kme,     &
                                               its,ite,jts,jte,kts,kte,fp_flags  )
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t),                          intent(inout) :: this
                    real(R64P), dimension(ims:ime,kms:kme,jms:jme), intent(in)    :: Xw       ! Sample Xi(w)       ! ____
                    real(R64P), dimension(ims:ime,kms:kme,jms:jme), intent(in)    :: Xw_conj  ! conjugate of sample Xi(w)
                    real(R64P), dimension(ims:ime,kms:kme,jms:jme), intent(in)    :: pdf  ! Probability distribution function 
                    integer(I64P),                                  intent(in)    :: ids,ide, &
                                                                                     jds,jde, &  ! Indices
                                                                                     kds,kde, &
                                                                                     ims,ime, &
                                                                                     jms,jme, &
                                                                                     kms,kme, &
                                                                                     its,ite, &
                                                                                     jts,jte, &
                                                                                     kts,kte 
                    logical(I32P), dimension(:),                    intent(inout)  :: fp_flags  ! Size should be set to 5*nevents
                    
          end subroutine
                                               
    end interface
    
    abstract interface
           ! FFT in-place SRandomField_t must have appropriate array member in order to hold the samples
           ! Originally array 'a'
          subroutine srf_transform_fft1D(this,transform,n,isgn,ip,w,fp_flags)
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t),        intent(inout)    :: this
                    character(len=*),             intent(in)       :: transform ! transform to choose
                    integer(I32P),                intent(in)       :: n,isgn
                    integer(I32P), dimension(0:), intent(in)       :: ip
                    real(R64P),    dimension(0:), intent(inout)    :: w
                    logical(I32P), dimension(5),  intent(inout)    :: fp_flags
                    
          end subroutine
                                        
    end interface
    
    abstract interface
            ! FFT in-place SRandomField_t must have appropriate array member in order to hold the samples
            ! Originally array 'a'
          subroutine srf_transform_fft2D(this,transform,n1max,n1,n2, &
                                         isgn,t,ip,w,fp_flags  )
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t),           intent(inout)    :: this
                    character(len=*),                intent(in)       :: transform
                    integer(I32P),                   intent(in)       :: n1max,n1,n2,isgn
                    integer(I32P), dimension(0:),    intent(in)       :: ip
                    real(R64P), dimension(0:8*n2-1), intent(inout)    :: t
                    real(R64P), dimension(0:),       intent(in)       :: w
                    logical(I32P), dimension(5),     intent(inout)    :: fp_flags
                    
          end subroutine
                                         
    end interface
    
    abstract interface
           ! FFT in-place SRandomField_t must have appropriate array member in order to hold the samples
           ! Originally array 'a' 
          subroutine srf_transform_fft3D(this,transform,n1max,n2max,n1, &
                                         n2,n3,isgn,t,ip,w,fp_flags  )
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t),          intent(inout) :: this
                    character(len=*),               intent(in)    :: transform
                    integer(I32P),                  intent(in)    :: n1max,n2max,n1, &
                                                                     n2,n3,isgn
                    integer(I32P), dimension(0:),   intent(in)    :: ip
                    real(R64P),    dimension(0:),   intent(inout) :: t,w
                    logical(I32P), dimension(5),    intent(inout) :: fp_flags
                    
          end subroutine
                                         
    end interface
    
    abstract interface
    
          subroutine srf_stdev_all_rank1(this,stdev,v_probs,rank,nevents,robust, &
                                         fp_flags  )
             import :: SRandomField_t
             implicit none
                     class(SRandomField_t),               intent(inout) :: this
                     real(R64P), dimension(nevents,:),    intent(out)   :: stdev
                     real(R64P), dimension(:),            intent(in)    :: v_probs ! probabilities
                     integer(I32P),                       intent(in)    :: rank,nevents
                     character(len=*),                    intent(in)    :: robust  ! robust method for variance computation
                     logical(I32P), dimension(nevents*5), intent(inout) :: fp_flags
                     
          end subroutine
    end interface
    
    abstract interface
    
          subroutine srf_stdev_all_rank2(this,stdev,v_probs,rank,nevents,robust, &
                                           fp_flags )
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t),                intent(inout) :: this
                    real(R64P), dimension(nevents,:)      intent(out)   :: stdev
                    real(R64P), dimension(:),             intent(in)    :: v_probs  ! probabilities
                    integer(I32P),                        intent(in)    :: rank,nevents
                    character(len=*),                     intent(in)    :: robust   ! robust method for variance computation
                    logical(I32P), dimension(nevents*5),  intent(inout) :: fp_flags
                    
          end subroutine
                                           
    end interface
    
    abstract interface
        
          subroutine srf_stdev_all_rank3(this,stdev,v_probs,rank,nevents,robust, &
                                           fp_flags )
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t),               intent(inout)  :: this
                    real(R64P), dimension(nevents,:),    intent(out)    :: stdev
                    real(R64P), dimension(:),            intent(in)     :: v_probs  ! probabilities
                    integer(I32P),                       intent(in)     :: rank,nevents
                    character(len=*),                    intent(in)     :: robust    ! robust method for variance computation
                    logical(I32P), dimension(nevents*5), intent(inout)  :: fp_flags
                    
          end subroutine
    
    end interface
    
    abstract interface
    
          subroutine srf_total_stdev(this,stdev,nevents,robust,fp_flags)
             import :: SRandomField_t
             implicit none
                    class(SRandomField_t),              intent(inout) :: this
                    real(R64P), dimension(nevents),     intent(out)   :: stdev
                    integer(I32P),                      intent(in)    :: nevents
                    character(len=*),                   intent(in)    :: robust
                    logical(I32P), dimension(nevents*5),intent(inout) :: fp_flags
                    
          end subroutine
    end interface
    
    


end module mod_abstract_SRF