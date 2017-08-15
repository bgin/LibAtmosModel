
module mod_constants

   
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_constants'
 !          
 !          Purpose:
 !                    Collection of various library constants
 !                   
 !                     
 !          History:
 !                        Date: 11-08-2017
 !                        Time: 09:48 GMT+2
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
    integer(I32P), parameter, public :: MOD_CONSTANTS_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_CONSTANTS_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_CONSTANTS_MICRO = 0
    
    ! Module/file full version
    integer(I32P), parameter, public :: MOD_CONSTANTS_FULLVER = 1000*MOD_CONSTANTS_MAJOR+100*MOD_CONSTANTS_MINOR + &
                                                                10*MOD_CONSTANTS_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_CONSTANTS_CREATION_DATE = "11-08-2017 11:21 +00200 (FRI 11 AUG 2017 GMT+2)"
    
    ! Module build date (should be set after every succesfful build)
    character(*),  parameter, public :: MOD_CONSTANTS_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_CONSTANTS_AUTHOR = "Programmer: Bernard Gingold e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_CONSTANTS_DESCRIPT = " Collection of various math and phys constants."
    
    !======================================================60
    !         Definition of various constants
    !   @Remark:
    !            Encoding is: LAM_constant_name
    !======================================================60
    
    ! Signals minimal number of samples
    integer(I32P), parameter, public :: LAM_MINSAMP    = 32
    
    ! Signal minimum frequency
    real(R64P),    parameter, public :: LAM_MINFREQ    = 1.0e+7_R64P
    
    ! One nano-second
    real(R64P),    parameter, public :: LAM_NS         = 0.000000001_R64P
    
    ! Ohm resistenace for propagating wave
    real(R64P),    parameter, public :: LAM_WRES       = 377.0_R64P
    
    ! Signal smallest time-step i.e. 1/2^-16
    real(R64P),    parameter, public :: LAM_SMALLTS    = 0.0000152587890625_R64P 
    
    ! Speed of light
    real(R64P),    parameter, public :: LAM_c          = 299792458._R64P
    
    ! Gravitational constant
    real(R64P),    parameter, public :: LAM_g          = 9.81.R64P
    
    ! Gas constant dry air i.e 287.04 m^2*s^-2*K^-1
    real(R64P),    parameter, public :: LAM_R          = 287.04_R64P 
    
    ! Surface refractivity
    real(R64P),    parameter, public :: LAM_N          = 313._R64P
    
    ! Refractivity constant C'd  i.e. 77.6 K mbar^-1
    real(R64P),    parameter, public :: LAM_Cd         = 77.6_R64P
    
    ! Refractivity constant C'w1 i.e. 71.6 K mbar^-1
    real(R64P),    parameter, public :: LAM_Cw1 = 71.6_R64P
    
    ! Refractivity constant C'w2 i.e. 3.7x10^5 K^2 mbar^-1
    real(R64P),    parameter, public :: LAM_Cw2 = 370000._R64P
    
    ! Number of molecule per unit volume Nv0 i.e. 2.6873x10^25 m^-3
    real(R64P),    parameter, public :: LAM_Nv0 = 26873000000000000000000000._R64P
    
    ! Inverse SRTQ of 2 i.e 1/SQRT(2)
    real(R64P),    parameter, public :: LAM_ISQRT2 = 0.7071067811865475488016887242097_R64P
    
    ! Null-valued complex constant
    complex(R64P), parameter, public :: LAM_ZC = DCMPLX(0._R64P,0._R64P)
    
    ! SQRT(2)
    real(R64P),    parameter, public :: LAM_SQRT2 =  1.414213562373095_R64P
    
    ! Earth mean radius i.e. 6371.0 km
    real(R64P),    parameter, public :: LAM_EMR = 6371._R64P
    
    ! Value of PI
    real(R64P),    parameter, public :: LAM_PI =  3.1415926535897932384626433832795_R64P
    
    ! Value of 2PI
    real(R64P),    parameter, public :: LAM_2PI =  6.283185307179586476925286766559_R64P
    
    ! Value of 4PI
    real(R64P),    parameter, public :: LAM_4PI =  12.566370614359172953850573533118_R64P
    
    ! Value of 8PI
    real(R64P),    parameter, public :: LAM_8PI =  25.132741228718345907701147066236_R64P
    
    ! Value of half PI
    real(R64P),    parameter, public :: LAM_HPI = 1.5707963267948966192313216916398_R64P
    
    ! Value of quarter PI
    real(R64P),    parameter, public :: LAM_QPI =  0.78539816339744830961566084581988_R64P
    
    ! Value of PI**2
    real(R64P),    parameter, public :: LAM_PIE2 =  9.8696044010893586188344909998762_R64P
    
    ! Value of PI**3
    real(R64P),    parameter, public :: LAM_PIE3 =  31.006276680299820175476315067101_R64P
    
    ! Value of PI**4
    real(R64P),    parameter, public :: LAM_PIE4 =  97.409091034002437236440332688705_R64P
    
    ! Value of PI**5
    real(R64P),    parameter, public :: LAM_PIE5 =  306.01968478528145326274131004344_R64P
    
    ! Value of SQRT(PI)
    real(R64P),    parameter, public :: LAM_SQRTPI =  1.7724538509055160272981674833411_R64P
    
    ! Value of SQRT(2PI)
    real(R64P),    parameter, public :: LAM_SQRT2PI =  2.506628274631000502415765284811_R64P
    
    ! Value of SQRT(4PI)
    real(R64P),    parameter, public :: LAM_SQRT4PI =  3.5449077018110320545963349666823_R64P
    
    ! Value of 1/SQRT(2PI)
    real(R64P),    parameter, public :: LAM_ISQRT2PI =  0.39894228040143267793994605993439_R64P
    
    ! Value of 1/SQRT(4PI)
    real(R64P),    parameter, public :: LAM_ISQRT4PI =  0.07957747154594766788444188168626_R64P
    
    ! Value of Ln(2)
    real(R64P),    parameter, public :: LAM_Ln2     =   0.69314718055994530941723212145818_R64P
    
    ! Value of Ln(4)
    real(R64P),    parameter, public :: LAM_Ln4     =   1.3862943611198906188344642429164_R64P
    
    ! Shift by 1000
    real(R64P),    parameter, public  :: LAM_SHIFT1000 = 1000.0_R64P
             
    ! Very small constant
    real(R64P), parameter,    public :: LAM_SMALL =   0.0000000000000001_R64P
    
    ! Smallest repreesentable number
    real(R64P), parameter,    public :: LAM_SMALLEST = TINY(LAM_SMALL)
             
    ! Prevent potential sliding towards denormal numbers
    real(R64P), parameter,    public :: LAM_TINY = LAM_SHIFT1000 * LAM_SMALLEST
             
    ! Define smallest allowed numeric value of Pressure Field
    real(R64P), parameter,    public :: LAM_TINYPRESS = LAM_TINY
             
    ! Very large constant
    real(R64P), parameter,    public :: LAM_LARGE = 1.0E+35_R64P
             
    real(R64P), parameter,    public :: LAM_LARGEST = HUGE(LAM_LARGE)
    
    ! Minimum number of Sawtooth approximation series waveform (sinusoids)
    integer(I32P), parameter, public :: LAM_MINK = 24
             
    ! Initialize by zero constant.
    real(R64P), parameter,    public :: LAM_ZINIT = 0._R64P
    
    real(R64P), parameter,    public :: LAM_ZR8 = 0._R64P
             
    ! Integral 1 (64-bit)        
    integer(I64P), parameter, public :: LAM_IONE8 = 1_I64P         
   
    ! Integral 0 (64-bit)        
    integer(I64P), parameter, public :: LAM_IZER8 = 0_I64P
    
    ! Integral 0 (32-bit)
    integer(I32P), parameter, public :: LAM_IZER4 = 0_I32P
     
    ! Machine epsilon (64-bit)
    real(R64P),    parameter, public :: LAM_MEPS8 =  0.00000000000000022204460492503131_R64P
    ! Standard tolerance (for statistical calculations)         
    real(R64P),    parameter, public :: LAM_STATOL  = 0.0000000000001_R64P
             
    real(R64P),    parameter, public :: LAM_ONER8 = 1.0_R64P
             
    real(R64P),    parameter, public :: LAM_QR64P = 0.25_R64P
             
    real(R64P),    parameter, public :: LAM_HR64P = 0.5_R64P
             
    real(R64P),    parameter, public :: LAM_PINF   = IEEE_VALUE(1._R64P,IEEE_POSITIVE_INF)
    
    ! Coefficeints for Gibson function of ice Ih
    
    ! Coeff G00 i.e. 632020.233335886_R64P J kg^-1 (real part)
    real(R64P),    parameter, public :: LAM_G00  =  -632020.233335886_R64P
    
    ! Coeff G01  i.e. 0.655022213658955_R64P J kg^-1  (real part)
    real(R64P),    parameter, public :: LAM_G01  =  0.655022213658955_R64P
    
    ! Coeff G02  i.e. -0.0000000189369929326131 J kg^-1  (real part)
    real(R64P),    parameter, public :: LAM_G02  = -0.0000000189369929326131_R64P 
    
    ! Coeff G03  i.e  0.00000000000000339746123271053 J kg^-1
    real(R64P),    parameter, public :: LAM_G03  =  0.00000000000000339746123271053_R64P
    
    ! Coeff G04  i.e -5.56464869058991e-22 J kg^-1
    real(R64P),    parameter, public :: LAM_G04  =  -5.56464869058991e-22_R64P
    
    ! Coeff Sn0 (absolute)
    real(R64P),    parameter, public :: LAM_Sn0  =   189.13_R64P
    
    

end module mod_constants