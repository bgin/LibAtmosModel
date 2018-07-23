
#include "Config.fpp"    
    
module mod_timsacwrap

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_timsacwrap'
 !          
 !          Purpose:
 !                      This module contains high-level wrappers around TIMSAC programs
 !                      
 !                       
 !          History:
 !                        Date: 19-07-2018
 !                        Time: 12:52 GMT+2
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
    
    use module_kinds, only : I32P, R64P
    use mod_timsac,   only :  AUSPECF,   &
                              AUTARMF,   &
                              AUTCORF,   &
                              BAYSEAF,   &
                              BISPECF,   &
                              BLOCARF,   &
                              BLOMARF,   &
                              BSUBSTF,   &
                              CANARMF,   &
                              CANOCAF,   &
                              COVGENF,   &
                              DECOMPF,   &
                              EXSARF,    &
                              FFTCORF,   &
                              FPEAUTF,   &
                              FPEC7F,    &
                              MARKOVF,   &
                              MLOCARF,   &
                              MLOMARF,   &
                              MULBARF,   &
                              MULCORF,   &
                              MULFRFF,   &
                              MULMARF,   &
                              MULNOSF,   &
                              MULRSPF,   &
                              MULSPEF,   &
                              NONSTF,    &
                              PRDCTRF,   &
                              XSARMAF
                             
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_TIMSACWRAP_MAJOR = 1_I32P
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_TIMSACWRAP_MINOR = 0_I32P
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_TIMSACWRAP_MICRO = 0_I32P
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_TIMSACWRAP_FULLVER = 1000_I32P*MOD_TIMSACWRAP_MAJOR + &
                                                                  100_I32P*MOD_TIMSACWRAP_MINOR  + &
                                                                  10_I32P*MOD_TIMSACWRAP_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_TIMSACWRAP_CREATE_DATE = "19-07-2018 12:46 +00200 (THR 19 JUL 2018 GMT+2)  "
    
    ! Module build date (  should be set after successful compilation)
    character(*),  parameter, public :: MOD_TIMSACWRAP_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_TIMSACWRAP_AUTHOR = " Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_TIMSACWRAP_DESCRIPT = "Wrapper around 'TIMSAC' programs. "
    
    contains
    
    
    !============================================================================
    !  Wrapper around: 'AUSPECF' program
    !  ----------- Original description -----------------
    !   THIS PROGRAM COMPUTES POWER SPECTRUM ESTIMATES FOR TWO
    !   TRIGONOMETRIC WINDOWS OF BLACKMAN-TUKEY TYPE BY GOERTZEL METHOD.
    !   ONLY ONE CARD OF LAGH(MAXIMUM LAG OF COVARIANCES TO BE USED FOR
    !   POWER SPECTRUM COMPUTATION) SHOULD BE ADDED ON TOP OF THE OUTPUT
    !   OF PROGRAM 5.1.1 AUTCOR TO FORM INPUT TO THIS PROGRAM.
    !   OUTPUTS ARE ESTIMATES P1(I),P2(I) FOR FREQUENCIES I/(2LAGH*DELTAT)
    !   AND THE TEST STATISTICS Q(I) FOR THE DIFFERENCES BETWEEN P1(I) AND
    !   P2(I).   Q(I) GREATER THAN 1 MEANS SIGNIFICANT DIFFERENCE.
    ! ============================================================================
    subroutine auspecf_wrapper( n,lagh1,cxx1,p1,p2,q,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          integer(I32P), intent(inout)                :: n
          integer(I32P), intent(in)                   :: lagh1
          real(R64P), dimension(lagh1), intent(in)    :: cxx1
          real(R64P), dimension(lagh1), intent(inout) :: p1
          real(R64P), dimension(lagh1), intent(inout) :: p2
          real(R64P), dimension(lagh1), intent(inout) :: q
          logical(I32P), dimension(5),  intent(inout) :: fp_flags
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)    :: status_value
!DIR$ ENDIF
          ! Exec code....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
          call AUSPECF(n,lagh1,cxx1,p1,p2,q)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " auspecf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
          
    end subroutine auspecf_wrapper
    
    !================================================================================86
    !   Wrapper around: 'AUTARMF' program
    !   ---------------- Original description ---------------------
    ! THIS PROGRAM PROVIDES AN AUTOMATIC AR-MA MODEL FITTING PROCEDURE.
    ! MODELS WITH VARIOUS ORDERS ARE FITTED AND THE BEST CHOICE IS DETER
    ! WITH THE AID OF THE STATISTICS AIC.
    ! THE MAXIMUM LIKELIHOOD ESTIMATES OF THE COEFFICIENTS OF A SCALAR
    ! AUTOREGRESSIVE MOVING AVERAGE MODEL Y(I)+B(1)Y(I-1)+...+B(IQ)Y(I-I
    ! =X(I)+A(1)X(I-1)+...+A(IP)X(I-IP) OF A TIME SERIES Y(I)
    ! ARE OBTAINED BY USING DAVIDON'S VARIANCE ALGORITHM.
    ! PURE AUTOREGRESSION IS NOT ALLOWED.
    ! FOR AR-MODELS USE THE INTERMEDIATE OUTPUTS OF CANARM.
    !===============================================================================86
    subroutine autarmf_wrapper(n,lagh01,cyy1,newl1,iqi1,iqi1_elem,b1,ipi1,ipi1_elem,a1,  &
                               newn,iq,b2,ip,a2,std,cxx2,g,saic,aicm, &
                               kq,kp,lmax,mmax,nmax, fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
    
          integer(I32P),                       intent(inout) :: n 
          integer(I32P),                       intent(in)    :: lagh01
          real(R64P),    dimension(lagh01),    intent(inout) :: cyy1
          integer(I32P),                       intent(in)    :: newl1
          integer(I32P), dimension(newl1),     intent(inout) :: iqi1
          integer(I32P),                       intent(in)    :: iqi1_elem
          real(R64P),    dimension(iqi1_elem), intent(inout) :: b1
          integer(I32P), dimension(newl1),     intent(inout) :: ipi1
          integer(I32P),                       intent(in)    :: ipi1_elem
          real(R64P),    dimension(ipi1_elem), intent(inout) :: a1
          integer(I32P),                       intent(inout) :: newn
          integer(I32P), dimension(nmax),      intent(inout) :: iq
          real(R64P),    dimension(mmax,nmax), intent(inout) :: b2
          integer(I32P), dimension(nmax),      intent(inout) :: ip
          real(R64P),    dimension(mmax,nmax), intent(inout) :: a2
          real(R64P),    dimension(mmax,nmax), intent(inout) :: std
          real(R64P),    dimension(nmax),      intent(inout) :: cxx2
          real(R64P),    dimension(mmax,nmax), intent(inout) :: g
          real(R64P),    dimension(nmax),      intent(inout) :: saic
          real(R64P),                          intent(inout) :: aicm
          integer(I32P),                       intent(inout) :: kq,kp,lmax
          integer(I32P),                       intent(in)    :: mmax,nmax
          logical(I32P), dimension(5),         intent(inout) :: fp_flags
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)    :: status_value
!DIR$ ENDIF
          ! Exec code ....
          ! Sanity checking
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          call AUTARMF(n,lagh01,cyy1,newl1,iqi1,b1,ipi1,a1,newn,iq,b2,  &
                       ip,a2,std,cxx2,g,saic,aicm,kq,kp,lmax,mmax,nmax  )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " autarmf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF             
    end subroutine autarmf_wrapper
                               
    !================================================================================86
    !    Wrapper around: 'AUTCOR' program.
    !    ---------------- Original description -------------------------                       
    !     THIS PROGRAM REQUIRES FOLLOWING INPUTS:
    !     N: LENGTH OF DATA
    !     LAGH: MAXIMUM LAG
    !     DFORM: INPUT FORMAT SPECIFICATION STATEMENT IN ONE CARD,
    !     FOR EXAMPLE
    !     (8F10.4)
    !     (X(I),I=1,N): ORIGINAL DATA.
    !     THE OUTPUTS ARE AUTOCOVARIANCES (CXX(I); I=0,LAGH) AND
    !     AUTO CORRELATIONS (NORMALIZED COVARIANCES).
    !================================================================================86
    subroutine autcorf_wrapper(x,n,cxx,cn,lagh1,xmean,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          real(R64P), dimension(n),     intent(in)    :: x
          integer(I32P),                intent(in)    :: n
          real(R64P), dimension(lagh1), intent(inout) :: cxx
          real(R64P), dimension(lagh1), intent(inout) :: cn
          integer(I32P),                intent(in)    :: lagh1
          real(R64P),                   intent(inout) :: xmean
          logical(I32P), dimension(5),  intent(inout) :: fp_flags
          ! Locals
!DIR$ IF (USE_IEEE_EXCPTIONS_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$ ENDIF
          ! Exec code .... 
          ! Sanity checking
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
          call AUTCORF(x,n,cxx,cn,lagh1,xmean)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " autcorf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF            
    end subroutine autcorf_wrapper
    
    !================================================================================86
    !   Wrapper around: 'BAYSEAF' program.
    !   ------------------ Original description ---------------------------
    !   THIS PROGRAM REALIZES A DECOMPOSITION OF TIME SERIES Y            
    !   INTO THE FORM                                                     
    !   Y(I) = T(I) +S(I)+I(I)+TDC(I)+OCF(I)                              
    !   WHERE  T(I)=TREND  S(I)=SEASONAL  I(I)=IRREGULAR                  
    !        TDC(I)=TRADING DAY COMPONENT     AND                       
    !        OCF(I)=OUTLIER CORRECTION FACTOR                           
    !                                                                   
    !   THE PROCEDURE IS BASED ON A BAYESIAN MODEL AND ITS                
    !   PERFORMANCE IS CONTROLLED BY THE SELECTION OF THE PARAMETERS OF   
    !   THE PRIOR DISTRIBUTION. 
    !================================================================================86
    subroutine bayseaf_wrapper(y,ndata,focast,cdata,dmoi,trend,season,tdcmp,irreg,  &
                               adjust,est,psds,psdt,avabic,ipara,para,arft,arfs,    &
                               arfn,iart,iars,iarn,fp_flags                          )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          real(R64P),   dimension(ndata),        intent(in)         :: y
          integer(I32P),                         intent(in)         :: ndata,focast
          real(R64P),   dimension(ndata),        intent(inout)      ::  cdata
          real(R64P),   dimension(ndata),        intent(inout)      :: dmoi
          real(R64P),   dimension(ndata+focast), intent(inout)      :: trend
          real(R64P),   dimension(ndata+focast), intent(inout)      :: season
          real(R64P),   dimension(ndata+focast), intent(inout)      :: tdcmp
          real(R64P),   dimension(ndata),        intent(inout)      :: irreg
          real(R64P),   dimension(ndata),        intent(inout)      :: adjust
          real(R64P),   dimension(ndata+focast), intent(inout)      :: est
          real(R64P),   dimension(ndata+focast), intent(inout)      :: psds
          real(R64P),   dimension(ndata+focast), intent(inout)      :: psdt
          real(R64P),                            intent(inout)      :: avabic
          integer(I32P), dimension(12),          intent(inout)      :: ipara
          real(R64P),    dimension(8),           intent(in)         :: para
          real(R64P),    dimension(3),           intent(in)         :: arft,arfs,arft
          integer(I32P),                         intent(in)         :: iart,iars,iarn
          logical(I32P), dimension(5),           intent(inout)      :: fp_flags
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$ ENDIF
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          call BAYSEAF(y,ndata,focast,cdata,dmoi,trend,season,tdcmp,irreg,adjust,  &
                       est,psds,psdt,avabic,ipara,para,arft,arfs,arfn,iart,iars,iarn )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " bayseaf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
          
    end subroutine bayseaf_wrapper
                               
    !================================================================================86
    !    Wrapper around: 'BISPECF' program.
    !    ------------------------ Original description -------------------------
    !     THIS PROGRAM COMPUTES BISPECTRUM USING THE DIRECT FOURIER TRANSFOR
    !     OF SAMPLE THIRD ORDER MOMENTS.
    !     THIS PROGRAM REQUIRES THE FOLLOWING INPUTS;
    !     OUTPUTS OF THE PROGRAM THIRMO:
    !     N; DATA LENGTH,
    !     MH; MAXIMUM LAG,
    !     CC(I); AUTOCOVARIANCES,
    !     C(I,J); THIRD ORDER MOMENTS.                       
    !================================================================================86
    subroutine bispecf_wrapper(n,mh,cc0,c0,p1,p2,q,a,br,bi,rat,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          integer(I32P),                     intent(in)    :: n
          integer(I32P),                     intent(in)    :: mh
          real(R64P),  dimension(mh+1),      intent(in)    :: cc0
          real(R64P),  dimension(mh+1,mh+1), intent(in)    :: c0
          real(R64P),  dimension(mh+1),      intent(inout) :: p1
          real(R64P),  dimension(mh+1),      intent(inout) :: p2
          real(R64P),  dimension(mh+1),      intent(inout) :: q
          real(R64P),  dimension(mh+1,mh+1), intent(inout) :: a
          real(R64P),  dimension(mh+1,mh+1), intent(inout) :: br
          real(R64P),  dimension(mh+1,mh+1), intent(inout) :: bi
          real(R64P),                        intent(inout) :: rat
          logical(I32P), dimension(5),       intent(inout) :: fp_flags
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$ ENDIF
          ! Exec code ....
          ! Sanity chck
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          call BISPECF(n,mh,cc0,c0,p1,p2,q,a,br,bi,rat)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " bispecf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF            
    end subroutine bispecf_wrapper
    
    !================================================================================86
    !   Wrapper around: 'BLOCARF' program
    !   ----------------------- Original description ------------------------------
    !    BAYESIAN METHOD OF LOCALLY STATIONARY AR MODEL FITTING; SCALAR CAS
    !                                                                   
    !    THIS PROGRAM LOCALLY FITS AUTOREGRESSIVE MODELS TO NON-STATIONARY 
    !    SERIES BY A BAYESIAN PROCEDURE.  POWER SPECTRA FOR STATIONARY SPAN
    !    ARE GRAPHICALLY PRINTED OUT.  (THIS PROGRAM IS TENTATIVE.)        
    !                                                                   
    !    INPUTS REQUIRED:                                                  
    !         MT:       INPUT DEVICE FOR ORIGINAL DATA (MT=5 : CARD READ
    !         LAG:      UPPER LIMIT OF THE ORDER OF AR MODEL, MUST BE LE
    !                   OR EQUAL TO 50.                                 
    !         NS:       LENGTH OF BASIC LOCAL SPAN                      
    !         KSW:      =0  CONSTANT VECTOR IS NOT INCLUDED AS A REGRESS
    !                   =1  CONSTANT VECTOR IS INCLUDED AS THE FIRST REG
    !                                                                   
    !           -- THE FOLLOWING INPUTS ARE REQUESTED BY SUBROUTINE REDA
    !         TITLE:    SPECIFICATION OF DATA                           
    !         N:        DATA LENGTH, MUST BE LESS THAN OR EQUAL TO 10000
    !         DFORM:    INPUT DATA SPECIFICATION STATEMENT.             
    !                   -- EXAMPLE  --     (8F10.5)                     
    !         (Z(I),I=1,N):  ORIGINAL DATA                              
    !           --------------------------------------------------------
    !================================================================================86
    subroutine blocarf_wrapper(zs,n,lag,ns0,kmax,zmean,sum,aic,c,b,a,sd,np,ne,sxx,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          real(R64P),  dimension(n),        intent(in)    :: zs
          integer(I32P),                    intent(in)    :: n
          integer(I32P),                    intent(in)    :: lag
          integer(I32P),                    intent(in)    :: ns0
          integer(I32P),                    intent(in)    :: kmax
          real(R64P),                       intent(inout) :: zmean
          real(R64P),                       intent(inout) :: sum
          real(R64P), dimension(kmax,kmax), intent(inout) :: aic
          real(R64P), dimension(kmax,kmax), intent(inout) :: c
          real(R64P), dimension(lag,kmax),  intent(inout) :: b
          real(R64P), dimension(lag,kmax),  intent(inout) :: a
          real(R64P), dimension(kmax),      intent(inout) :: sd
          integer(I32P), dimension(kmax),   intent(inout) :: np
          integer(I32P), dimension(kmax),   intent(inout) :: ne
          real(R64P), dimension(121,kmax),  intent(inout) :: sxx
          logical(I32P), dimension(5),      intent(inout) :: fp_flags
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$ ENDIF
          ! Exec code ....
          !Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          call BLOCARF(zs,n,lag,ns0,kmax,zmean,sum,aic,c,b,a,sd,np,ne,sxx)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " blocarf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine blocarf_wrapper
    
    !================================================================================86
    !   Wrapper around: 'BLOMARF' program.
    !   ----------------------- Original description ----------------------------
    !    BAYESIAN METHOD OF LOCALLY STATIONARY MULTIVARIATE AR MODEL FITTIN
    !                                                                   
    ! THIS PROGRAM LOCALLY FITS MULTI-VARIATE AUTOREGRESSIVE MODELS TO  
    ! NON-STATIONARY TIME SERIES BY A BAYESIAN PROCEDURE.               
    !                                                                   
    ! 
    !   THE FOLLOWING SUBROUTINES ARE DIRECTLY CALLED BY THIS PROGRAM:  
    !        MRDATA                                                    
    !         MNONSB                                                    
    !   --------------------------------------------------------------- 
    !   INPUTS REQUIRED;                                                
    !      MT:    INPUT DEVICE FOR ORIGINAL DATA (MT=5: CARD READER).   
    !      LAG:   UPPER LIMIT OF THE ORDER OF AR-MODEL, MUST BE LESS THA
    !             OR EQUAL TO 50.                                       
    !      NS:    LENGTH OF BASIC LOCAL SPAN.                           
    !      KSW:   =0  CONSTANT VECTOR IS NOT INCLUDED AS A REGRESSOR    
    !             =1  CONSTANT VECTOR IS INCLUDED AS THE FIRST REGRESSOR
    !                                                                    
    !        -- THE FOLLOWING INPUTS ARE REQUESTED BY SUBROUTINE MRDATA 
    !      TITLE: SPECIFICATION OF DATA                                 
    !      N:     DATA LENGTH, MUST BE LESS THAN OR EQUAL TO 1000.      
    !      ID:    DIMENSION OF DATA,  MUST BE LESS THAN 6               
    !                   < ID*(LAG+1)+KSW MUST BE LESS THAN 101 >        
    !      IFM:   INPUT FORMAT                                          
    !      FORM:  INPUT DATA FORMAT SPECIFICATION STATEMENT.            
    !             -- EXAMPLE --     (8F10.5)                            
    !      C(J):  CALIBRATION CONSTANT FOR CHANNEL J (J=1,ID)           
    !      Z(I,J): ORIGINAL DATA                                        
    !        -----------------------------------------------------------
    !================================================================================86
    subroutine blomarf_wrapper(zs,n,id,c,lag,ns0,kmax,zmean,zvari,bw,aic,a,  &
                               e,aicb,lks,lke,m, fp_flags                         )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          real(R64P),   dimension(n,id),          intent(in) :: zs
          integer(I32P),                          intent(in) :: n
          integer(I32P),                          intent(in) :: id
          real(R64P),   dimension(id),            intent(in) :: c
          integer(I32P),                          intent(in) :: lag
          integer(I32P),                          intent(in) :: ns0
          integer(I32P),                          intent(in) :: kmax
          real(R64P),  dimension(id),             intent(inout) :: zmean
          real(R64P),  dimension(id),             intent(inout) :: zvari
          real(R64P),  dimension(kmax,kmax),      intent(inout) :: bw
          real(R64P),  dimension(kmax,kmax),      intent(inout) :: aic
          real(R64P),  dimension(id,id,lag,kmax), intent(inout) :: a
          real(R64P),  dimension(id,id,kmax),     intent(inout) :: e
          real(R64P),  dimension(kmax),           intent(inout) :: aicb
          integer(I32P), dimension(kmax),         intent(inout) :: lks
          integer(I32P), dimension(kmax),         intent(inout) :: lke
          integer(I32P),                          intent(inout) :: m
          logical(I32P), dimension(5),            intent(inout) :: fp_flags
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1
          type(ieee_status_type)  :: status_value
!DIR$ ENDIF
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF             
          call BLOMARF(zs,n,id,c,lag,ns0,kmax,zmean,zvari,bw,aic,a,  &
                       e,aicb,lks,lke,m )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " blomarf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine blomarf_wrapper
                               
    !================================================================================86
    !    Wrapper around 'BSUBSTF' program                      
    !    --------------------- Original description --------------------------------
    ! THIS PROGRAM PRODUCES BAYESIAN ESTIMATES OF TIME SERIES MODELS SUC
    ! PURE AR MODELS, AR-MODELS WITH NON-LINEAR TERMS, AR-MODELS WITH PO
    ! TYPE MEAN VALUE FUNCTIONS, ETC.  THE GOODNESS OF FIT OF A MODEL IS
    ! CHECKED BY THE ANALYSIS OF SEVERAL STEPS AHEAD PREDICTION ERRORS. 
    ! BY PREPARING AN EXTERNAL SUBROUTINE SETX PROPERLY, ANY TIME SERIES
    ! WHICH IS LINEAR IN PARAMETERS CAN BE TREATED.                     
    !   ----------------------------------------------------------------
    !   THE FOLLOWING SUBROUTINES ARE DIRECTLY CALLED BY THIS PROGRAM:  
    !         REDATA                                                    
    !         REDLAG                                                    
    !         SETLAG                                                    
    !         REDREG                                                    
    !         REDUCT                                                    
    !        ARMFIT                                                    
    !         SBBAYS                                                    
    !         CHECK                                                     
    !   ----------------------------------------------------------------
    !                                                                   
    !   INPUTS REQUIRED:                                                
    !      MT:    ORIGINAL DATA INPUT DEVICE SPECIFICATION              
    !      IMODEL:=1  AUTOREGRESSIVE MODEL                              
    !             =2  POLYNOMIAL TYPE NON-LINEAR MODEL (LAG'S READ IN ) 
    !             =3  POLYNOMIAL TYPE NON-LINEAR MODEL (LAG'S AUTOMATICA
    !             =4  AR-MODEL WITH POLYNOMIAL MEAN VALUE FUNCTION      
    !             =5  ANY NON-LINEAR MODEL                              
    !             =6  POLYNOMIAL TYPE EXPONENTIALLY DAMPED NON-LINEAR MO
    !             =7  THIS MODEL IS RESERVED FOR THE USER'S OPTIONAL USE
    !      LAG:   MAXIMUM TIME LAG USED IN THE MODEL                    
    !      K:     NUMBER OF REGRESSORS                                  
    !      IL:    PREDICTION ERRORS CHECKING (UP TO IL-STEPS AHEAD) IS R
    !             N*IL SHOULD BE LESS THAN OR EQUAL TO 20000            
    !                                                                   
    ! --   THE FOLLOWING INPUTS ARE REQUIRED AT SUBROUTINE REDATA   --
    !                                                                   
    !      TITLE:   ORIGINAL DATA SPECIFICATION                         
    !      N:       DATA LENGTH                                         
    !      DFORM:   INPUT DATA FORMAT SPECIFICATION STATEMENT           
    !              -- EXAMPLE --  (8F10.5 )                            
    !      X(I) (I=1,N):   ORIGINAL DATA                                
    !   ----------------------------------------------------------------                       
    !================================================================================86
    subroutine bsubstf_wrapper(zs,n,imodel,lag,k,il,lg1,lg2,zmean,sum,m,aicm,sdm,a1,sd, &
                               aic,dic,aicb,sdb,ek,a2,ind,c,c1,c2,b,oeic,esum,omean,    &
                               om,e,emean,vari,skew,peak,cov,sxx,fp_flags                 )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          real(R64P),  dimension(n),        intent(in)    :: zs
          integer(I32P),                    intent(in)    :: n
          integer(I32P),                    intent(in)    :: imodel
          integer(I32P),                    intent(in)    :: lag
          integer(I32P),                    intent(in)    :: k
          integer(I32P),                    intent(in)    :: il
          integer(I32P), dimension(3,k),    intent(in)    :: lg1
          integer(I32P), dimension(5),      intent(in)    :: lg2
          real(R64P),                       intent(inout) :: zmean
          real(R64P),                       intent(inout) :: sum
          integer(I32P),                    intent(inout) :: m
          real(R64P),                       intent(inout) :: aicm
          real(R64P),                       intent(inout) :: sdm
          real(R64P), dimension(k),         intent(inout) :: a1
          real(R64P), dimension(k+1),       intent(inout) :: sd
          real(R64P), dimension(k+1),       intent(inout) :: aic
          real(R64P), dimension(k+1),       intent(inout) :: dic
          real(R64P),                       intent(inout) :: aicb
          real(R64P),                       intent(inout) :: sdb
          real(R64P),                       intent(inout) :: ek
          real(R64P), dimension(k),         intent(inout) :: a2
          integer(I32P), dimension(k),      intent(inout) :: ind
          real(R64P), dimension(k),         intent(inout) :: c
          real(R64P), dimension(k+1),       intent(inout) :: c1
          real(R64P), dimension(k),         intent(inout) :: c2
          real(R64P), dimension(k),         intent(inout) :: b
          real(R64P),                       intent(inout) :: oeic
          real(R64P), dimension(k+1),       intent(inout) :: esum
          real(R64P),                       intent(inout) :: omean
          real(R64P),                       intent(inout) :: om
          real(R64P), dimension(n,il),      intent(inout) :: e
          real(R64P), dimension(il),        intent(inout) :: emean
          real(R64P), dimension(il),        intent(inout) :: vari,skew,peak
          real(R64P), dimension(121),       intent(inout) :: cov,sxx
          logical(I32P), dimension(5),      intent(inout) :: fp_flags
          ! Locals
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF
          ! Exec code ...
          ! sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
          call BSUBSTF(zs,n,imodel,lag,k,il,lg1,lg2,zmean,sum,m,aicm,sdm,a1,  &
                       sd,aic,dic,aicb,sdb,ek,a2,ind,c,c1,c2,b,oeic,esum,     &
                       omean,om,e,emean,vari,skew,peak,cov,sxx     )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " bsubstf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF             
    end subroutine bsubstf_wrapper
                               
    !================================================================================86
    !   Wrapper around: 'CANARMF' program                          
    !   --------------------------- Original description ---------------------------
    !    THIS PROGRAM FITS AN AR-MA MODEL TO STATIONARY SCALAR TIME SERIES
    !    THROUGH THE ANALYSIS OF CANONICAL CORRELATIONS
    !    BETWEEN THE FUTURE AND PAST SETS OF OBSERVATIONS.
    !    THE OUTPUTS OF THIS PROGRAM SHOULD BE ADDED TO THE INPUTS
    !    TO THIS PROGRAM TO FORM AN INPUT TO THE PROGRAM AUTARM.
    !
    !    INPUTS REQUIRED:
    !    (N,LAGH0): N, LENGTH OF ORIGINAL DATA Y(I) (I=1,N)
    !                LAGH0, MAXIMUM LAG OF COVARIANCE
    !    CYY(I),I=0,LAGH0: AUTOCOVARIANCE SEQUENCE OF Y(I)
    !
    !    OUTPUTS:
    !    NEWL: NEWL=1, FOR DIRECT INPUT TO PROGRAM AUTARM
    !    M1M: ORDER OF AR
    !    BETA(I)(I=1,M1M): AR-COEFFICIENTS
    !    M1N: ORDER OF MA (=M1M-1)
    !    ALPHA(I)(I=1,M1N): MA-COEFFICIENTS
    !
    !    THE AR-MA MODEL IS GIVEN BY
    !    Y(N)+BETA(1)Y(N-1)+...+BETA(M1M)Y(N-M1M) = X(N)+ALPHA(1)X(N-1)+...
    !                                             ...+ALPHA(M1N)X(N-M1N)   
    !================================================================================86
    subroutine canarmf_wrapper(n,lagh3,cyy,coef,ifpl1,sd,aic,oaic,mo,a,nc,mm1,  &
                               mm2,v,z,y,xx,ndt,x3,x3min,min3,m1m,beta,m1n,alpha,mj1,mj2,fp_flags)
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$     ENDIF
          integer(I32P),                      intent(in)    :: n
          integer(I32P),                      intent(in)    :: lagh3
          real(R64P), dimension(lagh3),       intent(in)    :: cyy
          real(R64P), dimension(mj2),         intent(inout) :: coef
          integer(I32P),                      intent(in)    :: ifpl1
          real(R64P), dimension(0:mj1),       intent(inout) :: sd
          real(R64P), dimension(0:mj1),       intent(inout) :: aic
          real(R64P),                         intent(inout) :: oaic
          integer(I32P),                      intent(inout) :: mo
          real(R64P), dimension(mj1),         intent(inout) :: a
          integer(I32P),                      intent(inout) :: nc
          integer(I32P), dimension(mj1),      intent(inout) :: mm1
          integer(I32P), dimension(mj1),      intent(inout) :: mm2
          real(R64P), dimension(mj1,mj1,mj1), intent(inout) :: v
          real(R64P), dimension(mj1,mj1),     intent(inout) :: z
          real(R64P), dimension(mj1,mj1),     intent(inout) :: y
          real(R64P), dimension(mj1,mj1),     intent(inout) :: xx
          integer(I32P), dimension(mj1,mj1),  intent(inout) :: ndt
          real(R64P), dimension(mj1,mj1),     intent(inout) :: x3
          real(R64P), dimension(mj1),         intent(inout) :: x3min
          integer(I32P), dimension(mj1),      intent(inout) :: min3
          integer(I32P),                      intent(inout) :: m1m
          real(R64P), dimension(mj1),         intent(inout) :: beta
          integer(I32P),                      intent(inout) :: m1n
          real(R64P), dimension(mj1),         intent(inout) :: alpha
          integer(I32P),                      intent(in)    :: mj1,mj2
          logical(I32P), dimension(5),        intent(inout) :: fp_flags
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)   :: status_value
!DIR$     ENDIF
          ! Exec code ...
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          call CANARMF(n,lagh3,cyy,coef,ifpl1,sd,aic,oaic,mo,a,nc,mm1,   &
                       mm2,v,z,y,xx,ndt,x3,x3min,min3,m1m,beta,m1n,alpha,mj1,mj2)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " canarmf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
          
    end subroutine canarmf_wrapper
                               
    !================================================================================86                           
    !   Wrapper around: 'CANOCAF'   program                      
    !   --------------------- Original description -------------------------------
    ! THIS PROGRAM DOES CANONICAL CORRELATION ANALYSIS OF AN IR-DIMENSIO
    ! MULTIVARIATE TIME SERIES Y(I) (I=1,N).
    !
    ! FIRST AR-MODEL IS FITTED BY THE MINIMUM  A I C  PROCEDURE.
    ! THE RESULTS ARE USED TO ORTHO-NORMALIZE THE PRESENT AND PAST VARIA
    ! THE PRESENT AND FUTURE VARIABLES ARE TESTED SUCCESSIVELY TO DECIDE
    ! ON THE DEPENDENCE OF THEIR PREDICTORS. WHEN THE LAST DIC (AN INFOR
    ! CRITERION) IS NEGATIVE THE PREDICTOR OF THE VARIABLE IS DECIDED
    ! TO BE LINEARLY DEPENDENT ON THE ANTECEDENTS. 
    !  THE STRUCTURAL CHARACTERISTIC VECTOR H OF THE CANONICAL MARKOVIAN
    ! REPRESENTATION AND THE ESTIMATE OF THE TRANSITION MATRIX F, IN
    ! VECTOR FORM, ARE PUNCHED OUT. THE ESTIMATE OF THE INPUT MATRIX G A
    ! THE COVARIANCE MATRIX C OF THE INNOVATION, OBTAINED BY USING
    ! THE F-MATRIX AND THE AR-MODEL, ARE ALSO PUNCHED OUT.
    !================================================================================86 
    subroutine canocaf_wrapper(ir,inw,n,lagh1,ip0,ccv,l,aic,oaic,mo,osd,aao,nc,n1,n2,  &
                               vv,z,y,xx,ndt,x3,x3min,min3,f,m1nh,nh,g,iaw,vf,lmax,mj0,mj1,fp_flags )
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$     ENDIF    
          integer(I32P),                        intent(in) :: ir
          integer(I32P), dimension(ir),         intent(in) :: inw
          integer(I32P),                        intent(in) :: n
          integer(I32P),                        intent(in) :: lagh1
          integer(I32P),                        intent(in) :: ip0
          real(R64P), dimension(lagh1,ip0,ip0), intent(in) :: ccv
          integer(I32P),                        intent(inout) :: l
          real(R64P), dimension(mj0),           intent(inout) :: aic
          real(R64P),                           intent(inout) :: oaic
          integer(I32P),                        intent(inout) :: mo
          real(R64P), dimension(ir,ir),         intent(inout) :: osd
          real(R64P), dimension(mj0,ir,ir),     intent(inout) :: aao
          integer(I32P),                        intent(inout) :: nc
          integer(I32P), dimension(mj1),        intent(inout) :: n1
          integer(I32P), dimension(mj1),        intent(inout) :: n2
          real(R64P), dimension(mj1,mj1,mj1),   intent(inout) :: vv
          real(R64P), dimension(mj1,mj1),       intent(inout) :: z
          real(R64P), dimension(mj1,mj1),       intent(inout) :: y
          real(R64P), dimension(mj1,mj1),       intent(inout) :: xx
          integer(I32P), dimension(mj1,mj1),    intent(inout) :: ndt
          real(R64P),    dimension(mj1,mj1),    intent(inout) :: x3
          real(R64P),    dimension(mj0*ir),     intent(inout) :: x3min
          real(R64P),    dimension(mj1,mj1),    intent(inout) :: f
          integer(I32P),                        intent(inout) :: m1nh
          integer(I32P), dimension(mj1),        intent(inout) :: nh
          real(R64P),    dimension(mj1,ir),     intent(inout) :: g
          integer(I32P),                        intent(inout) :: iaw
          real(R64P), dimension(mj1*mj1),       intent(inout) :: vf
          integer(I32P),                        intent(in)    :: lmax,mj0,mj1
          logical(I32P), dimension(5),          intent(inout) :: fp_flags
!DIR$     IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)    :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
          call CANOCAF(ir,inw,n,lagh1,ip0,ccv,l,aic,oaic,mo,osd,aao,nc,n1,n2,  &
                       vv,z,y,xx,ndt,x3,x3min,f,m1nh,nh,g,iaw,vf,lmax,mj0,mj1  )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " canocaf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF          
    end subroutine canocaf_wrapper
                               
    !================================================================================86
    !    Wrapper around: 'COVGENF' program                       
    !    --------------------------- Original description -----------------------------
    !     THIS PROGRAM PRODUCES THE FOURIER TRANSFORM OF A POWER
    !     GAIN FUNCTION IN THE FORM OF AN AUTOCOVARIANCE SEQUENCE.
    !     THE GAIN FUNCTION IS DEFINED AS A RECTILINEAR FUNCTION WITH
    !     THE VALUES G(I) SPECIFIED AT THE FREQUENCIES F(I),I=1,K.
    !     THE OUTPUTS OF THIS PROGRAM ARE USED AS THE INPUTS TO THE CANONICA
    !     CORRELATION ANALYSIS PROGRAM CANARM, TO REALIZE A FILTER WITH
    !     THE DESIRED GAIN FUNCTION.
    !
    !     THE FOLLOWING INPUTS ARE REQUIRED:
    !       (L,K): L, DESIRED MAXIMUM LAG OF COVARIANCE (AT MOST 1024)
    !              K, NUMBER OF DATA POINTS (LESS THAN OR EQUAL TO 500)
    !       (F(I),G(I))(I=1,K): F(I), FREQUENCY. BY DEFINITION F(1)=0.0 AND F(
    !                       F(I)'S ARE ARRANGED IN INCREASING ORDER.
    !                       G(I), POWER GAIN OF THE FILTER AT THE FREQUEN
    !
    !    OUTPUTS:
    !        (N,LAGH): N=2048
    !        C(I)(I=0,LAGH): 
    !================================================================================86
    subroutine covgenf_wrapper(l,k,f,g,c,cn,fp_flags)
!DIR$     IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$     ENDIF
          integer(I32P),               intent(in)    :: l
          integer(I32P),               intent(in)    :: k
          real(R64P), dimension(k),    intent(in)    :: f
          real(R64P), dimension(k),    intent(in)    :: g
          real(R64P), dimension(l+1),  intent(inout) :: c
          real(R64P), dimension(l+1),  intent(inout) :: cn
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
!DIR$     IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF            
          call COVGENF(l,k,f,g,c,cn)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " covgenf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF            
    end subroutine covgenf_wrapper
                               
    !================================================================================86
    !     Wrapper around: 'DECOMPF'
    !     ------------------------ Original description ----------------------
    !      ...  TIME SERIES DECOMPOSITION (SEASONAL ADJUSTMENT) ...             
    !                                                                   
    !       THE BASIC MODEL:                                                  
    !                                                                   
    !            y(n) = T(n) + AR(n) + S(n) + TD(n) + R(n) + W(n)             
    !                                                                   
    !        where                                                           
    !            T(n):       trend component                                
    !            AR(n):      AR process                                     
    !            S(n):       seasonal component                             
    !            TD(n):      trading day factor                             
    !            R(n):       any other explanetory variables                
    !            W(n):       observational noise                            
    !                                                                   
    !       COMPONENT MODELS:                                                 
    !                                                                   
    !        Trend component                                                 
    !             T(N) =  T(N-1) + V1(N)                             :M1 = 1 
    !             T(N) = 2T(N-1) - T(N-2) + V1(N)                    :M1 = 2 
    !             T(N) = 3T(N-1) -3T(N-2) + T(N-2) + V1(N)           :M1 = 3 
    !                                                                   
    !       AR componet:                                                    
    !            AR(n) = a(1)AR(n-1) + ... + a(m2)AR(n-m2) + V2(n)          
    !                                                                   
    !       Seasonal component:                                             
    !        S(N) =  -S(N-1) - ... - S(N-PERIOD+1) + V3(N)    :SORDER=1 
    !        S(N) = -2S(N-1) - ... -PERIOB*S(N-PERIOD+1)      :SORDER=2 
    !                        - ... - S(n-2PERIOD+2) + V3(n)             
    !       Trading day effect:                                             
    !          TD(n) = b(1)*TRADE(n,1) + ... + b(7)*TRADE(n,7)            
    !                                                                   
    !       TRADE(n,i):  number of i-th days of the week in n-th data  
    !          b(1) + ... + b(7) = 0                                      
    !================================================================================86
    subroutine decompf_wrapper(datum,n,ipar,trend,seasnl,ar,trad,noise,para,imiss,omaxx,ier,fp_flags)
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$     ENDIF
          real(R64P),   dimension(n),       intent(in)    :: datum
          integer(I32P),                    intent(in)    :: n
          integer(I32P), dimension(9),      intent(in)    :: ipar
          real(R64P), dimension(n),         intent(inout) :: trend
          real(R64P), dimension(n),         intent(inout) :: seasnl
          real(R64P), dimension(n),         intent(inout) :: ar
          real(R64P), dimension(n),         intent(inout) :: trad
          real(R64P), dimension(n),         intent(inout) :: noise
          real(R64P), dimension(26),        intent(inout) :: para
          integer(I32P),                    intent(in)    :: imiss
          real(R64P),                       intent(in)    :: omaxx
          integer(I32P),                    intent(inout) :: ier
          logical(I32P), dimension(5),      intent(inout) :: fp_flags
!DIR$     IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF
          ! Exec code .....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          call DECOMPF(datum,n,ipar,trend,seasnl,ar,trad,noise,para,imiss,omaxx,ier)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " decompf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF          
    end subroutine decompf_wrapper
    
    !================================================================================86
    !    Wrapper around: 'EXSARF'  program
    !    ----------------------- Original description ----------------------------
    !   EXACT MAXIMUM LIKELIHOOD METHOD OF SCALAR AR-MODEL FITTING        
    !                                                                   
    !   THIS PROGRAM PRODUCES EXACT MAXIMUM LIKELIHOOD ESTIMATES OF THE   
    !   PARAMETERS OF A SCALAR AR-MODEL.                                  
    !                                                                   
    !   THE AR-MODEL IS GIVEN BY                                          
    !                                                                   
    !           Z(I) = A(1)*Z(I-1) + ... + A(K)*Z(I-K) + E(I)           
    !                                                                   
    !   WHERE E(I) IS A ZERO MEAN WHITE NOISE.                            
    !                                                                   
    ! --------------------------------------------------------------    
    !   THE FOLLOWING SUBROUTINES ARE DIRECTLY CALLED BY THIS PROGRAM:    
    !         REDATA                                                    
    !         REDUCT                                                    
    !         ARMFIT                                                    
    !         RECOEF                                                    
    !         ARMLE                                                     
    !         PRINTA                                                    
    ! --------------------------------------------------------------    
    !   INPUTS REQUIRED:                                                  
    !      MT:      INPUT DEVICE SPECIFICATION (MT=5: CARD READER)      
    !      LAG:     UPPER LIMIT OF AR-ORDER, MUST BE LESS THAN 51       
    !                                                                   
    ! --  THE FOLLOWING INPUTS ARE REQUESTED BY SUBROUTINE REDATA  --   
    !      TITLE:  TITLE OF DATA                                        
    !      N:      DATA LENGTH, MUST BE LESS THAN OR EQUAL TO 10000     
    !      DFORM:  INPUT DATA FORMAT SPECIFICATION STATEMENT            
    !              -- EXAMPLE --     (8F10.5)                           
    !      (Z(I),I=1,N):  ORIGINAL DATA                      
    !================================================================================86
    subroutine exsarf_wrapper(z1,n,lag,zmean,sum,sd,aic,dic,m1,amin,sdm1,a1,sdm2,a2,jer,fp_flags)
!DIR$     IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$     ENDIF
          real(R64P), dimension(n),         intent(in)    :: z1
          integer(I32P),                    intent(in)    :: n
          integer(I32P),                    intent(in)    :: lag
          real(R64P),                       intent(inout) :: zmean
          real(R64P),                       intent(inout) :: sum
          real(R64P), dimension(lag+1),     intent(inout) :: sd
          real(R64P), dimension(lag+1),     intent(inout) :: aic
          real(R64P), dimension(lag+1),     intent(inout) :: dic
          integer(I32P),                    intent(inout) :: m1
          real(R64P),                       intent(inout) :: amin
          real(R64P),                       intent(inout) :: sdm1
          real(R64P), dimension(lag),       intent(inout) :: a1
          real(R64P),                       intent(inout) :: sdm2
          real(R64P), dimension(lag),       intent(inout) :: a2
          integer(I32P),                    intent(inout) :: jer
          logical(I32P), dimension(5),      intent(inout) :: fp_flags
!DIR$     IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
          call EXSARF(z1,n,lag,zmean,sum,sd,aic,dic,m1,amin,sdm1,a1,sdm2,a2,jer)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " exsarf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
    
    !================================================================================86
    !   Wrapper around: 'FFTCORF'  program
    !   ---------------------- Original description ----------------------------
    !    THIS PROGRAM COMPUTES AUTO AND/OR CROSS
    !    COVARIANCES AND CORRELATIONS VIA FFT.
    !    IT REQUIRES FOLLOWING INPUTS:
    !    ISW: ISW=1...AUTO CORRELATION OF X (ONE-CHANNEL)
    !         ISW=2...AUTO CORRELATIONS OF X AND Y (TWO-CHANNEL)
    !         ISW=4...AUTO,CROSS CORRELATIONS OF X AND Y (TWO-CHANNEL)
    !    LD: LENGTH OF DATA
    !    LAGH: MAXIMUM LAG
    !    DFORM: INPUT FORMAT SPECIFICATION STATEMENT IN ONE CARD,
    !    FOR EXAMPLE
    !    (8F10.4)
    !    (X(I); I=1,LD): DATA OF CHANNEL X
    !    (Y(I); I=1,LD): DATA OF CHANNEL Y (FOR ISW=2 OR 4 ONLY)
    !================================================================================86
    subroutine  fftcorf_wrapper(ld,lagh1,n,n2p,isw,x1,y1,xa,x,y,cna1,cn1,cn2,amean,fp_flags)
!DIR$  IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
       use, intrinsic :: ieee_exceptions
!DIR$  ENDIF
       integer(I32P),                  intent(in)    :: ld
       integer(I32P),                  intent(in)    :: lagh1
       integer(I32P),                  intent(in)    :: n
       integer(I32P),                  intent(in)    :: n2p
       integer(I32P),                  intent(in)    :: isw
       real(R64P), dimension(ld),      intent(in)    :: x1
       real(R64P), dimension(ld),      intent(in)    :: y1
       real(R64P), dimension(n,2),     intent(inout) :: xa
       real(R64P), dimension(n),       intent(inout) :: x
       real(R64P), dimension(n),       intent(inout) :: y
       real(R64P), dimension(lagh1,2), intent(inout) :: cna1
       real(R64P), dimension(lagh1),   intent(inout) :: cn1
       real(R64P), dimension(lagh1),   intent(inout) :: cn2
       real(R64P), dimension(2),       intent(inout) :: amean
       logical(I32P), dimension(5),    intent(inout) :: fp_flags
       ! Locals
!DIR$  IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
       type(ieee_status_type)  :: status_value
!DIR$  ENDIF
       ! Exec code ....
       ! Sanity  check
       if(ANY(fp_flags)) then
           fp_flags = .false.
       end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
          call  FFTCORF(ld,lagh1,n,n2p,isw,x1,y1,xa,x,y,cna1,cn1,cn2,amean)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " fftcorf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF  
    end subroutine fftcorf_wrapper
    
    !================================================================================86
    !   Wrapper around: 'FPEAUTF' program
    !   ---------------------- Original description ----------------------------
    !   THIS PROGRAM PERFORMS FPE(FINAL PREDICTION ERROR) COMPUTATION FOR
    !   ONE-DIMENSIONAL AR-MODEL. A CARD CONTAINING THE FOLLOWING
    !   !INFORMATION OF L, UPPER LIMIT OF MODEL ORDER, SHOULD BE ADDED ON
    !   TOP OF THE OUTPUT OF PROGRAM 5.1.1 AUTCOR TO FORM THE INPUT TO
    !   THIS PROGRAM.
    !   CXX(0) IS READ AS INITIAL SD.
    !   THE OUTPUTS ARE THE COEFFICIENTS A(I) OF AR-PROCESS
    !   X(N)=A(1)X(N-1)+...+A(M)X(N-M)+E(N)
    !   AND THE VARIANCE SIGMA**2 OF E(N).
    !   CHI**2 SHOWS THE SIGNIFICANCE OF PARCOR=A(M) AS A CHI-SQUARED
    !   VARIABLE WITH D.F.=1.
    !================================================================================86
    subroutine fpeautf_wrapper(l,n,sd,cxx,ssd,fpe,rfpe,d,chi2,ofpe1,ofpe2,orfpe,mo,osd,a,ao,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)    
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          integer(I32P),              intent(in)    :: l
          integer(I32P),              intent(in)    :: n
          real(R64P), dimension(l),   intent(in)    :: sd
          real(R64P), dimension(l),   intent(in)    :: cxx
          real(R64P), dimension(l),   intent(inout) :: ssd
          real(R64P), dimension(l),   intent(inout) :: fpe
          real(R64P), dimension(l),   intent(inout) :: rfpe
          real(R64P), dimension(l),   intent(inout) :: d
          real(R64P), dimension(l),   intent(inout) :: chi2
          real(R64P),                 intent(inout) :: ofpe1,ofpe2,orfpe
          integer(I32P),              intent(inout) :: mo
          real(R64P),                 intent(inout) :: osd
          real(R64P), dimension(l,l), intent(inout) :: a
          real(R64P), dimension(l),   intent(inout) :: ao
          logical(I32P), dimension(5),intent(inout) :: fp_flags
          ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          call FPEAUTF(l,n,sd,cxx,ssd,fpe,rfpe,d,chi2,ofpe1,ofpe2,orfpe,mo,osd,a,ao)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " fpeautf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
    
    !================================================================================86
    !   Wrapper around: 'FPEC7F' program
    !   ----------------------- Original description -----------------------------
    !    THIS PROGRAM PERFORMS FPEC(AR-MODEL FITTING FOR CONTROL)
    !    COMPUTATION.
    !    BESIDES THE OUTPUTS OF PROGRAM 5.1.2   MULCOR, THE FOLLOWING
    !    INPUTS ARE REQUIRED:
    !    L: UPPER LIMIT OF MODEL ORDER M (LESS THAN 30)
    !    IR: NUMBER OF CONTROLLED VARIABLES
    !    IL: NUMBER OF MANINPULATED VARIABLES, IL=0 FOR MFPE COMPUTATION
    !    INW(I): INDICATOR; FIRST IR INDICATE THE CONTROLLED VARIABLES
    !            AND THE REST THE MANIPULATE VARIABLES WITHIN THE IP0 VARIABLES
    !   IN THE OUTPUT OF PROGRAM 5.1.2   MULCOR.
    !   THE OUTPUTS ARE THE PREDICTION ERROR COVARIANCE MATRIX OSD AND
    !   THE SET OF COEFFICIENT MATRICES A AND B TO BE USED IN
    !   PROGRAM 5.5.1   OPTIMAL CONTROLLER DESIGN.
    !================================================================================86
    subroutine fpec7f_wrapper(n,l,ir,ip,ip0,inw,r1,r2,fpec,rfpec,aic,ifpec,ofpec,orfpec,oaic,osd,ao,fp_flags)
!DIR$   IF(USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
        use, intrinsic :: ieee_exceptions
!DIR$   ENDIF
        integer(I32P),                         intent(in)    :: n
        integer(I32P),                         intent(in)    :: l
        integer(I32P),                         intent(in)    :: ir
        integer(I32P),                         intent(in)    :: ip
        integer(I32P),                         intent(in)    :: ip0
        integer(I32P), dimension(ip),          intent(in)    :: inw
        real(R64P),    dimension(l+1,ip0,ip0), intent(in)    :: r1
        real(R64P),    dimension(l+1,ip,ip),   intent(inout) :: r2
        real(R64P),    dimension(0:l),         intent(inout) :: fpec
        real(R64P),    dimension(0:l),         intent(inout) :: rfpec
        real(R64P),    dimension(0:l),         intent(inout) :: aic
        integer(I32P),                         intent(inout) :: ifpec
        real(R64P),                            intent(inout) :: ofpec,orfpec,oaic
        real(R64P),    dimension(ir,ir),       intent(inout) :: sd
        real(R64P),    dimension(l,ir,ip),     intent(inout) :: ao
        logical(I32P), dimension(5),           intent(inout) :: fp_flags
        ! Locals
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
        type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Exec code ....
          ! Sanity check
        if(ANY(fp_flags)) then
              fp_flags = .false.
        end if
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
        call ieee_get_status(status_value)
        call ieee_set_halting_mode(ieee_all,.false.)
        call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
        call FPEC7F(n,l,ir,ip,ip0,inw,r1,r2,fpec,rfpec,aic,ifpec,ofpec,orfpec,oaic,sd,ao)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " fpec7f_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF             
    end subroutine
    
    !================================================================================86
    !    Wrapper around: 'MARKOVF' program
    !    ----------------------- Original description -------------------------------
    !     IN THIS PROGRAM THE MATRICES A,B,C, STAND FOR TRANSITION MATRIX (F
    ! INPUT MATRIX (G), OUTPUT MATRIX (H), RESPECTIVELY.
    !
    ! THE INPUTS REQUIRED ARE AS FOLLOWS:
    ! (N,LAGH0,ID0):
    !     N, LENGTH OF ORIGINAL DATA
    !     LAGH0, MAXIMUM LAG OF COVARIANCE
    !     ID0, DIMENSION OF Y(I)
    ! (CYY(I)(I=0,LAGH0): COVARIANCE MATRIX SEQUENCE OF Y(I). CYY(I) ARE
    ! !                      THE OUTPUTS OF THE PROGRAM MULCOR OF TIMSAC AND
    !                       ARE USED AS THE INPUT TO THE PROGRAM CANOCA OF
    !                       TIMSAC-74. FOR THE CONSTRUCTION OF CYY(I),
    !                       SEE PROGRAM CANOCA.
    ! THE OUTPUTS OF PROGRAM CANACA:
    !      (ID,K):
    !          ID, DIMENSION OF THE TIME SERIES Y(I) (NOT GREATER THAN 5)
    !          K, DIMENSION OF THE STATE VECTOR (NOT GREATER THAN 10)
    !      (NH(I))(I=1,K): STRUCTURAL CHARACTERISTIC VECTOR
    !      (AW(I))(I=1,IAW): INITIAL ESTIMATE OF THE VECTOR OF FREE
    !                           PARAMETERS IN F (=A)
    !      B(I,J)(I=1,ID+1;J=1,ID): INITIAL ESTIMATES OF THE FREE
    !                                   PARAMETERS IN G (=B)
    ! ICONT: OUTPUT CONTROL
    !      = 0, FOR AR-MA COEFFICIENTS
    !      = 1, FOR SIMCON INPUT
    !      = 2, FOR BOTH
    !
    ! THE OUTPUTS OF THIS PROGRAM ARE;
    ! THE MAXIMUM LIKELIHOOD ESTIMATES
    !      (ID,K):
    !      (NH(I))(I=1,K):
    !      (AW(I))(I=1,IAW):
    !      (B(I,J))(I=1,ID+1;J=1,ID):
    ! AND THE OUTPUTS TO BE USED BY THE PROGRAMS PRDCTR AND SIMCON
    !      (ID,Q,Q-1):
    !      (B(I,J,L))(I,J=1,ID,L=1,Q): AR-COEFFICIENT MATRICES
    !      AND, WHEN ICONT=1 OR 2,
    !     (W(I,J,L))(I,J=1,ID,L=1,Q-1): IMPULSE RESPONSE MATRICES
    !      AND, WHEN ICONT=0 OR 1,
    !      (A(I,J,L))(I,J=1,ID;L=1,Q-1): MA-COEFFICIENT MATRICES
    ! AND
    !      (C0(I,J))(I,J=1,ID): INNOVATION COVARIANCE.
    !================================================================================86
    subroutine markovf_wrapper(n,lagh3,id,cyy0,k,nh,jaw,aw1,b1,icont,idd,ir,ij,  &
                               ik,ipq,g,a1,a,b,vd,iqm,bm,au,zz,c0,aicd,mj3,mj4,mj6,mj7,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF 
          integer(I32P),                      intent(in)    :: n
          integer(I32P),                      intent(in)    :: lagh3
          integer(I32P),                      intent(in)    :: id
          real(R64P), dimension(lagh3,id,id), intent(in)    :: cyy0
          integer(I32P),                      intent(in)    :: k
          integer(I32P), dimension(k),        intent(in)    :: nh
          integer(I32P),                      intent(in)    :: jaw
          real(R64P), dimension(jaw),         intent(in)    :: aw1
          real(R64P), dimension(k,id),        intent(in)    :: b1
          integer(I32P),                      intent(in)    :: icont
          integer(I32P), dimension(k),        intent(inout) :: idd
          integer(I32P), dimension(k),        intent(inout) :: ir
          integer(I32P), dimension(k),        intent(inout) :: ij
          integer(I32P), dimension(k),        intent(inout) :: ik
          integer(I32P),                      intent(inout) :: ipq
          real(R64P), dimension(mj4),         intent(inout) :: g
          real(R64P), dimension(k,k),         intent(inout) :: a1
          real(R64P), dimension(k,k),         intent(inout) :: a
          real(R64P), dimension(k,id),        intent(inout) :: b
          real(R64P), dimension(mj4,mj4),     intent(inout) :: vd
          integer(I32P),                      intent(inout) :: iqm
          real(R64P), dimension(id,id,mj6),   intent(inout) :: bm
          real(R64P), dimension(id,id,mj7),   intent(inout) :: au
          real(R64P), dimension(id,id,mj7),   intent(inout) :: zz
          real(R64P), dimension(id,id),       intent(inout) :: c0
          real(R64P),                         intent(inout) :: aicd
          integer(I32P),                      intent(in)    :: mj3,mj4,mj6,mj7
          logical(I32P), dimension(5),        intent(inout) :: fp_flags
          ! lLocals
!DIR$ IF( USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$ ENDIF
          ! Exec code .....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
        call ieee_get_status(status_value)
        call ieee_set_halting_mode(ieee_all,.false.)
        call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
        call MARKOVF(n,lagh3,id,cyy0,k,nh,jaw,aw1,b1,icont,idd,ir,ij,ik,ipq,  &
                     g,a1,a,b,vd,iqm,bm,au,zz,c0,aicd,mj3,mj4,mj6,mj7    )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " markovf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF         
    end subroutine markovf_wrapper
                               
    subroutine mlocarf_wrapper(zs,n,lag,ns0,ksw,nml,zmean,sum,a,mf,sdf,lk0,lk2,sxx, &
                               nnf,nns,ms,sdms,aics,mp,sdmp,aicp,fp_flags      )
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          real(R64P),   dimension(n),         intent(in)    :: zs
          integer(I32P),                      intent(in)    :: n
          integer(I32P),                      intent(in)    :: lag
          integer(I32P),                      intent(in)    :: ns0
          integer(I32P),                      intent(in)    :: ksw
          integer(I32P),                      intent(in)    :: nml
          real(R64P),                         intent(inout) :: zmean
          real(R64P),                         intent(inout) :: sum
          real(R64P), dimension(lag+ksw,nml), intent(inout) :: a
          integer(I32P), dimension(nml),      intent(inout) :: mf
          real(R64P),    dimension(nml),      intent(inout) :: sdf
          integer(I32P), dimension(nml),      intent(inout) :: lk0
          integer(I32P), dimension(nml),      intent(inout) :: lk2
          real(R64P),    dimension(121,nml),  intent(inout) :: sxx
          integer(I32P), dimension(nml),      intent(inout) :: nnf
          integer(I32P), dimension(nml),      intent(inout) :: nns
          integer(I32P), dimension(nml),      intent(inout) :: ms
          real(R64P),    dimension(nml),      intent(inout) :: sdms
          real(R64P),    dimension(nml),      intent(inout) :: aics
          integer(I32P), dimension(nml),      intent(inout) :: mp
          real(R64P),    dimension(nml),      intent(inout) :: sdmp
          real(R64P),    dimension(nml),      intent(inout) :: aicp
          logical(I32P), dimension(5),        intent(inout) :: fp_flags
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Exec code ....
          ! Sanity check
        if(ANY(fp_flags)) then
              fp_flags = .false.
        end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
        call ieee_get_status(status_value)
        call ieee_set_halting_mode(ieee_all,.false.)
        call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
        call MLOCARF(zs,n,lag,ns0,ksw,nml,zmean,sum,a,mf,sdf,lk0,lk2,sxx,  &
                     nnf,nns,ms,sdms,aics,mp,sdmp,aicp     )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
        call ieee_get_flag(ieee_all,fp_flags)
        if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " mlocarf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
        end if
        call ieee_set_status(status_value)
!DIR$ ENDIF   
    end subroutine mlocarf_wrapper
                               
    !================================================================================86
    !    Wrapper around: 'MLOMARF'  program                     
    !    ------------------------ Original description ---------------------------------
    !     MINIMUM AIC METHOD OF LOCALLY STATIONARY MULTIVARIATE AR MODEL FIT
    !                                                                   
    !     THIS PROGRAM LOCALLY FITS MULTI-VARIATE AUTOREGRESSIVE MODELS TO  
    !     NON-STATIONARY TIME SERIES BY THE MINIMUM AIC PROCEDURE USING THE 
    !     HOUSEHOLDER TRANSFORMATION.                                       
    !                                                                   
    !     BY THIS PROCEDURE, THE DATA OF LENGTH N ARE DIVIDED INTO J LOCALLY
    !     STATIONARY SPANS                                                  
    !                                                                   
    !            <-- N1 --> <-- N2 --> <-- N3 -->          <-- NJ -->   
    !           !----------!----------!----------!--------!----------!  
    !            <-----------------------  N  ---------------------->   
    !                                                                   
    !     WHERE NI (I=1,...,J) DENOTES THE NUMBER OF BASIC SPANS, EACH OF   
    !     LENGTH NS, WHICH CONSTITUTE THE I-TH LOCALLY STATIONARY SPAN.     
    !     AT EACH LOCAL SPAN, THE PROCESS IS REPRESENTED BY A STATIONARY    
    !     AUTOREGRESSIVE MODEL.
    !================================================================================86
    subroutine mlomarf_wrapper(zs,n,id,c,lag,ns0,ksw,k,zmean,zvari,nf,ns,ms,aic,mp,  &
                               aicp,mf,aicf,a,e,lk0,lke,m,fp_flags       )
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          real(R64P),   dimension(n,id),           intent(in)    :: zs
          integer(I32P),                           intent(in)    :: n
          integer(I32P),                           intent(in)    :: id
          real(R64P),   dimension(id),             intent(in)    :: c
          integer(I32P),                           intent(in)    :: lag
          integer(I32P),                           intent(in)    :: ns0
          integer(I32P),                           intent(in)    :: ksw
          integer(I32P),                           intent(in)    :: k
          real(R64P),      dimension(id),          intent(inout) :: zmean
          real(R64P),      dimension(id),          intent(inout) :: zvari
          integer(I32P),   dimension(k),           intent(inout) :: nf
          integer(I32P),   dimension(k),           intent(inout) :: ns
          integer(I32P),   dimension(k),           intent(inout) :: ms
          real(R64P),      dimension(k),           intent(inout) :: aic
          integer(I32P),   dimension(k),           intent(inout) :: mp
          real(R64P),      dimension(k),           intent(inout) :: aicp
          integer(I32P),   dimension(k),           intent(inout) :: mf
          real(R64P),      dimension(k),           intent(inout) :: aicf
          real(R64P),      dimension(id,id,lag,k), intent(inout) :: a
          real(R64P),      dimension(id,id,k),     intent(inout) :: e
          integer(I32P),   dimension(k),           intent(inout) :: lk0
          integer(I32P),   dimension(k),           intent(inout) :: lke
          integer(I32P),                           intent(inout) :: m
          logical(I32P),   dimension(5),           intent(inout) :: fp_flags
          ! Locals
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
          call MLOMARF(zs,n,id,c,lag,ns0,ksw,k,zmean,zvari,nf,ns,ms,aic,  &
                       mp,aicp,mf,aicf,a,e,lk0,lke,m            )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " mlomarf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF  
          
    end subroutine mlomarf_wrapper
                               
    !================================================================================86
    !    Wrapper around: 'MULBARF' program                       
    !    ----------------------- Original description -------------------------------
    !     MULTIVARIATE BAYESIAN METHOD OF AR MODEL FITTING                  
    !                                                                   
    ! THIS PROGRAM DETERMINES MULTI-VARIATE AUTOREGRESSIVE MODELS BY A  
    ! BAYESIAN PROCEDURE.  THE BASIC LEAST SQUARES ESTIMATES OF THE PARA
    ! ARE OBTAINED BY THE HOUSEHOLDER TRANSFORMATION.                   
    !                                                                   
    ! THE STATISTIC AIC IS DEFINED BY                                   
    !                                                                  
    !        AIC  =  N * LOG( DET(SD) ) + 2 * (NUMBER OF PARAMETERS)    
    !                                                                   
    !   WHERE                                                           
    !       N:    NUMBER OF DATA,                                       
    !       SD:   ESTIMATE OF INNOVATION VARIANCE MATRIX                
    !       DET:  DETERMINANT,                                          
    !       K:    NUMBER OF FREE PARAMETERS.                            
    !                                                                   
    ! BAYESIAN WEIGHT OF THE M-TH ORDER MODEL IS DEFINED BY             
    !     W(M)  = CONST * C(M) / (M+1)                                  
    ! WHERE                                                             
    !     CONST = NORMALIZING CONSTANT                                  
    !     C(M)  = EXP( -0.5*AIC(M) ).                                   
    ! THE BAYESIAN ESTIMATES OF PARTIAL AUTOREGRESSION COEFFICIENT MATRI
    ! OF FORWARD AND BACKWARD MODELS ARE OBTAINED BY (M=1,...,LAG)      
    !     G(M)  = G(M)*D(M)                                             
    !     H(M)  = H(M)*D(M),                                            
    ! WHERE THE ORIGINAL G(M) AND H(M) ARE THE (CONDITIONAL) MAXIMUM    
    ! LIKELIHOOD ESTIMATES OF THE HIGHEST ORDER COEFFICIENT MATRICES OF 
    ! FORWARD AND BACKWARD AR MODELS OF ORDER M AND D(M) IS DEFINED BY  
    !     D(M)  = W(M) + ... + W(LAG).                                  
    !                                                                   
    ! THE EQUIVALENT NUMBER OF PARAMETERS FOR THE BAYESIAN MODEL IS     
    ! !DEFINED BY                                                        
    !     EK = (D(1)**2 + ... + D(LAG)**2)*ID + ID*(ID+1)/2             
    ! WHERE ID DENOTES DIMENSION OF THE PROCESS.     
    !================================================================================86                           
    subroutine mulbarf_wrapper(zs,n,id,c,lag,zmean,zvari,sd,aic,dic,imin,aicm,sdmin, &
                               bw1,bw2,a,b,g,h,e,aicb,fp_flags                      )
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          real(R64P),   dimension(n,id),      intent(in)    :: zs
          integer(I32P),                      intent(in)    :: n
          integer(I32P),                      intent(in)    :: id
          real(R64P),   dimension(id),        intent(in)    :: c
          integer(I32P),                      intent(in)    :: lag
          real(R64P),   dimension(id),        intent(inout) :: zmean
          real(R64P),   dimension(id),        intent(inout) :: zvari
          real(R64P),   dimension(lag+1),     intent(inout) :: sd
          real(R64P),   dimension(lag+1),     intent(inout) :: aic
          real(R64P),   dimension(lag+1),     intent(inout) :: dic
          integer(I32P),                      intent(inout) :: imin
          real(R64P),                         intent(inout) :: aicm,sdmin
          real(R64P),   dimension(lag+1),     intent(inout) :: bw1
          real(R64P),   dimension(lag),       intent(inout) :: bw2
          real(R64P),   dimension(id,id,lag), intent(inout) :: a
          real(R64P),   dimension(id,id,lag), intent(inout) :: b
          real(R64P),   dimension(id,id,lag), intent(inout) :: g
          real(R64P),   dimension(id,id,lag), intent(inout) :: h
          real(R64P),   dimension(id,id),     intent(inout) :: e
          real(R64P),                         intent(inout) :: aicm
          logical(I32P),dimension(5),         intent(inout) :: fp_flags
          ! Locals
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF
          ! Exec code ...
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
          call MULBARF(zs,n,id,c,lag,zmean,zvari,sd,aic,dic,imin,aicm,sdmin, &
                       bw1,bw2,a,b,g,h,e,aicb   )
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " mulbarf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
                               
    !================================================================================86                           
    !    Wrapper around: 'MULCORF' program                       
    !    ---------------------------- Original description ----------------------------
    !    PROGRAM 5.1.2   MULTIPLE CORRELATION
    !       THIS PROGRAM REQUIRES FOLLOWING INPUTS:
    ! N: LENGTH OF DATA
    ! K: DIMENSION OF THE OBSERVATION VECTOR
    ! LAGH: MAXIMUM LAG
    ! ISW: ISW=1...ROWWISE DATA INPUT
    !       ISW=2...COLUMNWISE DATA INPUT
    ! DFORM: INPUT FORMAT SPECIFICATION STATEMENT IN ONE CARD,
    ! FOR EXAMPLE
    ! (8F10.4)
    ! (X1(S,I); S=1,...,N, I=1,...,K): ORIGINAL DATA MATRIX.
    ! THE OUTPUTS ARE (CIJ(L): L=0,1,...,LAGH) (I=1,...,K; J=1,...,K),
    ! WHERE CIJ(L)=COVARIANCE(XI(S+L),XJ(S)),
    ! !AND THEIR NORMALIZED (CORRELATION) VALUES.   
    !================================================================================86
    subroutine mulcorf_wrapper(x1,n,k,lagh1,sm,c,cn,fp_flags)
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          real(R64P),    dimension(n,k),        intent(in)    :: x1
          integer(I32P),                       intent(in)     :: n,k,lagh1
          real(R64P),    dimension(k),          intent(inout) :: sm
          real(R64P),    dimension(lagh1,k,k),  intent(inout) :: c,cn
          logical(I32P), dimension(5),          intent(inout) :: fp_flags
          ! Locals
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
          call MULCORF(x1,n,k,lagh1,sm,c,cn)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " mulcorf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF              
    
    end subroutine mulcorf_wrapper
    
    !================================================================================86
    !    Wrapper around: 'MULFRFF' program
    !    --------------------- Original description ---------------------------------
    !    PROGRAM 5.2.4   FREQUENCY RESPONSE FUNCTION (MULTIPLE CHANNEL)
    !     THIS PROGRAM COMPUTES MULTIPLE FREQUENCY RESPONSE FUNCTION, GAIN,
    !PHASE, MULTIPLE COHERENCY, PARTIAL COHERENCY AND RELATIVE ERROR
    ! STATISTICS.
    ! A CARD WITH THE TOATL NUMBER(K) OF INPUT VARIABLES AND ANOTHER
    ! WITH SPECIFICATION OF INPUT VARIABLES(INW(I),I=1,K) AND OUTPUT
    !VARIABLE(INW(K+1)) SHOULD BE ADDED ON TOP OF THE OUTPUT OF
    ! PROGRAM 5.2.2 MULSPE TO FORM THE INPUT TO THIS PROGRAM.
    ! WITHIN IP0 VARIABLES OF MULSPE OUTPUT, ONLY THOSE K+1 INW(I)-TH
    ! VARIABLES ARE TAKEN INTO COMPUTATION.
    !================================================================================86
    subroutine mulfrff_wrapper(k,inw,n,lagh1,ip0,p,x,c,s,g,ph,pch,r,chm,fp_flags)
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF    
          integer(I32P),                            intent(in)    :: k
          integer(I32P),  dimension(k+1),           intent(in)    :: inw
          integer(I32P),                            intent(in)    :: n,lagh1,ip0
          real(R64P),     dimension(lagh1,ip0,ip0), intent(in)    :: p
          complex(R64P),  dimension(ip0,ip0,lagh1), intent(inout) :: x
          real(R64P),     dimension(k,lagh1),       intent(inout) :: c,s,g
          real(R64P),     dimension(k,lagh1),       intent(inout) :: ph,pch,r
          real(R64P),     dimension(lagh1),         intent(inout) :: chm
          logical(I32P),  dimension(5),             intent(inout) :: fp_flags
          ! Locals
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
          call MULFRFF(k,inw,n,lagh1,ip0,p,x,c,s,g,ph,pch,r,chm)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " mulfrff_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF             
    end subroutine mulfrff_wrapper
    
    !================================================================================86
    !   Wrapper around: 'MULMARF' program
    !   -------------------------- Original description -----------------------------
    !     MULTIVARIATE CASE OF MINIMUM AIC METHOD OF AR MODEL FITTING.      
    !                                                                   
    ! THIS PROGRAM FITS A MULTI-VARIATE AUTOREGRESSIVE MODEL BY THE MINI
    ! AIC PROCEDURE.  ONLY THE POSSIBILITIES OF ZERO COEFFICIENTS AT THE
    ! BEGINNING AND END OF THE MODEL ARE CONSIDERED. THE LEAST SQUARES E
    !OF THE PARAMETERS ARE OBTAINED BY THE HOUSEHOLDER TRANSFORMATION. 
    !AIC IS DEFINED BY                                                 
    !                                                                   
    !        AIC  =  N * LOG( DET(SD) ) + 2 * (NUMBER OF PARAMETERS)    
    !                                                                   
    !   WHERE                                                           
    !       N:    NUMBER OF DATA,                                       
    !       SD:   ESTIMATE OF INNOVATION VARIANCE MATRIX                
    !      DET:  DETERMINANT,                                          
    !       K:    NUMBER OF FREE PARAMETERS.                            
    !                                                                  
    !                                                                   
    !   --------------------------------------------------------------- 
    !   THE FOLLOWING SUBROUTINES ARE DIRECTLY CALLED BY THIS PROGRAM.  
    !       MRDATA                                                      
    !       MREDCT                                                      
    !       MARFIT                                                      
    !   --------------------------------------------------------------- 
    !================================================================================86
    subroutine mulmarf_wrapper( zs,n,id,c,lag,zmean,zvari,sd1,aic1,dic1,im,aicm,   &
                                sdm,npr,jndf,af,ex,aic,ei,bi,e,b,lmax,aics,fp_flags  )
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF    
          real(R64P),     dimension(n,id),          intent(in)    :: zs
          integer(I32P),                            intent(in)    :: n
          integer(I32P),                            intent(in)    :: id
          real(R64P),     dimension(id),            intent(in)    :: c
          integer(I32P),                            intent(in)    :: lag
          real(R64P),     dimension(id),            intent(inout) :: zmean
          real(R64P),     dimension(id),            intent(inout) :: zvari
          real(R64P),     dimension(lag+1,id),      intent(inout) :: sd1
          real(R64P),     dimension(lag+1,id),      intent(inout) :: aic1
          real(R64P),     dimension(lag+1,id),      intent(inout) :: dic1
          integer(I32P),  dimension(id),            intent(inout) :: im
          real(R64P),     dimension(id),            intent(inout) :: aicm
          real(R64P),     dimension(id),            intent(inout) :: sdm
          integer(I32P),  dimension(id),            intent(inout) :: npr
          integer(I32P),  dimension((lag+1)*id,id), intent(inout) :: jndf
          real(R64P),     dimension((lag+1)*id,id), intent(inout) :: af
          real(R64P),     dimension(id),            intent(inout) :: ex
          real(R64P),     dimension(id),            intent(inout) :: aic
          real(R64P),     dimension(id,id),         intent(inout) :: ei
          real(R64P),     dimension(id,id,lag),     intent(inout) :: bi
          real(R64P),     dimension(id,id),         intent(inout) :: e
          real(R64P),     dimension(id,id,lag),     intent(inout) :: b
          integer(I32P),                            intent(inout) :: lmax
          real(R64P),                               intent(inout) :: aics
          logical(I32P),  dimension(5),             intent(inout) :: fp_flags
          ! Locals
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF
          ! Exec code ...
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          call MULMARF(zs,n,id,c,lag,zmean,zvari,sd1,aic1,dic1,im,aicm,sdm,npr,jndf,  &
                        af,ex,aic,ei,bi,e,b,lmax,aics  )
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " mulmarf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF             
    end subroutine mulmarf_wrapper
                                
    !================================================================================86
    !   Wrapper around: 'MULNOSF'  program                       
    !   ----------------------- Original description ----------------------------
    !     THIS PROGRAM COMPUTES RELATIVE POWER CONTRIBUTIONS IN DIFFERENTIAL     AND INTEGRATED FORM, ASSUMING THE ORTHOGONALITY BETWEEN NOISE
    !     SOURCES.
    !     THE PROGRAM OPERATES ON THE OUTPUT OF PROGRAM 5.3.2 FPEC WITH
    !     IL=0.
    !     THE RESULTS ARE GIVEN AT FREQUIENCIES I/(2*H).
    !================================================================================86
    subroutine mulnosf_wrapper(h,l,ip,sd,a,rs1,rs2,r,fp_flags)
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF 
          integer(I32P),                       intent(in)    :: h,l,ip
          real(R64P),   dimension(ip,ip),      intent(in)    :: sd
          real(R64P),   dimension(l,ip,ip),    intent(in)    :: a
          real(R64P),   dimension(ip,ip),      intent(inout) :: rs1
          real(R64P),   dimension(ip,ip,h+1),  intent(inout) :: rs2,r
          logical(I32P),dimension(5),          intent(inout) :: fp_flags
          ! Locals
 !DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          call MULNOSF(h,l,ip,sd,a,rs1,rs2,r)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " mulnosf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF              
    end subroutine
    
    !================================================================================86
    !      Wrapper around: 'MULRSPF' program
    !      -------------------------- Original description ---------------------------
    !       THIS PROGRAM COMPUTES RATIONAL SPECTRUM FOR IP-DIMENSIONAL
    !       AR-MA PROCESS
    !       X(N)=A(1)X(N-1)+...+A(L)X(N-L)+E(N)+B(1)E(N-1)+...+B(K)E(N-K),
    !       WHERE E(N) IS A WHITE NOISE WITH ZERO MEAN VECTOR AND COVARIANCE
    !       MATRIX SD.
    !       OUTPUTS ARE SPECTRUM MATRIX P(I) AT FREQUENCIES I/(2*H)
    !       (I=0,1,...,H).
    !================================================================================86
    subroutine mulrspf_wrapper(h,l,ip,k,sd,a,b,y,ch,fp_flags)
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF 
          integer(I32P),                        intent(in)    :: h,l,ip,k
          real(R64P),     dimension(ip,ip),     intent(in)    :: sd
          real(R64P),     dimension(l,ip,ip),   intent(in)    :: a
          real(R64P),     dimension(k,ip,ip),   intent(in)    :: b
          complex(R64P),  dimension(ip,ip,h+1), intent(inout) :: y
          real(R64P),     dimension(ip,ip,h+1), intent(inout) :: ch
          logical(I32P),  dimension(5),         intent(inout) :: fp_flags
          ! Locals
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF 
          ! Exec code ...
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          call MULRSPF(h,l,ip,k,sd,a,b,y,ch)
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " mulrspf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF          
    end subroutine mulrspf_wrapper
    
    !================================================================================86
    !    Wrapper around: 'MULSPEF'
    !    ------------------- Original description ------------------------------
    !     THIS PROGRAM COMPUTES MULTIPLE SPECTRUM ESTIMATES FROM THE OUTPUT
    ! OF PROGRAM 5.1.2 MULCOR, USING WINDOWS W1 AND W2.
    ! ONLY ONE CARD OF LAGH(MAXIMUM LAG OF COVARIANCES TO BE USED FOR
    ! SPECTRUM COMPUTATION) SHOULD BE ADDED ON TOP OF THE OUTPUT OF
    ! PROGRAM 5.1.2 MULCOR TO FORM THE INPUT TO THIS PROGRAM.
    ! IN THE CARD OUTPUT OF SPECTRUM MATRIX ON AND LOWER DIAGONAL ARE
    ! REAL PARTS AND UPPER DIAGONAL ARE IMAGINARY PARTS OF ON AND LOWER
    ! DIAGONAL SPECTRAL ELEMENTS.
    !================================================================================86
    subroutine mulspef_wrapper(n,k,lagh1,lagh3,cv,p1,p2,ps,pch1,pch2,fp_flags)
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF  
          integer(I32P),                        intent(in)    :: n,k,lagh1,lagh3
          real(R64P),    dimension(lagh3,k,k),  intent(in)    :: cv
          real(R64P),    dimension(lagh1,k,k),  intent(inout) :: p1,p2
          real(R64P),    dimension(lagh1,k),    intent(inout) :: ps
          real(R64P),    dimension(lagh1,k,k),  intent(inout) :: pch1,pch2
          logical(I32P), dimension(5),          intent(inout) :: fp_flags
          ! Locals
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF 
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          call MULSPEF( n,k,lagh1,lagh3,cv,p1,p2,ps,pch1,pch2)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " mulspef_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF            
    end subroutine mulspef_wrapper
    
    !================================================================================86
    !   Wrapper around: 'NONSTF' program
    !   ----------------------- Original description ------------------------------
    !    THIS PROGRAM LOCALLY FITS AUTOREGRESSIVE MODELS TO NON-STATIONARY
    ! TIME SERIES BY AIC CRITERION.
    ! POWER SPECTRA FOR STATIONARY SPANS ARE GRAPHICALLY PRINTED OUT.
    ! THE FOLLOWING INPUTS ARE REQUIRED;
    !     N: LENGTH OF DATA
    !     ISTP : LENGTH OF THE BASIC LOCAL SPAN
    !     DFORM : INPUT FORMAT SPECIFICATION IN ONE CARD, FOR EXAMPLE,'(8
    !     (X(I),I=1,N) : ORIGINAL DATA.
    !================================================================================86
    subroutine nonstf_wrapper(n,istp,data0,nm,lagh,jp0,coef0,va0,aic0,daic21,daic,k01,kount2,sxx,fp_flags)
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF  
          integer(I32P),                     intent(in)    :: n,istp
          real(R64P),    dimension(n),       intent(in)    :: data0
          integer(I32P),                     intent(in)    :: nm,lagh
          integer(I32P), dimension(nm),      intent(inout) :: jp0
          real(R64P),    dimension(lagh,nm), intent(inout) :: coef0
          real(R64P),    dimension(nm),      intent(inout) :: va0,aic0,daic21,daic
          integer(I32P), dimension(nm),      intent(inout) :: k01,kount2
          real(R64P),    dimension(121,nm),  intent(inout) :: sxx
          logical(I32P), dimension(5),       intent(inout) :: fp_flags
          ! Locals
 !DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF 
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          call NONSTF(n,istp,data0,nm,lagh,jp0,coef0,va0,aic0,daic21,daic,k01,kount2,sxx)
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " nonstf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine  nonstf_wrapper
    
    !================================================================================86
    !    Wrapper around: 'PRDCTRF' program
    !    -------------------- Original description ----------------------------------
    !     THIS PROGRAM OPERATES ON A REAL RECORD OF A VECTOR PROCESS
    !     Y(I) (I=1,N) AND COMPUTES PREDICTED VALUES. ONE STEP AHEAD
    !     PREDICTION STARTS AT TIME P AND ENDS AT TIME Q. PREDICTION IS
    !     CONTINUED WITHOUT NEW OBSERVATIONS UNTIL TIME Q+H.
    !     BASIC MODEL IS THE AUTOREGRESSIVE MOVING AVERAGE
    !     MODEL OF Y(I) WHICH IS GIVEN BY
    !     Y(I)+B(1)Y(I-1)+...+B(K)Y(I-K) = X(I)+A(1)X(I-1)+...+A(L)X(I-L).
    !
    !     THE FOLLOWING INPUTS ARE REQUIRED:
    !     (N,P,Q,H):
    !              N, LENGTH OF DATA
    !              P, ONE STEP AHEAD PREDICTION STARTING POSITION
    !              Q, LONG RANGE FORECAST STARTING POSITION
    !              H, MAXIMUM SPAN OF FORECAST (LESS THAN OR EQUAL TO 100)
    !              (Q+H MUST BE LESS THAN 1001)
    !     JSW: JSW=0 FOR DIRECT LOADING OF AR-MA COEFFICIENTS,
    !              THE OUTPUTS OF PROGRAM MARKOV WITH ICONT=0.
    !        JSW=1 FOR LOADING OF THE OUTPUTS OF PROGRAM MARKOV,
    !              THE OUTPUTS OF PROGRAM MARKOV WITH ICONT=1.
    !    (D,K,L):
    !          D, DIMENSION OF THE VECTOR Y(I)
    !          K, AR-ORDER (LESS THAN OR EQUAL TO 10)
    !          L, MA-ORDER (LESS THAN OR EQUAL TO 10)
    !          N,L,K,H,P,Q,D,JSW,ARE ALL INTEGERS
    !   (DFORM(I),I=1,20): INPUT FORMAT STATEMENT IN ONE CARD,
    !                         FOR EXAMPLE, (8F10.4)
    !    (NAME(I,J),I=1,20,J=1,D): NAME OF THE I-TH COMPONENT
    !    (Y(I,J),I=1,N;J=1,D): ORIGINAL DATA
    !    (B(I1,I2,J),I1=1,D,I2=1,D,J=1,K): AR-COEFFICIENT MATRICES.
    !    FOR JSW=0,
    !         (A(I1,I2,J),I1=1,D,I2=1,D,J=1,L): MA-COEFFICIENT MATRICES.
    !    FOR JSW=1,
    !     (W(I1,I2,J),I1=1,D,I2=1,D,J=1,L): IMPULSE RESPONSE MATRICES.
    !     (S(I,J),I=1,D,J=1,D): INNOVATION VARIANCE MATRIX
    !
    !      THE OUTPUTS OF THIS PROGRAM ARE THE REAL
    !!     AND PREDICTED VALUES OF Y(I).
    !================================================================================86
    subroutine  prdctrf_wrapper(n,p,q,h,d,k,l,jsw,yy,b,a,ww,s,y,yori,yd,x,z1,    &
                                z2,z3,zz1,zz2,zz3,fp_flags                      )
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          integer(I32P),                    intent(in)    :: n,p,q,h,d,k,l,jsw
          real(R64P),    dimension(n,d),    intent(in)    :: yy
          real(R64P),    dimension(d,d,k),  intent(in)    :: b
          real(R64P),    dimension(d,d,l),  intent(in)    :: a,ww
          real(R64P),    dimension(d,d),    intent(in)    :: s
          real(R64P),    dimension(q+h,d),  intent(inout) :: y
          real(R64P),    dimension(h+1,d),  intent(inout) :: yori
          real(R64P),    dimension(q+h,d),  intent(inout) :: yd
          real(R64P),    dimension(n,d),    intent(inout) :: x
          real(R64P),    dimension(q+h,d),  intent(inout) :: z1,z2,z3,zz1,zz2,zz3
          logical(I32P), dimension(5),      intent(inout) :: fp_flags
          ! Locals
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF 
          ! Exec code ...
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          call PRDCTRF(n,p,q,h,d,k,l,jsw,yy,b,a,ww,s,y,yordi,yd,x,  &
                       z1,z2,z3,zz1,zz2,zz3)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " prdctrf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF          
    end subroutine prdctrf_wrapper
    
    !================================================================================86
    !   Wrapper around: 'XSARMAF' program
    !   --------------------- Original description ------------------------------                         
    !     EXACT MAXIMUM LIKELIHOOD METHOD OF SCALAR AR-MA MODEL FITTING     
    !                                                                   
    !-----------------------------------------------------------------------
    ! THIS PROGRAM PRODUCES EXACT MAXIMUM LIKELIHOOD ESTIMATES OF THE   
    ! PARAMETERS OF A SCALAR AR-MA MODEL.                               
    !                                                                   
    ! THE AR-MA MODEL IS GIVEN BY                                       
    !                                                                   
    ! Y(I)+B(1)Y(I-1)+...+B(IQ)Y(I-IQ)=X(I)+A(1)X(I-1)+...+A(IP)X(I-IP),
    !                                                                   
    ! WHERE X(I) IS A ZERO MEAN WHITE NOISE.
    !================================================================================86    
    subroutine xsarmaf_wrapper(ys,n,iq,ip,p01,g1,tl1,p02,g2,alphb,alpha,tl2,sigma2,fp_flags)
!DIR$ IF ( USE_IEEE_EXCEPTION_HANDLING .EQ. 1 )
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          real(R64P),   dimension(n),     intent(in)    :: ys
          integer(I32P),                  intent(in)    :: n,iq,ip
          real(R64P),   dimension(ip+iq), intent(in)    :: p01
          real(R64P),   dimension(ip+iq), intent(inout) :: g1
          real(R64P),                     intent(inout) :: tl1
          real(R64P),   dimension(ip+iq), intent(inout) :: p02
          real(R64P),   dimension(ip+iq), intent(inout) :: g2
          real(R64P),   dimension(iq),    intent(inout) :: alphb
          real(R64P),   dimension(ip),    intent(inout) :: alpha
          real(R64P),                     intent(inout) :: tl2,sigma2
          logical(I32P), dimension(5),    intent(inout) :: fp_flags
          ! Locals
!DIR$     IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)  :: status_value
!DIR$     ENDIF
          ! Exec code ....
          ! Sanity check
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          call XSARMAF(ys,n,iq,ip,p01,g1,tl1,p02,g2,alphb,alpha,tl2,sigma2)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
                  
                  print "================================================================="
                  print " xsarmaf_wrapper: FLOATING-POINT EXCEPTION(S) OCCURRED -- ", fp_flags
                  print "================================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF             
    end subroutine
    
    
end module mod_timsacwrap