
module module_statistics

#include "Config.hpp"
!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'module_statistics'
 !          
 !          Purpose:
 !                         Modern Fortran version of STATSPAC library
 !          History:
 !                        Date: 08-07-2017
 !                        Time: 10:00 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !           
 !                  WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
 !                  DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
 !                  MARYLAND  20899

 !          Notification:
 !                  Changed and Adapted from original work by Bernard Gingold 
 !          
 !          Implemented changes:
 !
 !          1)        Packaging of free-standing subroutines into dedicated module.
 !          2)        Removing of assumed size arrays
 !          3)        Insertion of deffered shape arrays
 !          4)        Removing of 'continue' statements
 !          5)        Removing of 'data' statements
 !          6)        Disabling implicit typing
 !          7)  
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
    use module_class_error_check, only : array1D_not_alloc, array2D_not_alloc
    use ifcore
    implicit none
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! File major version
    integer(I32P), parameter, public :: MODULE_STATISTICS_MAJOR = 1
    
    ! File minor version
    integer(I32P), parameter, public :: MODULE_STATISTICS_MINOR = 0
    
    ! File micro(patch) version
    integer(I32P), parameter, public :: MODULE_STATISTICS_MICRO = 0
    
    ! File full version
    integer(I32P), parameter, public :: MODULE_STATISTICS_FULLVER = 1000*MODULE_STATISTICS_MAJOR + 100*MODULE_STATISTICS_MINOR + &
                                                                    10*MODULE_STATISTICS_MICRO
    
    ! File creation date
    character(*),  parameter, public :: MODULE_STATISTICS_CREATION_DATE="08-07-2017 10:34 AM -00200 (08 Sat Jul 2017 GMT+2)"
    
    ! File build date (set this value to last successful build date/time)
    character(*),  parameter, public :: MODULE_STATISTICS_BUILD_DATE=" "
    
    ! File name
    character(*),  parameter, public :: MODULE_STATISTICS_FILE_NAME="module_statistics_pkg.f90"
    
    ! File author
    character(*),  parameter, public :: MODULE_STATISTICS_AUTHOR="Adapted by Bernard Gingold from STATSPAC, e-mail: beniekg@gmail.com"
    
    ! Module description (short)
    character(*),  parameter, public :: MODULE_STATISTICS_DESCRIPTION="Statistical subroutines package"
    
    contains
    
    !==============================================================================84
    !  FOR: PERFORMING BARTLETT'S TEST FOR HOMOGENEITY OF VARIANCES ON
    !        THREE OR MORE VARIANCES (THE F TEST SHOULD BE USED IN THE CASE
    !        OF TWO VARIANCES).  IF THE INPUT PARAMETERS ARE NOT VALID AN 
    !        ERROR FLAG IS SET AND NOTHING FURTHER IS COMPUTED, OTHERWISE 
    !        THE FOLLOWING ARE COMPUTED: 
    !
    !           1) THE CHI-SQUARED STATISTIC (CH2),
    !           2) THE CUMULATIVE DISTRIBUTION FUNCTION OF THE CHI-SQUARED
    !              DISTRIBUTION EVALUATED AT CH2 (CH2CDF), AND
    !           3) THE POOLED VARIANCE (VARP) AND ITS CORRESPONDING
    !              DEGREES OF FREEDOM (DFP) 
    !
    !        THE VALUES IN 3) MAY BE USEFUL ONLY IF THE VARIANCES ARE
    !        DETERMINED TO BE EQUAL.  THE VALUE OF CH2CDF IS GOOD TO SIX
    !        DECIMAL PLACES.
    !
    !   SUBPROGRAMS CALLED: CDFGAM (GAMMA CUMULATIVE DISTRIBUTION FUNCTION)
    !   MODERNIZED BY @BERNARD GINGOLD ON JULY 8, 2017
    !   CURRENT VERSION COMPLETED FEBRUARY 3, 1987
    !
    !  REFERENCES: 
    !
    !  1) SNEDECOR, GEORGE W. AND COCHRAN, WILLIAM G., 'STATISTICAL
    !      METHODS', 6TH EDITION, IOWA STATE UNIVERSITY PRESS, PP. 296-298.
    !
    !   2) BROWNLEE, K.A., 'STATISTICAL THEORY AND METHODOLOGY IN SCIENCE 
    !      AND ENGINEERING', JOHN WILEY & SONS, 1960, PP. 225-227.
    !    DEFINITION OF PASSED PARAMETERS: 
    !
    !     * VAR = VECTOR (LENGTH N) OF VARIANCES (REAL)
    !
    !     * DF = VECTOR (LENGTH N) OF DEGREES OF FREEDOM CORRESPONDING
    !            TO THE VARIANCES (REAL)
    !
    !     * N = NUMBER OF VARIANCES [>2] (INTEGER)
    !
    !      CH2 = THE CHI-SQUARED STATISTIC ASSOCIATED WITH BARTLETT'S TEST
    !            (REAL) 
    !
    !   CH2CDF = THE CUMULATIVE DISTRIBUTION FUNCTION OF THE CHI-SQUARED
    !            DISTRIBUTION WITH N-1 DEGREES OF FREEDOM EVALUATED AT CH2
    !            (REAL) 
    !
    !     VARP = THE POOLED VARIANCE DETERMINED FROM THE N VARIANCES (REAL)
    !
    !      DFP = THE DEGREES OF FREEDOM ASSOCIATED WITH THE POOLED
    !            VARIANCE (REAL)
    !
    !    IFLAG = THE ERROR FLAG ON OUTPUT (INTEGER)   INTERPRETATION: 
    !    @Bernard Gingold added on 08/07/2017
    !    FP_FLAGS = ARRAY (LENGTH=5), OPTIONAL HOLDS STATUS OF FLOATING-POINT EXCEPTION FLAGS REGISTER
    !               STATIC ARRAY LOGICAL(KIND=4)
    !    ERRORS:
    !              0 -> NO ERRORS DETECTED
    !            1,2 -> ERROR FLAGS FROM SUBROUTINE CDFGAM
    !              3 -> N<3
    !              4 -> AT LEAST ONE DF(I) IS <= 0.0
    !              5 -> AT LEAST ONE VARIANCE(I) IS < 0.0
    !    @Bernard Gingold added on 08/07/2017
    !              6 -> UNALLOCATED ALLOCATBLE ARRAY EITHER VAR OR DF.
    !              7 -> OCCURRENCE OF ANY OF 5 FLOATING-POINT EXCEPTIONS
    !==============================================================================84
    subroutine bartlt(var,df,n,ch2,ch2cdf,  &
                      varp,dfp,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:), intent(in)                 :: var,df
          !DIR$ ASSUME_ALIGNED var:32, df:32
          integer(I64P),            intent(in)                 :: n
          real(R64P),               intent(inout)              :: ch2,ch2cdf,varp, &
                                                                   dfp
          integer(I32P),            intent(inout)              :: iflag
          logical(I32P), dimension(5), intent(inout)           :: fp_flags
          ! Locals
          real(R64P)                              :: a,c,eps,alpha,x,n64p
          integer(64P)                            :: i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
          
!DIR$ ENDIF
          ! Start of executable statements
          
          ! Error checking on input arguments
          if((array1D_not_alloc(var) .EQ. .true.) .OR. &
             (array1D_not_alloc(df)  .EQ. .true.)     ) then
              iflag = 6
              return
          end if
          if(n .LT. 3) then
             iflag = 3
             return
          end if
          do i = 1, n
             if(df(i) .LE. ZEROR64) then
                iflag = 4
                return
             end if
             if(var(i) .LT. ZEROR64) then
                 iflag = 5
             end if
          end do
          ! Compute needed summation
          a = 0.0_R64P
          c = 0.0_R64P
          x = 0.0_R64P
          n64p = DBLE(n-1) ! Was originally REAL(N-1)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,.false.)
!DIR$ ENDIF
          do i = 1, n
             a = a + df(i)
             varp = varp + df(i) * var(i)
             c = c + 1.0_R64P / df(i)
             ch2 = ch2 + df(i) * DLOG(var(i))
          end do
          ! Compute pooled variance and its degree of freedom
          varp = varp / a
          dfp  = a
          ! Compute Chi-squared statistics
          ch2 = a * DLOG(varp) - ch2
          a   = 1.0_R64P + (c - 1.0_R64P / a) / (3.0_R64P * n64p))
          ch2 = ch2 / a
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          
         
             call ieee_get_flag(ieee_all,fp_flags)
             if(ANY(fp_flags))    then
                iflag = 7
                write(ERROR_UNIT,*) "================================================="
                write(ERROR_UNIT,*) "  brtlt: FLOATING-POINT EXCEPTION(S) OCCURRED"
                write(ERROR_UNIT,*) "================================================="
             end if
        
           call ieee_set_status(status_value)
!DIR$ ENDIF
          ! Compute CDF at CH2
          x = 0.5_R64P * ch2
          alpha = 0.5_R64P * n64p
          eps = 0.000001_R64P
          ! call cdfgam 
    end subroutine
    
    !==============================================================================84
    !    CDFBET   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !        DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !        MARYLAND  20899

    !    FOR: COMPUTING THE CUMULATIVE DISTRIBUTION FUNCTION OF THE BETA
    !         DISTRIBUTION (ALSO KNOWN AS THE INCOMPLETE BETA RATIO) TO A
    !         SPECIFIED ACCURACY (TRUNCATION ERROR IN THE INFINITE SERIES).
    !         THE ALGORITHM, DESCRIBED IN REFERENCE 2, IS A MODIFICATION OF
    !    THE ALGORITHM OF REFERENCE 1.  THREE FEATURES HAVE BEEN ADDED: 
    !
    !     1) A PRECISE METHOD OF MEETING THE TRUNCATION ACCURACY,
    !     2) A CONSTANT W USED IN DETERMINING FOR WHICH X VALUES THE
    !          RELATION I(X,P,Q) = 1 - I(1-X,Q,P) IS TO BE USED, AND
    !     3) A CONSTANT UFLO >= THE UNDERFLOW LIMIT ON THE COMPUTER.
    !
    !     SUBPROGRAMS CALLED: DGAMLN (LOG OF GAMMA FUNCTION)
    !
    !      CURRENT VERSION COMPLETED OCTOBER 24, 1986
    !      MODERNIZED BY @BERNARD GINGOLD ON JULY 8, 2017
    !   REFERENCES: 
    !
    !   1) MAJUMDER, K.L. AND BHATTACHARJEE, G.P., 'THE INCOMPLETE BETA
    !      INTEGRAL', ALGORITHM AS 63, APPLIED STATISTICS, VOL. 22, NO. 3,
    !      1973, PP. 409-411.
    !
    !   2) REEVE, CHARLES P., 'AN ALGORITHM FOR COMPUTING THE BETA C.D.F. 
    !      TO A SPECIFIED ACCURACY', STATISTICAL ENGINEERING DIVISION
    !      NOTE 86-3, OCTOBER 1986.
    !  -------------------------------------------------------------------------
    !   DEFINITION OF PASSED PARAMETERS: 
    !
    !  * X = VALUE AT WHICH THE C.D.F. IS TO BE COMPUTED (REAL)
    !
    !  * P = FIRST PARAMETER OF THE BETA FUNCTION (>0) (REAL)
    !
    !  * Q = SECOND PARAMETER OF THE BETA FUNCTION (>0) (REAL)
    !
    !   * EPS =  THE DESIRED ABSOLUTE ACCURACY OF THE C.D.F. (>0) (REAL) 
    !
    !    IFLAG = ERROR INDICATOR ON OUTPUT (INTEGER)   INTERPRETATION: 
    !        0 -> NO ERRORS DETECTED
    !        1 -> EITHER P OR Q OR EPS IS <= UFLO 
    !        2 -> NUMBER OF TERMS EVALUATED IN THE INFINITE SERIES
    !             EXCEEDS JMAX
    !        4 -> FLOATING-POINT EXCEPTION OCCURRED
    !         CDFX = THE C.D.F. EVALUATED AT X (REAL)
    !
    !    * INDICATES PARAMETERS REQUIRING INPUT VALUES
    !==============================================================================84
    subroutine cdfbet(x,p,q,eps,iflag,cdfx,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P),                  intent(in)              :: x,p,q,eps
          integer(I32P),               intent(inout)           :: iflag
          real(R64P),                  intent(inout)           :: cdfx
          logical(I32P), dimension(5), intent(inout)           :: fp_flags
          ! Locals
          logical(I32P) :: LL 
          real(R64P)    :: dp,dq,xy,yx,pq,qp,r, &
                           pdfl,u,yxeps,v
          integer(I32P) :: j
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
          
!DIR$ ENDIF
          integer(I32P), parameter :: JMAX = 5000
          real(R64P),    parameter :: W    = 20.0_R64P
          real(R64P),    parameter :: UFLO = 0.00000000000000000000000000001_R64P
          ! Start of executable statements
          r = 0.0_R64P
          pdfl = 0.0_R64P
          u = 0.0_R64P
          yxeps = 0.0_R64P
          v = 0.0_R64P
          ! Check for arguments validity
          if(p   .LE. UFLO .OR. &
             q   .LE. UFLO .OR. &
             eps .LE. UFLO        ) then
             iflag = 1
             return
          end if
          ! Check for special cases of x
          if(x .LE. ZEROR64) then
             return
          end if
          if(any(fp_flags)) then
             fp_flags = .false.
          end if
          if(x .GE. 1.0_R64P) then
             cdfx = 1.0_R64P
          else
          ! Switch arguements if necessery
          LL =  p + W .GE. (p + q + 2.0_R64P * W) * x
          if(LL .EQ. .true.) then
             xy = x
             yx = 1.0_R64P - xy
             pq = p
             qp = q
          else
             yx = x
             xy = 1.0_R64P - yx
             qp = p
             pq = q
          end if
          ! Evaluate the BETA P.D.F and check for underflow
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,.false.)
!DIR$ ENDIF
          dp = pq - 1.0_R64P * DLOG(xy) - dgamln(pq) ! Implement dgamln
          dq = qp - 1.0_R64P * DLOG(yx) - dgamln(qp) !  --||--
          pdfl = dgamln(pq+qp) + dp + dq
          if(pdfl .LT. DLOG(UFLO)) then
          else
             u = DEXP(pdfl) * xy / pq
             r = xy/yx
10           if(qp .LE. 1.0_R64P) go to 20
             ! Increment PQ and decrement QP
             if(u.LE.eps*(1.0_R64P-(pq+qp)*xy/(pq+1.0_R64P))) go to 40
             cdfx = cdfx + u 
             pq = pq + 1.0_R64P
             qp = qp - 1.0_R64P
             u  = qp * r * u / pq
             go to 10
20           v = yx * u
             yxeps = yx * eps
             ! Increment PQ
Incr_PQ :    do j = 0, JMAX
                if(v .LE. yxeps) go to 40
                cdfx = cdfx + v
                pq = pq + 1.0_R64P
                v = (pq+qp-1.0_R64P) * xy * v / pq
             end do   Incr_PQ
             iflag = 2
          end if
40        if(.NOT. LL) cdfx = 1.0_R64P - cdfx
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
              iflag = 4
              write(ERROR_UNIT,*) "=================================================="
              write(ERROR_UNIT,*) " cdfbet: FLOATING-POINT EXECEPTION(S) OCCURRED"
              write(ERROR_UNIT,*) "=================================================="
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF
    end subroutine
    
    !==============================================================================84
    !     COMPUTE THE POISSON(ALAMB) PROBABILITIES OVER THE RANGE [L,K]
    !     WHERE THE TOTAL TAIL PROBABILITY IS LESS THAN EPS/2, SUM THE
    !     PROBABILITIES IN DOUBLE PRECISION, AND SHIFT THEM TO THE
    !     BEGINNING OF VECTOR V.
    !==============================================================================84
    subroutine poissf(alamb,eps,l,nspan,v,nv,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none    
          real(R64P),                  intent(in)    :: alamb,eps
          real(R64P), dimension(:),    intent(inout) :: v
          integer(I32P),               intent(inout) :: l,nspan
          integer(I32P),               intent(in)    :: nv
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)                                 :: dal,dk,dlimit,dsum, &
                                                        pl,pk
          integer(I32P)                              :: k,nk,inc,i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF          
          
          ! Start of executable statements
          if(array1D_not_alloc(v) .EQ. .true.) then
             iflag = 7
             return
          end if
          if(any(fp_flags)) then
             fp_flags = .false.
          end if
          dal = 0.0_R64P
          dk  = 0.0_R64P
          dlimit = 0.0_R64P
          dsum = 0.0_R64P
          pl = 0.0_R64P
          pk = 0.0_R64P
          dlimit = 1.0_R64P - 0.5_R64P * eps
          k = INT(alamb,kind=4)
          l = k + 1
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          if(alamb .EQ. ZEROR64) then
             pl = 1.0_R64P
          else
             dal = alamb
             dk  = DBLE(k,kind=8)
             pl  = DEXP(dk*DLOG(dal)-dal-dgamln(DBLE(k+1))) ! Implement dgamln
          end if
          pk = alamb * pl / DBLE(L)
          nk = nv / 2
          nl = nk + 1
          
10        if(pl .LT. pk) then
             nk = nk + 1
             if(nk .GT. nv) then
                iflag = 6
                return
             end if
             v(nk) = pk
             dsum = dsum + pk
             k = k + 1
             if(dsum .GE. dlimit) go to 20
             pk = alamb * pk / DBLE(k+1)
          else
             nl = nl - 1
             v(nl) = pl
             dsum = dsum + pl
             l = l - 1
             if(dsum .GE. dlimit) go to 20
             pl = DBLE(l) * pl / alamb
          end if
          go to 10
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 7
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " poissf: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF
20        inc = nl - 1
          do i = nl, nk
             v(i-inc) = v(i)
          end do
          nspan = nk - inc
              
    end subroutine
    
    !==============================================================================84
    !   COMPUTE THE BETA C.D.F.'S BY A RECURRENCE RELATION ALONG THE EDGES
    !   I = IMIN AND J = JMIN OF A GRID.  THE CORRESPONDING COMPONENTS OF
    !   THE F" C.D.F. ARE INCLUDED IN THE SUMMATION.  TERMS WHICH MIGHT
    !   CAUSE UNDERFLOW ARE SET TO ZERO.
    !
    !   MODIFIED BY BERNARD GINGOLD ON JULY 9 2017
    !==============================================================================84
    subroutine edgef(nk,fc,gc,xx,yy,bfk,cdfx,  &
                     poi,poj,eps3,iflag,fp_flag,L)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT    
          implicit none
          integer(I32P),               intent(in)    :: nk,L
          real(R64P),                  intent(in)    :: fc,gc,xx,yy,eps3
          real(R64P),                  intent(inout) :: cdfx
          real(R64P), dimension(:),    intent(inout) :: bfk
          real(R64P), dimension(:),    intent(in)    :: poi,poj
!DIR$     ASSUME_ALIGNED bfk:32, poi:32, poj:32
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P) :: darg,fd,di,dk,fk
          real(R64P), parameter :: deuflo = -69.0_R64P
          integer(I32P) :: k,kflag,i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF  
          ! Start of executable statements
          ! Sanity check on arg: fp_flags
          if(ANY(fp_flags) .EQ. .true.) then
             fp_flag = .false.
          end if
          if((array1D_not_alloc(bfk) .EQ. .true.) .OR. &
             (array1D_not_alloc(poi) .EQ. .true.) .OR. &
             (array1D_not_alloc(poj) .EQ. .true.)    )  then
              iflag = 7
              return
          end if
          fd = fc - 1.0_R64P
          k = MAX0(L,MIN0(nk,INT((gc-1.0_R64P)*xx/yy-fd)))
          fk = fd + DBLE(k)
          call cdfbet(xx,fk,gc,eps3,iflag,bfk(k),fp_flag)
          if(iflag /= 0) return
          if(L .EQ. 1) bfk(k) = 1.0_R64P - bfk(k)
          if(nk .EQ. 1) go to 40
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          darg = fk*DLOG(xx)+gc*DLOG(yy)- &
                 DLOG(fk)+dgamln(fk+gc)-dgamln(fk)-dgamln(gc)
          if(darg .LT. deuflo) then
             dk = 0.0_R64P
          else
             dk = DEXP(darg)*(-1.0_R64P) ** L
          end if
          if(k .GE. nk) go to 20
          bfk(k+1) = bfk(k) - dk
          di = dk
          kflag = 1
          do i = k+1, nk-1
             if(kflag .EQ. 1) then
                di = di*(fd+gc+DBLE(i-1))*xx/(fd+DBLE(i))
                if(dk+di .EQ. dk) then
                   kflag = 0
                   di = 0.0_R64P
                end if
             end if
             bfk(i+1) = bfk(i)-di
          end do
20        di = dk
          kflag = 1
          do i = k-1, L, -1
              if(kflag .EQ. 1) then
                 di = di*(fc+DBLE(i))/((fd+gc+DBLE(i))*xx)
                 if(dk+di .EQ. dk) then
                     kflag = 0
                     di = 0.0_R64P
                 end if
              end if
              bfk(i) = bfk(i+1)+di
          end do
!DIR$ SIMD
40        do i = L, nk
             cdfx = cdfx+poi(i)*poj(1)*bfk(i)
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " edgef: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF                    
             
    end subroutine
    
    !==============================================================================84
    !   COMPUTE THE POISSON(ALAMB) PROBABILITIES OVER THE RANGE [L,K]
    !    WHERE THE TOTAL TAIL PROBABILITY IS LESS THAN EPS/3, SUM THE
    !    PROBABILITIES IN DOUBLE PRECISION, AND SHIFT THEM TO THE
    !    BEGINNING OF VECTOR V. 
    !    MODIFIED BY BERNARD GINGOLD ON JULY 9 2017 
    !    MODIFICATION PERFORMED:
    !    1) ARGUMENT IFLAG = 7 IFF input array is not allocated
    !    2) ARGUMENT IFLAG = 8 IFF any of HW FP exception occurred
    !==============================================================================84
    subroutine poisst(alamb,eps,L,nspan,v,nv,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          real(R64P),                  intent(in)    :: alamb,eps
          integer(I32P),               intent(inout) :: L,nspan
          real(R64P), dimension(:),    intent(inout) :: v
!DIR$     ASSUME_ALIGNED v:32          
          integer(I32P),               intent(in)    :: nv
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)             :: dal,dk,dlimit,dsum,pl,pk
          real(R64P), parameter  :: third = 0.3333333333333333333333333_R64P
          integer(I32P)          :: i,k,nk,nl,nv,inc
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check on arg: fp_flags
          if(ANY(fp_flags)) then
             fp_flags = .false.
          end if
          if(array1D_not_alloc(v) .EQ. .true.) then
             iflag = 7
             return
          end if
          dlimit = 1.0_R64P - 2.0_R64P * eps * third
          k = IDINT(alamb)
          L = k + 1
          if(alamb .EQ. ZEROR64) then
             pl = 1.0_R64P
          else
             dal = alamb
             dk  = DBLE(k)
             pl  = DEXP(dk*DLOG(dal)-dal-dgamln(DBLE(k+1)))
          end if
          pk = alamb * pl / DBLE(L)
          nk = nv / 2
          nl = nk + 1
          dsum = 0.0_R64P
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          if (pl .LT. pk) then
              do
                 nk = nk + 1
                 if(nk .GT. nv) then
                    iflag = 6
                    return
                 end if
                 v(nk) = pk
                 dsum = dsum + pk
                 k = k + 1
                 if(dsum .GT. dlimit) exit
                 pk = alamb * pk / DBLE(k+1)
              end do
          else
              do
                 nl = nl - 1
                 v(nl) = pl
                 dsum = dsum + pl
                 L = L - 1
                 if(dsum .GE. dlimit) exit
                 pl = DBLE(L) * pl / alamb
              end do
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " poisst: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
          inc = nl - 1
          do i = nl, nk
              v(i-inc) = v(i)
          end do
          nspan = nk - inc
    end subroutine
    
    !==============================================================================84
    !  COMPUTE THE BETA C.D.F.'S BY A RECURRENCE RELATION ALONG THE EDGES
    !  I = IMIN AND J = JMIN OF A GRID.  THE CORRESPONDING COMPONENTS OF
    !  THE T" C.D.F. ARE INCLUDED IN THE SUMMATION.  TERMS WHICH MIGHT
    !  CAUSE UNDERFLOW ARE SET TO ZERO.
    !
    !==============================================================================84
    subroutine edget(nk,fc,gc,xx,yy,bfk,cdfx,  &
                     poi,poj,eps3,iflag,L,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          implicit none
          integer(I32P),               intent(in)    :: nk,L
          real(R64P),                  intent(in)    :: fc,gc,xx,yy,eps3
          real(R64P), dimension(:),    intent(inout) :: bfk
          real(R64P),                  intent(inout) :: cdfx 
          real(R64P), dimension(:),    intent(in)    :: poi,poj
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P) :: darg,fd,fk,dk,di
          real(R64P), parameter :: deuflo = -69.0_R64P
          integer(I32P) :: kflag,k,i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of arg: fp_flags
          if(ANY(fp_flags)) then
             fp_flags = .false.
          end if
          if((array1D_not_alloc(bfk).EQ. .true.) .OR. &
             (array1D_not_alloc(poi).EQ. .true.) .OR. &
             (array1D_not_alloc(poj).EQ. .true.)     ) then
              iflag = 7
              return
          end if
          fd = fc - 1.0_R64P
          k = MAX0(L,MIN0(nk,IDINT((gc-1.0_R64P)*xx/yy-fd)))
          fk = fd + DBLE(k)
          call cdfbet(xx,fk,gc,eps3,iflag,bfk(k),fp_flags)
          if(iflag /= 0) return
          if(L .EQ. 1) bfk(k) = 1.0_R64P - bfk(k)
          if(nk .EQ. 1) go to 40
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
          darg = fk*DLOG(xx)+gc*DLOG(yy)-DLOG(fk)+dgamln(fk+gc)- &
                 dgamln(fk)-dgamln(gc)
          if(darg .LT. deuflo) then
             dk = 0.0_R64P
          else
              dk = DEXP(darg)*(-1.0_R64P) ** L
          end if
          if(k .GE. nk) go to 20
          bfk(k+1) = bfk(k) - dk
          di = dk
          kflag = 1
          do i = k+1, nk-1
              if(kflag .EQ. 1) then
                 di = di*(fd+gc+DBLE(i-1))*xx/(fd+DBLE(i)) 
                 if(dk+di .EQ. dk) then
                     kflag = 0
                     di = 0.0_R64P
                 end if
              end if
              bfk(i+1) = bfk(i) - di
          end do
20        di = dk
          kflag = 1
          do i = k-1, L, -1
              if(kflag .EQ. 1) then
                  di = di*(fc+DBLE(i))/((fd+gc+DBLE(i))*xx)
                  if(dk+di .EQ. dk) then
                      kflag = 0
                      di = 0.0_R64P
                  end if
              end if
            bfk(i) = bfk(i+1) + di
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " edget: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
40        do i = L , nk
              cdfx = cdfx+poi(i)*poj(1)*bfl(i)
          end do
    end subroutine
                     
    !==============================================================================84
    !   COMPUTE DOUBLE SUMMATION OF COMPONENTS OF THE T" C.D.F. OVER THE 
    !   GRID I=IMIN TO IMAX AND J=JMIN TO JMAX              
    !==============================================================================84
    subroutine grid(ni,nj,fc,gc,bfi,bfj,poi,poj,xx,  &
                    yy,eps3,cdfx,iflag,fp_flags)
          implicit none
          integer(I32P),               intent(in)    :: ni,nj
          real(R64P),                  intent(in)    :: fc,gc
          real(R64P), dimension(:),    intent(inout) :: bfi,bfj
!DIR$     ASSUME_ALIGNED bfi:32, bfj:32
          real(R64P), dimension(:),    intent(in)    :: poi,poj
!DIR$     ASSUME_ALIGNED poi,poj
          real(R64P),                  intent(in)    :: xx,yy,eps3
          real(R64P),                  intent(inout) :: cdfx
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          integer(I32P)                              :: i,j
          ! Start of executable statements
          ! Argument checking performed by edget and edgef callees
          ! COMPUTE BETA C.D.F. BY RECURRENCE WHEN I=IMIN, J=JMIN TO JMAX
          call edget(nj,gc,fc,yy,xx,bfj,cdfx,poj,poi,eps3,iflag,1,fp_flags)
          if(ni.LE.1 .OR. iflag.NE.0) return
          !  COMPUTE BETA C.D.F. BY RECURRENCE WHEN J=JMIN, I=IMIN TO IMAX
          ! Sanity check of fp_flags
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          bfi(1) = bfj(1)
          call edget(ni,fc,gc,xx,yy,bfi,cdfx,poi,poj,eps3,iflag,2,fp_flags)
          if(nj.LE.1 .OR. iflag.NE.0) return
          ! COMPUTE BETA C.D.F. BY RECURRENCE WHEN I>IMIN, J>JMIN
          do i = 2, ni
              bfj(1) = bfi(i)
              do j = 2, nj
                  bfj(j) = xx*bfj(j)+yy*bfj(j-1)
                  cdfx   = cdfx+poi(i)*poj(j)*bfj(j)
              end do
          end do
          
    end subroutine
     
    !==============================================================================84
    !   CDFF   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !      DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !      MARYLAND  20899
    !
    !    FOR: COMPUTING THE CUMULATIVE DISTRIBUTION FUNCTION OF THE F
    !         DISTRIBUTION FROM THE CUMULATIVE DISTRIBUTION FUNCTION OF
    !         THE BETA DISTRIBUTION.  THE RELATIONSHIP BETWEEN THE TWO
    !         DISTRIBUTIONS IS GIVEN IN THE REFERENCE BELOW.
    !
    !    SUBPROGRAMS CALLED: CDFBET (BETA C.D.F.)
    !
    !     CURRENT VERSION COMPLETED AUGUST 15, 1987
    !
    !     REFERENCE: REEVE, CHARLES P., 'AN ALGORITHM FOR COMPUTING THE BETA
    !          C.D.F. TO A SPECIFIED ACCURACY', STATISTICAL ENGINEERING
    !           DIVISION NOTE 86-3, OCTOBER 1986.
    !
    !      DEFINITION OF PASSED PARAMETERS: 
    !
    !  * X = VALUE AT WHICH THE C.D.F. IS TO BE COMPUTED (REAL)
    !
    !  * DF1 = FIRST 'DEGREES OF FREEDOM' PARAMETER (>0) (REAL)
    !
    !  * DF2 = SECOND 'DEGREES OF FREEDOM' PARAMETER (>0) (REAL)
    !
    !  * EPS = THE DESIRED ABSOLUTE ACCURACY OF THE C.D.F. (>0) (REAL)
    !
    !   IFLAG = ERROR INDICATOR ON OUTPUT (INTEGER)   INTERPRETATION: 
    !         0 -> NO ERRORS DETECTED
    !        1,2 -> ERROR INDICATORS FROM THE BETA C.D.F. ROUTINE
    !          3 -> DF1 AND/OR DF2 IS NON-POSITIVE
    !
    !   CDFX = THE C.D.F. EVALUATED AT X (REAL)
    !
    !  * INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !==============================================================================84
    subroutine cdff(x,df1,df2,eps,iflag,cdfx,fp_flags)
          implicit none
          real(R64P),                  intent(in)    :: x,df1,df2,eps
          real(R64P),                  intent(inout) :: cdfx
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P) :: h,y,p,q
          ! Start of executable statements
          ! Sanity check on arg: fp_flags
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          !CHECK FOR NON-POSITIVE DEGREES OF FREEDOM
          if(DMIN1(df1,df2) .LE. ZEROR64) then
              iflag = 3
              return
          end if
          cdfx = 0.0_R64P
          h = df1 * x
          y = h / (h + df2)
          p = 0.5_R64P * df1
          q = 0.5_R64P * df2
          call cdfbet(y,p,q,eps,iflag,cdfx,fp_flags)
          
    end subroutine
    
    !==============================================================================84
    !   CDFGAM   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !        DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !        MD  20899
    !
    !    FOR: COMPUTING THE CUMULATIVE DISTRIBUTION FUNCTION OF THE GAMMA
    !         DISTRIBUTION (ALSO KNOWN AS THE INCOMPLETE GAMMA RATIO) TO A 
    !         SPECIFIED ACCURACY (TRUNCATION ERROR IN THE INFINITE SERIES).
    !         THE ALGORITHM, DESCRIBED IN REFERENCE 2, IS A MODIFICATION OF
    !         THE ALGORITHM OF REFERENCE 1.  THREE FEATURES HAVE BEEN ADDED: 
    !
    !        1) A PRECISE METHOD OF MEETING THE TRUNCATION ACCURACY,
    !        2) COMPUTATION OF THE UPPER TAIL AREA BY DECREMENTING ALPHA
    !           WHEN THAT METHOD IS MORE EFFICIENT, AND
    !        3) A CONSTANT UFLO >= THE UNDERFLOW LIMIT ON THE COMPUTER.
    !
    !           SUBPROGRAMS CALLED: DGAMLN (LOG OF GAMMA FUNCTION)
    !
    !           CURRENT VERSION COMPLETED OCTOBER 29, 1986
    !           MODIFIED BY BERNARD GINGOLD ON JULY 10 2017
    !           REFERENCES: 
    !
    !        1) LAU, CHI-LEUNG, 'A SIMPLE SERIES FOR THE INCOMPLETE GAMMA
    !           INTEGRAL', ALGORITHM AS 147, APPLIED STATISTICS, VOL. 29,
    !           NO. 1, 1980, PP. 113-114.
    !
    !        2) REEVE, CHARLES P., 'AN ALGORITHM FOR COMPUTING THE GAMMA C.D.F.
    !           TO A SPECIFIED ACCURACY', STATISTICAL ENGINEERING DIVISION
    !           NOTE 86-2, OCTOBER 1986.
    !
    !           DEFINITION OF PASSED PARAMETERS: 
    !
    !      * X = VALUE AT WHICH THE C.D.F IS TO BE COMPUTED (REAL)
    !
    !      * ALPHA = PARAMETER OF THE GAMMA FUNCTION (>0) (REAL)
    !
    !      * EPS = THE DESIRED ABSOLUTE ACCURACY OF THE C.D.F (>0) (REAL)
    !
    !        IFLAG = ERROR INDICATOR ON OUTPUT (INTEGER)   INTERPRETATION: 
    !              0 -> NO ERRORS DETECTED
    !              1 -> EITHER ALPHA OR EPS IS <= UFLO 
    !              2 -> NUMBER OF TERMS EVALUATED IN THE INFINITE SERIES
    !                   EXCEEDS IMAX.
    !              7 -> HW FP-EEXCEPTION HAS OCCURRED ADDED BY BERNARD GINGOLD
    !        CDFX = THE C.D.F. EVALUATED AT X (REAL)
    !
    !        * INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !
    !==============================================================================84
    subroutine cdfgam(x,alpha,eps,iflag,cdfx,fp_flag)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P),                  intent(in)    :: x,alpha,eps
          integer(I32P),               intent(inout) :: iflag
          real(R64P),                  intent(inout) :: cdfx
          logical(I32P), dimension(5), intent(inout) :: fp_flag
          ! Locals
          logical(I32P)            :: LL = .false.
          real(R64P)               :: dx,pdfl,p,u,eta,bl,epsx
          integer(I32P), parameter :: IMAX = 5000
          integer(I32P)            :: k,i,j
          real(R64P),    parameter :: UFLO = 1.0e-99
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          cdfx = 0.0_R64P
          !  CHECK FOR VALIDITY OF ARGUMENTS ALPHA AND EPS
          if(alpha.LE.UFLO .OR. eps.LE.UFLO) then
              iflag = 1
              return
          end if
          ! Sanity check on arg: fp_flags
          if(ANY(fp_flag)) then
              fp_flag = .false.
          end if
          iflag = 0
          !   CHECK FOR SPECIAL CASE OF X
          if(x .LE. ZEROR64)  return
          !  EVALUATE THE GAMMA P.D.F. AND CHECK FOR UNDERFLOW
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
          dx = x
          pdfl = (alpha-1.0_R64P*DLOG(dx)-dx-dgamln(alpha))
          if(pdfl .LT. DLOG(UFLO)) then
              if(x .GE. alpha) cdfx = 0.0_R64P
          else
              p = alpha
              u = DEXP(pdfl)
           !  DETERMINE WHETHER TO INCREMENT OR DECREMENT ALPHA (A.K.A. P)
              LL = .true.
              if(x .GE. p) then
                 k = DINT(p)
                 if(p .LE. DBLE(k)) k = k-1
                 eta = p-DBLE(K)
                 bl  = eta-1.0_R64P*DLOG(dx)-dx-dgamln(eta)
                 LL = bl .GT. DLOG(eps)
              end if
              epsx = eps/x
              if(LL .EQ. .true.) then
                 ! INCREMENT P
                  do i = 0, IMAX
                      if(u .LE. epsx*(p-x)) return
                      u = x*u/p
                      cdfx = cdfx+u
                      p = p+1.0_R64P
                  end do
                  iflag = 2
                 ! DECREMENT P
                  do j = 1, k
                     p = p-1.0_R64P
                     if(u .LE. epsx*(x-p)) exit
                     cdfx = cdfx+u
                     u = p*u/x
                  end do
                  cdfx = 1.0_R64P-cdfx
              end if
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " cdfgam: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF            
    end subroutine
    
    !==============================================================================84
    !    CDFNOR   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !    DIVISION, NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
    !    GAITHERSBURG, MARYLAND  20899
    !
    !    FOR: COMPUTING THE CUMULATIVE DISTRIBUTION FUNCTION OF THE STANDARD
    !          NORMAL DISTRIBUTION TO A SPECIFIED ACCURACY.  THE C.D.F. OF
    !          THE GAMMA DISTRIBUTION IS FIRST COMPUTED, THEN TRANSFORMED TO
    !          THE C.D.F. OF THE NORMAL DISTRIBUTION.
    !
    !     NOTE: TIMING TESTS ON THE CYBER 180/855 COMPUTER AT N.I.S.T.
    !           INDICATE THAT THE AVERAGE CPU TIME FOR COMPUTING ONE C.D.F. 
    !           IS ABOUT 0.0004 SECOND.
    !
    !     SUBPROGRAMS CALLED: CDFGAM (C.D.F. OF THE GAMMA DISTRIBUTION)
    !
    !     CURRENT VERSION COMPLETED APRIL 7, 1989
    !     MODIFIED BY BERNARD GINGOLD ON JULY 10 2017
    !
    !     DEFINITION OF PASSED PARAMETERS: 
    !
    !        * Z = THE VALUE FOR WHICH THE NORMAL C.D.F. IS TO BE COMPUTED
    !              [REAL]
    !
    !        * EPS = THE ABSOLUTE ACCURACY REQUIREMENT FOR THE C.D.F. [REAL]
    !
    !        IFLAG = ERROR INDICATOR ON OUTPUT [INTEGER]   INTERPRETATION: 
    !               0 -> NO ERRORS DETECTED.
    !               1 -> ERROR FLAG FROM CDFGAM 
    !               2 -> ERROR FLAG FROM CDFGAM 
    !
    !          CDFZ = THE C.D.F. OF THE STANDARD NORMAL DISTRIBUTION EVALUATED
    !                 AT X [REAL]
    !
    !         * INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !
    !==============================================================================84
    subroutine cdfnor(z,eps,iflag,cdfz,fp_flags)
          implicit none
          real(R64P),    intent(in)    :: z,eps
          integer(I32P), intent(inout) :: iflag
          real(R64P),    intent(inout) :: cdfz
          integer(I32P), intent(inout) :: fp_flags
          !Locals
          real(R64P) :: del,cdfx
          ! Start of executable statements
          ! Sanity check of arg: fp_flags
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          del = 2.0_R64P*eps
          if(z .EQ. ZEROR64) then
              cdfz = 0.0_R64P
          else
              x = 0.5_R64P*z*z
              call cdfgam(x,0.5_R64P,del,iflag,cdfx,fp_flags)
              if(iflag .NE. 0) return
              if(z .GT. ZEROR64) then
                  cdfz = 0.5_R64P+0.5_R64P*cdfx
              else
                  cdfz = 0.5_R64P-0.5_R64P*cdfx
              end if
          end if
    end subroutine     
    
    !==============================================================================84
    !  
    !    CDFT   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !           DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !           MARYLAND  20899
    !
    !    FOR: COMPUTING THE CUMULATIVE DISTRIBUTION FUNCTION OF THE T
    !         DISTRIBUTION FROM THE CUMULATIVE DISTRIBUTION FUNCTION OF
    !         THE BETA DISTRIBUTION.  THE RELATIONSHIP BETWEEN THE TWO
    !         DISTRIBUTIONS IS GIVEN IN THE REFERENCE BELOW.
    !
    !    SUBPROGRAMS CALLED: CDFBET (BETA C.D.F.)
    !
    !    CURRENT VERSION COMPLETED AUGUST 15, 1987
    !    MODIFIED BY BERNARD GINGOLD ON JULY 12 2017
    !
    !    REFERENCE: REEVE, CHARLES P., 'AN ALGORITHM FOR COMPUTING THE BETA
    !               C.D.F. TO A SPECIFIED ACCURACY', STATISTICAL ENGINEERING
    !               DIVISION NOTE 86-3, OCTOBER 1986.
    !
    !    DEFINITION OF PASSED PARAMETERS: 
    !
    !    * X = VALUE AT WHICH THE C.D.F. IS TO BE COMPUTED (REAL)
    !
    !    * DF = 'DEGREES OF FREEDOM' PARAMETER (>0) (REAL)
    !
    !    * EPS = THE DESIRED ABSOLUTE ACCURACY OF THE C.D.F. (>0) (REAL)
    !
    !    IFLAG = ERROR INDICATOR ON OUTPUT (INTEGER)   INTERPRETATION: 
    !          0 -> NO ERRORS DETECTED
    !          1,2 -> ERROR INDICATORS FROM THE BETA C.D.F. ROUTINE
    !          3 -> DF IS NON-POSITIVE
    !
    !    CDFX = THE C.D.F. EVALUATED AT X (REAL)
    !
    !    * INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !
    !==============================================================================84
    subroutine cdft(x,df,eps,iflag,cdfx,fp_flags)
          implicit none
          real(R64P),                  intent(in)    :: x,df,eps
          integer(I32P),               intent(inout) :: iflag
          real(R64P),                  intent(inout) :: cdfx
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          !Locals
          real(R64P)                                 :: h,y,p,q,cdfy
          ! Start of executable statements
          ! Sanity check on argument fp_flags
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          ! CHECK FOR NON-POSITIVE DEGREES OF FREEDOM
          if(df .LE. ZEROR64) then
              iflag = 3
              return
          end if
          cdfy = 0.0_R64P
          h = x*x
          y = h/(h+df)
          p = 0.5_R64P
          q = 0.5_R64P*df
          call cdfbet(y,p,q,eps,iflag,cdfy,fp_flags)
          cdfx = 0.5_R64P*(1.0_R64P+DSIGN(cdfy,x))
    end subroutine
    
    !==============================================================================84
    !    PURPOSE--THIS SUBROUTINE COMPUTES THE
    !              SAMPLE AUTOCORRELATION COEFFICIENT 
    !          OF THE DATA IN THE INPUT VECTOR X. 
    !          THE SAMPLE AUTOCORRELATION COEFFICIENT =  THE CORRELATION
    !          BETWEEN X(I) AND X(I+1) OVER THE ENTIRE SAMPLE.
    !          THE AUTOCORRELATION COEFFICIENT COEFFICIENT WILL BE A
    !          SINGLE PRECISION VALUE BETWEEN -1.0 AND 1.0
    !          (INCLUSIVELY). 
    ! INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
    !                            (UNSORTED) OBSERVATIONS.
    !                 --N      = THE INTEGER NUMBER OF OBSERVATIONS
    !                            IN THE VECTOR X. 
    !                 --IWRITE = AN INTEGER FLAG CODE WHICH 
    !                            (IF SET TO 0) WILL SUPPRESS
    !                            THE PRINTING OF THE
    !                            SAMPLE AUTOCORRELATION COEFFICIENT
    !                            AS IT IS COMPUTED;
    !                            OR (IF SET TO SOME INTEGER 
    !                            VALUE NOT EQUAL TO 0),
    !                            LIKE, SAY, 1) WILL CAUSE
    !                            THE PRINTING OF THE
    !                            SAMPLE AUTOCORRELATION COEFFICIENT
    !                            AT THE TIME IT IS COMPUTED.
    ! OUTPUT ARGUMENTS--XAUTOC = THE SINGLE PRECISION VALUE OF THE
    !                            COMPUTED SAMPLE AUTOCORRELATION
    !                            COEFFICIENT.
    !                            THIS SINGLE PRECISION VALUE
    !                            WILL BE BETWEEN -1.0 AND 1.0
    !                            (INCLUSIVELY).
    ! OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
    !         SAMPLE AUTOCORRELATION COEFFICIENT. 
    ! PRINTING--NONE, UNLESS IWRITE HAS BEEN SET TO A NON-ZERO
    !           INTEGER, OR UNLESS AN INPUT ARGUMENT ERROR
    !           CONDITION EXISTS.
    ! RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
    !               OF N FOR THIS SUBROUTINE.
    ! OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
    ! FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
    ! MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
    ! LANGUAGE--ANSI FORTRAN. 
    ! REFERENCES--JENKINS AND WATTS, SPECTRAL ANALYSIS AND
    !             ITS APPLICATIONS, 1968, PAGES 5, 182.
    ! WRITTEN BY--JAMES J. FILLIBEN
    !             STATISTICAL ENGINEERING DIVISION (714)
    !             NATIONAL BUREAU OF STANDARDS
    !             GAITHERSBURG, MD  20899
    !             PHONE:  301-921-3651
    ! ORIGINAL VERSION--JUNE      1972. 
    ! UPDATED         --SEPTEMBER 1975. 
    ! UPDATED         --NOVEMBER  1975. 
    ! MODIFIED BY - BERNARD GINGOLD  ON JULY 20 2017
    ! 
    !==============================================================================84
    subroutine autoco(x,n,xautoc,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT           
          implicit none
          real(R64P), dimension(:),    intent(inout) :: x
!DIR$     ASSUME_ALIGNED x:32
          integer(I64P),               intent(in)    :: n
          real(R64P),                  intent(inout) :: xautoc
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)    :: xbar,xbar1,xbar2,sum1,sum2,sum3,an
          integer(I64P) :: i,nm1,ip1
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF          
          ! Start of executable statements
          if(n.LT.1 .OR. n.EQ.0 .OR. n.NE.SIZE(x)) then
              write(ERROR_UNIT,*) "=================ERROR======================"
              write(ERROR_UNIT,*) " (      Invalid argument in autoco   )      "
              write(ERROR_UNIT,10) n
10            format("value of n=",I24.20)              
              write(ERROR_UNIT,*) "============================================"
              return
          else
             if(ANY(fp_flags)) then
                 fp_flags = .false.
             end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
             an = DBLE(n)
             do i = 1, n
                 xbar = xbar+x(i)
             end do
             xbar1 = xbar-x(n)
             xbar1 = xbar1/(an-1._R64P)
             xbar2 = xbar-x(1)
             xbar2 = xbar2/(an-1._R64P)
             sum1 = 0._R64P
             sum2 = 0._R64P
             sum3 = 0._R64P
             nm1 = n-1
             do i = 1, nm1
                 ip1 = i+1
                 sum1 = sum1+(x(i)-xbar1)*(x(ip1)-xbar2)
                 sum2 = sum2+(x(i)-xbar1)**2
                 sum3 = sum3+x(ip1)-xbar2)**2
             end do
             xautoc = sum1/(DSQRT(sum2*sum3))
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
            
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " autoco: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF               
         end if
    end subroutine
    
    !============================================================================84
    !     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
    !          FUNCTION VALUE FOR THE CAUCHY DISTRIBUTION
    !          WITH MEDIAN = 0 AND 75% POINT = 1. 
    !          THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
    !          THE PROBABILITY DENSITY FUNCTION
    !          F(X) = (1/PI)*(1/(1+X*X)).
    ! INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
    !                            WHICH THE CUMULATIVE DISTRIBUTION
    !                            FUNCTION IS TO BE EVALUATED.
    ! OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
    !                            DISTRIBUTION FUNCTION VALUE.
    ! OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
    !         FUNCTION VALUE CDF.
    !  WRITTEN BY--JAMES F. FILLIBEN
    !             STATISTICAL ENGINEERING LABORATORY (205.03)
    !             NATIONAL BUREAU OF STANDARDS
    !             WASHINGTON, D. C. 20234
    !             PHONE:  301-921-2315
    ! ORIGINAL VERSION--JUNE      1972. 
    ! UPDATED         --SEPTEMBER 1975. 
    ! UPDATED         --NOVEMBER  1975. 
    ! MODIFIED BY BERNARD GINGOLD ON JULY 20 2017
    !==============================================================================84
    subroutine caucdf(x,cdf)
          implicit none
          real(R64P), intent(in)    :: x
          real(R64P), intent(inout) :: cdf
          ! Locals
          real(R64P), parameter :: PI =  3.1415926535897932384626433832795_R64P
          ! Start of executable statements
          cdf = 0.5_R64P+((1._R64P/PI)*DATAN(x))
    end subroutine
    
    !==============================================================================84
    !   PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
    !          FUNCTION VALUE FOR THE CAUCHY DISTRIBUTION
    !          WITH MEDIAN = 0 AND 75% POINT = 1. 
    !          THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
    !          THE PROBABILITY DENSITY FUNCTION
    !          F(X) = (1/PI)*(1/(1+X*X)).
    ! INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
    !                            WHICH THE PROBABILITY DENSITY
    !                            FUNCTION IS TO BE EVALUATED.
    ! OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
    ! WRITTEN BY--JAMES J. FILLIBEN
    !             STATISTICAL ENGINEERING LABORATORY (205.03)
    !             NATIONAL BUREAU OF STANDARDS
    !             WASHINGTON, D. C. 20234
    !             PHONE:  301-921-2315
    ! ORIGINAL VERSION--JUNE      1972. 
    ! UPDATED         --SEPTEMBER 1975. 
    ! UPDATED         --NOVEMBER  1975.                           
    ! MODIFIED BY -- BERNARD GINGOLD ON JULY 20 2017
    !==============================================================================84
    subroutine caupdf(x,pdf)
          implicit none
          real(R64P), intent(in)    :: x
          real(R64P), intent(inout) :: pdf
          ! Locals
          real(R64P), parameter :: C = 0.31830988618379_R64P
          ! Start of executable statements
          pdf = c*(1._R64P/(1._R64P+x*x))
    end subroutine
    
    !==============================================================================84
    !    PURPOSE--THIS SUBROUTINE COMPUTES THE
    !          SAMPLE VARIANCE (WITH DENOMINATOR N-1)
    !          OF THE DATA IN THE INPUT VECTOR X. 
    !          THE SAMPLE VARIANCE = (THE SUM OF THE
    !          SQUARED DEVIATIONS ABOUT THE SAMPLE MEAN)/(N-1).
    ! INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
    !                            (UNSORTED OR SORTED) OBSERVATIONS.
    !                 --N      = THE INTEGER NUMBER OF OBSERVATIONS
    !                            IN THE VECTOR X. 
    !                 --IWRITE = AN INTEGER FLAG CODE WHICH 
    !                            (IF SET TO 0) WILL SUPPRESS
    !                            THE PRINTING OF THE
    !                            SAMPLE VARIANCE
    !                            AS IT IS COMPUTED;
    !                            OR (IF SET TO SOME INTEGER 
    !                            VALUE NOT EQUAL TO 0),
    !                            LIKE, SAY, 1) WILL CAUSE
    !                            THE PRINTING OF THE
    !                            SAMPLE VARIANCE
    !                            AT THE TIME IT IS COMPUTED.
    ! OUTPUT ARGUMENTS--XVAR   = THE SINGLE PRECISION VALUE OF THE
    !                            COMPUTED SAMPLE VARIANCE.
    ! OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
    !         SAMPLE VARIANCE (WITH DENOMINATOR N-1).
    !  WRITTEN BY--JAMES J. FILLIBEN
    !               STATISTICAL ENGINEERING LABORATORY (205.03)               
    !               NATIONAL BUREAU OF STANDARDS
    !               WASHINGTON, D. C. 20234
    !  MODIFIED BY -- BERNARD GINGOLD ON JULY 20 2017
    !==============================================================================84
    subroutine var(x,n,xvar,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:),    intent(in)    :: x
          integer(I64P),               intent(in)    :: n
          real(R64P),                  intent(inout) :: xvar
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)             :: an,sum,xmean
          integer(I64P)          :: i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          if(n.LT.1 .OR. n.EQ.1 .OR. n.NE.SIZE(x)) then
              write(ERROR_UNIT,*) "===============ERROR=============="
              write(ERROR_UNIT,*) " (   Invalid argument in var    ) "
              write(ERROR_UNIT,10) n
10            format("value of n=",I24.20)
              write(ERROR_UNIT,*) "==================================" 
              return
          else 
              if(ANY(fp_flags)) then
                  fp_flags = .false.
              end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF               
              an = DBLE(n)
              sum = 0._R64P
              do i = 1, n
                  sum = sum+x(i)
              end do
              xmean = sum/an
              sum = 0._R64P
              do i = 1, n
                  sum = sum+(x(i)-xmean)**2
              end do
              xvar = sum/(an-1_R64P)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
            
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " var: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
 !DIR$ ENDIF      
          end if     
    end subroutine
    
    !==============================================================================84
    !     PURPOSE--THIS SUBROUTINE GENERATES THE N ORDER STATISTIC MEDIANS
    !          FROM THE UNIFORM (RECTANGULAR)
    !          DISTRIBUTION ON THE UNIT INTERVAL (0,1).
    !          THIS DISTRIBUTION HAS MEAN = 0.5
    !          AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
    !          THIS DISTRIBUTION HAS THE PROBABILITY
    !          DENSITY FUNCTION F(X) = 1.
    ! INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER 
    !                            OF UNIFORM ORDER STATISTIC MEDIANS
    !                            TO BE GENERATED. 
    ! OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
    !                            (OF DIMENSION AT LEAST N)
    !                            INTO WHICH THE GENERATED
    !                            UNIFORM ORDER STATISTIC MEDIANS
    !                            WILL BE PLACED.
    !  OUTPUT--    THE N ORDER STATISTIC MEDIANS
    !              FROM THE RECTANGULAR DISTRIBUTION ON (0,1).
    !  WRITTEN BY--JAMES J. FILLIBEN
    !              STATISTICAL ENGINEERING LABORATORY (205.03)
    !              NATIONAL BUREAU OF STANDARDS
    !              WASHINGTON, D. C. 20234
    !  MODIFIED BY -- BERNARD GINGOLD ON JULY 20 2017
    !==============================================================================84
    subroutine unimed(n,x,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          integer(I64P),              intent(in)    :: n
          real(R64P), dimension(:),   intent(inout) :: x
          logical(I32P), dimension(5),intent(inout) :: fp_flags
          ! Locals
          real(R64P)    :: an,gam,ai
          integer(I64P) :: i,imax,irev,nhalf,nevodd
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          if(n.LT.1 .OR. n.EQ.1 .OR. n.NE.SIZE(x)) then
              write(ERROR_UNIT,*) "===============ERROR=============="
              write(ERROR_UNIT,*) " (   Invalid argument in unimed  ) "
              write(ERROR_UNIT,10) n
10            format("value of n=",I24.20)
              write(ERROR_UNIT,*) "==================================" 
              return
          else
              if(ANY(fp_flags)) then
                  fp_flags = .false.
              end if
              an = DBLE(n)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF              
              x(n) = 0.5_R64P**(1._R64P/an)
              x(1) = 1._R64P-x(n)
              nhalf = (n/2)+1
              nevodd = 2*(n/2)
              if(n.NE.nevodd) x(nhalf) = 0.5_R64P
              if(n.LE.3) return
              !     COMPUTE THE MEDIANS FOR THE OTHER ORDER STATISTICS
              gam = 0.3175_R64P
              imax = n/2
              do i = 1, imax
                 ai = DBLE(i)
                 irev = n-i+1
                 x(i) = (ai-gam)/(an-2._R64P*gam+1._R64P)
                 x(irev) = 1._R64P-x(i)
              end do
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " unimed: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF              
           end if   
    end subroutine
    
    !==============================================================================84
    !  COMPUTE CLASSICAL MEAN AND STANDARD DEVIATION OF DATA BETWEEN
    !  POSITIONS X(NLO) AND X(NHI) INCLUSIVE
    !  MODIFIED BY BERNARD GINGOLD ON JULY 12 2017
    !==============================================================================84
    subroutine meansd(x,nlo,nhi,amean,sd,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:),    intent(in)    :: x
!DIR$     ASSUME_ALIGNED x:32
          integer(I32P),               intent(in)    :: nlo,nhi
          real(R64P),                  intent(inout) :: amean,sd
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          integer(I32P)                              :: i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of argument: fp_flags
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          if(array1D_not_alloc(x) .EQ. .true.) then
              iflag =7
              return
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
!DIR$     SIMD
          do i = nlo, nhi
              amean = amean+x(i)       ! amean should be scalar expanded
          end do
          amean = amean/DBLE(nhi-nlo+1)
!DIR$     SIMD
          do i = nlo, nhi
              sd = sd+(x(i)-amean)**2   ! sd should be scalar expanded
          end do
          sd = DSQRT(sd/DBLE(nhi-nlo))
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " meansd: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
    
    !==============================================================================84
    !    MATINV   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !        DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !        MARYLAND  20899
    !
    !    FOR: COMPUTING THE INVERSE OF A GENERAL N BY N MATRIX IN PLACE,
    !          I.E., THE INVERSE OVERWRITES THE ORIGINAL MATRIX.  THE STEPS 
    !          OF THE ALGORITHM ARE DESCRIBED BELOW AS THEY OCCUR.  ROW
    !          INTERCHANGES ARE DONE AS NEEDED IN ORDER TO INCREASE THE
    !          ACCURACY OF THE INVERSE MATRIX.  WITHOUT INTERCHANGES THIS
    !          ALGORITHM WILL FAIL WHEN ANY OF THE LEADING PRINCIPAL
    !          SUBMATRICES ARE SINGULAR OR WHEN THE MATRIX ITSELF IS
    !          SINGULAR.  WITH INTERCHANGES THIS ALGORITHM WILL FAIL ONLY
    !          WHEN THE MATRIX ITSELF IS SINGULAR.  THE LEADING PRINCIPAL
    !
    !                                [A B C]
    !     SUBMATRICES OF THE MATRIX  [D E F]  ARE  [A]  AND  [A B] .
    !                                [G H I]                 [D E]
    !
    !     SUBPROGRAMS CALLED: -NONE-
    !
    !     CURRENT VERSION COMPLETED JANUARY 15, 1987
    !     MODIFIED BY BERNARD GINGOLD ON JULY 12 2017
    !     REFERENCE: STEWART, G.W., 'INTRODUCTION TO MATRIX COMPUTATIONS',
    !      ACADEMIC PRESS, INC., 1973
    !
    !      DEFINITION OF PASSED PARAMETERS
    !
    !      * A = MATRIX (SIZE NXN) TO BE INVERTED (REAL)
    !
    !      * LDA = LEADING DIMENSION OF MATRIX A [LDA>=N] (INTEGER)
    !
    !      * N = NUMBER OF ROWS AND COLUMNS OF MATRIX A (INTEGER)
    !
    !        IFLAG = ERROR INDICATOR ON OUTPUT (INTEGER)   INTERPRETATION:
    !             -8 -> HW FP EXCEPTION HAS OCCURRED DUTING COMPUTATION
    !             -7 -> UNALLOCATED ALLOCATABLE ARRAY A 
    !             -2 -> TOO MANY ROW INTERCHANGES NEEDED - INCREASE MX
    !             -1 -> N>LDA
    !              0 -> NO ERRORS DETECTED
    !              K -> MATRIX A FOUND TO BE SINGULAR AT THE KTH STEP OF
    !                   THE CROUT REDUCTION (1<=K<=N)
    !
    !       * INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !==============================================================================84
    subroutine matinv(a,n,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:),  intent(inout)  :: a
!DIR$     ASSUME_ALIGNED a:32
          integer(I32P),               intent(in)     :: n
          integer(I32P),               intent(inout)  :: iflag
          logical(I32P), dimension(5), intent(inout)  :: fp_flags
          ! Locals
          integer(I32P), parameter       :: MX = 100
          integer(I32P), dimension(MX,2) :: iex
          integer(I32P)                  :: i,k,j,L,nex
          real(R64P)                     :: s,q,r
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of argument: fp_flags
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          if(array2D_not_alloc(a) .EQ. .true.) then
              iflag = -7
              return
          end if
          if(n .GT. SIZE(a,dim=1)) then
              iflag = -1
              return
          end if
          !   COMPUTE A = LU BY THE CROUT REDUCTION WHERE L IS LOWER TRIANGULAR
          !   AND U IS UNIT UPPER TRIANGULAR (ALGORITHM 3.4, P. 138 OF THE
          !   REFERENCE)
          nex = 0
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF  
k_loop:   do k = 1, n  
             do i = k, n
                 s = a(i,k)
                 do L = 1, k-1
                     s = s-a(i,L)*a(L,k)
                 end do
                 a(i,k) = s
             end do
             !    INTERCHANGE ROWS IF NECESSARY
             q = 0.0_R64P
             L = 0
!DIR$        SIMD VECTORLENGTHFOR(kind=8))
             do i = k, N
                 r = DABS(a(i,k))
                 if(r .GT. q) then
                     q = r
                     L = i
                 end if
             end do
             if(L .EQ. 0) then
                 iflag = k
                 return
             end if
             if(L .NE. k) then
                 nex = nex+1
                 if(nex .GT. MX) then
                     iflag = -2
                     return
                 end if
                 iex(nex,1) = k
                 iex(nex,2) = L
                 do j = 1, n
                     q = a(k,j)
                     a(k,j) = a(L,j)
                     a(L,j) = q
                 end do
             end if
             ! END ROW INTERCHANGE SECTION
             do j = k+1, n
                 s = a(k,j)
                 do L = 1, k-1
                     s = s-a(k,L)*a(L,j)
                 end do
                 a(k,j) = s/a(k,k)
             end do
end do k_loop
             !  INVERT THE LOWER TRIANGLE L IN PLACE (SIMILAR TO ALGORITHM 1.5,
             !   P. 110 OF THE REFERENCE) 
             do k = n, 1, -1
                 a(k,k) = 1.0_R64P/a(k,k)
                 do i = k-1, 1, -1
                     s = 0.0_R64P
                     do j = i+1, k
                         s = s+a(j,i)*a(k,j)
                     end do
                     a(k,i) = -s/a(i,i)
                 end do
             end do
             !  INVERT THE UPPER TRIANGLE U IN PLACE (ALGORITHM 1.5, P. 110 OF
             !  THE REFERENCE) 
             do k = n, 1, -1
                 do i = k-1, 1, -1
                     s = a(i,k)
                     do j = i+1, k-1
                         s = s+a(i,j)*a(j,k)
                     end do
                     a(i,k) = -s
                 end do
             end do
             !  COMPUTE INV(A) = INV(U)*INV(L)
             do i = 1, n
                 do j = 1, n
                     if(j .GT. i) then
                         s = 0.0_R64P
                         L = j
                     else
                         s = A(i,j)
                         L = i+1
                     end if
                     do k = L, n
                         s = s+a(i,k)*a(k,j)
                     end do
                     a(i,j) = s
                 end do
             end do
             !  INTERCHANGE COLUMNS OF INV(A) TO REVERSE EFFECT OF ROW 
             !  INTERCHANGES OF A
             do i = nex, 1, -1
                 k = nex(i,1)
                 L = nex(i,2)
                 do j = 1, n
                     q = a(j,k)
                     a(j,k) = a(j,L)
                     a(j,L) = q
                 end do
             end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = -8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " matinv: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF                
    end subroutine
    
    !==============================================================================84
    !    MATIPD   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !             DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !             MARYLAND  20899
    !
    !   FOR: COMPUTING THE INVERSE OF A SYMMETRIC POSITIVE DEFINITE N BY N
    !        MATRIX IN PLACE, I.E., THE INVERSE OVERWRITES THE ORIGINAL
    !        MATRIX.  THE STEPS OF THE ALGORITHM ARE DESCRIBED BELOW AS
    !        THEY OCCUR.  ONLY THE LOWER TRIANGLE (INCLUDING THE DIAGONAL)
    !        OF THE INPUT MATRIX IS USED IN THE INVERSION.  THE COMPLETE
    !        INVERSE MATRIX IS RETURNED ON OUTPUT.  IF THE INPUT MATRIX IS
    !        NOT POSITIVE DEFINITE THE INVERSE CANNOT BE COMPUTED BY THE
    !        CHOLESKY FACTORIZATION USED HEREIN, THUS AN ERROR FLAG IS SET.
    !
    !   SUBPROGRAMS CALLED: -NONE-
    !
    !   CURRENT VERSION COMPLETED JANUARY 15, 1987
    !   MODIFIED BY BERNARD GINGOLD ON JULY 12 2017
    !
    !   REFERENCE: STEWART, G.W., 'INTRODUCTION TO MATRIX COMPUTATIONS',
    !          ACADEMIC PRESS, INC., 1973
    !     DEFINITION OF PASSED PARAMETERS
    !
    !      * A = SYMMETRIC POSITIVE DEFINITE MATRIX (SIZE NXN) TO BE INVERTED
    !           (REAL)
    !
    !      * LDA = LEADING DIMENSION OF MATRIX A [LDA>=N] (INTEGER)
    !
    !      * N = NUMBER OF ROWS AND COLUMNS OF MATRIX A (INTEGER)
    !
    !        IFLAG = ERROR INDICATOR ON OUTPUT (INTEGER)   INTERPRETATION: 
    !                -8 -> HW FP EXCEPTION HAS OCCURRED
    !                -7 -> UNALLOCATED ALLOCATABLE ARRAY a
    !                -1 -> N>LDA
    !                 0 -> NO ERRORS DETECTED
    !                 K -> MATRIX A FOUND NOT TO BE POSITIVE DEFINITE AT THE
    !                      KTH STEP OF THE CHOLESKY FACTORIZATION (1<=K<=N)
    !
    ! * INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !==============================================================================84
    subroutine matipd(a,n,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          real(R64P), dimension(:,:),  intent(inout) :: a
          integer(I32P),               intent(in)    :: n
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)    :: s
          integer(I32P) :: i,j,k
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check on argument fp_flags
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          if(array2D_not_alloc(a) .EQ. .true.) then
              iflag = -7
              return
          end if
          if(n .GT. SIZE(a,dim=1)) then
              iflag = -1
              return
          end if
          !  COMPUTE THE LOWER TRIANGULAR MATRIX L OF THE CHOLESKY
          !  FACTORIZATION A=LL' (ALGORITHM 3.9, P. 142 OF THE REFERENCE)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
k_loop1:   do k = 1, n
             do i = 1, k-1
                 s = a(k,i)
                 do j = 1, i-1
                     s = s-a(i,j)*a(k,j)
                 end do
                 a(k,i) = s/a(i,i)
             end do
             s = a(k,k)
!DIR$        SIMD VECTORLENGTHFOR(REAL(kind=8))
             do j = 1, k-1
                 s = s-a(k,j)**2
             end do
             if(s .LE. ZEROR64) then
                 iflag = k
                 return
             end if
             a(k,k) = DSQRT(s)
end do k_loop1
             ! INVERT THE LOWER TRIANGLE L IN PLACE (SIMILAR TO ALGORITHM 1.5,
             ! P. 110 OF THE REFERENCE) 

k_loop2:   do k = n, 1, -1
                 a(k,k) = 1.0_R64P/a(k,k)
                 do i = k-1, 1, -1
                     s = 0.0_R64P
!DIR$                SIMD VECTORLENGTHFOR(REAL(kind=8))
                     do j = i+1, k
                         s = s+a(j,i)*a(k,j)
                     end do
                     a(k,i) = -s/a(i,i)
                 end do
end do k_loop2
              !   COMPUTE INV(A)=INV(L)'*INV(L)
           do i = 1, n
               do j = i, n
                   s = 0.0_R64P
!DIR$              SIMD VECTORLENGTHFOR(REAL(kind=8))                   
                   do k = j, n
                       s = s+a(k,i)*a(k,j)
                   end do
                   a(i,j) = s
                   a(j,i) = s
               end do
           end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = -8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " matipd: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF            
             
    end subroutine
    
    !==============================================================================84
    ! MATXXI   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !          DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !          MARYLAND  20899
    !
    ! FOR:     COMPUTING THE INVERSE OF X'X AND THE DETERMINANT OF X'X WHERE
    !          X IS AN N BY M MATRIX WITH N >= M.  THE INVERSE IS RETURNED
    !          IN THE FIRST M ROWS AND M COLUMNS OF THE ORIGINAL MATRIX X.
    !          ENTRIES BELOW ROW M ARE LIKELY TO BE CHANGED.  NOTE THAT ROWS
    !          N+1 AND N+2 OF MATRIX X ARE USED FOR SCRATCH AREA, THEREFORE 
    !          LDX >= N+2.  THE STEPS OF THE ALGORITHM ARE DESCRIBED BELOW
    !          AS THEY OCCUR.
    !
    ! SUBPROGRAMS CALLED: -NONE-
    !
    ! CURRENT VERSION COMPLETED APRIL 8, 1988
    ! MODIFIED BY BERNARD GINGOLD ON JULY 12 2017
    ! REFERENCE: STEWART, G.W., 'INTRODUCTION TO MATRIX COMPUTATIONS',
    !          ACADEMIC PRESS, INC., 1973
    !
    ! DEFINITION OF PASSED PARAMETERS: 
    !
    ! * X = MATRIX (SIZE NXM) FOR WHICH INV(X'X) AND DET(X'X) ARE TO
    !       BE COMPUTED (REAL)
    !
    ! * LDX = LEADING DIMENSION OF X [LDX>=N+2] (INTEGER)
    ! 
    ! * N = NUMBER OF ROWS IN MATRIX X (INTEGER)
    !
    ! * M = NUMBER OF COLUMNS IN MATRIX X [M<=N] (INTEGER)
    !
    ! DET = DETERMINANT OF (X'X) (REAL) 
    !
    ! IFLAG = ERROR INDICATOR ON OUTPUT (INTEGER)   INTERPRETATION: 
    !       -8 -> HW FP EXCEPTION(S) SIGNALLED(QUIETLY)
    !       -7 -> UNALLOCATED ALLOCATABLE ARRAY
    !       -2 -> M>N
    !       -1 -> LDX<N+2
    !        0 -> NO ERRORS DETECTED
    !        K -> THE KTH DIAGONAL ELEMENT OF THE TRIANGULAR MATRIX R,
    !             FROM THE QR FACTORIZATION, IS ZERO
    !
    ! * INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !==============================================================================84
    subroutine matxxi(x,n,m,det,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          real(R64P), dimension(:,:),  intent(inout) :: x
!DIR$     ASSUME_ALIGNED x:32
          integer(I32P),               intent(in)    :: n,m
          real(R64P),                  intent(inout) :: det
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          integer(I32P)                              :: n1,n2,L,i,j,k
          real(R64P)                                 :: s,t,q,e
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of arguement: fp_flags
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          if(array2D_not_alloc(x) .EQ. .true.) then
             iflag = -7
             return
          end if
          if(n .LT. m) then
              iflag = -2
              return
          end if
          !   DEFINE SOME CONSTANTS
          n1 = n+1
          n2 = n+2
          !  COMPUTE QR FACTORIZATION OF MATRIX X (ALGORITHM 3.8, P. 236 OF
          !  THE REFERENCE) 
          L = MIN0(n-1,m)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF 
k_loop:   do k = 1, L
             e = 0.0_R64P
             do i = k, n
                 q = DABS(x(i,k))
                 if(q .GT. 0.0_R64P) e = q
             end do
             if(e .EQ. 0.0_R64P) then
                 x(n+1,k) = 0.0_R64P
             else
                 do i = k, n
                     x(i,k) = x(i,k)/e
                 end do
                 s = 0.0_R64P
!DIR$            SIMD VECTORLENGTHFOR(REAL(KIND=8))
                 do i = k, n
                     s = s+x(i,k)**2    ! Scalar expansion
                 end do
                 s = DSIGN(SQRT(s),x(k,k))
                 x(k,k) = x(k,k)+s
                 x(n1,k) = s*x(k,k)
                 x(n2,k) = -e*s
                 do j = k+1, m
                     t = 0.0_R64P
!DIR$            SIMD VECTORLENGTHFOR(REAL(KIND=8))                     
                     do i = k, n
                         t = t+x(i,k)*x(i,j)
                     end do
                     t = t/x(n1,k)
 !DIR$           SIMD VECTORLENGTHFOR(REAL(KIND=8))                    
                     do i = k, n
                         x(i,j) = x(i,j)-t*x(i,k)
                     end do
                 end do
             end if
end do k_loop
             if(n .EQ. m) x(n2,n) = x(n,n)
             !  MOVE DIAGONAL ELEMENTS BACK TO DIAGONAL AND COMPUTE DETERMINANT
             if(det .EQ. 0.0_R64P) det = 1.0_R64P ! sanity check
             do j = 1, m
                 x(j,j) = x(n2,j)
                 det = det*x(j,j)**2
                 if(x(j,j) .EQ. 0.0_R64P) then
                     iflag = j
                     return
                 end if
             end do
            ! INVERT UPPER TRIANGULAR MATRIX R IN PLACE (ALGORITHM 1.5, P. 110 
            ! OF THE REFERENCE) 
            do k = m, 1, -1
                x(k,k) = 1.0_R64P/x(k,k)
                do i = k-1, 1, -1
                    s = 0.0_R64P
!DIR$               SIMD VECTORLENGTHFOR(REAL(KIND=8))                    
                    do j = i+1, k
                        s = s-x(i,j)*x(j,k)
                    end do
                    x(i,k) = s/x(i,i)
                end do
            end do
            !  COMPUTE INV(X'X) = INV(R)*INV(R)'
            do i = 1, m
                do j = i, m
                    s = 0.0_R64P
                    do k = j, m
                        s = s+x(i,k)*x(j,k)
                    end do
                    x(i,j) = s
                    x(j,i) = s
                end do
            end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = -8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " matxxi: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF              
    end subroutine
    
    !==============================================================================84
    !   PPFBET   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !        DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !        MARYLAND 20899
    !
    !    FOR: EVALUATING THE INVERSE CUMULATIVE DISTRIBUTION FUNCTION
    !         (PERCENT POINT FUNCTION) OF THE BETA(P,Q) DISTRIBUTION.
    !         FOR A GIVEN PROBABILITY PR.  THE PERCENT POINT X IS COMPUTED 
    !         TO A SPECIFIED ABSOLUTE ACCURACY TOL WHEN POSSIBLE. THE
    !         METHOD OF BRENT, AS DESCRIBED IN THE REFERENCES BELOW, IS
    !         USED TO COMPUTE THE APPROXIMATE ZERO OF I(X,P,Q)-PR WHERE
    !         I(X,P,Q) IS THE CUMULATIVE DISTRIBUTION FUNCTION OF THE
    !         BETA(P,Q) DISTRIBUTION EVALUATED AT X.  THIS METHOD DOES
    !         NOT REQUIRE DERIVATIVES.
    !
    !    NOTE: THE CONSTANT EPS FOR MACHINE FLOATING POINT PRECISION IS
    !          MACHINE DEPENDENT.
    !
    !    SUBPROGRAMS CALLED: CDFBET (BETA CUMULATIVE DISTRIBUTION FUNCTION)
    !
    !    CURRENT VERSION COMPLETED OCTOBER 13, 1987
    !
    !    REFERENCES: 
    !
    !    1) PRESS, WILLIAM H., FLANNERY, BRIAN P., TEUKOLSKY, SAUL A.,
    !       AND VETTERLING, WILLIAM T., 'NUMERICAL RECIPES - THE ART OF
    !       SCIENTIFIC COMPUTING', CAMBRIDGE UNIVERSITY PRESS, 1986,
    !        PP. 251-254. 
    !
    !    2) BRENT, RICHARD P., 'ALGORITHMS FOR MINIMIZATION WITHOUT
    !       DERIVATIVES', PRENTICE-HALL, 1973, CH. 3-4.
    !     DEFINITION OF PASSED PARAMETERS: 
    !   * PR = A PROBABILITY VALUE IN THE INTERVAL [0,1] (REAL)
    !
    !   * P = THE FIRST PARAMETER (>0) OF THE BETA(P,Q) DISTRIBUTION
    !       (REAL)
    !
    !   * Q = THE SECOND PARAMETER (>0) OF THE BETA(P,Q) DISTRIBUTION
    !       (REAL)
    !
    !   * TOL = THE REQUIRED ACCURACY (>=1.0E-8) OF THE PERCENT POINT X
    !       (REAL)
    !
    !   IFLAG = ERROR INDICATOR ON OUTPUT (INTEGER)   INTERPRETATION: 
    !         0 -> NO ERRORS DETECTED
    !       1,2 -> ERROR FLAGS FROM SUBROUTINE CDFBET
    !         3 -> PR<0 OR PR>1
    !         4 -> P<=0 OR Q<=0
    !         5 -> TOL<1.0E-8 
    !         6 -> THE CDF'S AT THE ENDPOINTS HAVE THE SAME SIGN - NO 
    !              VALUE OF X IS DEFINED (THIS SHOULD NEVER OCCUR)
    !         7 -> MAXIMUM ITERATIONS EXCEEDED - CURRENT VALUE OF X
    !              RETURNED
    !         8 -> HW FP EXCEPTION(S) SIGNALLED(QUIETLY)
    !
    !   X = THE COMPUTED PERCENT POINT (REAL)
    !
    !  * INDICATES PARAMETERS REQUIRING INPUT VALUES    
    !
    !==============================================================================84
    subroutine ppfbet(pr,p,q,tol,iflag,x,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          real(R64P),                  intent(in)    :: pr,p,q,tol
          integer(I32P),               intent(inout) :: iflag
          real(R64P),                  intent(inout) :: x
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)                                 :: a,b,fa,fb,fc, &
                                                        c,d,e,tol1, &
                                                        xm,s,u,v,r,cdf
          integer(I32P), parameter                   :: ITMAX = 50
          integer(I32P)                              :: iter
          real(R64P),    parameter                   :: EPS = MACHEPSf64
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check on argument: fp_flags
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          if(pr.LT.ZEROR64 .OR. pr.GT.1.0_R64P) then
             iflag = 3
             return
          end if
          if(DMIN1(p,q) .LE. ZEROR64) then
              iflag = 4
              return
          end if
          if(tol .LT. MODSTATOL) then
              iflag = 5
              return
          end if
          cdf = 0.0_R64P
          a = 0.0_R64P
          b = 1.0_R64P
          fa = -pr
          fb = 1.0_R64P-pr
          !     CHECK FOR ZERO BEING BRACKETED
          if(fb*fa .GT. ZEROR64) then
              iflag = 6
              return
          end if
          fc = fb
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
          do iter = 1 ,ITMAX
              if(fb*fc .GT. ZEROR64) then
                  !  RENAME A,B,C AND ADJUST BOUNDING INTERVAL D
                  c = a
                  fc = fa
                  d = b-a
                  e = d
              end if
              if(DABS(fc) .LT. DABS(fb)) then
                  a = b
                  b = c
                  c = a
                  fa = fb
                  fb = fc
                  fc = fa
              end if
              !    CONVERGENCE CHECK
              tol1 = 2.0_R64P*eps*DABS(b)+0.5_R64P*tol
              xm   = 0.5_R64P*(c-b)
              if(DABS(xm).LE.tol1 .OR. fb.EQ.ZEROR64) THEN
                  X = B
                  return
              end if
              if(DABS(e).GE.tol1 .AND. DABS(fa).GT.DABS(fb)) then
                  ! ATTEMPT INVERSE QUADRATIC INTERPOLATION
                  s = fb/fa
                  if(a .EQ. c) then
                      u = 2.0_R64P*xm*s
                      v = 1.0_R64P-s
                  else
                      v = fa/fc
                      r = fb/fc
                      u = s*(2.0_R64P*xm*v*(v-r)-(b-a)*(r-1.0_R64P))
                      v = (v-1.0_R64P)*(r-1.0_R64P)*(s-1.0_R64P)
                  end if
                  ! CHECK WHETHER IN BOUNDS
                  if(u .GT. ZEROR64) v = -v
                     u = DABS(u)
                     if(2.0_R64P.LT.DMIN1(3.0_R64P*xm*v-DABS(tol1*v),DABS(e*v))) then
                         ! ACCEPT INTERPOLATION
                         e = d
                         d = u/v
                     else
                         !  INTERPOLATION FAILED, USE BISECTION
                         d = xm
                         e = d
                     end if
                  else
                      !  BOUNDS DECREASING TOO SLOWLY, USE BISECTION
                        d = xm
                        e = d
                  end if
                  !   MOVE LAST BEST GUESS TO A
                  a = b
                  fa = fb
                  !   EVALUATE NEW TRIAL ZERO
                  if(DABS(d) .GT. tol1) then
                      b = b+d
                  else
                      b = b+DSIGN(tol1,xm)
                  end if
                  call cdfbet(b,p,q,eps,iflag,cdf,fp_flags)
                  if(iflag .NE. 0) return
                  fb = cdf-pr
              end do
              iflag = 7
              x = b
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = -8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " ppfbet: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF                     
              
    end subroutine
    
    !==============================================================================84
    !   UPDATE THE MINIMUM OR MAXIMUM VALUE OF Z IF APPROPRIATE
    !==============================================================================84
    subroutine minmax(x,y,z,xmin,ymin,zmin,xmax,ymax,zmax)
          implicit none
          real(R64P), intent(in)    :: x,y,z
          real(R64P), intent(inout) :: xmin,ymin,zmin, &
                                       xmax,ymax,zmax
          ! Start of executable statements
          if(z .LT. zmin) then
              xmin = x
              ymin = y
              zmin = z
          end if
          if(z .GT. zmax) then
              xmax = x
              ymax = y
              zmax = z
          end if
    end subroutine      
    
    !==============================================================================84
    !    COMPUTE MEDIAN ABSOLUTE DEVIATION (MAD) OF X FROM C, AN ESTIMATE 
    !    OF THE CENTER OF DATA, BETWEEN X(NLO) AND X(NHI) INCLUSIVE.  VECTOR
    !    X IS EXPECTED TO BE SORTED AND IS UNCHANGED BY THIS SUBROUTINE.
    !    NOTE: IF THE NUMBER OF ENTRIES OF INTEREST, N, IS EVEN THE MAD IS
    !    THE N/2 LARGEST DEVIATION, A SLIGHT OVERESTIMATE.
    !    MODIFIED BY BERNARD GINGOLD ON JULY 13 2017
    !    ADDED:
    !    IFLAG -> 8 WHEN ARRAY IS UNALLOCATED
    !==============================================================================84
    subroutine mad(x,nlo,nhi,c,amad,iflag)
          implicit none
          real(R64P),    intent(inout) :: x
          integer(I32P), intent(in)    :: nlo,nhi
          real(R64P),    intent(in)    :: c
          real(R64P),    intent(inout) :: amad
          integer(I32P), intent(inout) :: iflag
          ! Locals
          integer(I32P)                :: mlo,mhi,k,i
          ! Start of executable statements
          ! Sanity check on allocation status of array x
          if(array1D_not_alloc(x) .EQ. .true.) then
              iflag = 8
              return
          end if
          mlo = nlo
          mhi = nhi
          k = (mhi-mlo)/2
          do i = 1, k
              if(x(mhi)+x(mlo) .GT. 2.0_R64P*c) then
                  mhi = mhi - 1
              else
                  mlo = mlo+1
                  exit
              end if
          end do
          amad = DMAX1(DABS(x(mhi)-c),DABS(x(mlo)-c))
    end subroutine
    
    !==============================================================================84
    !    RDBETA   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !            DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !
    !    FOR: GENERATING A RANDOM DEVIATE FROM THE BETA(P,Q) DISTRIBUTION
    !         WHERE P,Q > 0.  IF THIS CONDITION IS NOT MET THEN PROGRAM
    !         EXECUTION IS TERMINATED.
    !
    !    IF MIN(P,Q) > 1 THEN THE CHENG ALGORITHM BB IS USED.
    !    IF 0 < MIN(P,Q) <= 1 THEN THE CHENG ALGORITHM BC IS USED.
    !
    !    SUBPROGRAMS CALLED: RDUNI (STSPAC) - UNIFORM(0,1) GENERATOR
    !
    !    CURRENT VERSION COMPLETED JULY 30, 1984
    !    MODIFIED BY BERNARD GINGOLD ON JULY 13 2017
    !
    !    REFERENCE: KENNEDY, WILLIAM J. AND GENTLE, JAMES E., "STATISTICAL 
    !               COMPUTING", MARCEL DEKKER, INC., 1980, PP. 216-219
    !
    !    REFERENCE: CHENG, R.C.H., "GENERATING BETA VARIATES WITH
    !               NONINTEGRAL SHAPE PARAMETERS", COMMUNICATIONS OF
    !               THE ACM, VOL. 21, NO. 4, APRIL 1978, PP. 317-322.
    !
    !==============================================================================84
       function rdbeta(p,q) result(rdb)
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), intent(in) :: p,q
          ! Locals
          real(R64P)             :: rdb
          real(R64P)             :: h,a,b,alpha,beta,y,  &
                                    gamma,u1,u2,v,w,z,r, &
                                    s,t,delta,kappa1,kappa2
          real(R64P), parameter :: COEF1 = 1.3862944_R64P
          real(R64P), parameter :: COEF2 = 2.609438_R64P
          real(R64P), parameter :: COEF3 = 0.013888889_R64P
          real(R64P), parameter :: COEF4 = 0.041666667_R64P
          real(R64P), parameter :: COEF5 = 0.77777778_R64P
          real(R64P), parameter :: COEF6 = 0.5_R64P
          real(R64P), parameter :: COEF7 = 2.0_R64P
          ! Start of executable statements
          h = DMIN1(p,q)
          if(h .GT. ONER64P) then
              !  CHENG ALGORITHM (BB)
              a = h
              b = DMAX1(p,q)
              alpha = a+b
              beta = DSQRT((alpha-COEF7)/(COEF7*a*b-alpha))
              gamma = a+ONER64P/beta
              ! STEP 1
              
              u1 = rduni(0) ! MUST BE IMPLEMENTED
              u2 = rduni(0) ! MUST BE IMPLEMENTED : rduni
              v = beta*DLOG(u1/(ONER64P-u1))
              w = a*DEXP(v)
              z = u2*u1**2
              r = gamma*v-COEF1
              s = a+r-w
              do while(r+alpha*DLOG(alpha/(b+w)).LT.t)   ! <- STEP 4
                   u1 = rduni(0) ! MUST BE IMPLEMENTED
                   u2 = rduni(0) ! MUST BE IMPLEMENTED : rduni
                   v = beta*DLOG(u1/(ONER64P-u1))
                   w = a*DEXP(v)
                   z = u2*u1**2
                   r = gamma*v-COEF1
                   s = a+r-w
                   ! STEP 2
                   if(s+COEF2 .GE. COEF6*z) exit
                   ! STEP 3
                   t = DLOG(z)
                   if(s .GE. t) exit
              end do
              if(a .EQ. p) then
                  rdb = w/(b+w)
              else
                  rdb = b/(b+w)
              end if
          else if (h .GT. ZEROR64) then
               ! CHENG ALOGIRTHM
              a = DMAX1(p,q)
              b = h
              alpha = a+b
              beta = ONER64P/b
              delta = ONER64P+a-b
              kappa1 = delta*(COEF3+COEF4*b)/(a*beta-COEF5)
              kappa2 = QUARTER64P+( COEF6+QUARTER64P/delta)*b
              ! STEP 1
30            u1 = rduni(0)
              u2 = rduni(0)
              if(u1 .GE. COEF6) go to 40
              ! STEP 2
              y = u1*u2
              z = u1*y
              if(QUARTER64P*u2+z-y .GE. kappa1) then
                  go to 30
              else
                  go to 50
              end if
              ! STEP 3
40            z = u2*u1**2
              if(z .LE. QUARTER64P) then
                  v = beta*DLOG(u1/(ONER64P-u1))
                  w = a*DEXP(v)
                  if(a .EQ. p) then
                      rdb = w/(b+w)
                  else
                      rdb = b/(b+w)
                  end if
               end if
               if(z .HE. kappa2) go to 30
               ! STEP 5
50             v = beta*DLOG(u1/(ONER64P-i1))
               w = a*DEXP(v)
               if(alpha*(DLOG(alpha/(b+w))+v)-COEF1.LT.DLOG(z)) go to 30
          else
               ! PARAMETERS OUT OF RANGE
              write(ERROR_UNIT,*) "************ ERROR **************"
              write(ERROR_UNIT,*) " rdbeta: EITHER P OR Q IS <= 0.0"
              write(ERROR_UNIT,*) "*********************************"
              ERROR STOP "EXECUTION STOPPED IN FUNCTION: RDBETA"
          end if
              
    end function
       
    !==============================================================================84
    !    RDUNI   OBTAINED BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !             DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !             MD 20899 FROM THE SOFTWARE LIBRARY CMLIB AT NBS 
    !
    !     FOR: GENERATING UNIFORM PSEUDO-RANDOM DEVIATES IN THE INTERVAL
    !          [0,1) USING A LAGGED FIBONACCI GENERATOR.  THE INITIAL CALL
    !          TO RDUNI SHOULD BE OF THE FORM Z=RDUNI(K) WHERE K IS A
    !          POSITIVE INTEGER.  FURTHER CALLS SHOULD BE OF THE FORM
    !          Z=RDUNI(0).  MDIG IS A LOWER BOUND ON THE NUMBER OF BINARY
    !          DIGITS AVAILABLE FOR REPRESENTING INTEGERS, INCLUDING THE
    !          SIGN BIT.  THIS VALUE MUST BE AT LEAST 16.  TO ALLOW THE
    !          LONGEST SEQUENCE OF RANDOM NUMBERS WITHOUT CYCLING, SET
    !          MDIG TO ITS MAXIMUM VALUE.
    !
    !    THE NBS CENTRAL COMPUTERS WERE FOUND TO GENERATE DEVIATES
    !    AT THE FOLLOWING RATES USING THE INDICATED MAXIMUM VALUES
    !    OF MDIG: 
    !
    !         CYBER 180/855 ->   100,050 DEV/SEC   (MDIG=49)
    !             CYBER 205/622 ->   105,471 DEV/SEC   (MDIG=48)
    !
    !    SUBPROGRAMS CALLED: -NONE-
    !
    !    CURRENT VERSION COMPLETED FEBRUARY 9, 1987
    !    MODIFIED BY BERNARD GINGOLD ON JULY 13 2017
    !
    !    REFERENCE: MARSAGLIA, GEORGE, 'COMMENTS ON THE PERFECT UNIFORM
    !               RANDOM NUMBER GENERATOR', UNPUBLISHED NOTES, 
    !               WASHINGTON STATE UNIVERSITY.
    !==============================================================================84
    pure function rduni(jd)  result(rdu)
          implicit none
          integer(I32P), intent(in) :: jd
          ! Locals
          real(R64P)                   :: rdu
          integer(I32P), dimension(17) :: m = [30788,23052,2053,19346,10646,19427,23975,19049,10949, &
                                               19693,29746,26748,2796,23890,29168,31924,16499]
          integer(I32P)                :: m1 = 32767,m2=256,i=5,j=17,mdig=49
          integer(I32P)                :: jseed,k0,k1,j0
          ! Start of executable statements
          if(jd .EQ. 0) then
              k = m(i)-m(j)
              if(k .LT. 0) k = k+m1
              m(j) = k
              i = i-1
              if(i .EQ. 0) i = 17
              j = j-1
              if(j .EQ. 0) j = 17
              rdu = DBLE(k)/DBLE(m1)
          else
              m1 = 2**(mdig-2)+(2**(mdig-2)-1)
              m2 = 2**(mdig2)
              jseed = MIN0(IABS(jd),m1)
              if(MOD(jseed,2).EQ.0) jseed = jseed-1
              k0 = MOD(9069,m2)
              k1 = 9069/m2
              j0 = MOD(jseed,m2)
              j1 = jseed/m2
              do i = 1, 17
                  jseed = j0*k0
                  j1 = MOD(jseed/m2+j0*k1+j1*k0,m2/2)
                  j0 = MOD(jseed,m2)
                  m(i) = j0+m2*j1
              end do
              i = 5
              j = 17
              k = m(i)-m(j)
              if(k .LT. 0) k = k+m1
              m(j) = k
              i = i-1
              if(i .EQ. 0) i = 17
              j = j-1
              if(j .EQ. 0) j = 17
              rdu = DBLE(k)/DBLE(m1)
          end if
          
    end function
    
    !==============================================================================84
    !
    !   RDNOR    OBTAINED BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !            DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !            MD 20899 FROM THE SOFTWARE LIBRARY CMLIB AT NBS 
    !
    !    FOR: GENERATING NORMAL PSEUDO-RANDOM DEVIATES WITH MEAN ZERO AND
    !         VARIANCE ONE.  THE INITIAL CALL TO RDNOR SHOULD BE OF THE
    !         FORM Z=RDNOR(K) WHERE K IS A POSITIVE INTEGER.  FURTHER
    !         CALLS SHOULD BE OF THE FORM Z=RDNOR(0).  MDIG IS A LOWER
    !         BOUND ON THE NUMBER OF BINARY DIGITS AVAILABLE FOR 
    !         REPRESENTING INTEGERS, INCLUDING THE SIGN BIT.  THIS VALUE
    !         MUST BE AT LEAST 16.  TO ALLOW THE LONGEST SEQUENCE OF
    !         RANDOM DEVIATES WITHOUT CYCLING, SET MDIG TO ITS MAXIMUM
    !         VALUE.
    !
    !         THE NBS CENTRAL COMPUTERS WERE FOUND TO GENERATE DEVIATES
    !         AT THE FOLLOWING RATES USING THE INDICATED MAXIMUM VALUES
    !         OF MDIG: 
    !
    !         CYBER 180/855 ->   54,893 DEV/SEC   (MDIG=49) 
    !         CYBER 205/622 ->   89,098 DEV/SEC   (MDIG=48) 
    !
    !    MODIFIED BY BERNARD GINGOLD ON JULY 14 2017
    !
    !    SUBPROGRAMS CALLED: RDUNI 
    !    REFERENCE: MARSAGLIA & TSANG, 'FAST, EASILY IMPLEMENTED METHOD
    !          FOR SAMPLING FROM DECREASING OR SYMMETRIC UNIMODAL
    !          DENSITY FUNCTIONS', SIAM J SISC 1983.
    !==============================================================================84
    pure function rdnor(jd) result(prd)
          implicit none
          integer(I32P), intent(in) :: jd
          ! Locals
          real(R64P), dimension(65) :: v = [0.3409450_R64P,0.4573146_R64P, &
                                            0.5397793_R64P,0.6062427_R64P,0.6631691_R64P, &
                                            0.7136975_R64P,0.7596125_R64P,0.8020356_R64P,0.8417227_R64P,0.8792102_R64P,0.9148948_R64P, & 
                                            0.9490791_R64P,0.9820005_R64P,1.0138492_R64P,1.0447810_R64P,1.0749254_R64P,1.1043917_R64P, &
                                            1.1332738_R64P,1.1616530_R64P,1.1896010_R64P,1.2171815_R64P,1.2444516_R64P,1.2714635_R64P, &
                                            1.2982650_R64P,1.3249008_R64P,1.3514125_R64P,1.3778399_R64P,1.4042211_R64P,1.4305929_R64P, &
                                            1.4569915_R64P,1.4834526_R64P,1.5100121_R64P,1.5367061_R64P,1.5635712_R64P,1.5906454_R64P, &
                                            1.6179680_R64P,1.6455802_R64P,1.6735255_R64P,1.7018503_R64P,1.7306045_R64P,1.7598422_R64P, & 
                                            1.7896223_R64P,1.8200099_R64P,1.8510770_R64P,1.8829044_R64P,1.9155830_R64P,1.9492166_R64P, &
                                            1.9839239_R64P,2.0198430_R64P,2.0571356_R64P,2.0959930_R64P,2.1366450_R64P,2.1793713_R64P, &
                                            2.2245175_R64P,2.2725185_R64P,2.3239338_R64P,2.3795007_R64P,2.4402218_R64P,2.5075117_R64P, &
                                            2.5834658_R64P,2.6713916_R64P,2.7769943_R64P,2.7769943_R64P,2.7769943_R64P,2.7769943_R64P ]
          real(R64P), dimension(65) :: w = [0.10405134E-04_R64P,0.13956560E-04_R64P,0.16473259E-04_R64P,0.18501623E-04_R64P, &
                                            0.20238931E-04_R64P,0.21780983E-04_R64P,0.23182241E-04_R64P,0.24476931E-04_R64P, &
                                            0.25688121E-04_R64P,0.26832186E-04_R64P,0.27921226E-04_R64P,0.28964480E-04_R64P, &
                                            0.29969191E-04_R64P,0.30941168E-04_R64P,0.31885160E-04_R64P,0.32805121E-04_R64P, &
                                            0.33704388E-04_R64P,0.34585827E-04_R64P,0.35451919E-04_R64P,0.36304851E-04_R64P, &
                                            0.37146564E-04_R64P,0.37978808E-04_R64P,0.38803170E-04_R64P,0.39621114E-04_R64P, &
                                            0.40433997E-04_R64P,0.41243096E-04_R64P,0.42049621E-04_R64P,0.42854734E-04_R64P, &
                                            0.43659562E-04_R64P,0.44465208E-04_R64P,0.45272764E-04_R64P,0.46083321E-04_R64P, &
                                            0.46897980E-04_R64P,0.47717864E-04_R64P,0.48544128E-04_R64P,0.49377973E-04_R64P, &
                                            0.50220656E-04_R64P,0.51073504E-04_R64P,0.51937936E-04_R64P,0.52815471E-04_R64P, &
                                            0.53707761E-04_R64P,0.54616606E-04_R64P,0.55543990E-04_R64P,0.56492112E-04_R64P, &
                                            0.57463436E-04_R64P,0.58460740E-04_R64P,0.59487185E-04_R64P,0.60546402E-04_R64P, &
                                            0.61642600E-04_R64P,0.62780711E-04_R64P,0.63966581E-04_R64P,0.65207221E-04_R64P, &
                                            0.66511165E-04_R64P,0.67888959E-04_R64P,0.69353880E-04_R64P,0.70922996E-04_R64P, &
                                            0.72618816E-04_R64P,0.74471933E-04_R64P,0.76525519E-04_R64P,0.78843526E-04_R64P, &
                                            0.81526890E-04_R64P,0.84749727E-04_R64P,0.84749727E-04_R64P,0.84749727E-04_R64P, &
                                            0.84749727E-04_R64P ]
          real(R64P)                   :: aa=12.37586_R64P,b=0.4878992_R64P,c=12.67706_R64P,rmax=3.0518509E-5_R64P, &
                                          c1=0.9689279_R64P,c2=1.301198_R64P,pc=0.1958303E-1_R64P,xn=2.776994_R64P,prd, &
                                          x,y,s
          integer(I32P), dimension(17) :: m = [ 30788,23052,2053,19346,10646,19427,23975,19049,10949, &
                                                19693,29746,26748,2796,23890,29168,31924,16499        ]
          integer(I32P)                :: m1=32767,m2=256,i1=5,j1=17,mdig=49,i,jseed,j,k0,k1,j0,j1
           ! Start of executable statements
           if(jd .NE. 0) go to 40
           !  EXECUTE THIS SEGMENT EACH CALL
10         i = m(i1)-m(j1)
           if(i .LT. 0) i = i+m1
           m(j1) = i
           i1 = i1-1
           if(i1 .EQ. 0) i1 = 17
           j1 = j1-1
           if(j1 .EQ. 0) j1 = 17
           j = MOD(i,64)+1
           prd = i*w(j+1)
           if(((i/m2)/2)*2.EQ.(i/m2)) prd = -prd
           if(DABS(prd) .LE. v(j)) return
           !  SLOW PART - AA IS A*F(0) 
           x = DABS(prd)-v(j))/(v(j+1)-v(j))
           y = rduni(0)
           s = x+y
           if(s .GT. c2) go to 30
           if(s .LE. c1) return
           if(y .GT. c-aa*DEXP(-HALFR64P*(b-b*x)**2)) go to 30
           if(DEXP(-HALFR64P*v(j+1)**2)+y*pc/v(j+1).LE.DEXP(-HALFR64P*prd**2)) return
           !    TAIL PART - 3.855849 IS 0.5*XN**2
20         s = xn-DLOG(rduni(0))/xn
           do while(3.855849_R64P+DLOG(rduni(0))-xn*s.GT.-HALFR64P**2) 
              s = xn-DLOG(rduni(0))/xn
           end do
           prd = DSIGN(s,prd)
           return
30         prd = DSIGN(b-b*x,prd)
           return
           ! EXECUTE THIS SEGMENT ONLY IF JD<>0 
40         m1 = 2**(mdig-2)+(2**(mdig-2)-1)
           m2 = 2**(mdig/2)
           jseed = MIN0(IABS(jd),m1)
           if(MOD(jseed,2) .EQ. 0) jseed = jseed-1
           k0 = MOD(9069,M2)
           k1 = 9069/M2
           j0 = MOD(jseed,m2)
           j1 = jseed/m2
           do i = 1, 17
               jseed = j0*k0
               j1 = MOD(jseed/m2+j0*k1+j1*k0,m2/2)
               j0 = MOD(jseed,m2)
               m(i) = j0+m2*j1
           end do
           j1 = 17
           i1 = 5
           rmax = ONER64/DBLE(m1)
           !  SEED UNIFORM (0,1) GENERATOR (JUST A DUMMY CALL)
           prd = rduni(0)
           do i = 1, 65
               w(i) = rmax*v(i)
           end do
           go to 10 
    end function
    
    !==============================================================================84
    !   RDNOR3   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !             DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !             MARYLAND  20899
    !
    !    FOR: GENERATING STANDARD NORMAL PSEUDO-RANDOM DEVIATES BY THE
    !         POLAR METHOD AS DESCRIBED IN THE REFERENCE BELOW.  UNIFORM
    !         PSEUDO-RANDOM DEVIATES IN THE INTERVAL (0,1) ARE SUPPLIED
    !         BY A WICHMAN-HILL GENERATOR WHICH REQUIRES THREE INTEGER
    !        ARGUMENTS IX, IY, AND IZ.  THE NBS CENTRAL COMPUTERS WERE
    !         FOUND TO GENERATE NORMAL DEVIATES AT THE FOLLOWING RATES: 
    !
    !              CYBER 180/855 ->  22,546 DEVIATES/SECOND 
    !              CYBER 205/622 ->  42,997 DEVIATES/SECOND  .
    !
    !         BEFORE THE FIRST CALL TO THIS ROUTINE THE INTEGERS IX, IY, AND
    !         IZ SHOULD BE SET TO VALUES WITHIN THE RANGE [1,30000].  AFTER
    !         THAT THEIR VALUES SHOULD NOT BE CHANGED EXCEPT BY THIS ROUTINE.
    !
    !    SUBPROGRAMS CALLED: RDUNWH (WICHMAN-HILL UNIFORM GENERATOR)
    !
    !    CURRENT VERSION COMPLETED MAY 15, 1987
    !    MODIFIED BY BERNARD GINGOLD ON JULY 14 2017
    !
    !    REFERENCE: KNUTH, DONALD E., 'THE ART OF COMPUTER PROGRAMMING',
    !          VOL. 2/SEMINUMERICAL ALGORITHMS, 2ND EDITION, ADDISON- 
    !          WESLEY PUBLISHING CO., 1981, PP. 117-118.
    !==============================================================================84
    pure function rdnor3(ix,iy,iz) result(prd)
          implicit none
          integer(I32P), intent(in) :: ix,iy,iz
          ! Locals
          real(R64P)                :: prd,u,v,s,q
          logical(I32P)             :: LL = .false.
          ! Start of executable statements
          if(LL .EQ. .true.) then
              prd = v*q
          else
              u = 2.0_R64P*rdunwh(ix,iy,iz)-ONER64P
              v = 2.0_R64P*rdunwh(ix,iy,iz)-ONER64P
              s = u*u+v*v
              do while(s .GE. ONER64P)
                   u = 2.0_R64P*rdunwh(ix,iy,iz)-ONER64P
                   v = 2.0_R64P*rdunwh(ix,iy,iz)-ONER64P
                   s = u*u+v*v
              end do
              q = DSQRT(-2.0_R64P*DLOG(s)/s)
              prd = u*q
          end if
          LL = .NOT. LL
    end function
    
    !==============================================================================84
    !
    !     RDCHI2   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !               DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !
    !     FOR:      GENERATING A RANDOM DEVIATE FROM THE CHI-SQUARED(DF)
    !               DISTRIBUTION WHERE DF > 0 AND DOES NOT HAVE TO BE AN INTEGER.
    !               A CHI-SQUARED(DF) DEVIATE IS EQUAL TO TWICE A GAMMA(DF/2)
    !               DEVIATE.  SEE REFERENCE BELOW. 
    !
    !     SUBPROGRAMS CALLED: RDGAMM (STSPAC) 
    !                         RDNOR (STSPAC)
    !
    !     CURRENT VERSION COMPLETED FEBRUARY 28, 1986
    !     MODIFIED BY BERNARD GINGOLD ON JULY 14 2017
    !
    !     REFERENCE: KENNEDY, WILLIAM J. AND GENTLE, JAMES E., 'STATISTICAL 
    !                COMPUTING', MARCEL DEKKER, INC., 1980, PP. 209-216
    !
    !==============================================================================84
    pure function rdchi2(df)  result(prd)
          implicit none
          real(R64P), intent(in) :: df
          ! Locals
          real(R64P)             :: prd
          ! Start of executable sttements
          prd = 2.0_R64P*rdgamm(df*HALFR64P)
    end function
    
    !==============================================================================84
    !    RDGAMM   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !              DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG, MD 
    !
    !      FOR:    GENERATING A RANDOM DEVIATE FROM THE GAMMA(ALPHA) DISTRIBUTION.
    !              ONE OF THREE METHODS IS USED DEPENDING ON THE VALUE OF THE
    !              PARAMETER ALPHA (WHICH DOES NOT HAVE TO BE AN INTEGER): 
    !
    !         VALUE OF A              METHOD USED 
    !        -------------         ------------------
    !        0 < ALPHA < 1      AHRENS (GS)   [FIRST REFERENCE]
    !          ALPHA = 1        LOG TRANSFORMATION
    !          ALPHA > 1        DIETER-AHRENS (GD)   [SECOND REFERENCE]
    !
    !              IF A <= 0 AN ERROR MESSAGE IS PRINTED AND EXECUTION IS
    !                 TERMINATED.
    !
    !         DESCRIPTIONS OF EACH OF THESE ALGORITHMS CAN BE FOUND IN
    !         THE REFERENCE GIVEN BELOW.
    !
    !     SUBPROGRAMS CALLED:  RDUNI (STSPAC) - UNIFORM(0,1) GENERATOR
    !                          RDNOR (STSPAC) - NORMAL(0,1) GENERATOR
    !
    !     CURRENT VERSION COMPLETED FEBRUARY 28, 1986
    !     MODIFIED BY BERNARD GINGOLD ON JULY 14 2017
    !
    !     REFERENCE: KENNEDY, WILLIAM J. AND GENTLE, JAMES E., 'STATISTICAL 
    !                 COMPUTING', MARCEL DEKKER, INC., 1980, PP. 209-216
    !
    !     REFERENCE: AHRENS, J.H. AND DIETER, U., 'GENERATING GAMMA VARIATES
    !                BY A MODIFIED REJECTION TECHNIQUE', COMMUNICATIONS OF
    !                THE ACM, VOL. 25, NO. 1, PP. 47-54, JANUARY 1982.
    !
    !==============================================================================84
    pure function rdgamm(alpha) result(prd)
          implicit none
          real(R64P), intent(in) :: alpha
          ! Locals
          real(R64P)             :: prd, s2,s,d,t,  &
                                    x,u,q0,b,sigma, &
                                    c,v,qq,sum,ee,  &
                                    factor,exprod,et
          real(R64P), dimension(8) :: q = [+0.041666661_R64P,+0.020834040_R64P, &
                                           +0.007970958_R64P,+0.001686911_R64P, &
                                           -0.000775882_R64P,+0.001274709_R64P, &
                                           -0.000506403_R64P,+0.000213943_R64P]
          real(R64P), dimension(8) :: a = [+0.33333332_R64P,-0.24999995_R64P, &
                                           +0.20000622_R64P,-0.16667748_R64P, &
                                           +0.14236572_R64P,-0.12438558_R64P, &
                                           +0.12337954_R64P,-0.11275089_R64P ] 
          real(R64P), dimension(6) :: e = [1.0_R64P,0.50000027_R64P,0.1666605_R64P,  &
                                           0.04171864_R64P,0.00813673_R64P,0.00172501_R64P]  
          real(R64P)               :: ap = ZEROR64,app = ZEROR64
          integer(I32P)            :: k
          ! Start of executable statements
          if(alpha .GT. ONER64P) then
             !    DIETER-AHRENS ALGORITHM (GD)
             !    STEP 1
              if(alpha .NE. ap) then
                  ap = alpha
                  s2 = alpha-HALFR64P
                  s  = DSQRT(s2)
                  d  = DSQRT(32.0_R64P)-12.0_R64P*s
              end if
             ! STEP 2
              t = rdnor(0)
              x = s+HALFR64P*t
              if(t .GE. ZEROR64) then
                  prd = x**2
                  return
              end if
              ! STEP 3
              u = rduni(0)
              if(d*u .LE. t**3) then
                  prd = x**2
                  return
              end if
              ! STEP 4
              if(alpha .NE. app) then
                  app = alpha
                  q0 = 0.0_R64P
                  do k = 1, 8
                      q0 = q0+q(k)*alpha**(-k)
                  end do
                  if(alpha .LE. 3.686_R64P) then
                      b = 0.463_R64P+s+0.178_R64P*s2
                      sigma = 1.235_R64P
                      c = 0.195_R64P/s-0.079_R64P+0.16_R64P*s
                  else if(alpha .LE. 13.022_R64P) then
                      b = 1.654_R64P+0.0076_R64P*s2
                      sigma = 1.68_R64P/s+0.275_R64P
                      c = 0.062_R64P/s+0.024_R64P
                  else
                      b = 1.77_R64P
                      sigma = 0.75_R64P
                      c = 0.1515_R64P/s
                  end if
              end if
              ! STEP 5
              if(x .LE. ZEROR64) go to 30
              ! STEP 6
              v = t/(s+s)
              if(DABS(v) .GT. QUARTER64P) then
                  qq = q0-s*t+QUARTER64P*t**2+(s2+s2)*DLOG(1.0_R64P+v)
              else
                  sum = 0.0_R64P
                  do k = 1, 8
                      sum = sum+a(k)*v**k
                  end do
                  qq = q0+(0.5_R64P*t**2)*sum
              end if
              ! STEP 7
              if(DLOG(1.0_R64P-u) .LE. qq) then
                  prd = x**2
                  return
              end if
              ! STEP 8
30            ee = -DLOG(rduni(0))
              u = 2.0_R64P*rduni(0)-ONER64P
              t = b+DSIGN(ee*sigma,u)
              ! STEP 9
              if(t .LE. -0.71874484_R64P) go to 30
              ! STEP 10
              v = t/(s+s)
              if(DABS(v) .GT. QUARTER64P) then
                  qq = q0-s*t+QUARTER64P*t**2+(s2+s2)*DLOG(1.0_R64P+v)
              else
                  sum = 0.0_R64P
                  do k = 1, 8
                      sum = sum+a(k)*v**k
                  end do
                  qq = q0+(HALFR64P*t**2)*sum
              end if
              ! STEP 11
              if(qq .LE. ZEROR64) go to 30
              et = ee-HALFR64P*t**2
              if(qq .LE. HALFR64P) then
                  factor = 0.0_R64P
                  do k = 1, 6
                      factor = factor+e(k)*qq**k
                  end do
                  exprod = factor*DEXP(et)
              else
                  exprod = DEXP(qq+et)-DEXP(et)
              end if
              if(c*DABS(u) .GT. exprod) go to 30
              ! STEP 12
              prd = (s+0.5_R64P*t)**2
            else if (alpha .EQ. ONER64P)
              !LOG TRANSFORMATION
              prd = DLOG(rduni(0))
            else if (alpha .GT. ZEROR64) then
                ! AHRENS ALGORITHM
                ! STEP 1
                ee =  2.7182818_R64P
60              u  = rduni(0)
                b = (ee+alpha)/ee
                p = b*u
                if(p .GT. ONER64P) go to 70
                ! STEP 2
                prd = p**(1.0_R64P/alpha)
                v = rduni(0)
                if(v .GT. DEXP(-prd)) go to 60
                return
                ! STEP 3
                prd = -DLOG((b-p)/alpha)
                v = rduni(0)
                if(v .GT. prd**(alpha-1.0_R64P)) go to 60
                return
            else
                print *,' *** THE GAMMA PARAMETER MUST BE > 0'
                print *,' *** EXECUTION STOPPED IN FUNCTION: RDGAMM'
                STOP
            end if
    end function
    
    !==============================================================================84
    !    RDUNLL   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !              DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !              MARYLAND  20899
    !
    !    FOR: GENERATING UNIFORM PSEUDO-RANDOM DEVIATES IN THE INTERVAL
    !          (0,1) USING A CONGRUENTIAL GENERATOR.  THIS GENERATOR WAS
    !          DEVELOPED AT LAWRENCE LIVERMORE LABORATORIES UNDER THE NAME
    !          'LLRANDOMI' AND IS CURRENTLY USED IN THE IMSL LIBRARY.  IT
    !          HAS A CYCLE LENGTH OF 2**31-2 = 2,147,483,646.  BEFORE THE
    !          FIRST CALL TO THIS ROUTINE ISEED SHOULD BE ASSIGNED A VALUE
    !          IN THE RANGE [1,2147483646].  THEREAFTER THE VALUE OF ISEED
    !          SHOULD NOT BE CHANGED.
    !
    !     THE NBS CENTRAL COMPUTERS WERE FOUND TO GENERATE DEVIATES
    !     AT THE FOLLOWING RATES USING THIS ROUTINE: 
    !
    !             CYBER 180/855 ->   125,707 DEV/SEC
    !             CYBER 205/622 ->   260,373 DEV/SEC
    !
    !     SUBPROGRAMS CALLED: -NONE-
    !
    !     CURRENT VERSION COMPLETED FEBRUARY 10, 1987
    !     MODIFIED BY BERNARD GINGOLD ON JULY 13 2017
    !==============================================================================84
    function rdunll(iseed)  result(prd)
          implicit none
          integer(I32P), intent(inout) :: iseed
          ! Locals
          integer(I32P)             :: m  = 2147483647
          real(R64P)                :: rm = 2147483647.0_R64P
          real(R64P)                :: prd
          integer(I32P), parameter  :: COEF1 = 16807
          ! Start of executable statements
          prd = 0.0_R64P
          iseed = MOD(COEF1*iseed,m)
          prd = DBLE(iseed)/rm
    end function
    
    !==============================================================================84
    !   RDUNWH   COPIED BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !            DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !            MARYLAND 20899 FROM THE REFERENCE BELOW WITH SLIGHT
    !            MODIFICATIONS
    !
    !   FOR: GENERATING UNIFORM PSEUDO-RANDOM DEVIATES IN THE INTERVAL
    !        [0,1) BY THE WICHMAN-HILL METHOD.  AN EXACT VALUE OF ZERO
    !        IS POSSIBLE BUT NOT LIKELY.  THE CYCLE LENGTH EXCEEDS 6.95E12.
    !        TWO METHODS, WHICH GENERATE IDENTICAL DEVIATES, ARE AVAILABLE
    !        FOR USE DEPENDING ON THE LIMITS OF THE INTEGER ARITHMETIC.
    !        THE FIRST METHOD MAY BE USED ON COMPUTERS WITH 16-BIT WORDS. 
    !        THE METHOD NOT BEING USED CAN BE 'COMMENTED' OUT.  USING THE 
    !        SECOND METHOD, THE NBS CENTRAL COMPUTERS WERE FOUND TO
    !        GENERATE DEVIATES AT THE FOLLOWING RATES: 
    !
    !             CYBER 180/855 ->   48,995 DEVIATES/SECOND 
    !             CYBER 205/622 ->  126,564 DEVIATES/SECOND .
    !
    !        BEFORE THE FIRST CALL TO THIS ROUTINE THE INTEGERS IX, IY, AND
    !        IZ SHOULD BE SET TO VALUES WITHIN THE RANGE [1,30000].  AFTER
    !        THAT THEIR VALUES SHOULD NOT BE CHANGED EXCEPT BY THIS ROUTINE.
    !
    ! SUBPROGRAMS CALLED: -NONE-
    !
    !  CURRENT VERSION COMPLETED MAY 15, 1987
    !  MODIFIED BY BERNARD GINGOLD ON JULY 13 2017
    !
    !  REFERENCE: GRIFFITHS, P. AND HILL, I.D. (EDS.), 'APPLIED STATISTICS
    !             ALGORITHMS', THE ROYAL STATISTICAL SOCIETY/ELLIS HARWOOD
    !             LIMITED, 1985, ALGORITHM AS 183, PP. 238-242.
    !==============================================================================84
    function rdunwh(ix,iy,iz)  result(uprd)
          implicit none
          integer(I32P), intent(inout) :: ix,iy,iz
          ! Locals
          integer(I32P), parameter :: COEF1 = 170,COEF2 = 171,   &
                                      COEF3 = 172,COEF4 = 30269, &
                                      COEF5 = 30307,COEF6 = 30323
          real(R64P),    parameter :: COEF7 = 30269.0_R64P, &
                                      COEF8 = 30307.0_R64P, &
                                      COEF9 = 30323.0_R64P
          real(R64P)               :: sum,uprd
          ! Start of executable statements
          ix = MOD(COEF2*ix,COEF4)
          iy = MOD(COEF3*iy,COEF5)
          iz = MOD(COEF1*iz,COEF6)
          sum = DBLE(ix)/COEF7+DBLE(iy)/COEF8+DBLE(iz)/COEF9
          uprd = DMOD(sum,ONER64P)
    end function
    
    !==============================================================================84
    !   RDCONS   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !            DIVISION, NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
    !            GAITHERSBURG, MARYLAND  20899
    !
    !    FOR: GENERATING UNIFORMLY-SPACED PSEUDO-RANDOM POINTS ON THE
    !         SURFACE OF A CONE WITH HEIGHT H AND HALF-ANGLE PSI.  NO
    !         POINTS WILL BE GENERATED UNLESS H>0 AND 0<PSI<PI/2.  THE CONE
    !         TIP IS AT THE ORIGIN, AND ITS AXIS COINCIDES WITH THE Z AXIS.
    !         THE BASE LIES IN THE PLANE Z=H.  ON OUTPUT, EACH OF THE N
    !         COLUMNS OF MATRIX P CONTAINS THE X, Y, AND Z COORDINATES OF
    !         THE GENERATED POINTS.  THE LEADING DIMENSION OF P (LDP) MUST 
    !         BE 3 OR MORE IN THE CALLING PROGRAM.
    !
    !         IF IFLAG=0 ON INPUT, THEN POINTS WILL BE GENERATED ONLY ON THE
    !         CURVED SURFACE.  IF IFLAG<>0 ON INPUT, THEN POINTS WILL BE
    !         GENERATED ON BOTH THE CURVED SURFACE AND THE BASE. 
    !
    !         POINTS ARE GENERATED ON THE CURVED SURFACE BY TRANSFORMATIONS,
    !         AND ON THE BASE (IF REQUESTED) BY A REJECTION PROCEDURE
    !         WHICH REJECTS 1 - PI/4 = 0.2146 OF THE POINTS.
    !
    !     SUBPROGRAMS CALLED: RDUNI (STSPAC) - U(0,1) RANDOM DEVIATES
    !     CURRENT VERSION COMPLETED MAY 26, 1989
    !     MODIFIED BY BERNARD GINGOLD ON JULY 13 2017
    !
    !    DEFINITION OF PASSED PARAMETERS: 
    !
    !        P(LDP,*) = MATRIX (SIZE 3 BY N) WHOSE COLUMNS ARE THE N RANDOM
    !                   POINTS GENERATED ON THE CONE SURFACE.  THE LEADING
    !                   DIMENSION OF P IS LDP (>=3) AND THE SECOND DIMENSION,
    !                   DEFINED IN THE CALLING PROGRAM, MUST BE AT LEAST N.
    !                   [REAL]
    !
    !       * LDP = LEADING DIMENSION OF MATRIX P (>=3) [INTEGER]
    !
    !       * N = NUMBER OF POINTS TO BE GENERATED [INTEGER]
    !
    !       * H = HEIGHT OF THE CONE (>0) [REAL]
    !
    !       * PSI = HALF-ANGLE OF THE CONE (>0) [REAL] 
    !
    !       * IFLAG = INDICATOR FOR WHERE TO GENERATE POINTS ON INPUT
    !                 (0 -> CURVED SURFACE ONLY; OTHERWISE -> CURVED SURFACE 
    !                 AND BASE) [INTEGER]
    !
    !          ERROR FLAG ON OUTPUT.   INTERPRETATION: 
    !          0 -> NO ERRORS DETECTED. 
    !          1 -> H<=0 OR PSI<=0 OR PSI>=PI/2
    !          2 -> N<1.
    !          3 -> LDP<3.
    !          7 -> UNALLOCATED ARRAY ARGUEMENT
    !          8 -> HW FP EXCEPTION(S) SIGNALLED(QUIETLY)
    !        * INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !
    !==============================================================================84
    subroutine rdcons(p,n,h,psi,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), dimension(:,:),  intent(inout) :: p
          integer(I32P),               intent(in)    :: n
          real(R64P),                  intent(in)    :: h,psi
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P), parameter  :: TWOPI  = 8.0_R64P*DATAN(ONER64P)
          real(R64P), parameter  :: HALFPI = QUARTER64P*TWOPI
          real(R64P)             :: tanpsi = DTAN(psi)
          real(R64P)             :: uc,rr,z,r,phi,u1,uc,q,u2
          integer(I32P)          :: i
          logical(I32P)          :: bh,bpsi1,bpsi2
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF         
          ! Start of executable statements
          ! Sanity checking of input arguemnts
          if(array2D_not_alloc(p) .EQ. .true.) then
              iflag = 7
              return
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          bh    = h   .LE. ZEROR64
          bpsi1 = psi .LE. ZEROR64
          bpsi2 = psi .GE. HALFPI
          if(bh .OR. bpsi1 .OR. bpsi2) then
              iflag = 1
          else if(n .LT. 1) then
              iflag = 2
          else
              uc = ONER64P/(ONER64P+DSIN(psi))
              rr = h*tanpsi
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
              if(iflag .EQ. 0) then
                  do i = 1, n
                     z = h*DSQRT(rduni(0))
                     r = z*tanpsi
                     phi = TWOPI*rduni(0)
                     p(1,i) = r*DCOS(phi)
                     p(2,i) = r*DSIN(phi)
                     p(3,i) = z
                  end do
              else
                  do i = 1, n
                      u1 = rduni(0)
                      if(u1 .LE. uc) then
                          z = h*DSQRT(u1/uc)
                          r = z*tanpsi
                          phi = TWOPI*rduni(0)
                          p(1,i) = r*DCOS(phi)
                          p(2,i) = r*DSIN(phi)
                          p(3,i) = z
                      else
                          u1 = 2.0_R64P*rduni(0)-ONER64P
                          u2 = 2.0_R64P*rduni(0)-ONER64P
                          q  = u1**2+u2**2
                          do while(q .GT. ONER64P)
                             u1 = 2.0_R64P*rduni(0)-ONER64P
                             u2 = 2.0_R64P*rduni(0)-ONER64P
                             q  = u1**2+u2**2 
                          end do
                          p(1,i) = rr*u1
                          p(2,i) = rr*u2
                          p(3,i) = h
                      end if
                  end do
                 
              end if
               iflag = 0
         end if      
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " rdcons: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF         
    end subroutine 
    
    !==============================================================================84
    !    RDCONV   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !             DIVISION, NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
    !             GAITHERSBURG, MARYLAND  20899
    !
    !    FOR: GENERATING UNIFORMLY-SPACED PSEUDO-RANDOM POINTS WITHIN THE
    !         CONE WITH HEIGHT H AND HALF-ANGLE PSI.  NO POINTS WILL BE
    !         GENERATED UNLESS H>0 AND 0<PSI<PI/2.  THE CONE TIP IS AT THE 
    !         ORIGIN, AND ITS AXIS COINCIDES WITH THE Z AXIS.  THE BASE
    !         LIES IN THE PLANE Z=H.  ON OUTPUT, EACH OF THE N COLUMNS OF
    !         MATRIX P CONTAINS THE X, Y, AND Z COORDINATES OF THE GENERATED
    !         POINTS.  THE LEADING DIMENSION OF P (LDP) MUST BE 3 OR MORE
    !         IN THE CALLING PROGRAM.
    !
    !         POINTS ARE GENERATED BY A REJECTION PROCEDURE WHEREBY 0.2146 
    !         = 1-PI/4 OF THE POINTS ARE REJECTED.
    !
    !    SUBPROGRAMS CALLED: RDUNI (STSPAC) - U(0,1) RANDOM DEVIATES
    !
    !    CURRENT VERSION COMPLETED MAY 30, 1989
    !     MODIFIED BY BERNARD GINGOLD O JULY 13 2017
    !
    !    DEFINITION OF PASSED PARAMETERS: 
    !
    !         P(LDP,*) = MATRIX (SIZE 3 BY N) WHOSE COLUMNS ARE THE N RANDOM
    !                    POINTS GENERATED INSIDE THE CONE.  THE LEADING
    !                    DIMENSION OF P IS LDP (>=3) AND THE SECOND DIMENSION,
    !                    DEFINED IN THE CALLING PROGRAM, MUST BE AT LEAST N.
    !                    [REAL]
    !
    !         * LDP = LEADING DIMENSION OF MATRIX P (>=3) [INTEGER]
    !
    !         * N = NUMBER OF POINTS TO BE GENERATED [INTEGER]
    !
    !         * H = HEIGHT OF THE CONE (>0) [REAL]
    !
    !         * PSI = HALF-ANGLE OF THE CONE (>0) [REAL] 
    !
    !         IFLAG = ERROR FLAG ON OUTPUT.   INTERPRETATION: 
    !                 0 -> NO ERRORS DETECTED. 
    !                 1 -> H<=0 OR PSI<=0 OR H<PSI.
    !                 2 -> N<1.
    !                 3 -> LDP<3.
    !                 7 -> UNALLOCATED ALLOCATABLE ARRAY
    !                 8 -> HW FP EXCEPTION(S) SIGNALLED(QUIETLY)    
    !         * INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !
    !==============================================================================84
    subroutine rdconv(p,n,h,psi,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          implicit none
          real(R64P), dimension(:,:),  intent(inout) :: p
          integer(I32P),               intent(in)    :: n
          real(R64P),                  intent(in)    :: h,psi
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P), parameter :: TWOPI  = 8.0_R64P*DATAN(ONER64P)
          real(R64P), parameter :: HALFPI = QUARTER64P*TWOPI
          logical(I32P)         :: bh,bpsiz,bpsih
          real(R64P)            :: cubert,tanpsi,z,r,phi
          integer(I32P)         :: i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF          
          ! Start of executable statements
          ! Sanity check on input arguemnts
          if(array2D_not_alloc(p) .EQ. .true.) then
              iflag = 7
              return
          end if
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          bh    = h   .LE. ZEROR64
          bpsiz = psi .LE. ZEROR64
          bpsih = psi .GT. HALFPI
          if(bh .OR. bpsiz .OR. bpsih) then
              iflag = 1
          else if(n .LT. 1) then
              iflag = 2
          else
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF              
              iflag = 0
              cubert = 0.33333333333333333333333333333333_R64P
              tanpsi = DTAN(psi)
              do i = 1, n
                  z = h*rduni(0)**cubert
                  r = z*tanpsi*DSQRT(rduni(0))
                  phi = TWOPI*rduni(0)
                  p(1,i) = r*DCOS(phi)
                  p(2,i) = r*DSIN(phi)
                  p(3,i) = z
              end do
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " rdconv: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
    
    !==============================================================================84
    !    RDCYLS   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !             DIVISION, NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
    !             GAITHERSBURG, MARYLAND  20899
    !
    !     FOR: GENERATING UNIFORMLY-SPACED PSEUDO-RANDOM POINTS ON THE
    !          SURFACE OF THE CYLINDER WITH HEIGHT H AND RADIUS R.  NO
    !          POINTS WILL BE GENERATED UNLESS H>=0 AND R>=0.  THE AXIS
    !          OF THE CYLINDER COINCIDES WITH THE POSITIVE Z AXIS, AND THE
    !          ENDS OF THE CYLINDER ARE FORMED BY THE PLANES Z=0 AND Z=H.
    !          ON OUTPUT, EACH OF THE N COLUMNS OF MATRIX P CONTAINS THE X, 
    !          Y, AND Z COORDINATES OF THE GENERATED POINTS.  THE LEADING
    !          DIMENSION OF P (LDP) MUST BE 3 OR MORE IN THE CALLING PROGRAM.
    !
    !          AT THE USERS OPTION, POINTS MAY BE GENERATED ON THE CURVED
    !          SURFACE ONLY OR ON BOTH THE CURVED AND FLAT SURFACES.  POINTS
    !          ARE GENERATED ON THE CURVED SURFACE BY A TRANSFORMATION, AND 
    !          POINTS ARE GENERATED ON THE FLAT SURFACES BY A REJECTION
    !          PROCEDURE WHEREBY THE PROPORTION OF POINTS REJECTED IS 0.2146
    !          = 1-PI/4.
    !          NOTE THAT POINTS CAN BE GENERATED ON A STRAIGHT LINE BY
    !          SETTING H>0 AND R=0.  POINTS CAN BE GENERATED ON A CIRCLE BY 
    !          SETTING H=0 AND R>0. 
    !
    !      SUBPROGRAMS CALLED: RDUNI (STSPAC) - U(0,1) RANDOM DEVIATES
    !
    !      CURRENT VERSION COMPLETED MAY 30, 1989
    !      MODIFIED BY BERNARD GINGOLD ON JULY 13 2017
    !-----------------------------------------------------------------------
    !   DEFINITION OF PASSED PARAMETERS: 
    !
    !    P(LDP,*) = MATRIX (SIZE 3 BY N) WHOSE COLUMNS ARE THE N RANDOM
    !          POINTS GENERATED ON THE CYLINDER SURFACE.  THE LEADING 
    !          DIMENSION OF P IS LDP (>=3) AND THE SECOND DIMENSION,
    !          DEFINED IN THE CALLING PROGRAM, MUST BE AT LEAST N.
    !          [REAL]
    !    * LDP = LEADING DIMENSION OF MATRIX P (>=3) [INTEGER]
    !
    !    * N = NUMBER OF POINTS TO BE GENERATED [INTEGER]
    !
    !    * H = HEIGHT OF THE CYLINDER (>=0) [REAL]
    !
    !    * R = RADIUS OF THE CYLINDER (>=0) [REAL]
    !
    !    * IFLAG = INDICATOR FOR WHERE TO GENERATE POINTS ON INPUT
    !          (0 -> CURVED SURFACE ONLY; OTHERWISE -> CURVED SURFACE 
    !          AND BASE) [INTEGER]
    !
    !          ERROR FLAG ON OUTPUT.   INTERPRETATION: 
    !          0 -> NO ERRORS DETECTED. 
    !          1 -> H<0.
    !          2 -> R<0.
    !          3 -> LDP<3.
    !          7 -> UNALLOCATED ALLOCATABLE ARRAY
    !          8 -> HW FP EXCEPTION(S) SIGNALLED(QUIETLY)
    !
    !* INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !==============================================================================84
    subroutine rdcyls(p,n,h,r,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          real(R64P), dimension(:,:), intent(inout) :: p
          integer(I32P),              intent(in)    :: n
          real(R64P),                 intent(inout) :: h,r
          integer(I32P),              intent(inout) :: iflag
          logical(I32P), dimension(5),intent(inout) :: fp_flags
          ! Locals
          real(R64P), parameter :: TWOPI = 8.0_R64P*DTAN(ONER64P)
          real(R64P)            :: u,uc1,uc2,u1,u2,q
          integer(I32P)         :: i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF          
          ! Start of executable statements
          ! Sanity check of input arguemtns
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          if(array2D_not_alloc(p) .EQ. .true.) then
              iflag = 7
              return
          end if
          if(h .LT. ZEROR64) then
              iflag = 1
          else if(r .LT. ZEROR64) then
              iflag = 2
          else 
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF               
               if(iflag .EQ. 0) then
               !   GENERATE POINTS ON THE CURVED SURFACE ONLY
                  do i = 1, n
                     p(3,i) = h*rduni(0)
                     u = TWOPI*rduni(0)
                     p(1,i) = r*DCOS(u)
                     p(2,i) = r*DSIN(u)
                  end do
              else
              !  GENERATE POINTS ON BOTH THE CURVED AND FLAT SURFACES
              uc1 = h/(h+r)
              uc2 = HALFR64P*(ONER64P+uc1)
              do i = 1, n
                  u = rduni(0)
                  if(u .LE. uc1) then
                      p(3,i) = h*u/uc1
                      u = TWOPI*rduni(0)
                      p(1,i) = r*DCOS(u)
                      p(2,i) = r*DSIN(u)
                  else
                      u1 = 2.0_R64P*rduni(0)-ONER64P
                      u2 = 2.0_R64P*rduni(0)-ONER64P
                      q = u1*u1+u2*u2
                      do while(q .GT. ONER64P)
                          u1 = 2.0_R64P*rduni(0)-ONER64P
                          u2 = 2.0_R64P*rduni(0)-ONER64P
                          q = u1*u1+u2*u2
                      end do
                      p(1,i) = r*u1
                      p(2,i) = r*u2
                      if(u .LE. uc2) then
                          p(3,i) = ZEROR64
                      else
                          p(3,i) = h
                      end if
                  end if
              end do
          end if
          iflag = 0
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " rdcyls: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF                
    end if
    
    end subroutine
    !=============================================================================84
    !    RDCYLV   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !              DIVISION, NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
    !              GAITHERSBURG, MARYLAND  20899
    !
    !     FOR: GENERATING UNIFORMLY-SPACED PSEUDO-RANDOM POINTS WITHIN THE
    !          CYLINDER WITH HEIGHT H AND RADIUS R.  NO POINTS WILL BE
    !          GENERATED UNLESS H>=0 AND R>=0.  THE AXIS OF THE CYLINDER
    !          COINCIDES WITH THE POSITIVE Z AXIS, AND THE ENDS OF THE
    !          CYLINDER ARE FORMED BY THE PLANES Z=0 AND Z=H.  ON OUTPUT,
    !          EACH OF THE N COLUMNS OF MATRIX P CONTAINS THE X, Y, AND Z
    !          COORDINATES OF THE GENERATED POINTS.  THE LEADING DIMENSION
    !          OF P (LDP) MUST BE 3 OR MORE IN THE CALLING PROGRAM.
    !
    !          POINTS ARE GENERATED BY A REJECTION PROCEDURE WHEREBY THE
    !          PROPORTION OF POINTS REJECTED IS 0.2146 = 1-PI/4.
    !
    !          NOTE THAT POINTS CAN BE GENERATED (RATHER INEFFICIENTLY) ON
    !          A STRAIGHT LINE BY SETTING H>0 AND R=0.  POINTS CAN BE
    !          GENERATED ON A (TWO-DIMENSIONAL) DISK BY SETTING H=0 AND R>0.
    !
    !     SUBPROGRAMS CALLED: RDUNI (STSPAC) - U(0,1) RANDOM DEVIATES
    !
    !     CURRENT VERSION COMPLETED MAY 30, 1989
    !     MODIFEID BY BERNARD GINGOLD ON JULY 14 2017
    !     -----------------------------------------------------------------------
    !     DEFINITION OF PASSED PARAMETERS: 
    !
    !     P(LDP,*) = MATRIX (SIZE 3 BY N) WHOSE COLUMNS ARE THE N RANDOM
    !                POINTS GENERATED INSIDE THE CYLINDER.  THE LEADING
    !                DIMENSION OF P IS LDP (>=3) AND THE SECOND DIMENSION,
    !                DEFINED IN THE CALLING PROGRAM, MUST BE AT LEAST N.
    !                [REAL]
    !
    !     * LDP = LEADING DIMENSION OF MATRIX P (>=3) [INTEGER]
    !
    !     * N = NUMBER OF POINTS TO BE GENERATED [INTEGER]
    !
    !     * H = HEIGHT OF THE CYLINDER (>=0) [REAL]
    !
    !     * R = RADIUS OF THE CYLINDER (>=0) [REAL]
    !     IFLAG = ERROR FLAG ON OUTPUT.   INTERPRETATION: 
    !             0 -> NO ERRORS DETECTED. 
    !             1 -> H<0.
    !             2 -> R<0.
    !             7 -> UNALLOCATED ALLOCATABLE ARRAY P
    !             8 -> HW FP EXCEPTION(S) SIGNALLED(QUIETLY)
    !     * INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !==============================================================================84
    subroutine rdcylv(p,n,h,r,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          real(R64P), dimension(:,:),  intent(inout) :: p
          integer(I32P),               intent(in)    :: n
          real(R64P),                  intent(in)    :: h,r
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)                                 :: u,v,q
          integer(I32P)                              :: i
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF 
          ! Start of executable statements
          ! Sanity check of input arguments
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          if(array2D_not_alloc(p) .EQ. .true.) then
              iflag = 7
              return
          end if
          if(h .LT. ZEROR64) then
              iflag = 1
          else if(r .LT. ZEROR64) then
              iflag = 2
          else
              iflag = 0
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF                
              do i = 1, n
                  p(3,i) = h*rduni(0)
                  u = 2.0_R64P*rduni(0)-ONER64P
                  v = 2.0_R64P*rduni(0)-ONER64P
                  q = u*u+v*v
                  do while(q .GT. ONER64P)
                     u = 2.0_R64P*rduni(0)-ONER64P
                     v = 2.0_R64P*rduni(0)-ONER64P
                     q = u*u+v*v
                  end do
                  p(1,i) = r*u
                  p(2,i) = r*v
              end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " rdcylv: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF               
          end if
          
    end subroutine
    
    !==============================================================================84
    !    RDELLS   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !              DIVISION, NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
    !              GAITHERSBURG, MARYLAND  20899
    !
    !     FOR: GENERATING UNIFORMLY-SPACED PSEUDO-RANDOM POINTS ON THE
    !          SURFACE OF THE M-DIMENSIONAL ELLIPSOID DEFINED BY
    !
    !                        M
    !                       SUM (X /A )**2 = 1
    !                       J=1   J  J
    !
    !          WHERE EACH A(J)>0.  THE CENTER OF THE ELLIPSOID IS AT THE
    !          ORIGIN.  NOTE THAT M STANDARD NORMAL DEVIATES AND 1 STANDARD 
    !          UNIFORM DEVIATE ARE REQUIRED TO GENERATE EACH POINT.  IF THE 
    !          A(J) ARE NOT ALL EQUAL, THERE IS A POSITIVE PROBABILITY THAT 
    !          THE POINT WILL BE REJECTED.
    !
    !      SUBPROGRAMS CALLED: RDNOR (STSPAC) - N(0,1) RANDOM DEVIATES
    !                          RDUNI (STSPAC) - U(0,1) RANDOM DEVIATES
    !
    !     CURRENT VERSION COMPLETED JANUARY 8, 1990
    !     MODIFIED BY BERNARD GINGOLD ON JULY 14 2017
    !     -----------------------------------------------------------------------
    !     DEFINITION OF PASSED PARAMETERS: 
    !
    !     P(LDP,*) = MATRIX (SIZE M BY N) WHOSE COLUMNS ARE THE N RANDOM
    !                POINTS GENERATED ON THE SURFACE OF THE ELLIPSOID.  THE 
    !                LEADING DIMENSION OF P IS LDP (>=M) AND THE SECOND
    !                DIMENSION, DEFINED IN THE CALLING PROGRAM, MUST BE AT
    !                LEAST N. [REAL]
    !
    !      * LDP = LEADING DIMENSION OF MATRIX P (>=M) [INTEGER]
    !
    !      * N = NUMBER OF POINTS TO BE GENERATED [INTEGER]
    !
    !      * M = DIMENSION OF THE ELLIPSOID (>0) [INTEGER]
    !
    !      * A(*) = VECTOR (LENGTH M) OF ELLIPSOID SEMIAXES (>0) [REAL]
    !
    !      IFLAG = ERROR FLAG ON OUTPUT.   INTERPRETATION: 
    !            0 -> NO ERRORS DETECTED. 
    !            1 -> N<1 OR M<1.
    !            2 -> M>LDP.
    !            3 -> AT LEAST ONE A(J)<=0.
    !            7 -> UNALLOCATED ALLOCATBLE ARRAY ARGUMENTS
    !            8 -> HW FP EXCEPTION(S) SIGNALLED(QUIETLY)
    !    * INDICATES PARAMETERS REQUIRING INPUT VALUES 
    !==============================================================================84
    subroutine rdells(p,n,m,a,iflag,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          real(R64P), dimension(:,:),  intent(inout) :: p
          integer(I32P),               intent(in)    :: n,m
          real(R64P), dimension(:),    intent(in)    :: a
          integer(I32P),               intent(inout) :: iflag
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)                                 :: amin,q,r,u1
          integer(I32P)                              :: i,j
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type) :: status_value
!DIR$ ENDIF 
          ! Start of executable statements
          ! Sanity checking of input arguments
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          if(array2D_not_alloc(p) .OR. &
             array1D_not_alloc(a)      ) then
              iflag = 7
              return
          end if
          if(MIN0(n,m) .LE. 0) then
              iflag = 1
          else if(m .GT. SIZE(p,dim=1)) then
              iflag = 2
          else
              amin = a(1)
              do j = 2, m
                  amin = DMIN1(amin,a(j))
              end do
              if(amin .LE. ZEROR64) then
                  iflag = 3
              else
                  iflag = 0
 !DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_status(status_value)
          call ieee_set_halting_mode(ieee_all,.false.)
          call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF                  
                  do i = 1, n
20                    q = 0.0_R64P
                      r = 0.0_R64P
                      do j = 1, m
                          p(j,i) = rdnor(0) ! IMPLEMENT THIS FUNCTION!!
                          q = q+p(j,i)**2
                          r = r+(p(j,i)/a(j))**2
                      end do
                      q = DSQRT(q)
                      r = DSQRT(r)
                      u1 = rduni(0)
                      if(amin*r.LT.q*u1) go to 20
                      do j = 1, m
                          p(j,i) = a(j)*p(j,i)/q
                      end do
                  end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          call ieee_get_flag(ieee_all,fp_flags)
          if(ANY(fp_flags)) then
             iflag = 8
             write(ERROR_UNIT,*) "================================================"
             write(ERROR_UNIT,*) " rdells: FLOATING-POINT EXCEPTION(S) OCCURRED"
             write(ERROR_UNIT,*) "================================================"
          end if
          call ieee_set_status(status_value)
!DIR$ ENDIF                    
              end if
          end if
          
    end subroutine
    
    
    
    !==============================================================================84
    !    DGAMLN   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !           DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
    !        MARYLAND  20899
    !
    !     FOR: COMPUTING THE DOUBLE PRECISION LOG OF THE GAMMA FUNCTION WITH
    !          SINGLE PRECISION PARAMETER X>0.  THE MAXIMUM TRUNCATION ERROR
    !          IN THE INFINITE SERIES (SEE REFERENCE 1) IS DETERMINED BY THE
    !          CONSTANT XMIN.  WHEN X<XMIN A RECURRENCE RELATION IS USED IN 
    !          ORDER TO ACHIEVE THE REQUIRED ABSOLUTE ACCURACY.  THE TABLE
    !          BELOW GIVES THE MINIMUM VALUE OF X WHICH YIELDS THE CORRES-
    !          PONDING ABSOLUTE ACCURACY IN DGAMLN(X) ASSUMING THE MACHINE
    !          CARRIES ENOUGH DIGITS WHEN THOSE TO THE LEFT OF THE DECIMAL
    !          ARE CONSIDERED (SEE REFERENCE 2 FOR FURTHER DISCUSSION).  IF 
    !          THE LATTER CONDITION IS NOT MET, AN ERROR MESSAGE IS PRINTED.
    !
    !          THE CYBER 180/855 AT NBS CARRIES ABOUT 15 DIGITS IN SINGLE
    !          PRECISION, THEREFORE THE PRE-SET VALUE OF ABSACC IS 10**(-15)
    !          AND THE CORRESPONDING VALUE OF XMIN IS 6.894.  ON A DIFFERENT
    !          MACHINE THESE CONSTANTS SHOULD BE CHANGED ACCORDINGLY.
    !
    !       XMIN    ACCURACY    XMIN    ACCURACY    XMIN    ACCURACY
    !      ------   --------   ------   --------   ------   --------
    !       1.357     1E-3      4.592     1E-12    15.539     1E-21
    !       2.037     1E-6      6.894     1E-15    23.330     1E-24
    !       3.059     1E-9     10.351     1E-18    35.025     1E-27
    !
    !     NOTE: THIS IS EXACTLY THE SAME SOFTWARE AS SUBROUTINE DGAMLN
    !
    !     SUBPROGRAMS CALLED: -NONE-
    !
    !       CURRENT VERSION COMPLETED MAY 1, 1989
    !       Modified by @Bernard Gingold on  JULY 8 2017
    !     REFERENCES: 
    !
    !         1) ABRAMOWITZ, MILTON AND STEGUN, IRENE, 'HANDBOOK OF MATHEMATICAL
    !            FUNCTIONS', NBS APPLIED MATHEMATICS SERIES 55, NOV. 1970,
    !            EQ. 6.1.40, P 257.
    !
    !         2) REEVE, CHARLES P., 'ACCURATE COMPUTATION OF THE LOG OF THE GAMMA
    !            FUNCTION', STATISTICAL ENGINEERING DIVISION NOTE 86-1, OCTOBER 
    !            1986.
    !==============================================================================84
   
      function dgamln(x) result(glog)
          
          implicit none
          real(R64P), intent(in) :: x
          ! Locals
          integer(I32P) :: n,i
          real(R64P) :: glog,q,r,xn
          real(R64P), parameter :: xmin   =  6.894_R64P
          real(R64P), parameter :: absacc =  0.000000000000001_R64P
          real(R64P), parameter :: c      =  0.918938533204672741780329736_R64P
          real(R64P), parameter :: b1     =  0.833333333333333333333333333D-1
          real(R64P), parameter :: b2     =  -0.277777777777777777777777778D-2
          real(R64P), parameter :: b3     =  0.793650793650793650793650794D-3
          real(R64P), parameter :: b4     =  -0.595238095238095238095238095D-3
          real(R64P), parameter :: b5     =  0.841750841750841750841750842D-3
          real(R64P), parameter :: b6     =  -0.191752691752691752691752692D-2
          real(R64P), parameter :: b7     =  0.641025641025641025641025641D-2
          real(R64P), parameter :: b8     =  -0.295506535947712418300653595D-1
          ! Start of executable statements
          ! Terminate execution if x <= 0.0_R64P
          if(x .LE. ZEROR64) then
             ERROR STOP "dgamln: X <= 0.0_R64P"
          end if
          glog = 0.0_R64P
          n = MAX0(0,INT(xmin-x+1.0_R64P))
          xn = x + DBLE(n)
          r = 1.0_R64P / xn
          q = r * r
          glog = r*(b1+q*(b2+q*(b3+q*(b4+q*(b5+q*(b6+q*(b7+q*b8)))))))+c+ &
                 (xn-0.5_R64P)*DLOG(xn)-xn
          ! Use recurrence relation when n>0
          if(n .GT. 0) then
             q = 1.0_R64P
             do i = 0, n - 1
                 q = q * (x + DBLE(i))
             end do
             glog = glog - DLOG(q)
          end if
          ! PRINT WARNING IF ABSOLUTE ACCURACY HAS NOT BEEN ATTAINED
          if(glog + absacc .EQ. glog) then
             print *,' ********* WARNING FROM FUNCTION DGAMLN *********'
             print *,' REQUIRED ABSOLUTE ACCURACY NOT ATTAINED FOR X = ',x
          end if
          
    end function
    
    !==============================================================================84
    !   DNCMLN   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !             DIVISION, NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
    !        GAITHERSBURG, MARYLAND  20899
    !
    !   FOR: COMPUTING THE DOUBLE PRECISION NATURAL LOGARITHM OF
    !        'N CHOOSE M' = N!/[M!(N-M)!] WHERE N>=0, M>=0, AND M<=N.  THAT
    !         VALUE IS RETURNED IN DNCMLN.
    !
    !    SUBPROGRAMS CALLED: DGAMLN (NATURAL LOGARITHM OF GAMMA FUNCTION)
    !
    !    CURRENT VERSION COMPLETED MAY 1, 1989
    !    Adapted from original work by @Bernard Gingold on JULY 9 2017
    !==============================================================================84
     function dncmln(n,m) result(nlog)
          implicit none
          integer(I32P), intent(in) :: n,m
          ! Locals
          real(R64P) :: d1,d2,d3,r1,r2,r3
          ! Start of executable statements
          if(n .LT. 0) then
             ERROR STOP "dncmln: [FATAL]: N < 0"
          end if
          if(m .LT. 0) then
             ERROR STOP "dncmln: [FATAL]: M < 0"
          end if
          if(m .GT. n) then
             ERROR STOP "dncmln: [FATAL]: M > N"
          end if
          r1 = DBLE(n+1)
          r2 = DBLE(m+1)
          r3 = DBLE(n-m+1)
          d1 = dgamln(r1)
          d2 = dgamln(r2)
          d3 = dgamln(r3)
          nlog = d1 + d2 - d3
    end function
    
    !==============================================================================84
    !  
    !   DPR1LN   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
    !        DIVISION, NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
    !        GAITHERSBURG, MARYLAND  20899
    !
    !   FOR: COMPUTING THE DOUBLE PRECISION NATURAL LOGARITHM OF
    !
    !              N1!/[M1!(N1-M1)!] N2!/[M2!(N2-M2)!]
    !              -----------------------------------
    !               (N1+N2)!/[(M1+M2)!(N1+N2-M1-M2)!]
    !
    !        WHERE N1>=0, M1>=0, M1<=N1, N2>=0, M2>=0, AND M2<=N2.  THAT
    !        VALUE IS RETURNED IN DPR1LN.
    !
    !   SUBPROGRAMS CALLED: DNCMLN (NATURAL LOGARITHM OF 'N CHOOSE M')
    !
    !   CURRENT VERSION COMPLETED MAY 1, 1989
    !   ADAPTED AND MODIFIED BY @Bernard Gingold on JULY 9 2017
    !==============================================================================84
     function dpr1ln(n1,m1,n2,m2) result(dnlog)
          implicit none
          integer(I32P), intent(in) :: n1,m1,n2,m2
          ! Locals
          real(R64P) :: d1,d2,d3,dnlog
          ! Start of executable statements
          if(n1 .LT. 0) then
             ERROR STOP "dpr1ln: [FATAL]: N1 < 0"
          end if
          if(m1 .LT. 0) then
             ERROR STOP "dpr1ln: [FATAL]: M1 < 0"
          end if
          if(m1 .GT. n1) then
             ERROR STOP "dpr1ln: [FATAL]: M1 > N1"
          end if
          if(n2 .LT. 0) then
             ERROR STOP "dpr1ln: [FATAL]: N2 < 0"
          end if
          if(m2 .LT. 0) then
             ERROR STOP "dpr1ln: [FATAL]: M2 < 0"
          end if
          if(m2 .GT. n2) then
             ERROR STOP "dpr1ln: [FATAL]: M2 > N2"
          end if
          dnlog = 0.0_R64P
          d1 = dncmln(n1,m1)
          d2 = dncmln(n2,m2)
          d3 = dncmln(n1+n2,m1+m2)
          dnlog = d1 + d2 - d3
     end function
     
     !=============================================================================84
     !  COMPUTE THE INFINITY-NORM OF THE N BY N MATRIX A
     !=============================================================================84
     pure function anormi(a,n) result(anorm)
          implicit none
          real(R64P), dimension(:,:), intent(in) :: a
!DIR$     ASSUME_ALIGNED a:32
          integer(I32P),              intent(in) :: n
          ! Locals
          real(R64P)                             :: t
          integer(I32P)                          :: i,j
          ! Start of executable statements
          do j = 1, n
              t = 0.0_R64P
!DIR$         SIMD  VECTORLENGTHFOR(REAL(kind=8))            
              do i = 1, n
                  t = t+DABS(a(i,j))
              end do
              anorm = DMAX1(anorm,t)
          end do
    end function      
    
    
    
end module