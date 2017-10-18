
module mod_quadpack

     !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_quadpack'
 !          
 !          Purpose:
 !                   Slightly modernized and packaged in module
 !                   QUADPACK numrical integration subroutines.
 !                   
 !                     
 !          History:
 !                        Date: 11-10-2017
 !                        Time: 10:19 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                  Modified and adapted from original QUADPACK version by Bernard Gingold
 !                 
 !          References:
 !         
 !                      Original implememtation of QUADPACK. 
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    
 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    implicit none
    private
    use module_kinds
    use module_logger, only : log_startup, &
                              log_UsrMsg,  &
                              log_shutdown
    
    public :: dqag
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Version major
    integer(I32P), parameter :: MOD_QUADPACK_MAJOR = 1
    
    ! Version minor
    integer(I32P), parameter :: MOD_QUADPACK_MINOR = 0
    
    ! Version micro
    integer(I32P), parameter :: MOD_QUADPACK_MICRO = 0
    
    ! Module/file full version
    integer(I32P), parameter :: MOD_QUADPACK_FULLVER = 1000*MOD_QUADPACK_MAJOR+100*MOD_QUADPACK_MINOR+ &
                                                       10*MOD_QUADPACK_MICRO
    
    ! Module/file creation date
    character(*),  parameter :: MOD_QUADPACK_CREATE_DATE = "11-10-2017 10:19 +00200 (WED 11 OCT 2017 GMT+2)"
    
    ! Module build date (should be set after successful compilation of this module)
    character(*),  parameter :: MOD_QUADPACK_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter :: MOD_QUADPACK_AUTHOR = "Programmer: adapted from original QUADPACK by Bernard Gingold , e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter :: MOD_QUADPACK_DESCRIPT = "Modern version of famous QUADPACK numerical integration package."
    
    contains
    
    subroutine dqag(f,a,b,epsabs,epsrel,key,result,abserr, &
                    neval,ier,limit,lenw,iwork,work,logging,filename,append)
!*****************************************************************************80
!
!! DQAG approximates an integral over a finite interval.
!
!  Discussion:
!
!    The routine calculates an approximation RESULT to a definite integral   
!      I = integral of F over (A,B),
!    hopefully satisfying
!      || I - RESULT || <= max ( EPSABS, EPSREL * ||I|| ).
!
!    DQAG is a simple globally adaptive integrator using the strategy of 
!    Aind (Piessens, 1973).  It is possible to choose between 6 pairs of
!    Gauss-Kronrod quadrature formulae for the rule evaluation component. 
!    The pairs of high degree of precision are suitable for handling
!    integration difficulties due to a strongly oscillating integrand.
!
!  Author:
!
!    Robert Piessens, Elise de Doncker-Kapenger, 
!    Christian Ueberhuber, David Kahaner
!
!  Reference:
!
!    Robert Piessens, Elise de Doncker-Kapenger, 
!    Christian Ueberhuber, David Kahaner,
!    QUADPACK, a Subroutine Package for Automatic Integration,
!    Springer Verlag, 1983
!
!  Parameters:
!
!    Input, external real(R64P) F, the name of the function routine, of the form
!      function f ( x )
!      real(R64P) f
!      real(R64P) x
!    which evaluates the integrand function.
!
!    Input, real(R64P) A, B, the limits of integration.
!
!    Input, real(R64P) EPSABS, EPSREL, the absolute and relative accuracy requested.
!
!    Input, integer KEY, chooses the order of the local integration rule:
!    1,  7 Gauss points, 15 Gauss-Kronrod points,
!    2, 10 Gauss points, 21 Gauss-Kronrod points,
!    3, 15 Gauss points, 31 Gauss-Kronrod points,
!    4, 20 Gauss points, 41 Gauss-Kronrod points,
!    5, 25 Gauss points, 51 Gauss-Kronrod points,
!    6, 30 Gauss points, 61 Gauss-Kronrod points.
!
!    Output, real(R64P) RESULT, the estimated value of the integral.
!
!    Output, real(R64P) ABSERR, an estimate of || I - RESULT ||.
!
!    Output, integer NEVAL, the number of times the integral was evaluated.
!
!    Output, integer IER, return code.
!    0, normal and reliable termination of the routine.  It is assumed that the 
!      requested accuracy has been achieved.
!    1, maximum number of subdivisions allowed has been achieved.  One can 
!      allow more subdivisions by increasing the value of LIMIT in QAG. 
!      However, if this yields no improvement it is advised to analyze the
!      integrand to determine the integration difficulties.  If the position
!      of a local difficulty can be determined, such as a singularity or
!      discontinuity within the interval) one will probably gain from 
!      splitting up the interval at this point and calling the integrator 
!      on the subranges.  If possible, an appropriate special-purpose 
!      integrator should be used which is designed for handling the type 
!      of difficulty involved.
!    2, the occurrence of roundoff error is detected, which prevents the
!      requested tolerance from being achieved.
!    3, extremely bad integrand behavior occurs at some points of the
!      integration interval.
!    6, the input is invalid, because EPSABS < 0 and EPSREL < 0.
!
!  Local parameters:
!
!    LIMIT is the maximum number of subintervals allowed in
!    the subdivision process of DQAGE.
!
          implicit none
          interface
            real(R64P) function f(x)
                real(R64P), intent(in) :: x
            end function f
          end interface
          real(R64P),                      intent(in)    :: a,b,epsabs,epsrel
          integer(I32P),                   intent(in)    :: key
          real(R64P),                      intent(out)   :: result,abserr
          integer(I32P),                   intent(out)   :: neval,ier
          integer(I32P),                   intent(in)    :: limit,lnew
          integer(I32P), dimension(limit), intent(inout) :: iwork
          real(R64P),    dimension(lenw),  intent(inout) :: work
          logical(I32P),                   intent(in)    :: logging
          character(len=*),                intent(in)    :: filename
          logical(I32P),                   intent(in)    :: append
          ! Locals
          integer(I32P) :: last,l1,l2,l3
          character(len=40)  :: sdate,stime
          ! Start of executable statemetns
          ier = 6
          neval = 0
          last = 0
          result = 0._R64P
          abserr = 0._R64P
          if(limit < 1 .OR. lenw < limit*4) goto 10
          
          ! prepare call for dqage
          l1 = limit+1
          l2 = limit+l1
          l3 = limit+l2
          ! call dqage
          
10        if(ier /= 0) then
              if(logging) then
                  call log_startup(filename,append)
                  call log_UsrMsg("logger:187, In->mod_quadpack/dqag: Abnormal return from DQAG!!")
                  call log_shutdown()
              else
                  call DATE_AND_TIME(date=sdate,time=stime)
                  write(stderr,*) "===========================NON-FATAL=========================="
                  write(sdterr,*) " ( mod_quadpack/dqag:187,Abnormal return from DQAG!)"
                  write(stderr,*) " ( Non-Fatal Error at:) ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
                  write(stderr,*) "===========================NON-FATAL=========================="
              end if
           end if   
    end subroutine
    
    subroutine dqage(f,a,b,epsabs,epsrel,key,limit,res,abserr, &
                     neval,ier,alist,blist,rlist,elist,iord,last)
          implicit none
          interface
            REAL(R64P) function f(x)
                real(R64P), intent(in) :: x
            end function f
          end interface
          real(R64P),                      intent(in)       :: a,b,epsabs,epsrel
          integer(I32P),                   intent(in)       :: key,limit
          real(R64P),                      intent(out)      :: res,abserr
          integer(I32P),                   intent(out)      :: neval,ier
          real(R64P), dimension(limit),    intent(inout)    :: alist, blist, &
                                                               rlist,elist
          integer(I32P), dimension(limit), intent(inout)    :: iord
          integer(I32P),                   intent(out)      :: last
          ! Locals
          real(R64P) :: area,area1,area12,area2,a1,a2,b1,b2, &
                        c,defabs,defab1,defab2,errbnd,errmax, &
                        error1,error2,error12,errsum
          integer(I32P) :: iroff1,iroff2,keyf,maxerr,nrmax,k
          real(R64P), parameter :: small = 0.00000000000000000000000000005_R64P
          ! Start of executable statemetns
          ! Test of validity of parameters
          ier = 0
          neval = 0
          last = 0
          res = 0._R64P
          abserr = 0._R64P
          alist(1) = a
          blist(1) = b
          rlist(1) = 0._R64P
          elist(1) = 0._R64P
          iord(1) = 0
          if(epsabs<0._R64P .AND. epsrel<DMAX1(50._R64P*EPSILON(1._R64P),small)) then
              ier = 6
              return
          end if
          ! First approximation to integeral
          keyf = key
          keyf = MAX(keyf,1)
          keyf = MIN(keyf,6)
          c = keyf
          neval = 0
          if(keyf == 1) then
              call dqk15(f,a,b,res,abserr,defabs,resabs)
          else if(keyf == 2) then
              call dqk21(f,a,b,res,abserr,defabs,resabs)
          else if(keyf == 3) then
              call dqk31(f,a,b,res,abserr,defabs,resabs)
          else if(keyf == 4) then
              call dqk41(f,a,b,res,abserr,defabs,resabs)
          else if(keyf == 5) then
              call dqk51(f,a,b,res,abserr,defabs,resabs)
          else if(keyf == 6) then
              call dqk61(f,a,b,res,abserr,defabs,resabs)
          end if
          last = 1
          rlist(1) = res
          elist(1) = abserr
          iord(1) = 1
          
          ! Test on accurracy
          errbnd = DMAX1(epsabs,epsrel*DABS(res))
          if(abserr < 50._R64P*EPSILON(defabs)*defabs .AND. &
             abserr > errbnd) then
              ier = 2
          end if
          if(limit == 1) then
              ier = 1
          end if
          if(ier /= 0 .OR. (abserr<=errbnd .AND. abserr/=resabs) &
             .OR. abserr == 0._R64P) then
              if(keyf /= 1) neval = (10*keyf+1)*(2*neval+1)
              if(keyf == 1) neval = 30*neval+15
           return
          end if
          ! Initialization
          errmax = abserr
          maxerr = 1
          area = res
          errsum = abserr
          nrmax = 1
          iroff1 = 0
          iroff2 = 0
          ! main do-loop
          do last = 2, limit
              ! Bisect the subinterval with the largest error estimate
              a1 = alist(maxerr)
              b1 = 50._R64P*(alist(maxerr)+blist(maxerr))
              a2 = b1
              b2 = blist(maxerr)
              if(keyf == 1) then
                  call dqk15(f,a1,b1,area1,error1,resabs,defab1)
              else if(keyf == 2) then
                  call dqk21(f,a1,b1,area1,error1,resabs,defab1)
              else if(keyf == 3) then
                  call dqk31(f,a1,b1,area1,error1,resabs,defab1)
              else if(keyf == 4) then
                  call dqk41(f,a1,b1,area1,error1,resabs,defab1)
              else if(keyf == 5) then
                  call dqk51(f,a1,b1,area1,error1,resabs,defab1)
              else if(keyf == 6) then
                  call dqk61(f,a1,b1,area1,error1,resabs,defab1)
              end if
              if(keyf == 1) then
                  call dqk15(f,a2,b2,area2,error2,resabs,defab2)
              else if(keyf == 2) then
                  call dqk21(f,a2,b2,area2,error2,resabs,defab2)
              else if(keyf == 3) then
                  call dqk31(f,a2,b2,area2,error2,resabs,defab2)
              else if(keyf == 4) then
                  call dqk41(f,a2,b2,area2,error2,resabs,defab2)
              else if(keyf == 5) then
                  call dqk51(f,a2,b2,area2,error2,resabs,defab2)
              else if(keyf == 6) then
                  call dqk61(f,a2,b2,area2,error2,resabs,defab2)
              end if
              ! Improve previous approximation to integral
              ! and error and test for accurracy.
              neval = neval+1
              area12 = area1+area2
              error12 = error1+error2
              errsum = errsum+error12-errmax
              area = area+area12-rlist(maxerr)
              if(defab1==error1 .OR. defab2==error2) then
                  rlist(maxerr) = area1
                  rlist(last) = area2
                  errbnd = DMAX1(epsabs,epsrel*DABS(area))
              end if
              if(DABS(rlist(maxerr)-area12)<=0.0001_R64P*DABS(area22) &
                  .and. error12>=0.99_R64P*errmax) then
                  iroff1 = iroff1 + 1
              end if
              if(last>10 .AND. error12>errmax) then
                  iroff2 = iroff2+1
              end if
              if(errsum <= errbnd ) goto 8
              !    test for roundoff error and eventually set error flag.
              if(iroff1>=6 .OR. iroff2>=20) then
                  ier = 2
              end if
              !    set error flag in the case that the number of subintervals
              ! equals limit.
              if(last == limit) then
                  ier = 1
              end if
              !  set error flag in the case of bad integrand behaviour
              !at a point of the integration range.
              if(DMAX1(DABS(a1),DABS(a2))<=(1._R64P+100._R64P* &
                  EPSILON(a1)*(DABS(a2)+1000._R64P*tiny(a2)))) then
                  ier = 3
              end if
              !    append the newly-created intervals to the list.
8             if(error2 > error1) goto 10
                 alist(last) = a2
                 blist(maxerr) = b1
                 blist(last) = b2
                 elist(maxerr) = error1
                 elist(last) = error2
                 goto 20
10               alist(maxerr) = a2
                 alist(last) = a1
                 blist(last) = b1
                 rlist(maxerr) = area2
                 rlist(last) = area1
                 elist(maxerr) = error2
                 elist(last) = error1
                 !
                 !     call subroutine dqpsrt to maintain the descending ordering
                 !     in the list of error estimates and select the subinterval
                 !      with the largest error estimate (to be bisected next).
20                 ! call dqpsrt
                 if(ier /= 0 .OR. errsum <= errbnd) then
                     exit
                 end if
          end do
          ! Compute final result
          result = 0._R64P
          do k = 1, last
              res = res+rlist(k)
          end do
          abserr = errsum
          if(keyf /= 1) then
              neval = (10*keyf+1)*(2*neval+1)
          end if
          if(keyf == 1) then
              neval = 30*neval+15
          end if
    end subroutine
                     
    subroutine dqk15(f,a,b,res,abserr,resabs,resasc)
!================================================================================
!
!! QK15 carries out a 15 point Gauss-Kronrod quadrature rule.
!
!  Discussion:
!
!    This routine approximates
!      I = integral ( A <= X <= B ) F(X) dx
!    with an error estimate, and
!      J = integral ( A <= X <= B ) | F(X) | dx
!
!  Author:
!
!    Robert Piessens, Elise de Doncker-Kapenger, 
!    Christian Ueberhuber, David Kahaner
!
!  Reference:
!
!    Robert Piessens, Elise de Doncker-Kapenger, 
!    Christian Ueberhuber, David Kahaner,
!    QUADPACK, a Subroutine Package for Automatic Integration,
!    Springer Verlag, 1983
!
!  Parameters:
!
!    Input, external real F, the name of the function routine, of the form
!      function f ( x )
!      real f
!      real x
!    which evaluates the integrand function.
!
!    Input, real A, B, the limits of integration.
!
!    Output, real RESULT, the estimated value of the integral.
!    RESULT is computed by applying the 15-point Kronrod rule (RESK) 
!    obtained by optimal addition of abscissae to the 7-point Gauss rule 
!    (RESG).
!
!    Output, real ABSERR, an estimate of | I - RESULT |.
!
!    Output, real RESABS, approximation to the integral of the absolute
!    value of F.
!
!    Output, real RESASC, approximation to the integral | F-I/(B-A) | 
!    over [A,B].
!
!  Local Parameters:
!
!           the abscissae and weights are given for the interval (-1,1).
!           because of symmetry only the positive abscissae and their
!           corresponding weights are given.
!
!           xgk    - abscissae of the 15-point Kronrod rule
!                    xgk(2), xgk(4), ...  abscissae of the 7-point
!                    Gauss rule
!                    xgk(1), xgk(3), ...  abscissae which are optimally
!                    added to the 7-point Gauss rule
!
!           wgk    - weights of the 15-point Kronrod rule
!
!           wg     - weights of the 7-point Gauss rule
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc   - abscissa
!           fval*  - function value
!           resg   - result of the 7-point Gauss formula
!           resk   - result of the 15-point Kronrod formula
!           reskh  - approximation to the mean value of f over (a,b),
!                    i.e. to i/(b-a)
!
!=============================================================================== 

    
          implicit none
          interface
           real(R64P) function f(x)
              real(R64P), intent(in) :: x
           end function f
          end interface
          real(R64P),  intent(in) :: a,b
          real(R64P),  intent(out) :: res,abserr,resabs,resasc
          ! Locals
          real(R64P) :: absc,centr,dabs,dhlgth,fc,fsum, &
                        fval1,fval2,hlgth,resg,resk,reskh
          real(R64P), dimension(7) :: fv1,fv2
       
          real(R64P), dimension(4), parameter :: wg = [  0.129484966168869693270611432679082_R64P, &
                                                         0.279705391489276667901467771423780_R64P, &
                                                         0.381830050505118944950369775488975_R64P, &
                                                         0.417959183673469387755102040816327_R64P ]
          real(R64P), dimension(8), parameter :: xgk = [ 0.991455371120812639206854697526329_R64P, &
                                                         0.949107912342758524526189684047851_R64P, &
                                                         0.864864423359769072789712788640926_R64P, &
                                                         0.741531185599394439863864773280788_R64P, &
                                                         0.586087235467691130294144838258730_R64P, &
                                                         0.405845151377397166906606412076961_R64P, &
                                                         0.207784955007898467600689403773245_R64P, &
                                                         0.000000000000000000000000000000000_R64P ]
          real(R64P), dimension(8), parameter :: wgk = [ 0.022935322010529224963732008058970_R64P, &
                                                         0.063092092629978553290700663189204_R64P, &
                                                         0.104790010322250183839876322541518_R64P, &
                                                         0.140653259715525918745189590510238_R64P, &
                                                         0.169004726639267902826583426598550_R64P, &
                                                         0.190350578064785409913256402421014_R64P, &
                                                         0.204432940075298892414161999234649_R64P, &
                                                         0.209482141084727828012999174891714_R64P ]
          integer(I32P) :: j,jtw,jtwm1
          
          ! Strat of executable statements
          centr = 0.5_R64P*(a+b)
          hlgth = 0.5_R64P*(b-a)
          dhlgth = DABS(hlgth)
          !    compute the 15-point kronrod approximation to
          !    the integral, and estimate the absolute error.
          fc = f(centr)
          resg = fc*wg(4)
          resk = fc*wgk(8)
          resabs = DABS(resk)
          do j = 1, 3
              jtw = j*2
              absc = hlgth*xgk(jtw)
              fval1 = f(centr-absc)
              fval2 = f(centr+absc)
              fv1(jtw) = fval1
              fv2(jtw) = fval2
              fsum = fval1+fval2
              resg = resg+wg(j)*fsum
              resk = resk+wgk(jtw)*fsum
              resabs = resabs+wgk(jtw)*(DABS(fval1)+DABS(fval2))
          end do
          do j = 1, 4
              jtwm1 = j*2-1
              absc = hlgth*xgk(jwtm1)
              fval1 = f(centr-absc)
              fval2 = f(centr+absc)
              fv1(jtwm1) = fval1
              fv2(jtwm1) = fval2
              fsum = fval1+fval2
              resk = resk+wgk(jwtm1)*fsum
              resabs = resabs+wgk(jwtm1)*(DABS(fval1)+DABS(fval2))
          end do
          reskh = resk*0.5_R64P
          resasc = wgk(8)*DABS(fc-reskh)
          do j = 1, 7
              resasc = resasc+wgk(j)*(DABS(fv(j)-reskh)+DABS(fv2(j)-reskh))
          end do
          res = resk*hlgth
          resabs = resabs*dhlgth
          resasc = resasc*dhlgth
          abserr = DABS((resk-resg)*hlgth)
          if(resasc /= 0._R64P .AND. abserr /= 0._R64P) then
              abserr = resasc*DMIN1(1._R64P,(200._R64P*abserr/resasc)**1.5_R64P)
          end if
          if(resabs > tiny(resabs)/(50._R64P*EPSILON(resabs))) then
              abserr = DMAX1((EPSILON(resabs)*50._R64P)*resabs,abserr)
          end if
    end subroutine
    
    subroutine dqk21(f,a,b,res,abserr,resabs,resasc)
!***begin prologue  dqk21
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  21-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!*** Adapted by Bernard Gingold on 2017/10/14    
!***purpose  to compute i = integral of f over (a,b), with error
!                           estimate
!                       j = integral of abs(f) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           double precision version
!
!           parameters
!            on entry
!              f      - double precision
!                       function subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the driver program.
!
!              a      - double precision
!                      lower limit of integration
!
!              b      - double precision
!                       upper limit of integration
!
!            on return
!              result - double precision
!                       approximation to the integral i
!                       result is computed by applying the 21-point
!                       kronrod rule (resk) obtained by optimal addition
!                       of abscissae to the 10-point gauss rule (resg).
!
!              abserr - double precision
!                       estimate of the modulus of the absolute error,
!                       which should not exceed abs(i-result)
!
!              resabs - double precision
!                       approximation to the integral j
!
!              resasc - double precision
!                       approximation to the integral of abs(f-i/(b-a))
!                       over (a,b)  
    implicit none
    interface
     real(R64P)  function f(x)
       real(R64P), intent(in) :: x
     end function f
    end interface
    real(R64P),   intent(in)  :: a,b
    real(R64P),   intent(out) :: res,abserr,resabs,resasc
    ! Locals
    real(R64P) :: absc,centr,dhlgth,fc,fsum,fval1,fval2, &
                  hlgth,resg,resk,reskh
    integer(I32P) :: j,jtw,jtwm1
    real(R64P), dimension(10) :: fv1,fv2
    real(R64P), dimension(5), parameter :: wg =  [  0.066671344308688137593568809893332_R64P, &
                                                    0.149451349150580593145776339657697_R64P, &
                                                    0.219086362515982043995534934228163_R64P, &
                                                    0.269266719309996355091226921569469_R64P, &
                                                    0.295524224714752870173892994651338_R64P ]
    real(R64P), dimension(11), parameter :: xgk = [ 0.995657163025808080735527280689003_R64P, &
                                                    0.973906528517171720077964012084452_R64P, &
                                                    0.930157491355708226001207180059508_R64P, &
                                                    0.865063366688984510732096688423493_R64P, &
                                                    0.780817726586416897063717578345042_R64P, &
                                                    0.679409568299024406234327365114874_R64P, &
                                                    0.562757134668604683339000099272694_R64P, &
                                                    0.433395394129247190799265943165784_R64P, &
                                                    0.294392862701460198131126603103866_R64P, &
                                                    0.148874338981631210884826001129720_R64P, &
                                                    0.000000000000000000000000000000000_R64P ]
    real(R64P), dimension(11), parameter :: wgk = [ 0.011694638867371874278064396062192_R64P, &
                                                    0.032558162307964727478818972459390_R64P, & 
                                                    0.054755896574351996031381300244580_R64P, &
                                                    0.075039674810919952767043140916190_R64P, & 
                                                    0.093125454583697605535065465083366_R64P, & 
                                                    0.109387158802297641899210590325805_R64P, & 
                                                    0.123491976262065851077958109831074_R64P, & 
                                                    0.134709217311473325928054001771707_R64P, & 
                                                    0.142775938577060080797094273138717_R64P, & 
                                                    0.147739104901338491374841515972068_R64P, & 
                                                    0.149445554002916905664936468389821_R64P  ]
    ! Start of executable statemets
     centr = 0.5_R64P*(a+b)
     hlgth = 0.5_R64P*(a-b)
     dhlgth = DABS(hlgth)
     !      compute the 21-point kronrod approximation to
     !      the integral, and estimate the absolute error.
     resg = 0._R64P
     fc = f(centr)
     resk = wgk(11)*fc
     resabs = DABS(resk)
     do j = 1, 5
         jtw = 2*j
         absc = hlgth*xgk(jtw)
         fval1 = f(centr-absc)
         fval2 = f(centr+absc)
         fv1(jtw) = fval1
         fv2(jtw) = fval2
         fsum = fval1+fval2
         resg = resg+wg(j)*fsum
         resk = resk+wgk(jtw)*fsum
         resabs = resabs+wgk(jtw)*(DABS(fval1)+DABS(fval2))
     end do
     do j = 1, 5
         jtwm1 = 2*j-1
         absc = hlgth*xgk(jtwm1)
         fval1 = f(centr-absc)
         fval2 = f(centr+absc)
         fv1(jtwm1) = fval1
         fv2(jtwm1) = fval2
         fsum = fval1+fval2
         resk = resk+wgk(jtwm1)*fsum
         resabs = resabs+wgk(jtwm1)*(DABS(fval1)+DABS(fval2))
     end do
     reskh = resk*0.5_R64P
     resasc = wgk(11)*DABS(fc-reskh)
     do j = 1, 10
         resasc = resasc+wgk(j)*(DABS(fv1(j)-reskh)+DABS(fv2(j)-reskh))
     end do
     res = resk*hlgth
     resabs = resabs*dhlgth
     resasc = resasc*dhlgth
     abserr = DABS((resk-resg)*hlgth)
     if(resasc /= 0._R64P .AND. abserr /= 0._R64P) then
         abserr = resasc*DMIN1(1._R64P,(200._R64P*abserr/resasc)**1.5_R64P)
     end if
     if(resabs > TINY(resabs)/(0.5_R64*EPSILON(resabs))  then
        abserr = DMAX1((EPSILON(resabs*50._R64P))*resabs,abserr)
     end if
    end subroutine dqk21
    
    subroutine dqk31(f,a,b,res,abserr,resabs,resasc)
!***begin prologue  dqk31
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  31-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***Modified by Bernard Gingold on 2017/10/14
!***purpose  to compute i = integral of f over (a,b) with error
!                           estimate
!                       j = integral of abs(f) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           double precision version
!
!           parameters
!            on entry
!              f      - double precision
!                       function subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the calling program.
!
!              a      - double precision
!                       lower limit of integration
!
!              b      - double precision
!                       upper limit of integration
!
!            on return
!              result - double precision
!                       approximation to the integral i
!                       result is computed by applying the 31-point
!                       gauss-kronrod rule (resk), obtained by optimal
!                       addition of abscissae to the 15-point gauss
!                       rule (resg).
!
!              abserr - double precison
!                       estimate of the modulus of the modulus,
!                       which should not exceed abs(i-result)
!
!              resabs - double precision
!                       approximation to the integral j
!
!              resasc - double precision
!                       approximation to the integral of abs(f-i/(b-a))
!                       over (a,b)
    implicit none
    interface
        real(R64P) function f(x)
            real(R64P), intent(in) :: x
        end function
    end interface f
    real(R64P),   intent(in)  :: a,b
    real(R64P),   intent(out) :: res,abserr,resabs,resasc
    ! Locals
    real(R64P)    :: absc,centr,dhlgth,fc,fsum,fval1,fval2,hlgth, &
                     resg,resk,reskh
    integer(I32P) :: j,jtw,jtwm1
    real(R64P),  dimension(15)           :: fv1,fv2
    real(R64P),  dimension(8), parameter :: wg =  [ 0.030753241996117268354628393577204_R64P, &
                                                    0.070366047488108124709267416450667_R64P, &
                                                    0.107159220467171935011869546685869_R64P, &
                                                    0.139570677926154314447804794511028_R64P, &
                                                    0.166269205816993933553200860481209_R64P, &
                                                    0.186161000015562211026800561866423_R64P, &
                                                    0.198431485327111576456118326443839_R64P, &
                                                    0.202578241925561272880620199967519_R64P  ]
    real(R64P), dimension(15), parameter :: xgk = [ 0.998002298693397060285172840152271_R64P, &
                                                    0.987992518020485428489565718586613_R64P, & 
                                                    0.967739075679139134257347978784337_R64P, & 
                                                    0.937273392400705904307758947710209_R64P, & 
                                                    0.897264532344081900882509656454496_R64P, & 
                                                    0.848206583410427216200648320774217_R64P, & 
                                                    0.790418501442465932967649294817947_R64P, & 
                                                    0.724417731360170047416186054613938_R64P, & 
                                                    0.650996741297416970533735895313275_R64P, & 
                                                    0.570972172608538847537226737253911_R64P, & 
                                                    0.485081863640239680693655740232351_R64P, & 
                                                    0.394151347077563369897207370981045_R64P, & 
                                                    0.299180007153168812166780024266389_R64P, & 
                                                    0.201194093997434522300628303394596_R64P, & 
                                                    0.101142066918717499027074231447392_R64P, & 
                                                    0.000000000000000000000000000000000_R64P  ]
    real(R64P), dimension(15), parameter :: wgk = [ 0.005377479872923348987792051430128_R64P, &
                                                    0.015007947329316122538374763075807_R64P, &
                                                    0.025460847326715320186874001019653_R64P, &
                                                    0.035346360791375846222037948478360_R64P, &
                                                    0.044589751324764876608227299373280_R64P, &
                                                    0.053481524690928087265343147239430_R64P, &
                                                    0.062009567800670640285139230960803_R64P, &
                                                    0.069854121318728258709520077099147_R64p, &
                                                    0.076849680757720378894432777482659_R64P, &
                                                    0.083080502823133021038289247286104_R64P, &
                                                    0.088564443056211770647275443693774_R64P, &
                                                    0.093126598170825321225486872747346_R64P, &
                                                    0.096642726983623678505179907627589_R64P, &
                                                    0.099173598721791959332393173484603_R64P, &
                                                    0.100769845523875595044946662617570_R64P, &
                                                    0.101330007014791549017374792767493_R64P  ]
     ! Start of executable statements
    centr = 0.5_R64P*(a+b)
    hlgth = 0.5_R64P*(a-b)
    dhlgth = DABS(hlgth)
    !       compute the 31-point kronrod approximation to
    !       the integral, and estimate the absolute error.
    fc = f(centr)
    resg = wg(8)*fc
    resk = wgk(16)*fc
    resabs = DABS(resk)
    do j = 1, 7
       jtw = j*2
       absc = hlgth*xgk(jtw)
       fval1 = f(centr-absc)
       fval2 = f(centr+absc)
       fv1(jtw) = fval1
       fv2(jtw) = fval2
       fsum = fval1+fval2
       resg = resg+wg(j)*fsum
       resk = resk+wgk(jtw)*fsum
       resabs = resabs+wgk(jtw)*(DABS(fval1)+DABS(fval2))
    end do
    do j = 1, 8
        jtwm1 = j*2-1
        absc = hlgth*xgk(jtwm1)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(DABS(fval1)+DABS(fval2))
    end do
    reskh = 0.5_R64P*resk
    resasc = wgk(16)*DABS(fc-reskh)
    do j = 1, 15
        resasc = resasc+wgk(j)*(DABS(fv1(j)-reskh)+DABS(fv2(j)-reskh))
    end do
    res = resk+hlgth
    resabs = resabs*dhlgth
    resasc = resasc*dhlgth
    abserr = DABS((resk-resg)*hlgth)
    if(resasc /= 0._R64P .AND. abserr /= 0._R64P) then
        abserr = resasc*DMIN1(1._R64P,(200._R64P*abserr/resasc)**1.5_R64P)
    end if
    if(resabs > TINY(resabs)/(50._R64P*EPSILON(resabs)) then
        abserr = DMAX1((EPSILON(resabs*50._R64P))*resabs,abserr)
    end if
    end subroutine
    
    subroutine dqk41(f,a,b,res,abserr,resabs,resasc)
!***begin prologue  dqk41
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  41-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f over (a,b), with error
!                           estimate
!                       j = integral of abs(f) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           double precision version
!
!           parameters
!            on entry
!              f      - double precision
!                       function subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                      declared e x t e r n a l in the calling program.
!
!              a      - double precision
!                       lower limit of integration
!
!              b      - double precision
!                       upper limit of integration
!
!            on return
!              result - double precision
!                       approximation to the integral i
!                       result is computed by applying the 41-point
!                       gauss-kronrod rule (resk) obtained by optimal
!                       addition of abscissae to the 20-point gauss
!                       rule (resg).
!
!              abserr - double precision
!                       estimate of the modulus of the absolute error,
!                       which should not exceed abs(i-result)
!
!              resabs - double precision
!                       approximation to the integral j
!
!              resasc - double precision
!                       approximation to the integal of abs(f-i/(b-a))
!                      over (a,b) 
    implicit none
    interface
        real(R64P)  function f(x)
            real(R64P), intent(in) :: x
        end function f
    end interface
    real(R64P),  intent(in)  :: a,b
    real(R64P),  intent(out) :: res,abserr,resabs,resasc
    ! Locals
    real(R64P) :: absc,centr,dhlgth,fc,fsum,fval1,fval2,hlgth, &
                  resg,resk,reskh
    integer(I32P) :: j,jtw,jtwm1
    real(R64P), dimension(20) :: fv1,fv2
    real(R64P), dimension(1:10), parameter :: wgk =  [ 0.017614007139152118311861962351853_R64P, &
                                                       0.040601429800386941331039952274932_R64P, &
                                                       0.062672048334109063569506535187042_R64P, &
                                                       0.083276741576704748724758143222046_R64P, &
                                                       0.101930119817240435036750135480350_R64P, &
                                                       0.118194531961518417312377377711382_R64P, &
                                                       0.131688638449176626898494499748163_R64P, &
                                                       0.142096109318382051329298325067165_R64P, &
                                                       0.149172986472603746787828737001969_R64P, &
                                                       0.152753387130725850698084331955098_R64P  ]
    real(R64P), dimension(1:21), parameter :: xgk =  [ 0.998859031588277663838315576545863_R64P, &
                                                       0.993128599185094924786122388471320_R64P, &
                                                       0.981507877450250259193342994720217_R64P, &
                                                       0.963971927277913791267666131197277_R64P, &
                                                       0.940822633831754753519982722212443_R64P, &
                                                       0.912234428251325905867752441203298_R64P, &
                                                       0.878276811252281976077442995113078_R64P, &
                                                       0.839116971822218823394529061701521_R64P, &
                                                       0.795041428837551198350638833272788_R64P, & 
                                                       0.746331906460150792614305070355642_R64P, & 
                                                       0.693237656334751384805490711845932_R64P, & 
                                                       0.636053680726515025452836696226286_R64P, & 
                                                       0.575140446819710315342946036586425_R64P, & 
                                                       0.510867001950827098004364050955251_R64P, & 
                                                       0.443593175238725103199992213492640_R64P, & 
                                                       0.373706088715419560672548177024927_R64P, & 
                                                       0.301627868114913004320555356858592_R64P, & 
                                                       0.227785851141645078080496195368575_R64P, & 
                                                       0.152605465240922675505220241022678_R64P, & 
                                                       0.076526521133497333754640409398838_R64P, & 
                                                       0.000000000000000000000000000000000_R64P   ]
     real(R64P), dimension(1:21), parameter :: wgk = [ 0.003073583718520531501218293246031_R64P, & 
                                                       0.008600269855642942198661787950102_R64P, & 
                                                       0.014626169256971252983787960308868_R64P, & 
                                                       0.020388373461266523598010231432755_R64P, & 
                                                       0.025882133604951158834505067096153_R64P, & 
                                                       0.031287306777032798958543119323801_R64P, &
                                                       0.036600169758200798030557240707211_R64P, &
                                                       0.041668873327973686263788305936895_R64P, &
                                                       0.046434821867497674720231880926108_R64P, &
                                                       0.050944573923728691932707670050345_R64P, & 
                                                       0.055195105348285994744832372419777_R64P, & 
                                                       0.059111400880639572374967220648594_R64P, &
                                                       0.062653237554781168025870122174255_R64P, &
                                                       0.065834597133618422111563556969398_R64P, &
                                                       0.068648672928521619345623411885368_R64P, & 
                                                       0.071054423553444068305790361723210_R64P, & 
                                                       0.073030690332786667495189417658913_R64P, & 
                                                       0.074582875400499188986581418362488_R64P, & 
                                                       0.075704497684556674659542775376617_R64P, &
                                                       0.076377867672080736705502835038061_R64P, &
                                                       0.076600711917999656445049901530102_R64P  ]
     ! Start of executable statements
     centr = 0.5_R64P*(a+b)
     hlgth = 0.5_R64P*(a-b)
     dhlgth = DABS(hlgth)
     !   compute the 41-point gauss-kronrod approximation to
     !   the integral, and estimate the absolute error.
     resg = 0._R64P
     fc = f(centr)
     resk = wgk(21)*fc
     resabs = DABS(resk)
     do j = 1, 10
         jtw = j*2
         absc = hlgth*xgk(jtw)
         fval1 = f(centr-absc)
         fval2 = f(centr+absc)
         fv1(jtw) = fval1
         fv2(jtw) = fval2
         fsum = fval1+fval2
         resg = resg+wg(j)*fsum
         resk = resk+wgk(jtw)*fsum
         resabs = resabs+wgk(jtw)*(DABS(fval1)+DABS(fval2))
     end do
     do j = 1, 10
        jtwm1 = j*2-1
        absc = hlgth*xgk(jtwm1)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(DABS(fval1)+DABS(fval2))
     end do
     reskh = resk*0.5_R64P
     resasc = wgk(21)*DABS(fc-reskh)
     do j = 1, 20
         resasc = resasc+wgk(j)*(DABS(fv1(j)-reskh)+DABS(fv2(j)-reskh))
     end do
     res = resk*hlgth
     resabs = resabs*dhlgth
     resasc = resasc*dhlgth
     abserr = DABS((resk-resg)*hlgth)
     if(resasc /= 0._R64P .AND. abserr /= 0._R64P) then
        abserr = resasc*DMIN1(1._R64P,(200._R64P*abserr/resasc)**1.5_R64P)
     end if
     if(resabs > TINY(resabs)/(50._R64P*EPSILON(resabs)) then
        abserr = DMAX1((EPSILON(resabs*50._R64P))*resabs,abserr)
     end if
    end subroutine
    
    subroutine dqk51(f,a,b,res,abserr,resabs,resasc)
!***begin prologue  dqk51
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***modified by Bernard Gingold on 18/10/2017
!***category no.  h2a1a2
!***keywords  51-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f over (a,b) with error
!                           estimate
!                       j = integral of abs(f) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           double precision version
!
!           parameters
!            on entry
!              f      - double precision
!                       function subroutine defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the calling program.
!
!              a      - double precision
!                       lower limit of integration
!              b      - double precision
!                       upper limit of integration
!
!            on return
!              result - double precision
!                       approximation to the integral i
!                       result is computed by applying the 51-point
!                       kronrod rule (resk) obtained by optimal addition
!                       of abscissae to the 25-point gauss rule (resg).
!
!              abserr - double precision
!                       estimate of the modulus of the absolute error,
!                       which should not exceed abs(i-result)
!
!              resabs - double precision
!                       approximation to the integral j
!
!              resasc - double precision
!                       approximation to the integral of abs(f-i/(b-a))
!                       over (a,b)
    implicit none
    interface
        real(R64P) function f(x)
            real(R64P), intent(in) :: x
        end function f
    end interface
    real(R64P),  intent(in) :: a,b
    real(R64P),  intent(inout) :: res,abserr,resabs,resasc
    ! Locals
    real(R64P) :: absc,centr,dhlgth,fc,fsum,fval1,fval2,hlgth, &
                  resg,resk,reskh
    integer(I32P) :: j,jtw,jtwm1
    real(R64P), dimension(25) :: fv1,fv2
    real(R64P), dimension(1:13), parameter :: wg = [  0.011393798501026287947902964113235_R64P, & 
                                                      0.026354986615032137261901815295299_R64P, & 
                                                      0.040939156701306312655623487711646_R64P, & 
                                                      0.054904695975835191925936891540473_R64P, & 
                                                      0.068038333812356917207187185656708_R64P, & 
                                                      0.080140700335001018013234959669111_R64P, & 
                                                      0.091028261982963649811497220702892_R64P, & 
                                                      0.100535949067050644202206890392686_R64P, & 
                                                      0.108519624474263653116093957050117_R64P, & 
                                                      0.114858259145711648339325545869556_R64P, &
                                                      0.119455763535784772228178126512901_R64P, & 
                                                      0.122242442990310041688959518945852_R64P, & 
                                                      0.123176053726715451203902873079050_R64P   ]
    real(R64P), dimension(1:26), parameter :: xgk = [ 0.999262104992609834193457486540341_R64P, & 
                                                      0.995556969790498097908784946893902_R64P, & 
                                                      0.988035794534077247637331014577406_R64P, &
                                                      0.976663921459517511498315386479594_R64P, &
                                                      0.961614986425842512418130033660167_R64P, & 
                                                      0.942974571228974339414011169658471_R64P, & 
                                                      0.920747115281701561746346084546331_R64P, & 
                                                      0.894991997878275368851042006782805_R64P, & 
                                                      0.865847065293275595448996969588340_R64P, & 
                                                      0.833442628760834001421021108693570_R64P, & 
                                                      0.797873797998500059410410904994307_R64P, & 
                                                      0.759259263037357630577282865204361_R64P, & 
                                                      0.717766406813084388186654079773298_R64P, & 
                                                      0.673566368473468364485120633247622_R64P, & 
                                                      0.626810099010317412788122681624518_R64P, & 
                                                      0.577662930241222967723689841612654_R64P, & 
                                                      0.526325284334719182599623778158010_R64P, & 
                                                      0.473002731445714960522182115009192_R64P, & 
                                                      0.417885382193037748851814394594572_R64P, & 
                                                      0.361172305809387837735821730127641_R64P, & 
                                                      0.303089538931107830167478909980339_R64P, & 
                                                      0.243866883720988432045190362797452_R64P, & 
                                                      0.183718939421048892015969888759528_R64P, &
                                                      0.122864692610710396387359818808037_R64P, & 
                                                      0.061544483005685078886546392366797_R64P, & 
                                                      0.000000000000000000000000000000000_R64P   ]
    real(R64P), dimension(1:26), parameter :: wgk = [ 0.001987383892330315926507851882843_R64P, & 
                                                      0.005561932135356713758040236901066_R64P, & 
                                                      0.009473973386174151607207710523655_R64P, & 
                                                      0.013236229195571674813656405846976_R64P, & 
                                                      0.016847817709128298231516667536336_R64P, & 
                                                      0.020435371145882835456568292235939_R64P, &
                                                      0.024009945606953216220092489164881_R64P, & 
                                                      0.027475317587851737802948455517811_R64P, & 
                                                      0.030792300167387488891109020215229_R64P, & 
                                                      0.034002130274329337836748795229551_R64P, & 
                                                      0.037116271483415543560330625367620_R64P, & 
                                                      0.040083825504032382074839284467076_R64P, & 
                                                      0.042872845020170049476895792439495_R64P, & 
                                                      0.045502913049921788909870584752660_R64P, & 
                                                      0.047982537138836713906392255756915_R64P, & 
                                                      0.050277679080715671963325259433440_R64P, & 
                                                      0.052362885806407475864366712137873_R64P, & 
                                                      0.054251129888545490144543370459876_R64P, & 
                                                      0.055950811220412317308240686382747_R64P, & 
                                                      0.057437116361567832853582693939506_R64P, &
                                                      0.058689680022394207961974175856788_R64P, & 
                                                      0.059720340324174059979099291932562_R64P, & 
                                                      0.060539455376045862945360267517565_R64P, & 
                                                      0.061128509717053048305859030416293_R64P, & 
                                                      0.061471189871425316661544131965264_R64P, &
                                                      0.061580818067832935078759824240066_R64P   ]
    ! Start of executable statements
    centr = 0.5_R64P*(a+b)
    hlgth = 0.5_R64P*(a-b)
    dhlgth = DABS(hlgth)
    !       compute the 51-point kronrod approximation to
    !       the integral, and estimate the absolute error.
    fc = f(centr)
    resg = wg(13)*fc
    resk = wgk(26)*fc
    resabs = DABS(resk)
    do j = 1, 12
        jtw = j*2
        absc = hlgth*xgk(jtw)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(DABS(fval1)+DABS(fval2))
    end do
    do j = 1, 13
        jtwm1 = j*2
        absc = hlgth*xgk(jtwm1)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(DABS(fval1)+DABS(fval2))
    end do
    reskh = resk*0.5_R64P
    resasc = wgk(26)*DABS(fc-reskh)
    do j = 1, 25
        resasc = resasc+wgk(j)*(DABS(fv1(j)-reskh)+DABS(fv2(j)-reskh))
    end do
    res = reskh*hlgth
    resabs = resabs*dhlgth
    resasc = resasc*dhlgth
    abserr = DABS((resk-resg)*hlgth)
    if(resasc /= 0._R64P .AND. abserr /= 0._R64P) then
        abserr = resasc*DMIN1(1._R64P,(200._R64P*abserr/resasc)**1.5_R64P)
    end if
    if(resabs > TINY(resabs)/(50._R64P*EPSILON(resabs)) then
        abserr = DMAX1((EPSILON(resabs*50._R64P))*resabs,abserr)
    end if
    end subroutine
    
    subroutine dqk61(f,a,b,res,abserr,resabs,resasc)
!***begin prologue  dqk61
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  61-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***Modified by Bernard Gingold on 18/10/2017    
!***purpose  to compute i = integral of f over (a,b) with error
!                           estimate
!                       j = integral of dabs(f) over (a,b)
!***description
!
!        integration rule
!        standard fortran subroutine
!        double precision version
!
!
!        parameters
!         on entry
!           f      - double precision
!                    function subprogram defining the integrand
!                    function f(x). the actual name for f needs to be
!                    declared e x t e r n a l in the calling program.
!
!           a      - double precision
!                    lower limit of integration
!
!           b      - double precision
!                    upper limit of integration
!
!         on return
!           result - double precision
!                    approximation to the integral i
!                    result is computed by applying the 61-point
!                    kronrod rule (resk) obtained by optimal addition of
!                    abscissae to the 30-point gauss rule (resg).
!
!           abserr - double precision
!                    estimate of the modulus of the absolute error,
!                    which should equal or exceed dabs(i-result)
!
!           resabs - double precision
!                    approximation to the integral j
!
!           resasc - double precision
!                    approximation to the integral of dabs(f-i/(b-a)) 
    implicit none
    interface
        real(R64P) function f(x)
          real(R64P), intent(in) :: x
        end function f
    end interface
    real(R64P), intent(in) :: a,b
    real(R64P), intent(out) :: res,abserr,resabs,resasc
    ! Locals
    real(R64P) :: dabsc,centr,dhlgth,fc,fsum,fval1,fval2, &
                  hlgth,resg,resk,reskh
    integer(I32P) :: j,jwt,jwtm1
    real(R64P), dimension(30) :: fv1,fv2
    real(R64P), dimension(1:15), parameter :: wg  = [ 0.007968192496166605615465883474674_R64P, &
                                                      0.018466468311090959142302131912047_R64P, & 
                                                      0.028784707883323369349719179611292_R64P, & 
                                                      0.038799192569627049596801936446348_R64P, & 
                                                      0.048402672830594052902938140422808_R64P, & 
                                                      0.057493156217619066481721689402056_R64P, & 
                                                      0.065974229882180495128128515115962_R64P, & 
                                                      0.073755974737705206268243850022191_R64P, & 
                                                      0.080755895229420215354694938460530_R64P, & 
                                                      0.086899787201082979802387530715126_R64P, & 
                                                      0.092122522237786128717632707087619_R64P, & 
                                                      0.096368737174644259639468626351810_R64P, & 
                                                      0.099593420586795267062780282103569_R64P, & 
                                                      0.101762389748405504596428952168554_R64P, & 
                                                      0.102852652893558840341285636705415_R64P   ]
    real(R64P), dimension(1:30), parameter :: xgk = [ 0.999484410050490637571325895705811_R64P, & 
                                                      0.996893484074649540271630050918695_R64P, &
                                                      0.991630996870404594858628366109486_R64P, & 
                                                      0.983668123279747209970032581605663_R64P, & 
                                                      0.973116322501126268374693868423707_R64P, & 
                                                      0.960021864968307512216871025581798_R64P, & 
                                                      0.944374444748559979415831324037439_R64P, & 
                                                      0.926200047429274325879324277080474_R64P, & 
                                                      0.905573307699907798546522558925958_R64P, & 
                                                      0.882560535792052681543116462530226_R64P, & 
                                                      0.857205233546061098958658510658944_R64P, & 
                                                      0.829565762382768397442898119732502_R64P, & 
                                                      0.799727835821839083013668942322683_R64P, & 
                                                      0.767777432104826194917977340974503_R64P, & 
                                                      0.733790062453226804726171131369528_R64P, & 
                                                      0.697850494793315796932292388026640_R64P, & 
                                                      0.660061064126626961370053668149271_R64P, & 
                                                      0.620526182989242861140477556431189_R64P, & 
                                                      0.579345235826361691756024932172540_R64P, & 
                                                      0.536624148142019899264169793311073_R64P, & 
                                                      0.492480467861778574993693061207709_R64P, & 
                                                      0.447033769538089176780609900322854_R64P, & 
                                                      0.400401254830394392535476211542661_R64P, & 
                                                      0.352704725530878113471037207089374_R64P, & 
                                                      0.304073202273625077372677107199257_R64P, & 
                                                      0.254636926167889846439805129817805_R64P, & 
                                                      0.204525116682309891438957671002025_R64P, & 
                                                      0.153869913608583546963794672743256_R64P, & 
                                                      0.102806937966737030147096751318001_R64P, & 
                                                      0.051471842555317695833025213166723_R64P, & 
                                                      0.000000000000000000000000000000000_R64P  ]
    real(R64P), dimension(1:31), parameter :: wgk = [ 0.001389013698677007624551591226760_R64P, & 
                                                      0.003890461127099884051267201844516_R64P, & 
                                                      0.006630703915931292173319826369750_R64P, & 
                                                      0.009273279659517763428441146892024_R64P, & 
                                                      0.011823015253496341742232898853251_R64P, & 
                                                      0.014369729507045804812451432443580_R64P, & 
                                                      0.016920889189053272627572289420322_R64P, & 
                                                      0.019414141193942381173408951050128_R64P, & 
                                                      0.021828035821609192297167485738339_R64P, & 
                                                      0.024191162078080601365686370725232_R64P, & 
                                                      0.026509954882333101610601709335075_R64P, & 
                                                      0.028754048765041292843978785354334_R64P, &
                                                      0.030907257562387762472884252943092_R64P, & 
                                                      0.032981447057483726031814191016854_R64P, & 
                                                      0.034979338028060024137499670731468_R64P, &
                                                      0.036882364651821229223911065617136_R64P, & 
                                                      0.038678945624727592950348651532281_R64P, & 
                                                      0.040374538951535959111995279752468_R64P, & 
                                                      0.041969810215164246147147541285970_R64P, & 
                                                      0.043452539701356069316831728117073_R64P, & 
                                                      0.044814800133162663192355551616723_R64P, & 
                                                      0.046059238271006988116271735559374_R64P, & 
                                                      0.047185546569299153945261478181099_R64P, & 
                                                      0.048185861757087129140779492298305_R64P, & 
                                                      0.049055434555029778887528165367238_R64P, & 
                                                      0.049795683427074206357811569379942_R64P, & 
                                                      0.050405921402782346840893085653585_R64P, & 
                                                      0.050881795898749606492297473049805_R64P, & 
                                                      0.051221547849258772170656282604944_R64P, & 
                                                      0.051426128537459025933862879215781_R64P, & 
                                                      0.051494729429451567558340433647099_R64P   ]
    ! Start of executable statements
    centr = 0.5_R64P*(a+b)
    hlgth = 0.5_R64P*(a-b)
    dhlgth = DABS(hlgth)
    !  compute the 61-point kronrod approximation to the
    !  integral, and estimate the absolute error.
    resg = 0._R64P
    fc = f(centr)
    resk = wgk(31)*fc
    resabs = DABS(resk)
    do j = 1, 15
        jtw = j*2
        dabsc = hlgth*xgk(jtw)
        fval1 = f(centr-dabsc)
        fval2 = f(centr+dabsc)
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(DABS(fval1)+DABS(fval2))
    end do
    do j = 1, 15
          jtwm1 = j*2-1
        dabsc = hlgth*xgk(jtwm1)
        fval1 = f(centr-dabsc)
        fval2 = f(centr+dabsc)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(DABS(fval1)+DABS(fval2))
    end do
        reskh = resk*0.5_R64P
        resasc = wgk(31)*DABS(fc-reskh)
    do j = 1, 20
            resasc = resasc+wgk(j)*(DABS(fv1(j)-reskh)+DABS(fv2(j)-reskh))
    end do
        res = resk*hlgth
        resabs = resabs*dhlgth
        resasc = resasc*dhlgth
        abserr = DABS((resk-resg)*hlgth)
    if(resasc /= 0._R64P .AND. abserr /= 0._R64P) then
        abserr = resasc*DMIN1(1._R64P,(200._R64P*abserr/resasc)**1.5_R64P)
    end if
    if(resabs > TINY(resabs)/(50._R64P*EPSILON(resabs)) then
        abserr = DMAX1((EPSILON(resabs*50._R64P))*resabs,abserr)
    end if   
    end subroutine
    
                     
end module mod_quadpack