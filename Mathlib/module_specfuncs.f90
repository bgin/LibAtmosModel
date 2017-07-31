
module mod_specfuncs

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_specfuncs'
 !          
 !          Purpose:
 !                      Modern reimplementation of Special Functions
 !                      library written by  Shanjie Zhang, Jianming Jin
 !                     
 !          History:
 !                        Date: 20-07-2017
 !                        Time: 14:33 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Shanjie Zhang, Jianming Jin  http://in.ece.illinois.edu/routines/routines.html
 !                 
 !          Modification by:
 !                            Bernard Gingold 
 !          Changes or enhancement made:
 !
 !          1) Packaging subroutines into module.
 !          2) Adding code to inspect state of Floating-Point environment
 !          3) 
 !          4)
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
    use module_class_error_check, only : array1D_not_alloc, &
                                         array2D_not_alloc 
   
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_SPECFUNCS_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_SPECFUNCS_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_SPECFUNCS_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_SPECFUNCS_FULLVER = 1000*MOD_SPECFUNCS_MAJOR + 100*MOD_SPECFUNCS_MINOR + &
                                                                10*MOD_SPECFUNCS_MICRO
    
    ! Creation date
    character(*), parameter, public :: MOD_SPECFUNCS_CREATION_DATE = "20-07-2017 15:00 PM GMT+2 (THU 20 JULY 2017 15:00 -00200)"
    
    ! Build date should be modified and set to latest successful build date/time
    character(*), parameter, public :: MOD_SPECFUNCS_BUILD_DATE = " "
    
    ! Module author
    character(*), parameter, public :: MOD_SPECFUNCS_AUTHORS = "Original version: Shanjie Zhang, Jianming Jin, Modified: Bernard Gingold"
    
    ! Module description
    character(*), parameter, public :: MOD_SPECFUNCS_DESCRIPT = "Library of Special Functions"
    
    ! Module constants
    real(R64P), parameter, private :: CPI = 3.1415926535897932384626433832795_R64P
    
    real(R64P), parameter, private :: PINV = 0.31830988618379067153776752674503_R64P
    
    real(R64P), parameter, private :: ZR64P = 0._R64P
    
    real(R64P), parameter, private :: AIRYC1 = 0.355028053887817_R64P
    
    real(R64P), parameter, private :: AIRYC2 = 0.258819403792807_R64P
    
    real(R64P), parameter, private :: AIRYSR3 =  1.732050807568877_R64P
    
    real(R64P), parameter, private :: CTHIRD = 0.33333333333333333333333333333333_R64P 
    
    real(R64P), parameter, private :: CSMALL = 1.0D-100
    
    contains
    
    !==============================================================================84
    !  Subroutine: AIRYA
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state
    !==============================================================================84
    subroutine airya(x,ai,bi,ad,bd,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P),                  intent(in)    :: x
          real(R64P),                  intent(inout) :: ai,bi,ad,bd
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)  :: c1,c2,pir,sr3,vi1,vi2, &
                         vj1,vj2,vk1,vk2,vy1,   &
                         vy2,xa,xq,z
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity checking of input arguments
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          xa = DABS(x)
          pir = PINV
          c1 =  AIRYC1
          c2 =  AIRYC2
          sr3 = AIRYSR3
          z = xa**1.5_R64P/1.5_R64P
          xq = DSQRT(xa)
          call ajyik(z,vj1,vj2,vy1,vy2,vi1,vi2,vk1,vk2,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF
          if(x.EQ.ZR64P) then
              a1 = c1
              bi = sr3*c1
              ad = -c2
              bd = sr3*c2
          else if(ZR64P.LT.x) then
              ai = pir*xq/sr3*vk1
              bi = xq*(pir*vk1+2._R64P/sr3*vi1)
              ad = -xa/sr3*pir*vk2
              bd = xa*(pir*vk2+2._R64P/sr3*vi2)
          else
              a1 = 0.5_R64P*xq*(vj1-vy1/sr3)
              bi = -0.5_R64P*xq*(vj1/sr3+vy1)
              ad = 0.5_R64P*xa*(vj2+vy2/sr3)
              bd = 0.5_R64P*xa*(vj2/sr3-vy2)
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                 
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " airya: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF
    end subroutine
    
    !==============================================================================84
    !  Subroutine: AIRYB
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state
    !==============================================================================84
    subroutine airyb(x,ai,bi,ad,bd,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P),  intent(in)                    :: x
          real(R64P),  intent(inout)                 :: ai,bi,ad,bd
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P) :: c1,c2,df,dg,eps,fx,gx,pi,r,rp,   &
                        sad,sai,sbd,sbi,sda,sdb,sr3,ssa, &
                        xa,xar,xcs,xe,xf,xm,xp1,xq,xr1,  &
                        xr2,xss
          real(R64P), dimension(41) :: ck,dk
          integer(I32P) :: k,km
          integer(I32P), parameter :: kmax = 40
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of input fp_flags
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
         
          eps = MACHEPSF64
          pi = CPI
          c1 = AIRYC1
          c2 = AIRYC2
          sr3 = AIRYSR3
          xa = DABS(x)
          xq = DSQRT(xa)
          if(x.LE.0._R64P) then
              xm = 8._R64P
          else
              xm = 5._R64P
          end if
          if(x.EQ.0._R64P) then
              a1 = c1
              bi = sr3*c1
              ad = -c2
              bd = sr3*c2
              return
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
          if(x.LE.xm) then
              fx = 1._R64P
              r = 1._R64P
              do k = 1, kmax
                  r = r*x/(3._R64P*k)*x/(3._R64P*k-1._R64P)*x
                  fx = fx+r
                  if(DABS(r).LT.DABS(fx)*eps) exit
              end do
              gx = x
              r = x
              do k = 1, kmax
                  r = r*x/(3._R64P*k)*x/(3._R64P*k+1._R64P)*x
                  gx = gx+r
                  if(DABS(r).LT.DABS(gx)*eps) exit
              end do
              ai = c1*fx-c2*gx
              bi = sr3*(c1*fx+c2*gx)
              df = 0._R64P*x*x
              do k = 1, kmax
                  r = r*x/(3._R64P*k)*x/(3._R64P*k+2._R64P)*x
                  df = df+r
                  if(DABS(r).LT.DABS(df)*eps) exit
              end do
              dg = 1._R64P
              r = 1._R64P
              do k = 1, kmax
                  r = r*x/(3._R64P*k)*x/(3._R64P*k-2._R64P)*x
                  dg = dg+r
                  if(DABS(r).LT.DABS(dg)*eps) exit
              end do
              ad = c1*df-c2*dg
              bd = sr3*(c1*df+c2*dg)
          else
              xe = xa*xq/1.5_R64P
              xr1 = 1._R64P/xe
              xar = 1._R64P/xq
              xf = DSQRT(xar)
              rp = 0.5641895835477563_R64P
              r = 1._R64P
              do k = 1, kmax
                  r = r*(6._R64P*k-1._R64P) &
                      / 216._R64P*(6._R64P*k-3._R64P) &
                      / k*(6._R64P*k-5._R64P)/(2._R64P*k-1._R64P)
                  ck(k) = r
                  dk(k) = -(6._R64P*k+1._R64P)/(6._R64P*k-1._R64P)*ck(k)
              end do
              km = DINT(24.5_R64P-xa)
              if(xa.LT.6._R64P) then
                  km = 14
              end if
              if(15._R64P.LT.xa) then
                  km = 10
              end if
              if(0._R64P < x) then
                  sai = 1._R64P
                  sad = 1._R64P
                  r = 1._R64P
                  do k = 1, km
                      r = -r*xr1
                      sai = sai+ck(k)*r
                      sad = sad+dk(k)*r
                  end do
                  sbi = 1._R64P
                  sbd = 1._R64P
                  r = 1._R64P
                  do k = 1, km
                      r = r*xr1
                      sbi = sbi+ck(k)*r
                      sbd = sbd+dk(k)*r
                  end do
                  xp1 = DEXP(-xe)
                  ai = 0.5_R64P*rp*xf*xp1*sai
                  bi = rp*xf/xp1*sbi
                  ad = -0.5_R64P*rp/xf*xp1*sad
                  bd = rp/xf/xp1*sbd
              else
                  xcs = DCOS(xe+pi/4._R64P)
                  xss = DSIN(xe+pi/4._R64P)
                  ssa = 1._R64P
                  sda = 1._R64P
                  r = 1._R64P
                  xr2 = 1._R64P/(xe*xe)
                  do k = 1,km
                      r = -r*xr2
                      ssa = ssa+ck(2*k)*r
                      sda = sda+dk(2*k)*r
                  end do
                  ssb = ck(1)*xr1
                  sdb = dk(1)*xr1
                  r = xr1
                  do k = 1, km
                      r = -r*xr2
                      ssb = ssb+ck(2*k+1)*r
                      sdb = sdb+dk(2*k+1)*r
                  end do
                  ai = rp*xf*(xss*ssa-xcs*ssb)
                  bi = rp*xf*(xcs*ssa+xss*ssb)
                  ad = -rp/xf*(xcs*sda+xss*sdb)
                  bd = rp/xf*(xss*sda-xcs*sdb)
              end if
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                 
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " airyb: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF          
    end subroutine
    
    !==============================================================================84
    !  Subroutine: AIRYZO
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state
    !==============================================================================84
    subroutine airyzo(nt,kf,xa,xb,xc,xd,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          integer(I32P), intent(in)                  :: nt
          integer(I32P), intent(in)                  :: kf
          real(R64P), dimension(nt),   intent(out)   :: xa,xb, &
                                                        xc,xd
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)    :: ad,ai,bd,bi,pi,rt,rt0, &
                           u,u1,x
          integer(I32P) :: i
          real(R64P), parameter :: c1 = -15.5902_R64P
          real(R64P), parameter :: c2 =  0.929844_R64P
          real(R64P), parameter :: c3 =  0.138889_R64P
          real(R64P), parameter :: c4 =  0.10416667_R64P
          real(R64P), parameter :: c5 =  0.125_R64P
          real(R64P), parameter :: c6 =  0.000000001_R64P
          real(R64P), parameter :: c7 =  15.0168_R64P
          real(R64P), parameter :: c8 =  0.873954_R64P
          real(R64P), parameter :: c9 =  0.121528_R64P
          real(R64P), parameter :: c10 = 0.145833_R64P
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity checking of inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          pi = CPI
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
          do i = 1, nt
               if(kf.EQ.1) then
                   u = 3._R64P*pi*(4._R64P*i-1)*c5
                   u1 = 1._R64P/(u*u)
                   rt0 = -(u*u)**(CTHIRD) &
                    *((((-c1*u1+c2)*u1 &
                       -c3)*u1+c4)*u1+1._R64P)
               else if(kf.EQ.2) then
                    if(i.EQ.1) then
                      rt0 = -1.17371_R64P
                   else
                      u = 3._R64P*pi*(4._R64P*i-3._R64P)*c5
                      u1 = 1._R64P/(u*u)
                      rt0 = -(u*u)**(CTHIRD) &
                         *((((c1*u1+c2)*u1 &
                          -c3)*u1+c4)*u1+1._R64P)
                  end if
               end if
               do
                   x = rt0
                   call airyb(x,ai,bi,ad,bd,fp_flags)
                   if(kf.EQ.1) then
                       rt = rt0-ai/ad
                   else
                       rt = rt0-bi/bd
                   end if
                   if(DABS((rt-rt0)/rt).LE.c6) exit
                   rt0 = rt
               end do
               xa(i) = rt
               if(kf.EQ.1) then
                   xd(i) = ad
               else
                   xd(i) = bd
               end if
          end do
          do i = 1, nt
              if(kf.EQ.1) then
                  if(i.EQ.1) then
                      rt0 = -1.01879_R64P
                  else
                      u = 3._R64P*pi*(4._R64P*i-3._R64P)*c5
                      u1 = 1._R64P/(u*u)
                      rt0 = -(u*u)**(CTHIRD) &
                          *((((c7*u1-c8)*u1+c9) &
                          *u1-c10)*u1+1._R64P)
                  end if
              else if(kf.EQ.2) then
                  if(i.EQ.1) then
                      rt0 = -2.29444_R64P
                  else
                     u = 3._R64P*pi*(4._R64P*i-1._R64P)*c5
                     u1 = 1._R64P/(u*u)
                      rt0 = -(u*u)**(CTHIRD) &
                          *((((c7*u1-c8)*u1+c9) &
                          *u1-c10)*u1+1._R64P)
                  end if
              end if
              do
                 x = rt0
                 call airyb(x,ai,bi,ad,bd)
                 if(kf.EQ.1) then
                     rt = rt0-ad/(ai*x)
                 else
                     rt = rt0-bd/(bi*x)
                 end if
                 if(DABS((rt-rt0)/rt).LE.c6) exit
                 rt0 = rt
              end do
              xb(i) = rt
              if(kf.EQ.1) then
                  xc(i) = ai
              else
                  xc(i) = bi
              end if
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " airyzo: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF          
    end subroutine
    
    !==============================================================================84
    !  Subroutine: AJYIK
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state
    !==============================================================================84
    subroutine ajyik(x,vj1,vj2,vy1,vy2,vi1, &
                     vi2,vk1,vk2,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P), intent(in)                     :: x
          real(R64P),                  intent(inout) :: vj1,vj2, &
                                                        vy1,vy2, &
                                                        vi1,vi2, &
                                                        vk1,vk2
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P) :: a0,b0,c0,ck,gn,gn1,gn2,gp1, &
                        gp2,pi,pv1,pv2,px,qx,r,rp,  &
                        rp2,rq,sk,sum,uj1.uj2,uu0,  &
                        vl,vsl,vv,vv0,vjl,x,x2,xk,  &
                        tk
          integer(I32P) :: k,k0,l
          real(R64P), parameter :: c35 = 35._R64P, &
                                   c12 = 12._R64P, &
                                   c50 = 50._R64P, &
                                   c18 = 18._R64P, &
                                   c9  = 9._R64P
          integer(I32P), parameter :: kmax = 40
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start  of executable statements
          ! Sanity check of inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          if(x.EQ.0._R64P) then
             vj1 = 0._R64P
             vj2 = 0._R64P
             vy1 = -HUGE(1._R64P)
             vy2 =  HUGE(1._R64P)
             vi1 = 0._R64P
             vi2 = 0._R64P
             vk1 = -HUGE(1._R64P)
             vk2 = -HUGE(1._R64P)
             return
          end if
          pi = CPI
          rp2 = 0.63661977236758_R64P
          gp1 = 0.892979511569249_R64P
          gp2 = 0.902745292950934_R64P
          gn1 = 1.3541179394264_R64P
          gn2 = 2.678938534707747_R64P
          vv0 = 0.444444444444444_R64P
          uu0 = 1.1547005383793_R64P
          x2 = x*x
          if(x.LT.c35) then
              k0 = 12
          else if(x.LT.c50) then
              k0 = 10
          else
              k0 = 8
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
          if(x.LE.c12) then
              
              do l = 1, 2
                  v1 = CTHIRD
                  vj1 = 1._R64P
                  r = 1._R64P
                  do k = 1, kmax
                      r = -0.25_R64P*r*x2/(k*(k+vl))
                      vjl = vjl+r
                      if(DABS(r).LT.MACHEPSF64) exit
                  end do
                  a0 = (0.5_R64P*x)**vl
                  if(l.EQ.1) then
                      vj1 = a0/gp1*vjl
                  else
                      vj2 = a0/gp2*vj1
                  end if
              end do
          else
              do l = 1, 2
                  vv = vv0*l*l
                  px = 1._R64P
                  rp = 1._R64P
                  do k = 1, k0
                      tk = DBLE(k)
                      rp = -0.78125D-02_R64P*rp &
                         *(vv-(4._R64P*tk-3._R64P)**2) &
                         *(vv-(4._R64P*tk-1._R64P)**2) &
                          / (k*(2._R64P*tk-1._R64P)*x2)
                      px = px+rp
                  end do
                  qx = 1._R64P
                  rq = 1._R64P
                  do k = 1, k0
                      tk = DBLE(k)
                      rq = -0.78125D-02_R64P*rq &
                          *(vv-(4._R64P*tk-1._R64P)**2) &
                          *(vv-(4._R64P*tk+1._R64P)**2) &
                          / (k*(2._R64P*tk+1._R64P)*x2)
                      qx = qx+rq
                  end do
                  qx = 0.125_R64P*(vv-1._R64P)*qx/x
                  xk = x-(0.5_R64P*l/3._R64P+0.25_R64P)*pi
                  a0 = DSQRT(rp2/x)
                  ck = DCOS(xk)
                  sk = DSIN(xk)
                  if(l.EQ.1) then
                      vjl = a0*(px*ck-qx*sk)
                      vy1 = a0*(px*sk+qx*ck)
                  else
                      vj2 = a0*(px*ck-qx*sk)
                      vy2 = a0*(px*sk+qx*ck)
                  end if
              end do
          end if
        if(x.LE.c12) then
            do l = 1, 2
                v1 = CTHIRD
                vjl = 1._R64P
                r = 1._R64P
                do k = 1, kmax
                    tk = DBLE(k)
                    r = -0.25_R64P*r*x2/(tk*(tk-vl))
                    vjl = vjl + r
                    if(DABS(r).LT.MACHEPSF64) exit
                end do
                b0 = (2._R64P/x)**vl
                if(l.EQ.1) then
                    uj1 = b0*vjl/gn1
                else
                    uj2 = b0*vjl/gn2
                end if
            end do
            pv1 = pi/3._R64P
            pv2 = pi/1.5_R64P
            vy1 = uu0*(vj1*DCOS(pv1)-uj1)
            vy2 = uu0*(vj2*DCOS(pv2)-uj2)
        end if
        if(x.LE.c18) then
            do l = 1, 2
                v1 = l/3._R64P
                vil = 1._R64P
                r = 1._R64P
                do k = 1, kmax
                    tk = DBLE(k)
                    r = 0.25_R64P*r*x2/(tk*(tk+v1))
                    vil = vil+r
                    if(DABS(r).LT.MACHEPSF64) exit
                end do
                a0 = (0.5_R64P*x)**vl
                if(l.EQ.1) then
                    vi1 = a0/gp1*vil
                else
                    vi2 = a0/gp2*vil
                end if
            end do
        else
            c0 = DEXP(x)/DSQRT(2._R64P*pi*x)
            do l = 1, 2
                vv = vv0*l*l
                vsl = 1._R64P
                r = 1._R64P
                do k = 1, k0
                    tk = DBLE(k)
                    r = -0.125_R64P*r &
                        *(vv-(2._R64P*k-1._R64P)**2)/(tk*x)
                    vsl = vsl+r
                end do
                if(l.EQ.1) then
                    vi1 = c0*vsl
                else
                    vi2 = c0*vsl
                end if
            end do
        end if
        if(x.LE.c9) then
            do l = 1, 2
                vl = l/3._R64P
                if(l.EQ.1) then
                    gn = gn1
                else
                    gn = gn2
                end if
                a0 = (2._R64P/x)**v1/gn
                sum = 1._R64P
                r = 1._R64P
                do k = 1, 60
                    tk = DBLE(k)
                    r = 0.25_R64P*r*x2/(tk*(tk-vl))
                    sum = sum+r
                    if(DABS(r).LT.MACHEPSF64) exit
                end do
                if(l.EQ.1) then
                   vk1 = 0.5_R64P*uu0*pi*(sum*a0-vi1)
                else
                   vk2 = 0.5_R64P*uu0*pi*(sum*a0-vi2)
                end if
            end do
        else
            c0 = DEXP(-x)*DSQRT(0.5_R64P*pi/x)
            do l = 1, 2
                vv = vv0*l*l
                sum = 1._R64P
                r = 1._R64P
                do k = 1, k0
                    tk = DBLE(k)
                    r = 0.25_R64P*r*(vv-(2._R64P*tk-1._R64P)**2)/(k*x)
                    sum = sum+r
                end do
                if(l.EQ.1) then
                    vk1 = c0*sum
                else
                    vk2 = c0*sum
                end if
            end do
        end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " ajyik: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF        
                
    end subroutine
    !==============================================================================84
    !  Subroutine: ASWFA
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state                
    !==============================================================================84                 
    subroutine aswfa(m,n,c,x,kd,cv,s1f,s1d,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          integer(I32P),               intent(in)    :: m,n
          real(R64P),                  intent(in)    :: c,x,cv
          real(R64P),                  intent(inout) :: s1f,s1d
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P) :: a0,d0,d1,eps,r,su1,su2,x0,x1,tk
          real(R64P), dimension(200) :: ck,df
          integer(I32P) :: ip,k,kd,m,n,nm,nm2
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity checking of inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          eps = MACHEPSF64
          x0 = x
          x = DABS(x)
          if(n-m.EQ.2*(n-m)/2) then
             ip = 0
          else
              ip = 1
          end if
          nm = 10+DINT((n-m)/2+c)
          nm2 = nm/2-2
          call sdmn(m,n,c,cv,kd,df,fp_flags)
          call sckb(m,n,c,df,ck,fp_flags) 
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          if(m.EQ.0 .AND. x1.EQ.0._R64P) then
              a0 = 1._R64P
          else
              a0 = x1**(0.5_R64P*m)
          end if
          su1 = ck(1)
          do k = 1, nm2
              r = ck(k+1)*x1**k
              su1 = su1+r
              if(10.LE.k .AND. DABS(r/su1).LT.eps) exit
          end do
          s1f = a0*x**ip*su1
          if(x.EQ.1._R64P) then
              if(m.EQ.0) then
                 s1d = ip*ck(1)-2._R64P*ck(2)
              else if(m.EQ.1) then
                  s1d = -1.0D+100
              else if(m.EQ.2) then
                  s1d = -2._R64P*ck(1)
              else if(3.LE.m) then
                  s1d = 0._R64P
              end if
          else
              d0 = ip-m/x1*x**(ip+1._R64P)
              d1 = -2._R64P*a0*x**(ip+1._R64P)
              su2 = ck(2)
              do k = 2, nm2
                 tk = DBLE(k)
                 r = tk*ck(k+1)*x1**(k-1._R64P)
                 su2 = su2+r
                 if(10.LE.k .AND. DABS(r/su2).LT.eps) exit
              end do
              s1d = d0*a0*su1+d1*su2
          end if
        if(x0.LT.0._R64P) then
            if(ip.EQ.0) then
                s1d = -s1d
            else if(ip.EQ.1) then
                s1f = -s1f
            end if
        end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " aswfa: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF         
        x = x0
    end subroutine
    
    !==============================================================================84
    !  Subroutine: ASWFB
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state 
    !==============================================================================84
    subroutine aswfb(m,n,c,x,kd,cv,s1f,s1d,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT 
          implicit none
          integer(I32P),               intent(in)    :: m,n
          real(R64P),                  intent(in)    :: c,x
          integer(I32P),               intent(in)    :: kd
          real(R64P),                  intent(in)    :: cv
          real(R64P),                  intent(inout) :: s1f,s1d
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P) :: eps,su1,sw
          real(R64P), dimension(200)   :: df
          real(R64P), dimension(0:251) :: pd,pm
          integer(I32P) :: ip,k,kd,m,mk,n,nm,nm2
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statemetns
          ! Sanity check of inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          eps = 0.00000000000001_R64P
          if(n-m.EQ.2*(n-m)/2) then
              ip = 0
          else
              ip = 1
          end if
          nm = 25+IDINT((n-m)/2+c)
          nm = 2*nm+m
          call sdmn(m,n,c,cv,kd,df,fp_flags)
          call lpmns(m,nm2,x,pm,pd,fp_flags)
          su1 = 0._R64P
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          do k = 1, nm
              mk = m+2*(k-1)+ip
              su1 = su1+df(k)*pm(k)
              if(DABS(sw-su1).LT.DABS(su1)*eps) exit
              sw = su1
          end do
          s1f = (-1._R64P)**m*su1
          su1 = 0._R64P
          do k = 1, nm
              mk = m+2*(k-1)+ip
              su1 = su1+df(k)*pd(mk)
              if(DABS(sw-su1).LT.DABS(su1)*eps) exit
              sw = su1
          end do
          s1d = (-1._R64P)**m*su1
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " aswfb: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
    
    !==============================================================================84
    !  Subroutine: BETA
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state
    !==============================================================================84
    subroutine beta(p,q,bt,fp_flags)

          implicit none
          real(R64P),                  intent(in)    :: p,q
          real(R64P),                  intent(inout) :: bt
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P) :: gp,gpq,gq,p,ppq,q

          ! Start of executable statements
          ! Sanity check of inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          call sgamma(p,gp,fp_flags)
          call sgamma(q,gq,fp_flags)
          ppq = p+q
          call sgamma(ppq,gpq,fp_flags)
          bt = gp*gq/gpq
    end subroutine
    
    !==============================================================================84
    !  Subroutine: SGAMMA
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state 
    !  4) Reanming subroutine gamma in order to prevent name clash with builtin
    !     intrinsic gamma
    !==============================================================================84
    subroutine sgamma(x,ga,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          real(R64P),                  intent(in)    :: x
          real(R64P),                  intent(inout) :: ga
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P) :: gr,r,z,tk
          integer(I32P) :: k,m,m1
          real(R64P), dimension(26), parameter :: g = [1._R64P, 0.5772156649015329_R64P, &
                                                       -0.6558780715202538_R64P,         &
                                                       -0.420026350340952_R64P,          & 
                                                        0.1665386113822915_R64P,         &
                                                       -0.421977345555443D-01,           &
                                                       -0.96219715278770D-02,            &
                                                        0.72189432466630D-02,            &
                                                       -0.11651675918591D-02,            &
                                                       -0.2152416741149D-03,             &
                                                        0.1280502823882D-03,             & 
                                                       -0.201348547807D-04,              &
                                                       -0.12504934821D-05,               &
                                                        0.11330272320D-05,               &
                                                       -0.2056338417D-06,                & 
                                                        0.61160950D-08,                  &
                                                        0.50020075D-08,                  &
                                                       -0.11812746D-08,                  &
                                                        0.1043427D-09,                   & 
                                                        0.77823D-11,                     &
                                                       -0.36968D-11,                     &
                                                        0.51D-12,                        &
                                                       -0.206D-13,                       &
                                                       -0.54D-14,                        &
                                                        0.14D-14,                        &
                                                        0.1D-15                           ]
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF   
          ! Start of executable statements
          ! Sanity check of inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          if(x.EQ.DINT(x)) then
               if(0._R64P.LT.x) then
                   ga = 1._R64P
                   m1 = IDINT(x)-1
                   do k = 2, m1
                       tk = DBLE(k)
                       ga = ga*k
                   end do
               else
                   ga = HUGE(1._R64P)
               end if
          else
              if(1._R64P.LT.DABS(x)) then
                  z = DABS(x)
                  m = IDINT(z)
                  do k = 1, m
                      tk = DBLE(k)
                      r = r*(z-tk)
                  end do
                  z = z-DBLE(m)
              else
                  z = x
              end if
              gr = g(26)
              do k = 25, 1, -1
                  gr = gr*z+g(k)
              end do
              ga = 1._R64P/(gr*z)
              if(1._R64P.LT.DABS(x)) then
                  ga = ga*r
                  if(x.LT.0._R64P) then
                      ga = -CPI/(x*ga*DSIN(CPI*x))
                  end if
              end if
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " sgamma: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine  
    
    !==============================================================================84
    !  Subroutine: CIKNB  (complex modified Bessel functions In(z) and Kn(z))
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state 
    !==============================================================================84
    subroutine ciknb(n,z,nm,cbi,cdi,cbk,cdk,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          integer(I32P),                 intent(in)    :: n
          complex(R64P),                 intent(in)    :: z
          integer(I32P),                 intent(inout) :: nm
          complex(R64P), dimension(0:n), intent(inout) :: cbi,cdi,cbk,cdk
          logical(I32P), dimension(5),   intent(inout) :: fp_flags
          ! Locals
          complex(R64P) :: ca0,cbkl,cbs,cf,cf0,cf1,cg,cg0,cg1, &
                           ci,cr,cs0,csk0,z,z1
          real(R64P)    :: fac,vt,a0,tk
          real(R64P), parameter :: el =  0.57721566490153_R64P
          integer(I32P) :: msta1,msta2
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statememts
          ! Sanity check of inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          a0 = DABS(z)
          nm = n
          if(a0.LT.CSMALL) then
              do k = 0, n
                  cbi(k) = dcmplx(0._R64P,0._R64P)
                  cbk(k) = dcmplx(1.0E+30_R64P,0._R64P)
                  cdi(k) = dcmplx(0._R64P,0._R64P)
                  cdk(k) = -dcmplx(1.0E+30_R64P,0._R64P)
              end do
              cbi(0) = dcmplx(1._R64P,0._R64P)
              cdi(1) = dcmplx(0.5_R64P,0._R64P)
              return
          end if
          ci = dcmplx(0._R64P,1._R64P)
          if(real(z,kind=8).LT.0._R64P) then
              z1 = -z
          else
              z1 = z
          end if
          if(n.EQ.0) then
              nm = 1
          end if
          m = msta1(a0,200)
          if(n.LT.nm) then
              nm = m
          else
              m = msta2(a0,nm,15)
          end if
          cbs = 0._R64P
          csk0 = 0._R64P
          cf0 = 0._R64P
          cf1 = 1._R64P
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
          do k = m, 0, -1
              cf = 2._R64P*(k+1.R64P)*cf1/z1+cf0
              if(k.LE.nm) then
                  cbi(k) = cf
              end if
              if(k.NE.0 .AND. k.EQ.2*(k/2)) then
                  csk0 = csk0+4._R64P*cf/DBLE(k)
              end if
              cbs = cbs+2._R64P*cf
              cf0 = cf1
              cf1 = cf
          end do
          cs0 = DEXP(z1)/(cbs-cf)
          do k = 0, m
              cbi(k) = cs0*cbi(k)
          end do
          if(a0.LE.9._R64P) then
              cbk(0) = -(DLOG(0.5_R64P*zi)+el)*cbi(0)+cs0*csk0
              cbk(1) = (1._R64P/z1-cbi(1)*cbk(0))/cbi(0)
          else
              ca0 = DSQRT(CPI/(2._R64P*z1))*DEXP(z1)
              if(a0.LT.25._R64P) then
                  k0 = 16
              else if(a0.LT.80._R64P) then
                  k0 = 10
              else if(a0.LT.200._R64P) then
                  k0 = 8
              else
                  k0 = 6
              end if
              do l = 0, 1
                  cbkl = 1._R64P
                  vt = 4._R64P*l
                  cr = dcmplx(1._R64P,0._R64P)
                  do k = 1, k0
                      tk = DBLE(k)
                      cr = 0.125_R64P*cr &
                          *(vt-(2._R64P*tk-1._R64P)**2)/(tk*z1)
                      cbkl = cbkl+cr
                  end do
                  cbk(l) = ca0*cbkl
              end do
          end if
          cg0 = cbk0(0)
          cg1 = cbk(1)
          do k = 2, nm
              tk = DBLE(k)
              cg = 2._R64P*(tk-1._R64P)/z1*cg1+cg0
              cbk(k) = cg
              cg0 = cg1
              cg1 = cg
          end do
          if(real(z,kind=8).LT.0._R64P) then
               fac = 1._R64P
               do k = 0, nm
                   if(dimag(z).LT.0._R64P) then
                       cbk(k) = fac*cbk(k)+ci*CPI*cbi(k)
                   else
                       cbk(k) = fac*cbk(k)-ci*CPI*cbi(k)
                   end if
                   cbi(k) = fac*cbi(k)
                   fac = -fac
               end do
          end if
          cdi(0) = cbi(1)
          cdk(0) = -cbk(1)
          do k = 1, nm
              tk = DBLE(k)
              cdi(k) = cbi(k-1)-tk/z*cbi(k)
              cdk(k) = -cbi(k-1)-tk/z*cbk(k)
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " ciknb: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF          
    end subroutine
    
    !==============================================================================84
    !  Subroutine: RCTJ (Riccati-Bessel function of the first kind, and derivatives)
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state
    !==============================================================================84
    subroutine rctj(n,x,nm,rj,dj,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          integer(I32P),               intent(in)    :: n
          real(R64P),                  intent(in)    :: x
          integer(I32P),               intent(inout) :: nm
          real(R64P), dimension(0:n),  intent(inout) :: rj,dj
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)    :: cs,f,f0,f1,rj0,rj1,tk
          integer(I32P) :: k,m
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          nm = n
          if(DABS(x).LT.CSMALL) then
              do k = 0, n
                  rj(k) = 0._R64P
                  dj(k) = 0._R64P
              end do
              dj(0) = 1._R64P
              return
          end if
          rj(0) = DSIN(x)
          rj(1) = rj(0)/x-DCOS(x)
          rj0 = rj(0)
          rj1 = rj(1)
          if(2.LE.n) then
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF              
              m = msta1(x,200)
              if(m.LT.n) then
                  nm = m
              else
                  m = msta2(x,n,15)
              end if
              f0 = 0._R64P
              f1 = CSMALL
              do k = m, 0, -1
                  tk = DBLE(k)
                  f = (2._R64P*tk+3._R64P)*f1/x-f0
                  if(k.LE.nm) then
                      rj(k) = f
                  end if
                  f0 = f1
                  f1 = f
              end do
              if(DABS(rj1).LT.DABS(rj0)) then
                  cs = rj0/f
              else
                  cs = rj1/f0
              end if
              do k = 0, nm
                  rj(k) = cs*rj(k)
              end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " rctj: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF               
          end if
          
              dj(0) = DCOS(x)
              do k = 1, nm
                  tk = DBLE(k)
                  dj(k) = -tk*rj(k)/x+rj(k-1)
              end do
              
    end subroutine
    
    !==============================================================================84
    !  Subroutine: CFC ( complex Fresnel integral C(z) and C'(z))
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state 
    !==============================================================================84
    subroutine cfc(z,zf,zd,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
         
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          complex(R64P),               intent(in)    :: z
          complex(R64P),               intent(inout) :: zf,zd
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          complex(R64P) :: c,cf,cf0,cf1,cg,cr,z0,zp,zp2
          real(R64P)    :: eps,pi,w0,wa,wa0,tk
          integer(I32P) :: k,m
          integer(I32P), parameter :: kmax = 80
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF          
           ! Start of executable statements
           ! Sanity check of input arguments
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          eps = MACHEPSF64
          pi = CPI
          w0 = ABS(z)
          zp = 0.5_R64P*pi*z*z
          zp2 = zp*zp
          z0 = DCMPLX(0._R64P,0._R64P)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
          if(z.EQ.z0) then
              c = z0
          else if(w0.LE.2.5_R64P) then
              cr = z
              c = cr
              do k = 1, kmax
                  tk = DBLE(k)
                  cr = -0.5_R64P*cr*(4._R64P*tk-3._R64P) &
                      /tk/(2._R64P*tk-1._R64P) &
                      /(4._R64P*tk+1._R64P)*zp2
                  c = c+cr
                  wa = ABS(c)
                  if(ABS((wa-wa0)/wa).LT.eps .AND. 10.LT.k) exit
                  wa0 = wa
              end do
          else if(2.5_R64P.LT.w0 .AND. w0.LT.4.5_R64P) then
              m = 84
              c = z0
              cf1 = z0
              cf0 = DCMPLX(1.0D-30,0._R64P)
              do k = m, 0, -1
                  tk = DBLE(k)
                  cf = (2._R64P*tk+3._R64P)*cf0/zp-cf1
                  if(k.EQ.(k/2)*2) then
                      c = c+cf
                  end if
                  cf1 = cf0
                  cf0 = cf
              end do
              c = SQRT(2._R64P/(pi*zp))*SIN(zp)/cf*c
          else
              cr = DCMPLX(1._R64P,0._R64P)
              cf = DCMPLX(1._R64P,0._R64P)
              do k = 1, 20
                  cr = -0.25_R64P*cr*(4._R64P*DBLE(k)-1._R64P) &
                      *(4._R64P*DBLE(k)-3._R64P)/zp2
                  cf = cf+cr
              end do
              cr = 1._R64P/(pi*z*z)
              cg = cr
              do k = 1, 12
                  cr = -0.25_R64P*cr*(4._R64P*DBLE(k)+1._R64P) &
                      *(4._R64P*DBLE(k)-1._R64P)/zp2
                  cg = cg+cr
              end do
              c = 0.5_R64P+(cf*SIN(zp)-cg*COS(zp))/(pi*z)
           end if   
           zf = c   
           zd = COS(0.5_R64P*pi*z*z)   
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " cfc: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF            
    end subroutine
    
    !==============================================================================84
    !  Subroutine: LPMNS
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state 
    !==============================================================================84
    subroutine lpmns(m,n,x,pm,pd,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          integer(I32P),              intent(in)    :: m,n
          real(R64P),                 intent(in)    :: x
          real(R64P), dimension(0:n), intent(inout) :: pm,pd
          logical(I32P), dimension(5),intent(inout) :: fp_flags
          ! Locals
          integer(I32P) :: k
          real(R64P)    :: pm0,pm1,pm2,pmk,x0,tk
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of inputs
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          do k = 0, n
              pm(k) = 0._R64P
              pd(k) = 0._R64P
          end do
          if(DABS(x).EQ.1._R64P) then
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF              
              do k = 0, n
                  tk = DBLE(k)
                  if(m.EQ.0) then
                      pm(k) = 1._R64P
                      pd(k) = 0.5_R64P*tk*(tk+1._R64P)
                      if(x.LT.0._R64P) then
                          pm(k) = (-1._R64P)**k*pm(k)
                          pd(k) = (-1._R64P)**(k+1)*pd(k)
                      end if
                  else if(m.EQ.1) then
                      pd(k) = HUGE(1._R64P)
                  else if(m.EQ.2) then
                      pd(k) = -0.25_R64P*(tk+2._R64P)*(tk+1._R64P) &
                               *tk*(tk-1._R64P)
                      if(x.LT.0._R64P) then
                          pd(k) = (-1._R64P)**(k+1)*pd(k)
                      end if
                  end if
              end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " lpmns: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF              
              return
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          x0 = DABS(1._R64P-x*x)
          pm0 = 1._R64P
          pmk = pm0
          do k = 1, m
              tk = DBLE(k)
              pmk = (2._R64P*tk-1._R64P)*DSQRT(x0)*pm0
              pm0 = pmk
          end do
          pm1 = (2._R64P*m+1._R64P)*x*pm0
          pm(m) = pmk
          pm(m+1) = pm1
          do k = m+2,n
              tk = DBLE(k)
              pm2 = ((2._R64P*tk-1._R64P)*x*pm1 &
                  -(DBLE(k+m)-1._R64P)*pmk)/(k-m)
              pm(k) = pm2
              pmk = pm1
              pm1 = pm2
          end do
          pd(0) = ((1._R64P-DBLE(m)*pm(1)-x*pm(0)) &
                  /(x*x-1._R64P)
          do k = 1, n
              pd(k) = (k*x*pm(k)-(k+m)*pm(k-1)) &
                      /(x*x-1._R64P)
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " lpmns: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
    
    !==============================================================================84
    !  Function: MSTA1
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state 
    !==============================================================================84
    function msta1(x,mp) result(val)
          implicit none
          real(R64P),    intent(in) :: x
          integer(I32P), intent(in) :: mp
          ! Locals
          real(R64P)    :: a0,envj,f,f0,f1
          integer(I32P) :: val,it,n0,n1,nn
          ! Start of executable statements
          a0 = DABS(x)
          n0 = IDINT(1.1_R64P*a0)+1
          f0 = envj(n0,a0)-mp
          n1 = n0+5
          f1 = envj(n1,a0)-mp
          do it = 1, 20
              nn = n1-(n1-n0)/(1._R64P-f0/f1)
              f = envj(nn,a0)-mp
              if(IABS(nn-n1).LT.1) exit
              n0 = n1
              f0 = f1
              n1 = nn
              f1 = f
          end do
          val = nn
    end function
    
    !==============================================================================84
    !  Function: MSTA2
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state 
    !==============================================================================84
    function msta2(x,n,mp) result(val)
          implicit none
          real(R64P),    intent(in) :: x
          integer(I32P), intent(in) :: n,mp
          ! Locals
          integer(I32P) :: val,it,,n0,n1,nn
          real(R64P)    :: a0,ejn,f,f0,f1,hmp,obj
          ! Start of executable statments
          a0 = DABS(x)
          hmp = 0.5_R64P*DBLE(mp)
          ejn = envj(n,a0)
          if(ejn.LE.hmp) then
              obj = DBLE(mp)
              n0 = IDINT(1.1_R64P*a0)
          else
              obj = hmp+ejn
          end if
          f0 = envj(n0,a0)-obj
          n1 = n0+5
          f1 = envj(n1,a0)-obj
          do it = 1, 20
              nn = n1-(n1-20)/(1._R64P-f0/f1)
              f = envj(nn,a0)-obj
              if(IABS(nn-n1).LT.1) exit
              n0 = n1
              f0 = f1
              n1 = nn
              f1 = f
          end do
          val = nn+10
    end function
    
    !==============================================================================84
    !  Function: ENVJ
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state 
    !==============================================================================84
    function envj(n,x) result(val)
          implicit none
          integer(I32P), intent(in) :: n
          real(R64P),    intent(in) :: x
          ! Locals
          real(R64P) :: val
          val = 0.5_R64P*DLOG10(6.28_R64P*DBLE(n))-DBLE(n)*DLOG10(1.36_R64P*x/DBLE(n))
    end function
    
    !==============================================================================84
    !  Subroutine: SCKB
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state 
    !==============================================================================84
    subroutine sckb(m,n,c,df,ck,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          integer(I32P),               intent(in)    :: m,n
          real(R64P),                  intent(in)    :: c
          real(R64P), dimension(200),  intent(in)    :: df
          real(R64P), dimension(200),  intent(inout) :: ck
          logical(I32P), dimension(5), intent(inout) :: fp_flags
          ! Locals
          real(R64P)    :: d1,d2,d3,fac,r,r1,reg,sum,sw
          integer(I32P) :: i,i1,i2,ip,k,m,m,nm
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity check of input arguments
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          c = DMAX1(c,0.00000000001_R64P)
          nm = 25+DINT(0.5_R64P*(n-m)+c)
          if(n-m.EQ.2*(n-m)/2) then
              ip = 0
          else
              ip = 1
          end if
          if(80.LT.m+nm) then
              reg = 1.0D-200
          else
              reg = 1._R64P
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF           
          fac = -0.5_R64P**m
          do k = 0, nm-1
              fac = -fac
              i1 = 2*k+ip+1
              r = reg
              do i = i1, i1+2*m-1
                  r = r*i
              end do
              i2 = k+m+ip
              do i = i2, i2+k-1
                  r = r*(DBLE(i)+0.5_R64P)
              end do
              sum = r*df(k+1)
              do i = k+1, nm
                  d1 = 2._R64P*i+ip
                  d2 = 2._R64P*m+d1
                  d3 = i+m+ip-0.5_R64P
                  r = r*d2*(d2-1._R64P)*i*(d3+k) &
                      /(d1*(d1-1._R64P)*(i-k)*d3)
                  sum = sum+r*df(i+1)
                  if(DABS(sw-sum).LT.DABS(sum)*1.0D-14) exit
                  sw = sum
              end do
              r1 = reg
              do i = 2, m+k
                  r1 = r1*i
              end do
              ck(k+1) = fac*sum/r1
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " sckb: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
    
    !==============================================================================84
    !  Subroutine: SDMN
    !  Originally implemented by -- Shanjie Zhang, Jianming Jin
    !  contact:                     http://in.ece.illinois.edu/routines/routines.html
    !   
    !  Modified by -- Bernard Gingold
    !  contact: beniekg@gmail.com
    !  
    !  Reference:
    !              Shanjie Zhang, Jianming Jin,
    !              Computation of Special Functions,
    !              Wiley, 1996,
    !              ISBN: 0-471-11963-6,
    !              LC: QA351.C45. 
    !  
    !  Modifications and changes made:
    !  1) Packaging all subroutines in module
    !  2) Changing double precision to real(R64P) data type
    !  3) Passing additional argument of type logical(I32P) dimension(5) in
    !     order to collect HW specific flags of Floating-Point environment state
    !==============================================================================84
    subroutine sdmn(m,n,c,cv,kd,df,fp_flags)
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          use, intrinsic :: ieee_exceptions
!DIR$ ENDIF
          use ISO_FORTRAN_ENV, only : ERROR_UNIT    
          implicit none
          integer(I32P),                 intent(in) :: m,n      ! mode parameters
          real(R64P),                    intent(in) :: c,cv     ! spheroidal parameter , characteristic value
          integer(I32P),                 intent(in) :: kd
          real(R64P), dimension(200), intent(inout) :: df ! expansion coefficients
          logical(I32P), dimension(5),intent(inout) :: fp_flags ! FP environment exception flags
          ! Locals
          real(R64P) :: cs,cv,d2k,dk0,dk1,dk2,f,f0,tk, &
                        f1,f2,fl,fs,r1,r3,r4,s0,su1,su2,sw
          real(R64P), dimension(200) ::  a,d,g
          real(R64P), parameter :: c1 = 0.0000000001_R64P
         
          integer(I32P) :: i,ip,j,k,k1,kb,kd,m,n,nm
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
          type(ieee_status_type)                  :: status_value
!DIR$ ENDIF
          ! Start of executable statements
          ! Sanity checks of input arguments
          if(ANY(fp_flags)) then
              fp_flags = .false.
          end if
          nm = IDINT(0.5_R64P*(n-m)+c)
          if(c.LT.c1) then
              do i = 1, nm
                  df(i) = 0._R64P
              end do
              df((n-m)/2+1) = 1._R64P
              return
          end if
          cs = c*c*kd
          if(n-m.EQ.2*(n-m)/2) then
              ip = 0
          else
              ip = 1
          end if
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_status(status_value)
              call ieee_set_halting_mode(ieee_all,.false.)
              call ieee_set_flag(ieee_all,fp_flags)
!DIR$ ENDIF          
          do i = 1, nm+2
             if(ip.EQ.0) then
                 k = 2*(i-1)
             else
                 k = 2*i-1
             end if
             dk0 = m+k
             dk1 = m+k+1
             dk2 = 2*(m+k)
             d2k = 2*m+k
             a(i) = (d2k+2._R64P)*(d2k+1._R64P) &
                 /((d2k+3._R64P)*(d2k+5._R64P))*cs
             d(i) = dk0*dk1 &
                   +(2._R64P*dk0*dk1-2._R64P*m*m-1._R64P) &
                   /((dk2-1._R64P)*(dk2+3._R64P))*cs
             g(i) = k*(k-1._R64P)/((dk2-3._R64P)) &
                   *(dk2-1._R64P))*cs
          end do
          fs = 1._R64P
          f1 = 0._R64P
          f0 = CSMALL
          kb = 0
          df(nm+1) = 0._R64P
          do k = nm, 1, -1
              f = -((d(k+1)-cv)*f0+a(k+1)*f1)/g(k+1)
              if(DABS(df(k+1).LT.DABS(f)) then
              df(k) = f
              f1 = f0
              f0 = f
              if(CSMALL.LT.DABS(f)) then
                  do k1 = k, nm
                     df(k1) = df(k1)*CSMALL
                  end do
                  f1 = f1*CSMALL
                  f0 = f0*CSMALL
              end if
          else
              kb = k
              fl = df(k+1)
              f1 = CSMALL
              f2 = -(d(1)-cv)/(a(1)*f1
              df(1) = f1
              if(kb.EQ.1) then
                  fs = f2
              else if(kb.EQ.2) then
                  df(2) = f2
                  fs = -((d(2)-cv)*f2+g(2)*f1)/a(2)
              else
                  df(2) = f2
                  do j = 3, kb+1
                      f = -((d(j-1)-cv)*f2+g(j-1)*f1)/a(j-1)
                      if(j.LE.kb) then
                          df(j) = f
                      end if
                      if(CSMALL.LT.DABS(f)) then
                          do k1 = 1, j
                              df(k1) = df(k1)*CSMALL
                          end do
                          f = f*CSMALL
                          f2 = f2*CSMALL
                      end if
                      f1 = f2
                      f2 = f
                  end do
                  fs = f
              end if
            exit
          end if
    end do
          su1 = 0._R64P
          r1 = 1._R64P
          do j = m+ip+1, 2*(m+ip)
              r1 = r1*j
          end do
          su1 = df(1)*r1
          do k = 2, kb
              tk = DBLE(K)
              r1 = -r1*(DBLE(k+m+ip)-1.5_R64P)/(tk-1._R64P)
              su1 = su1+r1*df(k)
          end do
          su2 = 0._R64P
          do k = kb+1, nm
              if(K.NE.1) then
                  r1 = -r1*(DBLE(k+m+ip)-1.5_R64P)/(tk-1._R64P)
              end if
              su2 = su2+r1*df(k)
              if(DABS(sw-su2).LT.DABS(su2)*1.0D-14) exit
              sw = su2
          end do
          r3 = 1._R64P
          do j = 1, (m+n+ip)/2
              r3 = r3*(j+0._R64P*(n+m+ip))
          end do
          r4 = 1._R64P
          do j = 1, (n-m-ip)/2
              r4 = -4._R64P*r4*j
          end do
          s0 = r3/(fl*(su1/fs)+su2)/r4
          do k = 1, kb
              df(k) = fl/fs*s0*df(k)
          end do
          do k = kb+1, nm
              df(k) = s0*df(k)
          end do
!DIR$ IF (USE_IEEE_EXCEPTION_HANDLING .EQ. 1)
              call ieee_get_flag(ieee_all,fp_flags)
              if(ANY(fp_flags)) then
                  
                  write(ERROR_UNIT,*) "================================================================="
                  write(ERROR_UNIT,*) " sdmn: FLOATING-POINT EXCEPTION(S) OCCURRED"
                  write(ERROR_UNIT,*) "================================================================="
              end if
              call ieee_set_status(status_value)
!DIR$ ENDIF           
    end subroutine
    
    
    
end module mod_specfuncs