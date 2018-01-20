
module mod_dvode_iface

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_dvode_iface'
 !          
 !          Purpose:
 !                    Implementation of interface to DVODE subroutines
 !                    and functions.
 !          History:
 !                        Date: 20-01-2018
 !                        Time: 12:07 GMT+2
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
 !                    DVODE - documentation.
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    
 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    implicit none
    use module_kinds, only : I32P
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59

    ! Major version
    integer(I32P), parameter, public :: MOD_DVODE_IFACE_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_DVODE_IFACE_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_DVODE_IFACE_MICRO = 0
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_DVODE_IFACE_FULLVER = 1000*MOD_DVODE_IFACE_MAJOR+100*MOD_DVODE_IFACE_MINOR+ &
                                                                       10*MOD_DVODE_IFACE_MICRO
    
    ! Creation date string
    character(*),  parameter, public :: MOD_DVODE_IFACE_CREATE_DATE = "20-01-2018 12:10 +00200 (SAT 20 JAN 2018 GMT+2)"
    
    ! Build date string (should be set to successful build date/time).
    character(*),  parameter, public :: MOD_DVODE_IFACE_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_DVODE_IFACE_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module description
    character(*),  parameter, public :: MOD_DVODE_IFACE_DESCRIPT = "This module contains DVODE interfaces."
    
    interface
    
         SUBROUTINE DVODE(F, NEQ, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK, &
                          ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JAC, MF, &
                          RPAR, IPAR)
         
                EXTERNAL F, JAC
                DOUBLE PRECISION Y, T, TOUT, RTOL, ATOL, RWORK, RPAR
                INTEGER NEQ, ITOL, ITASK, ISTATE, IOPT, LRW, IWORK, LIW, &
                        MF, IPAR
                DIMENSION Y(*), RTOL(*), ATOL(*), RWORK(LRW), IWORK(LIW), &
                          RPAR(*), IPAR(*)
         
         END SUBROUTINE
                          
         SUBROUTINE DVHIN(N, T0, Y0, YDOT, F, RPAR, IPAR, TOUT, UROUND, &
                          EWT, ITOL, ATOL, Y, TEMP, H0, NITER, IER)
         
                EXTERNAL F
                DOUBLE PRECISION T0, Y0, YDOT, RPAR, TOUT, UROUND, EWT, ATOL, Y,   &
                                 TEMP, H0
                INTEGER N, IPAR, ITOL, NITER, IER
                DIMENSION Y0(*), YDOT(*), EWT(*), ATOL(*), Y(*),      &
                          TEMP(*), RPAR(*), IPAR(*)
         END SUBROUTINE
                          
         SUBROUTINE DVINDY(T, K, YH, LDYH, DKY, IFLAG)
         
                DOUBLE PRECISION T, YH, DKY
                INTEGER K, LDYH, IFLAG
                DIMENSION YH(LDYH,*), DKY(*) 
                
         END SUBROUTINE
         
         SUBROUTINE DVSTEP(Y, YH, LDYH, YH1, EWT, SAVF, VSAV, ACOR, &
                           WM, IWM, F, JAC, PSOL, VNLS, RPAR, IPAR)
         
                 EXTERNAL F, JAC, PSOL, VNLS
                 DOUBLE PRECISION Y, YH, YH1, EWT, SAVF, VSAV, ACOR, WM, RPAR
                 INTEGER LDYH, IWM, IPAR
                 DIMENSION Y(*), YH(LDYH,*), YH1(*), EWT(*), SAVF(*), VSAV(*),    &
                           ACOR(*), WM(*), IWM(*), RPAR(*), IPAR(*)
                 
         END SUBROUTINE
                           
         SUBROUTINE DVSET()
         
         END SUBROUTINE
         
         SUBROUTINE DVJUST(YH, LDYH, IORD)
         
                DOUBLE PRECISION YH
                INTEGER LDYH, IORD
                DIMENSION YH(LDYH,*)  
                
         END SUBROUTINE
         
         SUBROUTINE DVNLSD(Y, YH, LDYH, VSAV, SAVF, EWT, ACOR, IWM, WM, &
                           F, JAC, PDUM, NFLAG, RPAR, IPAR)
         
                EXTERNAL F, JAC, PDUM
                DOUBLE PRECISION Y, YH, VSAV, SAVF, EWT, ACOR, WM, RPAR
                INTEGER LDYH, IWM, NFLAG, IPAR
                DIMENSION Y(*), YH(LDYH,*), VSAV(*), SAVF(*), EWT(*), ACOR(*),    &
                            IWM(*), WM(*), RPAR(*), IPAR(*) 
                
         END SUBROUTINE
                           
         SUBROUTINE DVJAC(Y, YH, LDYH, EWT, FTEM, SAVF, WM, IWM, F, JAC, &
                          IERPJ, RPAR, IPAR)
         
                EXTERNAL F, JAC
                DOUBLE PRECISION Y, YH, EWT, FTEM, SAVF, WM, RPAR
                INTEGER LDYH, IWM, IERPJ, IPAR
                DIMENSION Y(*), YH(LDYH,*), EWT(*), FTEM(*), SAVF(*),    &
                           WM(*), IWM(*), RPAR(*), IPAR(*) 
                
         END SUBROUTINE
                          
         SUBROUTINE DACOPY(NROW, NCOL, A, NROWA, B, NROWB)
                
                DOUBLE PRECISION A, B
                INTEGER NROW, NCOL, NROWA, NROWB
                DIMENSION A(NROWA,NCOL), B(NROWB,NCOL) 
                
         END SUBROUTINE
         
         SUBROUTINE DVSOL(WM, IWM, X, IERSL)
         
                DOUBLE PRECISION WM, X
                INTEGER IWM, IERSL
                DIMENSION WM(*), IWM(*), X(*)
                
         END SUBROUTINE
         
         SUBROUTINE DVSCRO(RSAV, ISAV, JOB)
         
                DOUBLE PRECISION RSAV
                INTEGER ISAV, JOB
                DIMENSION RSAV(*), ISAV(*)
                
         END SUBROUTINE
         
         SUBROUTINE DEWSET(N ,ITOL, RTOL, ATOL, YCUR, EWT)
         
                INTEGER N, ITOL
                DOUBLE PRECISION RTOL, ATOL, YCUR, EWT
                DIMENSION RTOL(*), ATOL(*), YCUR(N), EWT(N)
       
         END SUBROUTINE
         
         FUNCTION DVNORM(N, V, W) RESULT(SUM)
         
                INTEGER N,   I
                DOUBLE PRECISION V, W,   SUM
                DIMENSION V(N), W(N)
                
         END FUNCTION
         
         SUBROUTINE DUMSUM(A, B, C)
         
                DOUBLE PRECISION A, B, C 
                
         END SUBROUTINE
         
         SUBROUTINE DGEFA(A, LDA, N, IPVT, INFO)
         
                INTEGER LDA,N,IPVT(*),INFO
                DOUBLE PRECISION A(LDA,*) 
                
         END SUBROUTINE
         
         SUBROUTINE DGESL(A, LDA, N, IPVT, B , JOB)
         
                INTEGER LDA,N,IPVT(*),JOB
                DOUBLE PRECISION A(LDA,*),B(*)
                
         END SUBROUTINE
         
         SUBROUTINE DGBFA(ABD, LDA, N, ML, MU, IPVT, INFO)
         
                INTEGER LDA,N,ML,MU,IPVT(*),INFO
                DOUBLE PRECISION ABD(LDA,*)
                
         END SUBROUTINE
         
         SUBROUTINE DGBSL(ABD, LDA, N, ML, MU, IPVT, B, JOB)
         
               INTEGER LDA,N,ML,MU,IPVT(*),JOB
               DOUBLE PRECISION ABD(LDA,*),B(*) 
               
         END SUBROUTINE
         
         SUBROUTINE DAXPY(N, DA, DX, INCX, DY, INCY)
         
                INTEGER N, INCX, INCY
                DOUBLE PRECISION DX(*), DY(*), DA
                
         END SUBROUTINE
         
         SUBROUTINE DCOPY(N, DX, INCX, DY, INCY)
         
                INTEGER N, INCX, INCY
                DOUBLE PRECISION DX(*), DY(*)
                
         END SUBROUTINE
         
         DOUBLE PRECISION FUNCTION DDOT(N, DX, INCX, DY, INCY)
         
                INTEGER N, INCX, INCY
                DOUBLE PRECISION DX(*), DY(*)
                
         END FUNCTION
         
         DOUBLE PRECISION FUNCTION DNRM2(N, DX, INCX)
         
                INTEGER N, INCX
                DOUBLE PRECISION DX(*)
                
         END FUNCTION
         
         SUBROUTINE DSCAL(N, DA, DX, INCX)
         
                INTEGER N, INCX
                DOUBLE PRECISION DA, DX(*)
                
         END SUBROUTINE
         
         INTEGER FUNCTION IDAMAX(N, DX, INCX)
         
               INTEGER N, INCX
               DOUBLE PRECISION DX(*)
               
         END FUNCTION
         
    end interface
    

end module mod_dvode_iface