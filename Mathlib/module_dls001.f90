
module mod_dls001

    !===========================================
    !     This module contain a content
    !     ODEPACK  common block: dls001
    !     This module must be used only
    !     by ODEPACK subroutines
    !===========================================

    !================================================================================
    !   
    !C! The following internal Common block contains
    !C (a) variables which are local to any subroutine but whose values must
    !C     be preserved between calls to the routine ("own" variables), and
    !C (b) variables which are communicated between subroutines.
    !C The block DLS001 is declared in subroutines DLSODE, DINTDY, DSTODE,
    !C DPREPJ, and DSOLSY.
    !C Groups of variables are replaced by dummy arrays in the Common
    !C declarations in routines where those variables are not used.
    !         COMMON /DLS001/ ROWNS(209),   &
    !    CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND, &
    !    INIT, MXSTEP, MXHNIL, NHNIL, NSLAST, NYH, IOWNS(6),  &
    !    ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L, &
    !    LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,  &
    !   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
    !===================================================================================
    implicit none
    use module_kinds, only : I32P,R64P
    
    ! Begin of data
    real(R64P), dimension(209), public :: ROWNS
    real(R64P), public                 :: ccmax
    real(R64P), public                 :: el0
    real(R64P), public                 :: h
    real(R64P), public                 :: hmin
    real(R64P), public                 :: hmxi
    real(R64P), public                 :: hu
    real(R64P), public                 :: rc
    real(R64P), public                 :: tn
    real(R64P), public                 :: uround
    
    integer(I32P), public               :: init
    integer(I32P), public               :: mxstep
    integer(I32P), public               :: mxhnil
    integer(I32P), public               :: nhnil
    integer(I32P), public               :: nslast
    integer(I32P), public               :: nyh
    integer(I32P), dimension(6),public  :: iowns
    integer(I32P), public               :: icf
    integer(I32P), public               :: ierpj
    integer(I32P), public               :: iersl
    integer(I32P), public               :: jcur
    integer(I32P), public               :: jstart
    integer(I32P), public               :: kflag
    integer(I32P), public               :: l
    integer(I32P), public               :: lyh
    integer(I32P), public               :: lacor
    integer(I32P), public               :: lsavf
    integer(I32P), public               :: lwm
    integer(I32P), public               :: liwm
    integer(I32P), public               :: meth
    integer(I32P), public               :: miter
    integer(I32P), public               :: maxord
    integer(I32P), public               :: maxcor
    integer(I32P), public               :: msbp
    integer(I32P), public               :: mxncf
    integer(I32P), public               :: n
    integer(I32P), public               :: nq
    integer(I32P), public               :: nst
    integer(I32P), public               :: nfe
    integer(I32P), public               :: nje
    integer(I32P), public               :: nqu
    ! End of common block dls001
end module mod_dls001