
module mod_mt3_mgr

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_mt2_mgr'
 !          
 !          Purpose:
 !                       Wrapper derived type which abstracts
 !                       and encapsulates calling procedure
 !                       of Mersenne-Twister subroutines.
 !                       Subroutine wrapper to mersenne twister
 !                       generators of type genrand_real1,
 !                       genrand_real2,genrand_real3,genrand_real53 are
 !                       invoked by derived type procedure
 !                       pointer component.
 !          History:
 !                        Date: 18-07-2017
 !                        Time: 08:58 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !           
 !                 Bernard Gingold 
 !
 !         
 !          
 !         
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

    implicit none
    use module_kinds
    use mod_wrap_mt95, only : init_field3D_mt1, &
                              init_field3D_mt2, &
                              init_field3D_mt3, &
                              init_field3D_mt53
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(I32P), parameter, public :: MOD_MT3_MGR_MAJOR = 1
    
    ! Minor version
    integer(I32P), parameter, public :: MOD_MT3_MGR_MINOR = 0
    
    ! Micro version
    integer(I32P), parameter, public :: MOD_MT3_MGR_MICRO = 0
    
    ! File/Module full version
    integer(I32P), parameter, public :: MOD_MT3_MGR_FULLVER = 1000*MOD_MT3_MGR_MAJOR + 100*MOD_MT3_MGR_MINOR + &
                                                              10*MOD_MT3_MGR_MICRO
    
    ! File/Module creation date
    character(*), parameter,  public :: MOD_MT3_MGR_CREATION_DATE="18-07-2017 11:16 AM GMT+2 (18 JULY TUE 2017 -00200)"
    
    ! File/Module build date (should be set after last successful build date/time)
    character(*), parameter,  public :: MOD_MT3_MGR_BUILD_DATE = " "
    
    ! File/Module author info
    character(*), parameter,  public :: MOD_MT3_MGR_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! File/Module description
    character(*), parameter,  public :: MOD_MT3_MGR_DESCRIPT = "Encapsulates Mersenne-Twister(field3D) procedures pointer"
    
    public :: MT_field3D_t
    
    type :: MT_field3D_t
        
        private
        
         ! Procedure  pointer to subroutines
         ! which initialize array components
         ! of field 3D.
         ! This procedure pointer must point to
         ! following subroutines:
         ! 1) init_field3D_mt1
         ! 2) init_field3D_mt2
         ! 3) init_field3D_mt3
         ! 4) init_field3D_mt53
        procedure(invoke_field3D_mtx), pointer :: m_pmtx  => NULL()
        
        logical(I32P)                          :: m_isbuilt
        
        contains
    
        procedure, pass(this), public          :: get_built_stat 
        
        procedure, pass(this), public          :: destroy
        
    end type MT_field3D_t
        
    abstract interface
        subroutine invoke_field3D_mtx(f3d,put,puta,option,its,ite, &
                                      kts,kte,jts,jte,ss,ssa,status,fp_flags)
          implicit none
          real(R64P), dimension(:,:,:), intent(out)            :: f3d
          integer(I32P),                intent(in)             :: put
          integer(I32P), dimension(:),  intent(in)             :: puta
          character(len=*),             intent(in)             :: option
          integer(I64P),                intent(in)             :: its,ite, &
                                                                  kts,kte, &
                                                                  jts,jte
          real(R64P),                   intent(inout),optional :: ss
          real(R64P), dimension(:,:,:), intent(inout),optional :: ssa
          integer(I32P),                intent(inout)          :: status
          logical(I32P), dimension(5),  intent(inout)          :: fp_flags
        end subroutine
    end interface
    
    interface MT_field3D_t
        procedure constructor
    end interface
    
    contains
    
    !==========================================================================80
    !   Class constructor
    !==========================================================================80
    type(MT_field3D_t) function constructor(pproc)
          implicit none
          procedure(invoke_field3D_mtx) :: pproc
          ! Start of executable statements
          constructor%m_pmtx => pproc
          if(ASSOCIATED(constructor%m_pmtx)) then
              constructor%m_isbuilt = .true.
          else
              constructor%m_isbuilt = .false.
              return
          end if
    end function
    
    !==========================================================================80
    !  Getter returning derived type component: m_isbuilt
    !==========================================================================80
    pure function get_built_stat(this) result(status)
          implicit none
          class(MT_field3D_t), intent(in) :: this
          ! Locals
          logical(I32P)                   :: status
          status = this%m_isbuilt
    end function
    
    !==========================================================================80
    ! Disassociates procedure pointer from its target
    !==========================================================================80
    subroutine destroy(this)
          implicit none
          class(MT_field3D_t), intent(inout) :: this
          ! Start of executable sattements
          if(ASSOCIATED(this%m_pmtx)) then
              NULLIFY(this%m_pmtx)
          end if
          if(.NOT. ASSOCIATED(this%m_pmtx)) then
              this%m_isbuilt = .false.
          end if
    end subroutine
    
end module mod_mt3_mgr