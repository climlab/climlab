module absems
!  BRIAN ROSE added this module to the CliMT implementation of CAM3 radiation
!  The purpose is to expose some data arrays at the Python level
!  So all I/O can be handled by Python-level reading of netcdf files.

    use shr_kind_mod, only: r8 => shr_kind_r8
    use ppgrid, only: pcols, pver, pverp

    implicit none
    !  reproducing what's in radae.F90
    real(r8), public, allocatable, target :: abstot_3d(:,:,:,:) ! Non-adjacent layer absorptivites
    real(r8), public, allocatable, target :: absnxt_3d(:,:,:,:) ! Nearest layer absorptivities
    real(r8), public, allocatable, target :: emstot_3d(:,:,:)   ! Total emissivity
    integer,  public :: ntoplw    !top level to solve for longwave cooling

    integer, parameter :: n_u = 25   ! Number of U in abs/emis tables
    integer, parameter :: n_p = 10   ! Number of P in abs/emis tables
    integer, parameter :: n_tp = 10  ! Number of T_p in abs/emis tables
    integer, parameter :: n_te = 21  ! Number of T_e in abs/emis tables
    integer, parameter :: n_rh = 7   ! Number of RH in abs/emis tables

    real(r8):: ah2onw(n_p, n_tp, n_u, n_te, n_rh)   ! absorptivity (non-window)
    real(r8):: eh2onw(n_p, n_tp, n_u, n_te, n_rh)   ! emissivity   (non-window)
    real(r8):: ah2ow(n_p, n_tp, n_u, n_te, n_rh)    ! absorptivity (window, for adjacent layers)
    real(r8):: cn_ah2ow(n_p, n_tp, n_u, n_te, n_rh)    ! continuum transmission for absorptivity (window)
    real(r8):: cn_eh2ow(n_p, n_tp, n_u, n_te, n_rh)    ! continuum transmission for emissivity   (window)
    real(r8):: ln_ah2ow(n_p, n_tp, n_u, n_te, n_rh)    ! line-only transmission for absorptivity (window)
    real(r8):: ln_eh2ow(n_p, n_tp, n_u, n_te, n_rh)    ! line-only transmission for emissivity   (window)

    save

    contains

    subroutine initialize_radbuffer
    !
    ! Initialize radiation buffer data
    !
    !!++CliMT:  BRIAN moving this from radae.F90 to absems.F90
      ntoplw = 1
    !!++CliMT: NOTE: buffers allocated at first instantiation, no allocation needed
    !                in successive instatiations
      if (.not. allocated(abstot_3d)) then
         allocate (abstot_3d(pcols,ntoplw:pverp,ntoplw:pverp,1))
         allocate (absnxt_3d(pcols,pver,4,1))
         allocate (emstot_3d(pcols,pverp,1))
      endif
    !!--CliMT

    !++CliMT inf/nan disabled
    !+++rca   abstot_3d(:,:,:,:) = inf
    !+++rca   absnxt_3d(:,:,:,:) = inf
    !+++rca   emstot_3d(:,:,:) = inf
      abstot_3d(:,:,:,:) = 0.
      absnxt_3d(:,:,:,:) = 0.
      emstot_3d(:,:,:) = 0.
    !---CliMT
      return
    end subroutine initialize_radbuffer

end module absems
