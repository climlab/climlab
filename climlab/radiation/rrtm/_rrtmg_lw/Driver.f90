!  CLIMLAB driver for RRTMG_LW radiation
!
!   using the latest version RRTMG_LW v4.85
!
!  This is a lightweight driver designed for wrapping with f2py
!  and calling from Python
!  Refer to RRTMG_LW source code for more documentation
!
!  Three functions are exposed here:
!   climlab_rrtmg_lw_ini (wrapper for rrtmg_lw_ini)
!   climlab_mcica_subcol_lw (wrapper for mcica_subcol_lw)
!   climlab_rrtmg_lw  (wrapper for rrtmg_lw)
!
!   The call signature for each of these is nearly identical to its
!   equivalent in the RRTMG_LW code.
!
!   See the python module climlab/radiation/rrtm/rrtmg_lw.py
!    to see how these are called from Python
!
!  Brian Rose
!  brose@albany.edu
!

subroutine climlab_rrtmg_lw_ini(cpdair)
    ! Modules
    use rrtmg_lw_init, only: rrtmg_lw_ini
    ! Input
    integer, parameter :: rb = selected_real_kind(12)
    real(kind=rb), intent(in) :: cpdair    ! Specific heat capacity of dry air
                                            ! at constant pressure at 273 K
                                            ! (J kg-1 K-1)
    ! Call the initialization routine
    call rrtmg_lw_ini(cpdair)
end subroutine climlab_rrtmg_lw_ini


subroutine climlab_mcica_subcol_lw &
    (ncol, nlay, icld, permuteseed, irng, play, &
    cldfrac, ciwp, clwp, reic, relq, tauc, cldfmcl, &
    ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl)

    ! Modules
    use parkind, only : im => kind_im
    use mcica_subcol_gen_lw, only: mcica_subcol_lw
    use parrrtm, only: nbndlw, ngptlw

! Input
    integer, parameter :: rb = selected_real_kind(12)
    integer(kind=im), intent(in) :: ncol            ! number of columns
    integer(kind=im), intent(in) :: nlay            ! number of model layers
    integer(kind=im), intent(inout) :: icld         ! Cloud overlap method
                                                    !    0: Clear only
                                                    !    1: Random
                                                    !    2: Maximum/random
                                                    !    3: Maximum
    integer(kind=im), intent(in) :: permuteseed     ! if the cloud generator is called multiple times,
                                                    ! permute the seed between each call.
                                                    ! between calls for LW and SW, recommended
                                                    ! permuteseed differes by 'ngpt'
    integer(kind=im), intent(inout) :: irng         ! flag for random number generator
                                                    !  0 = kissvec
                                                    !  1 = Mersenne Twister
    real(kind=rb), intent(in) :: play(ncol,nlay)    ! Layer pressures (hPa, mb)
    real(kind=rb), intent(in) :: cldfrac(ncol,nlay)      ! layer cloud fraction
    real(kind=rb), intent(in) :: ciwp(ncol,nlay)         ! in-cloud ice water path
    real(kind=rb), intent(in) :: clwp(ncol,nlay)         ! in-cloud liquid water path
    real(kind=rb), intent(in) :: reic(ncol,nlay)         ! cloud ice particle size
    real(kind=rb), intent(in) :: relq(ncol,nlay)         ! cloud liquid particle size
    real(kind=rb), intent(in) :: tauc(nbndlw,ncol,nlay)  ! in-cloud optical depth
! Output
    !   These quantities are computed by McICA
    real(kind=rb), intent(out) :: cldfmcl(ngptlw,ncol,nlay)    ! cloud fraction [mcica]
    real(kind=rb), intent(out) :: ciwpmcl(ngptlw,ncol,nlay)    ! in-cloud ice water path [mcica]
    real(kind=rb), intent(out) :: clwpmcl(ngptlw,ncol,nlay)    ! in-cloud liquid water path [mcica]
    real(kind=rb), intent(out) :: reicmcl(ncol,nlay)           ! ice partcle size (microns)  [mcica]
    real(kind=rb), intent(out) :: relqmcl(ncol,nlay)           ! liquid particle size (microns) [mcica]
    real(kind=rb), intent(out) :: taucmcl(ngptlw,ncol,nlay)    ! in-cloud optical depth [mcica]
!  These are not comments! Necessary directives to f2py to handle array dimensions
!f2py depend(ncol,nlay) play,
!f2py depend(ncol,nlay) cldfrac,ciwp,clwp,reic,relq
!f2py depend(ncol,nlay) tauc
! !f2py depend(ncol,nlay) cldfmcl,ciwpmcl,clwpmcl,taucmcl
! !f2py depend(ncol,nlay) reicmcl,relqmcl

    ! Call the Monte Carlo Independent Column Approximation
    !   (McICA, Pincus et al., JC, 2003)
    call mcica_subcol_lw(1, ncol, nlay, icld, permuteseed, irng, play, &
                       cldfrac, ciwp, clwp, reic, relq, tauc, cldfmcl, &
                       ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl)

end subroutine climlab_mcica_subcol_lw


subroutine climlab_rrtmg_lw &
    (ncol    ,nlay    ,icld     , ispec   , idrv    , &
    play    , plev    , tlay    , tlev    , tsfc    , &
    h2ovmr  , o3vmr   , co2vmr  , ch4vmr  , n2ovmr  , o2vmr , &
    cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr , emis    , &
    inflglw , iceflglw, liqflglw, cldfmcl , &
    taucmcl , ciwpmcl , clwpmcl , reicmcl , relqmcl , tauaer  , &
    olr_sr  , uflx    , dflx    , hr      , uflxc   , dflxc,  hrc, &
    duflx_dt,duflxc_dt)

! Modules
    use parkind, only : im => kind_im
    use parrrtm, only: nbndlw, ngptlw
    use rrtmg_lw_rad, only: rrtmg_lw

! Input
    integer, parameter :: rb = selected_real_kind(12)
    integer(kind=im), intent(in) :: ncol            ! number of columns
    integer(kind=im), intent(in) :: nlay            ! number of model layers
    integer(kind=im), intent(inout) :: icld         ! Cloud overlap method
    integer(kind=im), intent(inout) :: ispec        ! spectral OLR output flag
    integer(kind=im), intent(in) :: idrv            ! Flag for calculation of dFdT, the change
                                                    !    in upward flux as a function of
                                                    !    surface temperature [0=off, 1=on]
                                                    !    0: Normal forward calculation
                                                    !    1: Normal forward calculation with
                                                    !       duflx_dt and duflxc_dt output
    real(kind=rb), intent(in) :: play(ncol,nlay)    ! Layer pressures (hPa, mb)
    real(kind=rb), intent(in) :: plev(ncol,nlay+1)  ! Interface pressures (hPa, mb)
    real(kind=rb), intent(in) :: tlay(ncol,nlay)    ! Layer temperatures (K)
    real(kind=rb), intent(in) :: tlev(ncol,nlay+1)  ! Interface temperatures (K)
    real(kind=rb), intent(in) :: tsfc(ncol)         ! Surface temperature (K)
    real(kind=rb), intent(in) :: h2ovmr(ncol,nlay)  ! H2O volume mixing ratio
    real(kind=rb), intent(in) :: o3vmr(ncol,nlay)   ! O3 volume mixing ratio
    real(kind=rb), intent(in) :: co2vmr(ncol,nlay)  ! CO2 volume mixing ratio
    real(kind=rb), intent(in) :: ch4vmr(ncol,nlay)  ! Methane volume mixing ratio
    real(kind=rb), intent(in) :: n2ovmr(ncol,nlay)  ! Nitrous oxide volume mixing ratio
    real(kind=rb), intent(in) :: o2vmr(ncol,nlay)   ! Oxygen volume mixing ratio
    real(kind=rb), intent(in) :: cfc11vmr(ncol,nlay)  ! CFC11 volume mixing ratio
    real(kind=rb), intent(in) :: cfc12vmr(ncol,nlay)  ! CFC12 volume mixing ratio
    real(kind=rb), intent(in) :: cfc22vmr(ncol,nlay)  ! CFC22 volume mixing ratio
    real(kind=rb), intent(in) :: ccl4vmr(ncol,nlay)   ! CCL4 volume mixing ratio
    real(kind=rb), intent(in) :: emis(ncol,nbndlw)  ! Surface emissivity
    integer(kind=im), intent(in) :: inflglw         ! Flag for cloud optical properties
    integer(kind=im), intent(in) :: iceflglw       ! Flag for ice particle specification
    integer(kind=im), intent(in) :: liqflglw        ! Flag for liquid droplet specification
    real(kind=rb), intent(in) :: tauaer(ncol,nlay,nbndlw) ! aerosol optical depth at mid-point of LW spectral bands
    !   These quantities are computed by McICA
    real(kind=rb), intent(in) :: cldfmcl(ngptlw,ncol,nlay)    ! cloud fraction [mcica]
    real(kind=rb), intent(in) :: ciwpmcl(ngptlw,ncol,nlay)    ! in-cloud ice water path [mcica]
    real(kind=rb), intent(in) :: clwpmcl(ngptlw,ncol,nlay)    ! in-cloud liquid water path [mcica]
    real(kind=rb), intent(in) :: reicmcl(ncol,nlay)           ! ice partcle size (microns)  [mcica]
    real(kind=rb), intent(in) :: relqmcl(ncol,nlay)           ! liquid particle size (microns) [mcica]
    real(kind=rb), intent(in) :: taucmcl(ngptlw,ncol,nlay)    ! in-cloud optical depth [mcica]

! Output
    real(kind=rb), intent(out) :: olr_sr(ncol,nbndlw)    ! Spectrally-decomposed OLR (W/m2)
    real(kind=rb), intent(out) :: uflx(ncol,nlay+1)      ! Total sky longwave upward flux (W/m2)
    real(kind=rb), intent(out) :: dflx(ncol,nlay+1)      ! Total sky longwave downward flux (W/m2)
    real(kind=rb), intent(out) :: hr(ncol,nlay)          ! Total sky longwave radiative heating rate (K/d)
    real(kind=rb), intent(out) :: uflxc(ncol,nlay+1)     ! Clear sky longwave upward flux (W/m2)
    real(kind=rb), intent(out) :: dflxc(ncol,nlay+1)     ! Clear sky longwave downward flux (W/m2)
    real(kind=rb), intent(out) :: hrc(ncol,nlay)         ! Clear sky longwave radiative heating rate (K/d)

    real(kind=rb), intent(out) :: duflx_dt(ncol,nlay+1)  ! change in upward longwave flux (w/m2/K)
                                                         ! with respect to surface temperature
    real(kind=rb), intent(out) :: duflxc_dt(ncol,nlay+1) ! change in clear sky upward longwave flux (w/m2/K)
                                                         ! with respect to surface temperature

!  These are not comments! Necessary directives to f2py to handle array dimensions
!f2py depend(ncol,nlay) play, plev, tlay, tlev
!f2py depend(ncol,nlay) h2ovmr,o3vmr,co2vmr,ch4vmr,n2ovmr,o2vmr
!f2py depend(ncol,nlay) cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr
!f2py depend(ncol) tsfc, emis
!f2py depend(ncol) olr_sr
!f2py depend(ncol,nlay) tauaer
!f2py depend(ncol,nlay) cldfmcl,ciwpmcl,clwpmcl,taucmcl
!f2py depend(ncol,nlay) reicmcl,relqmcl
!f2py depend(ncol,nlay) uflx,dflx,hr,uflxc,dflxc,hrc,duflx_dt,duflxc_dt

    !  Call the RRTMG_LW driver to compute radiative fluxes
    call rrtmg_lw(ncol    ,nlay    ,icld    ,ispec           ,idrv  , &
             play    , plev    , tlay    , tlev    , tsfc    , &
             h2ovmr  , o3vmr   , co2vmr  , ch4vmr  , n2ovmr  , o2vmr , &
             cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr , emis    , &
             inflglw , iceflglw, liqflglw, cldfmcl , &
             taucmcl , ciwpmcl , clwpmcl , reicmcl , relqmcl , tauaer  , &
             olr_sr  , uflx    , dflx    , hr      , uflxc   , dflxc,  hrc, &
             duflx_dt,duflxc_dt )

end subroutine climlab_rrtmg_lw
