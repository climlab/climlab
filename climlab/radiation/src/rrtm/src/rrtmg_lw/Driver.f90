! see _rrtm_radiation for the python that prepares these arguments...
subroutine driver &
    (nbndlw, ncol, nlay, icld, &
    permuteseed, irng, idrv, play, plev, &
    tlay, tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, &
    o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, &
    emis, &
    inflglw, iceflgslw, liqflglw, tauc, cldfrac, &
    ciwp, clwp, reic, relq, &
    tauaer, &
    uflx, dflx, hr, uflxc, dflxc, hrc, duflx_dt, duflxc_dt)

! Modules
    use rrtmg_lw_rad, only: rrtmg_lw
    use parkind, only: im => kind_im
    use mcica_subcol_gen_lw, only: mcica_subcol_lw
    use rrtmg_lw_init, only: rrtmg_lw_ini


! Input
    integer, parameter :: rb = selected_real_kind(12)
    integer(kind=im), intent(in) :: nbndlw   !  number of spectral bands, set to 16
!     integer(kind=im), intent(in) :: iplon
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
    integer(kind=im), intent(in) :: iceflgslw       ! Flag for ice particle specification
    integer(kind=im), intent(in) :: liqflglw        ! Flag for liquid droplet specification
    real(kind=rb), intent(in) :: cldfrac(ncol,nlay)      ! layer cloud fraction
    real(kind=rb), intent(in) :: tauc(nbndlw,ncol,nlay)  ! in-cloud optical depth
    real(kind=rb), intent(in) :: ciwp(ncol,nlay)         ! in-cloud ice water path
    real(kind=rb), intent(in) :: clwp(ncol,nlay)         ! in-cloud liquid water path
    real(kind=rb), intent(in) :: reic(ncol,nlay)         ! cloud ice particle size
    real(kind=rb), intent(in) :: relq(ncol,nlay)         ! cloud liquid particle size
    real(kind=rb), intent(in) :: tauaer(ncol,nlay,nbndlw) ! aerosol optical depth at mid-point of LW spectral bands

! Output
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

    ! Local
    real(kind=rb) :: cldfmcl(140,ncol,nlay)
    real(kind=rb) :: taucmcl(140,ncol,nlay)
    real(kind=rb) :: ciwpmcl(140,ncol,nlay)
    real(kind=rb) :: clwpmcl(140,ncol,nlay)
    real(kind=rb) :: reicmcl(ncol,nlay)
    real(kind=rb) :: relqmcl(ncol,nlay)

    call mcica_subcol_lw(1, ncol, nlay, icld, permuteseed, irng, play, &
                       cldfrac, ciwp, clwp, reic, relq, tauc, cldfmcl, &
                       ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl)

    call rrtmg_lw(ncol    ,nlay    ,icld    ,idrv    , &
             play    ,plev    ,tlay    ,tlev    ,tsfc    , &
             h2ovmr  ,o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr  ,o2vmr , &
             cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis    , &
             inflglw ,iceflglw,liqflglw,cldfmcl , &
             taucmcl ,ciwpmcl ,clwpmcl ,reicmcl ,relqmcl , &
             tauaer  , &
             uflx    ,dflx    ,hr      ,uflxc   ,dflxc,  hrc, &
             duflx_dt,duflxc_dt )

end subroutine driver
