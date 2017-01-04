! see _rrtm_radiation for the python that prepares these arguments...
subroutine driver &
    (nbndlw, nbndsw, naerec, ncol, nlay, icld, &
    permuteseed_sw, permuteseed_lw, irng, idrv, cpdair, play, plev, &
    tlay, tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, &
    o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, aldif, aldir, asdif, &
    asdir, emis, coszen, adjes, dyofyr, scon, &
    inflgsw, inflglw, iceflgsw, iceflgslw, liqflgsw, liqflglw, tauc_sw, tauc_lw, cldfrac, ssac_sw, asmc_sw, &
    fsfc_sw, ciwp, clwp, reic, relq, &
    tauaer_sw, ssaaer_sw, asmaer_sw, ecaer_sw, tauaer_lw, &
    swuflx, swdflx, swhr, swuflxc, &
    swdflxc, swhrc, uflx, dflx, hr, uflxc, dflxc, hrc, duflx_dt, duflxc_dt)

! Modules
    use rrtmg_lw_rad, only: rrtmg_lw
    use rrtmg_sw_rad, only: rrtmg_sw
    use parkind, only: im => kind_im
    use mcica_subcol_gen_lw, only: mcica_subcol_lw
    use mcica_subcol_gen_sw, only: mcica_subcol_sw
    use rrtmg_lw_init, only: rrtmg_lw_ini
    use rrtmg_sw_init, only: rrtmg_sw_ini


! Input
    integer, parameter :: rb = selected_real_kind(12)
    integer(kind=im), intent(in) :: nbndlw
    integer(kind=im), intent(in) :: nbndsw
    integer(kind=im), intent(in) :: naerec
!     integer(kind=im), intent(in) :: iplon
    integer(kind=im), intent(in) :: ncol
    integer(kind=im), intent(in) :: nlay
    integer(kind=im), intent(inout) :: icld
    integer(kind=im), intent(in) :: permuteseed_sw
    integer(kind=im), intent(in) :: permuteseed_lw
    integer(kind=im), intent(inout) :: irng
    integer(kind=im), intent(in) :: idrv
    real(kind=rb), intent(in) :: cpdair
    real(kind=rb), intent(in) :: play(ncol,nlay)
    real(kind=rb), intent(in) :: plev(ncol,nlay+1)
    real(kind=rb), intent(in) :: tlay(ncol,nlay)
    real(kind=rb), intent(in) :: tlev(ncol,nlay+1)
    real(kind=rb), intent(in) :: tsfc(ncol)
    real(kind=rb), intent(in) :: h2ovmr(ncol,nlay)
    real(kind=rb), intent(in) :: o3vmr(ncol,nlay)
    real(kind=rb), intent(in) :: co2vmr(ncol,nlay)
    real(kind=rb), intent(in) :: ch4vmr(ncol,nlay)
    real(kind=rb), intent(in) :: n2ovmr(ncol,nlay)
    real(kind=rb), intent(in) :: o2vmr(ncol,nlay)
    real(kind=rb), intent(in) :: cfc11vmr(ncol,nlay)
    real(kind=rb), intent(in) :: cfc12vmr(ncol,nlay)
    real(kind=rb), intent(in) :: cfc22vmr(ncol,nlay)
    real(kind=rb), intent(in) :: ccl4vmr(ncol,nlay)
    real(kind=rb), intent(in) :: aldif(ncol)
    real(kind=rb), intent(in) :: aldir(ncol)
    real(kind=rb), intent(in) :: asdif(ncol)
    real(kind=rb), intent(in) :: asdir(ncol)
    real(kind=rb), intent(in) :: emis(ncol,nbndlw)
    real(kind=rb), intent(in) :: coszen(ncol)
    real(kind=rb), intent(in) :: adjes
    integer(kind=im), intent(in) :: dyofyr
    real(kind=rb), intent(in) :: scon
    integer(kind=im), intent(in) :: inflgsw
    integer(kind=im), intent(in) :: inflglw
    integer(kind=im), intent(in) :: iceflgsw
    integer(kind=im), intent(in) :: iceflgslw
    integer(kind=im), intent(in) :: liqflgsw
    integer(kind=im), intent(in) :: liqflglw
    real(kind=rb), intent(in) :: cldfrac(ncol,nlay)
    real(kind=rb), intent(in) :: tauc_sw(nbndsw,ncol,nlay)
    real(kind=rb), intent(in) :: tauc_lw(nbndlw,ncol,nlay)
    real(kind=rb), intent(in) :: ssac_sw(nbndsw,ncol,nlay)
    real(kind=rb), intent(in) :: asmc_sw(nbndsw,ncol,nlay)
    real(kind=rb), intent(in) :: fsfc_sw(nbndsw,ncol,nlay)
    real(kind=rb), intent(in) :: ciwp(ncol,nlay)
    real(kind=rb), intent(in) :: clwp(ncol,nlay)
    real(kind=rb), intent(in) :: reic(ncol,nlay)
    real(kind=rb), intent(in) :: relq(ncol,nlay)
    real(kind=rb), intent(in) :: tauaer_sw(ncol,nlay,nbndsw)
    real(kind=rb), intent(in) :: ssaaer_sw(ncol,nlay,nbndsw)
    real(kind=rb), intent(in) :: asmaer_sw(ncol,nlay,nbndsw)
    real(kind=rb), intent(in) :: ecaer_sw(ncol,nlay,naerec)
    real(kind=rb), intent(in) :: tauaer_lw(ncol,nlay,nbndlw)

! Output
    ! SW
    real(kind=rb), intent(out) :: swuflx(ncol,nlay+1)       ! Total sky shortwave upward flux (W/m2)
    real(kind=rb), intent(out) :: swdflx(ncol,nlay+1)       ! Total sky shortwave downward flux (W/m2)
    real(kind=rb), intent(out) :: swhr(ncol,nlay)         ! Total sky shortwave radiative heating rate (K/d)
    real(kind=rb), intent(out) :: swuflxc(ncol,nlay+1)      ! Clear sky shortwave upward flux (W/m2)
    real(kind=rb), intent(out) :: swdflxc(ncol,nlay+1)      ! Clear sky shortwave downward flux (W/m2)
    real(kind=rb), intent(out) :: swhrc(ncol,nlay)        ! Clear sky shortwave radiative heating rate (K/d)

    ! LW
    real(kind=rb), intent(out) :: uflx(ncol,nlay+1)         ! Total sky longwave upward flux (W/m2)
    real(kind=rb), intent(out) :: dflx(ncol,nlay+1)         ! Total sky longwave downward flux (W/m2)
    real(kind=rb), intent(out) :: hr(ncol,nlay)           ! Total sky longwave radiative heating rate (K/d)
    real(kind=rb), intent(out) :: uflxc(ncol,nlay+1)        ! Clear sky longwave upward flux (W/m2)
    real(kind=rb), intent(out) :: dflxc(ncol,nlay+1)        ! Clear sky longwave downward flux (W/m2)
    real(kind=rb), intent(out) :: hrc(ncol,nlay)          ! Clear sky longwave radiative heating rate (K/d)

    real(kind=rb), intent(out) :: duflx_dt(ncol,nlay+1)
    real(kind=rb), intent(out) :: duflxc_dt(ncol,nlay+1)

    ! Local
    real(kind=rb) :: cldfmcl_sw(112,ncol,nlay)
    real(kind=rb) :: taucmcl_sw(112,ncol,nlay)
    real(kind=rb) :: ssacmcl_sw(112,ncol,nlay)
    real(kind=rb) :: asmcmcl_sw(112,ncol,nlay)
    real(kind=rb) :: fsfcmcl_sw(112,ncol,nlay)
    real(kind=rb) :: ciwpmcl_sw(112,ncol,nlay)
    real(kind=rb) :: clwpmcl_sw(112,ncol,nlay)
    real(kind=rb) :: reicmcl_sw(ncol,nlay)
    real(kind=rb) :: relqmcl_sw(ncol,nlay)
    real(kind=rb) :: cldfmcl_lw(140,ncol,nlay)
    real(kind=rb) :: taucmcl_lw(140,ncol,nlay)
    real(kind=rb) :: ciwpmcl_lw(140,ncol,nlay)
    real(kind=rb) :: clwpmcl_lw(140,ncol,nlay)
    real(kind=rb) :: reicmcl_lw(ncol,nlay)
    real(kind=rb) :: relqmcl_lw(ncol,nlay)

    call mcica_subcol_sw(1, ncol, nlay, icld, permuteseed_sw, irng, play, &
                       cldfrac, ciwp, clwp, reic, relq, tauc_sw, ssac_sw, asmc_sw, fsfc_sw, &
                       cldfmcl_sw, ciwpmcl_sw, clwpmcl_sw, reicmcl_sw, relqmcl_sw, &
                       taucmcl_sw, ssacmcl_sw, asmcmcl_sw, fsfcmcl_sw)
    call mcica_subcol_lw(1, ncol, nlay, icld, permuteseed_lw, irng, play, &
                       cldfrac, ciwp, clwp, reic, relq, tauc_lw, cldfmcl_lw, &
                       ciwpmcl_lw, clwpmcl_lw, reicmcl_lw, relqmcl_lw, taucmcl_lw)
    !  Now calling the init subroutines from Python
    !  when object is first instantiated (see _rrtm_radiation.py)
    !call rrtmg_sw_ini(cpdair)
    !call rrtmg_lw_ini(cpdair)
    call rrtmg_sw(ncol    ,nlay    ,icld    , &
             play    ,plev    ,tlay    ,tlev    ,tsfc   , &
             h2ovmr , o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr ,o2vmr , &
             asdir   ,asdif   ,aldir   ,aldif   , &
             coszen  ,adjes   ,dyofyr  ,scon    , &
             inflgsw ,iceflgsw,liqflgsw,cldfmcl_sw , &
             taucmcl_sw ,ssacmcl_sw ,asmcmcl_sw ,fsfcmcl_sw , &
             ciwpmcl_sw ,clwpmcl_sw ,reicmcl_sw ,relqmcl_sw , &
             tauaer_sw  ,ssaaer_sw ,asmaer_sw  ,ecaer_sw   , &
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc)
    call rrtmg_lw(ncol    ,nlay    ,icld    ,idrv    , &
             play    ,plev    ,tlay    ,tlev    ,tsfc    , &
             h2ovmr  ,o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr  ,o2vmr , &
             cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis    , &
             inflglw ,iceflglw,liqflglw,cldfmcl_lw , &
             taucmcl_lw ,ciwpmcl_lw ,clwpmcl_lw ,reicmcl_lw ,relqmcl_lw , &
             tauaer_lw  , &
             uflx    ,dflx    ,hr      ,uflxc   ,dflxc,  hrc, &
             duflx_dt,duflxc_dt )

end subroutine driver
