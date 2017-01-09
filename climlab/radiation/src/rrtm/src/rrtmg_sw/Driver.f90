!  CLIMLAB driver for RRTMG_SW shortwave radiation
!
!  This is a lightweight driver that uses identical variable names
!  and units as found in the RRTM code. Refer to RRTMG_SW source code
!  for more documentation
!
!  Brian Rose
!  brose@albany.edu
!  (inspired by CliMT code by Rodrigo Caballero)

subroutine driver &
    (ncol, nlay, icld, permuteseed, irng, idrv, &
    play, plev, &
    tlay, tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, &
    o2vmr, aldif, aldir, asdif, &
    asdir, coszen, adjes, dyofyr, scon, &
    inflgsw, iceflgsw, liqflgsw, tauc_sw, cldfrac, ssac_sw, asmc_sw, &
    fsfc_sw, ciwp, clwp, reic, relq, &
    tauaer_sw, ssaaer_sw, asmaer_sw, ecaer_sw, &
    swuflx, swdflx, swhr, swuflxc, &
    swdflxc, swhrc)

! Modules
    use rrtmg_sw_rad, only: rrtmg_sw
    use parkind, only: im => kind_im
    use mcica_subcol_gen_sw, only: mcica_subcol_sw
    use rrtmg_sw_init, only: rrtmg_sw_ini
    use parrrsw, only: nbndsw, ngptsw, naerec


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
    real(kind=rb), intent(in) :: aldif(ncol)        ! UV/vis surface albedo direct rad
    real(kind=rb), intent(in) :: aldir(ncol)        ! Near-IR surface albedo direct rad
    real(kind=rb), intent(in) :: asdif(ncol)        ! UV/vis surface albedo: diffuse rad
    real(kind=rb), intent(in) :: asdir(ncol)        ! Near-IR surface albedo: diffuse rad
    real(kind=rb), intent(in) :: coszen(ncol)       ! Cosine of solar zenith angle
    real(kind=rb), intent(in) :: adjes              ! Flux adjustment for Earth/Sun distance
    integer(kind=im), intent(in) :: dyofyr          ! Day of the year (used to get Earth/Sun
                                                    !  distance if adjflx not provided)
    real(kind=rb), intent(in) :: scon               ! Solar constant (W/m2)
    integer(kind=im), intent(in) :: inflgsw         ! Flag for cloud optical properties
    integer(kind=im), intent(in) :: iceflgsw        ! Flag for ice particle specification
    integer(kind=im), intent(in) :: liqflgsw        ! Flag for liquid droplet specification
    real(kind=rb), intent(in) :: cldfrac(ncol,nlay)
    real(kind=rb), intent(in) :: tauc_sw(nbndsw,ncol,nlay)
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

end subroutine driver
