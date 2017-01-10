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
    play, plev, tlay, tlev, tsfc, &
    h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, &
    aldif, aldir, asdif, asdir, coszen, adjes, dyofyr, scon, &
    inflgsw, iceflgsw, liqflgsw, &
    cldfrac, ciwp, clwp, reic, relq, tauc, ssac, asmc, fsfc, &
    tauaer, ssaaer, asmaer, ecaer, &
    swuflx, swdflx, swhr, swuflxc, swdflxc, swhrc)

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
    real(kind=rb), intent(in) :: cldfrac(ncol,nlay)        ! layer cloud fraction
    real(kind=rb), intent(in) :: tauc(nbndsw,ncol,nlay)    ! in-cloud optical depth
    real(kind=rb), intent(in) :: ssac(nbndsw,ncol,nlay)    ! in-cloud single scattering albedo (non-delta scaled)
    real(kind=rb), intent(in) :: asmc(nbndsw,ncol,nlay)    ! in-cloud asymmetry parameter (non-delta scaled)
    real(kind=rb), intent(in) :: fsfc(nbndsw,ncol,nlay)    ! in-cloud forward scattering fraction (non-delta scaled)
    real(kind=rb), intent(in) :: ciwp(ncol,nlay)           ! in-cloud ice water path
    real(kind=rb), intent(in) :: clwp(ncol,nlay)           ! in-cloud liquid water path
    real(kind=rb), intent(in) :: reic(ncol,nlay)           ! cloud ice particle size
    real(kind=rb), intent(in) :: relq(ncol,nlay)           ! cloud liquid particle size
    real(kind=rb), intent(in) :: tauaer(ncol,nlay,nbndsw)  ! Aerosol optical depth (iaer=10 only)
    real(kind=rb), intent(in) :: ssaaer(ncol,nlay,nbndsw)  ! Aerosol single scattering albedo (iaer=10 only)
    real(kind=rb), intent(in) :: asmaer(ncol,nlay,nbndsw)  ! Aerosol asymmetry parameter (iaer=10 only)
    real(kind=rb), intent(in) :: ecaer(ncol,nlay,naerec)   ! Aerosol optical depth at 0.55 micron (iaer=6 only)

! Output
    real(kind=rb), intent(out) :: swuflx(ncol,nlay+1)    ! Total sky shortwave upward flux (W/m2)
    real(kind=rb), intent(out) :: swdflx(ncol,nlay+1)    ! Total sky shortwave downward flux (W/m2)
    real(kind=rb), intent(out) :: swhr(ncol,nlay)        ! Total sky shortwave radiative heating rate (K/d)
    real(kind=rb), intent(out) :: swuflxc(ncol,nlay+1)   ! Clear sky shortwave upward flux (W/m2)
    real(kind=rb), intent(out) :: swdflxc(ncol,nlay+1)   ! Clear sky shortwave downward flux (W/m2)
    real(kind=rb), intent(out) :: swhrc(ncol,nlay)       ! Clear sky shortwave radiative heating rate (K/d)

    ! Local
    !   These quantities are computed by McICA
    real(kind=rb) :: cldfmcl(ngptsw,ncol,nlay)   ! cloud fraction [mcica]
    real(kind=rb) :: taucmcl(ngptsw,ncol,nlay)   ! in-cloud optical depth [mcica]
    real(kind=rb) :: ssacmcl(ngptsw,ncol,nlay)   ! in-cloud single scattering albedo [mcica]
    real(kind=rb) :: asmcmcl(ngptsw,ncol,nlay)   ! in-cloud asymmetry parameter [mcica]
    real(kind=rb) :: fsfcmcl(ngptsw,ncol,nlay)   ! in-cloud forward scattering fraction [mcica]
    real(kind=rb) :: ciwpmcl(ngptsw,ncol,nlay)   ! in-cloud ice water path [mcica]
    real(kind=rb) :: clwpmcl(ngptsw,ncol,nlay)   ! in-cloud liquid water path [mcica]
    real(kind=rb) :: reicmcl(ncol,nlay)          ! ice partcle size (microns)
    real(kind=rb) :: relqmcl(ncol,nlay)          ! liquid particle size (microns)

    ! Call the Monte Carlo Independent Column Approximation
    !   (McICA, Pincus et al., JC, 2003)
    call mcica_subcol_sw(1, ncol, nlay, icld, permuteseed, irng, play, &
                       cldfrac, ciwp, clwp, reic, relq, tauc, ssac, asmc, fsfc, &
                       cldfmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, &
                       taucmcl, ssacmcl, asmcmcl, fsfcmcl)
    !  Call the RRTMG_SW driver to compute radiative fluxes
    call rrtmg_sw(ncol    ,nlay    ,icld    , &
             play    ,plev    ,tlay    ,tlev    ,tsfc   , &
             h2ovmr , o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr ,o2vmr , &
             asdir   ,asdif   ,aldir   ,aldif   , &
             coszen  ,adjes   ,dyofyr  ,scon    , &
             inflgsw ,iceflgsw,liqflgsw,cldfmcl , &
             taucmcl, ssacmcl ,asmcmcl ,fsfcmcl , &
             ciwpmcl ,clwpmcl ,reicmcl ,relqmcl , &
             tauaer  ,ssaaer ,asmaer ,ecaer   , &
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc)

end subroutine driver
