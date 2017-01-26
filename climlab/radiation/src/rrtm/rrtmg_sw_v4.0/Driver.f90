!  CLIMLAB driver for RRTMG_SW shortwave radiation
!
!  This is a lightweight driver that uses identical variable names
!  and units as found in the RRTM code. Refer to RRTMG_SW source code
!  for more documentation
!
!  Brian Rose
!  brose@albany.edu
!  (inspired by CliMT code by Rodrigo Caballero)
!
!  Updated for RRTMG_SW_v4.0

subroutine driver &
    (ncol, nlay, icld, permuteseed, irng, idrv, cpdair, &
    play, plev, tlay, tlev, tsfc, &
    h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, &
    aldif, aldir, asdif, asdir, coszen, adjes, dyofyr, scon, isolvar, &
    indsolvar, bndsolvar, solcycfrac, &
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
    use rrtmg_sw_init, only: rrtmg_sw_ini


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
    real(kind=rb), intent(in) :: cpdair    ! Specific heat capacity of dry air
                                           ! at constant pressure at 273 K
                                           ! (J kg-1 K-1)
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
                                                    !    Total solar irradiance averaged
                                                    !    over the solar cycle.
                                                    !    If scon = 0.0, the internal solar
                                                    !    constant, which depends on the
                                                    !    value of isolvar, will be used.
                                                    !    For isolvar=-1, scon=1368.22 Wm-2,
                                                    !    For isolvar=0,1,3, scon=1360.85 Wm-2,
                                                    !    If scon > 0.0, the internal solar
                                                    !    constant will be scaled to the
                                                    !    provided value of scon.
    integer(kind=im), intent(in) :: isolvar         ! Flag for solar variability method
                                                    !   -1 = (when scon .eq. 0.0): No solar variability
                                                    !        and no solar cycle (Kurucz solar irradiance
                                                    !        of 1368.22 Wm-2 only);
                                                    !        (when scon .ne. 0.0): Kurucz solar irradiance
                                                    !        scaled to scon and solar variability defined
                                                    !        (optional) by setting non-zero scale factors
                                                    !        for each band in bndsolvar
                                                    !    0 = (when SCON .eq. 0.0): No solar variability
                                                    !        and no solar cycle (NRLSSI2 solar constant of
                                                    !        1360.85 Wm-2 for the 100-50000 cm-1 spectral
                                                    !        range only), with facular and sunspot effects
                                                    !        fixed to the mean of Solar Cycles 13-24;
                                                    !        (when SCON .ne. 0.0): No solar variability
                                                    !        and no solar cycle (NRLSSI2 solar constant of
                                                    !        1360.85 Wm-2 for the 100-50000 cm-1 spectral
                                                    !        range only), is scaled to SCON
                                                    !    1 = Solar variability (using NRLSSI2  solar
                                                    !        model) with solar cycle contribution
                                                    !        determined by fraction of solar cycle
                                                    !        with facular and sunspot variations
                                                    !        fixed to their mean variations over the
                                                    !        average of Solar Cycles 13-24;
                                                    !        two amplitude scale factors allow
                                                    !        facular and sunspot adjustments from
                                                    !        mean solar cycle as defined by indsolvar
                                                    !    2 = Solar variability (using NRLSSI2 solar
                                                    !        model) over solar cycle determined by
                                                    !        direct specification of Mg (facular)
                                                    !        and SB (sunspot) indices provided
                                                    !        in indsolvar (scon = 0.0 only)
                                                    !    3 = (when scon .eq. 0.0): No solar variability
                                                    !        and no solar cycle (NRLSSI2 solar irradiance
                                                    !        of 1360.85 Wm-2 only);
                                                    !        (when scon .ne. 0.0): NRLSSI2 solar irradiance
                                                    !        scaled to scon and solar variability defined
                                                    !        (optional) by setting non-zero scale factors
                                                    !        for each band in bndsolvar
    real(kind=rb), intent(inout) :: indsolvar(2) ! Facular and sunspot amplitude
                                              ! scale factors (isolvar=1), or
                                              ! Mg and SB indices (isolvar=2)
                                              !    Dimensions: (2)
    real(kind=rb), intent(inout) :: bndsolvar(nbndsw) ! Solar variability scale factors
                                                   ! for each shortwave band
                                                   !    Dimensions: (nbndsw=14)
    real(kind=rb), intent(inout) :: solcycfrac   ! Fraction of averaged solar cycle (0-1)
                                              !    at current time (isolvar=1)

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

!  These are not comments! Necessary directives to f2py to handle array dimensions
!f2py depend(ncol,nlay) play, plev, tlay, tlev
!f2py depend(ncol,nlay) h2ovmr,o3vmr,co2vmr,ch4vmr,n2ovmr,o2vmr
!f2py depend(ncol) tsfc, aldif, aldir, asdif, asdir, coszen
!f2py depend(ncol,nlay) cldfrac,ciwp,clwp,reic,relq,tauc,ssac,asmc,fsfc
!f2py depend(ncol,nlay) ciwp,clwp,reic,relq,tauaer,ssaaer,asmaer,ecaer
!f2py depend(ncol,nlay) swuflx,swdflx,swhr,swuflxc,swdflxc,swhrc

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

     ! In principle the init routine should not need to be called every timestep
     !  But calling it from Python is not working... heatfac is not getting set properly
     call rrtmg_sw_ini(cpdair)

    !  Call the RRTMG_SW driver to compute radiative fluxes
    call rrtmg_sw(ncol    ,nlay    ,icld    ,iaer    , &
              play    ,plev    ,tlay    ,tlev    ,tsfc   , &
              h2ovmr , o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr ,o2vmr , &
              asdir   ,asdif   ,aldir   ,aldif   , &
              coszen  ,adjes   ,dyofyr  ,scon    ,isolvar, &
              inflgsw ,iceflgsw,liqflgsw,cldfmcl , &
              taucmcl ,ssacmcl ,asmcmcl ,fsfcmcl , &
              ciwpmcl ,clwpmcl ,reicmcl ,relqmcl , &
              tauaer  ,ssaaer  ,asmaer  ,ecaer   , &
              swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc, &
              bndsolvar,indsolvar,solcycfrac)

end subroutine driver
