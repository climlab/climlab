!  CLIMLAB column radiation model driver
!   Brian Rose
!  Adapted from Rodrigo Caballero's CliMT
!
!  change units of q from g/kg to kg/kg
!   pass cosine of zenith angle rather than angle itself
!   and also pass eccentricity factor
!  Pass grid dimensions as input argument using dynamic arrays
!
!  Pass greenhouse gas amounts as absolute volume mixing ratios (ppp) instead of ppm
!
!  Don't change sign of upwelling LW flux ... it should be positive
!
! This is a driver code for CAM3 radiation written for CliMT; it replaces the
! analogous driver found in Zender's CCM3 CRM

!  9-apr-09
!    - put in check for q and o3 <= 0
!  12-feb-09
!   - LW code works
!   - SW code tested with clouds -- find differences ~10 % when compared
!     to CCM3; I assume this is due to very simplistic SW cloud treatment in CCM3
!  11-feb-09
!   - SW code works, tested with no cloud gives same result as CCM3 to 2 sig figs
!  10-feb-2009:
!   - code compiles (only radcsw implemented til now)
!   - todo: check all variables have correct phys dimensions

subroutine crm(  &
     pcols,  &
     pver,   &
     pverp,  &
     aldif,  &
     aldir,  &
     asdif,  &
     asdir,  &
     eccf,   &
     coszrs,  &
     solin_in,  &
     flus,  &
     cldf,  &
     clwp,  &
     ciwp,  &
     in_cld, &
     o3mmr_in,  &
     r_liq,  &
     r_ice,  &
     p,  &
     dp,  &
     ps,  &
     q_in,  &
     tg,  &
     t,  &
     co2vmr,  &
     n2ovmr,  &
     ch4vmr,  &
     f11vmr,  &
     f12vmr,  &
     idosw,  &
     idolw,  &
     gravit,  &
     cpair,  &
     epsilo,  &
     stebol,  &
     qrs,  &
     qrl,  &
     swflx,  &
     swflxc, &
     lwflx,  &
     lwflxc, &
     sw_cf_toa,  &
     sw_cf_srf,  &
     lw_cf_toa,  &
     lw_cf_srf, &
     lw_toa,  &
     lw_srf,  &
     sw_toa,  &
     sw_srf, &
     swup, swdn, swupc, swdnc, &
     lwup,lwdn,lwupc,lwdnc)


  use shr_kind_mod,        only: r8 => shr_kind_r8
  !use ppgrid,              only: pcols, pver, pverp
  use prescribed_aerosols, only: naer_all
  use radae,               only: radae_init
  use radsw,               only: radsw_init, radcswmx
  use radlw,               only: radlw_init, radclwmx
  use physconst,           only: mwdry, mwco2, mwo3, mwch4, mwn2o, mwf11, mwf12
  use pkg_cldoptics,       only: cldefr, cldems, cldovrlap, cldclw
  use wv_saturation,       only: aqsat
  use chem_surfvals,       only: chem_surfvals_set_co2

  implicit none

  ! Input
  !  CLIMLAB now passing grid dimensions as input
  integer, intent(in) ::   pcols
  integer, intent(in) ::   pver
  integer, intent(in) ::   pverp
  !
  real(r8), intent(in) ::  aldif(pcols)
  real(r8), intent(in) ::  aldir(pcols)
  real(r8), intent(in) ::  asdif(pcols)
  real(r8), intent(in) ::  asdir(pcols)
  !real(r8), intent(in) ::  zen
  !  driver now expects cosine of zenith angle and eccentricity factor (climlab)
  real(r8), intent(in) ::  eccf
  real(r8), intent(in) ::  coszrs(pcols)
  real(r8), intent(in) ::  solin_in
  real(r8), intent(in) ::  flus
  real(r8), intent(in) ::  cldf(pver)
  real(r8), intent(in) ::  clwp(pver)
  real(r8), intent(in) ::  ciwp(pver)
  integer,  intent(in) ::  in_cld
  real(r8), intent(in) ::  o3mmr_in(pver)
  real(r8), intent(in) ::  r_liq(pver)
  real(r8), intent(in) ::  r_ice(pver)
  real(r8), intent(in) ::  p(pver)
  real(r8), intent(in) ::  dp(pver)
  real(r8), intent(in) ::  ps
  real(r8), intent(in) ::  q_in(pver)
  real(r8), intent(in) ::  tg
  real(r8), intent(in) ::  t(pver)
  real(r8), intent(in) ::  co2vmr
  real(r8), intent(in) ::  n2ovmr
  real(r8), intent(in) ::  ch4vmr
  real(r8), intent(in) ::  f11vmr
  real(r8), intent(in) ::  f12vmr
  integer,  intent(in) ::  idosw
  integer,  intent(in) ::  idolw
  real(r8), intent(in) ::  gravit
  real(r8), intent(in) ::  cpair
  real(r8), intent(in) ::  epsilo
  real(r8), intent(in) ::  stebol
  ! Output
  real(r8), intent(out) ::  qrs(pver)
  real(r8), intent(out) ::  qrl(pver)
  real(r8), intent(out) ::  swflx(pverp)
  real(r8), intent(out) ::  swflxc(pverp)
  real(r8), intent(out) ::  swup(pverp)
  real(r8), intent(out) ::  swdn(pverp)
  real(r8), intent(out) ::  swupc(pverp)
  real(r8), intent(out) ::  swdnc(pverp)
  real(r8), intent(out) ::  lwflx(pverp)
  real(r8), intent(out) ::  lwflxc(pverp)
  real(r8), intent(out) ::  lwup(pverp)
  real(r8), intent(out) ::  lwdn(pverp)
  real(r8), intent(out) ::  lwupc(pverp)
  real(r8), intent(out) ::  lwdnc(pverp)
  real(r8), intent(out) ::  sw_cf_toa
  real(r8), intent(out) ::  sw_cf_srf
  real(r8), intent(out) ::  lw_cf_toa
  real(r8), intent(out) ::  lw_cf_srf
  real(r8), intent(out) ::  lw_toa
  real(r8), intent(out) ::  lw_srf
  real(r8), intent(out) ::  sw_toa
  real(r8), intent(out) ::  sw_srf

  ! Local
  integer  :: k
  integer  :: lchnk ! identifier (not used in climt)
  integer  :: ncol  ! no. atmos columns (fixed to 1 in climt)
  real(r8) :: pstd  ! standard pressure in Pa
  real(r8) :: solincgs  ! solin in CGS
  real(r8) :: q(pver) ! specific humidity (kg/kg)
  real(r8) :: o3mmr(pver)
  real(r8) :: pmid(pver) ! mid-level pressure in Pa
  real(r8) :: pint(pverp) ! interface pressure in Pa
  real(r8) :: pbr(pver) ! mid-level pressure in dynes/cm2
  real(r8) :: pnm(pverp) ! interface pressure in dynes/cm2
  real(r8) :: lnpmid(pver)  ! Ln(pmid)
  real(r8) :: lnpint(pverp)  ! Ln(pint)
  real(r8) :: rel(pver) ! liquid effective drop size
  real(r8) :: rei(pver) ! ice effective drop size
  real(r8) :: fice(pver) ! cloud ice fraction
  real(r8) :: cicewp(pver) ! in-cloud ice water path
  real(r8) :: cliqwp(pver) ! in-cloud liquid water path
  real(r8) :: emis(pver) ! cloud emissivity
  real(r8) :: pmxrgn(pverp) ! Maximum pressure for each max overlapped region
  integer  :: nmxrgn(pcols)       ! Number of maximally overlapped regions
  !real(r8) :: eccf  ! eccentricity factor
  !real(r8) :: coszrs(pcols)  ! cosine zenith angle
  real(r8) :: lwupcgs(pcols)  ! upward surface LW flux (CGS)
  real(r8) :: aerosol(pver,naer_all) ! aerosol mass mix ratio
  real(r8) :: esat(pver)  ! sat vapour press
  real(r8) :: qsat(pver)  ! sat specific humid
  real(r8) :: rh(pver)    ! relative humidity
  real(r8) :: co2         ! GHG mass mixing ratios
  real(r8) :: n2o(pver)
  real(r8) :: ch4(pver)
  real(r8) :: cfc11(pver)
  real(r8) :: cfc12(pver)
  logical  :: doabsems ! True => compute GHG path lengths in radclw
 ! -- output stuff from radcsw, radclw
  real(r8) :: solin_out(pcols)         ! dummy
  real(r8) :: fsntoa(pcols)        ! Net solar flux at TOA
  real(r8) :: fsntoac(pcols)       ! Clear sky net solar flux at TOA
  real(r8) :: fsnirt(pcols)       ! Near-IR flux absorbed at toa
  real(r8) :: fsnrtc(pcols)        ! Clear sky near-IR flux absorbed at toa
  real(r8) :: fsnirtsq(pcols)      ! Near-IR flux absorbed at toa >= 0.7 microns
  real(r8) :: fsntc(pcols)         ! Clear sky total column abs solar flux
  real(r8) :: fsns(pcols)          ! Surface solar absorbed flux
  real(r8) :: fsnt(pcols)          ! Net column abs solar flux at model top
  real(r8) :: fsnsc(pcols)         ! Clear sky surface abs solar flux
  real(r8) :: fsds(pcols)          ! Surface solar down flux
  real(r8) :: fsdsc(pcols)         ! Clear sky surface downwelling solar flux
  real(r8) :: flut(pcols)          ! Upward flux at top of model
  real(r8) :: lwcf(pcols)          ! longwave cloud forcing
  real(r8) :: swcf(pcols)          ! shortwave cloud forcing
  real(r8) :: flutc(pcols)         ! Upward Clear Sky flux at top of model
  real(r8) :: flnt(pcols)          ! Net lw flux at model top
  real(r8) :: flntc(pcols)         ! Clear sky lw flux at model top
  real(r8) :: flns(pcols)          ! Net lw flux at srf (up-down)
  real(r8) :: flnsc(pcols)         ! Clear sky lw flux at srf (up-down)
  real(r8) :: flwds(pcols)         ! Down longwave flux at surface
  real(r8) :: fcns(pverp)   ! net clear-sky shortwave flux
  real(r8) :: fcnl(pverp)   ! net clear-sky longwave flux
  real(r8) :: sols(pcols)      ! Direct solar rad on surface (< 0.7)
  real(r8) :: soll(pcols)      ! Direct solar rad on surface (>= 0.7)
  real(r8) :: solsd(pcols)     ! Diffuse solar rad on surface (< 0.7)
  real(r8) :: solld(pcols)     ! Diffuse solar rad on surface (>= 0.7)
  real(r8) :: fsnirtoa(pcols)  ! Near-IR flux absorbed at toa
  real(r8) :: fsnrtoac(pcols)  ! Clear sky near-IR flux absorbed at toa
  real(r8) :: fsnrtoaq(pcols)  ! Net near-IR flux at toa >= 0.7 microns
  real(r8) :: frc_day(pcols) ! = 1 for daylight, =0 for night columns
  integer nspint            ! Num of spctrl intervals across solar spectrum
  integer naer_groups       ! Num of aerosol groups for optical diagnostics
  parameter ( nspint = 19 )
  parameter ( naer_groups = 7 )    ! current groupings are sul, sslt, all carbons, all dust, background, and all aerosols
  real(r8) :: aertau(nspint,naer_groups) ! Aerosol column optical depth
  real(r8) :: aerssa(nspint,naer_groups) ! Aerosol column averaged single scattering albedo
  real(r8) :: aerasm(nspint,naer_groups) ! Aerosol column averaged asymmetry parameter
  real(r8) :: aerfwd(nspint,naer_groups) ! Aerosol column averaged forward scattering

  ! Set up variables
  doabsems = .true.
  lchnk = 1
  ncol = 1
  rel = r_liq
  rei = r_ice
  pstd = 1.01325e5
  if (flus == -99.) then
     lwupcgs = stebol*tg**4 * 1.e3
  else
     lwupcgs = flus * 1.e3
  endif
  solincgs = solin_in * 1.e3 ! MKS -> CGS
  !driver now expects cosine of zenith angle (climlab)
  !coszrs = cos(zen*abs(acos(-1.))/180.) ! zenith angle specified by user
  !eccf = 1.    ! eccen factor already in solin
  ! driver now expects eccf as input
  aerosol = 1.e-16 ! aerosols disabled in CliMT
  do k=1,pver
     if (o3mmr_in(k).le.0.) then
        print*,'o3neg!',k,o3mmr_in(k)
        o3mmr(k)=1.e-16
     else
        o3mmr(k)=o3mmr_in(k)
     endif
     if (q_in(k).le.0.) then
        print*,'qneg!',k
        q(k)=1.e-16
     else
        !q(k) = q_in(k) * 1.e-3 ! g/kg -> kg/kg
        q(k) = q_in(k)  ! both in kg/kg
     end if
  enddo
 ! -- define interface pressures and convert units
  pmid = p * 100. ! mb -> Pa
  if (dp(1) == -99.) then
     pint(1)=1.e-9
     do k=2,pver
        pint(k)=0.5*(pmid(k-1)+pmid(k))
     enddo
     pint(pverp) = ps*100. ! mb -> Pa
  else
     pint(1:pver) = pmid - dp*100./2.
     pint(pverp) = pmid(pver) + dp(pver)*100./2.
  endif
  pbr = pmid * 10. ! Pa -> dynes/cm2
  pnm = pint * 10. ! Pa -> dynes/cm2
  lnpmid = log(pmid)
  lnpint = log(pint)
  ! -- Define  water paths etc
  do k=1,pver
     if (ciwp(k) == -99.) then
        ! if cloud ice is missing value, then define fractional amount
        ! of cloud that is ice using code from CCM3.6
        if(t(k).gt.263.16) then
           fice(k) = 0.0 ! if warmer than -10 degrees C then water phase
        else if (t(k).le.263.16.and.t(k).ge.243.16) then
           fice(k) = (263.16-t(k)) / 20.0 !if colder than -10C but warmer than -30C mixed phase
        else
           fice(k) = 1.0 ! if colder than -30C then ice phase
        end if
        if (in_cld == 0) then ! water path input as grid avg
           cicewp(k) = clwp(k) / max(1.e-10,cldf(k)) * fice(k)
           cliqwp(k) = clwp(k) / max(1.e-10,cldf(k)) * (1.-fice(k))
        else ! water path input as in-cloud
           cicewp(k) = clwp(k)  * fice(k)
           cliqwp(k) = clwp(k)  * (1.-fice(k))
        end if
     else if (cldf(k) == 0.) then
        cicewp(k) = 1.e-16
        cliqwp(k) = 1.e-16
        fice(k) = 1.e-16
     else
        if (in_cld == 0) then ! water path input as grid avg
           cicewp(k) = ciwp(k) / max(0.01_r8,cldf(k))
           cliqwp(k) = clwp(k) / max(0.01_r8,cldf(k))
        else ! water path input as in-cloud
           cicewp(k) = ciwp(k)
           cliqwp(k) = clwp(k)
        end if
        fice(k) = cicewp(k)/max(1.e-10_r8,(cicewp(k)+cliqwp(k)))
     endif
  end do
  ! -- compute cloud emissivities
  call cldems(pcols, pver, pverp, lchnk, ncol, cicewp+cliqwp, fice, rei, emis)
  ! -- compute cloud overlap quantities
  call cldovrlap(pcols, pver, pverp, lchnk, ncol, pint, cldf, nmxrgn, pmxrgn)
  ! -- compute relative humidity
  call aqsat(t, pmid, esat, qsat, 1, 1, pver, 1, pver)
  rh = q / qsat *                         &
       ((1. - epsilo) * qsat + epsilo) /  &
       ((1. - epsilo) * q + epsilo)
  ! -- compute GHG mass mix ratios
  !    NOTE: in CliMT, assume trace gases are well mixed everywhere
  !    (CAM3 has vertical and meridional variation in stratosphere)
  !  climlab -- assume values are actual volume mixing ratios (ppp)
  !n2o = mwn2o/mwdry * n2ovmr * 1.e-6 ! ppm->ppp
  !ch4 = mwch4/mwdry * ch4vmr * 1.e-6
  !cfc11 = mwf11/mwdry * f11vmr * 1.e-6
  !cfc12 = mwf12/mwdry * f12vmr * 1.e-6
  n2o = mwn2o/mwdry * n2ovmr
  ch4 = mwch4/mwdry * ch4vmr
  cfc11 = mwf11/mwdry * f11vmr
  cfc12 = mwf12/mwdry * f12vmr
  ! -- set value of CO2 vol mix ratio (this is picked up later by radae)
  !call chem_surfvals_set_co2(co2vmr*1.e-6) ! ppm -> ppp
  call chem_surfvals_set_co2(co2vmr)

  ! Initialize rad routines
  call radsw_init(gravit)
  call radlw_init(gravit, stebol)
  call radae_init(gravit, epsilo, stebol, pstd, mwdry, mwco2, mwo3)

  ! Compute SW
  if (idosw == 1) then
     call radcswmx(pcols, pver, pverp,                  &
          lchnk   ,ncol    ,                            &
          pnm     ,pbr     ,q       ,rh      ,o3mmr,    &
          aerosol ,cldf    ,cicewp  ,cliqwp  ,rel     , &
          rei     ,eccf    ,coszrs  ,solincgs,solin_out,&
          asdir   ,asdif   ,aldir   ,aldif   ,nmxrgn  , &
          pmxrgn  ,qrs     ,fsnt    ,fsntc   ,fsntoa  , &
          fsntoac ,fsnirt  ,fsnrtc  ,fsnirtsq,fsns    , &
          fsnsc   ,fsdsc   ,fsds    ,sols    ,soll    , &
          solsd   ,solld   ,frc_day ,                   &
          aertau  ,aerssa  ,aerasm  ,aerfwd  ,swflx   , &
          fcns, swup, swdn, swupc, swdnc)
     swflx=swflx*1.e-3 ! CGS->MKS for output
     swflxc = fcns*1.e-3
     swup   = swup*1.e-3
     swdn   = swdn*1.e-3
     swupc  = swupc*1.e-3
     swdnc  = swdnc*1.e-3

     !qrs = qrs*1.e-3  ! qrs already in MKS units, see radsw.F90
     sw_cf_srf = (fsns(1) - fsnsc(1))*1.e-3
     sw_cf_toa = (fsnt(1) - fsntc(1))*1.e-3
     sw_toa = fsnt(1)*1.e-3
     sw_srf = fsns(1)*1.e-3
  else
     swflx = solin_in*(1.-asdir(1))
     swflxc = swflx
     qrs = 0.
     sw_cf_srf = 0.
     sw_cf_toa = 0.
     sw_toa = solin_in*(1.-asdir(1))
     sw_srf = solin_in*(1.-asdir(1))
  endif

  ! Compute LW
  if (idolw == 1) then
     call radclwmx(pcols, pver, pverp,                  &
          lchnk, ncol, doabsems,     &
          lwupcgs, t, q, o3mmr, pbr,             &
          pnm, lnpmid, lnpint, n2o, ch4,      &
          cfc11, cfc12, cldf, emis, pmxrgn,    &
          nmxrgn, qrl, flns, flnt, flnsc,     &
          flntc, flwds, flut, flutc, aerosol(:,1), &
          lwflx, fcnl,lwup,lwdn,lwupc,lwdnc)
     ! CGS->MKS for output;
     lwflx  = lwflx*1.e-3
     lwflxc = fcnl*1.e-3
     lwup   = lwup*1.e-3
     lwdn   = lwdn*1.e-3
     lwupc  = lwupc*1.e-3
     lwdnc  = lwdnc*1.e-3
     !qrl = qrl*1.e-3  ! qrl already in MKS units, see radlw.F90
     !  climlab -- do not change signs here
     lw_cf_srf = (flns(1) - flnsc(1))*1.e-3
     lw_cf_toa = (flnt(1) - flntc(1))*1.e-3
     lw_toa = flnt(1)*1.e-3
     lw_srf = flns(1)*1.e-3
  endif

end subroutine crm
