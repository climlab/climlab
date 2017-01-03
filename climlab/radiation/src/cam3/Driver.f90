!  climlab
!   adapting the CliMT CAM3 driver
!  change units of q from g/kg to kg/kg
!  remove argument dt
!   remove output Tinc
!  Change units of outputs qrl, qrs, Tdot from J/kg/day to J/kg/s or W/kg
!  pass cosine of zenith angle rather than angle itself

subroutine driver(  &
     km,   &
     jm,   &
     im,   &
     idosw,   &
     idolw,   &
     pmid,   &
     dp,   &
     ps,   &
     t,   &
     tg,   &
     q,   &
     o3mmr,   &
     cldf,   &
     clwp,   &
     ciwp,   &
     in_cld,   &
     aldif,   &
     aldir,   &
     asdif,   &
     asdir,   &
     coszen,   &
     scon,   &
     flus,   &
     r_liq,   &
     r_ice,   &
     co2vmr,   &
     n2ovmr,   &
     ch4vmr,   &
     f11vmr,   &
     f12vmr,   &
     gravit,   &
     cpair,   &
     epsilo,   &
     stebol,   &
     tdot,   &
     srfflx,   &
     qrs,   &
     qrl,   &
     swflx_out,   &
     lwflx_out,   &
     sw_cf_toa,   &
     sw_cf_srf,   &
     lw_cf_toa,   &
     lw_cf_srf, &
     lw_toa,  &
     lw_srf,  &
     sw_toa,  &
     sw_srf, &
     lwup_out, lwdn_out)

!  climlab -- set grid dimension values here in the driver
  use ppgrid,              only: set_pver

! Input
  integer, intent(in) :: km,jm,im,idosw,idolw,in_cld
  real*8, intent(in) :: aldif(jm,im)
  real*8, intent(in) :: aldir(jm,im)
  real*8, intent(in) :: asdif(jm,im)
  real*8, intent(in) :: asdir(jm,im)
  real*8, intent(in) :: coszen(jm,im)
  real*8, intent(in) :: scon(jm,im)
  real*8, intent(in) :: flus(jm,im)
  real*8, intent(in) :: cldf(km,jm,im)
  real*8, intent(in) :: clwp(km,jm,im)
  real*8, intent(in) :: ciwp(km,jm,im)
  real*8, intent(in) :: o3mmr(km,jm,im)
  real*8, intent(in) :: r_liq(km,jm,im)
  real*8, intent(in) :: r_ice(km,jm,im)
  real*8, intent(in) :: pmid(km,jm,im)
  real*8, intent(in) :: dp(km,jm,im)
  real*8, intent(in) :: ps(jm,im)
  real*8, intent(in) :: q(km,jm,im)
  real*8, intent(in) :: tg(jm,im)
  real*8, intent(in) :: t(km,jm,im)
  real*8, intent(in) :: co2vmr
  real*8, intent(in) :: n2ovmr
  real*8, intent(in) :: ch4vmr
  real*8, intent(in) :: f11vmr
  real*8, intent(in) :: f12vmr
  real*8, intent(in) :: gravit
  real*8, intent(in) :: cpair
  real*8, intent(in) :: epsilo
  real*8, intent(in) :: stebol
  !f2py intent(in,hide)  km,jm,im

! Output
  real*8, intent(out) :: tdot(km,jm,im)
  real*8, intent(out) :: qrs(km,jm,im)
  real*8, intent(out) :: qrl(km,jm,im)
  real*8, intent(out) :: swflx_out(km,jm,im)
  real*8, intent(out) :: lwflx_out(km,jm,im)
  real*8, intent(out) :: lwup_out(km,jm,im)
  real*8, intent(out) :: lwdn_out(km,jm,im)
  real*8, intent(out) :: sw_cf_toa(jm,im)
  real*8, intent(out) :: sw_cf_srf(jm,im)
  real*8, intent(out) :: lw_cf_toa(jm,im)
  real*8, intent(out) :: lw_cf_srf(jm,im)
  real*8, intent(out) :: lw_toa(jm,im)
  real*8, intent(out) :: lw_srf(jm,im)
  real*8, intent(out) :: sw_toa(jm,im)
  real*8, intent(out) :: sw_srf(jm,im)
  real*8, intent(out) :: srfflx(jm,im)

! Local
  real*8 swflx(km+1)
  real*8 lwflx(km+1)
  real*8 lwup(km+1)
  real*8 lwdn(km+1)

!  climlab -- set grid dimensions
  call set_pver(km)

  do i=1,im
     do j=1,jm
        call crm(  &
             aldif(j,i),  &
             aldir(j,i),  &
             asdif(j,i),  &
             asdir(j,i),  &
             coszen(j,i),  &
             scon(j,i),  &
             flus(j,i),  &
             cldf(1,j,i),  &
             clwp(1,j,i),  &
             ciwp(1,j,i),  &
             in_cld, &
             o3mmr(1,j,i),  &
             r_liq(1,j,i),  &
             r_ice(1,j,i),  &
             pmid(1,j,i),  &
             dp(1,j,i),  &
             ps(j,i),  &
             q(1,j,i),  &
             tg(j,i),  &
             t(1,j,i),  &
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
             qrs(1,j,i),  &
             qrl(1,j,i),  &
             swflx,  &
             lwflx,  &
             sw_cf_toa(j,i),  &
             sw_cf_srf(j,i),  &
             lw_cf_toa(j,i),  &
             lw_cf_srf(j,i), &
             lw_toa(j,i),  &
             lw_srf(j,i),  &
             sw_toa(j,i),  &
             sw_srf(j,i), &
             lwup,lwdn &
             )

        swflx_out(:,j,i) = (swflx(1:km)+swflx(2:km+1))/2.
        lwflx_out(:,j,i) = (lwflx(1:km)+lwflx(2:km+1))/2.
        lwup_out(:,j,i) = (lwup(1:km)+lwup(2:km+1))/2.
        lwdn_out(:,j,i) = (lwdn(1:km)+lwdn(2:km+1))/2.
        srfflx(j,i) = sw_srf(j,i) + lw_srf(j,i)
     enddo
  enddo

  tdot   = qrs + qrl
  !tinc   = 2.*dt*tdot
  !qrs = qrs * 86400.
  !qrl = qrl * 86400.
  !tdot = tdot * 86400.

end subroutine driver






!integer function get_nlev()
!
!  integer get_km
!  external get_km
!
!  get_nlev = get_km()
!
!end function get_nlev
