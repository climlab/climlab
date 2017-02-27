!  CLIMLAB CAM3 radiation driver
!    Brian Rose
!
!   adapting from Rodrigo Caballero's CliMT CAM3 driver
!  change units of q from g/kg to kg/kg
!  remove argument dt
!   remove output Tinc
!  Change units of outputs qrl, qrs, Tdot from J/kg/day to J/kg/s or W/kg
!  pass cosine of zenith angle rather than angle itself
!   and also pass eccentricity factor
!   and solar constant rather than insolation
!  Pass grid dimensions as input arguments rather than global variables
!  set at compile time
!
!  Return flux at layer interfaces rather than averaging to get flux at layer centers
!
!  Return clear-sky fluxes too

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
     eccf,    &
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
     swflxc_out,  &
     lwflx_out,   &
     lwflxc_out,  &
     sw_cf_toa,   &
     sw_cf_srf,   &
     lw_cf_toa,   &
     lw_cf_srf, &
     lw_toa,  &
     lw_srf,  &
     sw_toa,  &
     sw_srf, &
     swup_out, swdn_out, swupc_out, swdnc_out, &
     lwup_out, lwdn_out, lwupc_out, lwdnc_out)

! Input
  integer, intent(in) :: km,jm,im,idosw,idolw,in_cld
  real*8, intent(in) :: aldif(jm,im)
  real*8, intent(in) :: aldir(jm,im)
  real*8, intent(in) :: asdif(jm,im)
  real*8, intent(in) :: asdir(jm,im)
  real*8, intent(in) :: eccf
  real*8, intent(in) :: coszen(jm,im)
  real*8, intent(in) :: scon
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

! Output
  real*8, intent(out) :: tdot(km,jm,im)
  real*8, intent(out) :: qrs(km,jm,im)
  real*8, intent(out) :: qrl(km,jm,im)
  real*8, intent(out) :: swflx_out(km+1,jm,im)
  real*8, intent(out) :: swflxc_out(km+1,jm,im)
  real*8, intent(out) :: swup_out(km+1,jm,im)
  real*8, intent(out) :: swdn_out(km+1,jm,im)
  real*8, intent(out) :: swupc_out(km+1,jm,im)
  real*8, intent(out) :: swdnc_out(km+1,jm,im)
  real*8, intent(out) :: lwflx_out(km+1,jm,im)
  real*8, intent(out) :: lwflxc_out(km+1,jm,im)
  real*8, intent(out) :: lwup_out(km+1,jm,im)
  real*8, intent(out) :: lwdn_out(km+1,jm,im)
  real*8, intent(out) :: lwupc_out(km+1,jm,im)
  real*8, intent(out) :: lwdnc_out(km+1,jm,im)
  real*8, intent(out) :: sw_cf_toa(jm,im)
  real*8, intent(out) :: sw_cf_srf(jm,im)
  real*8, intent(out) :: lw_cf_toa(jm,im)
  real*8, intent(out) :: lw_cf_srf(jm,im)
  real*8, intent(out) :: lw_toa(jm,im)
  real*8, intent(out) :: lw_srf(jm,im)
  real*8, intent(out) :: sw_toa(jm,im)
  real*8, intent(out) :: sw_srf(jm,im)
  real*8, intent(out) :: srfflx(jm,im)

  !  These are not comments! Necessary directives to f2py to handle array dimensions
  !f2py depend(jm,im) aldif,aldir,asdif,asdir,coszen,flus,ps,tg
  !f2py depend(km,jm,im) cldf,clwp,ciwp,o3mmr,r_liq,r_ice,pmid,dp,q,t
  !f2py depend(jm,im) sw_cf_toa,sw_cf_srf,lw_cf_toa,lw_cf_srf,lw_toa,lw_srf,sw_toa,sw_srf,srfflx
  !f2py depend(km,jm,im) swflx_out,swflxc_out,lwflx_out,lwflxc_out
  !f2py depend(km,jm,im) swup_out,swdn_out,swupc_out,swdnc_out
  !f2py depend(km,jm,im) lwup_out,lwdn_out,lwupc_out,lwdnc_out
  !f2py depend(km,jm,im) tdot,qrs,qrl

! Local
  real*8 swflx(km+1)
  real*8 swflxc(km+1)
  real*8 swup(km+1)
  real*8 swdn(km+1)
  real*8 swupc(km+1)
  real*8 swdnc(km+1)
  real*8 lwflx(km+1)
  real*8 lwflxc(km+1)
  real*8 lwup(km+1)
  real*8 lwdn(km+1)
  real*8 lwupc(km+1)
  real*8 lwdnc(km+1)

  ! CLIMLAB: take this code from ppgrid.F90
  ! Grid point resolution parameters

     integer pcols      ! number of columns (max)
     integer pver       ! number of vertical levels
     integer pverp      ! pver + 1
     parameter (pcols  = 1)

!  CLIMLAB -- set grid dimensions
  pver = km
  pverp = pver + 1

  do i=1,im
     do j=1,jm
        call crm(  &
             pcols,  &
             pver,   &
             pverp,  &
             aldif(j,i),  &
             aldir(j,i),  &
             asdif(j,i),  &
             asdir(j,i),  &
             eccf,        &
             coszen(j,i), &
             scon,        &
             flus(j,i),   &
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
             swflxc, &
             lwflx,  &
             lwflxc, &
             sw_cf_toa(j,i),  &
             sw_cf_srf(j,i),  &
             lw_cf_toa(j,i),  &
             lw_cf_srf(j,i), &
             lw_toa(j,i),  &
             lw_srf(j,i),  &
             sw_toa(j,i),  &
             sw_srf(j,i), &
             swup, swdn, swupc, swdnc, &
             lwup,lwdn, lwupc, lwdnc &
             )

        !  CLIMLAB differs from CliMT here ... we will report the actual flux at interfaces
        swflx_out(:,j,i)  = swflx
        swflxc_out(:,j,i) = swflxc
        swup_out(:,j,i) = swup
        swdn_out(:,j,i) = swdn
        swupc_out(:,j,i) = swupc
        swdnc_out(:,j,i) = swdnc
        lwflx_out(:,j,i)  = lwflx
        lwflxc_out(:,j,i) = lwflxc
        lwup_out(:,j,i) = lwup
        lwdn_out(:,j,i) = lwdn
        lwupc_out(:,j,i) = lwupc
        lwdnc_out(:,j,i) = lwdnc

        srfflx(j,i) = sw_srf(j,i) - lw_srf(j,i)
     enddo
  enddo

  tdot   = qrs + qrl
  !tinc   = 2.*dt*tdot
  !qrs = qrs * 86400.
  !qrl = qrl * 86400.
  !tdot = tdot * 86400.

end subroutine driver
