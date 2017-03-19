! Module rrtmg_lw_rad defined in file rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90

subroutine f90wrap_rrtmg_lw(ncol, nlay, icld, idrv, cpdair, play, plev, tlay, &
    tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, &
    cfc12vmr, cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw, cldfmcl, &
    taucmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, tauaer, uflx, dflx, hr, uflxc, &
    dflxc, hrc, duflx_dt, duflxc_dt, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, &
    n10, n11, n12, n13, n14, n15, n16, n17, n18, n19, n20, n21, n22, n23, n24, &
    n25, n26, n27, n28, n29, n30, n31, n32, n33, n34, n35, n36, n37, n38, n39, &
    n40, n41, n42, n43, n44, n45, n46, n47, n48, n49, n50, n51, n52, n53, n54, &
    n55, n56, n57, n58, n59, n60, n61, n62, n63, n64, n65)
    use rrtmg_lw_rad, only: rrtmg_lw
    implicit none
    
    integer(4), intent(in) :: ncol
    integer(4), intent(in) :: nlay
    integer(4), intent(inout) :: icld
    integer(4), intent(in) :: idrv
    real(8), intent(in) :: cpdair
    real(8), intent(in), dimension(n0,n1) :: play
    real(8), intent(in), dimension(n2,n3) :: plev
    real(8), intent(in), dimension(n4,n5) :: tlay
    real(8), intent(in), dimension(n6,n7) :: tlev
    real(8), intent(in), dimension(n8) :: tsfc
    real(8), intent(in), dimension(n9,n10) :: h2ovmr
    real(8), intent(in), dimension(n11,n12) :: o3vmr
    real(8), intent(in), dimension(n13,n14) :: co2vmr
    real(8), intent(in), dimension(n15,n16) :: ch4vmr
    real(8), intent(in), dimension(n17,n18) :: n2ovmr
    real(8), intent(in), dimension(n19,n20) :: o2vmr
    real(8), intent(in), dimension(n21,n22) :: cfc11vmr
    real(8), intent(in), dimension(n23,n24) :: cfc12vmr
    real(8), intent(in), dimension(n25,n26) :: cfc22vmr
    real(8), intent(in), dimension(n27,n28) :: ccl4vmr
    real(8), intent(in), dimension(n29,n30) :: emis
    integer(4), intent(in) :: inflglw
    integer(4), intent(in) :: iceflglw
    integer(4), intent(in) :: liqflglw
    real(8), intent(in), dimension(n31,n32,n33) :: cldfmcl
    real(8), intent(in), dimension(n34,n35,n36) :: taucmcl
    real(8), intent(in), dimension(n37,n38,n39) :: ciwpmcl
    real(8), intent(in), dimension(n40,n41,n42) :: clwpmcl
    real(8), intent(in), dimension(n43,n44) :: reicmcl
    real(8), intent(in), dimension(n45,n46) :: relqmcl
    real(8), intent(in), dimension(n47,n48,n49) :: tauaer
    real(8), intent(inout), dimension(n50,n51) :: uflx
    real(8), intent(inout), dimension(n52,n53) :: dflx
    real(8), intent(inout), dimension(n54,n55) :: hr
    real(8), intent(inout), dimension(n56,n57) :: uflxc
    real(8), intent(inout), dimension(n58,n59) :: dflxc
    real(8), intent(inout), dimension(n60,n61) :: hrc
    real(8), optional, intent(inout), dimension(n62,n63) :: duflx_dt
    real(8), optional, intent(inout), dimension(n64,n65) :: duflxc_dt
    integer :: n0
    !f2py intent(hide), depend(play) :: n0 = shape(play,0)
    integer :: n1
    !f2py intent(hide), depend(play) :: n1 = shape(play,1)
    integer :: n2
    !f2py intent(hide), depend(plev) :: n2 = shape(plev,0)
    integer :: n3
    !f2py intent(hide), depend(plev) :: n3 = shape(plev,1)
    integer :: n4
    !f2py intent(hide), depend(tlay) :: n4 = shape(tlay,0)
    integer :: n5
    !f2py intent(hide), depend(tlay) :: n5 = shape(tlay,1)
    integer :: n6
    !f2py intent(hide), depend(tlev) :: n6 = shape(tlev,0)
    integer :: n7
    !f2py intent(hide), depend(tlev) :: n7 = shape(tlev,1)
    integer :: n8
    !f2py intent(hide), depend(tsfc) :: n8 = shape(tsfc,0)
    integer :: n9
    !f2py intent(hide), depend(h2ovmr) :: n9 = shape(h2ovmr,0)
    integer :: n10
    !f2py intent(hide), depend(h2ovmr) :: n10 = shape(h2ovmr,1)
    integer :: n11
    !f2py intent(hide), depend(o3vmr) :: n11 = shape(o3vmr,0)
    integer :: n12
    !f2py intent(hide), depend(o3vmr) :: n12 = shape(o3vmr,1)
    integer :: n13
    !f2py intent(hide), depend(co2vmr) :: n13 = shape(co2vmr,0)
    integer :: n14
    !f2py intent(hide), depend(co2vmr) :: n14 = shape(co2vmr,1)
    integer :: n15
    !f2py intent(hide), depend(ch4vmr) :: n15 = shape(ch4vmr,0)
    integer :: n16
    !f2py intent(hide), depend(ch4vmr) :: n16 = shape(ch4vmr,1)
    integer :: n17
    !f2py intent(hide), depend(n2ovmr) :: n17 = shape(n2ovmr,0)
    integer :: n18
    !f2py intent(hide), depend(n2ovmr) :: n18 = shape(n2ovmr,1)
    integer :: n19
    !f2py intent(hide), depend(o2vmr) :: n19 = shape(o2vmr,0)
    integer :: n20
    !f2py intent(hide), depend(o2vmr) :: n20 = shape(o2vmr,1)
    integer :: n21
    !f2py intent(hide), depend(cfc11vmr) :: n21 = shape(cfc11vmr,0)
    integer :: n22
    !f2py intent(hide), depend(cfc11vmr) :: n22 = shape(cfc11vmr,1)
    integer :: n23
    !f2py intent(hide), depend(cfc12vmr) :: n23 = shape(cfc12vmr,0)
    integer :: n24
    !f2py intent(hide), depend(cfc12vmr) :: n24 = shape(cfc12vmr,1)
    integer :: n25
    !f2py intent(hide), depend(cfc22vmr) :: n25 = shape(cfc22vmr,0)
    integer :: n26
    !f2py intent(hide), depend(cfc22vmr) :: n26 = shape(cfc22vmr,1)
    integer :: n27
    !f2py intent(hide), depend(ccl4vmr) :: n27 = shape(ccl4vmr,0)
    integer :: n28
    !f2py intent(hide), depend(ccl4vmr) :: n28 = shape(ccl4vmr,1)
    integer :: n29
    !f2py intent(hide), depend(emis) :: n29 = shape(emis,0)
    integer :: n30
    !f2py intent(hide), depend(emis) :: n30 = shape(emis,1)
    integer :: n31
    !f2py intent(hide), depend(cldfmcl) :: n31 = shape(cldfmcl,0)
    integer :: n32
    !f2py intent(hide), depend(cldfmcl) :: n32 = shape(cldfmcl,1)
    integer :: n33
    !f2py intent(hide), depend(cldfmcl) :: n33 = shape(cldfmcl,2)
    integer :: n34
    !f2py intent(hide), depend(taucmcl) :: n34 = shape(taucmcl,0)
    integer :: n35
    !f2py intent(hide), depend(taucmcl) :: n35 = shape(taucmcl,1)
    integer :: n36
    !f2py intent(hide), depend(taucmcl) :: n36 = shape(taucmcl,2)
    integer :: n37
    !f2py intent(hide), depend(ciwpmcl) :: n37 = shape(ciwpmcl,0)
    integer :: n38
    !f2py intent(hide), depend(ciwpmcl) :: n38 = shape(ciwpmcl,1)
    integer :: n39
    !f2py intent(hide), depend(ciwpmcl) :: n39 = shape(ciwpmcl,2)
    integer :: n40
    !f2py intent(hide), depend(clwpmcl) :: n40 = shape(clwpmcl,0)
    integer :: n41
    !f2py intent(hide), depend(clwpmcl) :: n41 = shape(clwpmcl,1)
    integer :: n42
    !f2py intent(hide), depend(clwpmcl) :: n42 = shape(clwpmcl,2)
    integer :: n43
    !f2py intent(hide), depend(reicmcl) :: n43 = shape(reicmcl,0)
    integer :: n44
    !f2py intent(hide), depend(reicmcl) :: n44 = shape(reicmcl,1)
    integer :: n45
    !f2py intent(hide), depend(relqmcl) :: n45 = shape(relqmcl,0)
    integer :: n46
    !f2py intent(hide), depend(relqmcl) :: n46 = shape(relqmcl,1)
    integer :: n47
    !f2py intent(hide), depend(tauaer) :: n47 = shape(tauaer,0)
    integer :: n48
    !f2py intent(hide), depend(tauaer) :: n48 = shape(tauaer,1)
    integer :: n49
    !f2py intent(hide), depend(tauaer) :: n49 = shape(tauaer,2)
    integer :: n50
    !f2py intent(hide), depend(uflx) :: n50 = shape(uflx,0)
    integer :: n51
    !f2py intent(hide), depend(uflx) :: n51 = shape(uflx,1)
    integer :: n52
    !f2py intent(hide), depend(dflx) :: n52 = shape(dflx,0)
    integer :: n53
    !f2py intent(hide), depend(dflx) :: n53 = shape(dflx,1)
    integer :: n54
    !f2py intent(hide), depend(hr) :: n54 = shape(hr,0)
    integer :: n55
    !f2py intent(hide), depend(hr) :: n55 = shape(hr,1)
    integer :: n56
    !f2py intent(hide), depend(uflxc) :: n56 = shape(uflxc,0)
    integer :: n57
    !f2py intent(hide), depend(uflxc) :: n57 = shape(uflxc,1)
    integer :: n58
    !f2py intent(hide), depend(dflxc) :: n58 = shape(dflxc,0)
    integer :: n59
    !f2py intent(hide), depend(dflxc) :: n59 = shape(dflxc,1)
    integer :: n60
    !f2py intent(hide), depend(hrc) :: n60 = shape(hrc,0)
    integer :: n61
    !f2py intent(hide), depend(hrc) :: n61 = shape(hrc,1)
    integer :: n62
    !f2py intent(hide), depend(duflx_dt) :: n62 = shape(duflx_dt,0)
    integer :: n63
    !f2py intent(hide), depend(duflx_dt) :: n63 = shape(duflx_dt,1)
    integer :: n64
    !f2py intent(hide), depend(duflxc_dt) :: n64 = shape(duflxc_dt,0)
    integer :: n65
    !f2py intent(hide), depend(duflxc_dt) :: n65 = shape(duflxc_dt,1)
    call rrtmg_lw(ncol=ncol, nlay=nlay, icld=icld, idrv=idrv, cpdair=cpdair, &
        play=play, plev=plev, tlay=tlay, tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, &
        o3vmr=o3vmr, co2vmr=co2vmr, ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, &
        cfc11vmr=cfc11vmr, cfc12vmr=cfc12vmr, cfc22vmr=cfc22vmr, ccl4vmr=ccl4vmr, &
        emis=emis, inflglw=inflglw, iceflglw=iceflglw, liqflglw=liqflglw, &
        cldfmcl=cldfmcl, taucmcl=taucmcl, ciwpmcl=ciwpmcl, clwpmcl=clwpmcl, &
        reicmcl=reicmcl, relqmcl=relqmcl, tauaer=tauaer, uflx=uflx, dflx=dflx, &
        hr=hr, uflxc=uflxc, dflxc=dflxc, hrc=hrc, duflx_dt=duflx_dt, &
        duflxc_dt=duflxc_dt)
end subroutine f90wrap_rrtmg_lw

subroutine f90wrap_inatm(iplon, nlay, icld, iaer, play, plev, tlay, tlev, tsfc, &
    h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, &
    ccl4vmr, emis, inflglw, iceflglw, liqflglw, cldfmcl, taucmcl, ciwpmcl, &
    clwpmcl, reicmcl, relqmcl, tauaer, nlayers, pavel, pz, tavel, tz, tbound, &
    semiss, coldry, wkl, wbrodl, wx, pwvcm, inflag, iceflag, liqflag, cldfmc, &
    taucmc, ciwpmc, clwpmc, reicmc, relqmc, taua, n0, n1, n2, n3, n4, n5, n6, &
    n7, n8, n9, n10, n11, n12, n13, n14, n15, n16, n17, n18, n19, n20, n21, n22, &
    n23, n24, n25, n26, n27, n28, n29, n30, n31, n32, n33, n34, n35, n36, n37, &
    n38, n39, n40, n41, n42, n43, n44, n45, n46, n47, n48, n49, n50, n51, n52, &
    n53, n54, n55, n56, n57, n58, n59, n60, n61, n62, n63, n64, n65, n66, n67, &
    n68, n69, n70, n71, n72)
    use rrtmg_lw_rad, only: inatm
    implicit none
    
    integer(4), intent(in) :: iplon
    integer(4), intent(in) :: nlay
    integer(4), intent(in) :: icld
    integer(4), intent(in) :: iaer
    real(8), intent(in), dimension(n0,n1) :: play
    real(8), intent(in), dimension(n2,n3) :: plev
    real(8), intent(in), dimension(n4,n5) :: tlay
    real(8), intent(in), dimension(n6,n7) :: tlev
    real(8), intent(in), dimension(n8) :: tsfc
    real(8), intent(in), dimension(n9,n10) :: h2ovmr
    real(8), intent(in), dimension(n11,n12) :: o3vmr
    real(8), intent(in), dimension(n13,n14) :: co2vmr
    real(8), intent(in), dimension(n15,n16) :: ch4vmr
    real(8), intent(in), dimension(n17,n18) :: n2ovmr
    real(8), intent(in), dimension(n19,n20) :: o2vmr
    real(8), intent(in), dimension(n21,n22) :: cfc11vmr
    real(8), intent(in), dimension(n23,n24) :: cfc12vmr
    real(8), intent(in), dimension(n25,n26) :: cfc22vmr
    real(8), intent(in), dimension(n27,n28) :: ccl4vmr
    real(8), intent(in), dimension(n29,n30) :: emis
    integer(4), intent(in) :: inflglw
    integer(4), intent(in) :: iceflglw
    integer(4), intent(in) :: liqflglw
    real(8), intent(in), dimension(n31,n32,n33) :: cldfmcl
    real(8), intent(in), dimension(n34,n35,n36) :: taucmcl
    real(8), intent(in), dimension(n37,n38,n39) :: ciwpmcl
    real(8), intent(in), dimension(n40,n41,n42) :: clwpmcl
    real(8), intent(in), dimension(n43,n44) :: reicmcl
    real(8), intent(in), dimension(n45,n46) :: relqmcl
    real(8), intent(in), dimension(n47,n48,n49) :: tauaer
    integer(4), intent(out) :: nlayers
    real(8), intent(inout), dimension(n50) :: pavel
    real(8), intent(inout), dimension(n51) :: pz
    real(8), intent(inout), dimension(n52) :: tavel
    real(8), intent(inout), dimension(n53) :: tz
    real(8), intent(out) :: tbound
    real(8), intent(inout), dimension(n54) :: semiss
    real(8), intent(inout), dimension(n55) :: coldry
    real(8), intent(inout), dimension(n56,n57) :: wkl
    real(8), intent(inout), dimension(n58) :: wbrodl
    real(8), intent(inout), dimension(n59,n60) :: wx
    real(8), intent(out) :: pwvcm
    integer(4), intent(out) :: inflag
    integer(4), intent(out) :: iceflag
    integer(4), intent(out) :: liqflag
    real(8), intent(inout), dimension(n61,n62) :: cldfmc
    real(8), intent(inout), dimension(n63,n64) :: taucmc
    real(8), intent(inout), dimension(n65,n66) :: ciwpmc
    real(8), intent(inout), dimension(n67,n68) :: clwpmc
    real(8), intent(inout), dimension(n69) :: reicmc
    real(8), intent(inout), dimension(n70) :: relqmc
    real(8), intent(inout), dimension(n71,n72) :: taua
    integer :: n0
    !f2py intent(hide), depend(play) :: n0 = shape(play,0)
    integer :: n1
    !f2py intent(hide), depend(play) :: n1 = shape(play,1)
    integer :: n2
    !f2py intent(hide), depend(plev) :: n2 = shape(plev,0)
    integer :: n3
    !f2py intent(hide), depend(plev) :: n3 = shape(plev,1)
    integer :: n4
    !f2py intent(hide), depend(tlay) :: n4 = shape(tlay,0)
    integer :: n5
    !f2py intent(hide), depend(tlay) :: n5 = shape(tlay,1)
    integer :: n6
    !f2py intent(hide), depend(tlev) :: n6 = shape(tlev,0)
    integer :: n7
    !f2py intent(hide), depend(tlev) :: n7 = shape(tlev,1)
    integer :: n8
    !f2py intent(hide), depend(tsfc) :: n8 = shape(tsfc,0)
    integer :: n9
    !f2py intent(hide), depend(h2ovmr) :: n9 = shape(h2ovmr,0)
    integer :: n10
    !f2py intent(hide), depend(h2ovmr) :: n10 = shape(h2ovmr,1)
    integer :: n11
    !f2py intent(hide), depend(o3vmr) :: n11 = shape(o3vmr,0)
    integer :: n12
    !f2py intent(hide), depend(o3vmr) :: n12 = shape(o3vmr,1)
    integer :: n13
    !f2py intent(hide), depend(co2vmr) :: n13 = shape(co2vmr,0)
    integer :: n14
    !f2py intent(hide), depend(co2vmr) :: n14 = shape(co2vmr,1)
    integer :: n15
    !f2py intent(hide), depend(ch4vmr) :: n15 = shape(ch4vmr,0)
    integer :: n16
    !f2py intent(hide), depend(ch4vmr) :: n16 = shape(ch4vmr,1)
    integer :: n17
    !f2py intent(hide), depend(n2ovmr) :: n17 = shape(n2ovmr,0)
    integer :: n18
    !f2py intent(hide), depend(n2ovmr) :: n18 = shape(n2ovmr,1)
    integer :: n19
    !f2py intent(hide), depend(o2vmr) :: n19 = shape(o2vmr,0)
    integer :: n20
    !f2py intent(hide), depend(o2vmr) :: n20 = shape(o2vmr,1)
    integer :: n21
    !f2py intent(hide), depend(cfc11vmr) :: n21 = shape(cfc11vmr,0)
    integer :: n22
    !f2py intent(hide), depend(cfc11vmr) :: n22 = shape(cfc11vmr,1)
    integer :: n23
    !f2py intent(hide), depend(cfc12vmr) :: n23 = shape(cfc12vmr,0)
    integer :: n24
    !f2py intent(hide), depend(cfc12vmr) :: n24 = shape(cfc12vmr,1)
    integer :: n25
    !f2py intent(hide), depend(cfc22vmr) :: n25 = shape(cfc22vmr,0)
    integer :: n26
    !f2py intent(hide), depend(cfc22vmr) :: n26 = shape(cfc22vmr,1)
    integer :: n27
    !f2py intent(hide), depend(ccl4vmr) :: n27 = shape(ccl4vmr,0)
    integer :: n28
    !f2py intent(hide), depend(ccl4vmr) :: n28 = shape(ccl4vmr,1)
    integer :: n29
    !f2py intent(hide), depend(emis) :: n29 = shape(emis,0)
    integer :: n30
    !f2py intent(hide), depend(emis) :: n30 = shape(emis,1)
    integer :: n31
    !f2py intent(hide), depend(cldfmcl) :: n31 = shape(cldfmcl,0)
    integer :: n32
    !f2py intent(hide), depend(cldfmcl) :: n32 = shape(cldfmcl,1)
    integer :: n33
    !f2py intent(hide), depend(cldfmcl) :: n33 = shape(cldfmcl,2)
    integer :: n34
    !f2py intent(hide), depend(taucmcl) :: n34 = shape(taucmcl,0)
    integer :: n35
    !f2py intent(hide), depend(taucmcl) :: n35 = shape(taucmcl,1)
    integer :: n36
    !f2py intent(hide), depend(taucmcl) :: n36 = shape(taucmcl,2)
    integer :: n37
    !f2py intent(hide), depend(ciwpmcl) :: n37 = shape(ciwpmcl,0)
    integer :: n38
    !f2py intent(hide), depend(ciwpmcl) :: n38 = shape(ciwpmcl,1)
    integer :: n39
    !f2py intent(hide), depend(ciwpmcl) :: n39 = shape(ciwpmcl,2)
    integer :: n40
    !f2py intent(hide), depend(clwpmcl) :: n40 = shape(clwpmcl,0)
    integer :: n41
    !f2py intent(hide), depend(clwpmcl) :: n41 = shape(clwpmcl,1)
    integer :: n42
    !f2py intent(hide), depend(clwpmcl) :: n42 = shape(clwpmcl,2)
    integer :: n43
    !f2py intent(hide), depend(reicmcl) :: n43 = shape(reicmcl,0)
    integer :: n44
    !f2py intent(hide), depend(reicmcl) :: n44 = shape(reicmcl,1)
    integer :: n45
    !f2py intent(hide), depend(relqmcl) :: n45 = shape(relqmcl,0)
    integer :: n46
    !f2py intent(hide), depend(relqmcl) :: n46 = shape(relqmcl,1)
    integer :: n47
    !f2py intent(hide), depend(tauaer) :: n47 = shape(tauaer,0)
    integer :: n48
    !f2py intent(hide), depend(tauaer) :: n48 = shape(tauaer,1)
    integer :: n49
    !f2py intent(hide), depend(tauaer) :: n49 = shape(tauaer,2)
    integer :: n50
    !f2py intent(hide), depend(pavel) :: n50 = shape(pavel,0)
    integer :: n51
    !f2py intent(hide), depend(pz) :: n51 = shape(pz,0)
    integer :: n52
    !f2py intent(hide), depend(tavel) :: n52 = shape(tavel,0)
    integer :: n53
    !f2py intent(hide), depend(tz) :: n53 = shape(tz,0)
    integer :: n54
    !f2py intent(hide), depend(semiss) :: n54 = shape(semiss,0)
    integer :: n55
    !f2py intent(hide), depend(coldry) :: n55 = shape(coldry,0)
    integer :: n56
    !f2py intent(hide), depend(wkl) :: n56 = shape(wkl,0)
    integer :: n57
    !f2py intent(hide), depend(wkl) :: n57 = shape(wkl,1)
    integer :: n58
    !f2py intent(hide), depend(wbrodl) :: n58 = shape(wbrodl,0)
    integer :: n59
    !f2py intent(hide), depend(wx) :: n59 = shape(wx,0)
    integer :: n60
    !f2py intent(hide), depend(wx) :: n60 = shape(wx,1)
    integer :: n61
    !f2py intent(hide), depend(cldfmc) :: n61 = shape(cldfmc,0)
    integer :: n62
    !f2py intent(hide), depend(cldfmc) :: n62 = shape(cldfmc,1)
    integer :: n63
    !f2py intent(hide), depend(taucmc) :: n63 = shape(taucmc,0)
    integer :: n64
    !f2py intent(hide), depend(taucmc) :: n64 = shape(taucmc,1)
    integer :: n65
    !f2py intent(hide), depend(ciwpmc) :: n65 = shape(ciwpmc,0)
    integer :: n66
    !f2py intent(hide), depend(ciwpmc) :: n66 = shape(ciwpmc,1)
    integer :: n67
    !f2py intent(hide), depend(clwpmc) :: n67 = shape(clwpmc,0)
    integer :: n68
    !f2py intent(hide), depend(clwpmc) :: n68 = shape(clwpmc,1)
    integer :: n69
    !f2py intent(hide), depend(reicmc) :: n69 = shape(reicmc,0)
    integer :: n70
    !f2py intent(hide), depend(relqmc) :: n70 = shape(relqmc,0)
    integer :: n71
    !f2py intent(hide), depend(taua) :: n71 = shape(taua,0)
    integer :: n72
    !f2py intent(hide), depend(taua) :: n72 = shape(taua,1)
    call inatm(iplon=iplon, nlay=nlay, icld=icld, iaer=iaer, play=play, plev=plev, &
        tlay=tlay, tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, o3vmr=o3vmr, co2vmr=co2vmr, &
        ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, cfc11vmr=cfc11vmr, &
        cfc12vmr=cfc12vmr, cfc22vmr=cfc22vmr, ccl4vmr=ccl4vmr, emis=emis, &
        inflglw=inflglw, iceflglw=iceflglw, liqflglw=liqflglw, cldfmcl=cldfmcl, &
        taucmcl=taucmcl, ciwpmcl=ciwpmcl, clwpmcl=clwpmcl, reicmcl=reicmcl, &
        relqmcl=relqmcl, tauaer=tauaer, nlayers=nlayers, pavel=pavel, pz=pz, &
        tavel=tavel, tz=tz, tbound=tbound, semiss=semiss, coldry=coldry, wkl=wkl, &
        wbrodl=wbrodl, wx=wx, pwvcm=pwvcm, inflag=inflag, iceflag=iceflag, &
        liqflag=liqflag, cldfmc=cldfmc, taucmc=taucmc, ciwpmc=ciwpmc, clwpmc=clwpmc, &
        reicmc=reicmc, relqmc=relqmc, taua=taua)
end subroutine f90wrap_inatm

! End of module rrtmg_lw_rad defined in file rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90

