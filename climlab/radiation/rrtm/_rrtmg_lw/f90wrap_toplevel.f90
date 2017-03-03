subroutine f90wrap_driver(ncol, nlay, icld, permuteseed, irng, idrv, cpdair, &
    play, plev, tlay, tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, &
    cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw, &
    cldfrac, ciwp, clwp, reic, relq, tauc, tauaer, uflx, dflx, hr, uflxc, dflxc, &
    hrc, duflx_dt, duflxc_dt, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, &
    n12, n13, n14, n15, n16, n17, n18, n19, n20, n21, n22, n23, n24, n25, n26, &
    n27, n28, n29, n30, n31, n32, n33, n34, n35, n36, n37, n38, n39, n40, n41, &
    n42, n43, n44, n45, n46, n47, n48, n49, n50, n51, n52, n53, n54, n55, n56, &
    n57, n58, n59, n60, n61, n62)
    implicit none
    external driver
    
    integer(4), intent(in) :: ncol
    integer(4), intent(in) :: nlay
    integer(4), intent(inout) :: icld
    integer(4), intent(in) :: permuteseed
    integer(4), intent(inout) :: irng
    integer(4), intent(in) :: idrv
    real(4), intent(in) :: cpdair
    real(4), intent(in), dimension(n0,n1) :: play
    real(4), intent(in), dimension(n2,n3) :: plev
    real(4), intent(in), dimension(n4,n5) :: tlay
    real(4), intent(in), dimension(n6,n7) :: tlev
    real(4), intent(in), dimension(n8) :: tsfc
    real(4), intent(in), dimension(n9,n10) :: h2ovmr
    real(4), intent(in), dimension(n11,n12) :: o3vmr
    real(4), intent(in), dimension(n13,n14) :: co2vmr
    real(4), intent(in), dimension(n15,n16) :: ch4vmr
    real(4), intent(in), dimension(n17,n18) :: n2ovmr
    real(4), intent(in), dimension(n19,n20) :: o2vmr
    real(4), intent(in), dimension(n21,n22) :: cfc11vmr
    real(4), intent(in), dimension(n23,n24) :: cfc12vmr
    real(4), intent(in), dimension(n25,n26) :: cfc22vmr
    real(4), intent(in), dimension(n27,n28) :: ccl4vmr
    real(4), intent(in), dimension(n29,n30) :: emis
    integer(4), intent(in) :: inflglw
    integer(4), intent(in) :: iceflglw
    integer(4), intent(in) :: liqflglw
    real(4), intent(in), dimension(n31,n32) :: cldfrac
    real(4), intent(in), dimension(n33,n34) :: ciwp
    real(4), intent(in), dimension(n35,n36) :: clwp
    real(4), intent(in), dimension(n37,n38) :: reic
    real(4), intent(in), dimension(n39,n40) :: relq
    real(4), intent(in), dimension(n41,n42,n43) :: tauc
    real(4), intent(in), dimension(n44,n45,n46) :: tauaer
    real(4), intent(inout), dimension(n47,n48) :: uflx
    real(4), intent(inout), dimension(n49,n50) :: dflx
    real(4), intent(inout), dimension(n51,n52) :: hr
    real(4), intent(inout), dimension(n53,n54) :: uflxc
    real(4), intent(inout), dimension(n55,n56) :: dflxc
    real(4), intent(inout), dimension(n57,n58) :: hrc
    real(4), intent(inout), dimension(n59,n60) :: duflx_dt
    real(4), intent(inout), dimension(n61,n62) :: duflxc_dt
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
    !f2py intent(hide), depend(cldfrac) :: n31 = shape(cldfrac,0)
    integer :: n32
    !f2py intent(hide), depend(cldfrac) :: n32 = shape(cldfrac,1)
    integer :: n33
    !f2py intent(hide), depend(ciwp) :: n33 = shape(ciwp,0)
    integer :: n34
    !f2py intent(hide), depend(ciwp) :: n34 = shape(ciwp,1)
    integer :: n35
    !f2py intent(hide), depend(clwp) :: n35 = shape(clwp,0)
    integer :: n36
    !f2py intent(hide), depend(clwp) :: n36 = shape(clwp,1)
    integer :: n37
    !f2py intent(hide), depend(reic) :: n37 = shape(reic,0)
    integer :: n38
    !f2py intent(hide), depend(reic) :: n38 = shape(reic,1)
    integer :: n39
    !f2py intent(hide), depend(relq) :: n39 = shape(relq,0)
    integer :: n40
    !f2py intent(hide), depend(relq) :: n40 = shape(relq,1)
    integer :: n41
    !f2py intent(hide), depend(tauc) :: n41 = shape(tauc,0)
    integer :: n42
    !f2py intent(hide), depend(tauc) :: n42 = shape(tauc,1)
    integer :: n43
    !f2py intent(hide), depend(tauc) :: n43 = shape(tauc,2)
    integer :: n44
    !f2py intent(hide), depend(tauaer) :: n44 = shape(tauaer,0)
    integer :: n45
    !f2py intent(hide), depend(tauaer) :: n45 = shape(tauaer,1)
    integer :: n46
    !f2py intent(hide), depend(tauaer) :: n46 = shape(tauaer,2)
    integer :: n47
    !f2py intent(hide), depend(uflx) :: n47 = shape(uflx,0)
    integer :: n48
    !f2py intent(hide), depend(uflx) :: n48 = shape(uflx,1)
    integer :: n49
    !f2py intent(hide), depend(dflx) :: n49 = shape(dflx,0)
    integer :: n50
    !f2py intent(hide), depend(dflx) :: n50 = shape(dflx,1)
    integer :: n51
    !f2py intent(hide), depend(hr) :: n51 = shape(hr,0)
    integer :: n52
    !f2py intent(hide), depend(hr) :: n52 = shape(hr,1)
    integer :: n53
    !f2py intent(hide), depend(uflxc) :: n53 = shape(uflxc,0)
    integer :: n54
    !f2py intent(hide), depend(uflxc) :: n54 = shape(uflxc,1)
    integer :: n55
    !f2py intent(hide), depend(dflxc) :: n55 = shape(dflxc,0)
    integer :: n56
    !f2py intent(hide), depend(dflxc) :: n56 = shape(dflxc,1)
    integer :: n57
    !f2py intent(hide), depend(hrc) :: n57 = shape(hrc,0)
    integer :: n58
    !f2py intent(hide), depend(hrc) :: n58 = shape(hrc,1)
    integer :: n59
    !f2py intent(hide), depend(duflx_dt) :: n59 = shape(duflx_dt,0)
    integer :: n60
    !f2py intent(hide), depend(duflx_dt) :: n60 = shape(duflx_dt,1)
    integer :: n61
    !f2py intent(hide), depend(duflxc_dt) :: n61 = shape(duflxc_dt,0)
    integer :: n62
    !f2py intent(hide), depend(duflxc_dt) :: n62 = shape(duflxc_dt,1)
    call driver(ncol, nlay, icld, permuteseed, irng, idrv, cpdair, play, plev, tlay, &
        tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, &
        cfc12vmr, cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw, cldfrac, &
        ciwp, clwp, reic, relq, tauc, tauaer, uflx, dflx, hr, uflxc, dflxc, hrc, &
        duflx_dt, duflxc_dt)
end subroutine f90wrap_driver

