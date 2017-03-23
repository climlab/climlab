! Module rrtmg_sw_rad defined in file rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_rad.f90

subroutine f90wrap_rrtmg_sw(ncol, nlay, icld, iaer, play, plev, tlay, tlev, &
    tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, asdir, asdif, aldir, &
    aldif, coszen, adjes, dyofyr, scon, isolvar, inflgsw, iceflgsw, liqflgsw, &
    cldfmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl, ciwpmcl, clwpmcl, reicmcl, &
    relqmcl, tauaer, ssaaer, asmaer, ecaer, swuflx, swdflx, swhr, swuflxc, &
    swdflxc, swhrc, bndsolvar, indsolvar, solcycfrac, n0, n1, n2, n3, n4, n5, &
    n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16, n17, n18, n19, n20, n21, &
    n22, n23, n24, n25, n26, n27, n28, n29, n30, n31, n32, n33, n34, n35, n36, &
    n37, n38, n39, n40, n41, n42, n43, n44, n45, n46, n47, n48, n49, n50, n51, &
    n52, n53, n54, n55, n56, n57, n58, n59, n60, n61, n62, n63, n64, n65, n66, &
    n67, n68, n69, n70, n71, n72, n73, n74, n75, n76)
    use rrtmg_sw_rad, only: rrtmg_sw
    implicit none
    
    integer(4), intent(in) :: ncol
    integer(4), intent(in) :: nlay
    integer(4), intent(inout) :: icld
    integer(4), intent(inout) :: iaer
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
    real(8), intent(in), dimension(n21) :: asdir
    real(8), intent(in), dimension(n22) :: asdif
    real(8), intent(in), dimension(n23) :: aldir
    real(8), intent(in), dimension(n24) :: aldif
    real(8), intent(in), dimension(n25) :: coszen
    real(8), intent(in) :: adjes
    integer(4), intent(in) :: dyofyr
    real(8), intent(in) :: scon
    integer(4), intent(in) :: isolvar
    integer(4), intent(in) :: inflgsw
    integer(4), intent(in) :: iceflgsw
    integer(4), intent(in) :: liqflgsw
    real(8), intent(in), dimension(n26,n27,n28) :: cldfmcl
    real(8), intent(in), dimension(n29,n30,n31) :: taucmcl
    real(8), intent(in), dimension(n32,n33,n34) :: ssacmcl
    real(8), intent(in), dimension(n35,n36,n37) :: asmcmcl
    real(8), intent(in), dimension(n38,n39,n40) :: fsfcmcl
    real(8), intent(in), dimension(n41,n42,n43) :: ciwpmcl
    real(8), intent(in), dimension(n44,n45,n46) :: clwpmcl
    real(8), intent(in), dimension(n47,n48) :: reicmcl
    real(8), intent(in), dimension(n49,n50) :: relqmcl
    real(8), intent(in), dimension(n51,n52,n53) :: tauaer
    real(8), intent(in), dimension(n54,n55,n56) :: ssaaer
    real(8), intent(in), dimension(n57,n58,n59) :: asmaer
    real(8), intent(in), dimension(n60,n61,n62) :: ecaer
    real(8), intent(inout), dimension(n63,n64) :: swuflx
    real(8), intent(inout), dimension(n65,n66) :: swdflx
    real(8), intent(inout), dimension(n67,n68) :: swhr
    real(8), intent(inout), dimension(n69,n70) :: swuflxc
    real(8), intent(inout), dimension(n71,n72) :: swdflxc
    real(8), intent(inout), dimension(n73,n74) :: swhrc
    real(8), intent(inout), optional, dimension(n75) :: bndsolvar
    real(8), intent(inout), optional, dimension(n76) :: indsolvar
    real(8), intent(inout), optional :: solcycfrac
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
    !f2py intent(hide), depend(asdir) :: n21 = shape(asdir,0)
    integer :: n22
    !f2py intent(hide), depend(asdif) :: n22 = shape(asdif,0)
    integer :: n23
    !f2py intent(hide), depend(aldir) :: n23 = shape(aldir,0)
    integer :: n24
    !f2py intent(hide), depend(aldif) :: n24 = shape(aldif,0)
    integer :: n25
    !f2py intent(hide), depend(coszen) :: n25 = shape(coszen,0)
    integer :: n26
    !f2py intent(hide), depend(cldfmcl) :: n26 = shape(cldfmcl,0)
    integer :: n27
    !f2py intent(hide), depend(cldfmcl) :: n27 = shape(cldfmcl,1)
    integer :: n28
    !f2py intent(hide), depend(cldfmcl) :: n28 = shape(cldfmcl,2)
    integer :: n29
    !f2py intent(hide), depend(taucmcl) :: n29 = shape(taucmcl,0)
    integer :: n30
    !f2py intent(hide), depend(taucmcl) :: n30 = shape(taucmcl,1)
    integer :: n31
    !f2py intent(hide), depend(taucmcl) :: n31 = shape(taucmcl,2)
    integer :: n32
    !f2py intent(hide), depend(ssacmcl) :: n32 = shape(ssacmcl,0)
    integer :: n33
    !f2py intent(hide), depend(ssacmcl) :: n33 = shape(ssacmcl,1)
    integer :: n34
    !f2py intent(hide), depend(ssacmcl) :: n34 = shape(ssacmcl,2)
    integer :: n35
    !f2py intent(hide), depend(asmcmcl) :: n35 = shape(asmcmcl,0)
    integer :: n36
    !f2py intent(hide), depend(asmcmcl) :: n36 = shape(asmcmcl,1)
    integer :: n37
    !f2py intent(hide), depend(asmcmcl) :: n37 = shape(asmcmcl,2)
    integer :: n38
    !f2py intent(hide), depend(fsfcmcl) :: n38 = shape(fsfcmcl,0)
    integer :: n39
    !f2py intent(hide), depend(fsfcmcl) :: n39 = shape(fsfcmcl,1)
    integer :: n40
    !f2py intent(hide), depend(fsfcmcl) :: n40 = shape(fsfcmcl,2)
    integer :: n41
    !f2py intent(hide), depend(ciwpmcl) :: n41 = shape(ciwpmcl,0)
    integer :: n42
    !f2py intent(hide), depend(ciwpmcl) :: n42 = shape(ciwpmcl,1)
    integer :: n43
    !f2py intent(hide), depend(ciwpmcl) :: n43 = shape(ciwpmcl,2)
    integer :: n44
    !f2py intent(hide), depend(clwpmcl) :: n44 = shape(clwpmcl,0)
    integer :: n45
    !f2py intent(hide), depend(clwpmcl) :: n45 = shape(clwpmcl,1)
    integer :: n46
    !f2py intent(hide), depend(clwpmcl) :: n46 = shape(clwpmcl,2)
    integer :: n47
    !f2py intent(hide), depend(reicmcl) :: n47 = shape(reicmcl,0)
    integer :: n48
    !f2py intent(hide), depend(reicmcl) :: n48 = shape(reicmcl,1)
    integer :: n49
    !f2py intent(hide), depend(relqmcl) :: n49 = shape(relqmcl,0)
    integer :: n50
    !f2py intent(hide), depend(relqmcl) :: n50 = shape(relqmcl,1)
    integer :: n51
    !f2py intent(hide), depend(tauaer) :: n51 = shape(tauaer,0)
    integer :: n52
    !f2py intent(hide), depend(tauaer) :: n52 = shape(tauaer,1)
    integer :: n53
    !f2py intent(hide), depend(tauaer) :: n53 = shape(tauaer,2)
    integer :: n54
    !f2py intent(hide), depend(ssaaer) :: n54 = shape(ssaaer,0)
    integer :: n55
    !f2py intent(hide), depend(ssaaer) :: n55 = shape(ssaaer,1)
    integer :: n56
    !f2py intent(hide), depend(ssaaer) :: n56 = shape(ssaaer,2)
    integer :: n57
    !f2py intent(hide), depend(asmaer) :: n57 = shape(asmaer,0)
    integer :: n58
    !f2py intent(hide), depend(asmaer) :: n58 = shape(asmaer,1)
    integer :: n59
    !f2py intent(hide), depend(asmaer) :: n59 = shape(asmaer,2)
    integer :: n60
    !f2py intent(hide), depend(ecaer) :: n60 = shape(ecaer,0)
    integer :: n61
    !f2py intent(hide), depend(ecaer) :: n61 = shape(ecaer,1)
    integer :: n62
    !f2py intent(hide), depend(ecaer) :: n62 = shape(ecaer,2)
    integer :: n63
    !f2py intent(hide), depend(swuflx) :: n63 = shape(swuflx,0)
    integer :: n64
    !f2py intent(hide), depend(swuflx) :: n64 = shape(swuflx,1)
    integer :: n65
    !f2py intent(hide), depend(swdflx) :: n65 = shape(swdflx,0)
    integer :: n66
    !f2py intent(hide), depend(swdflx) :: n66 = shape(swdflx,1)
    integer :: n67
    !f2py intent(hide), depend(swhr) :: n67 = shape(swhr,0)
    integer :: n68
    !f2py intent(hide), depend(swhr) :: n68 = shape(swhr,1)
    integer :: n69
    !f2py intent(hide), depend(swuflxc) :: n69 = shape(swuflxc,0)
    integer :: n70
    !f2py intent(hide), depend(swuflxc) :: n70 = shape(swuflxc,1)
    integer :: n71
    !f2py intent(hide), depend(swdflxc) :: n71 = shape(swdflxc,0)
    integer :: n72
    !f2py intent(hide), depend(swdflxc) :: n72 = shape(swdflxc,1)
    integer :: n73
    !f2py intent(hide), depend(swhrc) :: n73 = shape(swhrc,0)
    integer :: n74
    !f2py intent(hide), depend(swhrc) :: n74 = shape(swhrc,1)
    integer :: n75
    !f2py intent(hide), depend(bndsolvar) :: n75 = shape(bndsolvar,0)
    integer :: n76
    !f2py intent(hide), depend(indsolvar) :: n76 = shape(indsolvar,0)
    call rrtmg_sw(ncol=ncol, nlay=nlay, icld=icld, iaer=iaer, play=play, plev=plev, &
        tlay=tlay, tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, o3vmr=o3vmr, co2vmr=co2vmr, &
        ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, asdir=asdir, asdif=asdif, &
        aldir=aldir, aldif=aldif, coszen=coszen, adjes=adjes, dyofyr=dyofyr, &
        scon=scon, isolvar=isolvar, inflgsw=inflgsw, iceflgsw=iceflgsw, &
        liqflgsw=liqflgsw, cldfmcl=cldfmcl, taucmcl=taucmcl, ssacmcl=ssacmcl, &
        asmcmcl=asmcmcl, fsfcmcl=fsfcmcl, ciwpmcl=ciwpmcl, clwpmcl=clwpmcl, &
        reicmcl=reicmcl, relqmcl=relqmcl, tauaer=tauaer, ssaaer=ssaaer, &
        asmaer=asmaer, ecaer=ecaer, swuflx=swuflx, swdflx=swdflx, swhr=swhr, &
        swuflxc=swuflxc, swdflxc=swdflxc, swhrc=swhrc, bndsolvar=bndsolvar, &
        indsolvar=indsolvar, solcycfrac=solcycfrac)
end subroutine f90wrap_rrtmg_sw

subroutine f90wrap_earth_sun(ret_earth_sun, idn)
    use rrtmg_sw_rad, only: earth_sun
    implicit none
    
    real(8), intent(out) :: ret_earth_sun
    integer(4), intent(in) :: idn
    ret_earth_sun = earth_sun(idn=idn)
end subroutine f90wrap_earth_sun

subroutine f90wrap_inatm_sw(iplon, nlay, icld, iaer, play, plev, tlay, tlev, &
    tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, adjes, dyofyr, scon, &
    isolvar, inflgsw, iceflgsw, liqflgsw, cldfmcl, taucmcl, ssacmcl, asmcmcl, &
    fsfcmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, tauaer, ssaaer, asmaer, &
    nlayers, pavel, pz, pdp, tavel, tz, tbound, coldry, wkl, adjflux, inflag, &
    iceflag, liqflag, cldfmc, taucmc, ssacmc, asmcmc, fsfcmc, ciwpmc, clwpmc, &
    reicmc, relqmc, taua, ssaa, asma, svar_f, svar_s, svar_i, svar_f_bnd, &
    svar_s_bnd, svar_i_bnd, bndsolvar, indsolvar, solcycfrac, n0, n1, n2, n3, &
    n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16, n17, n18, n19, &
    n20, n21, n22, n23, n24, n25, n26, n27, n28, n29, n30, n31, n32, n33, n34, &
    n35, n36, n37, n38, n39, n40, n41, n42, n43, n44, n45, n46, n47, n48, n49, &
    n50, n51, n52, n53, n54, n55, n56, n57, n58, n59, n60, n61, n62, n63, n64, &
    n65, n66, n67, n68, n69, n70, n71, n72, n73, n74, n75, n76, n77, n78, n79, &
    n80, n81, n82, n83, n84, n85, n86, n87, n88, n89, n90)
    use rrtmg_sw_rad, only: inatm_sw
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
    real(8), intent(in) :: adjes
    integer(4), intent(in) :: dyofyr
    real(8), intent(in) :: scon
    integer(4), intent(in) :: isolvar
    integer(4), intent(in) :: inflgsw
    integer(4), intent(in) :: iceflgsw
    integer(4), intent(in) :: liqflgsw
    real(8), intent(in), dimension(n21,n22,n23) :: cldfmcl
    real(8), intent(in), dimension(n24,n25,n26) :: taucmcl
    real(8), intent(in), dimension(n27,n28,n29) :: ssacmcl
    real(8), intent(in), dimension(n30,n31,n32) :: asmcmcl
    real(8), intent(in), dimension(n33,n34,n35) :: fsfcmcl
    real(8), intent(in), dimension(n36,n37,n38) :: ciwpmcl
    real(8), intent(in), dimension(n39,n40,n41) :: clwpmcl
    real(8), intent(in), dimension(n42,n43) :: reicmcl
    real(8), intent(in), dimension(n44,n45) :: relqmcl
    real(8), intent(in), dimension(n46,n47,n48) :: tauaer
    real(8), intent(in), dimension(n49,n50,n51) :: ssaaer
    real(8), intent(in), dimension(n52,n53,n54) :: asmaer
    integer(4), intent(out) :: nlayers
    real(8), intent(inout), dimension(n55) :: pavel
    real(8), intent(inout), dimension(n56) :: pz
    real(8), intent(inout), dimension(n57) :: pdp
    real(8), intent(inout), dimension(n58) :: tavel
    real(8), intent(inout), dimension(n59) :: tz
    real(8), intent(out) :: tbound
    real(8), intent(inout), dimension(n60) :: coldry
    real(8), intent(inout), dimension(n61,n62) :: wkl
    real(8), intent(inout), dimension(n63) :: adjflux
    integer(4), intent(out) :: inflag
    integer(4), intent(out) :: iceflag
    integer(4), intent(out) :: liqflag
    real(8), intent(inout), dimension(n64,n65) :: cldfmc
    real(8), intent(inout), dimension(n66,n67) :: taucmc
    real(8), intent(inout), dimension(n68,n69) :: ssacmc
    real(8), intent(inout), dimension(n70,n71) :: asmcmc
    real(8), intent(inout), dimension(n72,n73) :: fsfcmc
    real(8), intent(inout), dimension(n74,n75) :: ciwpmc
    real(8), intent(inout), dimension(n76,n77) :: clwpmc
    real(8), intent(inout), dimension(n78) :: reicmc
    real(8), intent(inout), dimension(n79) :: relqmc
    real(8), intent(inout), dimension(n80,n81) :: taua
    real(8), intent(inout), dimension(n82,n83) :: ssaa
    real(8), intent(inout), dimension(n84,n85) :: asma
    real(8), intent(out) :: svar_f
    real(8), intent(out) :: svar_s
    real(8), intent(out) :: svar_i
    real(8), intent(inout), dimension(n86) :: svar_f_bnd
    real(8), intent(inout), dimension(n87) :: svar_s_bnd
    real(8), intent(inout), dimension(n88) :: svar_i_bnd
    real(8), intent(inout), optional, dimension(n89) :: bndsolvar
    real(8), intent(inout), optional, dimension(n90) :: indsolvar
    real(8), intent(inout), optional :: solcycfrac
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
    !f2py intent(hide), depend(cldfmcl) :: n21 = shape(cldfmcl,0)
    integer :: n22
    !f2py intent(hide), depend(cldfmcl) :: n22 = shape(cldfmcl,1)
    integer :: n23
    !f2py intent(hide), depend(cldfmcl) :: n23 = shape(cldfmcl,2)
    integer :: n24
    !f2py intent(hide), depend(taucmcl) :: n24 = shape(taucmcl,0)
    integer :: n25
    !f2py intent(hide), depend(taucmcl) :: n25 = shape(taucmcl,1)
    integer :: n26
    !f2py intent(hide), depend(taucmcl) :: n26 = shape(taucmcl,2)
    integer :: n27
    !f2py intent(hide), depend(ssacmcl) :: n27 = shape(ssacmcl,0)
    integer :: n28
    !f2py intent(hide), depend(ssacmcl) :: n28 = shape(ssacmcl,1)
    integer :: n29
    !f2py intent(hide), depend(ssacmcl) :: n29 = shape(ssacmcl,2)
    integer :: n30
    !f2py intent(hide), depend(asmcmcl) :: n30 = shape(asmcmcl,0)
    integer :: n31
    !f2py intent(hide), depend(asmcmcl) :: n31 = shape(asmcmcl,1)
    integer :: n32
    !f2py intent(hide), depend(asmcmcl) :: n32 = shape(asmcmcl,2)
    integer :: n33
    !f2py intent(hide), depend(fsfcmcl) :: n33 = shape(fsfcmcl,0)
    integer :: n34
    !f2py intent(hide), depend(fsfcmcl) :: n34 = shape(fsfcmcl,1)
    integer :: n35
    !f2py intent(hide), depend(fsfcmcl) :: n35 = shape(fsfcmcl,2)
    integer :: n36
    !f2py intent(hide), depend(ciwpmcl) :: n36 = shape(ciwpmcl,0)
    integer :: n37
    !f2py intent(hide), depend(ciwpmcl) :: n37 = shape(ciwpmcl,1)
    integer :: n38
    !f2py intent(hide), depend(ciwpmcl) :: n38 = shape(ciwpmcl,2)
    integer :: n39
    !f2py intent(hide), depend(clwpmcl) :: n39 = shape(clwpmcl,0)
    integer :: n40
    !f2py intent(hide), depend(clwpmcl) :: n40 = shape(clwpmcl,1)
    integer :: n41
    !f2py intent(hide), depend(clwpmcl) :: n41 = shape(clwpmcl,2)
    integer :: n42
    !f2py intent(hide), depend(reicmcl) :: n42 = shape(reicmcl,0)
    integer :: n43
    !f2py intent(hide), depend(reicmcl) :: n43 = shape(reicmcl,1)
    integer :: n44
    !f2py intent(hide), depend(relqmcl) :: n44 = shape(relqmcl,0)
    integer :: n45
    !f2py intent(hide), depend(relqmcl) :: n45 = shape(relqmcl,1)
    integer :: n46
    !f2py intent(hide), depend(tauaer) :: n46 = shape(tauaer,0)
    integer :: n47
    !f2py intent(hide), depend(tauaer) :: n47 = shape(tauaer,1)
    integer :: n48
    !f2py intent(hide), depend(tauaer) :: n48 = shape(tauaer,2)
    integer :: n49
    !f2py intent(hide), depend(ssaaer) :: n49 = shape(ssaaer,0)
    integer :: n50
    !f2py intent(hide), depend(ssaaer) :: n50 = shape(ssaaer,1)
    integer :: n51
    !f2py intent(hide), depend(ssaaer) :: n51 = shape(ssaaer,2)
    integer :: n52
    !f2py intent(hide), depend(asmaer) :: n52 = shape(asmaer,0)
    integer :: n53
    !f2py intent(hide), depend(asmaer) :: n53 = shape(asmaer,1)
    integer :: n54
    !f2py intent(hide), depend(asmaer) :: n54 = shape(asmaer,2)
    integer :: n55
    !f2py intent(hide), depend(pavel) :: n55 = shape(pavel,0)
    integer :: n56
    !f2py intent(hide), depend(pz) :: n56 = shape(pz,0)
    integer :: n57
    !f2py intent(hide), depend(pdp) :: n57 = shape(pdp,0)
    integer :: n58
    !f2py intent(hide), depend(tavel) :: n58 = shape(tavel,0)
    integer :: n59
    !f2py intent(hide), depend(tz) :: n59 = shape(tz,0)
    integer :: n60
    !f2py intent(hide), depend(coldry) :: n60 = shape(coldry,0)
    integer :: n61
    !f2py intent(hide), depend(wkl) :: n61 = shape(wkl,0)
    integer :: n62
    !f2py intent(hide), depend(wkl) :: n62 = shape(wkl,1)
    integer :: n63
    !f2py intent(hide), depend(adjflux) :: n63 = shape(adjflux,0)
    integer :: n64
    !f2py intent(hide), depend(cldfmc) :: n64 = shape(cldfmc,0)
    integer :: n65
    !f2py intent(hide), depend(cldfmc) :: n65 = shape(cldfmc,1)
    integer :: n66
    !f2py intent(hide), depend(taucmc) :: n66 = shape(taucmc,0)
    integer :: n67
    !f2py intent(hide), depend(taucmc) :: n67 = shape(taucmc,1)
    integer :: n68
    !f2py intent(hide), depend(ssacmc) :: n68 = shape(ssacmc,0)
    integer :: n69
    !f2py intent(hide), depend(ssacmc) :: n69 = shape(ssacmc,1)
    integer :: n70
    !f2py intent(hide), depend(asmcmc) :: n70 = shape(asmcmc,0)
    integer :: n71
    !f2py intent(hide), depend(asmcmc) :: n71 = shape(asmcmc,1)
    integer :: n72
    !f2py intent(hide), depend(fsfcmc) :: n72 = shape(fsfcmc,0)
    integer :: n73
    !f2py intent(hide), depend(fsfcmc) :: n73 = shape(fsfcmc,1)
    integer :: n74
    !f2py intent(hide), depend(ciwpmc) :: n74 = shape(ciwpmc,0)
    integer :: n75
    !f2py intent(hide), depend(ciwpmc) :: n75 = shape(ciwpmc,1)
    integer :: n76
    !f2py intent(hide), depend(clwpmc) :: n76 = shape(clwpmc,0)
    integer :: n77
    !f2py intent(hide), depend(clwpmc) :: n77 = shape(clwpmc,1)
    integer :: n78
    !f2py intent(hide), depend(reicmc) :: n78 = shape(reicmc,0)
    integer :: n79
    !f2py intent(hide), depend(relqmc) :: n79 = shape(relqmc,0)
    integer :: n80
    !f2py intent(hide), depend(taua) :: n80 = shape(taua,0)
    integer :: n81
    !f2py intent(hide), depend(taua) :: n81 = shape(taua,1)
    integer :: n82
    !f2py intent(hide), depend(ssaa) :: n82 = shape(ssaa,0)
    integer :: n83
    !f2py intent(hide), depend(ssaa) :: n83 = shape(ssaa,1)
    integer :: n84
    !f2py intent(hide), depend(asma) :: n84 = shape(asma,0)
    integer :: n85
    !f2py intent(hide), depend(asma) :: n85 = shape(asma,1)
    integer :: n86
    !f2py intent(hide), depend(svar_f_bnd) :: n86 = shape(svar_f_bnd,0)
    integer :: n87
    !f2py intent(hide), depend(svar_s_bnd) :: n87 = shape(svar_s_bnd,0)
    integer :: n88
    !f2py intent(hide), depend(svar_i_bnd) :: n88 = shape(svar_i_bnd,0)
    integer :: n89
    !f2py intent(hide), depend(bndsolvar) :: n89 = shape(bndsolvar,0)
    integer :: n90
    !f2py intent(hide), depend(indsolvar) :: n90 = shape(indsolvar,0)
    call inatm_sw(iplon=iplon, nlay=nlay, icld=icld, iaer=iaer, play=play, &
        plev=plev, tlay=tlay, tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, o3vmr=o3vmr, &
        co2vmr=co2vmr, ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, adjes=adjes, &
        dyofyr=dyofyr, scon=scon, isolvar=isolvar, inflgsw=inflgsw, &
        iceflgsw=iceflgsw, liqflgsw=liqflgsw, cldfmcl=cldfmcl, taucmcl=taucmcl, &
        ssacmcl=ssacmcl, asmcmcl=asmcmcl, fsfcmcl=fsfcmcl, ciwpmcl=ciwpmcl, &
        clwpmcl=clwpmcl, reicmcl=reicmcl, relqmcl=relqmcl, tauaer=tauaer, &
        ssaaer=ssaaer, asmaer=asmaer, nlayers=nlayers, pavel=pavel, pz=pz, pdp=pdp, &
        tavel=tavel, tz=tz, tbound=tbound, coldry=coldry, wkl=wkl, adjflux=adjflux, &
        inflag=inflag, iceflag=iceflag, liqflag=liqflag, cldfmc=cldfmc, &
        taucmc=taucmc, ssacmc=ssacmc, asmcmc=asmcmc, fsfcmc=fsfcmc, ciwpmc=ciwpmc, &
        clwpmc=clwpmc, reicmc=reicmc, relqmc=relqmc, taua=taua, ssaa=ssaa, &
        asma=asma, svar_f=svar_f, svar_s=svar_s, svar_i=svar_i, &
        svar_f_bnd=svar_f_bnd, svar_s_bnd=svar_s_bnd, svar_i_bnd=svar_i_bnd, &
        bndsolvar=bndsolvar, indsolvar=indsolvar, solcycfrac=solcycfrac)
end subroutine f90wrap_inatm_sw

! End of module rrtmg_sw_rad defined in file rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_rad.f90

