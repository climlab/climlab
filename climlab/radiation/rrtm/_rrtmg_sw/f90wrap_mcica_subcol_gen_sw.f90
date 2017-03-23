! Module mcica_subcol_gen_sw defined in file rrtmg_sw_v4.0/gcm_model/src/mcica_subcol_gen_sw.f90

subroutine f90wrap_mcica_subcol_sw(iplon, ncol, nlay, icld, permuteseed, irng, &
    play, cldfrac, ciwp, clwp, rei, rel, tauc, ssac, asmc, fsfc, cldfmcl, &
    ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl, n0, &
    n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16, n17, &
    n18, n19, n20, n21, n22, n23, n24, n25, n26, n27, n28, n29, n30, n31, n32, &
    n33, n34, n35, n36, n37, n38, n39, n40, n41, n42, n43, n44, n45, n46, n47, &
    n48)
    use mcica_subcol_gen_sw, only: mcica_subcol_sw
    implicit none
    
    integer(4), intent(in) :: iplon
    integer(4), intent(in) :: ncol
    integer(4), intent(in) :: nlay
    integer(4), intent(in) :: icld
    integer(4), intent(in) :: permuteseed
    integer(4), intent(inout) :: irng
    real(8), intent(in), dimension(n0,n1) :: play
    real(8), intent(in), dimension(n2,n3) :: cldfrac
    real(8), intent(in), dimension(n4,n5) :: ciwp
    real(8), intent(in), dimension(n6,n7) :: clwp
    real(8), intent(in), dimension(n8,n9) :: rei
    real(8), intent(in), dimension(n10,n11) :: rel
    real(8), intent(in), dimension(n12,n13,n14) :: tauc
    real(8), intent(in), dimension(n15,n16,n17) :: ssac
    real(8), intent(in), dimension(n18,n19,n20) :: asmc
    real(8), intent(in), dimension(n21,n22,n23) :: fsfc
    real(8), intent(inout), dimension(n24,n25,n26) :: cldfmcl
    real(8), intent(inout), dimension(n27,n28,n29) :: ciwpmcl
    real(8), intent(inout), dimension(n30,n31,n32) :: clwpmcl
    real(8), intent(inout), dimension(n33,n34) :: reicmcl
    real(8), intent(inout), dimension(n35,n36) :: relqmcl
    real(8), intent(inout), dimension(n37,n38,n39) :: taucmcl
    real(8), intent(inout), dimension(n40,n41,n42) :: ssacmcl
    real(8), intent(inout), dimension(n43,n44,n45) :: asmcmcl
    real(8), intent(inout), dimension(n46,n47,n48) :: fsfcmcl
    integer :: n0
    !f2py intent(hide), depend(play) :: n0 = shape(play,0)
    integer :: n1
    !f2py intent(hide), depend(play) :: n1 = shape(play,1)
    integer :: n2
    !f2py intent(hide), depend(cldfrac) :: n2 = shape(cldfrac,0)
    integer :: n3
    !f2py intent(hide), depend(cldfrac) :: n3 = shape(cldfrac,1)
    integer :: n4
    !f2py intent(hide), depend(ciwp) :: n4 = shape(ciwp,0)
    integer :: n5
    !f2py intent(hide), depend(ciwp) :: n5 = shape(ciwp,1)
    integer :: n6
    !f2py intent(hide), depend(clwp) :: n6 = shape(clwp,0)
    integer :: n7
    !f2py intent(hide), depend(clwp) :: n7 = shape(clwp,1)
    integer :: n8
    !f2py intent(hide), depend(rei) :: n8 = shape(rei,0)
    integer :: n9
    !f2py intent(hide), depend(rei) :: n9 = shape(rei,1)
    integer :: n10
    !f2py intent(hide), depend(rel) :: n10 = shape(rel,0)
    integer :: n11
    !f2py intent(hide), depend(rel) :: n11 = shape(rel,1)
    integer :: n12
    !f2py intent(hide), depend(tauc) :: n12 = shape(tauc,0)
    integer :: n13
    !f2py intent(hide), depend(tauc) :: n13 = shape(tauc,1)
    integer :: n14
    !f2py intent(hide), depend(tauc) :: n14 = shape(tauc,2)
    integer :: n15
    !f2py intent(hide), depend(ssac) :: n15 = shape(ssac,0)
    integer :: n16
    !f2py intent(hide), depend(ssac) :: n16 = shape(ssac,1)
    integer :: n17
    !f2py intent(hide), depend(ssac) :: n17 = shape(ssac,2)
    integer :: n18
    !f2py intent(hide), depend(asmc) :: n18 = shape(asmc,0)
    integer :: n19
    !f2py intent(hide), depend(asmc) :: n19 = shape(asmc,1)
    integer :: n20
    !f2py intent(hide), depend(asmc) :: n20 = shape(asmc,2)
    integer :: n21
    !f2py intent(hide), depend(fsfc) :: n21 = shape(fsfc,0)
    integer :: n22
    !f2py intent(hide), depend(fsfc) :: n22 = shape(fsfc,1)
    integer :: n23
    !f2py intent(hide), depend(fsfc) :: n23 = shape(fsfc,2)
    integer :: n24
    !f2py intent(hide), depend(cldfmcl) :: n24 = shape(cldfmcl,0)
    integer :: n25
    !f2py intent(hide), depend(cldfmcl) :: n25 = shape(cldfmcl,1)
    integer :: n26
    !f2py intent(hide), depend(cldfmcl) :: n26 = shape(cldfmcl,2)
    integer :: n27
    !f2py intent(hide), depend(ciwpmcl) :: n27 = shape(ciwpmcl,0)
    integer :: n28
    !f2py intent(hide), depend(ciwpmcl) :: n28 = shape(ciwpmcl,1)
    integer :: n29
    !f2py intent(hide), depend(ciwpmcl) :: n29 = shape(ciwpmcl,2)
    integer :: n30
    !f2py intent(hide), depend(clwpmcl) :: n30 = shape(clwpmcl,0)
    integer :: n31
    !f2py intent(hide), depend(clwpmcl) :: n31 = shape(clwpmcl,1)
    integer :: n32
    !f2py intent(hide), depend(clwpmcl) :: n32 = shape(clwpmcl,2)
    integer :: n33
    !f2py intent(hide), depend(reicmcl) :: n33 = shape(reicmcl,0)
    integer :: n34
    !f2py intent(hide), depend(reicmcl) :: n34 = shape(reicmcl,1)
    integer :: n35
    !f2py intent(hide), depend(relqmcl) :: n35 = shape(relqmcl,0)
    integer :: n36
    !f2py intent(hide), depend(relqmcl) :: n36 = shape(relqmcl,1)
    integer :: n37
    !f2py intent(hide), depend(taucmcl) :: n37 = shape(taucmcl,0)
    integer :: n38
    !f2py intent(hide), depend(taucmcl) :: n38 = shape(taucmcl,1)
    integer :: n39
    !f2py intent(hide), depend(taucmcl) :: n39 = shape(taucmcl,2)
    integer :: n40
    !f2py intent(hide), depend(ssacmcl) :: n40 = shape(ssacmcl,0)
    integer :: n41
    !f2py intent(hide), depend(ssacmcl) :: n41 = shape(ssacmcl,1)
    integer :: n42
    !f2py intent(hide), depend(ssacmcl) :: n42 = shape(ssacmcl,2)
    integer :: n43
    !f2py intent(hide), depend(asmcmcl) :: n43 = shape(asmcmcl,0)
    integer :: n44
    !f2py intent(hide), depend(asmcmcl) :: n44 = shape(asmcmcl,1)
    integer :: n45
    !f2py intent(hide), depend(asmcmcl) :: n45 = shape(asmcmcl,2)
    integer :: n46
    !f2py intent(hide), depend(fsfcmcl) :: n46 = shape(fsfcmcl,0)
    integer :: n47
    !f2py intent(hide), depend(fsfcmcl) :: n47 = shape(fsfcmcl,1)
    integer :: n48
    !f2py intent(hide), depend(fsfcmcl) :: n48 = shape(fsfcmcl,2)
    call mcica_subcol_sw(iplon=iplon, ncol=ncol, nlay=nlay, icld=icld, &
        permuteseed=permuteseed, irng=irng, play=play, cldfrac=cldfrac, ciwp=ciwp, &
        clwp=clwp, rei=rei, rel=rel, tauc=tauc, ssac=ssac, asmc=asmc, fsfc=fsfc, &
        cldfmcl=cldfmcl, ciwpmcl=ciwpmcl, clwpmcl=clwpmcl, reicmcl=reicmcl, &
        relqmcl=relqmcl, taucmcl=taucmcl, ssacmcl=ssacmcl, asmcmcl=asmcmcl, &
        fsfcmcl=fsfcmcl)
end subroutine f90wrap_mcica_subcol_sw

subroutine f90wrap_generate_stochastic_clouds_sw(ncol, nlay, nsubcol, icld, &
    irng, pmid, cld, clwp, ciwp, tauc, ssac, asmc, fsfc, cld_stoch, clwp_stoch, &
    ciwp_stoch, tauc_stoch, ssac_stoch, asmc_stoch, fsfc_stoch, changeseed, n0, &
    n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16, n17, &
    n18, n19, n20, n21, n22, n23, n24, n25, n26, n27, n28, n29, n30, n31, n32, &
    n33, n34, n35, n36, n37, n38, n39, n40)
    use mcica_subcol_gen_sw, only: generate_stochastic_clouds_sw
    implicit none
    
    integer(4), intent(in) :: ncol
    integer(4), intent(in) :: nlay
    integer(4), intent(in) :: nsubcol
    integer(4), intent(in) :: icld
    integer(4), intent(inout) :: irng
    real(8), intent(in), dimension(n0,n1) :: pmid
    real(8), intent(in), dimension(n2,n3) :: cld
    real(8), intent(in), dimension(n4,n5) :: clwp
    real(8), intent(in), dimension(n6,n7) :: ciwp
    real(8), intent(in), dimension(n8,n9,n10) :: tauc
    real(8), intent(in), dimension(n11,n12,n13) :: ssac
    real(8), intent(in), dimension(n14,n15,n16) :: asmc
    real(8), intent(in), dimension(n17,n18,n19) :: fsfc
    real(8), intent(inout), dimension(n20,n21,n22) :: cld_stoch
    real(8), intent(inout), dimension(n23,n24,n25) :: clwp_stoch
    real(8), intent(inout), dimension(n26,n27,n28) :: ciwp_stoch
    real(8), intent(inout), dimension(n29,n30,n31) :: tauc_stoch
    real(8), intent(inout), dimension(n32,n33,n34) :: ssac_stoch
    real(8), intent(inout), dimension(n35,n36,n37) :: asmc_stoch
    real(8), intent(inout), dimension(n38,n39,n40) :: fsfc_stoch
    integer(4), optional, intent(in) :: changeseed
    integer :: n0
    !f2py intent(hide), depend(pmid) :: n0 = shape(pmid,0)
    integer :: n1
    !f2py intent(hide), depend(pmid) :: n1 = shape(pmid,1)
    integer :: n2
    !f2py intent(hide), depend(cld) :: n2 = shape(cld,0)
    integer :: n3
    !f2py intent(hide), depend(cld) :: n3 = shape(cld,1)
    integer :: n4
    !f2py intent(hide), depend(clwp) :: n4 = shape(clwp,0)
    integer :: n5
    !f2py intent(hide), depend(clwp) :: n5 = shape(clwp,1)
    integer :: n6
    !f2py intent(hide), depend(ciwp) :: n6 = shape(ciwp,0)
    integer :: n7
    !f2py intent(hide), depend(ciwp) :: n7 = shape(ciwp,1)
    integer :: n8
    !f2py intent(hide), depend(tauc) :: n8 = shape(tauc,0)
    integer :: n9
    !f2py intent(hide), depend(tauc) :: n9 = shape(tauc,1)
    integer :: n10
    !f2py intent(hide), depend(tauc) :: n10 = shape(tauc,2)
    integer :: n11
    !f2py intent(hide), depend(ssac) :: n11 = shape(ssac,0)
    integer :: n12
    !f2py intent(hide), depend(ssac) :: n12 = shape(ssac,1)
    integer :: n13
    !f2py intent(hide), depend(ssac) :: n13 = shape(ssac,2)
    integer :: n14
    !f2py intent(hide), depend(asmc) :: n14 = shape(asmc,0)
    integer :: n15
    !f2py intent(hide), depend(asmc) :: n15 = shape(asmc,1)
    integer :: n16
    !f2py intent(hide), depend(asmc) :: n16 = shape(asmc,2)
    integer :: n17
    !f2py intent(hide), depend(fsfc) :: n17 = shape(fsfc,0)
    integer :: n18
    !f2py intent(hide), depend(fsfc) :: n18 = shape(fsfc,1)
    integer :: n19
    !f2py intent(hide), depend(fsfc) :: n19 = shape(fsfc,2)
    integer :: n20
    !f2py intent(hide), depend(cld_stoch) :: n20 = shape(cld_stoch,0)
    integer :: n21
    !f2py intent(hide), depend(cld_stoch) :: n21 = shape(cld_stoch,1)
    integer :: n22
    !f2py intent(hide), depend(cld_stoch) :: n22 = shape(cld_stoch,2)
    integer :: n23
    !f2py intent(hide), depend(clwp_stoch) :: n23 = shape(clwp_stoch,0)
    integer :: n24
    !f2py intent(hide), depend(clwp_stoch) :: n24 = shape(clwp_stoch,1)
    integer :: n25
    !f2py intent(hide), depend(clwp_stoch) :: n25 = shape(clwp_stoch,2)
    integer :: n26
    !f2py intent(hide), depend(ciwp_stoch) :: n26 = shape(ciwp_stoch,0)
    integer :: n27
    !f2py intent(hide), depend(ciwp_stoch) :: n27 = shape(ciwp_stoch,1)
    integer :: n28
    !f2py intent(hide), depend(ciwp_stoch) :: n28 = shape(ciwp_stoch,2)
    integer :: n29
    !f2py intent(hide), depend(tauc_stoch) :: n29 = shape(tauc_stoch,0)
    integer :: n30
    !f2py intent(hide), depend(tauc_stoch) :: n30 = shape(tauc_stoch,1)
    integer :: n31
    !f2py intent(hide), depend(tauc_stoch) :: n31 = shape(tauc_stoch,2)
    integer :: n32
    !f2py intent(hide), depend(ssac_stoch) :: n32 = shape(ssac_stoch,0)
    integer :: n33
    !f2py intent(hide), depend(ssac_stoch) :: n33 = shape(ssac_stoch,1)
    integer :: n34
    !f2py intent(hide), depend(ssac_stoch) :: n34 = shape(ssac_stoch,2)
    integer :: n35
    !f2py intent(hide), depend(asmc_stoch) :: n35 = shape(asmc_stoch,0)
    integer :: n36
    !f2py intent(hide), depend(asmc_stoch) :: n36 = shape(asmc_stoch,1)
    integer :: n37
    !f2py intent(hide), depend(asmc_stoch) :: n37 = shape(asmc_stoch,2)
    integer :: n38
    !f2py intent(hide), depend(fsfc_stoch) :: n38 = shape(fsfc_stoch,0)
    integer :: n39
    !f2py intent(hide), depend(fsfc_stoch) :: n39 = shape(fsfc_stoch,1)
    integer :: n40
    !f2py intent(hide), depend(fsfc_stoch) :: n40 = shape(fsfc_stoch,2)
    call generate_stochastic_clouds_sw(ncol=ncol, nlay=nlay, nsubcol=nsubcol, &
        icld=icld, irng=irng, pmid=pmid, cld=cld, clwp=clwp, ciwp=ciwp, tauc=tauc, &
        ssac=ssac, asmc=asmc, fsfc=fsfc, cld_stoch=cld_stoch, clwp_stoch=clwp_stoch, &
        ciwp_stoch=ciwp_stoch, tauc_stoch=tauc_stoch, ssac_stoch=ssac_stoch, &
        asmc_stoch=asmc_stoch, fsfc_stoch=fsfc_stoch, changeSeed=changeseed)
end subroutine f90wrap_generate_stochastic_clouds_sw

subroutine f90wrap_kissvec(seed1, seed2, seed3, seed4, ran_arr, n0, n1, n2, n3, &
    n4)
    use mcica_subcol_gen_sw, only: kissvec
    implicit none
    
    integer(4), intent(inout), dimension(n0) :: seed1
    integer(4), intent(inout), dimension(n1) :: seed2
    integer(4), intent(inout), dimension(n2) :: seed3
    integer(4), intent(inout), dimension(n3) :: seed4
    real(8), intent(inout), dimension(n4) :: ran_arr
    integer :: n0
    !f2py intent(hide), depend(seed1) :: n0 = shape(seed1,0)
    integer :: n1
    !f2py intent(hide), depend(seed2) :: n1 = shape(seed2,0)
    integer :: n2
    !f2py intent(hide), depend(seed3) :: n2 = shape(seed3,0)
    integer :: n3
    !f2py intent(hide), depend(seed4) :: n3 = shape(seed4,0)
    integer :: n4
    !f2py intent(hide), depend(ran_arr) :: n4 = shape(ran_arr,0)
    call kissvec(seed1=seed1, seed2=seed2, seed3=seed3, seed4=seed4, &
        ran_arr=ran_arr)
end subroutine f90wrap_kissvec

! End of module mcica_subcol_gen_sw defined in file rrtmg_sw_v4.0/gcm_model/src/mcica_subcol_gen_sw.f90

