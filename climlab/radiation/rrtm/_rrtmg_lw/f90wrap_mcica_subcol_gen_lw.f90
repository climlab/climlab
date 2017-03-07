! Module mcica_subcol_gen_lw defined in file rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90

subroutine f90wrap_mcica_subcol_lw(iplon, ncol, nlay, icld, permuteseed, irng, &
    play, cldfrac, ciwp, clwp, rei, rel, tauc, cldfmcl, ciwpmcl, clwpmcl, &
    reicmcl, relqmcl, taucmcl, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, &
    n12, n13, n14, n15, n16, n17, n18, n19, n20, n21, n22, n23, n24, n25, n26, &
    n27, n28, n29, n30)
    use mcica_subcol_gen_lw, only: mcica_subcol_lw
    implicit none
    
    integer(4), intent(in) :: iplon
    integer(4), intent(in) :: ncol
    integer(4), intent(in) :: nlay
    integer(4), intent(in) :: icld
    integer(4), intent(in) :: permuteseed
    integer(4), intent(inout) :: irng
    real(4), intent(in), dimension(n0,n1) :: play
    real(4), intent(in), dimension(n2,n3) :: cldfrac
    real(4), intent(in), dimension(n4,n5) :: ciwp
    real(4), intent(in), dimension(n6,n7) :: clwp
    real(4), intent(in), dimension(n8,n9) :: rei
    real(4), intent(in), dimension(n10,n11) :: rel
    real(4), intent(in), dimension(n12,n13,n14) :: tauc
    real(4), intent(inout), dimension(n15,n16,n17) :: cldfmcl
    real(4), intent(inout), dimension(n18,n19,n20) :: ciwpmcl
    real(4), intent(inout), dimension(n21,n22,n23) :: clwpmcl
    real(4), intent(inout), dimension(n24,n25) :: reicmcl
    real(4), intent(inout), dimension(n26,n27) :: relqmcl
    real(4), intent(inout), dimension(n28,n29,n30) :: taucmcl
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
    !f2py intent(hide), depend(cldfmcl) :: n15 = shape(cldfmcl,0)
    integer :: n16
    !f2py intent(hide), depend(cldfmcl) :: n16 = shape(cldfmcl,1)
    integer :: n17
    !f2py intent(hide), depend(cldfmcl) :: n17 = shape(cldfmcl,2)
    integer :: n18
    !f2py intent(hide), depend(ciwpmcl) :: n18 = shape(ciwpmcl,0)
    integer :: n19
    !f2py intent(hide), depend(ciwpmcl) :: n19 = shape(ciwpmcl,1)
    integer :: n20
    !f2py intent(hide), depend(ciwpmcl) :: n20 = shape(ciwpmcl,2)
    integer :: n21
    !f2py intent(hide), depend(clwpmcl) :: n21 = shape(clwpmcl,0)
    integer :: n22
    !f2py intent(hide), depend(clwpmcl) :: n22 = shape(clwpmcl,1)
    integer :: n23
    !f2py intent(hide), depend(clwpmcl) :: n23 = shape(clwpmcl,2)
    integer :: n24
    !f2py intent(hide), depend(reicmcl) :: n24 = shape(reicmcl,0)
    integer :: n25
    !f2py intent(hide), depend(reicmcl) :: n25 = shape(reicmcl,1)
    integer :: n26
    !f2py intent(hide), depend(relqmcl) :: n26 = shape(relqmcl,0)
    integer :: n27
    !f2py intent(hide), depend(relqmcl) :: n27 = shape(relqmcl,1)
    integer :: n28
    !f2py intent(hide), depend(taucmcl) :: n28 = shape(taucmcl,0)
    integer :: n29
    !f2py intent(hide), depend(taucmcl) :: n29 = shape(taucmcl,1)
    integer :: n30
    !f2py intent(hide), depend(taucmcl) :: n30 = shape(taucmcl,2)
    call mcica_subcol_lw(iplon=iplon, ncol=ncol, nlay=nlay, icld=icld, &
        permuteseed=permuteseed, irng=irng, play=play, cldfrac=cldfrac, ciwp=ciwp, &
        clwp=clwp, rei=rei, rel=rel, tauc=tauc, cldfmcl=cldfmcl, ciwpmcl=ciwpmcl, &
        clwpmcl=clwpmcl, reicmcl=reicmcl, relqmcl=relqmcl, taucmcl=taucmcl)
end subroutine f90wrap_mcica_subcol_lw

subroutine f90wrap_generate_stochastic_clouds(ncol, nlay, nsubcol, icld, irng, &
    pmid, cld, clwp, ciwp, tauc, cld_stoch, clwp_stoch, ciwp_stoch, tauc_stoch, &
    changeseed, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, &
    n15, n16, n17, n18, n19, n20, n21, n22)
    use mcica_subcol_gen_lw, only: generate_stochastic_clouds
    implicit none
    
    integer(4), intent(in) :: ncol
    integer(4), intent(in) :: nlay
    integer(4), intent(in) :: nsubcol
    integer(4), intent(in) :: icld
    integer(4), intent(inout) :: irng
    real(4), intent(in), dimension(n0,n1) :: pmid
    real(4), intent(in), dimension(n2,n3) :: cld
    real(4), intent(in), dimension(n4,n5) :: clwp
    real(4), intent(in), dimension(n6,n7) :: ciwp
    real(4), intent(in), dimension(n8,n9,n10) :: tauc
    real(4), intent(inout), dimension(n11,n12,n13) :: cld_stoch
    real(4), intent(inout), dimension(n14,n15,n16) :: clwp_stoch
    real(4), intent(inout), dimension(n17,n18,n19) :: ciwp_stoch
    real(4), intent(inout), dimension(n20,n21,n22) :: tauc_stoch
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
    !f2py intent(hide), depend(cld_stoch) :: n11 = shape(cld_stoch,0)
    integer :: n12
    !f2py intent(hide), depend(cld_stoch) :: n12 = shape(cld_stoch,1)
    integer :: n13
    !f2py intent(hide), depend(cld_stoch) :: n13 = shape(cld_stoch,2)
    integer :: n14
    !f2py intent(hide), depend(clwp_stoch) :: n14 = shape(clwp_stoch,0)
    integer :: n15
    !f2py intent(hide), depend(clwp_stoch) :: n15 = shape(clwp_stoch,1)
    integer :: n16
    !f2py intent(hide), depend(clwp_stoch) :: n16 = shape(clwp_stoch,2)
    integer :: n17
    !f2py intent(hide), depend(ciwp_stoch) :: n17 = shape(ciwp_stoch,0)
    integer :: n18
    !f2py intent(hide), depend(ciwp_stoch) :: n18 = shape(ciwp_stoch,1)
    integer :: n19
    !f2py intent(hide), depend(ciwp_stoch) :: n19 = shape(ciwp_stoch,2)
    integer :: n20
    !f2py intent(hide), depend(tauc_stoch) :: n20 = shape(tauc_stoch,0)
    integer :: n21
    !f2py intent(hide), depend(tauc_stoch) :: n21 = shape(tauc_stoch,1)
    integer :: n22
    !f2py intent(hide), depend(tauc_stoch) :: n22 = shape(tauc_stoch,2)
    call generate_stochastic_clouds(ncol=ncol, nlay=nlay, nsubcol=nsubcol, &
        icld=icld, irng=irng, pmid=pmid, cld=cld, clwp=clwp, ciwp=ciwp, tauc=tauc, &
        cld_stoch=cld_stoch, clwp_stoch=clwp_stoch, ciwp_stoch=ciwp_stoch, &
        tauc_stoch=tauc_stoch, changeSeed=changeseed)
end subroutine f90wrap_generate_stochastic_clouds

subroutine f90wrap_kissvec(seed1, seed2, seed3, seed4, ran_arr, n0, n1, n2, n3, &
    n4)
    use mcica_subcol_gen_lw, only: kissvec
    implicit none
    
    integer(4), intent(inout), dimension(n0) :: seed1
    integer(4), intent(inout), dimension(n1) :: seed2
    integer(4), intent(inout), dimension(n2) :: seed3
    integer(4), intent(inout), dimension(n3) :: seed4
    real(4), intent(inout), dimension(n4) :: ran_arr
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

! End of module mcica_subcol_gen_lw defined in file rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90

