import numpy as np
import parrrtm, rrtmg_lw_init, mcica_subcol_gen_lw, rrtmg_lw_rad
nbndlw = parrrtm.parrrtm.nbndlw
ngptlw = parrrtm.parrrtm.ngptlw

def driver(ncol, nlay, icld, permuteseed, irng, idrv, cpdair,
        play, plev, tlay, tlev, tsfc,
        h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,
        cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis,
        inflglw, iceflglw, liqflglw,
        cldfrac, ciwp, clwp, reic, relq, tauc, tauaer,):
    #    ! Call the Monte Carlo Independent Column Approximation
    #    !   (McICA, Pincus et al., JC, 2003)
    cldfmcl = np.zeros((ngptlw,ncol,nlay))
    ciwpmcl = np.zeros((ngptlw,ncol,nlay))
    clwpmcl = np.zeros((ngptlw,ncol,nlay))
    reicmcl = np.zeros((ncol,nlay))
    relqmcl = np.zeros((ncol,nlay))
    taucmcl = np.zeros((ngptlw,ncol,nlay))
    mcica_subcol_gen_lw.mcica_subcol_gen_lw.mcica_subcol_lw(1, ncol, nlay, icld, permuteseed, irng, play,
                       cldfrac, ciwp, clwp, reic, relq, tauc, cldfmcl,
                       ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl)

    #! In principle the init routine should not need to be called every timestep
    #!  But calling it from Python is not working... heatfac is not getting set properly
    rrtmg_lw.rrtmg_lw_init.rrtmg_lw_ini(cpdair)

    #  Initialize return arrays
    uflx = np.zeros((ncol,nlay+1))
    dflx = np.zeros((ncol,nlay+1))
    hr = np.zeros((ncol,nlay))
    uflxc = np.zeros((ncol,nlay+1))
    dflxc = np.zeros((ncol,nlay+1))
    hrc = np.zeros(ncol,nlay)
    duflx_dt = np.zeros(ncol,nlay+1)
    duflxc_dt = np.zeros(ncol,nlay+1)

    #!  Call the RRTMG_LW driver to compute radiative fluxes
    rrtmg_lw_rad.rrtmg_lw_rad.rrtmg_lw(ncol    ,nlay    ,icld    ,idrv    ,
             play    , plev    , tlay    , tlev    , tsfc    ,
             h2ovmr  , o3vmr   , co2vmr  , ch4vmr  , n2ovmr  , o2vmr ,
             cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr , emis    ,
             inflglw , iceflglw, liqflglw, cldfmcl ,
             taucmcl , ciwpmcl , clwpmcl , reicmcl , relqmcl ,
             tauaer  ,
             uflx    , dflx    , hr      , uflxc   , dflxc,  hrc,
             duflx_dt,duflxc_dt )

    return uflx, dflx, hr, uflxc, dflxc, hrc, duflx_dt, duflxc_dt
