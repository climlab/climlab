import numpy as np
#import parrrtm, rrtmg_lw_init, mcica_subcol_gen_lw, rrtmg_lw_rad
#import parrrtm, mcica_subcol_gen_lw, rrtmg_lw_rad
#import parrrtm, mcica_subcol_gen_lw, rrtmg_lw
import parrrtm, rrtmg_lw
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
    cldfmcl = np.zeros((ngptlw,ncol,nlay), order='F')
    ciwpmcl = np.zeros((ngptlw,ncol,nlay), order='F')
    clwpmcl = np.zeros((ngptlw,ncol,nlay), order='F')
    reicmcl = np.zeros((ncol,nlay), order='F')
    relqmcl = np.zeros((ncol,nlay), order='F')
    taucmcl = np.zeros((ngptlw,ncol,nlay), order='F')
    rrtmg_lw.mcica_subcol_gen_lw.mcica_subcol_lw(1, ncol, nlay, icld, permuteseed, irng, play,
                       cldfrac, ciwp, clwp, reic, relq, tauc, cldfmcl,
                       ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl)

    #! In principle the init routine should not need to be called every timestep
    #!  But calling it from Python is not working... heatfac is not getting set properly
    rrtmg_lw.rrtmg_lw_init.rrtmg_lw_ini(cpdair)

    #  Initialize return arrays
    uflx = np.zeros((ncol,nlay+1), order='F')
    dflx = np.zeros((ncol,nlay+1), order='F')
    hr = np.zeros((ncol,nlay), order='F')
    uflxc = np.zeros((ncol,nlay+1), order='F')
    dflxc = np.zeros((ncol,nlay+1), order='F')
    hrc = np.zeros((ncol,nlay), order='F')
    duflx_dt = np.zeros((ncol,nlay+1), order='F')
    duflxc_dt = np.zeros((ncol,nlay+1), order='F')

    #!  Call the RRTMG_LW driver to compute radiative fluxes
    rrtmg_lw.rrtmg_lw_rad.rrtmg_lw(ncol    ,nlay    ,icld    ,idrv    ,
             play    , plev    , tlay    , tlev    , tsfc    ,
             h2ovmr  , o3vmr   , co2vmr  , ch4vmr  , n2ovmr  , o2vmr ,
             cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr , emis    ,
             inflglw , iceflglw, liqflglw, cldfmcl ,
             taucmcl , ciwpmcl , clwpmcl , reicmcl , relqmcl ,
             tauaer  ,
             uflx    , dflx    , hr      , uflxc   , dflxc,  hrc,
             duflx_dt,duflxc_dt )

    return uflx, dflx, hr, uflxc, dflxc, hrc, duflx_dt, duflxc_dt
