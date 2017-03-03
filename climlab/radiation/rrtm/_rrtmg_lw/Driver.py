import _Driver
import f90wrap.runtime
import logging

def driver(ncol, nlay, icld, permuteseed, irng, idrv, cpdair, play, plev, tlay, \
    tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, \
    cfc12vmr, cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw, cldfrac, \
    ciwp, clwp, reic, relq, tauc, tauaer, uflx, dflx, hr, uflxc, dflxc, hrc, \
    duflx_dt, duflxc_dt):
    """
    driver(ncol, nlay, icld, permuteseed, irng, idrv, cpdair, play, plev, tlay, \
        tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, \
        cfc12vmr, cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw, cldfrac, \
        ciwp, clwp, reic, relq, tauc, tauaer, uflx, dflx, hr, uflxc, dflxc, hrc, \
        duflx_dt, duflxc_dt)
    
    
    Defined at Driver.f90 lines 20-132
    
    Parameters
    ----------
    ncol : int
    nlay : int
    icld : int
    permuteseed : int
    irng : int
    idrv : int
    cpdair : float
    play : float array
    plev : float array
    tlay : float array
    tlev : float array
    tsfc : float array
    h2ovmr : float array
    o3vmr : float array
    co2vmr : float array
    ch4vmr : float array
    n2ovmr : float array
    o2vmr : float array
    cfc11vmr : float array
    cfc12vmr : float array
    cfc22vmr : float array
    ccl4vmr : float array
    emis : float array
    inflglw : int
    iceflglw : int
    liqflglw : int
    cldfrac : float array
    ciwp : float array
    clwp : float array
    reic : float array
    relq : float array
    tauc : float array
    tauaer : float array
    uflx : float array
    dflx : float array
    hr : float array
    uflxc : float array
    dflxc : float array
    hrc : float array
    duflx_dt : float array
    duflxc_dt : float array
    
    """
    _Driver.f90wrap_driver(ncol=ncol, nlay=nlay, icld=icld, permuteseed=permuteseed, \
        irng=irng, idrv=idrv, cpdair=cpdair, play=play, plev=plev, tlay=tlay, \
        tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, o3vmr=o3vmr, co2vmr=co2vmr, \
        ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, cfc11vmr=cfc11vmr, \
        cfc12vmr=cfc12vmr, cfc22vmr=cfc22vmr, ccl4vmr=ccl4vmr, emis=emis, \
        inflglw=inflglw, iceflglw=iceflglw, liqflglw=liqflglw, cldfrac=cldfrac, \
        ciwp=ciwp, clwp=clwp, reic=reic, relq=relq, tauc=tauc, tauaer=tauaer, \
        uflx=uflx, dflx=dflx, hr=hr, uflxc=uflxc, dflxc=dflxc, hrc=hrc, \
        duflx_dt=duflx_dt, duflxc_dt=duflxc_dt)

