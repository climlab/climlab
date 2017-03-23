import _rrtmg_lw
import f90wrap.runtime
import logging

class Rrtmg_Lw_Init(f90wrap.runtime.FortranModule):
    """
    Module rrtmg_lw_init
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 6-2657
    
    """
    @staticmethod
    def rrtmg_lw_ini(cpdair):
        """
        rrtmg_lw_ini(cpdair)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 28-176
        
        Parameters
        ----------
        cpdair : float
        
        """
        _rrtmg_lw.f90wrap_rrtmg_lw_ini(cpdair=cpdair)
    
    @staticmethod
    def lwdatinit(cpdair):
        """
        lwdatinit(cpdair)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 179-282
        
        Parameters
        ----------
        cpdair : float
        
        """
        _rrtmg_lw.f90wrap_lwdatinit(cpdair=cpdair)
    
    @staticmethod
    def lwcmbdat():
        """
        lwcmbdat()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 285-364
        
        
        """
        _rrtmg_lw.f90wrap_lwcmbdat()
    
    @staticmethod
    def cmbgb1():
        """
        cmbgb1()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 367-476
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb1()
    
    @staticmethod
    def cmbgb2():
        """
        cmbgb2()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 479-559
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb2()
    
    @staticmethod
    def cmbgb3():
        """
        cmbgb3()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 562-689
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb3()
    
    @staticmethod
    def cmbgb4():
        """
        cmbgb4()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 692-788
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb4()
    
    @staticmethod
    def cmbgb5():
        """
        cmbgb5()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 791-914
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb5()
    
    @staticmethod
    def cmbgb6():
        """
        cmbgb6()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 917-1003
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb6()
    
    @staticmethod
    def cmbgb7():
        """
        cmbgb7()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1006-1127
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb7()
    
    @staticmethod
    def cmbgb8():
        """
        cmbgb8()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1130-1246
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb8()
    
    @staticmethod
    def cmbgb9():
        """
        cmbgb9()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1249-1371
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb9()
    
    @staticmethod
    def cmbgb10():
        """
        cmbgb10()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1374-1458
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb10()
    
    @staticmethod
    def cmbgb11():
        """
        cmbgb11()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1461-1561
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb11()
    
    @staticmethod
    def cmbgb12():
        """
        cmbgb12()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1564-1633
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb12()
    
    @staticmethod
    def cmbgb13():
        """
        cmbgb13()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1636-1746
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb13()
    
    @staticmethod
    def cmbgb14():
        """
        cmbgb14()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1749-1833
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb14()
    
    @staticmethod
    def cmbgb15():
        """
        cmbgb15()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1836-1920
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb15()
    
    @staticmethod
    def cmbgb16():
        """
        cmbgb16()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1923-2016
        
        
        """
        _rrtmg_lw.f90wrap_cmbgb16()
    
    @staticmethod
    def lwcldpr():
        """
        lwcldpr()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 2019-2657
        
        
        """
        _rrtmg_lw.f90wrap_lwcldpr()
    
    _dt_array_initialisers = []
    

rrtmg_lw_init = Rrtmg_Lw_Init()

class Rrtmg_Lw_Rad(f90wrap.runtime.FortranModule):
    """
    Module rrtmg_lw_rad
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90 lines 7-906
    
    """
    @staticmethod
    def rrtmg_lw(ncol, nlay, icld, idrv, play, plev, tlay, tlev, tsfc, h2ovmr, \
        o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, \
        emis, inflglw, iceflglw, liqflglw, cldfmcl, taucmcl, ciwpmcl, clwpmcl, \
        reicmcl, relqmcl, tauaer, uflx, dflx, hr, uflxc, dflxc, hrc, duflx_dt=None, \
        duflxc_dt=None):
        """
        rrtmg_lw(ncol, nlay, icld, idrv, play, plev, tlay, tlev, tsfc, h2ovmr, o3vmr, \
            co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis, \
            inflglw, iceflglw, liqflglw, cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, \
            relqmcl, tauaer, uflx, dflx, hr, uflxc, dflxc, hrc[, duflx_dt, duflxc_dt])
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90 lines 90-577
        
        Parameters
        ----------
        ncol : int
        nlay : int
        icld : int
        idrv : int
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
        cldfmcl : float array
        taucmcl : float array
        ciwpmcl : float array
        clwpmcl : float array
        reicmcl : float array
        relqmcl : float array
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
        _rrtmg_lw.f90wrap_rrtmg_lw(ncol=ncol, nlay=nlay, icld=icld, idrv=idrv, \
            play=play, plev=plev, tlay=tlay, tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, \
            o3vmr=o3vmr, co2vmr=co2vmr, ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, \
            cfc11vmr=cfc11vmr, cfc12vmr=cfc12vmr, cfc22vmr=cfc22vmr, ccl4vmr=ccl4vmr, \
            emis=emis, inflglw=inflglw, iceflglw=iceflglw, liqflglw=liqflglw, \
            cldfmcl=cldfmcl, taucmcl=taucmcl, ciwpmcl=ciwpmcl, clwpmcl=clwpmcl, \
            reicmcl=reicmcl, relqmcl=relqmcl, tauaer=tauaer, uflx=uflx, dflx=dflx, \
            hr=hr, uflxc=uflxc, dflxc=dflxc, hrc=hrc, duflx_dt=duflx_dt, \
            duflxc_dt=duflxc_dt)
    
    @staticmethod
    def inatm(iplon, nlay, icld, iaer, play, plev, tlay, tlev, tsfc, h2ovmr, o3vmr, \
        co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis, \
        inflglw, iceflglw, liqflglw, cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, \
        relqmcl, tauaer, pavel, pz, tavel, tz, semiss, coldry, wkl, wbrodl, wx, \
        cldfmc, taucmc, ciwpmc, clwpmc, reicmc, relqmc, taua):
        """
        nlayers, tbound, pwvcm, inflag, iceflag, liqflag = inatm(iplon, nlay, icld, \
            iaer, play, plev, tlay, tlev, tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, \
            o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, \
            liqflglw, cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, tauaer, \
            pavel, pz, tavel, tz, semiss, coldry, wkl, wbrodl, wx, cldfmc, taucmc, \
            ciwpmc, clwpmc, reicmc, relqmc, taua)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90 lines 587-906
        
        Parameters
        ----------
        iplon : int
        nlay : int
        icld : int
        iaer : int
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
        cldfmcl : float array
        taucmcl : float array
        ciwpmcl : float array
        clwpmcl : float array
        reicmcl : float array
        relqmcl : float array
        tauaer : float array
        pavel : float array
        pz : float array
        tavel : float array
        tz : float array
        semiss : float array
        coldry : float array
        wkl : float array
        wbrodl : float array
        wx : float array
        cldfmc : float array
        taucmc : float array
        ciwpmc : float array
        clwpmc : float array
        reicmc : float array
        relqmc : float array
        taua : float array
        
        Returns
        -------
        nlayers : int
        tbound : float
        pwvcm : float
        inflag : int
        iceflag : int
        liqflag : int
        
        """
        nlayers, tbound, pwvcm, inflag, iceflag, liqflag = \
            _rrtmg_lw.f90wrap_inatm(iplon=iplon, nlay=nlay, icld=icld, iaer=iaer, \
            play=play, plev=plev, tlay=tlay, tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, \
            o3vmr=o3vmr, co2vmr=co2vmr, ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, \
            cfc11vmr=cfc11vmr, cfc12vmr=cfc12vmr, cfc22vmr=cfc22vmr, ccl4vmr=ccl4vmr, \
            emis=emis, inflglw=inflglw, iceflglw=iceflglw, liqflglw=liqflglw, \
            cldfmcl=cldfmcl, taucmcl=taucmcl, ciwpmcl=ciwpmcl, clwpmcl=clwpmcl, \
            reicmcl=reicmcl, relqmcl=relqmcl, tauaer=tauaer, pavel=pavel, pz=pz, \
            tavel=tavel, tz=tz, semiss=semiss, coldry=coldry, wkl=wkl, wbrodl=wbrodl, \
            wx=wx, cldfmc=cldfmc, taucmc=taucmc, ciwpmc=ciwpmc, clwpmc=clwpmc, \
            reicmc=reicmc, relqmc=relqmc, taua=taua)
        return nlayers, tbound, pwvcm, inflag, iceflag, liqflag
    
    _dt_array_initialisers = []
    

rrtmg_lw_rad = Rrtmg_Lw_Rad()

