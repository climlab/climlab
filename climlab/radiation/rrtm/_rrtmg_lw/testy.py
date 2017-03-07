import _testy
import f90wrap.runtime
import logging

class Mcica_Subcol_Gen_Lw(f90wrap.runtime.FortranModule):
    """
    Module mcica_subcol_gen_lw
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 7-563
    
    """
    @staticmethod
    def mcica_subcol_lw(iplon, ncol, nlay, icld, permuteseed, irng, play, cldfrac, \
        ciwp, clwp, rei, rel, tauc, cldfmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, \
        taucmcl):
        """
        mcica_subcol_lw(iplon, ncol, nlay, icld, permuteseed, irng, play, cldfrac, ciwp, \
            clwp, rei, rel, tauc, cldfmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 50-155
        
        Parameters
        ----------
        iplon : int
        ncol : int
        nlay : int
        icld : int
        permuteseed : int
        irng : int
        play : float array
        cldfrac : float array
        ciwp : float array
        clwp : float array
        rei : float array
        rel : float array
        tauc : float array
        cldfmcl : float array
        ciwpmcl : float array
        clwpmcl : float array
        reicmcl : float array
        relqmcl : float array
        taucmcl : float array
        
        """
        _testy.f90wrap_mcica_subcol_lw(iplon=iplon, ncol=ncol, nlay=nlay, icld=icld, \
            permuteseed=permuteseed, irng=irng, play=play, cldfrac=cldfrac, ciwp=ciwp, \
            clwp=clwp, rei=rei, rel=rel, tauc=tauc, cldfmcl=cldfmcl, ciwpmcl=ciwpmcl, \
            clwpmcl=clwpmcl, reicmcl=reicmcl, relqmcl=relqmcl, taucmcl=taucmcl)
    
    @staticmethod
    def generate_stochastic_clouds(ncol, nlay, nsubcol, icld, irng, pmid, cld, clwp, \
        ciwp, tauc, cld_stoch, clwp_stoch, ciwp_stoch, tauc_stoch, changeseed=None):
        """
        generate_stochastic_clouds(ncol, nlay, nsubcol, icld, irng, pmid, cld, clwp, \
            ciwp, tauc, cld_stoch, clwp_stoch, ciwp_stoch, tauc_stoch[, changeseed])
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 159-523
        
        Parameters
        ----------
        ncol : int
        nlay : int
        nsubcol : int
        icld : int
        irng : int
        pmid : float array
        cld : float array
        clwp : float array
        ciwp : float array
        tauc : float array
        cld_stoch : float array
        clwp_stoch : float array
        ciwp_stoch : float array
        tauc_stoch : float array
        changeseed : int
        
        \
            -------------------------------------------------------------------------------------------------
        \
            ----------------------------------------------------------------------------------------------------------------
         ---------------------
         Contact: Cecile Hannay (hannay@ucar.edu)
         Original code: Based on Raisanen et al., QJRMS, 2004.
         Modifications: Generalized for use with RRTMG and added Mersenne Twister as the \
             default
           random number generator, which can be changed to the optional kissvec random \
               number generator
           with flag 'irng'. Some extra functionality has been commented or removed.
           Michael J. Iacono, AER, Inc., February 2007
         Given a profile of cloud fraction, cloud water and cloud ice, we produce a set \
             of subcolumns.
         Each layer within each subcolumn is homogeneous, with cloud fraction equal to \
             zero or one
         and uniform cloud liquid and cloud ice concentration.
         The ensemble as a whole reproduces the probability function of cloud liquid and \
             ice within each layer
         and obeys an overlap assumption in the vertical.
         Overlap assumption:
          The cloud are consistent with 4 overlap assumptions: random, maximum, \
              maximum-random and exponential.
          The default option is maximum-random (option 3)
          The options are: 1=random overlap, 2=max/random, 3=maximum overlap, \
              4=exponential overlap
          This is set with the variable "overlap"
        mji - Exponential overlap option (overlap=4) has been deactivated in this \
            version
          The exponential overlap uses also a length scale, Zo. (real, parameter :: Zo = \
              2500. )
         Seed:
          If the stochastic cloud generator is called several times during the same \
              timestep,
          one should change the seed between the call to insure that the subcolumns are \
              different.
          This is done by changing the argument 'changeSeed'
          For example, if one wants to create a set of columns for the shortwave and \
              another set for the longwave ,
          use 'changeSeed = 1' for the first call and'changeSeed = 2' for the second call
         PDF assumption:
          We can use arbitrary complicated PDFS.
          In the present version, we produce homogeneuous clouds (the simplest case).
          Future developments include using the PDF scheme of Ben Johnson.
         History file:
          Option to add diagnostics variables in the history file. (using FINCL in the \
              namelist)
          nsubcol = number of subcolumns
          overlap = overlap type (1-3)
          Zo = length scale
          CLOUD_S = mean of the subcolumn cloud fraction ('_S" means Stochastic)
          CLDLIQ_S = mean of the subcolumn cloud water
          CLDICE_S = mean of the subcolumn cloud ice
         Note:
           Here: we force that the cloud condensate to be consistent with the cloud \
               fraction
           i.e we only have cloud condensate when the cell is cloudy.
           In CAM: The cloud condensate and the cloud fraction are obtained from 2 \
               different equations
           and the 2 quantities can be inconsistent (i.e. CAM can produce cloud fraction
           without cloud condensate or the opposite).
        \
            ---------------------------------------------------------------------------------------------------------------
        """
        _testy.f90wrap_generate_stochastic_clouds(ncol=ncol, nlay=nlay, nsubcol=nsubcol, \
            icld=icld, irng=irng, pmid=pmid, cld=cld, clwp=clwp, ciwp=ciwp, tauc=tauc, \
            cld_stoch=cld_stoch, clwp_stoch=clwp_stoch, ciwp_stoch=ciwp_stoch, \
            tauc_stoch=tauc_stoch, changeseed=changeseed)
    
    @staticmethod
    def kissvec(seed1, seed2, seed3, seed4, ran_arr):
        """
        kissvec(seed1, seed2, seed3, seed4, ran_arr)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 lines 530-562
        
        Parameters
        ----------
        seed1 : int array
        seed2 : int array
        seed3 : int array
        seed4 : int array
        ran_arr : float array
        
        \
            --------------------------------------------------------------------------------------------------
         public domain code
         made available from http://www.fortran.com/
         downloaded by pjr on 03/16/04 for NCAR CAM
         converted to vector form, functions inlined by pjr,mvr on 05/10/2004
         The  KISS (Keep It Simple Stupid) random number generator. Combines:
         (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
         (2) A 3-shift shift-register generator, period 2^32-1,
         (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
          Overall period>2^123;
        """
        _testy.f90wrap_kissvec(seed1=seed1, seed2=seed2, seed3=seed3, seed4=seed4, \
            ran_arr=ran_arr)
    
    _dt_array_initialisers = []
    

mcica_subcol_gen_lw = Mcica_Subcol_Gen_Lw()

class Rrtmg_Lw_Init(f90wrap.runtime.FortranModule):
    """
    Module rrtmg_lw_init
    
    
    Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 6-2657
    
    """
    @staticmethod
    def rrtmg_lw_ini(cpdair):
        """
        rrtmg_lw_ini(cpdair)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 28-175
        
        Parameters
        ----------
        cpdair : float
        
        """
        _testy.f90wrap_rrtmg_lw_ini(cpdair=cpdair)
    
    @staticmethod
    def lwdatinit(cpdair):
        """
        lwdatinit(cpdair)
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 178-281
        
        Parameters
        ----------
        cpdair : float
        
        """
        _testy.f90wrap_lwdatinit(cpdair=cpdair)
    
    @staticmethod
    def lwcmbdat():
        """
        lwcmbdat()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 284-363
        
        
        """
        _testy.f90wrap_lwcmbdat()
    
    @staticmethod
    def cmbgb1():
        """
        cmbgb1()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 366-475
        
        
        """
        _testy.f90wrap_cmbgb1()
    
    @staticmethod
    def cmbgb2():
        """
        cmbgb2()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 478-558
        
        
        """
        _testy.f90wrap_cmbgb2()
    
    @staticmethod
    def cmbgb3():
        """
        cmbgb3()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 561-688
        
        
        """
        _testy.f90wrap_cmbgb3()
    
    @staticmethod
    def cmbgb4():
        """
        cmbgb4()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 691-787
        
        
        """
        _testy.f90wrap_cmbgb4()
    
    @staticmethod
    def cmbgb5():
        """
        cmbgb5()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 790-913
        
        
        """
        _testy.f90wrap_cmbgb5()
    
    @staticmethod
    def cmbgb6():
        """
        cmbgb6()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 916-1002
        
        
        """
        _testy.f90wrap_cmbgb6()
    
    @staticmethod
    def cmbgb7():
        """
        cmbgb7()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1005-1126
        
        
        """
        _testy.f90wrap_cmbgb7()
    
    @staticmethod
    def cmbgb8():
        """
        cmbgb8()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1129-1245
        
        
        """
        _testy.f90wrap_cmbgb8()
    
    @staticmethod
    def cmbgb9():
        """
        cmbgb9()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1248-1370
        
        
        """
        _testy.f90wrap_cmbgb9()
    
    @staticmethod
    def cmbgb10():
        """
        cmbgb10()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1373-1457
        
        
        """
        _testy.f90wrap_cmbgb10()
    
    @staticmethod
    def cmbgb11():
        """
        cmbgb11()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1460-1560
        
        
        """
        _testy.f90wrap_cmbgb11()
    
    @staticmethod
    def cmbgb12():
        """
        cmbgb12()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1563-1632
        
        
        """
        _testy.f90wrap_cmbgb12()
    
    @staticmethod
    def cmbgb13():
        """
        cmbgb13()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1635-1745
        
        
        """
        _testy.f90wrap_cmbgb13()
    
    @staticmethod
    def cmbgb14():
        """
        cmbgb14()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1748-1832
        
        
        """
        _testy.f90wrap_cmbgb14()
    
    @staticmethod
    def cmbgb15():
        """
        cmbgb15()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1835-1919
        
        
        """
        _testy.f90wrap_cmbgb15()
    
    @staticmethod
    def cmbgb16():
        """
        cmbgb16()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 1922-2015
        
        
        """
        _testy.f90wrap_cmbgb16()
    
    @staticmethod
    def lwcldpr():
        """
        lwcldpr()
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 lines 2018-2656
        
        
        """
        _testy.f90wrap_lwcldpr()
    
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
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90 lines 89-576
        
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
        _testy.f90wrap_rrtmg_lw(ncol=ncol, nlay=nlay, icld=icld, idrv=idrv, play=play, \
            plev=plev, tlay=tlay, tlev=tlev, tsfc=tsfc, h2ovmr=h2ovmr, o3vmr=o3vmr, \
            co2vmr=co2vmr, ch4vmr=ch4vmr, n2ovmr=n2ovmr, o2vmr=o2vmr, cfc11vmr=cfc11vmr, \
            cfc12vmr=cfc12vmr, cfc22vmr=cfc22vmr, ccl4vmr=ccl4vmr, emis=emis, \
            inflglw=inflglw, iceflglw=iceflglw, liqflglw=liqflglw, cldfmcl=cldfmcl, \
            taucmcl=taucmcl, ciwpmcl=ciwpmcl, clwpmcl=clwpmcl, reicmcl=reicmcl, \
            relqmcl=relqmcl, tauaer=tauaer, uflx=uflx, dflx=dflx, hr=hr, uflxc=uflxc, \
            dflxc=dflxc, hrc=hrc, duflx_dt=duflx_dt, duflxc_dt=duflxc_dt)
    
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
        
        
        Defined at rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90 lines 586-905
        
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
            _testy.f90wrap_inatm(iplon=iplon, nlay=nlay, icld=icld, iaer=iaer, \
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

