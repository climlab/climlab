from __future__ import division
import numpy as np
from climlab import constants as const
from climlab.radiation.radiation import _Radiation_LW
from utils import _prepare_general_arguments
from utils import _climlab_to_rrtm, _climlab_to_rrtm_sfc, _rrtm_to_climlab
#  The compiled fortran extension module
import _rrtmg_lw
nbndlw = int(_rrtmg_lw.parrrtm.nbndlw)
ngptlw = int(_rrtmg_lw.parrrtm.ngptlw)
#  Python-based initialization of absorption data from netcdf file
import _rrtmg_init
_rrtmg_init.init_lw(_rrtmg_lw)


class RRTMG_LW(_Radiation_LW):
    def __init__(self,
            # GENERAL, used in both SW and LW
            icld = 1,    # Cloud overlap method, 0: Clear only, 1: Random, 2,  Maximum/random] 3: Maximum
            irng = 1,  # more monte carlo stuff
            idrv = 0,  # whether to also calculate the derivative of flux with respect to surface temp
            permuteseed =  300,
            inflglw  = 2,
            iceflglw = 1,
            liqflglw = 1,
            tauc = 0.,  # in-cloud optical depth
            tauaer = 0.,   # Aerosol optical depth at mid-point of LW spectral bands
            **kwargs):
        super(RRTMG_LW, self).__init__(**kwargs)
        #  define INPUTS
        self.add_input('icld', icld)
        self.add_input('irng', irng)
        self.add_input('idrv', idrv)
        self.add_input('permuteseed', permuteseed)
        self.add_input('inflglw', inflglw)
        self.add_input('iceflglw', iceflglw)
        self.add_input('liqflglw', liqflglw)
        self.add_input('tauc', tauc)
        self.add_input('tauaer', tauaer)

    def _prepare_lw_arguments(self):
        #  scalar integer arguments
        icld = self.icld
        irng = self.irng
        idrv = self.idrv
        permuteseed = self.permuteseed
        inflglw = self.inflglw
        iceflglw = self.iceflglw
        liqflglw = self.liqflglw

        (ncol, nlay, play, plev, tlay, tlev, tsfc,
        h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr,
        cfc12vmr, cfc12vmr, cfc22vmr, ccl4vmr,
        cldfrac, ciwp, clwp, relq, reic) = _prepare_general_arguments(self)
        # surface emissivity
        emis = self.emissivity * np.ones((ncol,nbndlw))
        #  These arrays have an extra dimension for number of bands
        # in-cloud optical depth [nbndlw,ncol,nlay]
        tauc = _climlab_to_rrtm(self.tauc * np.ones_like(self.Tatm))
        #  broadcast to get [nbndlw,ncol,nlay]
        tauc = tauc * np.ones([nbndlw,ncol,nlay])
        # Aerosol optical depth at mid-point of LW spectral bands [ncol,nlay,nbndlw]
        tauaer = _climlab_to_rrtm(self.tauaer * np.ones_like(self.Tatm))
        #  broadcast and transpose to get [ncol,nlay,nbndlw]
        tauaer = np.transpose(tauaer * np.ones([nbndlw,ncol,nlay]), (1,2,0))
        args = [ncol, nlay, icld, permuteseed, irng, idrv, const.cp,
                play, plev, tlay, tlev, tsfc,
                h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,
                cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis,
                inflglw, iceflglw, liqflglw,
                cldfrac, ciwp, clwp, reic, relq, tauc, tauaer,]
        return args

    def _compute_heating_rates(self):
        '''Prepare arguments and call the RRTGM_LW driver to calculate
        radiative fluxes and heating rates'''
        #args = self._prepare_lw_arguments()
        (ncol, nlay, icld, permuteseed, irng, idrv, cp,
                play, plev, tlay, tlev, tsfc,
                h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,
                cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis,
                inflglw, iceflglw, liqflglw,
                cldfrac, ciwp, clwp, reic, relq, tauc, tauaer,) = self._prepare_lw_arguments()
        #    ! Call the Monte Carlo Independent Column Approximation
        #    !   (McICA, Pincus et al., JC, 2003)
        #cldfmcl = np.zeros((ngptlw,ncol,nlay), dtype='float64', order='F')
        #ciwpmcl = np.zeros((ngptlw,ncol,nlay), dtype='float64', order='F')
        #clwpmcl = np.zeros((ngptlw,ncol,nlay), dtype='float64', order='F')
        #reicmcl = np.zeros((ncol,nlay), dtype='float64', order='F')
        #relqmcl = np.zeros((ncol,nlay), dtype='float64', order='F')
        #taucmcl = np.zeros((ngptlw,ncol,nlay), dtype='float64', order='F')
        #ncol = np.array(ncol, dtype='int32', order='F')
        #nlay = np.array(nlay, dtype='int32', order='F')
        #icld = np.atleast_1d(icld).astype('int16', order='F')
        #icld = np.array(icld, dtype='int16', order='F')
        #permuteseed = np.array(permuteseed, dtype='int32', order='F')
        #irng = np.array(irng, dtype='int16', order='F')
        #irng = np.atleast_1d(irng).astype('int16', order='F')
        # play = play.astype('float32', order='F')
        # cldfrac = cldfrac.astype('float32', order='F')
        # ciwp = ciwp.astype('float32', order='F')
        # clwp = clwp.astype('float32', order='F')
        # reic = reic.astype('float32', order='F')
        # relq = relq.astype('float32', order='F')
        # tauc = tauc.astype('float32', order='F')
        # _rrtmg_lw.climlab_mcica_subcol_lw(ncol, nlay, icld, permuteseed, irng, play,
        #                    cldfrac, ciwp, clwp, reic, relq, tauc, cldfmcl,
        #                    ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl)
        (cldfmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl) = \
            _rrtmg_lw.climlab_mcica_subcol_lw(ncol, nlay, icld,
                                              permuteseed, irng, play,
                                              cldfrac, ciwp, clwp, reic, relq, tauc)
        # #  Initialize return arrays
        # uflx = np.zeros((ncol,nlay+1), dtype='float64', order='F')
        # dflx = np.zeros((ncol,nlay+1), dtype='float64', order='F')
        # hr = np.zeros((ncol,nlay), dtype='float64', order='F')
        # uflxc = np.zeros((ncol,nlay+1), dtype='float64', order='F')
        # dflxc = np.zeros((ncol,nlay+1), dtype='float64', order='F')
        # hrc = np.zeros((ncol,nlay), dtype='float64', order='F')
        # duflx_dt = np.zeros((ncol,nlay+1), dtype='float64', order='F')
        # duflxc_dt = np.zeros((ncol,nlay+1), dtype='float64', order='F')

        #!  Call the RRTMG_LW driver to compute radiative fluxes
        # _rrtmg_lw.climlab_rrtmg_lw(ncol    ,nlay    ,icld    ,idrv    ,
        #          play    , plev    , tlay    , tlev    , tsfc    ,
        #          h2ovmr  , o3vmr   , co2vmr  , ch4vmr  , n2ovmr  , o2vmr ,
        #          cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr , emis    ,
        #          inflglw , iceflglw, liqflglw, cldfmcl ,
        #          taucmcl , ciwpmcl , clwpmcl , reicmcl , relqmcl ,
        #          tauaer  ,
        #          uflx    , dflx    , hr      , uflxc   , dflxc,  hrc,
        #          duflx_dt,duflxc_dt )

        # cldfmcl = np.zeros((ngptlw,ncol,nlay), dtype='float64', order='F')
        # ciwpmcl = np.zeros((ngptlw,ncol,nlay), dtype='float64', order='F')
        # clwpmcl = np.zeros((ngptlw,ncol,nlay), dtype='float64', order='F')
        # reicmcl = np.zeros((ncol,nlay), dtype='float64', order='F')
        # relqmcl = np.zeros((ncol,nlay), dtype='float64', order='F')
        # taucmcl = np.zeros((ngptlw,ncol,nlay), dtype='float64', order='F')
        # cldfmcl_ = cldfmcl.astype('float64', order='F')
        # taucmcl = taucmcl.astype('float64', order='F')
        # ciwpmcl = ciwpmcl.astype('float64', order='F')
        # clwpmcl = clwpmcl.astype('float64', order='F')
        # reicmcl = reicmcl.astype('float64', order='F')
        # relqmcl = relqmcl.astype('float64', order='F')

        # cldfmcl = cldfmcl_out.copy()
        # taucmcl = taucmcl_out.copy()
        # ciwpmcl = ciwpmcl_out.copy()
        # clwpmcl = clwpmcl_out.copy()
        # reicmcl = reicmcl_out.copy()
        # relqmcl = relqmcl_out.copy()

        (uflx, dflx, hr, uflxc, dflxc, hrc, duflx_dt, duflxc_dt) = \
            _rrtmg_lw.climlab_rrtmg_lw(ncol    ,nlay    ,icld    ,idrv    ,
                 play    , plev    , tlay    , tlev    , tsfc    ,
                 h2ovmr  , o3vmr   , co2vmr  , ch4vmr  , n2ovmr  , o2vmr ,
                 cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr , emis    ,
                 inflglw , iceflglw, liqflglw, cldfmcl ,
                 taucmcl , ciwpmcl , clwpmcl , reicmcl , relqmcl ,
                 tauaer )

        #  Call the RRTM code!
        #uflx, dflx, hr, uflxc, dflxc, hrc, duflx_dt, duflxc_dt = lwdriver(*args)
        #  Output is all (ncol,nlay+1) or (ncol,nlay)
        self.LW_flux_up = _rrtm_to_climlab(uflx)
        self.LW_flux_down = _rrtm_to_climlab(dflx)
        self.LW_flux_up_clr = _rrtm_to_climlab(uflxc)
        self.LW_flux_down_clr = _rrtm_to_climlab(dflxc)
        #  Compute quantities derived from fluxes, including OLR
        self._compute_LW_flux_diagnostics()
        #  calculate heating rates from flux divergence
        LWheating_Wm2 = np.diff(self.LW_flux_net, axis=-1)
        LWheating_clr_Wm2 = np.diff(self.LW_flux_net_clr, axis=-1)
        self.heating_rate['Ts'] = -self.LW_flux_net[..., -1, np.newaxis]
        self.heating_rate['Tatm'] = LWheating_Wm2
        #  Convert to K / day
        Catm = self.Tatm.domain.heat_capacity
        self.TdotLW = LWheating_Wm2 / Catm * const.seconds_per_day
        self.TdotLW_clr = LWheating_clr_Wm2 / Catm * const.seconds_per_day
        #self.TdotLW = _rrtm_to_climlab(hr)  # heating rate in K/day
        #self.TdotLW_clr = _rrtm_to_climlab(hrc)  # clear-sky heating rate in K/day
