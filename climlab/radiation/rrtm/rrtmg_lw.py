from __future__ import division, print_function, absolute_import
import numpy as np
import warnings
from climlab import constants as const
from climlab.radiation.radiation import _Radiation_LW
from climlab.domain import Field, Axis, domain
from .utils import _prepare_general_arguments
from .utils import _climlab_to_rrtm, _rrtm_to_climlab
# These values will get overridden by reading from Fortran extension
nbndlw = 1; ngptlw = 1;
try:
    #  The compiled fortran extension module
    from . import _rrtmg_lw
    nbndlw = int(_rrtmg_lw.parrrtm.nbndlw)
    ngptlw = int(_rrtmg_lw.parrrtm.ngptlw)
    #  Initialize absorption data
    _rrtmg_lw.climlab_rrtmg_lw_ini(const.cp)
except:
    warnings.warn('Cannot import and initialize compiled Fortran extension, RRTMG_LW module will not be functional.')

# Longwave spectral band limits (wavenumbers in cm^-1)
# Data copied from rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90
wavenum_bounds = np.array([  10., 350., 500., 630., 700., 820.,
                      980.,1080.,1180.,1390.,1480.,1800.,
                     2080.,2250.,2380.,2600.,3250.])
wavenum_delta = np.diff(wavenum_bounds)


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
            return_spectral_olr = False, # Whether or not to return OLR averaged over each band
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
        self.add_input('return_spectral_olr', return_spectral_olr)

        # Spectrally-decomposed OLR
        if self.return_spectral_olr:
            # Adjust output flag
            self._ispec = 1  # Spectral OLR output flag, 0: only calculate total fluxes, 1: also return spectral OLR
            # set up appropriately sized Field object to store the spectral OLR diagnostic
            wavenum_ax = Axis(axis_type='abstract', bounds=wavenum_bounds)
            spectral_axes = {**self.OLR.domain.axes, 'wavenumber': wavenum_ax}
            spectral_domain = domain._Domain(axes=spectral_axes)
            #  HACK need to reorder axes in the domain object
            shape = list(self.OLR.shape)
            shape.append(wavenum_ax.num_points)
            spectral_domain.shape = tuple(shape)
            spectral_domain.axis_index = {**self.OLR.domain.axis_index, 'wavenumber': len(shape)-1}
            # This ensures that the spectral dimension (length: nbndlw) is appended after existing grid dimensions
            blank_field = Field((self.OLR[...,np.newaxis] * wavenum_delta), domain=spectral_domain)
            self.add_diagnostic('OLR_spectral', blank_field)
        else:
            self._ispec = 0,  # Spectral OLR output flag, 0: only calculate total fluxes, 1: also return spectral OLR


    def _prepare_lw_arguments(self):
        #  scalar integer arguments
        icld  = self.icld
        ispec = self._ispec
        irng  = self.irng
        idrv  = self.idrv
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
        args = [ncol, nlay, icld, ispec, permuteseed, irng, idrv, const.cp,
                play, plev, tlay, tlev, tsfc,
                h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,
                cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis,
                inflglw, iceflglw, liqflglw,
                cldfrac, ciwp, clwp, reic, relq, tauc, tauaer,]
        return args

    def _compute_heating_rates(self):
        '''Prepare arguments and call the RRTGM_LW driver to calculate
        radiative fluxes and heating rates'''
        (ncol, nlay, icld, ispec, permuteseed, irng, idrv, cp,
                play, plev, tlay, tlev, tsfc,
                h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,
                cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis,
                inflglw, iceflglw, liqflglw,
                cldfrac, ciwp, clwp, reic, relq, tauc, tauaer,) = self._prepare_lw_arguments()
        if icld == 0:  # clear-sky only
            cldfmcl = np.zeros((ngptlw,ncol,nlay))
            ciwpmcl = np.zeros((ngptlw,ncol,nlay))
            clwpmcl = np.zeros((ngptlw,ncol,nlay))
            reicmcl = np.zeros((ncol,nlay))
            relqmcl = np.zeros((ncol,nlay))
            taucmcl = np.zeros((ngptlw,ncol,nlay))
        else:
            #  Call the Monte Carlo Independent Column Approximation (McICA, Pincus et al., JC, 2003)
            (cldfmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl) = \
                _rrtmg_lw.climlab_mcica_subcol_lw(
                            ncol, nlay, icld,
                            permuteseed, irng, play,
                            cldfrac, ciwp, clwp, reic, relq, tauc)
            #  Call the RRTMG_LW driver to compute radiative fluxes
        (olr_sr, uflx, dflx, hr, uflxc, dflxc, hrc, duflx_dt, duflxc_dt) = \
            _rrtmg_lw.climlab_rrtmg_lw(ncol, nlay, icld, ispec, idrv,
                 play, plev, tlay, tlev, tsfc,
                 h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,
                 cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis,
                 inflglw, iceflglw, liqflglw, cldfmcl,
                 taucmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl,
                 tauaer)
        #  Output is all (ncol,nlay+1) or (ncol,nlay)
        self.LW_flux_up = _rrtm_to_climlab(uflx) + 0.*self.LW_flux_up
        self.LW_flux_down = _rrtm_to_climlab(dflx) + 0.*self.LW_flux_down
        self.LW_flux_up_clr = _rrtm_to_climlab(uflxc) + 0.*self.LW_flux_up_clr
        self.LW_flux_down_clr = _rrtm_to_climlab(dflxc) + 0.*self.LW_flux_down_clr
        #  Compute quantities derived from fluxes, including OLR
        self._compute_LW_flux_diagnostics()
        # Except for spectrally-decomposed TOA flux, olr_sr (ncol, nbndlw)
        if self.return_spectral_olr:
            #  Need to deal with broadcasting for two different cases: single column and latitude axis
            # case single column: self.OLR is (1,),  self.OLR_spectral is (1, nbndlw),  olr_sr is (1,nbndlw)
            #  squeeze olr_sr down to (nbndlw,)
            # then use np.squeeze(olr_sr)[..., np.newaxis, :] to get back to (1, nbndlw)
            # case latitude axis: self.OLR is (num_lat,1), self.OLR_spectral is (num_lat, 1, nbndlw), olr_sr is (num_lat, nbndlw)
            #  np.squeeze(olr_sr) has no effect in this case
            # add the newaxis because the domain has a size-1 depth axis ---> (num_lat, 1, nbndlw)
            self.OLR_spectral = np.squeeze(olr_sr)[...,np.newaxis,:] + 0.*self.OLR_spectral
        #  calculate heating rates from flux divergence
        LWheating_Wm2 = np.array(np.diff(self.LW_flux_net, axis=-1)) + 0.*self.Tatm
        LWheating_clr_Wm2 = np.array(np.diff(self.LW_flux_net_clr, axis=-1)) + 0.*self.Tatm
        self.heating_rate['Ts'] = np.array(-self.LW_flux_net[..., -1, np.newaxis]) + 0.*self.Ts
        self.heating_rate['Tatm'] = LWheating_Wm2
        #  Convert to K / day
        Catm = self.Tatm.domain.heat_capacity
        self.TdotLW = LWheating_Wm2 / Catm * const.seconds_per_day
        self.TdotLW_clr = LWheating_clr_Wm2 / Catm * const.seconds_per_day
