from __future__ import division
import numpy as np
from climlab import constants as const
from climlab.radiation.radiation import _Radiation_SW
from utils import _prepare_general_arguments
from utils import _climlab_to_rrtm, _climlab_to_rrtm_sfc, _rrtm_to_climlab
from . import nbndsw, naerec
from _rrtmg_sw import driver as swdriver

class RRTMG_SW(_Radiation_SW):
    def __init__(self,
            # GENERAL, used in both SW and LW
            icld = 1,    # Cloud overlap method, 0: Clear only, 1: Random, 2,  Maximum/random] 3: Maximum
            irng = 1,  # more monte carlo stuff
            idrv = 0,  # whether to also calculate the derivative of flux with respect to surface temp
            permuteseed = 150,
            dyofyr = 0,       # day of the year used to get Earth/Sun distance (if not adjes)
            inflgsw  = 2,
            iceflgsw = 1,
            liqflgsw = 1,
            tauc = 0.,
            ssac = 0.,  # In-cloud single scattering albedo
            asmc = 0.,  # In-cloud asymmetry parameter
            fsfc = 0.,  # In-cloud forward scattering fraction (delta function pointing forward "forward peaked scattering")
            # AEROSOLS
            tauaer = 0.,   # Aerosol optical depth (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
            ssaaer = 0.,   # Aerosol single scattering albedo (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
            asmaer = 0.,   # Aerosol asymmetry parameter (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
            ecaer  = 0.,   # Aerosol optical depth at 0.55 micron (iaer=6 only), Dimensions,  (ncol,nlay,naerec)] #  (non-delta scaled)
            # new arguments for RRTMG_SW version 4.0
            isolvar = 0,    # ! Flag for solar variability method
                            # !   -1 = (when scon .eq. 0.0): No solar variability
                            # !        and no solar cycle (Kurucz solar irradiance
                            # !        of 1368.22 Wm-2 only);
                            # !        (when scon .ne. 0.0): Kurucz solar irradiance
                            # !        scaled to scon and solar variability defined
                            # !        (optional) by setting non-zero scale factors
                            # !        for each band in bndsolvar
                            # !    0 = (when SCON .eq. 0.0): No solar variability
                            # !        and no solar cycle (NRLSSI2 solar constant of
                            # !        1360.85 Wm-2 for the 100-50000 cm-1 spectral
                            # !        range only), with facular and sunspot effects
                            # !        fixed to the mean of Solar Cycles 13-24;
                            # !        (when SCON .ne. 0.0): No solar variability
                            # !        and no solar cycle (NRLSSI2 solar constant of
                            # !        1360.85 Wm-2 for the 100-50000 cm-1 spectral
                            # !        range only), is scaled to SCON
                            # !    1 = Solar variability (using NRLSSI2  solar
                            # !        model) with solar cycle contribution
                            # !        determined by fraction of solar cycle
                            # !        with facular and sunspot variations
                            # !        fixed to their mean variations over the
                            # !        average of Solar Cycles 13-24;
                            # !        two amplitude scale factors allow
                            # !        facular and sunspot adjustments from
                            # !        mean solar cycle as defined by indsolvar
                            # !    2 = Solar variability (using NRLSSI2 solar
                            # !        model) over solar cycle determined by
                            # !        direct specification of Mg (facular)
                            # !        and SB (sunspot) indices provided
                            # !        in indsolvar (scon = 0.0 only)
                            # !    3 = (when scon .eq. 0.0): No solar variability
                            # !        and no solar cycle (NRLSSI2 solar irradiance
                            # !        of 1360.85 Wm-2 only);
                            # !        (when scon .ne. 0.0): NRLSSI2 solar irradiance
                            # !        scaled to scon and solar variability defined
                            # !        (optional) by setting non-zero scale factors
                            # !        for each band in bndsolvar
            indsolvar = np.zeros(2),      # Facular and sunspot amplitude scale factors (isolvar=1),
                                          # or Mg and SB indices (isolvar=2)
            bndsolvar = np.zeros(nbndsw), # Solar variability scale factors for each shortwave band
            solcycfrac = 0.,              # Fraction of averaged solar cycle (0-1) at current time (isolvar=1)
            **kwargs):
        super(RRTMG_SW, self).__init__(**kwargs)
        #  define INPUTS
        self.add_input('icld', icld)
        self.add_input('irng', irng)
        self.add_input('idrv', idrv)
        self.add_input('permuteseed', permuteseed)
        self.add_input('dyofyr', dyofyr)
        self.add_input('inflgsw', inflgsw)
        self.add_input('iceflgsw', iceflgsw)
        self.add_input('liqflgsw', liqflgsw)
        self.add_input('tauc', tauc)
        self.add_input('ssac', ssac)
        self.add_input('asmc', asmc)
        self.add_input('fsfc', fsfc)
        self.add_input('tauaer', tauaer)
        self.add_input('ssaaer', ssaaer)
        self.add_input('asmaer', asmaer)
        self.add_input('ecaer', ecaer)
        self.add_input('isolvar', isolvar)
        self.add_input('indsolvar', indsolvar)
        self.add_input('bndsolvar', bndsolvar)
        self.add_input('solcycfrac', solcycfrac)

    def _prepare_sw_arguments(self):
        #  prepare insolation
        #  CLIMLAB provides local insolation, solar constant and
        #  (optionally, maybe, not yet implemented) the adjustment factor for Sun-Earth distance
        #  which is (dbar / d)^2 in Hartmann's notation
        #   the factor by which the total irradiance differs from mean annual solar constant
        #   due to time of year and elliptical orbit
        #
        #  RRTMG_SW expects solar constant, adjustment factor, and cosine of zenith angle
        #
        #  actually for now let's just let all these things be specified as inputs

        #  scalar integer arguments
        icld = self.icld
        irng = self.irng
        idrv = self.idrv
        permuteseed = self.permuteseed
        inflgsw = self.inflgsw
        iceflgsw = self.iceflgsw
        liqflgsw = self.liqflgsw
        dyofyr = self.dyofyr
        isolvar = self.isolvar
        solcycfrac = self.solcycfrac
        #  scalar real arguments
        adjes = self.eccentricity_factor
        scon = self.S0
        indsolvar = self.indsolvar
        bndsolvar = self.bndsolvar

        (ncol, nlay, play, plev, tlay, tlev, tsfc,
        h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr,
        cfc12vmr, cfc12vmr, cfc22vmr, ccl4vmr,
        cldfrac, ciwp, clwp, relq, reic) = _prepare_general_arguments(self)

        aldif = _climlab_to_rrtm_sfc(self.aldif * np.ones_like(self.Ts))
        aldir = _climlab_to_rrtm_sfc(self.aldir * np.ones_like(self.Ts))
        asdif = _climlab_to_rrtm_sfc(self.asdif * np.ones_like(self.Ts))
        asdir = _climlab_to_rrtm_sfc(self.asdir * np.ones_like(self.Ts))
        coszen = _climlab_to_rrtm_sfc(self.coszen * np.ones_like(self.Ts))
        #  These arrays have an extra dimension for number of bands
        # in-cloud optical depth [nbndsw,ncol,nlay]
        tauc = _climlab_to_rrtm(self.tauc * np.ones_like(self.Tatm))
        #  broadcast to get [nbndsw,ncol,nlay]
        tauc = tauc * np.ones([nbndsw,ncol,nlay])
        # In-cloud single scattering albedo, same operation
        ssac = _climlab_to_rrtm(self.ssac * np.ones_like(self.Tatm)) * np.ones([nbndsw,ncol,nlay])
        # In-cloud asymmetry parameter
        asmc = _climlab_to_rrtm(self.asmc * np.ones_like(self.Tatm)) * np.ones([nbndsw,ncol,nlay])
        # In-cloud forward scattering fraction (delta function pointing forward "forward peaked scattering")
        fsfc = _climlab_to_rrtm(self.fsfc * np.ones_like(self.Tatm)) * np.ones([nbndsw,ncol,nlay])
        # Aerosol optical depth (iaer=10 only), (ncol,nlay,nbndsw)] #  (non-delta scaled)
        tauaer = _climlab_to_rrtm(self.tauaer * np.ones_like(self.Tatm))
        #  broadcast and transpose to get [ncol,nlay,nbndsw]
        tauaer = np.transpose(tauaer * np.ones([nbndsw,ncol,nlay]), (1,2,0))
        # Aerosol single scattering albedo (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
        ssaaer = np.transpose(_climlab_to_rrtm(self.ssaaer * np.ones_like(self.Tatm)) * np.ones([nbndsw,ncol,nlay]), (1,2,0))
        # Aerosol asymmetry parameter (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
        asmaer = np.transpose(_climlab_to_rrtm(self.asmaer * np.ones_like(self.Tatm)) * np.ones([nbndsw,ncol,nlay]), (1,2,0))
        # Aerosol optical depth at 0.55 micron (iaer=6 only), Dimensions,  (ncol,nlay,naerec)] #  (non-delta scaled)
        ecaer = np.transpose(_climlab_to_rrtm(self.ecaer * np.ones_like(self.Tatm)) * np.ones([naerec,ncol,nlay]), (1,2,0))

        args = [ncol, nlay, icld, permuteseed, irng, idrv, const.cp,
                play, plev, tlay, tlev, tsfc,
                h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,
                aldif, aldir, asdif, asdir, coszen, adjes, dyofyr, scon, isolvar,
                indsolvar, bndsolvar, solcycfrac,
                inflgsw, iceflgsw, liqflgsw,
                cldfrac, ciwp, clwp, reic, relq, tauc, ssac, asmc, fsfc,
                tauaer, ssaaer, asmaer, ecaer,]

        return args

    def _compute_heating_rates(self):
        '''Prepare arguments and call the RRTGM_SW driver to calculate
        radiative fluxes and heating rates'''
        args = self._prepare_sw_arguments()
        #  Call the RRTM code!
        swuflx, swdflx, swhr, swuflxc, swdflxc, swhrc = swdriver(*args)
        #  Output is all (ncol,nlay+1) or (ncol,nlay)
        self.SW_flux_up = _rrtm_to_climlab(swuflx)
        self.SW_flux_down = _rrtm_to_climlab(swdflx)
        self.SW_flux_up_clr = _rrtm_to_climlab(swuflxc)
        self.SW_flux_down_clr = _rrtm_to_climlab(swdflxc)
        #  Compute quantities derived from fluxes, including ASR
        self._compute_SW_flux_diagnostics()
        #  calculate heating rates from flux divergence
        self.heating_rate['Ts'] = self.SW_flux_net[..., -1, np.newaxis]
        self.heating_rate['Tatm'] = -np.diff(self.SW_flux_net, axis=-1)
        self.TdotSW = _rrtm_to_climlab(swhr)  # heating rate in K/day
        self.TdotSW_clr = _rrtm_to_climlab(swhrc)  # clear-sky heating rate in K/day