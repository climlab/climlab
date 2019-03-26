from __future__ import division, print_function, absolute_import
import numpy as np
import warnings
from climlab import constants as const
from climlab.radiation.radiation import _Radiation_SW
from .utils import _prepare_general_arguments
from .utils import _climlab_to_rrtm, _climlab_to_rrtm_sfc, _rrtm_to_climlab
# These values will get overridden by reading from Fortran extension
nbndsw = 1; naerec = 1; ngptsw = 1;
try:
    #  The compiled fortran extension module
    from . import _rrtmg_sw
    nbndsw = int(_rrtmg_sw.parrrsw.nbndsw)
    naerec = int(_rrtmg_sw.parrrsw.naerec)
    ngptsw = int(_rrtmg_sw.parrrsw.ngptsw)
    #  Initialize absorption data
    _rrtmg_sw.climlab_rrtmg_sw_ini(const.cp)
except:
    warnings.warn('Cannot import and initialize compiled Fortran extension, RRTMG_SW module will not be functional.')


class RRTMG_SW(_Radiation_SW):
    def __init__(self,
            # GENERAL, used in both SW and LW
            icld = 1,    # Cloud overlap method, 0: Clear only, 1: Random, 2,  Maximum/random] 3: Maximum
            irng = 1,  # more monte carlo stuff
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
            iaer = 0,   #! Aerosol option flag
                        #!    0: No aerosol
                        #!    6: ECMWF method: use six ECMWF aerosol types input aerosol optical depth at 0.55 microns for each aerosol type (ecaer)
                        #!    10:Input aerosol optical properties: input total aerosol optical depth, single scattering albedo and asymmetry parameter (tauaer, ssaaer, asmaer) directly
            tauaer = 0.,   # Aerosol optical depth (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
            ssaaer = 0.,   # Aerosol single scattering albedo (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
            asmaer = 0.,   # Aerosol asymmetry parameter (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
            ecaer  = 0.,   # Aerosol optical depth at 0.55 micron (iaer=6 only), Dimensions,  (ncol,nlay,naerec)] #  (non-delta scaled)
            # new arguments for RRTMG_SW version 4.0
            isolvar = -1,    # ! Flag for solar variability method
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
            indsolvar = np.ones(2),      # Facular and sunspot amplitude scale factors (isolvar=1),
                                         # or Mg and SB indices (isolvar=2)
            bndsolvar = np.ones(nbndsw), # Solar variability scale factors for each shortwave band
            solcycfrac = 1.,              # Fraction of averaged solar cycle (0-1) at current time (isolvar=1)
            **kwargs):
        super(RRTMG_SW, self).__init__(**kwargs)
        #  define INPUTS
        self.add_input('icld', icld)
        self.add_input('irng', irng)
        self.add_input('permuteseed', permuteseed)
        self.add_input('dyofyr', dyofyr)
        self.add_input('inflgsw', inflgsw)
        self.add_input('iceflgsw', iceflgsw)
        self.add_input('liqflgsw', liqflgsw)
        self.add_input('iaer', iaer)
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
        permuteseed = self.permuteseed
        inflgsw = self.inflgsw
        iceflgsw = self.iceflgsw
        liqflgsw = self.liqflgsw
        dyofyr = self.dyofyr
        isolvar = self.isolvar
        solcycfrac = self.solcycfrac
        iaer = self.iaer
        #  scalar real arguments
        adjes = self.eccentricity_factor
        scon = self.S0
        indsolvar = self.indsolvar
        bndsolvar = self.bndsolvar

        (ncol, nlay, play, plev, tlay, tlev, tsfc,
        h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr,
        cfc12vmr, cfc12vmr, cfc22vmr, ccl4vmr,
        cldfrac, ciwp, clwp, relq, reic) = _prepare_general_arguments(self)

        aldif = _climlab_to_rrtm_sfc(self.aldif, self.Ts)
        aldir = _climlab_to_rrtm_sfc(self.aldir, self.Ts)
        asdif = _climlab_to_rrtm_sfc(self.asdif, self.Ts)
        asdir = _climlab_to_rrtm_sfc(self.asdir, self.Ts)
        coszen = _climlab_to_rrtm_sfc(self.coszen, self.Ts)
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

        args = [ncol, nlay, icld, iaer, permuteseed, irng,
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
        (ncol, nlay, icld, iaer, permuteseed, irng,
         play, plev, tlay, tlev, tsfc,
         h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,
         aldif, aldir, asdif, asdir, coszen, adjes, dyofyr, scon, isolvar,
         indsolvar, bndsolvar, solcycfrac,
         inflgsw, iceflgsw, liqflgsw,
         cldfrac, ciwp, clwp, reic, relq, tauc, ssac, asmc, fsfc,
         tauaer, ssaaer, asmaer, ecaer,) = self._prepare_sw_arguments()
        if icld == 0:  # clear-sky only
            cldfmcl = np.zeros((ngptsw,ncol,nlay))
            ciwpmcl = np.zeros((ngptsw,ncol,nlay))
            clwpmcl = np.zeros((ngptsw,ncol,nlay))
            reicmcl = np.zeros((ncol,nlay))
            relqmcl = np.zeros((ncol,nlay))
            taucmcl = np.zeros((ngptsw,ncol,nlay))
            ssacmcl = np.zeros((ngptsw,ncol,nlay))
            asmcmcl = np.zeros((ngptsw,ncol,nlay))
            fsfcmcl = np.zeros((ngptsw,ncol,nlay))
        else:
            #  Call the Monte Carlo Independent Column Approximation (McICA, Pincus et al., JC, 2003)
            (cldfmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, taucmcl,
            ssacmcl, asmcmcl, fsfcmcl) = _rrtmg_sw.climlab_mcica_subcol_sw(
                            ncol, nlay, icld, permuteseed, irng, play,
                            cldfrac, ciwp, clwp, reic, relq, tauc, ssac, asmc, fsfc)
        #  Call the RRTMG_SW driver to compute radiative fluxes
        (swuflx, swdflx, swhr, swuflxc, swdflxc, swhrc) = \
            _rrtmg_sw.climlab_rrtmg_sw(ncol, nlay, icld, iaer,
                play, plev, tlay, tlev, tsfc,
                h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,
                asdir, asdif, aldir, aldif,
                coszen, adjes, dyofyr, scon, isolvar,
                inflgsw, iceflgsw, liqflgsw, cldfmcl,
                taucmcl, ssacmcl, asmcmcl, fsfcmcl,
                ciwpmcl, clwpmcl, reicmcl, relqmcl,
                tauaer, ssaaer, asmaer, ecaer,
                bndsolvar, indsolvar, solcycfrac)
        #  Output is all (ncol,nlay+1) or (ncol,nlay)
        self.SW_flux_up = _rrtm_to_climlab(swuflx) + 0.*self.SW_flux_up
        self.SW_flux_down = _rrtm_to_climlab(swdflx) + 0.*self.SW_flux_down
        self.SW_flux_up_clr = _rrtm_to_climlab(swuflxc) + 0.*self.SW_flux_up_clr
        self.SW_flux_down_clr = _rrtm_to_climlab(swdflxc) + 0.*self.SW_flux_down_clr
        #  Compute quantities derived from fluxes, including ASR
        self._compute_SW_flux_diagnostics()
        #  calculate heating rates from flux divergence
        SWheating_Wm2 = np.array(-np.diff(self.SW_flux_net, axis=-1)) + 0.*self.Tatm
        SWheating_clr_Wm2 = np.array(-np.diff(self.SW_flux_net_clr, axis=-1)) + 0.*self.Tatm
        self.heating_rate['Ts'] = np.array(self.SW_flux_net[..., -1, np.newaxis]) + 0.*self.Ts
        self.heating_rate['Tatm'] = SWheating_Wm2
        #  Convert to K / day
        Catm = self.Tatm.domain.heat_capacity
        self.TdotSW = SWheating_Wm2 / Catm * const.seconds_per_day
        self.TdotSW_clr = SWheating_clr_Wm2 / Catm * const.seconds_per_day
