from __future__ import division
from __future__ import absolute_import
import numpy as np
from climlab import constants as const
from climlab.radiation.radiation import _Radiation_SW, _Radiation_LW
from .rrtmg_lw import RRTMG_LW
from .rrtmg_sw import RRTMG_SW, nbndsw


class RRTMG(_Radiation_SW, _Radiation_LW):
    '''Container to drive combined LW and SW radiation models.

    For some details about inputs and diagnostics, see the `radiation` module.
    '''
    def __init__(self,
            # GENERAL, used in both SW and LW
            icld = 1,    # Cloud overlap method, 0: Clear only, 1: Random, 2,  Maximum/random] 3: Maximum
            irng = 1,  # more monte carlo stuff
            idrv = 0,  # whether to also calculate the derivative of flux with respect to surface temp
            permuteseed_sw =  150,  # used for monte carlo clouds; must differ from permuteseed_lw by number of subcolumns
            permuteseed_lw =  300,  # learn about these later...
            dyofyr = 0,       # day of the year used to get Earth/Sun distance (if not adjes)
            # CLOUDS, SW see http://www.arm.gov/publications/proceedings/conf16/extended_abs/iacono_mj.pdf
            inflgsw  = 2, # Flag for cloud optical properties
                        # INFLAG = 0 direct specification of optical depths of clouds;
                        #            cloud fraction and cloud optical depth (gray) are
                        #            input for each cloudy layer
                        #        = 1 calculation of combined ice and liquid cloud optical depths (gray)
                        #            as in CCM2; cloud fraction and cloud water path are input for
                        #            each cloudy layer.
                        #        = 2 calculation of separate ice and liquid cloud optical depths, with
                        #            parameterizations determined by values of ICEFLAG and LIQFLAG.
                        #            Cloud fraction, cloud water path, cloud ice fraction, and
                        #            effective ice radius are input for each cloudy layer for all
                        #            parameterizations.  If LIQFLAG = 1, effective liquid droplet radius
                        #            is also needed.
            inflglw  = 2,
            iceflgsw = 1,  # Flag for ice particle specification
                        #             ICEFLAG = 0 the optical depths (gray) due to ice clouds are computed as in CCM3.
                        #                     = 1 the optical depths (non-gray) due to ice clouds are computed as closely as
                        #                         possible to the method in E.E. Ebert and J.A. Curry, JGR, 97, 3831-3836 (1992).
                        #                     = 2 the optical depths (non-gray) due to ice clouds are computed by a method
                        #                         based on the parameterization used in the radiative transfer model Streamer
                        #                         (reference,  J. Key, Streamer User's Guide, Technical Report 96-01] Boston
                        #                         University, 85 pp. (1996)), which is closely related to the parameterization
                        #                         of water clouds due to Hu and Stamnes (see below).
                        #             = 3 the optical depths (non-gray) due to ice clouds are computed by a method
                        # based on the parameterization given in Fu et al., J. Clim.,11,2223-2237 (1998).
                           # specific definition of reic depends on setting of iceflglw:
                           # iceflglw = 0,  ice effective radius, r_ec, (Ebert and Curry, 1992)]
                           #               r_ec must be >= 10.0 microns
                           # iceflglw = 1,  ice effective radius, r_ec, (Ebert and Curry, 1992)]
                           #               r_ec range is limited to 13.0 to 130.0 microns
                           # iceflglw = 2,  ice effective radius, r_k, (Key, Streamer Ref. Manual] 1996)
                           #               r_k range is limited to 5.0 to 131.0 microns
                           # iceflglw = 3,  generalized effective size, dge, (Fu, 1996)]
                           #               dge range is limited to 5.0 to 140.0 microns
                           #               [dge = 1.0315 * r_ec]

            iceflglw = 1,
            liqflgsw = 1,  # Flag for liquid droplet specification
                        # LIQFLAG = 0 the optical depths (gray) due to water clouds are computed as in CCM3.
                        #         = 1 the optical depths (non-gray) due to water clouds are computed by a method
                        #             based on the parameterization of water clouds due to Y.X. Hu and K. Stamnes,
                        #             J. Clim., 6, 728-742 (1993).
            liqflglw = 1,
            tauc_sw = 0.,  # In-cloud optical depth
            tauc_lw = 0.,  # in-cloud optical depth
            ssac_sw = 0.,  # In-cloud single scattering albedo
            asmc_sw = 0.,  # In-cloud asymmetry parameter
            fsfc_sw = 0.,  # In-cloud forward scattering fraction (delta function pointing forward "forward peaked scattering")
            # AEROSOLS
            tauaer_sw = 0.,   # Aerosol optical depth (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
            ssaaer_sw = 0.,   # Aerosol single scattering albedo (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
            asmaer_sw = 0.,   # Aerosol asymmetry parameter (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
            ecaer_sw  = 0.,   # Aerosol optical depth at 0.55 micron (iaer=6 only), Dimensions,  (ncol,nlay,naerec)] #  (non-delta scaled)
            tauaer_lw = 0.,   # Aerosol optical depth at mid-point of LW spectral bands
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
        super(RRTMG, self).__init__(**kwargs)

        # Remove specific inputs from kwargs dictionary.
        #  We want any changes implemented in the parent __init__ method to be preserved here
        remove_list = ['absorber_vmr','cldfrac','clwp','ciwp','r_liq','r_ice',
                       'emissivity','aldif','aldir','asdif','asdir','S0','coszen',
                       'eccentricity_factor','insolation',]
        for item in remove_list:
            if item in kwargs:
                ignored = kwargs.pop(item)

        LW = RRTMG_LW(absorber_vmr = self.absorber_vmr,
                     cldfrac = self.cldfrac,
                     clwp = self.clwp,
                     ciwp = self.ciwp,
                     r_liq = self.r_liq,
                     r_ice = self.r_ice,
                     icld = icld,
                     irng = irng,
                     idrv = idrv,
                     permuteseed = permuteseed_lw,
                     emissivity = self.emissivity,
                     inflglw = inflglw,
                     iceflglw = iceflglw,
                     liqflglw = liqflglw,
                     tauc = tauc_lw,
                     tauaer = tauaer_lw,
                     **kwargs)
        SW = RRTMG_SW(absorber_vmr = self.absorber_vmr,
                     cldfrac = self.cldfrac,
                     clwp = self.clwp,
                     ciwp = self.ciwp,
                     r_liq = self.r_liq,
                     r_ice = self.r_ice,
                     icld = icld,
                     irng = irng,
                     permute = permuteseed_sw,
                     aldif = self.aldif,
                     aldir = self.aldir,
                     asdif = self.asdif,
                     asdir = self.asdir,
                     S0 = self.S0,
                     coszen = self.coszen,
                     eccentricity_factor = self.eccentricity_factor,
                     dyofyr = dyofyr,
                     insolation = self.insolation,
                     inflgsw = inflgsw,
                     iceflgsw = iceflgsw,
                     tauc = tauc_sw,
                     ssac = ssac_sw,
                     asmc = asmc_sw,
                     fsfc = fsfc_sw,
                     tauaer = tauaer_sw,
                     ssaaer = ssaaer_sw,
                     asmaer = asmaer_sw,
                     ecaer = ecaer_sw,
                     isolvar = isolvar,
                     indsolvar = indsolvar,
                     bndsolvar = bndsolvar,
                     solcycfrac = solcycfrac,
                     **kwargs)
        self.add_subprocess('SW', SW)
        self.add_subprocess('LW', LW )

        self.add_input('icld', icld)
        self.add_input('irng', irng)
        self.add_input('idrv', idrv)
        self.add_input('permuteseed_sw', permuteseed_sw)
        self.add_input('permuteseed_lw', permuteseed_lw)
        self.add_input('dyofyr', dyofyr)
        self.add_input('inflgsw', inflgsw)
        self.add_input('inflglw', inflglw)
        self.add_input('iceflgsw', iceflgsw)
        self.add_input('iceflglw', iceflglw)
        self.add_input('liqflgsw', liqflgsw)
        self.add_input('liqflglw', liqflglw)
        self.add_input('tauc_sw', tauc_sw)
        self.add_input('tauc_lw', tauc_lw)
        self.add_input('ssac_sw', ssac_sw)
        self.add_input('asmc_sw', asmc_sw)
        self.add_input('fsfc_sw', fsfc_sw)
        self.add_input('tauaer_sw', tauaer_sw)
        self.add_input('ssaaer_sw', ssaaer_sw)
        self.add_input('asmaer_sw', asmaer_sw)
        self.add_input('ecaer_sw', ecaer_sw)
        self.add_input('tauaer_lw', tauaer_lw)
        self.add_input('isolvar', isolvar)
        self.add_input('indsolvar', indsolvar)
        self.add_input('bndsolvar', bndsolvar)
        self.add_input('solcycfrac', solcycfrac)
