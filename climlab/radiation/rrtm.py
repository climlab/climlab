''' climlab wrapper for RRTMG_LW and RRTMG_SW radiation schemes'''
from __future__ import division
import numpy as np
from climlab import EnergyBudget, TimeDependentProcess
from climlab import constants as const
import _rrtmg_lw, _rrtmg_sw
from _rrtm_radiation_init import read_lw_abs_data, read_sw_abs_data

#  Python-based initialization of absorption data from netcdf file
read_sw_abs_data(_rrtmg_sw)
read_lw_abs_data(_rrtmg_lw)
#  Call the modified fortran init subroutine (netcdf calls are commented out)
_rrtmg_sw.rrtmg_sw_init.rrtmg_sw_ini(const.cp)
_rrtmg_lw.rrtmg_lw_init.rrtmg_lw_ini(const.cp)

#  Ready for calls to _rrtmg_lw.driver() and _rrtmg_sw.driver()
rrtm_inputs = [
        # GENERAL, used in both SW and LW
        ['icld', 1 if 'cldf' in climt_inputs else 0], # Cloud overlap method, 0: Clear only, 1: Random, 2,  Maximum/random] 3: Maximum
        ['permuteseed_sw',  150], # used for monte carlo clouds; must differ from permuteseed_lw by number of subcolumns
        ['permuteseed_lw',  300], # learn about these later...
        ['irng',  1], # more monte carlo stuff
        ['idrv',  0], # whether to also calculate the derivative of flux with respect to surface temp
        ['cpdair',  climt_inputs['Cpd']],
        ['play',  [[interpolated_p(i + .5) for i in range(number_of_layers)]]], # pressure in each layer
        ['plev',  [climt_inputs['pbound']]], # pressure at boundaries of each layer
        ['tlay',  [[interpolated_T(i + .5) for i in range(number_of_layers)]]], # temperature in each layer
        ['tlev',  [climt_inputs['Tbound']]], # temperature at boundaries of each layer
        ['tsfc',  [climt_inputs['Ts']]], # surface temperature
        # GASES, used in both SW and LW
        ['h2ovmr',  h2o_concentration],
        ['o3vmr',  [[o3[0][0] * 0.603428 for o3 in climt_inputs['o3']]]], # convert from kg/kg to volume mixing ratio using molecular weight of dry air / ozone
        ['co2vmr',  [[co2 * 1.e-6 for co2 in climt_inputs['co2']]]],
        ['ch4vmr',  [[ch4 * 1.e-6 for ch4 in climt_inputs['ch4']]]],
        ['n2ovmr',  [[n2o * 1.e-6 for n2o in climt_inputs['n2o']]]],
        ['o2vmr',  [climt_inputs['o2']]],
        ['cfc11vmr',  [[cfc11 * 1.e-6 for cfc11 in climt_inputs['cfc11']]]],
        ['cfc12vmr',  [[cfc12 * 1.e-6 for cfc12 in climt_inputs['cfc12']]]],
        ['cfc22vmr',  [[cfc22 * 1.e-6 for cfc22 in climt_inputs['cfc22']]]],
        ['ccl4vmr',  [climt_inputs['ccl4']]],
        # SURFACE OPTICAL PROPERTIES
        # SW
        ['aldif',  [climt_inputs['aldif'][0][0]]],
        ['aldir',  [climt_inputs['aldir'][0][0]]],
        ['asdif',  [climt_inputs['asdif'][0][0]]],
        ['asdir',  [climt_inputs['asdir'][0][0]]],
        # LW
        ['emis',  [[1. or 1 - emis for emis in climt_inputs['lw_surface_emissivity']]]],
        # THE SUN - SW
        ['coszen',  [cos(climt_inputs['zen'][0][0] * 2. * pi / 360.)]], # cosine of the solar zenith angle
        ['adjes',  1.], # flux adjustment for earth/sun distance (if not dyofyr)
        ['dyofyr',  0], # day of the year used to get Earth/Sun distance (if not adjes)
        ['solin',  climt_inputs['solin']], # solar constant
        # ['scon',  climt_inputs['scon']], # solar constant
        # CLOUDS, SW see http://www.arm.gov/publications/proceedings/conf16/extended_abs/iacono_mj.pdf
        ['inflgsw',  2], # Flag for cloud optical properties
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
        ['inflglw',  2], #
        ['iceflgsw',  1], #  # Flag for ice particle specification
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
        ['iceflgslw',  1], #
        ['liqflgsw',  1], # # Flag for liquid droplet specification
            # LIQFLAG = 0 the optical depths (gray) due to water clouds are computed as in CCM3.
            #         = 1 the optical depths (non-gray) due to water clouds are computed by a method
            #             based on the parameterization of water clouds due to Y.X. Hu and K. Stamnes,
            #             J. Clim., 6, 728-742 (1993).
        ['liqflglw',  1], #
        ['tauc_sw',  [[[0.]* number_of_layers]] * len(SW_BANDS)], # In-cloud optical depth [IS THIS ONE NEEDED GIVEN THE OTHERS?]
        ['tauc_lw',  [[climt_inputs['tauc_lw'] or [0.] * number_of_layers]] * len(LW_BANDS)],  # in-cloud optical depth
        ['cldfrac',  [[c[0][0] for c in climt_inputs['cldf']]]], # # layer cloud fraction
        ['ssac_sw',  [[climt_inputs['cloud_single_scattering_albedo'] or [0.] * number_of_layers]] * len(SW_BANDS)], #  # In-cloud single scattering albedo
        ['asmc_sw',  [[climt_inputs['cloud_asymmetry_parameter'] or [0.] * number_of_layers]] * len(SW_BANDS)], # # In-cloud asymmetry parameter
        ['fsfc_sw',  [[climt_inputs['cloud_forward_scattering_fraction'] or [0.] * number_of_layers]] * len(SW_BANDS)], ## In-cloud forward scattering fraction (delta function pointing forward "forward peaked scattering")
        ['ciwp',  [[c[0][0] for c in climt_inputs['ciwp']]]], # # in-cloud ice water path (g/m2)
        ['clwp',  [[c[0][0] for c in climt_inputs['clwp']]]], # # in-cloud liquid water path (g/m2)
        ['reic',  [[c[0][0] for c in climt_inputs['r_ice']]]], # # Cloud ice particle effective size (microns)
                      # specific definition of reicmcl depends on setting of iceflglw:
                      # iceflglw = 0,  ice effective radius, r_ec, (Ebert and Curry, 1992)]
                      #               r_ec must be >= 10.0 microns
                      # iceflglw = 1,  ice effective radius, r_ec, (Ebert and Curry, 1992)]
                      #               r_ec range is limited to 13.0 to 130.0 microns
                      # iceflglw = 2,  ice effective radius, r_k, (Key, Streamer Ref. Manual] 1996)
                      #               r_k range is limited to 5.0 to 131.0 microns
                      # iceflglw = 3,  generalized effective size, dge, (Fu, 1996)]
                      #               dge range is limited to 5.0 to 140.0 microns
                      #               [dge = 1.0315 * r_ec]
        ['relq',  [[c[0][0] for c in climt_inputs['r_liq']]]], # # Cloud water drop effective radius (microns)


        # AEROSOLS
        # SW
        ['tauaer_sw', [climt_inputs['tauaer_sw'] or [[0.] * len(SW_BANDS)] * number_of_layers]], # Aerosol optical depth (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
        ['ssaaer_sw', [climt_inputs['ssaaer_sw'] or [[0.] * len(SW_BANDS)] * number_of_layers]], # Aerosol single scattering albedo (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
        ['asmaer_sw', [climt_inputs['asmaer_sw'] or [[0.] * len(SW_BANDS)] * number_of_layers]], # Aerosol asymmetry parameter (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
        ['ecaer_sw', [[[0.] * 6] * number_of_layers]], # Aerosol optical depth at 0.55 micron (iaer=6 only), Dimensions,  (ncol,nlay,naerec)] #  (non-delta scaled)
        ['tauaer_lw', [climt_inputs['tauaer_lw'] or [[0.] * len(LW_BANDS)] * number_of_layers]] #
    ]


class RRTMG(TimeDependentProcess)
    def __init__(self, **kwargs):
        super(RRTMG, self).__init__(**kwargs)
        self.add_subprocess('SW', RRTMG_SW(**kwargs))
        self.add_subprocess('LW', RRTMG_LW(**kwargs))


class RRTMG_LW(EnergyBudget):
    def __init__(self,
                 #  volume mixing ratios (PPMV) for greenhouse gases
                 #  (if scalar assume well-mixed)
                 absorber_vmr = {'CO2': }
                 CO2=380.,
                 N2O=1.E-9,
                 CH4=1.E-9,
                 CFC11=1.E-9,
                 CFC12=1.E-9,
                 O3=1.E-9,
                 q=None,
                 O3init = False,
                 O3file = 'apeozone_cam3_5_54.nc',
                 **kwargs):
        super(RRTMG_LW, self).__init__(**kwargs)
        #  define INPUTS
        self.add_input('icld', 1)
        #  define diagnostics

    def _compute_radiative_heating(self):
        '''Compute radiative fluxes and heating rates.

        Must be implemented by daughter classes.'''

        ncol = None  #  figure this out
        nlay = self.lev.size
        play = self._climlab_to_rrtm(self.lev)
        plev = self._climlab_to_rrtm(self.lev_bounds)
        tlay = self._climlab_to_rrtm(self.Tatm)
        tlev = None  # figure this out -- interface temperatures
        #  Looks like element 0 of arrays is surface in RRTM code
        #  opposite of climlab convention
        args = [ncol, nlay, icld, permuteseed,
                irng, idrv,
                play, plev, tlay, tlev, tsfc,
                h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,
                cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis,
                inflglw, iceflgslw, liqflglw,
                cldfrac, ciwp, clwp, reic, relq, tauc, tauaer,]
        uflx, dflx, hr, uflxc, dflxc, hrc, duflx_dt, duflxc_dt = _rrtmg_lw.driver(*args)
        #  hr is the heating rate in K/day from RRTMG_LW
        #  Need to set to W/m2
        Catm = self.Tatm.domain.heat_capacity
        self.heating_rate['Tatm'] = self._rrtm_to_climlab(hr) / const.seconds_per_day * Catm
        #  Set some diagnostics
        self.OLR = self._rrtm_to_climlab(uflx)[:,0]
        self.OLRclr = self._rrtm_to_climlab(uflxc)[:,0]
        self.TdotLW = self._rrtm_to_climlab(hr)  # heating rate in K/day
