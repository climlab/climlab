''' climlab wrapper for RRTMG_LW and RRTMG_SW radiation schemes'''
from __future__ import division
import numpy as np
from climlab.process import EnergyBudget, TimeDependentProcess
from climlab import constants as const
import _rrtmg_lw, _rrtmg_sw
from _rrtm_radiation_init import read_lw_abs_data, read_sw_abs_data

#  Python-based initialization of absorption data from netcdf file
#read_sw_abs_data(_rrtmg_sw)
#read_lw_abs_data(_rrtmg_lw)
#  Call the modified fortran init subroutine (netcdf calls are commented out)
#_rrtmg_sw.rrtmg_sw_init.rrtmg_sw_ini(const.cp)
_rrtmg_lw.rrtmg_lw_init.rrtmg_lw_ini(const.cp)

#  Ready for calls to _rrtmg_lw.driver() and _rrtmg_sw.driver()

#  Get number of bands from fortran modules
nbndsw = int(_rrtmg_sw.parrrsw.nbndsw)
nbndlw = int(_rrtmg_lw.parrrtm.nbndlw)

## RRTM inputs  from CliMT code
# GENERAL, used in both SW and LW
icld = 1    # Cloud overlap method, 0: Clear only, 1: Random, 2,  Maximum/random] 3: Maximum
permuteseed_sw =  150  # used for monte carlo clouds; must differ from permuteseed_lw by number of subcolumns
permuteseed_lw =  300  # learn about these later...
irng = 1  # more monte carlo stuff
idrv = 0  # whether to also calculate the derivative of flux with respect to surface temp
# GASES, used in both SW and LW
#h2ovmr = 0.
#o3vmr = 0.
#co2vmr = 0.
#ch4vmr = 0.
#n2ovmr = 0.
#o2vmr = 0.
#cfc11vmr = 0.
#cfc12vmr = 0.
#cfc22vmr = 0.
#ccl4vmr = 0.
# SURFACE OPTICAL PROPERTIES
# SW
alb = 0.3
aldif = alb
aldir = alb
asdif = alb
asdir = alb
# LW
#emis = 1.
# THE SUN - SW
coszen = 1.      # cosine of the solar zenith angle
adjes = 1.       # flux adjustment for earth/sun distance (if not dyofyr)
dyofyr = 0       # day of the year used to get Earth/Sun distance (if not adjes)
scon = const.S0  # solar constant
# CLOUDS, SW see http://www.arm.gov/publications/proceedings/conf16/extended_abs/iacono_mj.pdf
inflgsw  = 2 # Flag for cloud optical properties
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
inflglw  = 2
iceflgsw = 1  # Flag for ice particle specification
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
iceflglw = 1
liqflgsw = 1  # Flag for liquid droplet specification
            # LIQFLAG = 0 the optical depths (gray) due to water clouds are computed as in CCM3.
            #         = 1 the optical depths (non-gray) due to water clouds are computed by a method
            #             based on the parameterization of water clouds due to Y.X. Hu and K. Stamnes,
            #             J. Clim., 6, 728-742 (1993).
liqflglw = 1


class RRTMG(TimeDependentProcess):
    def __init__(self, **kwargs):
        super(RRTMG, self).__init__(**kwargs)
        self.add_subprocess('SW', RRTMG_SW(**kwargs))
        self.add_subprocess('LW', RRTMG_LW(**kwargs))


class RRTMG_LW(EnergyBudget):
    def __init__(self,
                 #  volume mixing ratios (PPMV) for greenhouse gases
                 #  (if scalar assume well-mixed)
                 absorber_vmr = {'CO2': 380.},
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

    def _compute_heating_rates(self):
        '''Compute radiative fluxes and heating rates.

        Must be implemented by daughter classes.'''

        nlay = self.lev.size
        play = _climlab_to_rrtm(self.lev)
        plev = _climlab_to_rrtm(self.lev_bounds)
        tlay = _climlab_to_rrtm(self.Tatm)
        tlev = _climlab_to_rrtm(interface_temperature(**self.state))
        ncol, nlay = tlay.shape
        tsfc = self.Ts  #  this needs to be adapted for horizontal dimensions

        cldfrac = _climlab_to_rrtm(0. * self.Tatm) # layer cloud fraction
        ciwp = _climlab_to_rrtm(0. * self.Tatm)    # in-cloud ice water path (g/m2)
        clwp = _climlab_to_rrtm(0. * self.Tatm)    # in-cloud liquid water path (g/m2)
        relq = _climlab_to_rrtm(0. * self.Tatm)    # Cloud water drop effective radius (microns)
        reic = _climlab_to_rrtm(0. * self.Tatm)     # Cloud ice particle effective size (microns)
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
        #  These arrays have an extra dimension for number of bands
        dim_sw1 = [nbndsw]; dim_sw1.extend(tlay.shape)     # [nbndsw,ncol,nlay]
        dim_lw1 = [nbndlw]; dim_lw1.extend(tlay.shape)
        dim_sw2 = list(tlay.shape); dim_sw2.append(nbndsw)  # [ncol,nlay,nbndsw]
        dim_lw2 = list(tlay.shape); dim_lw2.append(nbndlw)
        tauc_sw = np.zeros(dim_sw1) # In-cloud optical depth
        tauc_lw = np.zeros(dim_lw1) # in-cloud optical depth
        ssac_sw = np.zeros(dim_sw1) # In-cloud single scattering albedo
        asmc_sw = np.zeros(dim_sw1) # In-cloud asymmetry parameter
        fsfc_sw = np.zeros(dim_sw1) # In-cloud forward scattering fraction (delta function pointing forward "forward peaked scattering")

        # AEROSOLS
        tauaer_sw = np.zeros(dim_sw2)   # Aerosol optical depth (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
        ssaaer_sw = np.zeros(dim_sw2)   # Aerosol single scattering albedo (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
        asmaer_sw = np.zeros(dim_sw2)   # Aerosol asymmetry parameter (iaer=10 only), Dimensions,  (ncol,nlay,nbndsw)] #  (non-delta scaled)
        ecaer_sw  = np.zeros(dim_sw2)   # Aerosol optical depth at 0.55 micron (iaer=6 only), Dimensions,  (ncol,nlay,naerec)] #  (non-delta scaled)
        tauaer_lw = np.zeros(dim_lw2)   # Aerosol optical depth at mid-point of LW spectral bands

        # GASES
        h2ovmr = 0. * np.zeros_like(tlay) + 1E-9
        o3vmr = 0. * np.zeros_like(tlay) + 1E-9
        co2vmr = 0. * np.zeros_like(tlay) + 380./1E6
        ch4vmr = 0. * np.zeros_like(tlay) + 1E-9
        n2ovmr = 0. * np.zeros_like(tlay) + 1E-9
        o2vmr = 0. * np.zeros_like(tlay) + 1E-9
        cfc11vmr = 0. * np.zeros_like(tlay) + 1E-9
        cfc12vmr = 0. * np.zeros_like(tlay) + 1E-9
        cfc22vmr = 0. * np.zeros_like(tlay) + 1E-9
        ccl4vmr = 0. * np.zeros_like(tlay) + 1E-9

        # surface emissivity
        emis = np.ones((ncol,nbndlw))

        tauaer = tauaer_lw
        tauc = tauc_lw

        args = [ncol, nlay, icld, permuteseed_lw, irng, idrv,
                play, plev, tlay, tlev, tsfc,
                h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,
                cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr, emis,
                inflglw, iceflglw, liqflglw,
                cldfrac, ciwp, clwp, reic, relq, tauc, tauaer,]

        #  Debugging
        for j in range(len(args)):
            if np.isscalar(args[j]):
                thing = args[j]
            else:
                thing = args[j].shape
            print j, thing
        self.args = args

        #  Call the RRTM code!
        uflx, dflx, hr, uflxc, dflxc, hrc, duflx_dt, duflxc_dt = _rrtmg_lw.driver(*args)
        #  For debugging purposes: raw output
        self.uflx = uflx
        self.dflx = dflx
        self.hr = hr
        self.uflxc = uflxc
        self.dflxc = dflxc
        self.hrc = hrc

        #  Output is all (ncol,nlay+1) or (ncol,nlay)
        self.flux_up = _rrtm_to_climlab(uflx)
        self.flux_down = _rrtm_to_climlab(dflx)
        self.flux_up_clr = _rrtm_to_climlab(uflxc)
        self.flux_down_clr = _rrtm_to_climlab(dflxc)
        #  hr is the heating rate in K/day from RRTMG_LW
        #  Need to set to W/m2
        Catm = self.Tatm.domain.heat_capacity
        #self.heating_rate['Tatm'] = _rrtm_to_climlab(hr) / const.seconds_per_day * Catm
        #  calculate slab ocean heating rate from flux divergence
        #self.heating_rate['Ts'] = self.flux_down[..., -1] - self.flux_up[..., -1]
        #  Set some diagnostics
        self.OLR = self.flux_up[..., 0]
        self.OLRclr = self.flux_up_clr[..., 0]
        self.TdotLW = _rrtm_to_climlab(hr)  # heating rate in K/day


class RRTMG_SW(EnergyBudget):
    def __init__(self, **kwargs):
        super(RRTMG_SW, self).__init__(**kwargs)

def interface_temperature(Ts, Tatm, **kwargs):
    '''Compute temperature at model layer interfaces.'''
    #  Actually it's not clear to me how the RRTM code uses these values
    lev = Tatm.domain.axes['lev'].points
    lev_bounds = Tatm.domain.axes['lev'].bounds
    #  For now just return something dumb but with the right dimensions
    #return np.append(Tatm, Tatm[..., -1])
    Tinterp = np.interp(lev_bounds[1:-1], lev, Tatm)
    #  add TOA value
    Tinterp = np.insert(Tinterp, 0, Tatm[0])
    Tinterp = np.append(Tinterp, Ts)
    return Tinterp

def _climlab_to_rrtm(field, append_ncol=True):
    '''Prepare field with proper dimension order.
    RRTM code expects arrays with (ncol, nlay)
    and with pressure decreasing from surface at element 0

    climlab grid dimensions are any of:
        - (KM,)
        - (JM, KM)
        - (JM, IM, KM)

    This is not yet fully implemented.'''

    # Make this work just with 1D (KM,) arrays
    #  (KM,)  -->  (1, nlay)
    if np.isscalar(field):
        return field
    else:
        #  Flip along the last axis to reverse the pressure order
        field = field[..., ::-1]
        #  Do we need to append an extra dimension for singleton horizontal ncol?
        if append_ncol:
            return field[np.newaxis, ...]
        else:
            return field

def _rrtm_to_climlab(field):
    if np.isscalar(field):
        return field
    else:
        field = field[..., ::-1]
        return np.squeeze(field)
