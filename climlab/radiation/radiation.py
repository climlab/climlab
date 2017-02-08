'''
_Radiation is the base class for radiation processes
currently CAM3 and RRTMG

Basic characteristics:

State:
- Ts (surface radiative temperature)
- Tatm (air temperature)

Input (specified or provided by parent process):
- (fix this up)

Shortave processes should define these diagnostics (minimum):
- ASR (W/m2, net absorbed shortwave radiation)
- SW_flux_up   (W/m2, defined at pressure level interfaces)
- SW_flux_down (W/m2, defined at pressure level interfaces)
- SW_degrees_per_day   (K/day, radiative heating rate)

May also have all the same diagnostics for clear-sky,
e.g. ASR_clr, SW_flux_up_clr, etc.

Longwave processes should define these diagnostics (minimum):
- OLR (W/m2, net outgoing longwave radiation at TOA)
- LW_flux_up
- LW_flux_down
- LW_degrees_per_day

and may also have the same diagnostics for clear-sky.


WORK IN PROGRESS....
'''
from __future__ import division
import numpy as np
from climlab.process import EnergyBudget
from climlab.radiation import ManabeWaterVapor
import netCDF4 as nc
import os
from scipy.interpolate import interp1d, interp2d
from climlab import constants as const


def default_absorbers(Tatm, ozone_file = 'apeozone_cam3_5_54.nc'):
    '''Initialize a dictionary of radiatively active gases.
    All values are volumetric mixing ratios.

    Ozone is set to a climatology
    Water vapor is set based on a prescribed relative humidity profile.

    All other gases are assumed well-mixed.

    Specific values are based on the AquaPlanet Experiment protocols.'''
    absorber_vmr = {}
    absorber_vmr['CO2']   = 348. / 1E6
    absorber_vmr['CH4']   = 1650. / 1E9
    absorber_vmr['N2O']   = 306. / 1E9
    absorber_vmr['O2']    = 0.
    absorber_vmr['CFC11'] = 0.
    absorber_vmr['CFC12'] = 0.
    absorber_vmr['CFC22'] = 0.
    absorber_vmr['CCL4']  = 0.

    h2o = ManabeWaterVapor(state={'Tatm': Tatm})
    #  should be converting from specific humidity to volume mixing ratio here...
    absorber_vmr['H2O'] = h2o.q

    datadir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'data', 'ozone'))
    ozonefilepath = os.path.join(datadir, ozone_file)
    #  Open the ozone data file
    print 'Getting ozone data from', ozonefilepath
    ozonedata = nc.Dataset(ozonefilepath)
    ozone_lev = ozonedata.variables['lev'][:]
    ozone_lat = ozonedata.variables['lat'][:]
    #  zonal and time average
    ozone_zon = np.mean(ozonedata.variables['OZONE'], axis=(0,3))
    ozone_global = np.average(ozone_zon, weights=np.cos(np.deg2rad(ozone_lat)), axis=1)
    lev = Tatm.domain.axes['lev'].points
    if Tatm.shape == lev.shape:
        # 1D interpolation on pressure levels using global average data
        f = interp1d(ozone_lev, ozone_global)
        #  interpolate data to model levels
        absorber_vmr['O3'] = f(lev)
    else:
        #  Attempt 2D interpolation in pressure and latitude
        f2d = interp2d(ozone_lat, ozone_lev, ozone_zon)
        try:
            lat = Tatm.domain.axes['lat'].points
            f2d = interp2d(ozone_lat, ozone_lev, ozone_zon)
            absorber_vmr['O3'] = f2d(lat, lev).transpose()
        except:
            print 'Interpolation of ozone data failed.'
            print 'Reverting to default O3.'
            absorber_vmr['O3'] = np.zeros_like(Tatm)
    return absorber_vmr

class _Radiation(EnergyBudget):
    '''Base class for radiation models (currently CAM3 and RRTMG).
    '''
    def __init__(self,
            #  Absorbing gases, volume mixing ratios
            absorber_vmr = None,
            cldfrac = 0.,  # layer cloud fraction
            clwp = 0.,     # in-cloud liquid water path (g/m2)
            ciwp = 0.,     # in-cloud ice water path (g/m2)
            r_liq = 0.,    # Cloud water drop effective radius (microns)
            r_ice = 0.,    # Cloud ice particle effective size (microns)
            ozone_file = 'apeozone_cam3_5_54.nc',
            **kwargs):
        super(_Radiation, self).__init__(**kwargs)
        #  Define inputs, default values, diagnostics
        if absorber_vmr is None:
            self.add_input('absorber_vmr', default_absorbers(self.Tatm, ozone_file))
        else:
            self.add_input('absorber_vmr', absorber_vmr)
        # self.add_input('H2Ovmr', H2Ovmr)
        # self.add_input('O3vmr', O3vmr)
        # self.add_input('CO2vmr', CO2vmr)
        # self.add_input('CH4vmr', CH4vmr)
        # self.add_input('N2Ovmr', N2Ovmr)
        # self.add_input('O2vmr', O2vmr)
        # self.add_input('CFC11vmr', CFC11vmr)
        # self.add_input('CFC12vmr', CFC12vmr)
        # self.add_input('CFC22vmr', CFC22vmr)
        # self.add_input('CCL4vmr', CCL4vmr)
        self.add_input('cldfrac', cldfrac)
        self.add_input('clwp', clwp)
        self.add_input('ciwp', ciwp)
        self.add_input('r_liq', r_liq)
        self.add_input('r_ice', r_ice)


class _Radiation_SW(_Radiation):
    def __init__(self,
                 albedo = None,
                 aldif = 0.3,
                 aldir = 0.3,
                 asdif = 0.3,
                 asdir = 0.3,
                 insolation = const.S0/4.,
                 cosZen = 0.5,    # cosine of the solar zenith angle
                 **kwargs):
        super(_Radiation_SW, self).__init__(**kwargs)
        self.add_input('insolation', insolation)
        self.add_input('cosZen', cosZen)
        if albedo is not None:
            aldif = albedo
            aldir = albedo
            asdif = albedo
            asdir = albedo
        self.add_input('aldif', aldif)
        self.add_input('aldir', aldir)
        self.add_input('asdif', asdif)
        self.add_input('asdir', asdir)


class _Radiation_LW(_Radiation):
    def __init__(self,
                 emissivity = 1.,  # surface emissivity
                 **kwargs):
        super(_Radiation_LW, self).__init__(**kwargs)
        self.add_input('emissivity', emissivity)
