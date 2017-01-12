'''
climlab wrap of the CAM3 radiation code
'''
from __future__ import division
import numpy as np
import netCDF4 as nc
from climlab import constants as const
from climlab.radiation import Radiation
#import _cam3_interface
import os
from scipy.interpolate import interp1d, interp2d
#  the compiled fortran extension
import _cam3_radiation

ToExtension = ['do_sw','do_lw','p','dp','ps','Tatm','Ts','q','O3','cldf','clwp',
               'ciwp', 'in_cld','aldif','aldir','asdif','asdir','cosZen',
               'insolation','flus','r_liq','r_ice','CO2','N2O','CH4','CFC11',
               'CFC12','g','Cpd','epsilon','stebol']

FromExtension = ['TdotRad','SrfRadFlx','swhr','lwhr','swflx','lwflx','SwToaCf',
                 'SwSrfCf','LwToaCf','LwSrfCf','LwToa','LwSrf','SwToa','SwSrf',
                 'lwuflx','lwdflx']

# Initialise absorptivity / emissivity data
here = os.path.dirname(__file__)
datadir = os.path.abspath(os.path.join(here, os.pardir, 'data', 'cam3rad'))
AbsEmsDataFile = os.path.join(datadir, 'abs_ems_factors_fastvx.c030508.nc')
#  Open the absorption data file
data = nc.Dataset(AbsEmsDataFile)
#  The fortran module that holds the data
mod = _cam3_radiation.absems
#  Populate storage arrays with values from netcdf file
for field in ['ah2onw', 'eh2onw', 'ah2ow', 'ln_ah2ow', 'cn_ah2ow', 'ln_eh2ow', 'cn_eh2ow']:
    setattr(mod, field, data.variables[field][:].T)
data.close()


class CAM3Radiation(Radiation):
    '''
    climlab wrapper for the CAM3 radiation code.

    Scalar input values:
    - Well-mixed greenhouse gases in ppmv (all default to zero except CO2):
        - CO2 (380.)
        - N2O
        - CH4
        - CFC11
        - CFC12
    Input values with same dimension as Ts:
        - cosZen (cosine of solar zenith angle, default = 1.)
        - insolation (W/m2, default = const.S0/4)
        - surface albedos (all default to 0.07):
            - asdir (shortwave direct)
            - asdif (shortwave diffuse)
            - aldir (near-infrared, direct)
            - aldif (near-infrared, diffuse)
    Input values with same dimension as Tatm:
        - O3 (ozone mass mixing ratio)
        - q (specific humidity in kg/kg)
        - cldf (cloud fraction, default is zero)
        - clwp (cloud liquid water path, default is zero)
        - ciwp (cloud ice water path, default is zero)
        - r_liq (liquid effective drop size, microns, default is 10.)
        - r_ice (ice effective drop size, microns, default is 30.)

    Diagnostics:
        - ASR (absorbed solar radiation, W/m2)
        - ASRcld (shortwave cloud radiative effect, all-sky - clear-sky flux, W/m2)
        - OLR (outgoing longwave radiation, W/m2)
        - OLRcld (longwave cloud radiative effect, all-sky - clear-sky flux, W/m2)
        - TdotRad (net radiative heating rate,  K / day)
        - TdotLW  (longwave radiative heating rate, K / day)
        - TdotSW  (shortwave radiative heating rate, K / day)

    Many other quantities are available in a slightly different format
    from the CAM3 wrapper, stored in self.Output.

    See the code for examples of how to translate these quantities in climlab format.
    '''
    def __init__(self,
                 CO2=380.,
                 N2O=1.E-9,
                 CH4=1.E-9,
                 CFC11=1.E-9,
                 CFC12=1.E-9,
                 O3=1.E-9,
                 q=None,
                 cosZen=1.,
                 insolation=const.S0/4.,
                 asdir=0.07,
                 asdif=0.07,
                 aldir=0.07,
                 aldif=0.07,
                 O3init = False,
                 O3file = 'apeozone_cam3_5_54.nc',
                 **kwargs):
        super(CAM3Radiation, self).__init__(**kwargs)
        ###  Declare all input variables
        #  Specific humidity in kg/kg
        if q is None:
            q = 1.e-8*np.ones_like(self.Tatm)  # Driver.f90 expect q in kg/kg
        self.add_input('q', q)
        # well-mixed absorbing gases in ppmv (scalars)
        self.add_input('CO2', CO2)
        self.add_input('N2O', N2O)
        self.add_input('CH4', CH4)
        self.add_input('CFC11', CFC11)
        self.add_input('CFC12', CFC12)
        # initalize ozone
        self.add_input('O3', np.ones_like(self.Tatm) * O3)
        # Cloud frac
        self.add_input('cldf', 0.*self.Tatm )
        # Cloud water / ice path
        self.add_input('clwp', 0.*self.Tatm)
        self.add_input('ciwp', 0.*self.Tatm)
        # Effective radius cloud drops
        self.add_input('r_liq', 0.*self.Tatm + 10.)
        self.add_input('r_ice', 0.*self.Tatm + 30.)
        # Albedos
        self.add_input('asdir', 0.*self.Ts + asdir)
        self.add_input('asdif', 0.*self.Ts + asdif)
        self.add_input('aldir', 0.*self.Ts + asdir)
        self.add_input('aldif', 0.*self.Ts + asdif)
        # cosine of the average zenith angle
        self.add_input('cosZen', 0.*self.Ts + cosZen)
        # Insolation in W/m2
        self.add_input('insolation', 0.*self.Ts + insolation)

        self.KM = self.lev.size
        try:
            self.JM = self.lat.size
        except:
            self.JM = 1
        try:
            self.IM = self.lon.size
        except:
            self.IM = 1
        self.do_sw = 1  # '1=do, 0=do not compute SW'
        self.do_lw = 1  # '1=do, 0=do not compute LW'
        self.in_cld = 0 # '1=in-cloud, 0=grid avg cloud water path'
        #  Set up some useful defaults, mostly following climt/state.py
        self.shape3D = (self.KM, self.JM, self.IM)
        self.shape2D = self.shape3D[1:]
        #self.ps = const.ps * np.ones(self.shape2D)
        #  surface pressure should correspond to model domain!
        self.ps = self.lev_bounds[-1] * np.ones(self.shape2D)
        self.p = self.lev
        #   why are we passing missing instead of the actual layer thicknesses?
        self.dp = np.zeros_like(self.p) - 99. # set as missing

        # Surface upwelling LW
        self.flus = np.zeros_like(self.Ts) - 99. # set to missing as default
        # physical constants
        self.g = const.g
        self.Cpd = const.cp
        self.epsilon = const.Rd / const.Rv
        self.stebol = const.sigma

        # initialize diagnostics
        self.add_diagnostic('ASR', 0. * self.Ts)
        self.add_diagnostic('ASRcld', 0. * self.Ts)
        self.add_diagnostic('OLR', 0. * self.Ts)
        self.add_diagnostic('OLRcld', 0. * self.Ts)
        self.add_diagnostic('TdotRad', 0. * self.Tatm)
        self.add_diagnostic('TdotLW', 0. * self.Tatm)
        self.add_diagnostic('TdotSW', 0. * self.Tatm)

        # automatic ozone data initialization
        if O3init:
            datadir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'data', 'ozone'))
            O3filepath = os.path.join(datadir, O3file)
            #  Open the ozone data file
            print 'Getting ozone data from', O3filepath
            O3data = nc.Dataset(O3filepath)
            O3lev = O3data.variables['lev'][:]
            O3lat = O3data.variables['lat'][:]
            #  zonal and time average
            O3zon = np.mean(O3data.variables['OZONE'], axis=(0,3))
            O3global = np.average(O3zon, weights=np.cos(np.deg2rad(O3lat)), axis=1)
            if self.O3.shape == self.lev.shape:
                # 1D interpolation on pressure levels using global average data
                f = interp1d(O3lev, O3global)
                #  interpolate data to model levels
                self.O3 = f(self.lev)
            else:
                #  Attempt 2D interpolation in pressure and latitude
                f2d = interp2d(O3lat, O3lev, O3zon)
                self.O3 = f2d(self.lat, self.lev).transpose()
                try:
                    f2d = interp2d(O3lat, O3lev, O3zon)
                    self.O3 = f2d(self.lat, self.lev).transpose()
                except:
                    print 'Interpolation of ozone data failed.'
                    print 'Reverting to default O3.'

    def _climlab_to_cam3(self, field):
        '''Prepare field with proper dimension order.
        CAM3 code expects 3D arrays with (KM, JM, 1)
        and 2D arrays with (JM, 1).

        climlab grid dimensions are any of:
            - (KM,)
            - (JM, KM)
            - (JM, IM, KM)

        (longitude dimension IM not yet implemented here).'''
        if np.isscalar(field):
            return field
        #  Check to see if column vector needs to be replicated over latitude
        elif self.JM > 1:
            if (field.shape == (self.KM,)):
                return np.tile(field[...,np.newaxis], self.JM)
            else:
                return np.squeeze(np.transpose(field))[..., np.newaxis]
        else:  #  1D vertical model
            return field[..., np.newaxis, np.newaxis]

    def _cam3_to_climlab(self, field):
        ''' Output is either (KM, JM, 1) or (JM, 1).
        Transform this to...
            - (KM,) or (1,)  if JM==1
            - (KM, JM) or (JM, 1)   if JM>1

        (longitude dimension IM not yet implemented).'''
        if self.JM > 1:
            if len(field.shape)==2:
                return field
            elif len(field.shape)==3:
                return np.squeeze(np.transpose(field))
        else:
            return np.squeeze(field)

    def _compute_radiative_heating(self):
        # List of arguments to be passed to extension
        #args = [ getattr(self,key) for key in self.ToExtension ]
        args = []
        for key in ToExtension:
            value = getattr(self, key)
            if np.isscalar(value):
                args.append(value)
            else:
                args.append(self._climlab_to_cam3(value))
        #  new concept -- extension is NOT an attribute of the climlab process
        OutputValues = _cam3_radiation.driver(*args)
        Output = dict( zip(FromExtension, OutputValues ))
        self.Output = Output
        #for name, value in Output.iteritems():
        #    setattr(self, name, value)
        #  SrfRadFlx is net downward flux at surface
        self.heating_rate['Ts'] = self._cam3_to_climlab(Output['SrfRadFlx'])
        # lwhr and swhr are heating rates in W/kg
        #  (qrl and qrs in CAM3 code)
        #  TdotRad is the sum lwhr + swhr, also in W/kg
        #  Need to set to W/m2
        Catm = self.Tatm.domain.heat_capacity
        self.heating_rate['Tatm'] = (self._cam3_to_climlab(Output['TdotRad']) *
                                     (Catm / const.cp))
        #  Set some diagnostics (minimal for now!)
        self.OLR = -self._cam3_to_climlab(Output['LwToa'])
        self.OLRcld = -self._cam3_to_climlab(Output['LwToaCf'])
        self.ASR = self._cam3_to_climlab(Output['SwToa'])
        self.ASRcld = self._cam3_to_climlab(Output['SwToaCf'])
        #  radiative heating rates in K / day
        KperDayFactor = const.seconds_per_day / const.cp
        self.TdotRad = self._cam3_to_climlab(Output['TdotRad']) * KperDayFactor
        self.TdotLW = self._cam3_to_climlab(Output['lwhr']) * KperDayFactor
        self.TdotSW = self._cam3_to_climlab(Output['swhr']) * KperDayFactor


class CAM3Radiation_LW(CAM3Radiation):
    def __init__(self, **kwargs):
        super(CAM3Radiation_LW, self).__init__(**kwargs)
        self.do_sw = 0  # '1=do, 0=do not compute SW'
        self.do_lw = 1  # '1=do, 0=do not compute LW'


class CAM3Radiation_SW(CAM3Radiation):
    def __init__(self, **kwargs):
        super(CAM3Radiation_LW, self).__init__(**kwargs)
        self.do_sw = 1  # '1=do, 0=do not compute SW'
        self.do_lw = 0  # '1=do, 0=do not compute LW'
