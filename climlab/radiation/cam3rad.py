'''
climlab wrap of the CAM3 radiation code
'''
import numpy as np
import netCDF4 as nc
from climlab import constants as const
from climlab.radiation.radiation import Radiation
import _cam3_interface
import os
from scipy.interpolate import interp1d, interp2d


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
                 extname='_cam3_radiation',
                 **kwargs):
        super(CAM3Radiation, self).__init__(**kwargs)
        newinput = ['q',
                    'CO2',
                    'N2O',
                    'CH4',
                    'CFC11',
                    'CFC12',
                    'O3',
                    'cldf',
                    'clwp',
                    'ciwp',
                    'r_liq',
                    'r_ice',
                    'asdir',
                    'asdif',
                    'aldir',
                    'aldif',
                    'cosZen',
                    'insolation']
        self.add_input(newinput)
        # well-mixed absorbing gases in ppmv (scalars)
        self.CO2 = CO2
        self.N2O = N2O
        self.CH4 = CH4
        self.CFC11 = CFC11
        self.CFC12 = CFC12

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
        #lev = (np.arange(self.KM)+0.5) * const.ps/self.KM
        #self.p = np.transpose(np.resize(lev,self.shape3D[::-1]))
        self.p = self.lev
        self.dp = np.zeros_like(self.p) - 99. # set as missing
        #  Specific humidity in kg/kg
        if q is None:
            self.q = 1.e-8*np.ones_like(self.Tatm)  # Driver.f90 expect q in kg/kg
        else:
            self.q = q
        self.O3 = np.ones_like(self.Tatm) * O3

        # Cloud frac
        self.cldf = np.zeros_like(self.Tatm)
        # Cloud water path
        self.clwp = np.zeros_like(self.Tatm)
        self.ciwp = np.zeros_like(self.Tatm)
        # Effective radius cloud drops
        self.r_liq = np.zeros_like(self.Tatm) + 10.
        self.r_ice = np.zeros_like(self.Tatm) + 30.
        # Surface upwelling LW
        self.flus = np.zeros_like(self.Ts) - 99. # set to missing as default
        # Albedos
        self.asdir = np.zeros_like(self.Ts) + asdir
        self.asdif = np.zeros_like(self.Ts) + asdif
        self.aldir = np.zeros_like(self.Ts) + asdir
        self.aldif = np.zeros_like(self.Ts) + asdif
        # cosine of the average zenith angle
        self.cosZen = np.zeros_like(self.Ts) + cosZen
        # Insolation in W/m2
        self.insolation = np.zeros_like(self.Ts) + insolation
        # physical constants
        self.g = const.g
        self.Cpd = const.cp
        self.epsilon = const.Rd / const.Rv
        self.stebol = const.sigma

        # initialize diagnostics
        self.init_diagnostic('ASR', 0. * self.Ts)
        self.init_diagnostic('ASRcld', 0. * self.Ts)
        self.init_diagnostic('OLR', 0. * self.Ts)
        self.init_diagnostic('OLRcld', 0. * self.Ts)
        self.init_diagnostic('TdotRad', 0. * self.Tatm)
        self.init_diagnostic('TdotLW', 0. * self.Tatm)
        self.init_diagnostic('TdotSW', 0. * self.Tatm)

        # automatic ozone data initialization
        if O3init:
            datadir = os.path.abspath(os.path.dirname(__file__) + '/../data/ozone')
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

        #  Check to see if an extension object already exists.
        #  Might need to change the name
        extname = _cam3_interface._modify_extname(extname)
        _cam3_interface._build_extension(KM=self.KM, JM=self.JM, IM=self.IM,
            extname=extname)
        _cam3_interface._init_extension(module=self, extname=extname)

    def _climlab_to_cam3(self, field):
        '''Prepare field wit proper dimension order.
        CAM3 code expects 3D arrays with (KM, JM, 1)
        and 2D arrays with (JM, 1).

        climlab grid dimensions are any of:
            - (KM,)
            - (JM, KM)

        (longitude dimension IM not yet implemented).'''
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
        for key in _cam3_interface.ToExtension:
            value = getattr(self, key)
            if np.isscalar(value):
                args.append(value)
            else:
                args.append(self._climlab_to_cam3(value))
        OutputValues = self.extension.driver(*args)
        Output = dict( zip(_cam3_interface.FromExtension, OutputValues ))
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
