'''
climlab wrap of the CAM3 radiation code


We need to do this more cleanly.
There should be an object like cam3wrap
that handles all the conversion between the two apis.
'''
import numpy as np
from climlab import constants as const
from climlab.radiation.radiation import Radiation
import _cam3_interface


class CAM3Radiation(Radiation):
    def __init__(self,
                 CO2=380.,
                 N2O=1.E-9,
                 CH4=1.E-9,
                 CFC11=1.E-9,
                 CFC12=1.E-9,
                 O3=1.E-9,
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
        self.ps = const.ps * np.ones(self.shape2D)
        #lev = (np.arange(self.KM)+0.5) * const.ps/self.KM
        #self.p = np.transpose(np.resize(lev,self.shape3D[::-1]))
        self.p = self.lev
        self.dp = np.zeros_like(self.p) - 99. # set as missing
        #  Specific humidity in kg/kg
        self.q = 1.e-8*np.ones_like(self.Tatm)  # Driver.f90 expect q in kg/kg
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
        self.asdir = np.zeros_like(self.Ts) + 0.07
        self.asdif = np.zeros_like(self.Ts) + 0.07
        self.aldir = np.zeros_like(self.Ts) + 0.07
        self.aldif = np.zeros_like(self.Ts) + 0.07
        self.cosZen = np.zeros_like(self.Ts) + 1.  # cosine of the average zenith angle
        # Insolation
        self.insolation = 341.5 * np.ones_like(self.Ts)
        # physical constants
        self.g = const.g
        self.Cpd = const.cp
        self.epsilon = const.Rd / const.Rv
        self.stebol = const.sigma

        _cam3_interface._build_extension(self.KM, self.JM, self.IM)
        _cam3_interface._init_extension(self)


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
        #  Need to set to W/m2
        Catm = self.Tatm.domain.heat_capacity
        self.heating_rate['Tatm'] = (self._cam3_to_climlab(Output['TdotRad']) *
                                     (Catm / const.cp))


class CAM3Radiation_LW(CAM3Radiation):
    def __init__(self, **kwargs):
        super(CAM3Radiation_LW, self).__init__(**kwargs)
        self.do_sw = 0  # '1=do, 0=do not compute SW'
        self.do_lw = 1  # '1=do, 0=do not compute LW'
        newdiags = ['OLR']
        self.add_diagnostics(newdiags)

    def _compute_radiative_heating(self):
        super(CAM3Radiation_LW, self)._compute_radiative_heating()
        #  Set some diagnostics
        self.OLR = self._cam3_to_climlab(self.Output['LwToa'])


class CAM3Radiation_SW(CAM3Radiation):
    def __init__(self, **kwargs):
        super(CAM3Radiation_LW, self).__init__(**kwargs)
        self.do_sw = 1  # '1=do, 0=do not compute SW'
        self.do_lw = 0  # '1=do, 0=do not compute LW'
        newdiags = ['ASR']
        self.add_diagnostics(newdiags)

    def _compute_radiative_heating(self):
        super(CAM3Radiation_SW, self)._compute_radiative_heating()
        #  Set some diagnostics
        self.ASR = self._cam3_to_climlab(self.Output['LwToa'])
