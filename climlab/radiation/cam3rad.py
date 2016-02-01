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
                 q=None,
                 cosZen=1.,
                 insolation=const.S0/4.,
                 asdir=0.07,
                 asdif=0.07,
                 aldir=0.07,
                 aldif=0.07,
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
        #self.asdir = np.zeros_like(self.Ts) + 0.07
        #self.asdif = np.zeros_like(self.Ts) + 0.07
        #self.aldir = np.zeros_like(self.Ts) + 0.07
        #self.aldif = np.zeros_like(self.Ts) + 0.07
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

        #  THESE ARE NOT INPUT! THEY ARE DIAGNOSTICS
        #  But it is helpful to initialize them to zero
        self.ASR = 0. * self.Ts
        self.ASRcld = 0. * self.Ts
        self.OLR = 0. * self.Ts
        self.OLRcld = 0. * self.Ts
        # newdiags = ['OLR',
        #             'OLRcld',
        #             'ASR',
        #             'ASRcld',
        #            ]
        # self.add_diagnostics(newdiags)

        _cam3_interface._build_extension(self.KM, self.JM, self.IM)
        _cam3_interface._init_extension(self)

    @property
    def ASR(self):
        return self.diagnostics['ASR']
    @ASR.setter
    def ASR(self, value):
        self.diagnostics['ASR'] = value
    @property
    def ASRcld(self):
        return self.diagnostics['ASRcld']
    @ASRcld.setter
    def ASRcld(self, value):
        self.diagnostics['ASRcld'] = value
    @property
    def OLR(self):
        return self.diagnostics['OLR']
    @OLR.setter
    def OLR(self, value):
        self.diagnostics['OLR'] = value
    @property
    def OLRcld(self):
        return self.diagnostics['OLRcld']
    @OLRcld.setter
    def OLRcld(self, value):
        self.diagnostics['OLRcld'] = value

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
