import numpy as np
from climlab.process.diagnostic import DiagnosticProcess
from climlab.utils.legendre import P2
from climlab.domain.field import Field


class P2Albedo(DiagnosticProcess):
    def __init__(self, a0=0.33, a2=0.25, **kwargs):
        '''Second order Legendre polynomial formula for surface albedo.'''
        super(P2Albedo, self).__init__(**kwargs)
        self.param['a0'] = a0
        self.param['a2'] = a2
        lat = self.domains['default'].axes['lat'].points
        phi = np.deg2rad(lat)
        albedo = a0 + a2 * P2(np.sin(phi))
        # make sure that the diagnostic has the correct field dimensions.
        dom = self.domains['default']
        self.diagnostics['albedo'] = Field(albedo, domain=dom)


class StepFunctionAlbedo(DiagnosticProcess):
    def __init__(self, Tf=-10., albedo_noice=0.3, albedo_ice=0.6, **kwargs):
        super(DiagnosticProcess, self).__init__(**kwargs)
        self.param['Tf'] = Tf
        self.time_type = 'diagnostic'
        self.properties['albedo_noice'] = albedo_noice
        self.properties['albedo_ice'] = albedo_ice

    def _get_current_albedo(self):
        '''Simple step-function albedo based on ice line at temperature Tf.'''
        Tf = self.param['Tf']
        Ts = self.state['Ts']
        return np.where(Ts >= Tf, self.properties['albedo_noice'], 
                        self.properties['albedo_ice'])
                        
    def compute(self):
        self.diagnostics['albedo'] = self._get_current_albedo()
