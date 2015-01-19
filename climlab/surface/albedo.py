from climlab.process.diagnostic import DiagnosticProcess
import numpy as np


class StepFunctionAlbedo(DiagnosticProcess):
    def __init__(self, Tf=-10., albedo_noice=0.33, albedo_ice=0.6, **kwargs):
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
