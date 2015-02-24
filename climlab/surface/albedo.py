import numpy as np
from climlab.process.diagnostic import DiagnosticProcess
from climlab.utils.legendre import P2
from climlab.domain.field import Field


class ConstantAlbedo(DiagnosticProcess):
    def __init__(self, albedo=0.33, **kwargs):
        '''Uniform prescribed albedo.'''
        super(ConstantAlbedo, self).__init__(**kwargs)
        self.param['albedo'] = albedo
        #lat = self.domains['default'].axes['lat'].points
        #albedo = np.ones_like(lat)
        # make sure that the diagnostic has the correct field dimensions.
        dom = self.domains['default']
        self.diagnostics['albedo'] = Field(albedo, domain=dom)
        

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


class Iceline(DiagnosticProcess):
    def __init__(self, Tf=-10., **kwargs):
        super(DiagnosticProcess, self).__init__(**kwargs)
        self.param['Tf'] = Tf

    def find_icelines(self):
        Tf = self.param['Tf']
        Ts = self.state['Ts']
        lat_bounds = self.domains['Ts'].axes['lat'].bounds
        noice = np.where(Ts >= Tf, True, False)
        ice = np.where(Ts < Tf, True, False)
        self.diagnostics['noice'] = noice
        self.diagnostics['ice'] = ice
        if ice.all():
            # 100% ice cover
            icelat = np.array([-0., 0.])
        elif noice.all():
            # zero ice cover
            icelat = np.array([-90., 90.])
        else:  # there is some ice edge
            # Taking np.diff of a boolean array gives True at the boundaries between True and False
            boundary_indices = np.where(np.diff(ice.squeeze()))[0] + 1
            icelat = lat_bounds[boundary_indices]  # an array of boundary latitudes
        self.diagnostics['icelat'] = icelat

        
    def compute(self):
        self.find_icelines()

 
class StepFunctionAlbedo(DiagnosticProcess):
    def __init__(self, Tf=-10., a0=0.3, a2=0.078, ai=0.62, **kwargs):
        super(DiagnosticProcess, self).__init__(**kwargs)
        self.param['Tf'] = Tf
        self.param['a0'] = a0
        self.param['a2'] = a2
        self.param['ai'] = ai
        sfc = self.domains['Ts']
        self.add_subprocess('iceline', Iceline(Tf=Tf, state=self.state))
        self.add_subprocess('warm albedo', P2Albedo(a0=a0, a2=a2, domains=sfc))
        self.add_subprocess('cold albedo', ConstantAlbedo(albedo=ai, domains=sfc))
        self.topdown = False  # call subprocess compute methods first

    def _get_current_albedo(self):
        '''Simple step-function albedo based on ice line at temperature Tf.'''
        ice = self.subprocess['iceline'].diagnostics['ice']
        # noice = self.subprocess['iceline'].diagnostics['noice']
        cold_albedo = self.subprocess['cold albedo'].diagnostics['albedo']
        warm_albedo = self.subprocess['warm albedo'].diagnostics['albedo']
        albedo = Field(np.where(ice, cold_albedo, warm_albedo), domain=self.domains['Ts'])
        return albedo

    def compute(self):
        self.diagnostics['albedo'] = self._get_current_albedo()
