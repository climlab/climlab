import numpy as np
from climlab.process.diagnostic import DiagnosticProcess
from climlab.utils.legendre import P2
from climlab.domain.field import Field


class ConstantAlbedo(DiagnosticProcess):
    def __init__(self, albedo=0.33, **kwargs):
        '''Uniform prescribed albedo.'''
        super(ConstantAlbedo, self).__init__(**kwargs)
        self.albedo = albedo

    @property
    def albedo(self):
        return self.diagnostics['albedo']
    @albedo.setter
    def albedo(self, value):
        dom = self.domains['default']
        self.diagnostics['albedo'] = Field(value, domain=dom)
        self.param['albedo'] = value


class P2Albedo(DiagnosticProcess):
    def __init__(self, a0=0.33, a2=0.25, **kwargs):
        '''Second order Legendre polynomial formula for surface albedo.'''
        super(P2Albedo, self).__init__(**kwargs)
        self.a0 = a0
        self.a2 = a2
        self.init_diagnostic('albedo')
        self._compute_fixed()

    @property
    def a0(self):
        return self._a0
    @a0.setter
    def a0(self, value):
        self._a0 = value
        self.param['a0'] = value
        self._compute_fixed()
    @property
    def a2(self):
        return self._a2
    @a2.setter
    def a2(self, value):
        self._a2 = value
        self.param['a2'] = value
        self._compute_fixed()

    def _compute_fixed(self):
        '''Recompute any fixed quantities after a change in parameters'''
        phi = np.deg2rad(self.lat)
        try:
            albedo = self.a0 + self.a2 * P2(np.sin(phi))
        except:
            albedo = np.zeros_like(phi)
        # make sure that the diagnostic has the correct field dimensions.
        dom = self.domains['default']
        self.albedo = Field(albedo, domain=dom)



class Iceline(DiagnosticProcess):
    def __init__(self, Tf=-10., **kwargs):
        super(DiagnosticProcess, self).__init__(**kwargs)
        self.param['Tf'] = Tf
        self.init_diagnostic('icelat')

    def find_icelines(self):
        Tf = self.param['Tf']
        Ts = self.state['Ts']
        lat_bounds = self.domains['Ts'].axes['lat'].bounds
        self.noice = np.where(Ts >= Tf, True, False)
        self.ice = np.where(Ts < Tf, True, False)
        if self.ice.all():
            # 100% ice cover
            self.icelat = np.array([-0., 0.])
        elif self.noice.all():
            # zero ice cover
            self.icelat = np.array([-90., 90.])
        else:  # there is some ice edge
            # Taking np.diff of a boolean array gives True at the boundaries between True and False
            boundary_indices = np.where(np.diff(self.ice.squeeze()))[0] + 1
            # check for asymmetry case: [-90,x] or [x,90]
            #  -> boundary_indices hold only one value for icelat
            if boundary_indices.size == 1:   
                if self.ice[0] == True:   # case: [x,90]
                    # extend indice array by missing value for northpole
                    boundary_indices = np.append(boundary_indices, self.ice.size)
                elif  self.ice[-1] == True:   # case: [-90,x]
                    # extend indice array by missing value for northpole
                    boundary_indices = np.insert(boundary_indices,0 ,0)
            self.icelat = lat_bounds[boundary_indices]  # an array of boundary latitudes

    def _compute(self):
        self.find_icelines()
        return {}


class StepFunctionAlbedo(DiagnosticProcess):
    def __init__(self, Tf=-10., a0=0.3, a2=0.078, ai=0.62, **kwargs):
        super(DiagnosticProcess, self).__init__(**kwargs)
        self.param['Tf'] = Tf
        self.param['a0'] = a0
        self.param['a2'] = a2
        self.param['ai'] = ai
        sfc = self.domains['Ts']
        self.add_subprocess('iceline', Iceline(Tf=Tf, state=self.state))
        self.add_subprocess('warm_albedo', P2Albedo(a0=a0, a2=a2, domains=sfc))
        self.add_subprocess('cold_albedo', ConstantAlbedo(albedo=ai, domains=sfc))
        self.topdown = False  # call subprocess compute methods first
        self.init_diagnostic('albedo')

    def _get_current_albedo(self):
        '''Simple step-function albedo based on ice line at temperature Tf.'''
        ice = self.subprocess['iceline'].ice
        # noice = self.subprocess['iceline'].diagnostics['noice']
        cold_albedo = self.subprocess['cold_albedo'].albedo
        warm_albedo = self.subprocess['warm_albedo'].albedo
        albedo = Field(np.where(ice, cold_albedo, warm_albedo), domain=self.domains['Ts'])
        return albedo

    def _compute(self):
        self.albedo = self._get_current_albedo()
        return {}
