from climlab.process.time_dependent_process import _TimeDependentProcess
import climlab.utils.heat_capacity as heat_capacity


class AplusBT(_TimeDependentProcess):
    '''The simplest linear longwave radiation module.'''
    def __init__(self, A=200., B=2., **kwargs):
        super(AplusBT, self).__init__(**kwargs)
        self.param['A'] = A
        self.param['B'] = B
    
    def compute(self):
        A = self.param['A']
        B = self.param['B']
        C = heat_capacity.slab_ocean(self.param['water_depth'])
        # assume that all state variables are temperature
        for varname, value in self.state.iteritems():
            flux = A + B * value
            self.diagnostics['OLR'] = flux
            self.tendencies[varname] = (-self.diagnostics['OLR'] *
                                 self.param['timestep'] / C)
