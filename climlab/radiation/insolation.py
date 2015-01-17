from climlab.process.diagnostic import DiagnosticProcess
from climlab.utils.legendre import P2
from climlab import constants as const
import numpy as np

# THis is a strictly diagnostic process...
# and also one that doesn't require any state variable!
#  Should use the 'slab atmosphere' domain
#
# serious implementations will have to think about time and access my insolation library
# but the simplest classes will have fixed insolation.

class _Insolation(DiagnosticProcess):
    '''Parent class for insolation processes.
    Calling compute() will update self.diagnostics['insolation']
    with current insolation values.'''
    
    def _get_current_insolation(self):
        pass
    
    def compute(self):
        '''Update all diagnostic quantities using current model state.'''
        self._get_current_insolation()


class FixedInsolation(_Insolation):
    def __init__(self, **kwargs):
        super(FixedInsolation, self).__init__(**kwargs)
        self.diagnostics['insolation'] = self.param['Q']
    def _get_current_insolation(self):
        self.diagnostics['insolation'] = self.param['Q']
        # since this is fixed, could also just assign it in __init__

class P2Insolation(_Insolation):
    def __init__(self, S0=const.S0, s2=-0.48, **kwargs):
        super(P2Insolation, self).__init__(**kwargs)
        if 'Q' in self.param and 'S0' not in self.param:
            self.param['S0'] = 4*self.param['Q']
        elif 'S0' not in self.param and 'Q' not in self.param:
            self.param['S0'] = S0
        if 's2' not in self.param:
            self.param['s2'] = s2
        lat = self.domains['default'].axes['lat'].points
        phi = np.deg2rad(lat)
        self.diagnostics['insolation'] = self.param['S0'] / 4 * (1. + 
                                         self.param['s2'] * P2(np.sin(phi)))
        
    def _get_current_insolation(self):
        pass