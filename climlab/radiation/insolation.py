from climlab.process.diagnostic import DiagnosticProcess
from climlab.utils.legendre import P2

# THis is a strictly diagnostic process...
# and also one that doesn't require any state variable!
#  Should use the 'slab atmosphere' domain
#
# serious implementations will have to think about time and access my insolation library
# but the simplest classes will have fixed insolation.

class _Insolation(DiagnosticProcess):
    '''Parent class for insolation processes.'''
    def compute(self):
        '''Update all diagnostic quantities using current model state.'''
        self._get_current_insolation()


class FixedInsolation(_Insolation):
    def _get_current_insolation(self):
        self.diagnostics['Q'] = self.param['Q']
        

class P2Insolation(_Insolation):
    def _get_current_insolation(self):
        self.diagnostics['Q'] = 