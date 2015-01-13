import numpy as np
from time_dependent_process import _TimeDependentProcess
import heat_capacity


#==============================================================================
# def AplusBT(T, param):
#     tendencies = {}
#     diagnostics = {}
#     A = param['A']
#     B = param['B']
#     OLR = A + B*T
#     tendencies['Ts'] = -OLR / heat_capacity.slab_ocean(param['water_depth'])
#     diagnostics['OLR'] = OLR
#==============================================================================


class _Radiation(_TimeDependentProcess):
    '''Abstract parent class for all radiation modules.'''
    def __init__(self, **kwargs):
        super(_Radiation, self).__init__(**kwargs)
        #  heat capacity of atmospheric layers
        self.c_atm = heat_capacity.atmosphere(self.grid['lev'].delta)
        #  heat capacity of surface in J / m**2 / K
        self.c_sfc = heat_capacity.slab_ocean(self.param['water_depth'])
        self.process_type = 'explicit'

    def emission(self):
        '''No emission unless overwritten by daughter class.'''
        emit_sfc = np.zeros_like(self.state['Ts'])
        emit_atm = np.zeros_like(self.state['Tatm'])
        self.diagnostics['emit_sfc'] = emit_sfc
        self.diagnostics['emit_atm'] = emit_atm

    def radiative_tendencies(self):
        pass

    def compute(self):
        self.radiative_tendencies()
