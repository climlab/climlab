from climlab.process.energy_budget import _EnergyBudget


#  I want to have a very simple 'linear' class that just implements 
#  OLR = A + B*T
#  that will work elegantly for any temperature state variable


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


class _Radiation(_EnergyBudget):
    '''Abstract parent class for all radiation modules.'''
    def __init__(self, **kwargs):
        super(_Radiation, self).__init__(**kwargs)
        self.process_type = 'explicit'

    def emission(self):
        pass

    def radiative_tendencies(self):
        pass

    def compute(self):
        self.radiative_tendencies()
        
        