import numpy as np
from climlab.process.time_dependent_process import _TimeDependentProcess
import climlab.utils.heat_capacity as heat_capacity


class _EnergyBudget(_TimeDependentProcess):
    '''Parent class for explicit energy budget processes.
    Solves equations like C dT/dt = (flux convergence)
    Every EnergyBudget object has a heat_capacity dictionary
    with items corresponding to each state variable.'''
    def __init__(self, **kwargs):
        super(EnergyBudget, self).__init__(**kwargs)
        self.process_type = 'explicit'
        self.heat_capacity = {}
        for varname in self.state.keys():
            if varname is 'Ts':
                self.heat_capacity[varname] = heat_capacity.slab_ocean(self.param['water_depth'])
            elif varname is 'Tatm':
                self.heat_capacity[varname] = heat_capacity.atmosphere(self.grid['lev'].delta)
            else:
                # unrecognized state variable... mark heat capacity as missing or not applicable
                self.heat_capacity[varname] = None
        
    def _heating_rates(self, statevar):
        '''Compute energy flux convergences to get heating rates in W / m**2.
        Input argument statevar is a string specifying which variable to get heating for.'''
        return np.zeros_like(self.state[statevar])
        
    def _temperature_tendencies(self):
        for varname in self.state.keys():
            self.tendencies[varname] = (self._heating_rates(varname) *
                          self.param['timestep'] / self.heat_capacity[varname])

    def compute(self):
        '''Update all diagnostic quantities using current model state.'''
        self._temperature_tendencies()
