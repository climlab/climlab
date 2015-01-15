import numpy as np
from climlab.process.time_dependent_process import TimeDependentProcess


class EnergyBudget(TimeDependentProcess):
    '''Parent class for explicit energy budget processes.
    Solves equations like C dT/dt = (flux convergence)
    Every EnergyBudget object has a heat_capacity dictionary
    with items corresponding to each state variable.'''
    def __init__(self, **kwargs):
        super(EnergyBudget, self).__init__(**kwargs)
        self.process_type = 'explicit'
        self.heating_rate = {}

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.
        This method should be over-ridden by daughter classes.'''
        for varname in self.state.keys():
            self.heating_rate['varname'] = np.zeros_like(self.state[varname])

    def _temperature_tendencies(self):
        self._compute_heating_rates()
        for varname, value in self.state.iteritems():
            #C = self.state_domain[varname].heat_capacity
            C = value.domain.heat_capacity
            self.tendencies[varname] = (self.heating_rate[varname] / C)

    def compute(self):
        '''Update all diagnostic quantities using current model state.'''
        self._temperature_tendencies()
