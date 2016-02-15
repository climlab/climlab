import numpy as np
from climlab.process.time_dependent_process import TimeDependentProcess


class EnergyBudget(TimeDependentProcess):
    '''Parent class for explicit energy budget processes.
    Solves equations like C dT/dt = (flux convergence)
    Every EnergyBudget object has a heat_capacity dictionary
    with items corresponding to each state variable.'''
    def __init__(self, **kwargs):
        super(EnergyBudget, self).__init__(**kwargs)
        self.time_type = 'explicit'
        self.heating_rate = {}

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.
        This method should be over-ridden by daughter classes.'''
        for varname in self.state.keys():
            self.heating_rate[varname] = self.state[varname] * 0.

    def _temperature_tendencies(self):
        self._compute_heating_rates()
        tendencies = {}
        for varname, value in self.state.iteritems():
            #C = self.state_domain[varname].heat_capacity
            C = value.domain.heat_capacity
            try:  # there may be state variables without heating rates
                tendencies[varname] = (self.heating_rate[varname] / C)
            except:
                pass
        return tendencies

    def _compute(self):
        tendencies = self._temperature_tendencies()
        return tendencies


class ExternalEnergySource(EnergyBudget):
    '''A fixed energy source or sink to be specified by the user.
    The user should modify the fields in the heating_rate dictionary,
    which contain heating rates in W / m**2 for all state variables.'''
    def __init__(self, **kwargs):
        super(ExternalEnergySource, self).__init__(**kwargs)
        for varname in self.state.keys():
            self.heating_rate[varname] = self.state[varname] * 0.

    def _compute_heating_rates(self):
        pass
