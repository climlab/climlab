import numpy as np
from climlab.process.time_dependent_process import TimeDependentProcess
import xray
from climlab.domain import grid

class EnergyBudget(TimeDependentProcess):
    '''Parent class for explicit energy budget processes.
    Solves equations like C dT/dt = (flux convergence)
    Every EnergyBudget object has a heat_capacity dictionary
    with items corresponding to each state variable.'''
    def __init__(self, **kwargs):
        super(EnergyBudget, self).__init__(**kwargs)
        #self.attrs['process_type'] = 'explicit'
        self.process_type = 'explicit'
        #self.heating_rate = {}
        self.heat_capacity = grid.heat_capacity(self.state)

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.
        Return them in a new xray.Dataset corresponding to state variables
        This method should be over-ridden by daughter classes.'''
        heating_rates = self.state * 0.
        return heating_rates

    def _temperature_tendencies(self):
        heating_rates = self._compute_heating_rates()
        try:
            tendencies = heating_rates / self.heat_capacity
        except:
            self.heat_capacity = grid.heat_capacity(self.state)
            tendencies = heating_rates / self.heat_capacity
        return tendencies

    def _compute(self):
        '''Update all diagnostic quantities using current model state.'''
        tendencies = self._temperature_tendencies()
        return tendencies

#==============================================================================
#
# class ExternalEnergySource(EnergyBudget):
#     '''A fixed energy source or sink to be specified by the user.
#     The user should modify the fields in the heating_rate dictionary,
#     which contain heating rates in W / m**2 for all state variables.'''
#     def __init__(self, **kwargs):
#         super(ExternalEnergySource, self).__init__(**kwargs)
#         for varname in self.state.keys():
#             self.heating_rate[varname] = np.zeros_like(self.state[varname])
#
#     def _compute_heating_rates(self):
#         pass
#
#==============================================================================
