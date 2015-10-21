import numpy as np
from climlab.process.time_dependent_process import TimeDependentProcess
import xray


class EnergyBudget(TimeDependentProcess):
    '''Parent class for explicit energy budget processes.
    Solves equations like C dT/dt = (flux convergence)
    Every EnergyBudget object has a heat_capacity dictionary
    with items corresponding to each state variable.'''
    def __init__(self, **kwargs):
        super(EnergyBudget, self).__init__(**kwargs)
        self.attrs['process_type'] = 'explicit'
        #self.heating_rate = {}

    def _compute_heating_rates(self, inputData=None):
        '''Compute energy flux convergences to get heating rates in W / m**2.
        This method should be over-ridden by daughter classes.'''
        heating_rates = self * 0.        
        diagnostics = xray.Dataset(coords=self.coords)
        return heating_rates, diagnostics

    def _temperature_tendencies(self, inputData=None):
        heating_rates, diagnostics = self._compute_heating_rates(inputData=inputData)
        #  Need to set up heat capacity as a Dataset with variables as process
        tendencies = heating_rates / self.heat_capacity
        return tendencies, diagnostics
        
    def _compute(self, inputData=None):
        '''Update all diagnostic quantities using current model state.'''
        tendencies, diagnostics = self._temperature_tendencies(inputData=inputData)
        return tendencies, diagnostics

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
