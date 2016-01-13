from climlab.process.energy_budget import EnergyBudget
from climlab.domain.field import global_mean


class BudykoTransport(EnergyBudget):
    ''' module which calculates the ebm heat transport as the difference 
    between the local temperature and the global mean temperature.
    
    parameters: b   - budyko transport parameter
    
    implemented by Moritz Kreuzer
    '''
    def __init__(self, b=3.81, **kwargs):
        super(BudykoTransport, self).__init__(**kwargs)
        self.b = b        
        
    @property
    def b(self):
        return self._b
    @b.setter
    def b(self, value):
        self._b = value
        self.param['b'] = value
        
    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
        for varname, value in self.state.iteritems():
            self.heating_rate[varname] = - self.b * (value - global_mean(value))

