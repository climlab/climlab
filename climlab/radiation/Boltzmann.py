from climlab import constants as const
from climlab.process.energy_budget import EnergyBudget


class Boltzmann(EnergyBudget):
    '''a radiation subprocess which computes longwave radiation
    with the Stefan-Boltzmann law for black/grey body radiation
    
    parameter:  eps - emissivity
                tau - transmissivity
    
    implemented by Moritz Kreuzer    
    '''
    def __init__(self, eps= 0.65, tau=0.95, **kwargs):
        super(Boltzmann, self).__init__(**kwargs)
        self.eps = eps
        self.tau = tau

    @property
    def eps(self):
        return self._eps
    @eps.setter
    def eps(self, value):
        self._eps = value
        self.param['eps'] = value

    @property
    def tau(self):
        return self._tau
    @tau.setter
    def tau(self, value):
        self._tau = value
        self.param['tau'] = value
    
    def emission(self):
        for varname, value in self.state.iteritems():
            flux = self.eps * self.tau * const.sigma * (value + const.tempCtoK)**4.
            self.OLR = flux
            self.diagnostics['OLR'] = self.OLR
    
    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
        self.emission()
        for varname, value in self.state.iteritems():
            self.heating_rate[varname] = -self.OLR
