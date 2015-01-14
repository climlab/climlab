from climlab.process.energy_budget import _EnergyBudget


class _Radiation(_EnergyBudget):
    '''Abstract parent class for all radiation modules.'''
    #def __init__(self, **kwargs):
    #    super(_Radiation, self).__init__(**kwargs)
    #    self.process_type = 'explicit'

    def emission(self):
        pass
    
    def radiative_heating(self):
        '''Compute radiative flux convergences to get heating rates in W / m**2'''
        self.emission()
        pass

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.
        This method should be over-ridden by daughter classes.'''
        self.radiative_heating()        
        #self.heating_rate = {}
        #for varname in self.state.keys():
        #    self.heating_rate['varname'] = np.zeros_like(self.state[varname])
