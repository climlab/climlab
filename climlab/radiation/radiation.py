from climlab.process.energy_budget import EnergyBudget


class _Radiation(EnergyBudget):
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
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
        self.radiative_heating()        

class _ShortwaveRadiation(_Radiation):
    def __init__(self, **kwargs):
        super(_ShortwaveRadiation, self).__init__(**kwargs)
        self.input['from space'] = None
