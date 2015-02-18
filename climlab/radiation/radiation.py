import numpy as np
from climlab.utils.thermo import blackbody_emission
from climlab.radiation.transmissivity import Transmissivity
from climlab.process.energy_budget import EnergyBudget

#  Renamed from NbandModel to Radiation
#  Now strictly atmospheric process
#  must be coupled to a surface model

class Radiation(EnergyBudget):
    '''Base class for all band radiation models,
    including grey and semi-grey model.
    
    Input argument absorptivity is band absorptivity
    (should same size as grid).
    
    By default emissivity = absorptivity.
    Subclasses can override this is necessary (e.g. for shortwave model)'''
    def __init__(self, absorptivity=None, albedo_sfc=0, **kwargs):
        super(Radiation, self).__init__(**kwargs)
        self.absorptivity = absorptivity
        self.albedo_sfc = albedo_sfc
        self.flux_from_space = np.zeros_like(self.Ts)
        self.flux_to_sfc = np.zeros_like(self.Ts)
        self.flux_from_sfc = np.zeros_like(self.Ts)
        self.heating_rate['Ts'] = np.zeros_like(self.Ts)
        
    @property
    def absorptivity(self):
        return self.trans.absorptivity
    @absorptivity.setter
    def absorptivity(self, value):
        self.trans = Transmissivity(np.array(value))
    @property
    def emissivity(self):
        # This ensures that emissivity = absorptivity at all times
    #  needs to be overridden for shortwave classes
        return self.absorptivity
    @property
    def transmissivity(self):
        return self.trans.transmissivity
    @transmissivity.setter
    def transmissivity(self, value):
        self.absorptivity = 1 - value

    def emission(self):
        return self.emissivity * blackbody_emission(self.Tatm)

    def radiative_heating(self):
        emission = self.emission()
        self.diagnostics['emission'] = emission
        try:
            fromspace = self.flux_from_space

        except:
            fromspace = np.zeros_like(self.state['Ts'])
        
        self.flux_down = self.trans.flux_down(fromspace, emission)
        flux_up_bottom = self.flux_from_sfc + self.albedo_sfc*self.flux_down[0]
        self.flux_up = self.trans.flux_up(flux_up_bottom, emission)
        self.flux_net = self.flux_up - self.flux_down
        # absorbed radiation (flux convergence) in W / m**2
        self.absorbed = -np.diff(self.flux_net)
        self.absorbed_total = np.sum(self.absorbed)
        self.diagnostics['absorbed_atm'] = self.absorbed
        self.heating_rate['Tatm'] = self.absorbed
        self.flux_to_sfc = self.flux_down[0]

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
        self.radiative_heating()        

