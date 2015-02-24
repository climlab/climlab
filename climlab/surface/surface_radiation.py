import numpy as np
from climlab import thermo
from climlab.process.energy_budget import EnergyBudget


class SurfaceRadiation(EnergyBudget):
    def __init__(self, albedo_sfc=None, **kwargs):
        super(SurfaceRadiation, self).__init__(**kwargs)
        if albedo_sfc is None:
            self.albedo_sfc = np.zeros_like(self.Ts)
        else:
            self.albedo_sfc = albedo_sfc*np.ones_like(self.Ts)
        self.LW_from_atm = np.zeros_like(self.Ts)
        self.SW_from_atm = np.zeros_like(self.Ts)
        self.LW_to_atm = np.zeros_like(self.Ts)
        self.SW_to_atm = np.zeros_like(self.Ts)
        self.heating_rate['Tatm'] = np.zeros_like(self.Tatm)

        
    def emission(self):
        return thermo.blackbody_emission(self.Ts)
    
    def reflected_flux(self):
        return self.SW_from_atm * self.albedo_sfc

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
        self.LW_to_atm = self.emission()
        self.SW_to_atm = self.reflected_flux()
        self.heating_rate['Ts'] = ( self.LW_from_atm - self.LW_to_atm 
                                  + self.SW_from_atm - self.SW_to_atm )
