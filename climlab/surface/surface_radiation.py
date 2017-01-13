from __future__ import division
import numpy as np
from climlab import thermo
from climlab.process.energy_budget import EnergyBudget


class SurfaceRadiation(EnergyBudget):
    def __init__(self, albedo_sfc=None, **kwargs):
        super(SurfaceRadiation, self).__init__(**kwargs)
        newinput = ['albedo_sfc',
                    'LW_from_atm',
                    'SW_from_atm',]
        self.declare_input(newinput)
        if albedo_sfc is None:
            self.albedo_sfc = np.zeros_like(self.Ts)
        else:
            self.albedo_sfc = albedo_sfc*np.ones_like(self.Ts)
        self.LW_from_atm = 0. * self.Ts
        self.SW_from_atm = 0. * self.Ts
        newdiags = ['LW_to_atm',
                    'SW_to_atm']
        for name in newdiags:
            self.add_diagnostic(name)
        self.LW_to_atm = 0. * self.Ts
        self.SW_to_atm = 0. * self.Ts
        self.heating_rate['Tatm'] = np.zeros_like(self.Tatm)

    def _compute_emission(self):
        return thermo.blackbody_emission(self.Ts)

    def _compute_reflected_flux(self):
        return self.SW_from_atm * self.albedo_sfc

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in :math:`W/m^2`.'''
        self.LW_to_atm = self._compute_emission()
        self.SW_to_atm = self._compute_reflected_flux()
        self.heating_rate['Ts'] = ( self.LW_from_atm - self.LW_to_atm
                                  + self.SW_from_atm - self.SW_to_atm )
