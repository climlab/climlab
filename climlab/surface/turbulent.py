import numpy as np
from climlab.utils.thermo import qsat
from climlab import constants as const
from climlab.process.energy_budget import EnergyBudget


class SurfaceFlux(EnergyBudget):
    def __init__(self, Cd=3E-3, **kwargs):
        super(SurfaceFlux, self).__init__(**kwargs)
        self.Cd = Cd
        #  fixed wind speed (for now)
        self.U = 5. * np.ones_like(self.Ts)
        self.heating_rate['Tatm'] = np.zeros_like(self.Tatm)

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W/m2.'''
        self.compute_flux()
        self.heating_rate['Ts'] = -self.flux
        self.heating_rate['Tatm'][..., 0, np.newaxis] = self.flux


class SensibleHeatFlux(SurfaceFlux):
    def compute_flux(self):
        # this ensure same dimensions as Ts
        Ta = self.Tatm[..., 0, np.newaxis]
        Ts = self.Ts
        DeltaT = Ts - Ta
        #  air density
        rho = const.ps * const.mb_to_Pa / const.Rd / Ta
        #  wind speed
        #  flux from bulk formula
        self.flux = const.cp*rho*self.Cd*self.U*DeltaT
        self.diagnostics['SHF'] = self.flux


class LatentHeatFlux(SurfaceFlux):
    def compute_flux(self):
        #  specific humidity at lowest model level
        #  assumes pressure is the last axis
        q = self.q[..., 0, np.newaxis]
        Ta = self.Tatm[..., 0, np.newaxis]
        qs = qsat(self.Ts, const.ps)
        Deltaq = qs - q
        #  air density
        rho = const.ps * const.mb_to_Pa / const.Rd / Ta
        #  flux from bulk formula
        self.flux = const.Lhvap * rho * self.Cd * self.U * Deltaq
        self.diagnostics['LHF'] = self.flux
