from __future__ import division
import numpy as np
from climlab.utils.thermo import qsat
from climlab import constants as const
from climlab.process.energy_budget import EnergyBudget
from climlab.domain.field import Field


class SurfaceFlux(EnergyBudget):
    def __init__(self, Cd=3E-3, resistance=1., **kwargs):
        super(SurfaceFlux, self).__init__(**kwargs)
        self.Cd = Cd
        self.resistance = resistance
        self.heating_rate['Tatm'] = np.zeros_like(self.Tatm)
        #  fixed wind speed (for now)
        self.add_input('U', 5. * np.ones_like(self.Ts))
        #  retrieving surface pressure from model grid
        self.ps = self.lev_bounds[-1]

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in :math:`W/m^2`.'''
        self._compute_flux()
        self.heating_rate['Ts'] = -self._flux
        # Modify only the lowest model level
        self.heating_rate['Tatm'][..., -1, np.newaxis] = self._flux

    def _air_density(self, Ta):
        return self.ps * const.mb_to_Pa / const.Rd / Ta


class SensibleHeatFlux(SurfaceFlux):
    def __init__(self, Cd=3E-3, **kwargs):
        super(SensibleHeatFlux, self).__init__(Cd=Cd, **kwargs)
        self.add_diagnostic('SHF', 0.*self.Ts)

    def _compute_flux(self):
        # this ensure same dimensions as Ts
        #  (and use only the lowest model level)
        Ta = Field(self.Tatm[..., -1, np.newaxis], domain=self.Ts.domain)
        Ts = self.Ts
        DeltaT = Ts - Ta
        rho = self._air_density(Ta)
        #  flux from bulk formula
        self._flux = self.resistance * const.cp * rho * self.Cd * self.U * DeltaT
        self.SHF = self._flux



class LatentHeatFlux(SurfaceFlux):
    def __init__(self, Cd=3E-3, **kwargs):
        super(LatentHeatFlux, self).__init__(Cd=Cd, **kwargs)
        self.add_diagnostic('LHF', 0.*self.Ts)
        self.add_diagnostic('evaporation', 0.*self.Ts)

    def _compute_flux(self):
        #  specific humidity at lowest model level
        #  assumes pressure is the last axis
        q = Field(self.q[..., -1, np.newaxis], domain=self.Ts.domain)
        Ta = Field(self.Tatm[..., -1, np.newaxis], domain=self.Ts.domain)
        qs = qsat(self.Ts, self.ps)
        Deltaq = Field(qs - q, domain=self.Ts.domain)
        rho = self._air_density(Ta)
        #  flux from bulk formula
        self._flux = self.resistance * const.Lhvap * rho * self.Cd * self.U * Deltaq
        self.LHF = self._flux
        # evporation rate, convert from W/m2 to m/s
        self.evaporation = self.LHF/const.Lhvap/const.rho_w

    def _compute(self):
        '''Overides the _compute method of EnergyBudget'''
        tendencies = self._temperature_tendencies()
        if 'q' in self.state:
            # in a model with active water vapor, this flux should affect
            #  water vapor tendency, NOT air temperature tendency!
            tendencies['Tatm'] *= 0.
            Pa_per_hPa = 100.
            air_mass_per_area = self.Tatm.domain.lev.delta[...,-1] * Pa_per_hPa / const.g
            specific_humidity_tendency = 0.*self.q
            specific_humidity_tendency[...,-1,np.newaxis] = self.LHF/const.Lhvap / air_mass_per_area
            tendencies['q'] = specific_humidity_tendency
        return tendencies
