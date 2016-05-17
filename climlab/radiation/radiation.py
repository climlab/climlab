'''
Radiation is the base class climlab grey radiation and band modules

Basic characteristics:

State:
- Ts (surface radiative temperature)
- Tatm (air temperature)

Input (specified or provided by parent process):
- flux_from_space

Diagnostics (minimum)
- flux_to_sfc
- flux_to_space
- absorbed
- absorbed_total
'''
import numpy as np
from climlab.process.energy_budget import EnergyBudget


class Radiation(EnergyBudget):
    '''Base class for radiation models.

    The following boundary values need to be specified by user or parent process:
    - flux_from_space

    The following values are computed are stored in the .diagnostics dictionary:
    - flux_from_sfc
    - flux_to_sfc
    - flux_to_space
    - absorbed
    - absorbed_total
    (all in W/m2)
    '''
    def __init__(self, **kwargs):
        super(Radiation, self).__init__(**kwargs)
        newinput = ['flux_from_space',]
        self.add_input(newinput)
        self.flux_from_space = 0. * self.Ts
        #  initialize all diagnostics to zero
        self.init_diagnostic('flux_from_sfc', 0. * self.Ts)
        self.init_diagnostic('flux_to_sfc', 0. * self.Ts)
        self.init_diagnostic('flux_to_space', 0. * self.Ts)
        self.init_diagnostic('absorbed', 0. * self.Tatm)
        self.init_diagnostic('absorbed_total', 0. * self.Ts)

    def _compute_radiative_heating(self):
        '''Compute radiative fluxes and heating rates.

        Must be implemented by daughter classes.'''
        pass

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in :math:`W/m^2`.'''
        self._compute_radiative_heating()


class RadiationLW(Radiation):
    def __init__(self, emissivity_sfc=1., albedo_sfc=0., **kwargs):
        super(RadiationLW, self).__init__(emissivity_sfc=emissivity_sfc,
                                          albedo_sfc=albedo_sfc,
                                          **kwargs)


class RadiationSW(Radiation):
    def __init__(self, emissivity_sfc=0., albedo_sfc=1., **kwargs):
        super(RadiationLW, self).__init__(emissivity_sfc=emissivity_sfc,
                                          albedo_sfc=albedo_sfc,
                                          **kwargs)
