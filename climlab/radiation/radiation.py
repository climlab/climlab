'''
Radiation is the base class for all climlab radiation modules

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
from climlab.utils.thermo import blackbody_emission
from climlab.radiation.transmissivity import Transmissivity
from climlab.process.energy_budget import EnergyBudget


class Radiation(EnergyBudget):
    '''Base class for radiation models.

    The following boundary values need to be specified by user or parent process:
    - flux_from_space

    The following values are computed are stored in the .diagnostics dictionary:
    - flux_from_sfc
    - flux_to_sfc
    - flux_to_space
    -  plus a bunch of others!!
    '''
    def __init__(self, **kwargs):
        super(Radiation, self).__init__(**kwargs)
        newinput = ['flux_from_space',]
        self.add_input(newinput)
        self.flux_from_space = 0. * self.Ts
        newdiags = ['flux_from_sfc',
                    'flux_to_sfc',
                    'flux_to_space',
                    'absorbed',
                    'absorbed_total']
        self.add_diagnostics(newdiags)
        #  THESE ARE NOT INPUT! THEY ARE DIAGNOSTICS
        #  But it is helpful to initialize them to zero
        self.flux_from_sfc = 0. * self.Ts
        self.flux_to_sfc = 0. * self.Ts
        self.flux_to_space = 0. * self.Ts
        self.absorbed = 0. * self.Tatm
        self.absorbed_total = 0. * self.Ts

    def _compute_radiative_heating(self):
        '''Compute radiative fluxes and heating rates.

        Must be implemented by daughter classes.'''
        pass

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
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
