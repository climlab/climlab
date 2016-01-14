'''
Radiation is the base class for all climlab radiation modules

Basic characteristics:

State:
- Tatm (air temperature)

Input (specified or provided by parent process):
- flux_from_space
- flux_from_sfc

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
    '''Base class for all band radiation models,
    including grey and semi-grey model.

    Input argument absorptivity is band absorptivity
    (should same size as grid).

    By default emissivity = absorptivity.
    Subclasses can override this is necessary (e.g. for shortwave model).

    The following boundary values need to be specified by user or parent process:
    - albedo_sfc (default is zero)
    - flux_from_space
    - flux_from_sfc
    - absorptivity
    - reflectivity (default is zero)
    These are accessible (and settable) as process attributes
    Also stored in process.input dictionary

    The following values are computed are stored in the .diagnostics dictionary:
    - flux_to_sfc
    - flux_to_space
    -  plus a bunch of others!!
    '''
    def __init__(self, **kwargs):
        super(Radiation, self).__init__(**kwargs)
        newinput = ['flux_from_space',
                    'flux_from_sfc']
        self.add_input(newinput)
        self.flux_from_space = 0. * self.Ts
        self.flux_from_sfc = 0. * self.Ts
        newdiags = ['flux_to_sfc',
                    'flux_to_space',
                    'absorbed',
                    'absorbed_total']
        self.add_diagnostics(newdiags)
        #  THESE ARE NOT INPUT! THEY ARE DIAGNOSTICS
        #  But it is helpful to initialize them to zero
        self.flux_to_sfc = 0. * self.Ts
        self.flux_to_space = 0. * self.Ts
        self.absorbed = 0. * self.Tatm
        self.absorbed_total = 0. * self.Ts
        #  This is set to zero because Ts is a state var...
        #   But it shouldn't be??
        self.heating_rate['Ts'] = np.zeros_like(self.Ts)

    def _compute_radiative_heating(self):
        '''Compute radiative fluxes and heating rates.

        Must be implemented by daughter classes.'''
        pass

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
        self._compute_radiative_heating()
