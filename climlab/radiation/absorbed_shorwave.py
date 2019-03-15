from __future__ import division, print_function
from climlab.process.energy_budget import EnergyBudget
from climlab import constants as const


class SimpleAbsorbedShortwave(EnergyBudget):
    '''A class for the shortwave radiation process in a one-layer EBM.
    The basic assumption is that all the shortwave absorption occurs at the surface.

    Computes the diagnostic ``ASR`` (absorbed shortwave radiation)
    from the formula ``self.ASR = (1-self.albedo) * self.insolation``
    and applies this as a tendency on the surface temperature ``self.Ts``

    ``albedo`` and ``insolation`` are given as inputs.
    These should either be scalars
    or have same dimensions as state variable ``Ts``

    User can supply constants, or link to diagnostics of specific insolation
    and albedo processes.
    '''
    def __init__(self,
                 insolation=const.S0/4,
                 albedo=0.3,
                 **kwargs):
        super(SimpleAbsorbedShortwave, self).__init__(**kwargs)
        self.add_input('albedo', albedo)
        self.add_input('insolation', insolation)
        self.add_diagnostic('ASR', 0.*self.Ts)
        self.topdown = False  # call subprocess compute methods first

    def _compute_heating_rates(self):
        self.ASR = (1-self.albedo) * self.insolation
        self.heating_rate['Ts'] = self.ASR
