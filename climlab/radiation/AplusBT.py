'''Simple linear radiation module.
Usage example:

import climlab
sfc, atm = climlab.domain.single_column()  # creates a column atmosphere and scalar surface
# Create a state variable
Ts = climlab.Field(15., domain=sfc)
# Make a dictionary of state variables
s = {'Ts': Ts}
olr = climlab.radiation.AplusBT(state=s)
print olr
# to compute tendencies and diagnostics
olr.compute()
#  or to actually update the temperature
olr.step_forward()
print olr.state
'''
from climlab.process.energy_budget import EnergyBudget
from climlab.utils import constants as const
import numpy as np


class AplusBT(EnergyBudget):
    '''The simplest linear longwave radiation module.
    Should be invoked with a single temperature state variable.'''
    def __init__(self, A=200., B=2., **kwargs):
        super(AplusBT, self).__init__(**kwargs)
        self.A = A
        self.B = B
        self.init_diagnostic('OLR', 0. * self.Ts)

    @property
    def A(self):
        return self._A
    @A.setter
    def A(self, value):
        self._A = value
        self.param['A'] = value
    @property
    def B(self):
        return self._B
    @B.setter
    def B(self, value):
        self._B = value
        self.param['B'] = value

    def _compute_emission(self):
        for varname, value in self.state.iteritems():
            self.OLR[:] = self.A + self.B * value

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
        self._compute_emission()
        for varname, value in self.state.iteritems():
            self.heating_rate[varname] = -self.OLR


class AplusBT_CO2(EnergyBudget):
    '''longwave radiation module considering CO2 concentration
    see Caldeira & Kasting [1992] for further reading.

    parameter:  CO2  - CO2 concentration in atmosphere (ppm)

    implemented by Moritz Kreuzer'''
    def __init__(self, CO2=300, **kwargs):
        super(AplusBT_CO2, self).__init__(**kwargs)
        self.CO2 = CO2

    @property
    def CO2(self):
        return self._CO2
    @CO2.setter
    def CO2(self, value):
        self._CO2 = value
        self.param['CO2'] = value

    def emission(self):
        l = np.log(self.CO2/300.)
        A = -326.400 + 9.16100*l - 3.16400*l**2 + 0.546800*l**3
        B =    1.953 - 0.04866*l + 0.01309*l**2 - 0.002577*l**3
        for varname, value in self.state.iteritems():
            flux = A + B * (value + const.tempCtoK)
            self.OLR = flux
            self.diagnostics['OLR'] = self.OLR

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
        self.emission()
        for varname, value in self.state.iteritems():
            self.heating_rate[varname] = -self.OLR
