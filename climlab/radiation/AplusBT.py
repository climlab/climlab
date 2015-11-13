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
#  OR, we can pass a single state variable
olr = climlab.radiation.AplusBT(state=Ts)
print olr
# to compute tendencies and diagnostics
olr.compute()
#  or to actually update the temperature
olr.step_forward()
print olr.state


New prototype usage for xray version:
(need some simpler methods for creating new state variables)

import numpy as np
import xray, climlab
initial = climlab.domain.grid.zonal_mean_surface()
Tarray = np.ones_like(initial.lat).reshape((initial.lat.size, 1))
initial['Ts'] = (('lat', 'depth'), Tarray)
model = climlab.radiation.AplusBT(state=initial.copy(deep=True))
tend = model.compute()
model.step_forward()
import matplotlib.pyplot as plt
initial.Ts.plot()
model.Ts.plot()
plt.show()

'''
from climlab.process.energy_budget import EnergyBudget


class AplusBT(EnergyBudget):
    '''The simplest linear longwave radiation module.
    Should be invoked with a single temperature state variable.'''
    def __init__(self, A=200., B=2., **kwargs):
        super(AplusBT, self).__init__(**kwargs)
        #self.attrs['A'] = A
        #self.attrs['B'] = B
        self.A = A
        self.B = B

    @property
    def A(self):
        return self.param['A']
    @A.setter
    def A(self, value):
        self.param['A'] = value
    @property
    def B(self):
        return self.param['B']
    @B.setter
    def B(self, value):
        self.param['B'] = value

#    def emission(self):
#        for varname, value in self.state.iteritems():
#            flux = self.A + self.B * value
#            self.OLR = flux
#            self.diagnostics['OLR'] = self.OLR

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
        flux = self.A + self.B * self.Ts
        #flux.attrs['var_type'] = 'diag'
        self.diagnostic['OLR'] = flux
        heating_rates = -flux
        return heating_rates
