'''Simple linear radiation module.
Usage example:

import climlab.radiation.AplusBT as AplusBT
import climlab.domain.domain as domain
import climlab.domain.field as field
dom = domain.single_column()  # creates a column atmosphere and scalar surface
# here we just want the surface
sfc = dom['sfc']
# Create a state variable
Ts = field.Field(15., domain=sfc)
# Make a dictionary of state variables
s = {'Ts': Ts}
olr = AplusBT.AplusBT(state=s)
#  OR, we can pass a single state variable
olr = AplusBT.AplusBT(state=Ts)
# to compute tendencies and diagnostics
olr.compute()
#  or to actually update the temperature
olr.step_forward()
print olr.state
'''
from climlab.radiation.radiation import _Radiation


class AplusBT(_Radiation):
    '''The simplest linear longwave radiation module.
    Should be invoked with a single temperature state variable.'''
    def __init__(self, A=200., B=2., **kwargs):
        super(AplusBT, self).__init__(**kwargs)
        if 'A' not in self.param:
            self.param['A'] = A
            self.param['B'] = B

    def emission(self):
        A = self.param['A']
        B = self.param['B']
        for varname, value in self.state.iteritems():
            flux = A + B * value
            self.diagnostics['OLR'] = flux
    
    def radiative_heating(self):
        '''Compute radiative flux convergences to get heating rates in W / m**2'''
        super(AplusBT, self).radiative_heating()
        for varname, value in self.state.iteritems():
            self.heating_rate[varname] = -self.diagnostics['OLR']
