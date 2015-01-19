from climlab.process.diagnostic import DiagnosticProcess
from climlab.domain.field import Field
from climlab.utils.legendre import P2
from climlab import constants as const
import numpy as np
from climlab.solar.insolation import daily_insolation

# THis is a strictly diagnostic process...
# and also one that doesn't require any state variable!
#  Should use the 'slab atmosphere' domain
#
# serious implementations will have to think about time and access my insolation library
# but the simplest classes will have fixed insolation.

class _Insolation(DiagnosticProcess):
    '''Parent class for insolation processes.
    Calling compute() will update self.diagnostics['insolation']
    with current insolation values.'''

    def _get_current_insolation(self):
        pass

    def compute(self):
        '''Update all diagnostic quantities using current model state.'''
        self._get_current_insolation()


class FixedInsolation(_Insolation):
    def __init__(self, **kwargs):
        super(FixedInsolation, self).__init__(**kwargs)
        self.diagnostics['insolation'] = self.param['Q']

    def _get_current_insolation(self):
        self.diagnostics['insolation'] = self.param['Q']
        # since this is fixed, could also just assign it in __init__


class P2Insolation(_Insolation):
    def __init__(self, S0=const.S0, s2=-0.48, **kwargs):
        super(P2Insolation, self).__init__(**kwargs)
        if 'Q' in self.param and 'S0' not in self.param:
            self.param['S0'] = 4*self.param['Q']
        elif 'S0' not in self.param and 'Q' not in self.param:
            self.param['S0'] = S0
        if 's2' not in self.param:
            self.param['s2'] = s2
        lat = self.domains['default'].axes['lat'].points
        phi = np.deg2rad(lat)
        insolation = (self.param['S0'] / 4 *
                      (1. + self.param['s2'] * P2(np.sin(phi))))
        # make sure that the diagnostic has the correct field dimensions.
        dom = self.domains['default']
        self.diagnostics['insolation'] = Field(insolation, domain=dom)

    def _get_current_insolation(self):
        pass


class AnnualMeanInsolation(_Insolation):
    def __init__(self, S0=const.S0, orb=const.orb_present, **kwargs):
        super(AnnualMeanInsolation, self).__init__(**kwargs)
        lat = self.domains['default'].axes['lat'].points
        days_of_year = np.linspace(0., const.days_per_year, 100)
        temp_array = daily_insolation(lat, days_of_year, orb=orb, S0=S0)
        insolation = np.mean(temp_array, axis=1)
        # make sure that the diagnostic has the correct field dimensions.
        dom = self.domains['default']
        self.diagnostics['insolation'] = Field(insolation, domain=dom)


class DailyInsolation(_Insolation):
    def __init__(self, S0=const.S0, orb=const.orb_present, **kwargs):
        super(DailyInsolation, self).__init__(**kwargs)
        lat = self.domains['default'].axes['lat'].points
        days_of_year = self.time['days_of_year']
        self.properties['insolation_array'] = daily_insolation(lat, days_of_year, orb=orb, S0=S0)
    
    def _get_current_insolation(self):
        #  this probably only works for 1D (latitude) domains
        insolation_array = self.properties['insolation_array']
        # make sure that the diagnostic has the correct field dimensions.
        dom = self.domains['default']
        time_index = self.time['day_of_year_index']   # THIS ONLY WORKS IF self IS THE MASTER PROCESS
        insolation = insolation_array[:,time_index]
        self.diagnostics['insolation'] = Field(insolation, domain=dom)
