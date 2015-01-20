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
    # parameter S0 is now stored using a python property
    # can be changed through self.S0 = newvalue
    # which will also update the parameter dictionary
    # CAUTION: changing self.param['S0'] will not work!
    def __init__(self, S0=const.S0, **kwargs):
        super(_Insolation, self).__init__(**kwargs)
        self.S0 = S0
        
    @property
    def S0(self):
        return self._S0
    @S0.setter
    def S0(self, value):
        self._S0 = value
        self.param['S0'] = value
        self._compute_fixed()

    def _compute_fixed(self):
        '''Recompute any fixed quantities after a change in parameters'''
        #self.oh = 'eh'
        pass
    
    def _get_current_insolation(self):
        pass

    def compute(self):
        '''Update all diagnostic quantities using current model state.'''
        self._get_current_insolation()


class FixedInsolation(_Insolation):
    def __init__(self, S0=const.S0/4, **kwargs):
        super(FixedInsolation, self).__init__(S0=S0, **kwargs)

    def _compute_fixed(self):
        self.diagnostics['insolation'] = self.S0


class P2Insolation(_Insolation):
    def __init__(self, S0=const.S0, s2=-0.48, **kwargs):
        super(P2Insolation, self).__init__(S0=S0, **kwargs)
        self.s2 = s2

    @property
    def s2(self):
        return self._s2
    @s2.setter
    def s2(self, value):
        self._s2 = value
        self.param['s2'] = value
        self._compute_fixed()

    def _compute_fixed(self):
        lat = self.domains['default'].axes['lat'].points
        phi = np.deg2rad(lat)
        try:
            insolation = self.S0 / 4 * (1. + self.s2 * P2(np.sin(phi)))
            # make sure that the diagnostic has the correct field dimensions.
            dom = self.domains['default']
            self.diagnostics['insolation'] = Field(insolation, domain=dom)
        except:
            pass


class AnnualMeanInsolation(_Insolation):
    def __init__(self, S0=const.S0, orb=const.orb_present, **kwargs):
        super(AnnualMeanInsolation, self).__init__(S0=S0, **kwargs)
        self.param['orb'] = orb
        self._compute_fixed()

    def _compute_fixed(self):
        try:
            lat = self.domains['default'].axes['lat'].points
            days_of_year = np.linspace(0., const.days_per_year, 100)
            temp_array = daily_insolation(lat, days_of_year,
                                          orb=self.param['orb'],
                                          S0=self.S0)
            insolation = np.mean(temp_array, axis=1)
            # make sure that the diagnostic has the correct field dimensions.
            dom = self.domains['default']
            self.diagnostics['insolation'] = Field(insolation, domain=dom)
        except:
            pass


class DailyInsolation(_Insolation):
    def __init__(self, S0=const.S0, orb=const.orb_present, **kwargs):
        super(DailyInsolation, self).__init__(S0=S0, **kwargs)
        self.param['orb'] = orb

    def _compute_fixed(self):
        lat = self.domains['default'].axes['lat'].points
        days_of_year = self.time['days_of_year']
        self.properties['insolation_array'] = daily_insolation(lat, days_of_year, orb=self.param['orb'], S0=self.S0)

    def _get_current_insolation(self):
        #  this probably only works for 1D (latitude) domains
        insolation_array = self.properties['insolation_array']
        # make sure that the diagnostic has the correct field dimensions.
        dom = self.domains['default']
        time_index = self.time['day_of_year_index']   # THIS ONLY WORKS IF self IS THE MASTER PROCESS
        insolation = insolation_array[:, time_index]
        self.diagnostics['insolation'] = Field(insolation, domain=dom)
