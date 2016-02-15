import numpy as np
from climlab.process.diagnostic import DiagnosticProcess
from climlab.domain.field import Field
from climlab.utils.legendre import P2
from climlab import constants as const
from climlab.solar.insolation import daily_insolation

# REVISE TO MAKE ALL OF THESE CALLABLE WITH NO ARGUMENTS.
# SET SOME SENSIBLE DEFAULTS FOR DOMAINS

#  the diagnostic self.insolation is set with correct dimensions at
#  creation time. After that, make sure to always modify it though
#  self.insolation[:] = ...
#  so that links to the insolation in other processes will work

#  REALLY NEED TO CHANGE THE WAY DIAGNOSTIC PROCESSES ARE CREATED
#  CAN'T RELY ON NAMES OF DOMAINS
#  should be easy to pass a state variable object or something with the right shape
#  and have the process gracefully set the correct dimensions


class _Insolation(DiagnosticProcess):
    '''Parent class for insolation processes.
    Calling compute() will update self.insolation with current values.'''
    # parameter S0 is now stored using a python property
    # can be changed through self.S0 = newvalue
    # which will also update the parameter dictionary
    # CAUTION: changing self.param['S0'] will not work!
    def __init__(self, S0=const.S0, **kwargs):
        super(_Insolation, self).__init__(**kwargs)
        #  initialize diagnostic with correct shape
        self.init_diagnostic('insolation')
        try:
            self.insolation = np.zeros(self.domains['sfc'].shape)
        except:
            self.insolation = np.zeros(self.domains['default'].shape)
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
        pass

    def _get_current_insolation(self):
        pass

    def _compute(self):
        self._get_current_insolation()
        return {}

class FixedInsolation(_Insolation):
    def __init__(self, S0=const.S0/4, **kwargs):
        super(FixedInsolation, self).__init__(S0=S0, **kwargs)

    def _compute_fixed(self):
        #ins_adjustment = self.S0 - self.insolation
        #self.insolation += ins_adjustment
        self.insolation[:] = self.S0


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
        phi = np.deg2rad(self.lat)
        #  Why is there a silent fail here? Should get rid of this.
        try:
            insolation = self.S0 / 4 * (1. + self.s2 * P2(np.sin(phi)))
            # make sure that the diagnostic has the correct field dimensions.
            dom = self.domains['default']
            #self.insolation = Field(insolation, domain=dom)
            self.insolation[:] = Field(insolation, domain=dom)
        except:
            pass


# These classes calculate insolation based on orbital parameters
#  and astronomical formulas

class AnnualMeanInsolation(_Insolation):
    def __init__(self, S0=const.S0, orb=const.orb_present, **kwargs):
        super(AnnualMeanInsolation, self).__init__(S0=S0, **kwargs)
        #self.param['orb'] = orb
        self.orb = orb
        self._compute_fixed()

    @property
    def orb(self):
        return self._orb
    @orb.setter
    def orb(self, value):
        self._orb = value
        self.param['orb'] = value
        self._compute_fixed()

    def _daily_insolation_array(self):
        lat = self.lat
        days_of_year = self.time['days_of_year']
        orb = self.orb
        S0 = self.S0
        return daily_insolation(lat, days_of_year, orb=orb, S0=S0)

    def _compute_fixed(self):
        try:
            temp_array = self._daily_insolation_array()
            insolation = np.mean(temp_array, axis=1)
            # make sure that the diagnostic has the correct field dimensions.
            dom = self.domains['default']
            self.insolation[:] = Field(insolation, domain=dom)
        except:
            pass


class DailyInsolation(AnnualMeanInsolation):
    def _compute_fixed(self):
        try:
            self.insolation_array = self._daily_insolation_array()
        except:
            pass

    def _get_current_insolation(self):
        #  this probably only works for 1D (latitude) domains
        insolation_array = self.insolation_array
        # make sure that the diagnostic has the correct field dimensions.
        dom = self.domains['default']
        time_index = self.time['day_of_year_index']   # THIS ONLY WORKS IF self IS THE MASTER PROCESS
        insolation = insolation_array[..., time_index]
        self.insolation[:] = Field(insolation, domain=dom)
