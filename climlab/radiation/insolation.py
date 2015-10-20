import numpy as np
from climlab.process.diagnostic import DiagnosticProcess
from climlab.domain.field import Field
from climlab.utils.legendre import P2
from climlab import constants as const
from climlab.solar.insolation import daily_insolation

# REVISE TO MAKE ALL OF THESE CALLABLE WITH NO ARGUMENTS.
# SET SOME SENSIBLE DEFAULTS FOR DOMAINS


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
        #lat = self.domains['default'].axes['lat'].points 
        lat = self.lat
        phi = np.deg2rad(lat)
        try:
            insolation = self.S0 / 4 * (1. + self.s2 * P2(np.sin(phi)))
            # make sure that the diagnostic has the correct field dimensions.
            dom = self.domains['default']
            self.diagnostics['insolation'] = Field(insolation, domain=dom)
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
            self.diagnostics['insolation'] = Field(insolation, domain=dom)
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
        self.diagnostics['insolation'] = Field(insolation, domain=dom)
