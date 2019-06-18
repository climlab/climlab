from __future__ import division
import numpy as np
from climlab.process.diagnostic import DiagnosticProcess
from climlab import constants as const
from climlab.utils.thermo import clausius_clapeyron


class FixedRelativeHumidity(DiagnosticProcess):
    def __init__(self, relative_humidity=0.77, qStrat=5.E-6, **kwargs):
        '''Compute water vapor mixing ratio profile
        Assuming constant relative humidity.

        relative_humidity is the specified RH.
        Same value is applied everywhere.
        qStrat is the minimum specific humidity, ensuring that there is
        some water vapor in the stratosphere.

        The attribute RH_profile can be modified to set different
        vertical profiles of relative humidity
        (see daughter class ManabeWaterVapor() ).'''
        super(FixedRelativeHumidity, self).__init__(**kwargs)
        newinput = ['relative_humidity',
                    'qStrat',
                    'RH_profile',]
        self.declare_input(newinput)
        self.relative_humidity = relative_humidity
        self.qStrat = qStrat
        self.RH_profile = self.relative_humidity * np.ones_like(self.Tatm)
        #  go ahead and set the initial q based on initial temperature
        self.add_diagnostic('q', 0.*self.Tatm)
        self._compute()

    def _compute(self):
        es = clausius_clapeyron(self.Tatm)
        e = self.RH_profile * es
        # convert to specific humidity (assume dilute)
        qH2O = e/self.lev * const.Rd / const.Rv
        #  mixing ratio can't be smaller than qStrat
        #  (need some water in the stratosphere!)
        q = np.maximum(self.qStrat, qH2O)
        #  Just set this directly here
        q_adjustment = q - self.q
        self.q += q_adjustment
        return {}



class ManabeWaterVapor(FixedRelativeHumidity):
    def __init__(self, **kwargs):
        '''Compute water vapor mixing ratio profile following
        Manabe and Wetherald JAS 1967
        Fixed surface relative humidity and a specified fractional profile.

        relative_humidity is the specified surface RH
        qStrat is the minimum specific humidity, ensuring that there is
        some water vapor in the stratosphere.'''
        super(ManabeWaterVapor, self).__init__(**kwargs)
        p = self.lev
        Q = p / const.ps
        self.RH_profile = self.relative_humidity * ((Q - 0.02) / (1-0.02))
        self._compute() # call this again so the diagnostic is correct initially
