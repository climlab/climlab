import numpy as np
from climlab.process.diagnostic import DiagnosticProcess
from climlab import constants as const
from climlab.utils.thermo import clausius_clapeyron


class ManabeWaterVapor(DiagnosticProcess):
    def __init__(self, relative_humidity=0.77, qStrat=5.E-6, **kwargs):
        '''Compute water vapor mixing ratio profile following 
        Manabe and Wetherald JAS 1967
        Fixed surface relative humidity and a specified fractional profile.

        relative_humidity is the specified surface RH
        qStrat is the minimum specific humidity, ensuring that there is 
        some water vapor in the stratosphere.'''
        super(ManabeWaterVapor, self).__init__(**kwargs)
        self.relative_humidity = relative_humidity
        self.qStrat = qStrat
    
    def compute(self):
        p = self.Tatm.domain.lev
        Q = p / const.ps
        h = self.relative_humidity * ( (Q - 0.02) / (1-0.02) )
        es = clausius_clapeyron( self.Tatm )
        e = h * es
        # convert to specific humidity (assume dilute)
        qH2O = e/p * const.Rd / const.Rv 
        #  mixing ratio can't be smaller than qStrat 
        #  (need some water in the stratosphere!)
        self.q = np.maximum( self.qStrat, qH2O )
