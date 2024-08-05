from __future__ import division
import numpy as np
from .zonal_heat_diffusion import ZonalHeatDiffusion
from climlab.utils.thermo import qsat
from climlab import constants as const


class ZonalMoistDiffusion(ZonalHeatDiffusion):
    def __init__(self, D=0.24, relative_humidity=0.8, **kwargs):
        self.relative_humidity = relative_humidity
        super(ZonalMoistDiffusion, self).__init__(D=D, **kwargs)
        self._update_diffusivity()

    def _update_diffusivity(self):
        Tinterp = np.interp(self.lon_bounds, self.lon, np.squeeze(self.Ts))
        Tkelvin = Tinterp + const.tempCtoK
        f = moist_amplification_factor(Tkelvin, self.relative_humidity)
        heat_capacity = self.Ts.domain.heat_capacity
        self.K = self.D / heat_capacity * const.a**2 * (1+f)

    def _implicit_solver(self):
        self._update_diffusivity()
        #  and then do all the same stuff the parent class would do...
        return super(ZonalMoistDiffusion, self)._implicit_solver()


def moist_amplification_factor(Tkelvin, relative_humidity=0.8):
    '''Compute the moisture amplification factor for the moist diffusivity
    given relative humidity and reference temperature profile.'''
    deltaT = 0.01
    #  slope of saturation specific humidity at 1000 hPa
    dqsdTs = (qsat(Tkelvin+deltaT/2, 1000.) - qsat(Tkelvin-deltaT/2, 1000.)) / deltaT
    return const.Lhvap / const.cp * relative_humidity * dqsdTs
    
