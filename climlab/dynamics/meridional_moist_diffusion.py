'''Solver for 1D meridional diffusion on moist static energy gradient.

The grid must be evenly spaced in latitude.

Thermal diffusivity D is specified in W/m2/K
at mid-points between latitude points.

The method handles constant or spatially variable diffusivity.

Diffusivity is multiplied by a temperature-dependent factor
accounting for the latent heat contribution to the near-surface
moist static energy.

The magnitude of this factor is determined by input parameter ``relative_humidity``.
'''
from __future__ import division
import numpy as np
from .meridional_heat_diffusion import MeridionalHeatDiffusion
from climlab.utils.thermo import qsat
from climlab import constants as const


class MeridionalMoistDiffusion(MeridionalHeatDiffusion):
    def __init__(self, D=0.24, relative_humidity=0.8, **kwargs):
        self.relative_humidity = relative_humidity
        super(MeridionalMoistDiffusion, self).__init__(D=D, **kwargs)
        self._update_diffusivity()

    def _update_diffusivity(self):
        Tinterp = np.interp(self.lat_bounds, self.lat, np.squeeze(self.Ts))
        Tkelvin = Tinterp + const.tempCtoK
        f = moist_amplification_factor(Tkelvin, self.relative_humidity)
        heat_capacity = self.Ts.domain.heat_capacity
        self.K = self.D / heat_capacity * const.a**2 * (1+f)

    def _implicit_solver(self):
        self._update_diffusivity()
        #  and then do all the same stuff the parent class would do...
        return super(MeridionalMoistDiffusion, self)._implicit_solver()


def moist_amplification_factor(Tkelvin, relative_humidity=0.8):
    '''Compute the moisture amplification factor for the moist diffusivity
    given relative humidity and reference temperature profile.'''
    deltaT = 0.01
    #  slope of saturation specific humidity at 1000 hPa
    dqsdTs = (qsat(Tkelvin+deltaT/2, 1000.) - qsat(Tkelvin-deltaT/2, 1000.)) / deltaT
    return const.Lhvap / const.cp * relative_humidity * dqsdTs
