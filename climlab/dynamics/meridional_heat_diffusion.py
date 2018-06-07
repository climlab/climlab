'''Solver for 1D meridional diffusion down the surface temperature gradient.

The grid must be evenly spaced in latitude.

Thermal diffusivity D is specified in W/m2/K
at mid-points between latitude points.

The method handles constant or spatially variable diffusivity.
'''
from __future__ import division
import numpy as np
from .meridional_diffusion import MeridionalDiffusion
from climlab import constants as const


class MeridionalHeatDiffusion(MeridionalDiffusion):
    '''A 1D diffusion solver for Energy Balance Models.

    Solves the meridional heat diffusion equation

    $$ C \frac{\partial T}{\partial t} = -\frac{1}{\cos\phi} \frac{\partial}{\partial \phi} \left[ -D \cos\phi \frac{\partial T}{\partial \phi} \right]$$

    on an evenly-spaced latitude grid, with a state variable $T$, a heat capacity $C$ and diffusivity $D$.

    Assuming $T$ is a temperature in $K$ or $^\circ$C, then the units are:

    - $D$ in W m$^{-2}$ K$^{-1}$
    - $C$ in J m$^{-2}$ K$^{-1}$

    If the state variable has other units, then $D$ and $C$ should be expressed
    per state variabe unit.

    $D$ is provided as input, and can be either scalar
    or vector defined at latitude boundaries (length).

    $C$ is normally handled automatically for temperature state variables in CLIMLAB.
    '''
    def __init__(self,
                 D=0.555,  # in W / m^2 / degC
                 use_banded_solver=True,
                 **kwargs):
        #  First just use a dummy value for K
        super(MeridionalHeatDiffusion, self).__init__(K=1.,
                        use_banded_solver=use_banded_solver, **kwargs)
        #  Now initialize properly
        self.D = D
        self.add_diagnostic('heat_transport', np.zeros_like(self.lat_bounds))
        self.add_diagnostic('heat_transport_convergence', np.zeros_like(self.lat))

    @property
    def D(self):
        return self._D
    @D.setter
    def D(self, Dvalue):
        self._D = Dvalue
        self._update_diffusivity()

    def _update_diffusivity(self):
        for varname, value in self.state.items():
            heat_capacity = value.domain.heat_capacity
        # diffusivity in units of m**2/s
        self.K = self.D / heat_capacity * const.a**2

    def _update_diagnostics(self, newstate):
        super(MeridionalHeatDiffusion, self)._update_diagnostics(newstate)
        for varname, value in self.state.items():
            heat_capacity = value.domain.heat_capacity
        self.heat_transport[:] = (self.diffusive_flux * heat_capacity *
                        2 * np.pi * const.a * self._weight1 * 1E-15) # in PW
        self.heat_transport_convergence[:] = (self.diffusive_flux_convergence *
                        heat_capacity)  # in W/m**2
