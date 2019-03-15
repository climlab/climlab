r"""Solver for the 1D meridional heat diffusion equation on the sphere:

.. math::

    C\frac{\partial}{\partial t} T(\phi,t) = \frac{1}{\cos\phi} \frac{\partial}{\partial \phi} \left[ \cos\phi ~ D ~ \frac{\partial T}{\partial \phi} \right]

for a temperature state variable :math:`T(\phi,t)`,
a vertically-integrated heat capacity :math:`C`,
and arbitrary thermal diffusivity :math:`D(\phi,t)`
in units of W/m2/K.

The diffusivity :math:`D` can be a single scalar,
or optionally a vector *specified at grid cell boundaries*
(so its length must be exactly 1 greater than the length of :math:`\phi`).

:math:`D` can be modified by the user at any time
(e.g., after each timestep, if it depends on other state variables).

The heat capacity :math:`C` is normally handled automatically by CLIMLAB
as part of the grid specification.

A fully implicit timestep is used for computational efficiency. Thus the computed
tendency :math:`\frac{\partial T}{\partial t}` will depend on the timestep.

The diagnostics ``diffusive_flux`` and ``diffusive_flux_convergence`` are computed
as described in the parent class ``MeridionalDiffusion``.
Two additional diagnostics are computed here,
which are meaningful if :math:`T` represents a *zonally averaged temperature*:

- ``heat_transport`` given by :math:`\mathcal{H}(\phi) = -2 \pi ~ a^2 ~ \cos\phi ~ D ~ \frac{\partial T}{\partial \phi}` in units of PW (petawatts).
- ``heat_transport_convergence`` given by :math:`-\frac{1}{2 \pi ~a^2 \cos\phi} \frac{\partial \mathcal{H}}{\partial \phi}` in units of W/m2

The grid must be *evenly spaced in latitude*.

The state variable :math:`T` may be multi-dimensional, but the diffusion
will operate along the latitude dimension only.
"""
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
