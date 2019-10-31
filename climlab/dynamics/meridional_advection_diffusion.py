r"""General solver of the 1D meridional advection-diffusion equation on the sphere:

.. math::

    \frac{\partial}{\partial t} \psi(\phi,t) &= -\frac{1}{a \cos\phi} \frac{\partial}{\partial \phi} \left[ \cos\phi ~ F(\phi,t) \right] \\
    F &= U(\phi) \psi(\phi) -\frac{K(\phi)}{a} ~ \frac{\partial \psi}{\partial \phi}

for a state variable :math:`\psi(\phi,t)`, arbitrary diffusivity :math:`K(\phi)`
in units of :math:`x^2 ~ t^{-1}`, and advecting velocity :math:`U(\phi)`.
:math:`\phi` is latitude and :math:`a` is the Earth's radius (in meters).

:math:`K` and :math:`U` can be scalars,
or optionally vector *specified at grid cell boundaries*
(so their lengths must be exactly 1 greater than the length of :math:`\phi`).

:math:`K` and :math:`U` can be modified by the user at any time
(e.g., after each timestep, if they depend on other state variables).

A fully implicit timestep is used for computational efficiency. Thus the computed
tendency :math:`\frac{\partial \psi}{\partial t}` will depend on the timestep.

In addition to the tendency over the implicit timestep,
the solver also calculates several diagnostics from the updated state:

- ``diffusive_flux`` given by :math:`-\frac{K(\phi)}{a} ~ \frac{\partial \psi}{\partial \phi}` in units of :math:`[\psi]~[x]`/s
- ``advective_flux`` given by :math:`U(\phi) \psi(\phi)` (same units)
- ``total_flux``, the sum of advective, diffusive and prescribed fluxes
- ``flux_convergence`` (or instantanous scalar tendency) given by the right hand side of the first equation above, in units of :math:`[\psi]`/s

Non-uniform grid spacing is supported.

The state variable :math:`\psi` may be multi-dimensional, but the diffusion
will operate along the latitude dimension only.
"""
from __future__ import division
import numpy as np
from .advection_diffusion import AdvectionDiffusion, Diffusion
from climlab import constants as const


class MeridionalAdvectionDiffusion(AdvectionDiffusion):
    """A parent class for meridional advection-diffusion processes.
    """
    def __init__(self,
                 K=0.,
                 U=0.,
                 use_banded_solver=False,
                 prescribed_flux=0.,
                 **kwargs):
        super(MeridionalAdvectionDiffusion, self).__init__(K=K, U=U,
                diffusion_axis='lat', use_banded_solver=use_banded_solver, **kwargs)
        # Conversion of delta from degrees (grid units) to physical length units
        phi_stag = np.deg2rad(self.lat_bounds)
        phi = np.deg2rad(self.lat)
        self._Xcenter[...,:] = phi*const.a
        self._Xbounds[...,:] = phi_stag*const.a
        self._weight_bounds[...,:] = np.cos(phi_stag)
        self._weight_center[...,:] = np.cos(phi)
        #  Now properly compute the weighted advection-diffusion matrix
        self.prescribed_flux = prescribed_flux
        self.K = K
        self.U = U


class MeridionalDiffusion(MeridionalAdvectionDiffusion):
    """A parent class for meridional diffusion-only processes,
    with advection set to zero.

    Otherwise identical to the parent class.
    """
    def __init__(self,
                 K=0.,
                 use_banded_solver=False,
                 prescribed_flux=0.,
                 **kwargs):
        # Just initialize the AdvectionDiffusion class with U=0
        super(MeridionalDiffusion, self).__init__(
            U=0.,
            K=K,
            prescribed_flux=prescribed_flux,
            use_banded_solver=use_banded_solver, **kwargs)
