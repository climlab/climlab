'''
Modules for simple dynamics, mostly for use in Energy Balance Models.

:class:`~climlab.dynamics.BudykoTransport` is a relaxation to global mean.

Other modules are 1D advection-diffusion solvers (implemented using implicit timestepping).

:class:`~climlab.dynamics.AdvectionDiffusion` is a general-purpose 1D
advection-diffusion process. It can be used out-of-the-box for models with
Cartesian grid geometry, but also accepts weighting functions for the
divergence operator on curvilinear grids.

:class:`~climlab.dynamics.MeridionalAdvectionDiffusion` implements the
1D advection-diffusion process on the sphere (flux in the north-south direction).

Subclass :class:`~climlab.dynamics.MeridionalHeatDiffusion` is the appropriate class
for the traditional diffusive EBM, in which transport is parameterized as a
meridional diffusion process down the zonal-mean surface temperature gradient.

:class:`~climlab.dynamics.MeridionalMoistDiffusion` implements the moist EBM,
with transport down an approximate gradient in near-surface moist static energy.
'''

from __future__ import absolute_import
from .budyko_transport import BudykoTransport
from .advection_diffusion import AdvectionDiffusion, Diffusion
from .meridional_advection_diffusion import MeridionalAdvectionDiffusion, MeridionalDiffusion
from .meridional_heat_diffusion import MeridionalHeatDiffusion
from .meridional_moist_diffusion import MeridionalMoistDiffusion
