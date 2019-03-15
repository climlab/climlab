'''
Modules for simple dynamics, mostly for use in Energy Balance Models.

:class:`~climlab.dynamics.BudykoTransport` is a relaxation to global mean.

Other modules are 1D diffusion solvers (implemented using implicit timestepping).

:class:`~climlab.convection.MeridionalHeatDiffusion` is the appropriate class
for the traditional diffusive EBM, in which transport is parameterized as a
meridional diffusion process down the zonal-mean surface temperature gradient.

:class:`~climlab.convection.MeridionalMoistDiffusion` implements the moist EBM,
with transport down an approximate gradient in near-surface moist static energy.
'''

from __future__ import absolute_import
from .budyko_transport import BudykoTransport
from .diffusion import Diffusion
from .meridional_diffusion import MeridionalDiffusion
from .meridional_heat_diffusion import MeridionalHeatDiffusion
from .meridional_moist_diffusion import MeridionalMoistDiffusion
