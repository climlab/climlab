from __future__ import division
import numpy as np
from .zonal_advection_diffusion import ZonalDiffusion
from climlab import constants as const

class ZonalHeatDiffusion(ZonalDiffusion):
    '''A 1D diffusion solver for Energy Balance Models.

    Solves the meridional heat diffusion equation

    .. math::

        C \frac{\partial T}{\partial t} = -\frac{1}{\cos\phi} \frac{\partial}{\partial \phi} \left[ -D \cos\phi \frac{\partial T}{\partial \phi} \right]

    on an evenly-spaced latitude grid, with a state variable :math:`T`,
    a heat capacity :math:`C` and diffusivity :math:`D`.

    Assuming :math:`T` is a temperature in K or degC, then the units are:

    - :math:`D` in W m-2 K-1
    - :math:`C` in J m-2 K-1

    :math:`D` is provided as input, and can be either scalar
    or vector defined at latitude boundaries.

    :math:`C` is normally handled automatically for temperature state variables in CLIMLAB.
    '''
    def __init__(self,
                 D=0.555,  # in W / m^2 / degC
                 use_banded_solver=False,
                 **kwargs):
        #  First just use a dummy value for K
        super(ZonalHeatDiffusion, self).__init__(K=1.,
                        use_banded_solver=use_banded_solver, **kwargs)
        #  Now initialize properly
        self.D = D
        self.add_diagnostic('heat_transport_zonal', 0.*self.diffusive_flux_zonal)
        self.add_diagnostic('heat_transport_convergence_zonal', 0.*self.flux_convergence_zonal)

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
        super(ZonalHeatDiffusion, self)._update_diagnostics(newstate)
        for varname, value in self.state.items():
            heat_capacity = value.domain.heat_capacity
        coslat_bounds = np.moveaxis(self._weight_bounds,-1,self.diffusion_axis_index)
        if (not np.isscalar(heat_capacity)) and (len(heat_capacity.shape) > 2):
            # downsample along necessary axis
            #heat_capacity = (heat_capacity[:,1:,:] + heat_capacity[:,:-1,:]) / 2
            heat_capacity = (heat_capacity[1:,:,:] + heat_capacity[:-1,:,:]) / 2
        self.heat_transport_zonal[:] = (self.diffusive_flux_zonal * heat_capacity *
            2 * np.pi * const.a * coslat_bounds * 1E-15) # in PW
        if (not np.isscalar(heat_capacity)) and (len(heat_capacity.shape) > 2):
            # downsample along the other axis
            heat_capacity = (heat_capacity[:,1:,:] + heat_capacity[:,:-1,:]) / 2
            #heat_capacity = (heat_capacity[1:,:,:] + heat_capacity[:-1,:,:]) / 2
        self.heat_transport_convergence_zonal[:] = (self.flux_convergence_zonal *
                        heat_capacity)  # in W/m**2
