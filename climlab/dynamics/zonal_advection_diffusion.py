from __future__ import division
import numpy as np
from .advection_diffusion import AdvectionDiffusion, Diffusion
from climlab import constants as const


class ZonalAdvectionDiffusion(AdvectionDiffusion):
    """A parent class for meridional advection-diffusion processes.
    """
    def __init__(self,
                 K=0.,
                 U=0.,
                 use_banded_solver=False,
                 prescribed_flux=0.,
                 **kwargs):
        super(ZonalAdvectionDiffusion, self).__init__(K=K, U=U,
                diffusion_axis='lon', use_banded_solver=use_banded_solver, 
                is_periodic=True, **kwargs)
        # Conversion of delta from degrees (grid units) to physical length units
        lambd_stag = np.deg2rad(self.lon_bounds)
        lambd = np.deg2rad(self.lon)
        self._Xcenter[...,:] = lambd*const.a
        self._Xbounds[...,:] = lambd_stag*const.a
        self._weight_bounds[...,:] = 1.0
        self._weight_center[...,:] = 1.0
        #  Now properly compute the weighted advection-diffusion matrix
        self.prescribed_flux = prescribed_flux
        self.K = K
        self.U = U

class ZonalDiffusion(ZonalAdvectionDiffusion):
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
        super(ZonalDiffusion, self).__init__(
            U=0.,
            K=K,
            prescribed_flux=prescribed_flux,
            use_banded_solver=use_banded_solver, **kwargs)

