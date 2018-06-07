'''General solver for 1D meridional diffusion on the sphere.

The grid must be evenly spaced in latitude.

Diffusivity K is specified in m**2/s
at mid-points between latitude points.
The method handles constant or spatially variable diffusivity.
'''

from __future__ import division
import numpy as np
from .diffusion import Diffusion
from climlab import constants as const


class MeridionalDiffusion(Diffusion):
    """A parent class for Meridional diffusion processes.

    Calculates the energy transport in a diffusion like process along the
    temperature gradient:

        .. math::

            H(\\varphi) = \\frac{D}{\\cos \\varphi}\\frac{\\partial}{\\partial \\varphi} \\left( \\cos\\varphi \\frac{\\partial T(\\varphi)}{\\partial \\varphi} \\right)


     for an Energy Balance Model whose Energy Budget can be noted as:

     .. math::

        C(\\varphi) \\frac{dT(\\varphi)}{dt} = R\\downarrow (\\varphi) - R\\uparrow (\\varphi) + H(\\varphi)



    **Initialization parameters** \n

    An instance of ``MeridionalDiffusion`` is initialized with the following
    arguments:

    :param float K:     diffusion parameter in units of :math:`m^2/s`

    **Object attributes** \n

    Additional to the parent class :class:`~climlab.dynamics.diffusion.Diffusion`
    which is initialized with ``diffusion_axis='lat'``, following object
    attributes are modified during initialization:

    :ivar array _K_dimensionless:    As _K_dimensionless has been computed like
                                    :math:`K_{\\textrm{dimensionless}}= K \\frac{\\Delta t}{(\\Delta \\textrm{bounds})^2}`
                                    with :math:`K` in units :math:`1/s`,
                                    the :math:`\\Delta (\\textrm{bounds})` have to
                                    be converted from ``deg`` to ``rad`` to make
                                    the array actually dimensionless.
                                    This is done during initialiation.
    :ivar array _diffTriDiag:        the diffusion matrix is recomputed with
                                    appropriate weights for the meridional case
                                    by :func:`_make_meridional_diffusion_matrix`

    :Example:

        Meridional Diffusion of temperature
        as a stand-alone process:

        .. plot:: code_input_manual/example_meridional_diffusion.py
           :include-source:

    """
    def __init__(self,
                 K=None,
                 **kwargs):
        super(MeridionalDiffusion, self).__init__(K=K,
                                                diffusion_axis='lat', **kwargs)
        # Conversion of delta from degrees (grid units) to physical length units
        self.delta *= (np.deg2rad(1.) * const.a)
        for dom in list(self.domains.values()):
            lataxis = dom.axes['lat']
        phi_stag = np.deg2rad(lataxis.bounds)
        phi = np.deg2rad(lataxis.points)
        self._weight1 = np.cos(phi_stag)
        self._weight2 = np.cos(phi)
        #  Now properly compute the weighted diffusion matrix
        self.K = K
