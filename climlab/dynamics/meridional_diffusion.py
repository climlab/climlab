r"""General solver of the 1D meridional diffusion equation on the sphere:

.. math::

    \frac{\partial}{\partial t} \Psi(\phi,t) &= -\frac{1}{a \cos\phi} \frac{\partial}{\partial \phi} \left[ \cos\phi ~ F(\phi,t) \right] \\
    F &= -\frac{K}{a} ~ \frac{\partial \Psi}{\partial \phi}

for a state variable :math:`\Psi(\phi,t)` and arbitrary diffusivity :math:`K(\phi,t)`
in units of :math:`x^2 ~ t^{-1}`. :math:`\phi` is latitude and
:math:`a` is the Earth's radius (in meters).

The diffusivity :math:`K` can be a single scalar,
or optionally a vector *specified at grid cell boundaries*
(so its length must be exactly 1 greater than the length of :math:`\phi`).

:math:`K` can be modified by the user at any time
(e.g., after each timestep, if it depends on other state variables).

A fully implicit timestep is used for computational efficiency. Thus the computed
tendency :math:`\frac{\partial \Psi}{\partial t}` will depend on the timestep.

In addition to the tendency over the implicit timestep,
the solver also calculates two diagnostics from the updated state:

- ``diffusive_flux`` given by :math:`F(\phi)` in units of :math:`[\Psi]` m/s
- ``diffusive_flux_convergence`` given by :math:`-\frac{1}{a \cos\phi} \frac{\partial}{\partial \phi} \left[ \cos\phi ~ F(\phi,t) \right]` in units of :math:`[\Psi]`/s

The grid must be *evenly spaced in latitude*.

The state variable :math:`\Psi` may be multi-dimensional, but the diffusion
will operate along the latitude dimension only.
"""
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
