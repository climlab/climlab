r"""Solver for the 1D meridional moist static energy diffusion equation on the sphere:

.. math::

    C\frac{\partial}{\partial t} T(\phi,t) = \frac{1}{\cos\phi} \frac{\partial}{\partial \phi} \left[ \cos\phi ~ D ~(1+f(T))~ \frac{\partial T}{\partial \phi} \right]

where :math:`f(T)` is a temperature-dependent moisture amplification factor given by

.. math::

    f(T) = \frac{L^2 r q^*(T)}{c_p R_v T^2}

which expresses the effect of latent heat on the near-surface moist static energy,
where :math:`q^*(T)` is the saturation specific humidity at temperature :math:`T`
and :math:`r` is a relative humidity.

This class operates identically to ``MeridionalHeatDiffusion``
but calculates :math:`f`
automatically at each timestep and applies it to the diffusivity.

The magnitude of the moisture amplification is controlled by the input parameter
`relative_humidity` (i.e. :math:`r` in the equation above).

It can be used to implement a modified Energy Balance Model accounting for the
effects of moisture on the heat transport efficiency.


Derivation of the moist diffusion equation
------------------------------------------

Assume that heat transport is down the gradient of **moist static energy**
:math:`m = c_p T + L q + g Z`

For an EBM we want to parameterize everything in terms of a surface temperature :math:`T_s`.
So we write :math:`m_s = c_p T_s + L r q^*(T_s)`,
where :math:`m_s` is the moist static energy of near-surface air parcels,
:math:`r` is a near-surface relative humidity,
and :math:`q^*` is the **saturation specific humidity** at a reference surface pressure.

Now express this quantity in temperature units by defining a *moist temperature*

.. math::

    T_m = \frac{m_s}{c_p} = T_s + \frac{L r}{c_p} q^*(T_s)

:math:`T_m` is the temperature a dry air parcel would have
that has the same total enthalpy as a moist air parcel at temperature :math:`T_s`

The down-gradient heat transport parameterization can then be written

.. math::
    \mathcal{H} = -2 \pi a^2 D_m \frac{\partial T_m}{\partial \phi}

where :math:`D_m` is the thermal diffusion coefficient for this moist model, in units of W/m2/K.

The equation we are trying to solve is thus

.. math::

    C \frac{\partial T_s}{\partial t} = \frac{1}{\cos\phi} \frac{\partial}{\partial \phi} \left( \cos\phi D_m \frac{\partial T_m}{\partial \phi} \right)

which we can write in terms of :math:`T_s` only by substituting in for :math:`T_m`:

.. math::

    C \frac{\partial T_s}{\partial t} = \frac{1}{\cos\phi} \frac{\partial}{\partial \phi} \left( \cos\phi D_m \left(\frac{\partial T_s}{\partial \phi} + \frac{\partial}{\partial \phi} \left(\frac{L r}{c_p} q^*(T_s)\right)\right)\right)

If we make the simplifying assumption that the **relative humidity :math:`r` is constant**
(not a function of latitude), then

.. math::

    C \frac{\partial T_s}{\partial t} = \frac{1}{\cos\phi} \frac{\partial}{\partial \phi} \left( \cos\phi D_m \left(\frac{\partial T_s}{\partial \phi} + \frac{L r}{c_p} \frac{\partial q^*}{\partial \phi} \right)\right)

To a good approximation (see Hartmann's book and others),
the Clausius-Clapeyron relation for saturation specific humidity gives

.. math::

    \frac{\partial q^*}{dT} = \frac{L}{R_v T^2} q^*(T)

Then using a chain rule we have

.. math::

    \frac{\partial q^*}{\partial \phi} = \frac{\partial q^*}{\partial T_s} \frac{\partial T_s}{\partial \phi}  = \frac{L q^*(T_s)}{R_v T_s^2}  \frac{\partial T_s}{\partial \phi}

Plugging this into our model equation we get

.. math::

    C \frac{\partial T_s}{\partial t} = \frac{1}{\cos\phi} \frac{\partial}{\partial \phi} \left( \cos\phi D_m \frac{\partial T_s}{\partial \phi} \left(1 + \frac{L^2 r q^*(T_s)}{c_p R_v T_s^2} \right)\right)

This is now in a form that is compatible with our diffusion solver.

Just let

.. math::

    D = D_m \left( 1 + f(T_s) \right)

where

.. math::

    f(T_s) = \frac{L^2 r q^*(T_s)}{c_p R_v T_s^2}

or, equivalently,

.. math::

    f(T_s) = \frac{L r }{c_p} \frac{\partial q^*}{dT}\bigg|_{T_s}

Given a temperature distribution :math:`T_s(\phi)` at any given time,
we can calculate the diffusion coefficient :math:`D(\phi)` from this formula.

This calculation is implemented in the ``MeridionalMoistDiffusion`` class.
"""
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
