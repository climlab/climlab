r"""
climlab process for large-scale condensation

The process object ``climlab.dynamics.LargeScaleCondensation`` does the following at each timestep:

- Calculate saturation specific humidity given air temperatures at every grid point
- Calculate supersaturation by comparing actual specific humidity to saturation specific humidity
- Compute a specific humidity tendency based on a relaxation toward saturation (if supersaturated)
- Compute a heating rate and temperature tendency due to the latent heating of condensation
- Compute precipitation rate at the surface, assuming all condensate in each column is instantly precipitated

State variables:

- ``Tatm``: air temperature in K
- ``q``: specific humidity in kg kg\ :sup:`-1`

Input parameters and default values:

- ``condensation_time``: condensation time constant in units of seconds (default: 4 hours)
- ``RH_ref``: reference relative humidity value, dimensionless (default value 0.9)

Diagnostics:

- ``latent_heating``: latent heating rate (every grid cell) in units of W m\ :sup:`-2`
- ``precipitation``: precipitation rate (column total) in units of kg m\ :sup:`-2` s\ :sup:`-1` or mm s\ :sup:`-1`

The condensation rule follows the SPEEDY model (Molteni 2003 doi:10.1007/s00382-002-0268-2). 
Condensation is modeled as a relaxation of relative humidity toward a 
specified profile wherever the tropospheric relative humidity exceeds the target.

Given specific humidity :math:`q` and saturation specific humidity :math:`q_{sat}(T,p)`, 
relative humidity is calculated from

.. math::

    r = \frac{q}{q_{sat}}

which is compared against a specified reference profile :math:`r_{lsc}` which may vary spatially.

At grid cells where :math:`r > r_{lsc}`, the specific humidity tendency is calculated from

.. math::

    \left(\frac{\partial q}{\partial t}\right)_{lsc} = -\frac{(q - r_{lsc} q_{sat})}{\tau_{lsc}}

and is zero otherwise.

The two parameters of the scheme are the relaxation time constant :math:`\tau_{lsc}` and the reference RH profile :math:`r_{lsc}`.

We follow SPEEDY and set an "aggressive" default time constant :math:`\tau_{lsc} = 4` hours.

For the reference profile, SPEEDY sets a smoothly decreasing vertical profile with :math:`r_{lsc} = 0.9` at the surface 
and :math:`r_{lsc} \approx 0.8` at the tropopause. 
For simplicity, we will default to a uniform default value of :math:`r_{lsc} = 0.9`.

The temperature tendency due to latent heating (in units of K s\ :sup:`-1`) is calculated from

.. math::

    \left(\frac{\partial T}{\partial t}\right)_{lsc} = -\frac{L}{c_p} \left(\frac{\partial q}{\partial t}\right)_{lsc}

with the associated heating rate diagnostic (in units of W m\ :sup:`-2`) computed from

.. math::

    h_{lsc} = C \left(\frac{\partial T}{\partial t}\right)_{lsc}

where :math:`C = \frac{c_p dp}{g}` is the heat capacity per unit area in J K\ :sup:`-1` m\ :sup:`-2`,
and the precipitation rate is calculated from the vertical integral:

.. math::

    P = -\frac{1}{g} \int_0^{p_0} \left(\frac{\partial q}{\partial t}\right)_{lsc} dp

or equivalently

.. math::

    P = + \int_0^{p_0} \frac{h_{lsc}}{L}

where the integral implies a sum over all grid cells in each atmospheric column.
"""
import numpy as np
from climlab.process import TimeDependentProcess
from climlab.utils import constants as const
from climlab.utils.thermo import qsat


class LargeScaleCondensation(TimeDependentProcess):
    '''Climlab process class for LargeScaleCondensation. 
    Condensation is modeled as a relaxation of relative humidity toward a 
    specified reference value wherever the tropospheric relative humidity 
    exceeds the target.
    
    State variables:

    - ``Tatm``: air temperature in K
    - ``q``: specific humidity in kg kg\ :sup:`-1`

    Input parameters and default values:

    - ``condensation_time``: condensation time constant in units of seconds (default: 4 hours)
    - ``RH_ref``: reference relative humidity value, dimensionless (default value 0.9)

    Diagnostics:

    - ``latent_heating``: latent heating rate (every grid cell) in units of W m\ :sup:`-2`
    - ``precipitation``: precipitation rate (column total) in units of kg m\ :sup:`-2` s\ :sup:`-1` or mm s\ :sup:`-1`
    '''
    def __init__(self, 
                 condensation_time = 4. * const.seconds_per_hour, 
                 RH_ref = 0.9,
                 **kwargs):
        super(LargeScaleCondensation, self).__init__(**kwargs)
        self.condensation_time = condensation_time
        self.RH_ref = RH_ref
        self.add_diagnostic('latent_heating', 0.*self.Tatm)
        self.add_diagnostic('precipitation', 0.*self.Ts)
        
    def _compute(self):
        qsaturation = qsat(self.Tatm, self.lev) 
        qtendency = -(self.q - self.RH_ref*qsaturation) / self.condensation_time

        tendencies = {}
        tendencies['q'] = np.minimum(qtendency, 0.)
        tendencies['Tatm'] = -const.Lhvap/const.cp * tendencies['q']
        self.latent_heating[:] = tendencies['Tatm'] * self.Tatm.domain.heat_capacity
        self.precipitation[:,0] = np.sum(self.latent_heating, axis=-1)/const.Lhvap
        return tendencies