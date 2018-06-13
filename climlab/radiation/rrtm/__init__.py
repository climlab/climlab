''' climlab wrapper for RRTMG radiation schemes.

This is implemented with classes :class:`~climlab.radiation.rrtm.RRTMG_LW`, and
:class:`~climlab.radiation.rrtm.RRTMG_SW`,
as well as a container class :class:`~climlab.radiation.rrtm.RRTMG`
that has LW and SW radiation as subprocesses.

Input arguments and diagnostics follow specifications in
:class:`~climlab.radiation._Radiation`

See <http://rtweb.aer.com/rrtm_frame.html> for more information about the RRTMG code.

    :Example:

        Here is a quick example of setting up a single-column
        Radiative-Convective model with fixed relative humidity::

            import climlab
            alb = 0.25
            #  State variables (Air and surface temperature)
            state = climlab.column_state(num_lev=30)
            #  Fixed relative humidity
            h2o = climlab.radiation.ManabeWaterVapor(name='WaterVapor', state=state)
            #  Couple water vapor to radiation
            rad = climlab.radiation.RRTMG(name='Radiation', state=state, specific_humidity=h2o.q, albedo=alb)
            #  Convective adjustment
            conv = climlab.convection.ConvectiveAdjustment(name='Convection', state=state, adj_lapse_rate=6.5)
            #  Couple everything together
            rcm = climlab.couple([rad,h2o,conv], name='Radiative-Convective Model')
            #  Run the model
            rcm.integrate_years(1)
            #  Check for energy balance
            print(rcm.ASR - rcm.OLR)

'''
from __future__ import division, absolute_import
from .rrtmg import RRTMG
from .rrtmg_lw import RRTMG_LW
from .rrtmg_sw import RRTMG_SW
from .utils import _climlab_to_rrtm, _rrtm_to_climlab
