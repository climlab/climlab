'''
Prototype for wrapping CliMT radiation modules into climlab processes.

Usage:

import climlab
# initialize a climlab column model with temperature and water vapor
#  We set num_lev=26 to match the default climt dimensions.
col = climlab.BandRCModel(num_lev=26)
col.step_forward()
col.Tatm
#  climt must be pre-compiled and installed with correct grid dimensions.
#  otherwise this won't work.
from climlab.radiation.climtrad import CliMTRad
c2 = CliMTRad(state=col.state)
c2.step_forward()

This seems to be working but so far I have only implemented the longwave
atmospheric part. Need to couple to surface, and make shortwave functional.

Also need to test out RRTM scheme.
'''

import numpy as np
from climlab.process.energy_budget import EnergyBudget
from climlab import constants as const

try:
    import climt
except:
    raise ImportError('Cannot import climt package.')

class CliMTRad(EnergyBudget):
    '''A wrapper for radiation modules in Rodrigo Caballero's CliMT package.

    See documentation in CliMT/lib/climt/radiation.py.

    At the moment this requires that CliMT is pre-compiled
    with the correct grid dimensions.'''
    def __init__(self, scheme='cam3', **kwargs):
        super(CliMTRad, self).__init__(**kwargs)
        self.r = climt.radiation(scheme=scheme)
        self.param['climt_scheme'] = scheme

    def _temperature_tendencies(self):
        #  This will just set all tendencies to zero
        super(CliMTRad, self)._temperature_tendencies()
        #  Call the climt object
        self.r(p=np.flipud(self.lev), ps=1000., T=np.flipud(self.Tatm),
               Ts=self.Ts, q=np.flipud(self.q), cldf=np.zeros_like(self.Tatm),
               clwp=np.zeros_like(self.Tatm))
        # Now set the temperature tendency in K / s
        #   Need to convert from K / day in CliMT
        self.tendencies['Tatm'] = (np.flipud(np.squeeze(self.r.lwhr)) /
                                   const.seconds_per_day)
        #  so far this is just LW
        #  The module also computes SW but we need to set insolation and zenith
