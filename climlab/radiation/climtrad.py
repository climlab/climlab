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
state = col.state
state.update({'q': col.q})
c2 = CliMTRad(state=state)
c2.flux_from_sfc = climlab.constants.sigma * c2.Ts**4
c2.compute_diagnostics()

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
        newinput = ['flux_from_sfc',
                    'cldf',
                    'clwp',]
        self.add_input(newinput)
        #  cloud input
        self.cldf = 0. * self.Tatm
        self.clwp = 0. * self.Tatm
        #  upwelling surface radiation... should be set by parent process
        self.flux_from_sfc = 0. * self.Ts
        newdiags = ['flux_to_sfc',
                    'flux_to_space',]
        self.add_diagnostics(newdiags)


    def _temperature_tendencies(self):
        #  This will just set all tendencies to zero
        tendencies = {}
        for varname, value in self.state.iteritems():
            tendencies[varname] = 0. * value
        #  Call the climt object
        #  (vertical axis is reversed, and needs specific humidity in g/kg)
        self.r(p=np.flipud(self.lev), ps=1000., T=np.flipud(self.Tatm),
               Ts=self.Ts, q=np.flipud(self.q)*1000., flus=self.flux_from_sfc,
               cldf=self.cldf, clwp=self.clwp)
        # some DIAGNOSTICS (translating from the CliMT conventions)
        self.flux_to_space = -self.r.LwToa
        self.flux_to_sfc = self.flux_from_sfc + self.r.LwSrf
        # Now set the temperature tendency in K / s
        #   Need to convert from K / day in CliMT
        tendencies['Tatm'] = (np.flipud(np.squeeze(self.r.lwhr)) /
                                   const.seconds_per_day)
        #  so far this is just LW
        #  The module also computes SW but we need to set insolation and zenith
        return tendencies
