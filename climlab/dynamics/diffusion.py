'''Module for one-dimensional diffusion operators.

Here is an example showing implementation of a vertical diffusion.
Example shows that a subprocess can work on just a subset of the parent process
state variables.

import numpy as np
from climlab import column, diffusion
c = column.SingleColumnModel()
p = c.grid['lev'].bounds
K = 0.005 * np.ones_like(p)
dstate = {'Tatm': c.state['Tatm']}
d = diffusion.Diffusion(K=K, grid=c.grid, state=dstate, param=c.param)
c.subprocess = {'radiation': d}
print c.state
print d.state
c.step_forward()
print c.state
print d.state
'''
import numpy as np
from scipy.linalg import solve_banded
from climlab.process.implicit import ImplicitProcess


## NEED TO UPDATE THIS TO USE NEW DOMAINS

class Diffusion(ImplicitProcess):
    '''Parent class for implicit diffusion modules.'''
    def __init__(self,
                 K=None,
                 diffusion_axis=None,
                 **kwargs):
        super(Diffusion, self).__init__(**kwargs)
        self.K = K  # Diffusivity in units of [length]**2 / time
        # if diffusion axis is not specified and there is only one axis...
        if diffusion_axis is None and len(self.grid.keys()) is 1:
            self.diffusion_axis = self.grid.keys()[0]
        else:
            self.diffusion_axis = diffusion_axis
        # This currently only works with evenly space points
        delta = np.mean(self.grid[self.diffusion_axis].delta)
        self.K_dimensionless = self.K * self.param['timestep'] / delta
        self.diffTriDiag = _make_diffusion_matrix(self.K_dimensionless)

    def _implicit_solver(self):
        # Time-stepping the diffusion is just inverting this matrix problem:
        # self.T = np.linalg.solve( self.diffTriDiag, Trad )
        newstate = {}
        for varname, value in self.state.iteritems():
            newvar = solve_banded((1, 1), self.diffTriDiag, value)
            newstate[varname] = newvar
        return newstate


class MeridionalDiffusion(Diffusion):
    '''Meridional diffusion process.'''
    def __init__(self,
                 K=None,
                 **kwargs):
        super(MeridionalDiffusion, self).__init__(K=K, diffusion_axis='lat', **kwargs)
        self.diffTriDiag = _make_meridional_diffusion_matrix(self.K_dimensionless, self.grid)


def _make_diffusion_matrix(K, weight1=None, weight2=None):
    '''K is array of dimensionless diffusivities at cell boundaries:
        physical K (length**2 / time) / (delta length)**2 * (delta time)
    '''
    J = K.size - 1
    if weight1 is None:
        weight1 = np.ones_like(K)
    if weight2 is None:
        weight2 = np.ones(J)
    weightedK = weight1 * K
    Ka1 = weightedK[0:J] / weight2
    Ka3 = weightedK[1:J+1] / weight2
    Ka2 = np.insert(Ka1[1:J], 0, 0) + np.append(Ka3[0:J-1], 0)
    #  Atmosphere tridiagonal matrix
    diag = np.empty((3, J))
    diag[0, 1:] = -Ka3[0:J-1]
    diag[1, :] = 1 + Ka2
    diag[2, 0:J-1] = -Ka1[1:J]
    return diag


def _make_meridional_diffusion_matrix(K, grid):
    lataxis = grid['lat']
    phi_stag = np.deg2rad(lataxis.bounds)
    phi = np.deg2rad(lataxis.points)
    weight1 = np.cos(phi_stag)
    weight2 = np.cos(phi)
    diag = _make_diffusion_matrix(K, weight1, weight2)
    return diag
