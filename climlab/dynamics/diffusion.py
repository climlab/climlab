'''Module for one-dimensional diffusion operators.

Here is an example showing implementation of a vertical diffusion.
Example shows that a subprocess can work on just a subset of the parent process
state variables.

import climlab
from climlab.dynamics.diffusion import Diffusion
c = climlab.GreyRadiationModel()
K = 0.5
d = Diffusion(K=K, state=c.state['Tatm'], **c.param)
c.subprocess['diffusion'] = d
print c.state
print d.state
c.step_forward()
print c.state
print d.state

And here is an example of meridional diffusion of temperature
as a stand-alone process:

REWORKED FOR THE NEW XRAY INTERFACE

import numpy as np
import xray, climlab
initial = climlab.domain.grid.zonal_mean_surface()
Tarray = (12. - 40. * climlab.utils.legendre.P2(np.sin(np.deg2rad(initial.lat.values)))).reshape((initial.lat.size, 1))
initial['Ts'] = (('lat', 'depth'), Tarray)
# thermal diffusivity in W/m**2/degC
D = 0.55
# meridional diffusivity in 1/s
K = (D / climlab.domain.grid.heat_capacity(initial))['Ts'].values
d = climlab.dynamics.diffusion.MeridionalDiffusion(state=initial.copy(deep=True), K=K)
tend = d.compute()
d.step_forward()
d.integrate_years(1.)
import matplotlib.pyplot as plt
initial.Ts.plot()
d.Ts.plot()
plt.show()

This works but seems very slow.
'''


import numpy as np
from scipy.linalg import solve_banded
from climlab.process.implicit import ImplicitProcess
from climlab.process.process import get_axes


class Diffusion(ImplicitProcess):
    '''Parent class for implicit diffusion modules.
    Solves the 1D heat equation
    dA/dt = d/dy( K * dA/dy )

    The diffusivity K should be given in units of [length]**2 / time
    where length is the unit of the spatial axis
    on which the diffusion is occuring.

    Input flag use_banded_solver sets whether to use
    scipy.linalg.solve_banded
    rather than the default
    numpy.linalg.solve

    banded solver is faster but only works for 1D diffusion.'''
    def __init__(self,
                 K=None,
                 diffusion_axis=None,
                 use_banded_solver=False,
                 **kwargs):
        super(Diffusion, self).__init__(**kwargs)
        self.K = K  # Diffusivity in units of [length]**2 / time
        self.use_banded_solver = use_banded_solver
        if diffusion_axis is None:
            self.diffusion_axis = _guess_diffusion_axis(self)
        else:
            self.diffusion_axis = diffusion_axis
        # This currently only works with evenly spaced points
        #delta = self.lat_delta.mean(dim='lat').values
        #bounds = self.lat_bounds
        #K_dimensionless = (self.K * np.ones_like(bounds.values) *
        #                        self.timestep / delta**2)
        #K_dimensionless = self.lat_bounds * 0.
        #K_dimensionless.values += (self.param['K'] * self.timestep / delta**2)
        #  just sticking with plain numpy implemention here
        #self._K_dimensionless = K_dimensionless
        #self.diffTriDiag = _make_diffusion_matrix(self._K_dimensionless.values)

    @property
    def K(self):
        return self.param['K']
    @K.setter
    def K(self, value):
        self.param['K'] = value
        K_dimensionless = self.lat_bounds * 0.
        # This currently only works with evenly spaced points
        delta = np.mean(self.lat_delta.values)
        K_dimensionless.values += (self.param['K'] * self.timestep / delta**2)
        #  just sticking with plain numpy implemention here
        self._K_dimensionless = K_dimensionless
        self.diffTriDiag = _make_diffusion_matrix(self._K_dimensionless.values)

    def _implicit_solver(self):
        # Time-stepping the diffusion is just inverting this matrix problem:
        # self.T = np.linalg.solve( self.diffTriDiag, Trad )
        #newstate = self.state * 0.
        newstate = {}
        for varname, var in self.state.iteritems():
            if self.use_banded_solver:
                newvar = _solve_implicit_banded(var.values, self.diffTriDiag)
            else:
                newvar = np.linalg.solve(self.diffTriDiag, var.values)
            newstate[varname] = newvar
        return newstate


def _solve_implicit_banded(current, banded_matrix):
    #  can improve performance by storing the banded form once and not
    #  recalculating it...
    #  but whatever
    J = banded_matrix.shape[0]
    diag = np.zeros((3, J))
    diag[1, :] = np.diag(banded_matrix, k=0)
    diag[0, 1:] = np.diag(banded_matrix, k=1)
    diag[2, :-1] = np.diag(banded_matrix, k=-1)
    return solve_banded((1, 1), diag, current)


class MeridionalDiffusion(Diffusion):
    '''Meridional diffusion process.
    K in units of 1 / s.
    '''
    def __init__(self,
                 K=None,
                 **kwargs):
        super(MeridionalDiffusion, self).__init__(K=K,
                                                diffusion_axis='lat', **kwargs)
        self._K_dimensionless.values *= 1./np.deg2rad(1.)**2
        #for dom in self.domains.values():
        #    latax = dom.axes['lat']
        self.diffTriDiag = (
            _make_meridional_diffusion_matrix(self._K_dimensionless.values,
               self.lat.values, self.lat_bounds.values))


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
    #  this code makes a 3xN matrix, suitable for use with solve_banded
    #diag = np.empty((3, J))
    #diag[0, 1:] = -Ka3[0:J-1]
    #diag[1, :] = 1 + Ka2
    #diag[2, 0:J-1] = -Ka1[1:J]
    #  Build the full banded matrix instead
    A = (np.diag(1 + Ka2, k=0) +
         np.diag(-Ka3[0:J-1], k=1) +
         np.diag(-Ka1[1:J], k=-1))
    return A


def _make_meridional_diffusion_matrix(K, latpoints, latbounds):
    phi_stag = np.deg2rad(latbounds)
    phi = np.deg2rad(latpoints)
    weight1 = np.cos(phi_stag)
    weight2 = np.cos(phi)
    diag = _make_diffusion_matrix(K, weight1, weight2)
    return diag


def _guess_diffusion_axis(process_or_domain):
    '''Input: a process, domain or dictionary of domains.
    If there is only one axis with length > 1 in the process or
    set of domains, return the name of that axis.
    Otherwise raise an error.'''
    axes = get_axes(process_or_domain)
    diff_ax = {}
    for axname, ax in axes.iteritems():
        if ax.num_points > 1:
            diff_ax.update({axname: ax})
    if len(diff_ax.keys()) == 1:
        return diff_ax.keys()[0]
    else:
        raise ValueError('More than one possible diffusion axis.')
