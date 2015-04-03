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

import numpy as np
import climlab
from climlab.dynamics.diffusion import MeridionalDiffusion
from climlab.utils import legendre
sfc = climlab.domain.zonal_mean_surface(num_lat=90, water_depth=10.)
lat = sfc.lat.points
initial = 12. - 40. * legendre.P2(np.sin(np.deg2rad(lat)))
# make a copy of initial so that it remains unmodified
Ts = climlab.Field(np.array(initial), domain=sfc)
# thermal diffusivity in W/m**2/degC
D = 0.55
# meridional diffusivity in 1/s
K = D / sfc.heat_capacity
d = MeridionalDiffusion(state=Ts, K=K)
d.integrate_years(1.)
import matplotlib.pyplot as plt
plt.plot(lat, initial, lat, Ts)
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
        self.param['K'] = K  # Diffusivity in units of [length]**2 / time
        self.use_banded_solver = use_banded_solver
        if diffusion_axis is None:
            self.diffusion_axis = _guess_diffusion_axis(self)
        else:
            self.diffusion_axis = diffusion_axis
        # This currently only works with evenly spaced points
        for dom in self.domains.values():
            delta = np.mean(dom.axes[self.diffusion_axis].delta)
            bounds = dom.axes[self.diffusion_axis].bounds
        self.K_dimensionless = (self.param['K'] * np.ones_like(bounds) *
                                self.param['timestep'] / delta**2)
        self.diffTriDiag = _make_diffusion_matrix(self.K_dimensionless)

    def _implicit_solver(self):
        # Time-stepping the diffusion is just inverting this matrix problem:
        # self.T = np.linalg.solve( self.diffTriDiag, Trad )
        newstate = {}
        for varname, value in self.state.iteritems():
            if self.use_banded_solver:
                newvar = _solve_implicit_banded(value, self.diffTriDiag)
            else:
                newvar = np.linalg.solve(self.diffTriDiag, value)
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
        self.K_dimensionless *= 1./np.deg2rad(1.)**2
        for dom in self.domains.values():
            latax = dom.axes['lat']
        self.diffTriDiag = \
            _make_meridional_diffusion_matrix(self.K_dimensionless, latax)


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


def _make_meridional_diffusion_matrix(K, lataxis):
    phi_stag = np.deg2rad(lataxis.bounds)
    phi = np.deg2rad(lataxis.points)
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
