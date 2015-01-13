import numpy as np
from scipy.linalg import solve_banded
from time_dependent_process import _TimeDependentProcess


class Diffusion(_TimeDependentProcess):
    '''Parent class for implicit diffusion modules.'''
    def __init__(self, K=None, **kwargs):
        super(Diffusion, self).__init__(**kwargs)
        #  heat capacity of surface in J / m**2 / K
        self.process_type = 'implicit'
        self.K = K  # Diffusivity in units of [length]**2 / time
        self.diffusion_axis = 'lev'
        # This currently only works with evenly space points
        delta = np.mean(self.grid[self.diffusion_axis].delta)
        self.K_dimensionless = self.K * self.param['timestep'] / delta
        self.diffTriDiag = _make_diffusion_matrix(self.K_dimensionless)
        self.adjusted_state = {}

    def compute(self):
        # Time-stepping the diffusion is just inverting this matrix problem:
        # self.T = np.linalg.solve( self.diffTriDiag, Trad )
        for varname, value in self.state.iteritems():
            newvar = solve_banded((1, 1), self.diffTriDiag, value)
            self.adjusted_state[varname] = newvar
            self.tendencies[varname] = newvar - value


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
