from __future__ import division
from builtins import object
import numpy as np
# experimental...  this is not well documented!
from numpy.core.umath_tests import matrix_multiply

'''
Testing multi-dimensional column radiation

:Example:

    .. code-block:: python

        import numpy as np
        import climlab
        sfc, atm = climlab.domain.zonal_mean_column()
        absorb = np.ones(atm.shape)
        trans = climlab.radiation.transmissivity.Transmissivity(absorptivity=absorb,axis=1)
        fromspace = np.zeros(sfc.shape)
        emission = 200*np.ones(atm.shape)
        A = trans.flux_down(fluxDownTop=fromspace, emission=emission)
        A.shape

'''

class Transmissivity(object):
    '''Class for calculating and store transmissivity between levels,
    and computing radiative fluxes between levels.

    Input: numpy array of absorptivities.
    It is assumed that the last dimension is vertical levels.

    Attributes: (all stored as numpy arrays):

    * N: number of levels
    * absorptivity: level absorptivity (N)
    * transmissivity: level transmissivity (N)
    * Tup: transmissivity matrix for upwelling beam (N+1, N+1)
    * Tdown: transmissivity matrix for downwelling beam (N+1, N+1)


    Example for N = 3 atmospheric layers:

    tau is a vector of transmissivities

    .. math::

        \\tau = \\left[ 1, \\tau_0, \\tau_1, \\tau_2 \\right]

    A is a matrix

    .. math::

        A= \\left[ \\begin{array}{cccc}
        1       & 1         & 1         & 1         \\\\
        \\tau_0 & 1         & 1         & 1         \\\\
        \\tau_0 & \\tau_1   & 1         & 1         \\\\
        \\tau_0 & \\tau_1   & \\tau_2   & 1         \\\\
        \\end{array} \\right]

    We then take the cumulative product along columns,
    and finally take the lower triangle of the result to get

    .. math::

        Tup= \\left[ \\begin{array}{cccc}
                               1 &               0 &       0 &  0   \\\\
                         \\tau_0 &               1 &       0 &  0   \\\\
                 \\tau_1 \\tau_0 &         \\tau_1 &       1 &  0   \\\\
        \\tau_2 \\tau_1 \\tau_00 & \\tau_2 \\tau_1 & \\tau_2 &  1   \\\\
        \\end{array} \\right]

    and Tdown = transpose(Tup)

    Construct an emission vector for the downwelling beam:

    Edown = [E0, E1, E2, fromspace]

    Now we can get the downwelling beam by matrix multiplication:

    D = Tdown * Edown

    For the upwelling beam, we start by adding the reflected part
    at the surface to the surface emissions:

    Eup = [emit_sfc + albedo_sfc*D[0], E0, E1, E2]

    So that the upwelling flux is

    U = Tup * Eup

    The total flux, positive up is thus

    F = U - D

    The absorbed radiation at the surface is then -F[0]
    The absorbed radiation in the atmosphere is the flux convergence:

    -diff(F)

    '''
    #  quick hack to get some simple cloud albedo
    def __init__(self, absorptivity, reflectivity=None, axis=0):
        self.axis = axis
        #if absorptivity.ndim is not 1:
        #    raise ValueError('absorptivity argument must be a vector')
        if reflectivity is None:
            reflectivity = np.zeros_like(absorptivity)
        self.reflectivity = reflectivity
        self.absorptivity = absorptivity
        self.transmissivity = 1 - absorptivity - reflectivity
        self.shape = self.absorptivity.shape
        N = np.size(self.absorptivity, axis=self.axis)
        self.N = N
        #  For now, let's assume that the vertical axis is the last axis
        Tup, Tdown = compute_T_vectorized(self.transmissivity)
        self.Tup = Tup
        self.Tdown = Tdown

    #def flux_down(self, fluxDownTop, emission=None):
    def flux_up(self, fluxUpBottom, emission=None):
        '''Compute downwelling radiative flux at interfaces between layers.

        Inputs:

            * fluxDownTop: flux down at top
            * emission: emission from atmospheric levels (N)
              defaults to zero if not given

        Returns:

            * vector of downwelling radiative flux between levels (N+1)
              element 0 is the flux down to the surface.

        '''
        if emission is None:
            emission = np.zeros_like(self.absorptivity)
        E = np.concatenate((emission, np.atleast_1d(fluxUpBottom)), axis=-1)
        #  dot product (matrix multiplication) along last axes
        return np.squeeze(matrix_multiply(self.Tup, E[..., np.newaxis]))

    def flux_reflected_up(self, fluxDown, albedo_sfc=0.):
        #reflectivity = np.concatenate((np.atleast_1d(albedo_sfc),
        #                               self.reflectivity), axis=-1)
        reflectivity = np.concatenate((self.reflectivity, np.atleast_1d(albedo_sfc)), axis=-1)
        return reflectivity*fluxDown

    #def flux_up(self, fluxUpBottom, emission=None):
    def flux_down(self, fluxDownTop, emission=None):
        '''Compute upwelling radiative flux at interfaces between layers.

        Inputs:

            * fluxUpBottom: flux up from bottom
            * emission: emission from atmospheric levels (N)
              defaults to zero if not given

        Returns:
            * vector of upwelling radiative flux between levels (N+1)
              element N is the flux up to space.

        '''
        if emission is None:
            emission = np.zeros_like(self.absorptivity)
        E = np.concatenate((np.atleast_1d(fluxDownTop),emission), axis=-1)
        #  dot product (matrix multiplication) along last axes
        return np.squeeze(matrix_multiply(self.Tdown, E[..., np.newaxis]))
#
#def compute_T(transmissivity):
#    # fully vectorized version
#    # works on a vector transmissivity
#    N = transmissivity.size
#    tau = np.concatenate((np.atleast_1d(1.), transmissivity))
#    B = np.tile(tau, (N+1,1)).transpose()
#    A = np.tril(B,k=-1) + np.tri(N+1).transpose()
#    Tup = np.tril(np.cumprod(A, axis=0))
#    Tdown = np.transpose(Tup)
#    return Tup, Tdown

def compute_T_vectorized(transmissivity):
    #  really vectorized version... to work with arbitrary dimensions
    #  of input transmissivity
    #  Assumption is the last dimension of transmissivity is vertical
    trans_shape = np.shape(transmissivity)
    N = trans_shape[-1]
    otherdims = trans_shape[:-1]
    ones = np.ones(otherdims)[..., np.newaxis]
    tau = np.concatenate((ones, transmissivity), axis=-1)
    tiletau = np.tile(tau[..., np.newaxis],N+1)
    # equivalent to taking transpose of last two axes
    #B = np.rollaxis(tiletau, -1, -2)
    B = tiletau
    matdims = np.append(np.array(otherdims),[1,1])
    # dimensions of matrix should be [otherdims,N+1,N+1]
    tri = np.tile(np.tri(N+1).transpose(),matdims)
    #  np.tril refuses to broadcast over other dims in numpy < 1.9
    #  use a custom version instead, below
    #  Performance is BETTER with numpy 1.9
    A = tril(B,k=-1) + tri
    #Tup = tril(np.cumprod(A, axis=-2))
    ##  transpose over last two axes
    #Tdown = np.rollaxis(Tup, -1, -2)
    Tdown = tril(np.cumprod(A, axis=-2))
    #  transpose over last two axes
    Tup = np.rollaxis(Tdown, -1, -2)
    return Tup, Tdown


def tril(array, k=0):
    '''Lower triangle of an array.
    Return a copy of an array with elements above the k-th diagonal zeroed.
    Need a multi-dimensional version here because numpy.tril does not
    broadcast for numpy verison < 1.9.'''
    try:
        tril_array = np.tril(array, k=k)
    except:
        # have to loop
        tril_array = np.zeros_like(array)
        shape = array.shape
        otherdims = shape[:-2]
        for index in np.ndindex(otherdims):
            tril_array[index] = np.tril(array[index], k=k)
    return tril_array
