from __future__ import division
import numpy as np
from climlab import constants as const
import sys


def convective_adjustment_direct(p, T, c, lapserate=6.5):
    """Convective Adjustment to a specified lapse rate.

    Input argument lapserate gives the lapse rate expressed in degrees K per km
    (positive means temperature increasing downward).

    Default lapse rate is 6.5 K / km.

    Returns the adjusted Column temperature.
    inputs:
    p is pressure in hPa
    T is temperature in K
    c is heat capacity in in J / m**2 / K

    Implements the conservative adjustment algorithm from Akmaev (1991) MWR
    """
    # make sure lapserate has same dimensionality as T
    lapserate = lapserate * np.ones_like(T)
    # largely follows notation and algorithm in Akmaev (1991) MWR
    alpha = const.Rd / const.g * lapserate / 1.E3 # same dimensions as lapserate
    L = p.size
    ###  now handles variable lapse rate in multiple dimensions
    # prepend const.ps = 1000 hPa as ref pressure to compute potential temperature
    pextended = np.insert(p,0,const.ps)
    #  For now, let's assume that the vertical axis is the last axis
    Pi = np.cumprod((p / pextended[:-1])**alpha, axis=-1)  # Akmaev's equation 14 recurrence formula
    beta = 1./Pi
    theta = T * beta
    q = Pi * c
    n_k = np.zeros(L, dtype=np.int8)
    theta_k = np.zeros_like(p)
    s_k = np.zeros_like(p)
    t_k = np.zeros_like(p)
    thetaadj = Akmaev_adjustment_multidim(theta, q, beta, n_k,
                                           theta_k, s_k, t_k)
    T = thetaadj * Pi
    return T


def Akmaev_adjustment_multidim(theta, q, beta, n_k, theta_k, s_k, t_k):
    num_lev = theta.shape[-1]  # number of vertical levels
    otherdims = theta.shape[:-1] # everything except last dimension, which we assume is vertical
    if otherdims != ():
        othersize = np.prod(otherdims)
        theta_reshape = theta.reshape((othersize, num_lev))
        q_reshape = q.reshape((othersize, num_lev))
        beta_reshape = beta.reshape((othersize, num_lev))
        for n in range(othersize):
            theta_reshape[n,:] = Akmaev_adjustment(theta_reshape[n,:],
               q_reshape[n,:], beta_reshape[n,:], n_k, theta_k, s_k, t_k)
        theta = theta_reshape.reshape(theta.shape)
    else:
        theta = Akmaev_adjustment(theta, q, beta, n_k, theta_k, s_k, t_k)
    return theta


def Akmaev_adjustment(theta, q, beta, n_k, theta_k, s_k, t_k):
    '''Single column only.'''
    L = q.size  # number of vertical levels
    # Akmaev step 1
    k = 1
    n_k[k-1] = 1
    theta_k[k-1] = theta[k-1]
    l = 2
    while True:
        # Akmaev step 2
        n = 1
        thistheta = theta[l-1]
        while True:
            # Akmaev step 3
            if theta_k[k-1] <= thistheta:
                # Akmaev step 6
                k += 1
                break  # to step 7
            else:
                if n <= 1:
                    s = q[l-1]
                    t = s*thistheta
                # Akmaev step 4
                if n_k[k-1] <= 1:
                # lower adjacent level is not an earlier-formed neutral layer
                    s_k[k-1] = q[l-n-1]
                    t_k[k-1] = s_k[k-1] * theta_k[k-1]
                # Akmaev step 5
                    #  join current and underlying layers
                n += n_k[k-1]
                s += s_k[k-1]
                t += t_k[k-1]
                s_k[k-1] = s
                t_k[k-1] = t
                thistheta = t/s
                if k==1:
                    #  joint neutral layer is the first one
                    break  # to step 7
                k -= 1
                # back to step 3
        # Akmaev step 7
        if l == L:  # the scan is over
            break  # to step 8
        l += 1
        n_k[k-1] = n
        theta_k[k-1] = thistheta
        # back to step 2

    # update the potential temperatures
    while True:
        while True:
            # Akmaev step 8
            if n==1: # current model level was not included in any neutral layer
                break  # to step 11
            while True:
                #  Akmaev step 9
                theta[l-1] = thistheta
                if n==1:
                    break
                #  Akmaev step 10
                l -= 1
                n -= 1
                #  back to step 9
        # Akmaev step 11
        if k==1:
            break
        k -= 1
        l -= 1
        n = n_k[k-1]
        thistheta = theta_k[k-1]
        # back to step 8
    return theta

#  Attempt to use numba to compile the Akmaev_adjustment function
#  which gives at least 10x speedup
#   If numba is not available or compilation fails, the code will be executed
#   in pure Python. Results should be identical
try:
    from numba import jit
    Akmaev_adjustment = jit(signature_or_function=Akmaev_adjustment)
except ImportError:
    pass
