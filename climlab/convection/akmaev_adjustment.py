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
    # largely follows notation and algorithm in Akmaev (1991) MWR
    alpha = const.Rd / const.g * lapserate / 1.E3 # same dimensions as lapserate
    L = p.size
    #  Here const.ps = 1000 hPa is just used as a reference pressure
    #  to compute potential temperature, so is valid even for different
    #  surface pressures
    Pi = (p[:]/const.ps)**alpha  # will need to modify to allow variable lapse rates
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


# @jit  # numba.jit not working here. Not clear why.
#  At least we get something like 10x speedup from the inner loop
def Akmaev_adjustment_multidim(theta, q, beta, n_k, theta_k, s_k, t_k):
    L = q.size  # number of vertical levels
    size0 = theta.shape[0] #np.size(T, axis=0)
    if size0 != L:
        num_lat = size0
        for lat in range(num_lat):
            theta[lat,:] = Akmaev_adjustment(theta[lat,:], q, beta, n_k,
                                              theta_k, s_k, t_k)
    else:
        num_lat = 1
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

###  ONLY DO THIS IN PYTHON 2!
#  Because of a bug in numba for while / break loops that is causing all calls
#  to ConvectiveAdjustment to hang forever!
#   https://github.com/numba/numba/issues/2273
if sys.version_info < (3,0):
    try:
        from numba import jit
        Akmaev_adjustment = jit(signature_or_function=Akmaev_adjustment)
        #print 'Compiling Akmaev_adjustment() with numba.'
    except:
        pass
