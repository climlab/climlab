# -*- coding: utf-8 -*-
"""
Created on Tue Jan  6 15:36:40 2015

@author: Brian
"""
import numpy as np
import constants as const


#  This routine works but is slow... lots of explicit looping
def convective_adjustment(p, T, c, lapserate=6.5):
    """Convective Adjustment to a specified lapse rate.

    Input argument lapserate gives the lapse rate expressed in degrees K per km
    (positive means temperature increasing downward).

    Default lapse rate is 6.5 K / km.

    Returns the adjusted Column temperature.
    inputs:
    p is pressure in hPa
    T is temperature in K
    c is heat capacity in in J / m**2 / K
    """

    # largely follows notation and algorithm in Akamaev (1991) MWR

    if lapserate is 'DALR':
        lapserate = const.g / const.cp * 1.E3
    try:
        alpha = const.Rd / const.g * lapserate / 1.E3
        # col.conv_lapse_rate = lapserate
    except:
        raise ValueError('Problem with lapse rate')

    # Tcol = np.concatenate(([col.Ts], col.Tatm))
    # pnew = np.concatenate(([const.ps], col.p))
    Tcol = T
    pnew = p
    L = pnew.size
    Pi = (pnew/const.ps)**alpha
    beta = 1./Pi
    theta = Tcol * beta
    # q = Pi * np.concatenate(([col.c_sfc], col.c_atm * np.ones_like(col.p)))
    q = Pi * c

    n_k = np.zeros(L, dtype=np.int8)
    theta_k = np.zeros_like(pnew)
    s_k = np.zeros_like(pnew)
    t_k = np.zeros_like(pnew)

    k = 0
    n_k[0] = 1
    theta_k[0] = beta[0] * Tcol[0]
    for l in range(1, L):
        n = 1
        theta = beta[l] * Tcol[l]
        done = False
        while not done:
            if (theta_k[k] > theta):
                # stratification is unstable
                if n == 1:
                    # current layer is not an earlier-formed neutral layer
                    s = q[l]
                    t = s * theta
                if (n_k[k] < 2):
                    # lower adjacent level is not an earlier-formed neutral layer
                    s_k[k] = q[l-n]
                    t_k[k] = s_k[k] * theta_k[k]
                #  join current and underlying layers
                n += n_k[k]
                s += s_k[k]
                s_k[k] = s
                t += t_k[k]
                t_k[k] = t
                theta = t / s
                if k == 0:
                    # joint neutral layer in the first one, done checking lower layers
                    done = True
                else:
                    k -= 1
                    # go back and check stability of the lower adjacent layer
            else:
                k += 1  # statification is stable
                done = True
        # if l < L-1:
        n_k[k] = n
        theta_k[k] = theta
    #  finished looping through to check stability

    # update the temperatures
    newtheta = np.zeros(L)
    count = 0
    for i in range(L):
        newtheta[count+np.arange(n_k[i])] = theta_k[i]
        count += n_k[i]

    Tcol = newtheta * Pi

    return Tcol
