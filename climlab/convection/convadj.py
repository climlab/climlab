from __future__ import division
import numpy as np
from climlab import constants as const
from climlab.process.time_dependent_process import TimeDependentProcess
from climlab.domain.field import Field


class ConvectiveAdjustment(TimeDependentProcess):
    '''Convective adjustment process
    Instantly returns column to neutral lapse rate

    Adjustment includes the surface IF 'Ts' is included in the state
    dictionary. Otherwise only the atmopsheric temperature is adjusted.'''
    def __init__(self, adj_lapse_rate=None, **kwargs):
        super(ConvectiveAdjustment, self).__init__(**kwargs)
        # lapse rate for convective adjustment, in K / km
        self.adj_lapse_rate = adj_lapse_rate
        self.param['adj_lapse_rate'] = adj_lapse_rate
        self.time_type = 'adjustment'
        self.adjustment = {}
        patm = self.lev
        c_atm = self.Tatm.domain.heat_capacity
        if 'Ts' in self.state:
            c_sfc = self.Ts.domain.heat_capacity
            #self.pnew = np.append(patm, const.ps)
            #  surface pressure should correspond to model domain!
            ps = self.lev_bounds[-1]
            self.pnew = np.append(patm, ps)
            self.cnew = np.append(c_atm, c_sfc)
        else:
            self.pnew = patm
            self.cnew = c_atm
    @property
    def adj_lapse_rate(self):
        return self._adj_lapse_rate
    @adj_lapse_rate.setter
    def adj_lapse_rate(self, lapserate):
        if lapserate is 'DALR':
            self._adj_lapse_rate = const.g / const.cp * 1.E3
        else:
            self._adj_lapse_rate = lapserate
        self.param['adj_lapse_rate'] = self._adj_lapse_rate

    def _compute(self):
        #lapse_rate = self.param['adj_lapse_rate']
        if self.adj_lapse_rate is None:
            #self.adjustment = self.state * 0.
            self.adjustment['Ts'] = self.Ts * 0.
            self.adjustment['Tatm'] = self.Tatm * 0.
        else:
            #  For now, let's assume that the vertical axis is the last axis
            unstable_Tatm = self.Tatm
            if 'Ts' in self.state:
                unstable_Ts = np.atleast_1d(self.Ts)
                #Tcol = np.concatenate((unstable_Ts, unstable_Tatm),axis=-1)
                Tcol = np.concatenate((unstable_Tatm, unstable_Ts),axis=-1)
            else:
                Tcol = unstable_Tatm
            #  convective adjustment routine expect reversered vertical axis
            pflip = self.pnew[..., ::-1]
            Tflip = Tcol[..., ::-1]
            cflip = self.cnew[..., ::-1]
            Tadj_flip = convective_adjustment_direct(pflip, Tflip, cflip, lapserate=self.adj_lapse_rate)
            Tadj = Tadj_flip[..., ::-1]
            if 'Ts' in self.state:
                Ts = Field(Tadj[...,-1], domain=self.Ts.domain)
                Tatm = Field(Tadj[...,:-1], domain=self.Tatm.domain)
                self.adjustment['Ts'] = Ts - self.Ts
            else:
                Tatm = Field(Tadj, domain=self.Tatm.domain)
            self.adjustment['Tatm'] = Tatm - self.Tatm
        # # express the adjustment (already accounting for the finite time step)
        # #  as a tendency per unit time, so that it can be applied along with explicit
        # tendencies = {}
        # for name, adj in self.adjustment.iteritems():
        #     tendencies[name] = adj / self.param['timestep']
        # return tendencies
        #  go back to just returning the adjustment, independent of timestep
        #  because the parent process might have set a different timestep!
        return self.adjustment


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
    """
    # largely follows notation and algorithm in Akamaev (1991) MWR

    #if lapserate is 'DALR':
    #    lapserate = const.g / const.cp * 1.E3
    #try:
    #    alpha = const.Rd / const.g * lapserate / 1.E3
    #except:
    #    raise ValueError('Problem with lapse rate')
    #lapserate = lapserate * np.ones(T.shape)  # same dimensions as T
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
    thetaadj = Akamaev_adjustment_multidim(theta, q, beta, n_k,
                                           theta_k, s_k, t_k)
    T = thetaadj * Pi
    return T


# @jit  # numba.jit not working here. Not clear why.
#  At least we get something like 10x speedup from the inner loop
def Akamaev_adjustment_multidim(theta, q, beta, n_k, theta_k, s_k, t_k):
    L = q.size  # number of vertical levels
    size0 = theta.shape[0] #np.size(T, axis=0)
    if size0 != L:
        num_lat = size0
        for lat in range(num_lat):
            theta[lat,:] = Akamaev_adjustment(theta[lat,:], q, beta, n_k,
                                              theta_k, s_k, t_k)
    else:
        num_lat = 1
        theta = Akamaev_adjustment(theta, q, beta, n_k, theta_k, s_k, t_k)
    return theta


def Akamaev_adjustment(theta, q, beta, n_k, theta_k, s_k, t_k):
    '''Single column only.'''
    L = q.size  # number of vertical levels
    # Akamaev step 1
    k = 1
    n_k[k-1] = 1
    theta_k[k-1] = theta[k-1]
    l = 2
    while True:
        # Akamaev step 2
        n = 1
        thistheta = theta[l-1]
        while True:
            # Akamaev step 3
            if theta_k[k-1] <= thistheta:
                # Akamaev step 6
                k += 1
                break  # to step 7
            else:
                if n <= 1:
                    s = q[l-1]
                    t = s*thistheta
                # Akamaev step 4
                if n_k[k-1] <= 1:
                # lower adjacent level is not an earlier-formed neutral layer
                    s_k[k-1] = q[l-n-1]
                    t_k[k-1] = s_k[k-1] * theta_k[k-1]
                # Akamaev step 5
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
        # Akamaev step 7
        if l == L:  # the scan is over
            break  # to step 8
        l += 1
        n_k[k-1] = n
        theta_k[k-1] = thistheta
        # back to step 2


#
#    for l in range(2, L+1):  # l = 2,3,4,...,L
#        n = 1
#        thistheta = theta[l-1]
#        done = False
#        while not done:
#            if (theta_k[k-1] > thistheta):
#                s = 0.
#                t = 0.
#                # stratification is unstable
#                if n == 1:
#                    # current layer is not an earlier-formed neutral layer
#                    s = q[l-1]
#                    t = s * thistheta
#                if (n_k[k-1] < 2):
#                    # lower adjacent level is not an earlier-formed neutral layer
#                    s_k[k-1] = q[l-n-1]
#                    t_k[k-1] = s_k[k-1] * theta_k[k-1]
#                #  join current and underlying layers
#                n += n_k[k-1]
#                s += s_k[k-1]
#                s_k[k-1] = s
#                t += t_k[k-1]
#                t_k[k-1] = t
#                thistheta = t / s
#                if k == 1-1:
#                    # joint neutral layer in the first one, done checking lower layers
#                    done = True
#                else:
#                    k -= 1
#                    # go back and check stability of the lower adjacent layer
#            else:
#                k += 1  # statification is stable
#                done = True
#        if l < L:
#            n_k[k-1] = n
#            theta_k[k-1] = thistheta
#    #  finished looping through to check stability

    # update the potential temperatures
    while True:
        while True:
            # Akamaev step 8
            if n==1: # current model level was not included in any neutral layer
                break  # to step 11
            while True:
                #  Akamaev step 9
                theta[l-1] = thistheta
                if n==1:
                    break
                #  Akamaev step 10
                l -= 1
                n -= 1
                #  back to step 9
        # Akamaev step 11
        if k==1:
            break
        k -= 1
        l -= 1
        n = n_k[k-1]
        thistheta = theta_k[k-1]
        # back to step 8

#    for l in range(L, 0, -1):
#        if n > 1:
#        while n>1:
#            theta[l-1] = thistheta
#            n -= 1
#        else:
#            k -= 1
#            n = n_k[k]
#            thistheta = theta_k[k]
    return theta

#  Attempt to use numba to compile the Akamaev_adjustment function
#  which gives at least 10x speedup
#   If numba is not available or compilation fails, the code will be executed
#   in pure Python. Results should be identical
try:
    from numba import jit
    Akamaev_adjustment = jit(signature_or_function=Akamaev_adjustment)
    #print 'Compiling Akamaev_adjustment() with numba.'
except:
    pass
