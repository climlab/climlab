import numpy as np
from climlab import constants as const
from climlab.process.time_dependent_process import TimeDependentProcess
from climlab.domain.field import Field


class ConvectiveAdjustment(TimeDependentProcess):
    def __init__(self, adj_lapse_rate=None, **kwargs):
        super(ConvectiveAdjustment, self).__init__(**kwargs)
        # lapse rate for convective adjustment, in K / km
        self.adj_lapse_rate = adj_lapse_rate
        self.param['adj_lapse_rate'] = adj_lapse_rate
        self.time_type = 'adjustment'
        self.adjustment = {}
        patm = self.lev
        c_atm = self.Tatm.domain.heat_capacity
        c_sfc = self.Ts.domain.heat_capacity       
        self.pnew = np.flipud(np.append(np.flipud(patm), const.ps))
        self.cnew = np.flipud(np.append(np.flipud(c_atm), c_sfc))
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
    
    def compute(self):
        #lapse_rate = self.param['adj_lapse_rate']
        if self.adj_lapse_rate is None:
            self.adjusted_state = self.state
        else:
            #  For now, let's assume that the vertical axis is the last axis
            unstable_Ts = np.atleast_1d(self.Ts)
            unstable_Tatm = self.Tatm
            Tcol = np.concatenate((unstable_Ts, unstable_Tatm),axis=-1)
            Tadj = convective_adjustment_direct(self.pnew, Tcol, self.cnew, lapserate=self.adj_lapse_rate)            
            Ts = Field(Tadj[...,0], domain=self.Ts.domain)
            Tatm = Field(Tadj[...,1:], domain=self.Tatm.domain)
            self.adjustment['Ts'] = Ts - self.Ts
            self.adjustment['Tatm'] = Tatm - self.Tatm


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
    Pi = (p[:]/const.ps)**alpha  # will need to modify to allow variable lapse rates
    beta = 1./Pi    
    theta = T * beta
    q = Pi * c
    n_k = np.zeros(L, dtype=np.int8)
    theta_k = np.zeros_like(p)
    s_k = np.zeros_like(p)
    t_k = np.zeros_like(p)
    thetanew = np.zeros_like(p)
    thetaadj = Akamaev_adjustment_multidim(theta, q, beta, n_k, 
                                           theta_k, s_k, t_k, thetanew)
    T = thetaadj * Pi
    return T

from numba import jit
#@jit(nopython=True)
def Akamaev_adjustment_multidim(theta, q, beta, n_k, theta_k, s_k, t_k, thetanew):
    L = q.size  # number of vertical levels
    size0 = theta.shape[0] #np.size(T, axis=0)
    if size0 != L:
        num_lat = size0
        for lat in range(num_lat):
            thetanew = Akamaev_adjustment(theta[lat,:], q, beta, n_k, theta_k, s_k, t_k)
            #theta[lat,:] = thetanew  # this causes problems with jit
            for l in range(L):
                theta[lat,l] = thetanew[l]
    else:
        num_lat = 1
        theta = Akamaev_adjustment(theta, q, beta, n_k, theta_k, s_k, t_k)
    return theta

#@jit(nopython=True)
def Akamaev_adjustment(theta, q, beta, n_k, theta_k, s_k, t_k):
    '''Single column only.'''
    L = q.size  # number of vertical levels
    k = 0
    n_k[0] = 1
    theta_k[0] = theta[0]
    
    for l in range(1, L):
        n = 1
        thistheta = theta[l]
        done = False
        while not done:
            if (theta_k[k] > thistheta):
                s = 0.
                t = 0.
                # stratification is unstable
                if n == 1:
                    # current layer is not an earlier-formed neutral layer
                    s = q[l]
                    t = s * thistheta
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
                thistheta = t / s
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
        theta_k[k] = thistheta
    #  finished looping through to check stability

    # update the potential temperatures
    for l in range(L-1,-1,-1):
        theta[l] = thistheta
        if n>1:
            n -= 1
        else:
            k -= 1
            n = n_k[k]
            thistheta = theta_k[k]
    return theta
