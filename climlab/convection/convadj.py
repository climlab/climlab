import numpy as np
from climlab import constants as const
from climlab.process.energy_budget import TimeDependentProcess
from climlab.domain.field import Field


class ConvectiveAdjustment(TimeDependentProcess):
    def __init__(self, adj_lapse_rate=None, **kwargs):
        super(ConvectiveAdjustment, self).__init__(**kwargs)
        # lapse rate for convective adjustment, in K / km
        self.param['adj_lapse_rate'] = adj_lapse_rate
        self.time_type = 'adjustment'
        self.adjustment = {}
    
    def compute(self):
        lapse_rate = self.param['adj_lapse_rate']
        if lapse_rate is None:
            self.adjusted_state = self.state
        else:
            unstable_Ts = np.array(self.Ts)
            unstable_Tatm = np.array(self.Tatm)
            c_atm = self.Tatm.domain.heat_capacity
            c_sfc = self.Ts.domain.heat_capacity
            Tcol = np.flipud(np.append(np.flipud(unstable_Tatm), unstable_Ts))
            patm = self.Tatm.domain.lev.points
            pnew = np.flipud(np.append(np.flipud(patm), const.ps))
            cnew = np.flipud(np.append(np.flipud(c_atm), c_sfc))
            #pnew = np.concatenate(([const.ps], patm))
            #cnew = np.concatenate(([c_sfc], c_atm))
            Tadj = convective_adjustment_direct(pnew, Tcol, cnew,
                                        lapserate=lapse_rate)
            Ts = Field(Tadj[0], domain=self.Ts.domain)
            num_lev = self.Tatm.domain.lev.num_points
            Tatm = Field(Tadj[1:num_lev+1], domain=self.Tatm.domain)
            self.adjustment['Ts'] = Ts - self.Ts
            self.adjustment['Tatm'] = Tatm - self.Tatm


#  This routine works but is slow... lots of explicit looping
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

    if lapserate is 'DALR':
        lapserate = const.g / const.cp * 1.E3
    try:
        alpha = const.Rd / const.g * lapserate / 1.E3
    except:
        raise ValueError('Problem with lapse rate')

    Tcol = T
    pnew = p
    L = pnew.size
    Pi = (pnew/const.ps)**alpha
    beta = 1./Pi
    theta = Tcol * beta
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
