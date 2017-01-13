'''
Cloud parameterizations following
Stephens, G. (1978): Radiation Profiles in Extended Water Clouds. II: Parameterization Schemes
J. Atm. Sci. 35, 2123--2132


w is cloud liquid water content in g/m3
w =
total vertical LWP in g/m2
W = w * deltaz

unfinished...
'''
from __future__ import division
import numpy as np

def compute_tauN(W):
    #  Stephens eq. 10
    #  parameterization for cloud optical thickness
    #  0.3 - 0.75 micron band
    log10_tauN1 = 0.2633 + 1.7095*np.log(np.log10(W))
    #0.75 - 4 micron band
    log10_tauN2 = 0.3492 + 1.6518*np.log(np.log10(W))
    tauN1 = 10**log10_tauN1
    tauN2 = 10**log10_tauN2
    return tauN1, tauN2

# Just use the first band, non-absorbing
def Reflection(tauN, coszen):
    #beta = compute_beta(tauN, coszen)
    # from table 2 of Stephens... beta does not vary much
    beta = 0.065
    return beta*tauN / coszen / (1+beta*tauN/coszen)

def compute_eps(W):
    #  absorption cross-section in m2/g
    a0down = 0.158
    a0up = 0.130  # equation 16
    eps_up = 1 - np.exp(-a0up * W)  # equation 15
    eps_down = 1 - np.exp(-a0down * W)
    return eps_up, eps_down

def compute_beta(tauN, coszen):
    poly_beta1 = np.array([0.,
                           0.1145,
                          -0.0538,
                           0.1024,
                           0.0067,
                           0.0662,
                          -0.3249,
                           0.0001213,
                          -0.007498,
                           0.00547,
                           0.1412])
    x = np.log(tauN)
    y = coszen
    result = 0.
    for i in range(1,5):
        for j in range(1,i+1):
            result += poly_beta1[n] * x**(i-j) * y**(j-i)
    return result
