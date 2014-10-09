'''thermo.py

A collection of function definitions to handle common
thermodynamic calculations for the atmosphere.

Code developed by Brian Rose, University at Albany
brose@albany.edu
in support of the class ATM/ENV 415: Climate Laboratory
'''

import numpy as np
from constants import *

#def PotentialTemperature(T,p):
def potential_temperature(T,p):
    """Compute potential temperature for an air parcel.
    
    Input:  T is temperature in Kelvin
            p is pressure in mb or hPa
    Output: potential temperature in Kelvin.
    """
    
    theta = T*(ps/p)**kappa
    return theta

#def TfromTHETA(theta,p):
def temperature_from_potential(theta,p):
    """Convert potential temperature to in-situ temperature.
    
    Input:  theta is potential temperature in Kelvin
            p is pressure in mb or hPa
    Output: absolute temperature in Kelvin.
    """
    
    T = theta/((ps/p)**kappa)
    return T


#def ClausiusClapeyron(T):
def clausius_clapeyron(T):
    """Compute saturation vapor pressure as function of temperature T.
    
    Input: T is temperature in Kelvin
    Output: saturation vapor pressure in mb or hPa
    
    Formula from Rogers and Yau "A Short Course in Cloud Physics" (Pergammon Press), p. 16
    claimed to be accurate to within 0.1% between -30degC and 35 degC
    Based on the paper by Bolton (1980, Monthly Weather Review).
    """

    Tcel = T - tempCtoK
    es = 6.112 * np.exp(17.67*Tcel/(Tcel+243.5))
    return es

def qsat(T,p):
    """Compute saturation specific humidity as function of temperature and pressure.

    Input:  T is temperature in Kelvin
            p is pressure in hPa or mb
    Output: saturation specific humidity (dimensionless).
    """
    
    eps = Rd/Rv
    es = clausius_clapeyron(T)
    q = eps * es / (p - (1 - eps) * es )
    return q
#  End of function qsat(T,p)


def pseudoadiabat(T,p):
    """Compute the local slope of the pseudoadiabat at given temperature and pressure
    
    Inputs:   p is pressure in hPa or mb
              T is local temperature in Kelvin
    Output:   dT/dp, the rate of temperature change for pseudoadiabatic ascent
              
    the pseudoadiabat describes changes in temperature and pressure for an air 
    parcel at saturation assuming instantaneous rain-out of the super-saturated water
    
    Formula from Raymond Pierrehumbert, "Principles of Planetary Climate" 
    """

    esoverp = clausius_clapeyron(T) / p
    Tcel = T - tempCtoK
    L = (2.501 - 0.00237 * Tcel) * 1.E6   # Accurate form of latent heat of vaporization in J/kg
    ratio = L / T / Rv
    dTdp = (T / p * kappa * (1 + esoverp * ratio) / 
        (1 + kappa * (cpv / Rv + (ratio-1) * ratio) * esoverp))
    return dTdp
# End of function pseudoadiabat(T,p)

#def EIS(T0,T700):
def estimated_inversion_strength(T0,T700):
    '''Compute the "estimated inversion strength", T0 is surface temp, T700 is temp at 700 hPa, both in K.
    Following Wood and Bretherton, J. Climate 2006.
    '''
    
    RH = 0.8
    T850 = (T0+T700)/2;
    LCL = (20 + (T0-tempCtoK)/5)*(1-RH)  # approximate formula... could do this more accurately.
    LTS = potential_temperature(T700, 700) - T0  # Lower Tropospheric Stability (theta700 - theta0)
    Gammam = g/cp*(1.0 - (1.0 + Lhvap*qsat(T850,850) / Rd / T850) / (1.0 + Lhvap**2 * qsat(T850,850)/cp/Rv/T850**2))
    z700 = (Rd*T0/g)*np.log(1000/700)
    return LTS - Gammam*(z700 - LCL)
