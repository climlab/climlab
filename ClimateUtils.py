#  Version 1.3
#   added some more constants...
#
#   Version 1.2
#     fixed a bug that prevented ClausiusClapeyron to work on arrays
#   and added some new constants
#
##  Version 1.1
##   Made days_per_year more accurate, for consistent insolation calculations with orbital.py
##   also set the solar constant S0 to match the Trenberth et al. data we've been using in class
#  Brian Rose
#   March 10 2014
#  A collection of constants and function definitions to handle common
#  thermodynamic calculations for the atmosphere etc

import numpy as np

#  Define a bunch of useful physical and thermodynamic constants

a = 6.373E6      #  Radius of Earth, in m
Lhvap = 2.5E6    #  Latent heat of vaporization, in J / kg
Lhsub = 2.834E6   # Latent heat of sublimation, in J / kg
Lhfus = Lhsub - Lhvap  #  Latent heat of fusion, in J / kg
cp = 1004.        #  specific heat at constant pressure for dry air, in J / kg / K
Rd = 287.         #  gas constant for dry air, in J / kg / K
kappa = Rd / cp
Rv = 461.5       #  gas constant for water vapor, in J / kg / K
cpv = 1875.       # specific heat at constant pressure for water vapor, in J / kg / K
Omega = 2 * np.math.pi / 24. /3600.  # Earth's rotation rate, in s**(-1)
g = 9.8          #  gravitational acceleration, in m / s**2
sigma = 5.67E-8  #  Stefan-Boltzmann constant for blackbody radiation, W / m**2 / K**4
kBoltzmann = 1.38E-23  #  J / K, the Boltzmann constant

S0 = 1365.2       #  solar constant, W / m**2
ps = 1000.       #  approximate surface pressure, mb or hPa

rho_w = 1000.    #  density of water, kg / m**3
cw = 4181.3      #  specific heat of liquid water, J / kg / K

tempCtoK = 273.15   # 0degC in kelvin
mb_to_Pa = 100.  # conversion factor from mb to Pa

#  Some useful time conversion factors
seconds_per_minute = 60.
seconds_per_hour = 60. * seconds_per_minute
seconds_per_day = 24. * seconds_per_hour
seconds_per_month = 30. * seconds_per_day  # approximate!
days_per_year = 365.2422  # the length of the "tropical year" -- time between vernal equinoxes
seconds_per_year = seconds_per_day * days_per_year

area_earth = 4 * np.math.pi * a**2

#  Some useful general-purpose functions for atmosphere and ocean science


def PotentialTemperature(T,p):
    """Compute potential temperature for an air parcel.
    
    Input:  T is temperature in Kelvin
            p is pressure in mb or hPa
    Output: potential temperature in Kelvin."""
    
    #  not checking anymore because we take advantage of automatic NumPy broadcasting
    #inputCheck(T,p)
    theta = T*(ps/p)**kappa
    return theta

def TfromTHETA(theta,p):
    """Convert potential temperature to in-situ temperature.
    
    Input:  theta is potential temperature in Kelvin
            p is pressure in mb or hPa
    Output: absolute temperature in Kelvin."""
    
    #  not checking anymore because we take advantage of automatic NumPy broadcasting
    #inputCheck(T,p)
    T = theta/((ps/p)**kappa)
    return T


def ClausiusClapeyron(T):
    """Compute saturation vapor pressure as function of temperature T.
    
    Input: T is temperature in Kelvin
    Output: saturation vapor pressure in mb or hPa
    
    Formula from Rogers and Yau "A Short Course in Cloud Physics" (Pergammon Press), p. 16
    claimed to be accurate to within 0.1% between -30degC and 35 degC
    Based on the paper by Bolton (1980, Monthly Weather Review)."""

    Tcel = T - tempCtoK
    es = 6.112 * np.exp(17.67*Tcel/(Tcel+243.5))
    return es
#  End of function ClausiusClapeyron(T)

def qsat(T,p):
    """Compute saturation specific humidity as function of temperature and pressure.

    Input:  T is temperature in Kelvin
            p is pressure in hPa or mb
    Output: saturation specific humidity (dimensionless)."""
    
    #  not checking anymore because we take advantage of automatic NumPy broadcasting
    #inputCheck(T,p)
    eps = Rd/Rv
    es = ClausiusClapeyron(T)
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
    
    Formula from Raymond Pierrehumbert, "Principles of Planetary Climate" """

    #  not checking anymore because we take advantage of automatic NumPy broadcasting
    #inputCheck(T,p)
    esoverp = ClausiusClapeyron(T) / p
    Tcel = T - tempCtoK
    L = (2.501 - 0.00237 * Tcel) * 1.E6   # Accurate form of latent heat of vaporization in J/kg
    ratio = L / T / Rv
    dTdp = (T / p * kappa * (1 + esoverp * ratio) / 
        (1 + kappa * (cpv / Rv + (ratio-1) * ratio) * esoverp))
    return dTdp
# End of function pseudoadiabat(T,p)

def EIS(T0,T700):
    '''compute the "estimated inversion strength", T0 is surface temp, T700 is temp at 700 hPa, both in K.
    Following Wood and Bretherton, J. Climate 2006'''
    RH = 0.8
    T850 = (T0+T700)/2;
    LCL = (20 + (T0-tempCtoK)/5)*(1-RH)  # approximate formula... could do this more accurately.
    LTS = PotentialTemperature(T700, 700) - T0  # Lower Tropospheric Stability (theta700 - theta0)
    Gammam = g/cp*(1.0 - (1.0 + Lhvap*qsat(T850,850) / Rd / T850) / (1.0 + Lhvap**2 * qsat(T850,850)/cp/Rv/T850**2))
    z700 = (Rd*T0/g)*np.log(1000/700)
    return LTS - Gammam*(z700 - LCL)


# This routine just checks for correct dimensions in the input arrays T and p
def inputCheck(T,p):
    if ( np.shape(T) != np.shape(p) ) and np.size(T)>1 and np.size(p)>1:
        raise ValueError('Inputs arrays must have same dimensions, or be scalar.')
#  End of function inputCheck(T,p)
