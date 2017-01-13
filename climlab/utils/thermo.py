"""
A collection of function definitions to handle common
thermodynamic calculations for the atmosphere.
"""

from __future__ import division
import numpy as np
from climlab import constants as const


def potential_temperature(T,p):
    """Compute potential temperature for an air parcel.

    Input:  T is temperature in Kelvin
            p is pressure in mb or hPa
    Output: potential temperature in Kelvin.

    """
    theta = T*(const.ps/p)**const.kappa
    return theta

def theta(T,p):
    '''Convenience method, identical to thermo.potential_temperature(T,p).'''
    return potential_temperature(T,p)


def temperature_from_potential(theta,p):
    """Convert potential temperature to in-situ temperature.

    Input:  theta is potential temperature in Kelvin
            p is pressure in mb or hPa
    Output: absolute temperature in Kelvin.

    """
    T = theta/((const.ps/p)**const.kappa)
    return T


def T(theta,p):
    '''Convenience method, identical to thermo.temperature_from_potential(theta,p).'''
    return temperature_from_potential(theta,p)


def clausius_clapeyron(T):
    """Compute saturation vapor pressure as function of temperature T.

    Input: T is temperature in Kelvin
    Output: saturation vapor pressure in mb or hPa

    Formula from Rogers and Yau "A Short Course in Cloud Physics" (Pergammon Press), p. 16
    claimed to be accurate to within 0.1% between -30degC and 35 degC
    Based on the paper by Bolton (1980, Monthly Weather Review).

    """
    Tcel = T - const.tempCtoK
    es = 6.112 * np.exp(17.67*Tcel/(Tcel+243.5))
    return es


def qsat(T,p):
    """Compute saturation specific humidity as function of temperature and pressure.

    Input:  T is temperature in Kelvin
            p is pressure in hPa or mb
    Output: saturation specific humidity (dimensionless).

    """
    eps = const.Rd / const.Rv
    es = clausius_clapeyron(T)
    q = eps * es / (p - (1 - eps) * es )
    return q


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
    Tcel = T - const.tempCtoK
    L = (2.501 - 0.00237 * Tcel) * 1.E6   # Accurate form of latent heat of vaporization in J/kg
    ratio = L / T / const.Rv
    dTdp = (T / p * const.kappa * (1 + esoverp * ratio) /
        (1 + const.kappa * (const.cpv / const.Rv + (ratio-1) * ratio) * esoverp))
    return dTdp


def estimated_inversion_strength(T0,T700):
    '''Compute the "estimated inversion strength", T0 is surface temp, T700 is temp at 700 hPa, both in K.
    Following Wood and Bretherton, J. Climate 2006.

    '''
    RH = 0.8
    T850 = (T0+T700)/2.;
    LCL = (20. + (T0-const.tempCtoK)/5.)*(1.-RH)  # approximate formula... could do this more accurately.
    LTS = potential_temperature(T700, 700) - T0  # Lower Tropospheric Stability (theta700 - theta0)
    Gammam = (const.g/const.cp*(1.0 - (1.0 + const.Lhvap*qsat(T850,850) /
              const.Rd / T850) / (1.0 + const.Lhvap**2 * qsat(T850,850)/
              const.cp/const.Rv/T850**2)))
    z700 = (const.Rd*T0/const.g)*np.log(1000./700.)
    return LTS - Gammam*(z700 - LCL)

def EIS(T0,T700):
    '''Convenience method, identical to thermo.estimated_inversion_strength(T0,T700)'''
    return estimated_inversion_strength(T0,T700)

def blackbody_emission(T):
    '''Blackbody radiation following the Stefan-Boltzmann law.'''
    return const.sigma * T**4

def Planck_frequency(nu, T):
    '''The Planck function B(nu,T):
    the flux density for blackbody radiation in frequency space
    nu is frequency in 1/s
    T is temperature in Kelvin

    Formula from Raymond Pierrehumbert, "Principles of Planetary Climate"

    '''
    h = const.hPlanck
    c = const.c_light
    k = const.kBoltzmann
    return 2*h*nu**3/c**2/(np.exp(h*nu/k/T)-1)

def Planck_wavenumber(n, T):
    '''The Planck function (flux density for blackbody radition)
    in wavenumber space
    n is wavenumber in 1/cm
    T is temperature in Kelvin

    Formula from Raymond Pierrehumbert, "Principles of Planetary Climate"

    '''
    c = const.c_light
    # convert to mks units
    n = n*100.
    return c * Planck_frequency(n*c, T)

def Planck_wavelength(l, T):
    '''The Planck function (flux density for blackbody radiation)
    in wavelength space
    l is wavelength in meters
    T is temperature in Kelvin

    Formula from Raymond Pierrehumbert, "Principles of Planetary Climate"

    '''
    h = const.hPlanck
    c = const.c_light
    k = const.kBoltzmann
    u = h*c/l/k/T
    return 2*k**5*T**5/h**4/c**3*u**5/(np.exp(u)-1)
