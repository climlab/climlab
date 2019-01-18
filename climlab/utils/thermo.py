"""
A collection of function definitions to handle common
thermodynamic calculations for the atmosphere.
"""

from __future__ import division
from numpy import exp, log
from .constants import (ps, kappa, tempCtoK, eps, Rd, Rv, cpv, cp, g, Lhvap,
                        sigma, hPlanck, c_light, kBoltzmann, molecular_weight)

def potential_temperature(T,p):
    """Compute potential temperature for an air parcel.

    Input:  T is temperature in Kelvin
            p is pressure in mb or hPa
    Output: potential temperature in Kelvin.

    """
    theta = T*(ps/p)**kappa
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
    T = theta/((ps/p)**kappa)
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
    Tcel = T - tempCtoK
    es = 6.112 * exp(17.67*Tcel/(Tcel+243.5))
    return es

def qsat(T,p):
    """Compute saturation specific humidity as function of temperature and pressure.

    Input:  T is temperature in Kelvin
            p is pressure in hPa or mb
    Output: saturation specific humidity (dimensionless).

    """
    es = clausius_clapeyron(T)
    q = eps * es / (p - (1 - eps) * es )
    return q

def virtual_temperature_from_mixing_ratio(T,w):
    '''Virtual temperature Tv
    T is air temperature (K)
    w is water vapor mixing ratio (dimensionless)'''
    return T * ((1+w/eps)/(1+w))

def vapor_pressure_from_specific_humidity(p,q):
    '''Vapor pressure (same units as input p)
    p is total air pressure
    q is specific humidity (dimensionless) -- mass of vapor per unit mass moist air'''
    return p * (q/(eps+q*(1-eps)))

def mixing_ratio_from_vapor_pressure(p,e):
    '''Water vapor mixing ratio
    p is air pressure
    e is vapor pressure
    p and e must be in same units (e.g. hPa)
    '''
    return eps * e / (p-e)

def rho_moist(T,p,q):
    '''Density of moist air.
    T is air temperature (K)
    p is air pressure (hPa)
    q is specific humidity (hPa)

    returns density in kg/m3
    '''
    e = vapor_pressure_from_specific_humidity(p,q)
    w = mixing_ratio_from_vapor_pressure(p,e)
    Tv = virtual_temperature_from_mixing_ratio(T,w)
    return p*100./ Rd/Tv

def pseudoadiabat(T,p):
    """Compute the local slope of the pseudoadiabat at given temperature and pressure

    Inputs:   p is pressure in hPa or mb
              T is local temperature in Kelvin
    Output:   dT/dp, the rate of temperature change for pseudoadiabatic ascent
                in units of K / hPa

    The pseudoadiabat describes changes in temperature and pressure for an air
    parcel at saturation assuming instantaneous rain-out of the super-saturated water

    Formula consistent with eq. (2.33) from Raymond Pierrehumbert, "Principles of Planetary Climate"
    which nominally accounts for non-dilute effects, but computes the derivative
    dT/dpa, where pa is the partial pressure of the non-condensible gas.

    Integrating the result dT/dp treating p as total pressure effectively makes the dilute assumption.
    """
    esoverp = clausius_clapeyron(T) / p
    Tcel = T - tempCtoK
    L = (2.501 - 0.00237 * Tcel) * 1.E6   # Accurate form of latent heat of vaporization in J/kg
    ratio = L / T /  Rv
    dTdp = (T / p * kappa * (1 + esoverp * ratio) /
        (1 + kappa * (cpv / Rv + (ratio-1) * ratio) * esoverp))
    return dTdp

def lifting_condensation_level(T, RH):
    '''Compute the Lifiting Condensation Level (LCL) for a given temperature and relative humidity

    Inputs:  T is temperature in Kelvin
            RH is relative humidity (dimensionless)

    Output: LCL in meters

    This is height (relative to parcel height) at which the parcel would become saturated during adiabatic ascent.

    Based on approximate formula from Bolton (1980 MWR) as given by Romps (2017 JAS)

    For an exact formula see Romps (2017 JAS), doi:10.1175/JAS-D-17-0102.1
    '''
    Tadj = T-55.  # in Kelvin
    return cp/g*(Tadj - (1/Tadj - log(RH)/2840.)**(-1))

def estimated_inversion_strength(T0,T700):
    '''Compute the Estimated Inversion Strength or EIS,
    following Wood and Bretherton (2006, J. Climate)

    Inputs: T0 is surface temp in Kelvin
           T700 is air temperature at 700 hPa in Kelvin

    Output: EIS in Kelvin

    EIS is a normalized measure of lower tropospheric stability acccounting for
    temperature-dependence of the moist adiabat.
    '''
    # Interpolate to 850 hPa
    T850 = (T0+T700)/2.;
    # Assume 80% relative humidity to compute LCL, appropriate for marine boundary layer
    LCL = lifting_condensation_level(T0, 0.8)
    # Lower Tropospheric Stability (theta700 - theta0)
    LTS = potential_temperature(T700, 700) - T0
    #  Gammam  = -dtheta/dz is the rate of potential temperature decrease along the moist adiabat
    #  in K / m
    Gammam = (g/cp*(1.0 - (1.0 + Lhvap*qsat(T850,850) / Rd / T850) /
             (1.0 + Lhvap**2 * qsat(T850,850)/ cp/Rv/T850**2)))
    #  Assume exponential decrease of pressure with scale height given by surface temperature
    z700 = (Rd*T0/g)*log(1000./700.)
    return LTS - Gammam*(z700 - LCL)

def EIS(T0,T700):
    '''Convenience method, identical to thermo.estimated_inversion_strength(T0,T700)'''
    return estimated_inversion_strength(T0,T700)

def blackbody_emission(T):
    '''Blackbody radiation following the Stefan-Boltzmann law.'''
    return sigma * T**4

def Planck_frequency(nu, T):
    '''The Planck function B(nu,T):
    the flux density for blackbody radiation in frequency space
    nu is frequency in 1/s
    T is temperature in Kelvin

    Formula (3.1) from Raymond Pierrehumbert, "Principles of Planetary Climate"
    '''
    return 2*hPlanck*nu**3/c_light**2/(exp(hPlanck*nu/kBoltzmann/T)-1)

def Planck_wavenumber(n, T):
    '''The Planck function (flux density for blackbody radition)
    in wavenumber space
    n is wavenumber in 1/cm
    T is temperature in Kelvin

    Formula from Raymond Pierrehumbert, "Principles of Planetary Climate", page 140.
    '''
    # convert to mks units
    n = n*100.
    return c_light * Planck_frequency(n*c_light, T)

def Planck_wavelength(l, T):
    '''The Planck function (flux density for blackbody radiation)
    in wavelength space
    l is wavelength in meters
    T is temperature in Kelvin

    Formula (3.3) from Raymond Pierrehumbert, "Principles of Planetary Climate"
    '''
    u = hPlanck*c_light/l/kBoltzmann/T
    return 2*kBoltzmann**5*T**5/hPlanck**4/c_light**3*u**5/(exp(u)-1)

def vmr_to_mmr(vmr, gas):
    '''
    Convert volume mixing ratio to mass mixing ratio for named gas.
    ( molecular weights are specific in climlab.utils.constants.py )
    '''
    return vmr * molecular_weight[gas] / molecular_weight['dry air']

def mmr_to_vmr(mmr, gas):
    '''
    Convert mass mixing ratio to volume mixing ratio for named gas.
    ( molecular weights are specific in climlab.utils.constants.py )
    '''
    return mmr * molecular_weight['dry air'] / molecular_weight[gas]
