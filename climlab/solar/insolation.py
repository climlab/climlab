"""This module contains general-purpose routines for computing
daily-average incoming solar radiation at the top of the atmosphere.

    :Example:
        Compute the timeseries of insolation at 65N at summer
        solstice over the past 5 Myears::

            import numpy as np
            from climlab.solar.orbital import OrbitalTable
            from climlab.solar.insolation import daily_insolation

            # array with specified kyears (can be plain numpy or xarray.DataArray)
            years = np.linspace(-5000, 0, 5001)

            # subset of orbital parameters for specified time
            orb = OrbitalTable.interp(kyear=years)

            # insolation values for past 5 Myears at 65N at summer solstice (day 172)
            S65 = daily_insolation(lat=65, day=172, orb=orb)
            # returns an xarray.DataArray object with insolation values in W/m2

.. note::

    Ported and modified from MATLAB code daily_insolation.m         \n
    *Original authors:*                                             \n

     Ian Eisenman and Peter Huybers, Harvard University, August 2006

    Available online at http://eisenman.ucsd.edu/code/daily_insolation.m

If using calendar days, solar longitude is found using an
approximate solution to the differential equation representing conservation
of angular momentum (Kepler's Second Law).  Given the orbital parameters
and solar longitude, daily average insolation is calculated exactly
following :cite:`Berger_1978`. Further references: :cite:`Berger_1991`.

"""

from __future__ import division
import numpy as np
from climlab import constants as const
from numpy import sqrt, deg2rad, rad2deg, sin, cos, tan, arcsin, arccos, pi
import xarray as xr

#  suppress warning message generated by arccos here!
oldsettings = np.seterr(invalid='ignore')


def _compute_solar_angles(lat, day, orb, lon=None, day_type=1, days_per_year=const.days_per_year):
    phi, day, ecc, long_peri, obliquity, input_is_xarray, lam = _standardize_inputs(lat, day, orb, lon)
    if day_type==1: # calendar days
        lambda_long = deg2rad(solar_longitude(day, orb, days_per_year))
    elif day_type==2: #solar longitude (1-360) is specified in input, no need to convert days to longitude
        lambda_long = deg2rad(day)
    else:
        raise ValueError('Invalid day_type.')
    delta = declination_angle(obliquity, lambda_long)
    rho = _solar_distance_Berger(ecc, lambda_long, long_peri)
    irradiance_factor = rho**(-2)
    if lon is not None:
        # np.fmod(day, 1.0) finds the "fractional" time of day with a range of [0,1)
        # where 0 is midnight, and 0.9999... is 23:59. lon/360 converts longitude 
        # to time since moving along the longitude axis produces the same effect as
        # changing time while keeping longitude the same. the fractional time and
        # fractional longitude are added together since they now both represent 
        # longitude/time of day. This lets us calculate the local solar time (in 
        # "fractional" units) and then convert to hour angle. The -0.5 is included
        # in order to assert that noon occurs when the sun is overhead (so h=0 at
        # LST=0.5 aka time=noon).
        LST = np.fmod((np.fmod(day, 1.0) + (lam/(2*pi))), 1.0)
        # hour angle in rad
        h = (LST - 0.5) * 2*pi
    else:
        h = None
    return phi, delta, irradiance_factor, h, input_is_xarray


def daily_something(lat, day, orb=const.orb_present, S0=const.S0, 
                     day_type=1, days_per_year=const.days_per_year,):
    phi, delta, irradiance_factor, h, input_is_xarray = _compute_solar_angles(lat, day, orb, day_type=day_type, days_per_year=days_per_year)
    # This is the cosine of the solar zenith angle averaged over 24 hours:
    coszen = coszen_daily_time_weighted(phi, delta)
    if not input_is_xarray:
        coszen = coszen.transpose().values
        irradiance_factor = irradiance_factor.tranpose().values
    return coszen, irradiance_factor


def instant_something(lat, day, lon=0., orb=const.orb_present, S0=const.S0, 
                        days_per_year=const.days_per_year):
    phi, delta, irradiance_factor, h, input_is_xarray = _compute_solar_angles(lat, day, orb, lon=lon,
                                                                        days_per_year=days_per_year)
    coszen = coszen_instantaneous(phi, delta, h)
    if not input_is_xarray:
        coszen = coszen.transpose().values
        irradiance_factor = irradiance_factor.tranpose().values
    return coszen, irradiance_factor


def daily_insolation(lat, day, orb=const.orb_present, S0=const.S0, 
                     day_type=1, days_per_year=const.days_per_year,
                     ):
    """Compute daily average insolation given latitude, time of year and orbital parameters.

    Orbital parameters can be interpolated to any time in the last 5 Myears with
    ``climlab.solar.orbital.OrbitalTable`` (see example above).

    Longer orbital tables are available with ``climlab.solar.orbital.LongOrbitalTable``

    Inputs can be scalar, ``numpy.ndarray``, or ``xarray.DataArray``.

    The return value will be ``numpy.ndarray`` if **all** the inputs are ``numpy``.
    Otherwise ``xarray.DataArray``.

    **Function-call argument** \n

    :param array lat:       Latitude in degrees (-90 to 90).
    :param array day:       Indicator of time of year. See argument ``day_type``
                            for details about format.
    :param dict orb:        a dictionary with three members (as provided by
                            ``climlab.solar.orbital.OrbitalTable``)

                            * ``'ecc'`` - eccentricity

                                * unit: dimensionless
                                * default value: ``0.017236``

                            * ``'long_peri'`` - longitude of perihelion (precession angle)

                                * unit: degrees
                                * default value: ``281.37``

                            * ``'obliquity'`` - obliquity angle

                                * unit: degrees
                                * default value: ``23.446``

    :param float S0:        solar constant                                  \n
                            - unit: :math:`\\textrm{W}/\\textrm{m}^2`       \n
                            - default value: ``1365.2``
    :param int day_type:    Convention for specifying time of year (+/- 1,2) [optional].

                            *day_type=1* (default):
                             day input is calendar day (1-365.24), where day 1
                             is January first. The calendar is referenced to the
                             vernal equinox which always occurs at day 80.

                            *day_type=2:*
                             day input is solar longitude (0-360 degrees). Solar
                             longitude is the angle of the Earth's orbit measured from spring
                             equinox (21 March). Note that calendar days and solar longitude are
                             not linearly related because, by Kepler's Second Law, Earth's
                             angular velocity varies according to its distance from the sun.
    :raises: :exc:`ValueError`
                            if day_type is neither 1 nor 2
    :returns:               Daily average solar radiation in unit
                            :math:`\\textrm{W}/\\textrm{m}^2`.

                            Dimensions of output are ``(lat.size, day.size, ecc.size)``
    :rtype:                 array


    Code is fully vectorized to handle array input for all arguments.       \n
    Orbital arguments should all have the same sizes.
    This is automatic if computed from
    :func:`~climlab.solar.orbital.OrbitalTable.lookup_parameters`

        For more information about computation of solar insolation see the
        :ref:`Tutorial` chapter.

    """
    phi, delta, irradiance_factor, h, input_is_xarray = _compute_solar_angles(lat, day, orb, day_type=day_type, days_per_year=days_per_year)
    # This is the cosine of the solar zenith angle averaged over 24 hours:
    coszen = coszen_daily_time_weighted(phi, delta)
    # Compute daily average insolation
    Fsw = _compute_insolation(S0, irradiance_factor, coszen)
    if input_is_xarray:
        return Fsw
    else:
        # Dimensional ordering consistent with previous numpy code
        return Fsw.transpose().values

def _solar_distance_Berger(ecc, lambda_long, long_peri):
    """Earth-Sun distance relative to its reference value at which the solar constant is measured
    See Berger (JAS 1978), unnumbered equation on page 2367.
    
    Inputs:
    - ecc: eccentricity (dimensionless)
    - lambda_long: solar longitude angle (radians)
    - long_peri: longitude of perihelion (radians)
    """
    return (1-ecc**2) / (1 + ecc*cos(lambda_long - long_peri))

def _compute_insolation(S0, irradiance_factor, coszen):
    return S0 * irradiance_factor * coszen


def instant_insolation(lat, day, lon=0., orb=const.orb_present, S0=const.S0, 
                        days_per_year=const.days_per_year):
    """Compute instantaneous insolation given latitude, longitude, time of year and orbital parameters.

    Orbital parameters can be interpolated to any time in the last 5 Myears with
    ``climlab.solar.orbital.OrbitalTable`` (see example above).

    Longer orbital tables are available with ``climlab.solar.orbital.LongOrbitalTable``

    Inputs can be scalar, ``numpy.ndarray``, or ``xarray.DataArray``.

    The return value will be ``numpy.ndarray`` if **all** the inputs are ``numpy``.
    Otherwise ``xarray.DataArray``.

    **Function-call argument** \n

    :param array lat:       Latitude in degrees (-90 to 90).
    :param array day:       Indicator of time of year. Format is calendar day (1-365.24), where day 1
                             is January first. The calendar is referenced to the
                             vernal equinox which always occurs at day 80.
    :param array lon:       Longitude in degrees (0 to 360), optional. Defaults to zero.                            
    :param dict orb:        a dictionary with three members (as provided by
                            ``climlab.solar.orbital.OrbitalTable``)

                            * ``'ecc'`` - eccentricity

                                * unit: dimensionless
                                * default value: ``0.017236``

                            * ``'long_peri'`` - longitude of perihelion (precession angle)

                                * unit: degrees
                                * default value: ``281.37``

                            * ``'obliquity'`` - obliquity angle

                                * unit: degrees
                                * default value: ``23.446``

    :param float S0:        solar constant                                  \n
                            - unit: :math:`\\textrm{W}/\\textrm{m}^2`       \n
                            - default value: ``1365.2``
    :returns:               Daily average solar radiation in unit
                            :math:`\\textrm{W}/\\textrm{m}^2`.

                            Dimensions of output are ``(lat.size, day.size, ecc.size)``
    :rtype:                 array


    Code is fully vectorized to handle array input for all arguments.       \n
    Orbital arguments should all have the same sizes.
    This is automatic if computed from
    :func:`~climlab.solar.orbital.OrbitalTable.lookup_parameters`

        For more information about computation of solar insolation see the
        :ref:`Tutorial` chapter.

     """
    phi, delta, irradiance_factor, h, input_is_xarray = _compute_solar_angles(lat, day, orb, lon=lon, days_per_year=days_per_year)
    # instantaneous cosine of solar zenith angle
    coszen = coszen_instantaneous(phi, delta, h)
    # Compute insolation
    Fsw = _compute_insolation(S0, irradiance_factor, coszen)
    if input_is_xarray:
        return Fsw
    else:
        # Dimensional ordering consistent with previous numpy code
        return Fsw.transpose().values

def declination_angle(obliquity, lambda_long):
    """Compute solar declination angle in radians.

    Inputs:
    - obliquity: obliquity angle in radians
    - lambda_long: solar longitude angle in radians
    """
    return arcsin(sin(obliquity) * sin(lambda_long))

def hour_angle_at_sunset(phi, delta):
    """Compute the hour angle (in radians) at sunset.
    Formulas based on Berger (1978) eqns (8), (9).
    
    Inputs:
    - phi: latitude in radians
    - delta: solar declination angle in radians
    """
    return xr.where( abs(delta)-pi/2+abs(phi) < 0., # there is sunset/sunrise
              arccos(-tan(phi)*tan(delta)),
              # otherwise figure out if it's all night or all day
              xr.where(phi*delta>0., pi, 0.) )

def coszen_instantaneous(phi, delta, h):
    """Cosine of solar zenith angle (instantaneous)
    Returns zero if the sun is below the horizon.
    
    Inputs:
    - phi: latitude in radians
    - delta: solar declination angle in radians
    - h: hour angle in radians
    """
    coszen = (sin(phi)*sin(delta) + cos(phi)*cos(delta)*cos(h))
    return np.maximum(coszen, 0.0)

def coszen_daily_time_weighted(phi, delta):
    """Cosine of solar zenith angle averaged in time over 24 hours.
    
    Inputs:
    - phi: latitude in radians
    - delta: solar declination angle in radians
    """
    h0 = hour_angle_at_sunset(phi, delta)
    return (h0*sin(phi)*sin(delta) + cos(phi)*cos(delta)*sin(h0)) / pi

def coszen_daily_insolation_weighted(phi, delta):
    """Cosine of solar zenith angle, insolation-weighted daily average.
    
    Inputs:
    - phi: latitude in radians
    - delta: solar declination angle in radians
    """
    h0 = hour_angle_at_sunset(phi, delta)
    denominator = h0*sin(phi)*sin(delta) + cos(phi)*cos(delta)*sin(h0)
    numerator = (h0*(2* sin(phi)**2*sin(delta)**2 + cos(phi)**2*cos(delta)**2) + 
        cos(phi)*cos(delta)*sin(h0)*(4*sin(phi)*sin(delta) + cos(phi)*cos(delta)*cos(h0)))
    return numerator / denominator / 2

def coszen_daily_time_weighted_sunlit(delta, phi):
    """Cosine of solar zenith angle averaged in time sunlit hours only.
    
    Inputs:
    - phi: latitude in radians
    - delta: solar declination angle in radians
    """
    h0 = hour_angle_at_sunset(delta, phi)
    return sin(phi)*sin(delta) + cos(phi)*cos(delta)*sin(h0)/h0

def solar_longitude(day, orb=const.orb_present, days_per_year=const.days_per_year):
    """Estimates solar longitude from calendar day.

    Method is using an approximation from :cite:`Berger_1978` section 3
    (lambda = 0 at spring equinox).

    **Function-call arguments** \n

    :param array day:           Indicator of time of year.
    :param dict orb:            a dictionary with three members (as provided by
                                :class:`~climlab.solar.orbital.OrbitalTable`)

                                * ``'ecc'`` - eccentricity

                                    * unit: dimensionless
                                    * default value: ``0.017236``

                                * ``'long_peri'`` - longitude of perihelion
                                  (precession angle)

                                    * unit: degrees
                                    * default value: ``281.37``

                                * ``'obliquity'`` - obliquity angle

                                    * unit: degrees
                                    * default value: ``23.446``
    :param float days_per_year: number of days in a year (optional)
                                (default: 365.2422)
                                Reads the length of the year from
                                :mod:`~climlab.utils.constants` if available.
    :returns:                   solar longitude ``lambda_long`` in degrees
                                in dimension``( day.size, ecc.size )``
    :rtype:                     array

    Works for both scalar and vector orbital parameters.
    """
    ecc = orb['ecc']
    long_peri_rad = deg2rad( orb['long_peri'])
    delta_lambda = (day - 80.) * 2*pi/days_per_year
    beta = sqrt(1 - ecc**2)
    lambda_long_m = -2*((ecc/2 + (ecc**3)/8 ) * (1+beta) * sin(-long_peri_rad) -
        (ecc**2)/4 * (1/2 + beta) * sin(-2*long_peri_rad) + (ecc**3)/8 *
        (1/3 + beta) * sin(-3*long_peri_rad)) + delta_lambda
    lambda_long = ( lambda_long_m + (2*ecc - (ecc**3)/4)*sin(lambda_long_m - long_peri_rad) +
        (5/4)*(ecc**2) * sin(2*(lambda_long_m - long_peri_rad)) + (13/12)*(ecc**3)
        * sin(3*(lambda_long_m - long_peri_rad)) )
    return rad2deg(lambda_long)


def _standardize_inputs(lat, day, orb, lon=None):
    # Inputs can be scalar, numpy vector, or xarray.DataArray.
    #  If numpy, convert to xarray so that it will broadcast correctly
    lat_is_xarray = True
    day_is_xarray = True
    lon_is_xarray = True

    if type(lat) is np.ndarray:
        lat_is_xarray = False
        lat = xr.DataArray(lat, coords=[lat], dims=['lat'])
    if type(day) is np.ndarray:
        day_is_xarray = False
        day = xr.DataArray(day, coords=[day], dims=['day'])

    ecc = orb['ecc']
    long_peri = deg2rad(orb['long_peri'])
    obliquity = deg2rad(orb['obliquity'])
    # Convert latitude (and all other angles) to radians
    phi = deg2rad(lat)

    input_is_xarray = lat_is_xarray or day_is_xarray

    if lon is not None:
        if type(lon) is np.ndarray:
            lon_is_xarray = False
            lon = xr.DataArray(lon, coords=[lon], dims=['lon'])
        input_is_xarray = input_is_xarray or lon_is_xarray
        lam = deg2rad(lon)
        return phi, day, ecc, long_peri, obliquity, input_is_xarray, lam
    else:
        return phi, day, ecc, long_peri, obliquity, input_is_xarray, lon
