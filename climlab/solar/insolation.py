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


def daily_insolation(lat, day, orb=const.orb_present, S0=const.S0, day_type=1):
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
    # Inputs can be scalar, numpy vector, or xarray.DataArray.
    #  If numpy, convert to xarray so that it will broadcast correctly
    lat_is_xarray = True
    day_is_xarray = True
    if type(lat) is np.ndarray:
        lat_is_xarray = False
        lat = xr.DataArray(lat, coords=[lat], dims=['lat'])
    if type(day) is np.ndarray:
        day_is_xarray = False
        day = xr.DataArray(day, coords=[day], dims=['day'])
    ecc = orb['ecc']
    long_peri = orb['long_peri']
    obliquity = orb['obliquity']
    # Convert precession angle and latitude to radians
    phi = deg2rad( lat )

    # lambda_long (solar longitude) is the angular distance along Earth's orbit measured from spring equinox (21 March)
    if day_type==1: # calendar days
        lambda_long = solar_longitude(day,orb)
    elif day_type==2: #solar longitude (1-360) is specified in input, no need to convert days to longitude
        lambda_long = deg2rad(day)
    else:
        raise ValueError('Invalid day_type.')

    # Compute declination angle of the sun
    delta = arcsin(sin(deg2rad(obliquity)) * sin(lambda_long))

    #  suppress warning message generated by arccos here!
    oldsettings = np.seterr(invalid='ignore')
    # Compute Ho, the hour angle at sunrise / sunset
    #  Check for no sunrise or no sunset: Berger 1978 eqn (8),(9)
    Ho = xr.where( abs(delta)-pi/2+abs(phi) < 0., # there is sunset/sunrise
              arccos(-tan(phi)*tan(delta)),
              # otherwise figure out if it's all night or all day
              xr.where(phi*delta>0., pi, 0.) )
    # this is not really the daily average cosine of the zenith angle...
    #  it's the integral from sunrise to sunset of that quantity...
    coszen = Ho*sin(phi)*sin(delta) + cos(phi)*cos(delta)*sin(Ho)
    # Compute insolation: Berger 1978 eq (10)
    Fsw = S0/pi*( (1+ecc*cos(lambda_long -deg2rad(long_peri)))**2 / (1-ecc**2)**2 * coszen)
    if not (lat_is_xarray or day_is_xarray):
        # Dimensional ordering consistent with previous numpy code
        return Fsw.transpose().values
    else:
        return Fsw


def solar_longitude( day, orb=const.orb_present, days_per_year = None ):
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
    :returns:                   solar longitude ``lambda_long``
                                in dimension``( day.size, ecc.size )``
    :rtype:                     array

    Works for both scalar and vector orbital parameters.



    """
    if days_per_year is None:
        days_per_year = const.days_per_year
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
    return lambda_long
