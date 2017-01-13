"""This module contains general-purpose routines for computing incoming
solar radiation at the top of the atmosphere.

Currently, only daily average insolation is computed.

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


def daily_insolation(lat, day, orb=const.orb_present, S0=None, day_type=1):
    """Compute daily average insolation given latitude, time of year and orbital parameters.

    Orbital parameters can be computed for any time in the last 5 Myears with
    :func:`~climlab.solar.orbital.OrbitalTable.lookup_parameters` (see example below).


    **Function-call argument** \n

    :param array lat:       Latitude in degrees (-90 to 90).
    :param array day:       Indicator of time of year. See argument ``day_type``
                            for details about format.
    :param dict orb:        a dictionary with three members (as provided by
                            :class:`~climlab.solar.orbital.OrbitalTable`)

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

    :Example:
        to compute the timeseries of insolation at 65N at summer
        solstice over the past 5 Myears::

            from climlab.solar.orbital import OrbitalTable
            from climlab.solar.insolation import daily_insolation

            # import orbital table
            table = OrbitalTable()

            # array with specified kyears
            years = np.linspace(-5000, 0, 5001)

            # orbital parameters for specified time
            orb = table.lookup_parameters( years )

            # insolation values for past 5 Myears at 65N at summer solstice
            S65 = daily_insolation( 65, 172, orb )

        For more information about computation of solar insolation see the
        :ref:`Tutorial` chapter.

     """

    # If input argument S0 is not given, use the standard Earth value
    if S0 is None:
        S0 = const.S0

    # Inputs can be scalar or vector. If scalar, convert to 0d numpy arrays
    lat = np.array( lat )
    day = np.array( day )
    ecc = np.array( orb['ecc'] )
    long_peri = np.array( orb['long_peri'] )
    obliquity = np.array( orb['obliquity'] )

    # Convert precession angle and latitude to radians
    phi = np.deg2rad( lat )

    # lambda_long (solar longitude) is the angular distance along Earth's orbit measured from spring equinox (21 March)
    if day_type==1: # calendar days
        lambda_long = solar_longitude( day, orb )
    elif day_type==2: #solar longitude (1-360) is specified in input, no need to convert days to longitude
        lambda_long = np.tile( np.expand_dims( np.deg2rad( day ), axis=1 ), [1, ecc.size] )
    else:
        raise ValueError('Invalid day_type.')

    # Compute declination angle of the sun
    delta = np.array( np.arcsin( np.sin( np.deg2rad(obliquity) ) * np.sin( lambda_long ) ) )

    # recast all the arrays to dimensions (lat.size, day.size, ecc.size)
    delta_big = np.tile( delta, [ lat.size, 1, 1 ] )
    lambda_long_big = np.tile( lambda_long, [ lat.size, 1, 1 ] )
    phi_day = np.tile( np.expand_dims( phi, axis=1 ), [1, day.size] )
    phi_big = np.tile( np.expand_dims( phi_day, axis=2 ), [1, 1, ecc.size] )

    #  suppress warning message generated by arccos here!
    oldsettings = np.seterr(invalid='ignore')
    # Compute Ho, the hour angle at sunrise / sunset
    #  Check for no sunrise or no sunset: Berger 1978 eqn (8),(9)
    Ho = np.where( abs( delta_big ) - np.math.pi / 2. + abs( phi_big ) < 0.,
                  np.arccos( -np.tan( phi_big ) * np.tan( delta_big ) ),
            np.where( phi_big * delta_big > 0. , np.math.pi, 0. ) )
    # this is not really the daily average cosine of the zenith angle...
    #  it's the integral from sunrise to sunset of that quantity...
    coszen = (Ho*np.sin(phi_big)*np.sin(delta_big) +
              np.cos(phi_big)*np.cos(delta_big)*np.sin(Ho))
    # Compute insolation: Berger 1978 eq (10)
    Fsw = S0/np.math.pi*( (1. + ecc*np.cos(lambda_long_big -
          np.deg2rad(long_peri)))**2 / (1. - ecc**2)**2 * coszen)

    #  Remove singleton dimensions and return
    return np.squeeze( Fsw )


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

    day = np.array(day)
    ecc = np.array(orb['ecc'])
    long_peri_rad = np.deg2rad( np.array(orb['long_peri']) )
    delta_lambda_long_m = ( ( np.tile( np.expand_dims( day, axis=1 ), [1, ecc.size]) - 80. ) * 2. *
        np.math.pi / days_per_year )
    beta = ( 1 - ecc**2 )**(1./2.)
    lambda_long_m0 = ( -2. * ( (1./2. * ecc + 1./8. * ecc**3 ) * (1. + beta) * np.sin(-long_peri_rad) -
        1./4.* (ecc**2) * ( 1./2. + beta ) * np.sin( -2. * long_peri_rad ) + 1./8. * (ecc**3) *
        (1./3. + beta) * ( np.sin(-3. * long_peri_rad) ) ) )
    lambda_long_m = np.tile( lambda_long_m0, [day.size, 1] ) + delta_lambda_long_m
    lambda_long = ( lambda_long_m + ( 2*ecc - 1/4. * (ecc**3)) * np.sin(lambda_long_m - long_peri_rad) +
        (5./4.) * (ecc**2) * np.sin(2 * ( lambda_long_m - long_peri_rad )) + (13./12.) * (ecc**3)
        * np.sin(3*( lambda_long_m - long_peri_rad )) )

    # lambda_long should have dimensions lambda_long.size = ( day.size, ecc.size )
    return lambda_long
