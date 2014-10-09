"""orbital_extended.py

Modification of orbital.py
Code developed by Brian Rose, University at Albany
brose@albany.edu
in support of the class ENV 480: Climate Laboratory
Spring 2014

added capability to read from extended orbital data tables
from Laskar et al. (La2004)"""

import numpy as np
from scipy import interpolate

def lookup_parameters( kyear = 0 ):
    """Look up orbital parameters for given kyear before present.
    
    Input kyear should be a positive number, thousands of years before present.
    
    Returns three values: 
    ecc = eccentricity (dimensionless)
    long_peri = longitude of perihelion (precession angle) (degrees)
    obliquity = obliquity angle or axial tilt (degrees).
    
    Values for eccentricity, obliquity, and longitude of perihelion for the
    past 5 Myr are taken from Berger and Loutre 1991 (data from
    ncdc.noaa.gov). 

    References: 
    Berger A. and Loutre M.F. (1991). Insolation values for the climate of
      the last 10 million years. Quaternary Science Reviews, 10(4), 297-317.
    Berger A. (1978). Long-term variations of daily insolation and
      Quaternary climatic changes. Journal of Atmospheric Science, 35(12),
      2362-2367."""
    
    #  values are computed by evaluating the spline fits to the data tables
    #ecc = interpolate.splev(kyear,tck_ecc,der=0)
    #long_peri = interpolate.splev(kyear,tck_long_peri,der=0)
    #obliquity = interpolate.splev(kyear,tck_obliquity,der=0)
    #  linear interpolation:
    ecc = f_ecc(-kyear)
    long_peri = f_long_peri(-kyear)
    obliquity = f_obliquity(-kyear)
    
    orb = {'ecc':ecc, 'long_peri':long_peri, 'obliquity':obliquity}
    
    return orb
    #return ecc,long_peri,obliquity
#  End of function lookup_parameters


def daily_insolation(lat, day, ecc = 0.017236, long_peri = 281.37, obliquity = 23.446, S0 = None, day_type=1 ):
    """Compute daily average insolation given latitude, time of year and orbital parameters.
    
    Orbital parameters can be computed for any time in the last 5 Myears with
    ecc,long_peri,obliquity = orbital.lookup_parameters(kyears)
    
    Inputs:
    lat:      Latitude in degrees (-90 to 90).
    day:      Indicator of time of year, by default day 1 is Jan 1.
    ecc:      eccentricity (dimensionless)
    long_peri:    longitude of perihelion (precession angle) (degrees)
    obliquity:  obliquity angle (degrees)
    S0:       Solar constant in W/m^2, will try to read from ClimateUtils.py
    day_type: Convention for specifying time of year (+/- 1,2) [optional].
        day_type=1 (default): day input is calendar day (1-365.24), where day 1 
        is January first.  The calendar is referenced to the vernal equinox 
        which always occurs at day 80.
        day_type=2: day input is solar longitude (0-360 degrees). Solar
        longitude is the angle of the Earth's orbit measured from spring
        equinox (21 March). Note that calendar days and solar longitude are
        not linearly related because, by Kepler's Second Law, Earth's
        angular velocity varies according to its distance from the sun.
    Default values for orbital parameters are present-day
    
    Output:
    Fsw = Daily average solar radiation in W/m^2.
    
    Dimensions of output are (lat.size, day.size, ecc.size)
    
    Code is fully vectorized to handle array input for all arguments.
    Orbital arguments should all have the same sizes. 
    This is automatic if computed from lookup_parameters()
    
    e.g. to compute the timeseries of insolation at 65N at summer solstice over the past 5 Myears
        years = np.linspace(0, 5000, 5001)
        orb = orbital.lookup_parameters( years )
        S65 = orbital.daily_insolation( 65, 172, **orb )
    
    Ported from MATLAB code daily_insolation.m
    Original authors: 
        Ian Eisenman and Peter Huybers, Harvard University, August 2006
    Available online at http://eisenman.ucsd.edu/code/daily_insolation.m
        
    If using calendar days, solar longitude is found using an
    approximate solution to the differential equation representing conservation
    of angular momentum (Kepler's Second Law).  Given the orbital parameters
    and solar longitude, daily average insolation is calculated exactly
    following Berger 1978.

    References: 
    Berger A. and Loutre M.F. (1991). Insolation values for the climate of
     the last 10 million years. Quaternary Science Reviews, 10(4), 297-317.
    Berger A. (1978). Long-term variations of daily insolation and
     Quaternary climatic changes. Journal of Atmospheric Science, 35(12),
     2362-2367."""
    
    #  If not given, try to set solar constant to the value in ClimateUtils.
    #  If that doesn't work, set a realistic default.
    if S0 is None:
        try: from ClimateUtils import S0
        except: S0 = 1365.2
    
    # Inputs can be scalar or vector. If scalar, convert to 0d numpy arrays
    lat = np.array( lat )
    day = np.array( day )
    ecc = np.array( ecc )
    long_peri = np.array( long_peri )
    obliquity = np.array( obliquity )
    
    # Convert precession angle and latitude to radians
    phi = np.deg2rad( lat )
    
    # lambda_long (solar longitude) is the angular distance along Earth's orbit measured from spring equinox (21 March)
    if day_type==1: # calendar days
        lambda_long = solar_longitude( day, ecc=ecc, long_peri=long_peri )
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
    Ho = np.where( abs( delta_big ) - np.math.pi / 2. + abs( phi_big ) < 0., np.arccos( -np.tan( phi_big ) * np.tan( delta_big ) ), 
            np.where( phi_big * delta_big > 0. , np.math.pi, 0. ) )
    # Compute insolation: Berger 1978 eq (10)
    Fsw = S0 / np.math.pi * ( ( 1 + ecc * np.cos( lambda_long_big - np.deg2rad(long_peri) ))**2 / (1 - ecc**2)**2 * 
        ( Ho * np.sin( phi_big ) * np.sin( delta_big ) + np.cos( phi_big ) * np.cos( delta_big ) * np.sin( Ho ) ) )
    
    #  Remove singleton dimensions and return
    return np.squeeze( Fsw )
#  End of function daily_insolation()


def solar_longitude( day, ecc = 0.017236, long_peri = 281.37 ):
    '''Estimate solar longitude (lambda = 0 at spring equinox) from calendar day 
    using an approximation from Berger 1978 section 3.
    
    Works for both scalar and vector orbital parameters.
    
    Reads the length of the year from ClimateUtils.py if available.'''
    
    try: from ClimateUtils import days_per_year
    except: days_per_year = 365.2422
    
    day = np.array(day)
    ecc = np.array(ecc)
    long_peri_rad = np.deg2rad( np.array(long_peri) )
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
#  End of function computeSolarLongitude()


#  This is the table of orbital parameter values, copied from Eisenman and Huybers
#  The data are entered here into a numpy.array object called orbital_table
#  Column 0 is kyr from present (negative because backward in time)
#  Column 1 is eccentricity (dimensionless)
#  Column 2 is longitude of perihelion (degrees)
#  Column 3 is obliquity angle (degrees)

#  Search for =====  to find the bottom of the table
#orbital_table = np.array([[0,0.017236,101.37,23.446],
#	[-5000,0.026851,79.43,22.525]])
# =====   end of table of values

import La2004_orbital

kyear0 = La2004_orbital.data[:,0]  # kyears after present... negative means past!
ecc0 = La2004_orbital.data[:,1] # eccentricity
obliquity0 = np.rad2deg(La2004_orbital.data[:,2])  # obliquity angle, convert to degrees
# add 180 degrees to long_peri (see lambda definition, Berger 1978 Appendix)
long_peri0rad = La2004_orbital.data[:,3] + np.deg2rad(180.) # longitude of perihelion (precession angle)
long_peri0 = np.rad2deg( np.unwrap( long_peri0rad ) ) # remove discontinuities (360 degree jumps)

#  use linear interpolation
f_ecc = interpolate.interp1d(kyear0, ecc0)
f_long_peri = interpolate.interp1d(kyear0, long_peri0)
f_obliquity = interpolate.interp1d(kyear0, obliquity0)

#  Here we fit a cubic spline to the orbital data
#  which is used in the function orbital.lookup_parameters to actually evaluate the parameters at a given time
#   Fit the spline once here rather than at each instance of orbital.lookup_parameters

#kyear0 = -orbital_table[:,0] # kyears before present for data (kyear0>=0);
#ecc0 = orbital_table[:,1] # eccentricity
## add 180 degrees to long_peri (see lambda definition, Berger 1978 Appendix)
#long_peri0rad = np.deg2rad(orbital_table[:,2] + 180.) # longitude of perihelion (precession angle)
#long_peri0 = np.rad2deg( np.unwrap( long_peri0rad ) ) # remove discontinuities (360 degree jumps)
#obliquity0 = orbital_table[:,3] # obliquity angle
# compute cubic spline fits to the data

#tck_ecc = interpolate.splrep( kyear0, ecc0, s=0 )
#tck_long_peri = interpolate.splrep( kyear0, long_peri0, s=0 )
#tck_obliquity = interpolate.splrep( kyear0, obliquity0, s=0 )

