"""orbital.py
Code developed by Brian Rose, University at Albany
brose@albany.edu
in support of the class ATM/ENV 415: Climate Laboratory

This module contains orbital data tables for the past 5 Myr, and some 
general-purpose routines for computing incoming solar radiation.

The function orbital.lookup_parameters(t) returns a list (ecc,long_peri,obliquity) 
corresponding to eccentricity, solar longitude of perihelion relative to vernal equinox, 
and obliquiy angle 
at time t = kyrs before present

Cubic spline coefficients tck_ecc, tck_long_peri, tck_obliquity are computed once upon 
importing this module and are then available for subsequent lookup calls.

The function daily_insolation() computes daily average insolation at any time of
year and location given orbital parameters.
"""

import numpy as np
from scipy import interpolate
import os

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
      2362-2367.
      """
    
    #  values are computed by evaluating the spline fits to the data tables
    ecc = interpolate.splev(kyear,tck_ecc,der=0)
    long_peri = interpolate.splev(kyear,tck_long_peri,der=0)
    obliquity = interpolate.splev(kyear,tck_obliquity,der=0)
    
    orb = {'ecc':ecc, 'long_peri':long_peri, 'obliquity':obliquity}
    
    return orb
    #return ecc,long_peri,obliquity
#  End of function lookup_parameters


#  Orbital parameters are read from the accompanying file
#     'BergerLoutre91_orbital.csv'
#  Column 0 is kyr from present (negative because backward in time)
#  Column 1 is eccentricity (dimensionless)
#  Column 2 is longitude of perihelion (degrees)
#  Column 3 is obliquity angle (degrees)
#  The data are loaded here into a numpy.array object called orbital_table

# NOTE: this is currently a bit of a mess.
#  I have a module to read the Berger and Loutre data
#  from NCDC ftp repository
#  as well as La2004 orbital data
# and there should be a switch to choose data source.
#  Also, I think that the two data sources are in different units
#
#  TO DO:  implement a class for holding orbital data,
#  along with methods for interpolating to specific years
#  with that, some methods for accessing various data sources.
#  Divorce this stuff from the actual insolation code, 
#  which should be in its own module file and completely
#  independent of the orbital data stuff.

filename = 'BergerLoutre91_orbital.csv'
#  This gives the full path to the data file, assuming it's in the same directory
fullfilename = os.path.join(os.path.dirname(__file__), filename)
print 'Loading orbital parameter data from file ' + fullfilename
orbital_table = np.fromfile(fullfilename, dtype=float, sep=',')
orbital_table = np.reshape(orbital_table, (5001,4))

#  No longer including the table of values here in the module file
#orbital_table = np.array([[0,0.017236,101.37,23.446],


#  Here we fit a cubic spline to the orbital data
#  which is used in the function orbital.lookup_parameters to actually evaluate the parameters at a given time
#   Fit the spline once here rather than at each instance of orbital.lookup_parameters

kyear0 = -orbital_table[:,0] # kyears before present for data (kyear0>=0);
ecc0 = orbital_table[:,1] # eccentricity
# add 180 degrees to long_peri (see lambda definition, Berger 1978 Appendix)
long_peri0rad = np.deg2rad(orbital_table[:,2] + 180.) # longitude of perihelion (precession angle)
long_peri0 = np.rad2deg( np.unwrap( long_peri0rad ) ) # remove discontinuities (360 degree jumps)
obliquity0 = orbital_table[:,3] # obliquity angle
# compute cubic spline fits to the data
tck_ecc = interpolate.splrep( kyear0, ecc0, s=0 )
tck_long_peri = interpolate.splrep( kyear0, long_peri0, s=0 )
tck_obliquity = interpolate.splrep( kyear0, obliquity0, s=0 )
