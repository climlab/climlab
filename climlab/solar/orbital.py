"""This module defines the class :class:`OrbitalTable` which holds orbital data,
and includes a method :func:`~OrbitalTable.lookup_parameters`
which interpolates the orbital data for a specific year
(- works equally well for arrays of years).

The base class :class:`OrbitalTable()` is designed to work with 5 Myears of orbital data
(**eccentricity, obliquity, and longitude of perihelion**) from :cite:`Berger_1991`.

Data will be read from the file orbit91, which was originally obtained from
ftp://ftp.ncdc.noaa.gov/pub/data/paleo/insolation/
If the file isn't found locally, the module will attempt to read it remotely
from the above URL.

A subclass :class:`LongOrbitalTable()` works with La2004 orbital data for
-51 to +21 Myears as calculated by :cite:`Laskar_2004`.
See http://vo.imcce.fr/insola/earth/online/earth/La2004/README.TXT

"""
from __future__ import division, print_function
import numpy as np
import os
import pandas as pd
import xarray as xr


def _get_Berger_data():
    '''Read in the Berger and Loutre orbital table as a pandas dataframe, convert to xarray
    '''
    orbit91_file = 'orbit91'
    orbit91_url = "https://www1.ncdc.noaa.gov/pub/data/paleo/climate_forcing/orbital_variations/insolation/"
    try:
        #  This gives the full path to the data file, assuming it's in the same directory
        local_path = os.path.dirname(__file__)
        path = os.path.join(local_path, orbit91_file)
        # The first column of the data file is used as the row index, and represents kyr from present
        orbit91_pd = pd.read_table(path, delim_whitespace=True, skiprows=1)
        print('Loaded Berger and Loutre (1991) orbital parameter data from file ' + path)
    except:
        print('Failed to load orbital data locally, trying to access it from NCDC archive.')
        try:
            path = os.path.join(orbit91_url, orbit91_file)
            orbit91_pd = pd.read_table(path, delim_whitespace=True, skiprows=1)
            print('Accessing Berger and Loutre (1991) orbital data from ' + base_url)
            print('Reading file ' + past_file)
        except:
            raise Exception('Failed to load the orbital data.')
    #  As xarray structure with the dimension named 'kyear'
    orbit = xr.Dataset(orbit91_pd).rename({'dim_0': 'kyear'})
    #  Now change names
    orbit = orbit.rename({'ECC': 'ecc', 'OMEGA': 'long_peri',
                          'OBL': 'obliquity', 'PREC': 'precession'})
    # add 180 degrees to long_peri (see lambda definition, Berger 1978 Appendix)
    orbit['long_peri'] += 180.
    orbit['precession'] *= -1.
    return orbit

def _get_Laskar_data():
    base_url = 'http://vo.imcce.fr/insola/earth/online/earth/La2004/'
    past_file = 'INSOLN.LA2004.BTL.ASC'
    future_file = 'INSOLP.LA2004.BTL.ASC'
    print('Attempting to access La2004 orbital data from ' + base_url)
    longorbit = {}
    xlongorbit = {}
    longorbit['past'] = pd.read_table(base_url + past_file,
                            delim_whitespace=True, header=None, index_col=0,
                             names=['kyear','ecc','obliquity','long_peri'])
    longorbit['future'] = pd.read_table(base_url + future_file,
                            delim_whitespace=True, header=None, index_col=0,
                            skiprows=1, # first row is kyear=0, redundant
                            names=['kyear','ecc','obliquity','long_peri'])
    for time in ['past', 'future']:
        # Cannot convert to float until we replace the D notation with E for floating point numbers
        longorbit[time].replace(to_replace='D', value='E', regex=True, inplace=True)
        xlongorbit[time] = xr.Dataset()
        xlongorbit[time]['ecc'] = xr.DataArray(pd.to_numeric(longorbit[time]['ecc']))
        for field in ['obliquity', 'long_peri']:
            xlongorbit[time][field] = xr.DataArray(np.rad2deg(pd.to_numeric(longorbit[time][field])))
    longorbit = xr.concat([xlongorbit['past'], xlongorbit['future']], dim='kyear')
    # add 180 degrees to long_peri (see lambda definition, Berger 1978 Appendix)
    longorbit['long_peri'] += 180.
    longorbit['precession'] = longorbit.ecc*np.sin(np.deg2rad(longorbit.long_peri))
    return longorbit

OrbitalTable = _get_Berger_data()
LongOrbitalTable = _get_Laskar_data()
