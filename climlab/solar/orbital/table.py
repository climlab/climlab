from __future__ import division, print_function
import numpy as np
import os
import pandas as pd
import xarray as xr


threddspath = 'http://thredds.atmos.albany.edu:8080/thredds/fileServer/CLIMLAB/orbital/orbit91'

def _get_Berger_data():
    '''Read in the Berger and Loutre orbital table as a pandas dataframe, convert to xarray
    '''
    orbit91_file = 'orbit91'
    orbit91_url = "https://www1.ncdc.noaa.gov/pub/data/paleo/climate_forcing/orbital_variations/insolation/"
    try:
        #  This gives the full path to the data file, assuming it's in the same directory
        local_path = os.path.dirname(__file__)
        #path = os.path.join(local_path, orbit91_file)
        path = threddspath
        # The first column of the data file is used as the row index, and represents kyr from present
        orbit91_pd = pd.read_csv(path, delim_whitespace=True, skiprows=1)
        print('Loaded Berger and Loutre (1991) orbital parameter data from file ' + path)
    except:
        print('Failed to load orbital data locally, trying to access it from NCDC archive.')
        try:
            path = os.path.join(orbit91_url, orbit91_file)
            orbit91_pd = pd.read_csv(path, delim_whitespace=True, skiprows=1)
            print('Accessed Berger and Loutre (1991) orbital data from ' + base_url)
        except:
            raise Exception('Failed to load the Berger and Loutre (1991) orbital data.')
    #  As xarray structure with the dimension named 'kyear'
    orbit = xr.Dataset(orbit91_pd).rename({'dim_0': 'kyear'})
    #  Now change names
    orbit = orbit.rename({'ECC': 'ecc', 'OMEGA': 'long_peri',
                          'OBL': 'obliquity', 'PREC': 'precession'})
    # add 180 degrees to long_peri (see lambda definition, Berger 1978 Appendix)
    orbit['long_peri'] += 180.
    orbit['precession'] *= -1.

    orbit.attrs['Description'] = 'The Berger and Loutre (1991) orbital data table'
    orbit.attrs['Citation'] = 'https://doi.org/10.1016/0277-3791(91)90033-Q'
    orbit.attrs['Source'] = path
    orbit.attrs['Note'] = 'Longitude of perihelion is defined to be 0 degrees at Northern Vernal Equinox. This differs by 180 degrees from orbit91 source file.'
    return orbit
