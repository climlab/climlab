from __future__ import division, print_function
import numpy as np
import os, pooch
import pandas as pd
import xarray as xr
from climlab.utils import _datapath_http


#  Possible sources for the Berger and Loutre 1991 data table
#NCDCpath = "https://www1.ncdc.noaa.gov/pub/data/paleo/climate_forcing/orbital_variations/insolation/orbit91"
path = _datapath_http + 'orbital/orbit91'

def _get_Berger_data(verbose=True):
    '''Read in the Berger and Loutre orbital table as a pandas dataframe, convert to xarray
    '''
    orbit91handle = pooch.retrieve(path,
        known_hash="3afc20dda7b385bdd366bc4c9cf60be02d8defdb4c0f317430ca8386d62f81a3")
    orbit91_pd = pd.read_csv(orbit91handle, delim_whitespace=True, skiprows=1, verbose=True)
    #  As xarray structure with the dimension named 'kyear'
    orbit = xr.Dataset(orbit91_pd).rename({'dim_0': 'kyear'})
    #  Now change names
    orbit = orbit.rename({'ECC': 'ecc', 'OMEGA': 'long_peri',
                          'OBL': 'obliquity', 'PREC': 'precession'})
    # add 180 degrees to long_peri (see lambda definition, Berger 1978 Appendix)
    orbit['long_peri'] += 180.
    # apply np.unwrap to remove discontinuities in the longitude of perihelion
    orbit['long_peri'] = np.rad2deg(xr.apply_ufunc(np.unwrap, np.deg2rad(orbit.long_peri)))
    orbit['precession'] *= -1.

    orbit.attrs['Description'] = 'The Berger and Loutre (1991) orbital data table'
    orbit.attrs['Citation'] = 'https://doi.org/10.1016/0277-3791(91)90033-Q'
    orbit.attrs['Source'] = path
    orbit.attrs['Note'] = 'Longitude of perihelion is defined to be 0 degrees at Northern Vernal Equinox. This differs by 180 degrees from orbit91 source file.'
    return orbit
