from __future__ import division, print_function
import numpy as np
import os
import pandas as pd
import xarray as xr
from climlab.utils.data_source import load_data_source

#  Two possible sources for the Berger and Loutre 1991 data table
NCDCpath = "https://www1.ncdc.noaa.gov/pub/data/paleo/climate_forcing/orbital_variations/insolation/orbit91"
threddspath = "http://thredds.atmos.albany.edu:8080/thredds/fileServer/CLIMLAB/orbital/orbit91"
#  This gives the full local path to the data file
local_path = os.path.join(os.path.dirname(__file__), "data", "orbit91.dat")

def _get_Berger_data(verbose=True):
    '''Read in the Berger and Loutre orbital table as a pandas dataframe, convert to xarray
    '''
    # The first column of the data file is used as the row index, and represents kyr from present
    orbit91_pd, path = load_data_source(local_path = local_path,
                    remote_source_list = [threddspath, NCDCpath],
                    open_method = pd.read_csv,
                    open_method_kwargs = {'delim_whitespace': True, 'skiprows':1},
                    verbose=verbose,)
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
