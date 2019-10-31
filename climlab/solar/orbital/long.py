from __future__ import division, print_function
import numpy as np
import os
import pandas as pd
import xarray as xr
from climlab.utils.data_source import load_data_source


base_url = 'http://vo.imcce.fr/insola/earth/online/earth/La2004/'
filenames = {'past': 'INSOLN.LA2004.BTL.ASC',
             'future': 'INSOLP.LA2004.BTL.ASC'}

def _get_Laskar_data(verbose=True):
    longorbit = {}
    sources = {}
    pandas_kwargs = {'delim_whitespace':True,
                     'header':None,
                     'index_col':0,
                     'names':['kyear','ecc','obliquity','long_peri'],}
    for time in filenames:
        local_path = os.path.join(os.path.dirname(__file__), "data", filenames[time])
        remote_path = base_url + filenames[time]
        if time is 'future':
            pandas_kwargs['skiprows'] = 1 # first row is kyear=0, redundant
        longorbit[time], path = load_data_source(local_path=local_path,
                remote_source_list=[remote_path],
                open_method = pd.read_csv,
                open_method_kwargs=pandas_kwargs,
                verbose=verbose)
        sources[time] = path
    xlongorbit = {}
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
    longorbit.attrs['Description'] = 'The Laskar et al. (2004) orbital data table'
    longorbit.attrs['Citation'] = 'https://doi.org/10.1051/0004-6361:20041335'
    longorbit.attrs['Source'] = [sources[time] for time in sources]
    longorbit.attrs['Note'] = 'Longitude of perihelion is defined to be 0 degrees at Northern Vernal Equinox. This differs by 180 degrees from the source files.'
    return longorbit

OrbitalTable = _get_Laskar_data()
