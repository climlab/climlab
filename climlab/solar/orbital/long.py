from __future__ import division, print_function
import numpy as np
import os, pooch
import pandas as pd
import xarray as xr


base_url = 'http://vo.imcce.fr/insola/earth/online/earth/La2004/'
filenames = {'past': 'INSOLN.LA2004.BTL.ASC',
             'future': 'INSOLP.LA2004.BTL.ASC'}
hashes = {'past': "3f13b9f8e69085baf40bc67a2669e6f6af4148fef9218ed2158772aa91e35f8c",
          'future': "8e5ac423374802a4ce2a0958672271ed408054cc70214056c0c221c3d5b14750"}

def _get_Laskar_data(verbose=True):
    longorbithandle = {}
    longorbit = {}
    sources = {}
    pandas_kwargs = {'delim_whitespace':True,
                     'header':None,
                     'index_col':0,
                     'names':['kyear','ecc','obliquity','long_peri'],}
    for time in filenames:
        remote_path = base_url + filenames[time]
        if time == 'future':
            pandas_kwargs['skiprows'] = 1 # first row is kyear=0, redundant
        path = remote_path
        longorbithandle[time] = pooch.retrieve(url=path, known_hash=hashes[time])
        longorbit[time] = pd.read_csv(longorbithandle[time], **pandas_kwargs)
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
