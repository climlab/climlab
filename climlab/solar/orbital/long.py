from __future__ import division, print_function
import numpy as np
import pandas as pd
import xarray as xr

def _get_Laskar_data():
    base_url = 'http://vo.imcce.fr/insola/earth/online/earth/La2004/'
    past_file = 'INSOLN.LA2004.BTL.ASC'
    future_file = 'INSOLP.LA2004.BTL.ASC'
    print('Accessing Laskar et al. (2004) orbital data from ' + base_url)
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

OrbitalTable = _get_Laskar_data()
