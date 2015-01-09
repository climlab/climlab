# Trying out some ideas about using netCDF for data model

import numpy as np
import netCDF4 as nc
from climlab import constants as const

def make_ncColumn(num_levels=30):
    rootgrp = nc.Dataset('test.nc', mode='w', diskless=True)
    #  note the 'diskless' flag means create the dataset in memory only, don't write to disk
    # will need to think about how to handle i/o in a sensible way.    
    
    # netCDF4 format allows for groups (and subgroups) of variables
    # useful here... keep state variables and diagnostic quantities seperate
    state = rootgrp.createGroup('state')
    diagnostics = rootgrp.createGroup('diagnostics')
    params = rootgrp.createGroup('params')
    
    lev = rootgrp.createDimension('lev', num_levels)
    levbounds = rootgrp.createDimension('lev_bounds', num_levels+1)
    #lat = rootgrp.createDimension('lat', 73)
    #lon = rootgrp.createDimension('lon', 144)
    
    #  setting size to 0 or None creates an unlimited dimension
    # time = rootgrp.createDimension('time', None)
    # lat = rootgrp.createDimension('lat', None)
    # lon = rootgrp.createDimension('lon', None)
    
    # a scalar (no dimensions)
    params.createVariable('Q','float')
    params.variables['Q'].assignValue(341.5)
    #  or this way, same thing
    params.variables['Q'][:] = 341.5
    
    # coordinate variable
    levels = rootgrp.createVariable('level','float',('lev',))
    lev_bounds = rootgrp.createVariable('lev_bounds','float',('lev_bounds',))
    dp = const.ps / num_levels
    levels[:] = np.linspace(const.ps - dp/2, dp/2, num_levels)
    lev_bounds[:] = np.concatenate(([const.ps], (levels[0:num_levels-1] +
                                          levels[1:num_levels])/2, [0.]))

    # scalar variable
    Ts = state.createVariable('Ts','float')
    # vector variable
    Tatm = state.createVariable('Tatm','float',('lev',))
    
    # initialize them
    Ts[:] = 288.
    Tatm[:] = np.linspace(Ts[:]-10., 200., num_levels)

    return rootgrp