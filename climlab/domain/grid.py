import numpy as np
from climlab.domain.axis import create_axis
import xray
from climlab.utils import heat_capacity as _heat_capacity

# To replace domain.py, axis,py and field,py in the new xray-based version
#  of climlab
#
#  Just want some convenience functions that create state variable datasets
#  with appropriate grid information

def single_column(num_lev=30, water_depth=1., lev=None, **kwargs):
    '''Convenience method to create domains for a single column of atmosphere
    overlying a slab of water.
    
    num_lev is the number of pressure levels (evenly spaced from surface to TOA)
    water_depth is the depth of the slab.
    
    Returns xray.Dataset object with coordinate arrays lev and depth
    as well as their bounds and deltas
    
    Usage:
    from climlab.domain import grid
    col = grid.single_column()
    print col
    
    Can also pass a pressure array or pressure level axis object
    '''
    if lev is None:
        levax = create_axis(axis_type='lev', num_points=num_lev)
    elif isinstance(lev, xray.Dataset):
        levax = lev
    else:
        try:
            levax = create_axis(axis_type='lev', points=lev)
        except:
            raise ValueError('lev must be a valid coordinate dataset or pressure array')
            
    depthax = create_axis(axis_type='depth', bounds=[water_depth, 0.])
    #  need to implement some sensible way to get heat capacity information

    #  merge the two coordinate datasets
    return levax.update(depthax)


def heat_capacity(grid_data):
    atm = _heat_capacity.atmosphere(grid_data.lev_delta)
    ocn = _heat_capacity.ocean(grid_data.depth_delta)
    return xray.Dataset(variables={'C_atm': atm, 'C_ocn': ocn})
