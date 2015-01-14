#  new domain class
#   every process should exist in the context of a domain

import numpy as np
from climlab.domain.axis import Axis
#from climlab.domain.grid import Grid
import climlab.utils.heat_capacity as heat_capacity

# A domain has these properties:
#   - dimensions, axes
#   - some well defined "volume" property
#   - heat capacity

#  Different flavors (sub-classes) of domains include
#    - atmosphere: slab, pressure-varying
#    - ocean: slab, depth-varying
#    - land, ice, etc.... will implement later

# A domain is essentially a grid plus properties

#  get rid of the grid class! it isn't helping.
# just have an explicit list of axes

def make_slabocean_grid(num_points=1):
    '''Convenience method to create a simple grid for a slab ocean.'''
    depthax = Axis(axis_type='depth', num_points=num_points)
    return depthax

def make_slabatm_grid(num_points=1):
    '''Convenience method to create a simple grid for a slab ocean.'''
    depthax = Axis(axis_type='lev', num_points=num_points)
    return depthax


class _Domain(object):
    def __str__(self):
        return ("climlab Domain of type " + self.domain_type + " with " +
                str(self.num_points) + " points.")
    def __init__(self, grid=None, **kwargs):
        self.domain_type = 'undefined'
        # self.grid should be a dictionary of axes
        # make it possible to give just a single axis:
        if type(grid) is dict:
            self.grid = grid
        elif type(grid) is Axis:
            ax = grid
            self.grid = {ax.axis_type: ax}
        else:
            raise ValueError('grid needs to be Axis object or dictionary of Axis object')
        self.numdims = len(self.grid.keys())
        shape = []
        for axType, ax in self.grid.iteritems():
            shape.append(ax.num_points)
        self.shape = tuple(shape)

        self.set_heat_capacity()

    def set_heat_capacity(self):
        self.heat_capacity = None
        #  implemented by daughter classes


class Atmosphere(_Domain):
    def __init__(self, **kwargs):
        super(Atmosphere, self).__init__(**kwargs)
        self.domain_type = 'atm'

    def set_heat_capacity(self):
        self.heat_capacity = heat_capacity.atmosphere(self.grid['lev'].delta)


class Ocean(_Domain):
    def __init__(self, **kwargs):
        super(Ocean, self).__init__(**kwargs)
        self.domain_type = 'ocean'

    def set_heat_capacity(self):
        self.heat_capacity = heat_capacity.ocean(self.grid['depth'].delta)

class SlabOcean(Ocean):
    def __init__(self, grid=make_slabocean_grid(), **kwargs):
        super(SlabOcean, self).__init__(grid=grid, **kwargs)

class SlabAtmosphere(Atmosphere):
    def __init__(self, grid=make_slabatm_grid(), **kwargs):
        super(SlabAtmosphere, self).__init__(grid=grid, **kwargs)
    
def single_column(num_points=30, water_depth=1., **kwargs):
    '''Convenience method to create domains for a single column of atmosphere
    overlying a slab of water.
    
    num_points is the number of pressure levels (evenly spaced from surface to TOA)
    water_depth is the depth of the slab.
    
    Returns two Domain object (slab ocean, atmosphere)
    
    Usage:
    '''
    levax = Axis(axis_type='lev', num_points=num_points)
    depthax = Axis(axis_type='depth', num_points=1)
    return SlabOcean(grid=depthax, **kwargs), Atmosphere(grid=levax, **kwargs)
    
