#  new domain class
#   every process should exist in the context of a domain

from climlab.domain.axis import Axis
from climlab.utils import heat_capacity


class _Domain(object):
    def __str__(self):
        return ("climlab Domain object with domain_type=" + self.domain_type + " and shape=" +
                str(self.shape))
    def __init__(self, axes=None, **kwargs):
        self.domain_type = 'undefined'
        # self.axes should be a dictionary of axes
        # make it possible to give just a single axis:
        self.axes = self._make_axes_dict(axes)
        self.numdims = len(self.axes.keys())
        shape = []
        axcount = 0
        axindex = {}
        #  ordered list of axes
        #  lev OR depth is last
        #  lat is second-last
        add_lev = False
        add_depth = False
        add_lat = False
        axlist = self.axes.keys()
        if 'lev' in axlist:
            axlist.remove('lev')
            add_lev = True
        elif 'depth' in axlist:
            axlist.remove('depth')
            add_depth = True
        if 'lat' in axlist:
            axlist.remove('lat')
            add_lat = True
        axlist2 = axlist[:]
        if add_lat:
            axlist2.append('lat')
        if add_depth:
            axlist2.append('depth')
        if add_lev:
            axlist2.append('lev')
        #for axType, ax in self.axes.iteritems():        
        for axType in axlist2:
            ax = self.axes[axType]
            shape.append(ax.num_points)
            #  can access axes as object attributes
            setattr(self, axType, ax)
            #
            axindex[axType] = axcount
            axcount += 1
        self.axis_index = axindex
        self.shape = tuple(shape)

        self.set_heat_capacity()

    def set_heat_capacity(self):
        self.heat_capacity = None
        #  implemented by daughter classes
    
    def _make_axes_dict(self, axes):
        if type(axes) is dict:
            axdict = axes
        elif type(axes) is Axis:
            ax = axes
            axdict = {ax.axis_type: ax}
        elif axes is None:
            axdict = {'empty': None}
        else:
            raise ValueError('axes needs to be Axis object or dictionary of Axis object')
        return axdict



class Atmosphere(_Domain):
    def __init__(self, **kwargs):
        super(Atmosphere, self).__init__(**kwargs)
        self.domain_type = 'atm'

    def set_heat_capacity(self):
        self.heat_capacity = heat_capacity.atmosphere(self.axes['lev'].delta)


class Ocean(_Domain):
    def __init__(self, **kwargs):
        super(Ocean, self).__init__(**kwargs)
        self.domain_type = 'ocean'

    def set_heat_capacity(self):
        self.heat_capacity = heat_capacity.ocean(self.axes['depth'].delta)


def make_slabocean_axis(num_points=1):
    '''Convenience method to create a simple axis for a slab ocean.'''
    depthax = Axis(axis_type='depth', num_points=num_points)
    return depthax

def make_slabatm_axis(num_points=1):
    '''Convenience method to create a simple axis for a slab atmosphere.'''
    depthax = Axis(axis_type='lev', num_points=num_points)
    return depthax



class SlabOcean(Ocean):
    def __init__(self, axes=make_slabocean_axis(), **kwargs):
        super(SlabOcean, self).__init__(axes=axes, **kwargs)

class SlabAtmosphere(Atmosphere):
    def __init__(self, axes=make_slabatm_axis(), **kwargs):
        super(SlabAtmosphere, self).__init__(axes=axes, **kwargs)
    
    
def single_column(num_lev=30, water_depth=1., lev=None, **kwargs):
    '''Convenience method to create domains for a single column of atmosphere
    overlying a slab of water.
    
    num_lev is the number of pressure levels (evenly spaced from surface to TOA)
    water_depth is the depth of the slab.
    
    Returns a list of 2 Domain objects (slab ocean, atmosphere)
    
    Usage:
    sfc, atm = domain.single_column()
        or
    sfc, atm = domain.single_column(num_lev=2, water_depth=10.)
    print sfc, atm
    
    Can also pass a pressure array or pressure level axis object
    '''
    if lev is None:
        levax = Axis(axis_type='lev', num_points=num_lev)
    elif isinstance(lev, Axis):
        levax = lev
    else:
        try:
            levax = Axis(axis_type='lev', points=lev)
        except:
            raise ValueError('lev must be Axis object or pressure array')
    depthax = Axis(axis_type='depth', bounds=[water_depth, 0.])
    slab = SlabOcean(axes=depthax, **kwargs)
    atm = Atmosphere(axes=levax, **kwargs)
    return slab, atm


def zonal_mean_surface(num_lat=90, water_depth=10., lat=None, **kwargs):
    if lat is None:
        latax = Axis(axis_type='lat', num_points=num_lat)
    elif isinstance(lat, Axis):
        latax = lat
    else:
        try:
            latax = Axis(axis_type='lat', points=lat)
        except:
            raise ValueError('lat must be Axis object or latitude array')
    depthax = Axis(axis_type='depth', bounds=[water_depth, 0.])
    axes = {'depth': depthax, 'lat': latax}
    slab = SlabOcean(axes=axes, **kwargs)
    return slab

def zonal_mean_column(num_lat=90, num_lev=30, water_depth=10., lat=None, 
                      lev=None, **kwargs):
    if lat is None:
        latax = Axis(axis_type='lat', num_points=num_lat)
    elif isinstance(lat, Axis):
        latax = lat
    else:
        try:
            latax = Axis(axis_type='lat', points=lat)
        except:
            raise ValueError('lat must be Axis object or latitude array')
    if lev is None:
        levax = Axis(axis_type='lev', num_points=num_lev)
    elif isinstance(lev, Axis):
        levax = lev
    else:
        try:
            levax = Axis(axis_type='lev', points=lev)
        except:
            raise ValueError('lev must be Axis object or pressure array')

    depthax = Axis(axis_type='depth', bounds=[water_depth, 0.])
    #axes = {'depth': depthax, 'lat': latax, 'lev': levax}
    slab = SlabOcean(axes={'lat':latax, 'depth':depthax}, **kwargs)
    atm = Atmosphere(axes={'lat':latax, 'lev':levax}, **kwargs)
    return slab, atm

def box_model_domain(num_points=2, **kwargs):
    '''Create a box model domain (a single abstract axis).'''
    ax = Axis(axis_type='abstract', num_points=num_points)
    boxes = _Domain(axes=ax, **kwargs)
    boxes.domain_type = 'box'
    return boxes
    
