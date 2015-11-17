#  new domain class
#   every process should exist in the context of a domain

#from climlab.domain.axis import Axis
#  This added for experimental new "Domain as xray.Dataset" concept
import xray
#from climlab.domain.axis import create_axis
from climlab.utils import heat_capacity
from climlab import constants as const
import numpy as np

#  A Domain object is actually a xray.Dataset
#  that contains named coordinate axes
#  It can also contain time-indepedent properties of the domain
#  e.g. heat capacity

axis_types = ['lev', 'lat', 'lon', 'depth', 'abstract']


def create_axis(axis_type='abstract', num_points=10, points=None, bounds=None):
    '''Create a new climlab Axis object
    Valid axis types are:
        'lev'
        'lat'
        'lon'
        'depth'
        'abstract' (default)
    '''
    if axis_type in axis_types:
        pass
    elif axis_type in ['p', 'press', 'pressure', 'P', 'Pressure', 'Press']:
        axis_type = 'lev'
    elif axis_type in ['Latitude', 'latitude']:
        axis_type = 'lat'
    elif axis_type in ['Longitude', 'longitude']:
        axis_type = 'lon'
    elif axis_type in ['depth', 'Depth', 'waterDepth', 'water_depth', 'slab']:
        axis_type = 'depth'
    else:
        raise ValueError('axis_type %s not recognized' % axis_type)

    defaultEndPoints = {'lev': (0., const.ps),
                        'lat': (-90., 90.),
                        'lon': (0., 360.),
                        'depth': (0., 10.),
                        'abstract': (0, num_points)}
    defaultUnits = {'lev': 'mb',
                    'lat': 'degrees',
                    'lon': 'degrees',
                    'depth': 'meters',
                    'abstract': 'none'}
    # if points and/or bounds are supplied, make sure they are increasing
    if points is not None:
        try:
            # using np.atleast_1d() ensures that we can use a single point
            points = np.sort(np.atleast_1d(np.array(points, dtype=float)))
        except:
            raise ValueError('points must be array_like.')
    if bounds is not None:
        try:
            bounds = np.sort(np.atleast_1d(np.array(bounds, dtype=float)))
        except:
            raise ValueError('bounds must be array_like.')

    if bounds is None:
        # assume default end points
        end0 = defaultEndPoints[axis_type][0]
        end1 = defaultEndPoints[axis_type][1]
        if points is not None:
            # only points are given
            num_points = points.size
            bounds = points[:-1] + np.diff(points)/2.
            temp = np.append(np.flipud(bounds), end0)
            bounds = np.append(np.flipud(temp), end1)
        else:
            # no points or bounds
            # create an evenly spaced axis
            delta = (end1 - end0) / num_points
            bounds = np.linspace(end0, end1, num_points+1)
            points = np.linspace(end0 + delta/2., end1-delta/2., num_points)
    else:  # bounds are given
        end0 = bounds[0]
        end1 = bounds[1]
        num_points = bounds.size - 1
        if points is None:
            # only bounds given. Assume points are halfway between bounds
            points = bounds[:-1] + np.diff(bounds)/2.
        else:
            # points and bounds both given, check that they are compatible
            if points.size != num_points:
                raise ValueError('points and bounds have incompatible sizes')
    # pressure axis should decrease from surface to TOA
    if axis_type is 'lev':
        points = np.flipud(points)
        bounds = np.flipud(bounds)
    name_points = axis_type
    name_bounds = axis_type + '_bounds'
    name_delta = axis_type + '_delta'
    delta = np.abs(np.diff(bounds))
    #  create a new dataset with just these two coordinate arrays
    ax = xray.Dataset(coords={name_points: points, name_bounds: bounds})
    ax[name_points].attrs['units'] = defaultUnits[axis_type]
    ax[name_bounds].attrs['units'] = defaultUnits[axis_type]
    #  the delta field will be a data_var in the Dataset (not coordinate)
    ax[name_delta] = xray.DataArray(delta, coords={name_points: points})
    ax[name_delta].attrs['units'] = defaultUnits[axis_type]
    ax.axes = ax.coords
    # This is a hack
    #  This whole section only works for 1D domains
    ax.shape = ax[name_points].shape

    return ax



class _Domain(xray.Dataset):
    pass
    #  adding properties for backwards compatibility
#    @property
#    def axes(self):
#        return self.coords


# class _Domain(object):
#     def __str__(self):
#         return ("climlab Domain object with domain_type=" + self.domain_type + " and shape=" +
#                 str(self.shape))
#     def __init__(self, axes=None, **kwargs):
#         self.domain_type = 'undefined'
#         # self.axes should be a dictionary of axes
#         # make it possible to give just a single axis:
#         self.axes = self._make_axes_dict(axes)
#         self.numdims = len(self.axes.keys())
#         shape = []
#         axcount = 0
#         axindex = {}
#         #  ordered list of axes
#         #  lev OR depth is last
#         #  lat is second-last
#         add_lev = False
#         add_depth = False
#         add_lat = False
#         axlist = self.axes.keys()
#         if 'lev' in axlist:
#             axlist.remove('lev')
#             add_lev = True
#         elif 'depth' in axlist:
#             axlist.remove('depth')
#             add_depth = True
#         if 'lat' in axlist:
#             axlist.remove('lat')
#             add_lat = True
#         axlist2 = axlist[:]
#         if add_lat:
#             axlist2.append('lat')
#         if add_depth:
#             axlist2.append('depth')
#         if add_lev:
#             axlist2.append('lev')
#         #for axType, ax in self.axes.iteritems():
#         for axType in axlist2:
#             ax = self.axes[axType]
#             shape.append(ax.num_points)
#             #  can access axes as object attributes
#             setattr(self, axType, ax)
#             #
#             axindex[axType] = axcount
#             axcount += 1
#         self.axis_index = axindex
#         self.shape = tuple(shape)
#
#         self.set_heat_capacity()
#
#     def set_heat_capacity(self):
#         self.heat_capacity = None
#         #  implemented by daughter classes
#
#     def _make_axes_dict(self, axes):
#         if type(axes) is dict:
#             axdict = axes
#         elif type(axes) is Axis:
#             ax = axes
#             axdict = {ax.axis_type: ax}
#         elif axes is None:
#             axdict = {'empty': None}
#         else:
#             raise ValueError('axes needs to be Axis object or dictionary of Axis object')
#         return axdict



# class Atmosphere(_Domain):
#     def __init__(self, **kwargs):
#         super(Atmosphere, self).__init__(**kwargs)
#         self.domain_type = 'atm'
#
#     def set_heat_capacity(self):
#         self.heat_capacity = heat_capacity.atmosphere(self.axes['lev'].delta)
#
#
# class Ocean(_Domain):
#     def __init__(self, **kwargs):
#         super(Ocean, self).__init__(**kwargs)
#         self.domain_type = 'ocean'
#
#     def set_heat_capacity(self):
#         self.heat_capacity = heat_capacity.ocean(self.axes['depth'].delta)


def make_slabocean_axis(num_points=1):
    '''Convenience method to create a simple axis for a slab ocean.'''
    depthax = Axis(axis_type='depth', num_points=num_points)
    return depthax

def make_slabatm_axis(num_points=1):
    '''Convenience method to create a simple axis for a slab atmosphere.'''
    depthax = Axis(axis_type='lev', num_points=num_points)
    return depthax



# class SlabOcean(Ocean):
#     def __init__(self, axes=make_slabocean_axis(), **kwargs):
#         super(SlabOcean, self).__init__(axes=axes, **kwargs)
#
# class SlabAtmosphere(Atmosphere):
#     def __init__(self, axes=make_slabatm_axis(), **kwargs):
#         super(SlabAtmosphere, self).__init__(axes=axes, **kwargs)


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
        levax = create_axis(axis_type='lev', num_points=num_lev)
    elif isinstance(lev, xray.Dataset):
        levax = lev
    else:
        try:
            levax = create_axis(axis_type='lev', points=lev)
        except:
            raise ValueError('lev must be a valid coordinate dataset or pressure array')

    depthax = create_axis(axis_type='depth', bounds=[water_depth, 0.])
    #slab = SlabOcean(axes=depthax, **kwargs)
    #atm = Atmosphere(axes=levax, **kwargs)
    slab = depthax
    atm = levax
    slab.attrs['domain_type'] = 'ocean'
    slab.heat_capacity = heat_capacity.ocean(slab.depth_delta)
    atm.attrs['domain_type'] = 'atm'
    atm.heat_capacity = heat_capacity.atmosphere(atm.lev_delta)
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
