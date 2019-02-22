from __future__ import division
from builtins import str
from builtins import object
import numpy as np
from climlab import constants as const
import xarray as xr


axis_types = ['lev', 'lat', 'lon', 'depth', 'abstract']


def Axis(axis='abstract', num_points=10, points=None, bounds=None):
    if axis in axis_types:
        pass
    elif axis in ['p', 'press', 'pressure', 'P', 'Pressure', 'Press']:
        axis = 'lev'
    elif axis in ['Latitude', 'latitude']:
        axis = 'lat'
    elif axis in ['Longitude', 'longitude']:
        axis = 'lon'
    elif axis in ['depth', 'Depth', 'waterDepth', 'water_depth', 'slab']:
        axis = 'depth'
    else:
        raise ValueError('axis %s not recognized' % axis)
    self = xr.Dataset()
    self.attrs['axis'] = axis

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
        end0 = defaultEndPoints[axis][0]
        end1 = defaultEndPoints[axis][1]
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
    #self.attrs['num_points'] = num_points
    self.attrs['units'] = defaultUnits[axis]
    self[axis] = xr.DataArray(points, dims=axis, coords={axis: points})
    self[axis].attrs['axis'] = axis
    self[axis+'_bounds'] = xr.DataArray(bounds, dims=(axis+'_bounds'), coords={(axis+'_bounds'): bounds})
    self[axis+'_bounds'].attrs['axis'] = axis
    self[axis+'_bounds'].attrs['c_grid_axis_shift'] = -0.5
    #self['delta'] = xr.DataArray(np.abs(np.diff(self.bounds)), dims=axis_type, coords={axis_type: points})
    #self[axis+'_delta'] = xr.DataArray(np.abs(np.diff(bounds)), dims=axis, coords={axis: points})
    #self[axis+'_delta'].attrs['axis'] = axis

    return self
