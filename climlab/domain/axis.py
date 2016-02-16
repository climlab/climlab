import numpy as np
from climlab import constants as const


axis_types = ['lev', 'lat', 'lon', 'depth', 'abstract']


# will need to implement a simple cartesian distance axis type
# and probaly also an abstract dimensionless axis type (for box models)

class Axis(object):
    """Creates a new climlab Axis object.
    

    
    **Initialization parameters** \n
        
    An instance of ``Axis`` is initialized with the following 
    arguments *(for detailed information see Object attributes below)*:
    
    :param str axis_type:   information about the type of axis
    :param int num_points:  number of points on axis
    :param array points:    array with specific points (optional)
    :param array bounds:    array with specific bounds between points (optional)
    :raises: :exc:`ValueError`
                            if ``axis_type`` is not one of the valid types or 
                            their euqivalents (see below).
    :raises: :exc:`ValueError`
                            if ``points`` are given and not array-like.
    :raises: :exc:`ValueError`
                            if ``bounds`` are given and not array-like.
    
    **Object attributes** \n
    
    Following object attributes are generated during initialization:
        
    :ivar str axis_type:    Information about the type of axis. Valid axis types are: 
    
                                * ``'lev'``
                                * ``'lat'``
                                * ``'lon'``
                                * ``'depth'``
                                * ``'abstract'`` (default)

    :ivar int num_points:   number of points on axis
    :ivar str units:        Unit of the axis. During intialization the unit is 
                            chosen from the ``defaultUnits`` dictionary (see below).
    :ivar array points:     array with all points of the axis (grid)
    :ivar array bounds:     array with all bounds between points (staggered grid)
    :ivar array delta:      array with spatial differences between bounds
    

    **Axis Types** \n
    
    Inputs for the ``axis_type`` like ``'p'``, ``'press'``, ``'pressure'``, 
    ``'P'``, ``'Pressure'`` and ``'Press'`` are refered to as ``'lev'``.
    
    ``'Latitude'`` and ``'latitude'`` are refered to as ``'lat'``.

    ``'Longitude'`` and ``'longitude'`` are refered to as ``'lon'``.
        
    And ``'depth'``, ``'Depth'``, ``'waterDepth'``, ``'water_depth'`` and
    ``'slab'`` are refered to as ``'depth'``.
    
    
    The **default units** are:

    .. code::
        
        defaultUnits = {'lev': 'mb',
                        'lat': 'degrees',
                        'lon': 'degrees',
                        'depth': 'meters',
                        'abstract': 'none'}
    
    If bounds are not given during initialization, **default end points** are used:
    
    .. code::
        
        defaultEndPoints = {'lev': (0., climlab.constants.ps),
                            'lat': (-90., 90.),
                            'lon': (0., 360.),
                            'depth': (0., 10.),
                            'abstract': (0, num_points)}
    
        
    """
    def __str__(self):
        return ("Axis of type " + self.axis_type + " with " +
                str(self.num_points) + " points.")

    def __init__(self, axis_type='abstract', num_points=10, points=None, bounds=None):
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
        self.axis_type = axis_type

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
        self.num_points = num_points
        self.units = defaultUnits[axis_type]
        # pressure axis should decrease from surface to TOA
        #  NO! Now define the lowest (near-to-surface) element as lev[-1]
        #   and the nearest to space as lev[0]
        #if axis_type is 'lev':
        #    points = np.flipud(points)
        #    bounds = np.flipud(bounds)
        self.points = points
        self.bounds = bounds
        self.delta = np.abs(np.diff(self.bounds))
