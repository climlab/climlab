# -*- coding: utf-8 -*-
"""
Created on Thu Jan  8 11:55:04 2015

@author: Brian
"""

import numpy as np
import constants as const


class Axis:
    '''
    '''
    def __init__(self,
                 axisType='lev',
                 npoints=30,
                 points=None,
                 bounds=None):
        # Initialize dictionaries
        self.value = {}
        self.units = {}
        self.long_name = {}

        axisTypes = ['lev', 'lat', 'lon']
        if axisType in ['p', 'press', 'pressure', 'P', 'Pressure', 'Press']:
            axisType = 'lev'
        if axisType in ['Latitude', 'latitude']:
            axisType = 'lat'
        if axisType in ['Longitude', 'longitude']:
            axisType = 'lon'
        if axisType not in axisTypes:
            raise ValueError('axisType %s not recognized' % axisType)
        else:
            self.axisType = axisType

        defaultEndPoints = {'lev': (0., const.ps),
                            'lat': (-90., 90.),
                            'lon': (0., 360.)}
        defaultUnits = {'lev': 'mb',
                        'lat': 'degrees',
                        'lon': 'degrees'}
        # if points and/or bounds are supplied, make sure they are increasing
        if points is not None:
            try:
                points = np.sort(np.array(points, dtype=float))
            except:
                raise ValueError('points must be array_like.')
        if bounds is not None:
            try:
                bounds = np.sort(np.array(bounds, dtype=float))
            except:
                raise ValueError('bounds must be array_like.')

        if bounds is None:
            # assume default end points
            end0 = defaultEndPoints[axisType][0]
            end1 = defaultEndPoints[axisType][1]
            if points is not None:
                # only points are given
                npoints = points.size
                bounds = points[:-1] + np.diff(points)/2.
                bounds = np.concatenate((end0, bounds, end1))
            else:
                # no points or bounds
                # create an evenly spaced axis
                delta = (end1 - end0) / npoints
                bounds = np.linspace(end0, end1, npoints+1)
                points = np.linspace(end0 + delta/2., end1-delta/2., npoints)
        else:  # bounds are given
            end0 = bounds[0]
            end1 = bounds[1]
            npoints = bounds.size - 1
            if points is None:
                # only bounds given. Assume points are halfway between bounds
                points = bounds[:-1] + np.diff(bounds)/2.
            else:
                # points and bounds both given, check that they are compatible
                if points.size != npoints:
                    raise ValueError('points and bounds have incompatible sizes')
        self.npoints = npoints
        self.units = defaultUnits[axisType]
        self.points = points
        self.bounds = bounds

        # Not sure if I should bother with the dictionary stuff        
        #self.value['npoints'] = self.npoints
        #self.value['units'] = self.units
        #self.value['axisType'] = self.axisType

    def __getitem__(self, key):
        try:
            return self.value[key]
        except:
            raise IndexError('\n\n %s not in Axis' % str(key))

# Make this class behave like a dictionary
    def __setitem__(self, key, value):
        self.value[key] = value

    def keys(self):
        return self.value.keys()

    def __iter__(self):
        return self.value.__iter__()
