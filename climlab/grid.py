# Borrows some code and concepts from climt
# implements a grid class that behaves like a dictionary
#  but uses the climlab axis class

import numpy as np
import axis


# a grid should basically just be a list of axes

class Grid:
    '''
    '''
    def __str__(self):
        return('Grid with (lev, lat, lon) dimensions ' + str(self.shape3D))

    def __init__(self, lev=None, lat=None, lon=None):
        '''Inputs should be Axis objects.
        '''
        # Initialise dicts
        self.value = {}
        self.units = {}
        self.num_points = {}

        for ax in axis.axisTypes:
            self.units[ax] = '-'
        try:
            self.num_lev = lev.num_points
            self.units['lev'] = lev.units
            self.value['lev'] = lev
        except:
            self.num_lev = 1
        try:
            self.num_lat = lat.num_points
            self.units['lat'] = lat.units
            self.value['lat'] = lat
        except:
            self.num_lat = 1
        try:
            self.num_lon = lon.num_points
            self.units['lon'] = lon.units
            self.value['lon'] = lon
        except:
            self.num_lon = 1
        self.shape3D = (self.num_lev, self.num_lat, self.num_lon)
        # The dimensions of each state variable... singleton dims removed
        self.stateDim = np.shape(np.squeeze(np.ones(self.shape3D)))

        self.num_points['lev'] = self.num_lev
        self.num_points['lat'] = self.num_lat
        self.num_points['lon'] = self.num_lon

        self.lev = lev
        self.lat = lat
        self.lon = lon

    def __getitem__(self, key):
        try:
            return self.value[key]
        except:
            raise IndexError('\n\n %s not in Grid' % str(key))

    def __setitem__(self, key, value):
        self.value[key] = value

    def keys(self):
        return self.value.keys()

    def __iter__(self):
        return self.value.__iter__()
