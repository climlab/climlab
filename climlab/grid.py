# This code is mostly borrowed from climt
# implements a grid class that behaves like a dictionary

import numpy as np


class Grid:
    '''
    '''
#    def __init__(self, Component, **kwargs):
# We will either pass dimensions, or actual arrays holding the coordinate data
    def __init__(self,
                 nlev=10,  # these will be ignored if data are specified
                 nlat=0,
                 nlon=0,
                 p=None,
                 lat=None,
                 lon=None,
                 levType='pressure',
                 **kwargs):
        '''
        '''
        # Initialise dicts
        self.value = {}
        self.units = {}
        self.long_name = {}

        # Get shape appropriate for component
        # self.Shape3D = Component._getShape3D(**kwargs)
        # self.Shape3D = Component.Shape3D
        if p is not None:
            try:
                nlev = p.size
            except:
                raise TypeError('p must be a numpy array')
        if lat is not None:
            try:
                nlat = lat.size
            except:
                raise TypeError('p must be a numpy array')
        if lon is not None:
            try:
                nlon = lon.size
            except:
                raise TypeError('p must be a numpy array')
        self.Shape3D = (nlev, nlat, nlon)
        self.nlev = nlev
        self.nlat = nlat
        self.nlon = nlon

        # Levels
        self.value['nlev'] = self.Shape3D[0]
        self.long_name['lev'] = 'level'

        if levType == 'pressure':
            self.long_name['lev'] = 'presssure'
            self.units['lev'] = 'mb'
        if nlev == 0:
            self.units['lev'] = '-'
        if p is None:
            self._setAxis('lev', levType=levType, **kwargs)
        else:
            self._setAxis('lev', levType=levType, p=p)

        # Latitude
        self.value['nlat'] = self.Shape3D[1]
        self.long_name['lat'] = 'latitude'
        self.units['lat'] = 'degrees'
        if lat is None:
            self._setAxis('lat', **kwargs)
        else:
            self._setAxis('lat', lat=lat, **kwargs)

        # Longitude
        self.value['nlon'] = self.Shape3D[2]
        self.long_name['lon'] = 'longitude'
        self.units['lon'] = 'degrees'
        if lon is None:
            self._setAxis('lon', **kwargs)
        else:
            self._setAxis('lon', lon=lon, **kwargs)

    def _setAxis(self, AxisName, levType=None, **kwargs):
        '''
        Sets the value of a grid coordinate axis.
        '''
        i = ['lev', 'lat', 'lon'].index(AxisName)
        n = self.Shape3D[i]
        if AxisName in kwargs:
            self.value[AxisName] = np.array(kwargs[AxisName])
            # ensure axis is an array
            if self.value[AxisName].ndim == 0:
                self.value[AxisName] = np.array([kwargs[AxisName], ])
        elif AxisName is 'lev' and levType == 'pressure' and 'p' in kwargs:
            # this gets first column of p (do it like this because we don't know dims of p)
            self.value['lev'] = np.transpose(np.array(kwargs['p'])).copy().flat[:n]
        else:
            if AxisName is 'lon':
                self.value[AxisName] = (np.arange(n)+0.5)*360./n
                if n == 1:
                    self.value[AxisName] = np.array([0.])
            if AxisName is 'lat':
                self.value[AxisName] = (np.arange(n)+0.5)*180./n -90.
            if AxisName is 'lev':
                if levType == 'pressure':
                    self.value[AxisName] = (np.arange(n)+0.5)*1000./n
                elif levType is None:
                    self.value[AxisName] = np.arange(n)
                else:
                    raise ValueError('\n\n levType %s not recognized' % levType)
        assert n == len(self.value[AxisName]), \
               '\n\n Length of input %s does not match Shape3D' % AxisName

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
