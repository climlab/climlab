from __future__ import division
from builtins import str
from builtins import object
from climlab.domain.axis import Axis
from climlab.utils import heat_capacity


class _Domain(object):
    """Private parent class for `Domains`.

    A `Domain` defines an area or spatial base for a climlab
    :class:`~climlab.process.process.Process` object. It consists of axes which
    are :class:`~climlab.domain.axis.Axis` objects that define the dimensions
    of the `Domain`.

    In a `Domain` the heat capacity of grid points, bounds or cells/boxes is
    specified.

    There are daughter classes :class:`~climlab.domain.domain.Atmosphere` and
    :class:`~climlab.domain.domain.Ocean` of the private
    :class:`~climlab.domain.domain._Domain` class implemented which themselves
    have daughter classes :class:`~climlab.domain.domain.SlabAtmosphere` and
    :class:`~climlab.domain.domain.SlabOcean`.

    Several methods are implemented that create `Domains` with special
    specifications. These are

	- :func:`~climlab.domain.domain.single_column`

	- :func:`~climlab.domain.domain.zonal_mean_column`

	- :func:`~climlab.domain.domain.box_model_domain`


    **Initialization parameters** \n

    An instance of ``_Domain`` is initialized with the following
    arguments:

    :param axes:    Axis object or dictionary of Axis object where domain will
                    be defined on.
    :type axes:     dict or :class:`~climlab.domain.axis.Axis`


    **Object attributes** \n

    Following object attributes are generated during initialization:

    :ivar str domain_type:      Set to ``'undefined'``.
    :ivar dict axes:            A dictionary of the domains axes. Created by
                                :func:`_make_axes_dict` called with input
                                argument ``axes``
    :ivar int numdims:          Number of :class:`~climlab.domain.axis.Axis` objects
                                in ``self.axes`` dictionary.
    :ivar dict ax_index:        A dictionary of domain axes and their corresponding index
                                in an ordered list of the axes with:        \n
                                - ``'lev'`` or ``'depth'`` is last
                                - ``'lat'`` is second last
    :ivar tuple shape:          Number of points of all domain axes. Order in
                                tuple given by ``self.ax_index``.
    :ivar array heat_capacity:  the domain's heat capacity over axis specified
                                in function call of :func:`set_heat_capacity`

    """
    def __str__(self):
        return ("climlab Domain object with domain_type=" + self.domain_type + " and shape=" +
                str(self.shape))
    def __init__(self, axes=None, **kwargs):
        self.domain_type = 'undefined'
        # self.axes should be a dictionary of axes
        # make it possible to give just a single axis:
        self.axes = self._make_axes_dict(axes)
        self.numdims = len(list(self.axes.keys()))
        shape = []
        axcount = 0
        axindex = {}
        #  ordered list of axes
        #  lev OR depth is last
        #  lat is second-last
        add_lev = False
        add_depth = False
        add_lon = False
        add_lat = False
        axlist = list(self.axes.keys())
        if 'lev' in axlist:
            axlist.remove('lev')
            add_lev = True
        elif 'depth' in axlist:
            axlist.remove('depth')
            add_depth = True
        if 'lon' in axlist:
            axlist.remove('lon')
            add_lon = True
        if 'lat' in axlist:
            axlist.remove('lat')
            add_lat = True
        axlist2 = axlist[:]
        if add_lat:
            axlist2.append('lat')
        if add_lon:
            axlist2.append('lon')
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
        """A dummy function to set the heat capacity of a domain.

        *Should be overridden by daugter classes.*

        """
        self.heat_capacity = None
        #  implemented by daughter classes

    def _make_axes_dict(self, axes):
        """Makes an axes dictionary.

        .. note::

            In case the input is ``None``, the dictionary :code:`{'empty': None}`
            is returned.

        **Function-call argument** \n

        :param axes:    axes input
        :type axes:     dict or single instance of
                        :class:`~climlab.domain.axis.Axis` object or ``None``
        :raises: :exc:`ValueError`  if input is not an instance of Axis class
                                    or a dictionary of Axis objetcs
        :returns: dictionary of input axes
        :rtype: dict

        """
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

    def __getitem__(self, indx):
        # Make domains sliceable
        # First create a bare domain object (without calling the __init__ method)
        dout = type(self).__new__(type(self))
        # inherit *most* of the attributes of self
        #  For now we are just slicing the heat capacity
        #  But would be great to have some logic for slicing axes
        #   I am not 100% percent clear on how all this works
        #   But for now we're just going to "try" to slice to avoid
        #   some failures
        for key, value in self.__dict__.items():
            if key == 'heat_capacity':
                try:
                    dout.heat_capacity = self.heat_capacity[indx]
                except:
                    dout.heat_capacity = self.heat_capacity
            elif key == 'shape':
                try:
                    dout.shape = self.heat_capacity[indx].shape
                except:
                    dout.shape = self.shape
            else:
                setattr(dout, key, value)
        return dout


class Atmosphere(_Domain):
    """Class for the implementation of an Atmosphere Domain.

    **Object attributes** \n

    Additional to the parent class :class:`~climlab.domain.domain._Domain`
    the following object attribute is modified during initialization:

    :ivar str domain_type:    is set to ``'atm'``

    :Example:

        Setting up an Atmosphere Domain::

            >>> import climlab
            >>> atm_ax = climlab.domain.Axis(axis_type='pressure', num_points=10)
            >>> atm_domain = climlab.domain.Atmosphere(axes=atm_ax)

            >>> print atm_domain
            climlab Domain object with domain_type=atm and shape=(10,)

            >>> atm_domain.axes
            {'lev': <climlab.domain.axis.Axis object at 0x7fe5b8ef8e10>}

            >>> atm_domain.heat_capacity
            array([ 1024489.79591837,  1024489.79591837,  1024489.79591837,
                    1024489.79591837,  1024489.79591837,  1024489.79591837,
                    1024489.79591837,  1024489.79591837,  1024489.79591837,
                    1024489.79591837])

    """
    def __init__(self, **kwargs):
        super(Atmosphere, self).__init__(**kwargs)
        self.domain_type = 'atm'

    def set_heat_capacity(self):
        """Sets the heat capacity of the Atmosphere Domain.

        Calls the utils heat capacity function
        :func:`~climlab.utils.heat_capacity.atmosphere` and gives the delta
        array of grid points of it's level axis
        ``self.axes['lev'].delta`` as input.

        **Object attributes** \n

        During method execution following object attribute is modified:

        :ivar array heat_capacity:  the ocean domain's heat capacity over
                                    the ``'lev'`` Axis.

        """
        self.heat_capacity = heat_capacity.atmosphere(self.axes['lev'].delta)


class Ocean(_Domain):
    """Class for the implementation of an Ocean Domain.

    **Object attributes** \n

    Additional to the parent class :class:`~climlab.domain.domain._Domain`
    the following object attribute is modified during initialization:

    :ivar str domain_type:    is set to ``'ocean'``

    :Example:

        Setting up an Ocean Domain::

            >>> import climlab
            >>> ocean_ax = climlab.domain.Axis(axis_type='depth', num_points=5)
            >>> ocean_domain = climlab.domain.Ocean(axes=ocean_ax)

            >>> print ocean_domain
            climlab Domain object with domain_type=ocean and shape=(5,)

            >>> ocean_domain.axes
            {'depth': <climlab.domain.axis.Axis object at 0x7fe5b8f102d0>}

            >>> ocean_domain.heat_capacity
            array([ 8362600.,  8362600.,  8362600.,  8362600.,  8362600.])

    """
    def __init__(self, **kwargs):
        super(Ocean, self).__init__(**kwargs)
        self.domain_type = 'ocean'

    def set_heat_capacity(self):
        """Sets the heat capacity of the Ocean Domain.

        Calls the utils heat capacity function
        :func:`~climlab.utils.heat_capacity.ocean` and gives the delta
        array of grid points of it's depth axis
        ``self.axes['depth'].delta`` as input.

        **Object attributes** \n

        During method execution following object attribute is modified:

        :ivar array heat_capacity:  the ocean domain's heat capacity over
                                    the ``'depth'`` Axis.

        """
        self.heat_capacity = heat_capacity.ocean(self.axes['depth'].delta)


def make_slabocean_axis(num_points=1):
    """Convenience method to create a simple axis for a slab ocean.

    **Function-call argument** \n

    :param int num_points:    number of points for the slabocean Axis [default: 1]
    :returns:               an Axis with ``axis_type='depth'`` and ``num_points=num_points``
    :rtype:                 :class:`~climlab.domain.axis.Axis`

    :Example:

        ::

            >>> import climlab
            >>> slab_ocean_axis = climlab.domain.make_slabocean_axis()

            >>> print slab_ocean_axis
            Axis of type depth with 1 points.

            >>> slab_ocean_axis.axis_type
            'depth'

            >>> slab_ocean_axis.bounds
            array([  0.,  10.])

            >>> slab_ocean_axis.units
            'meters'

    """
    depthax = Axis(axis_type='depth', num_points=num_points)
    return depthax

def make_slabatm_axis(num_points=1):
    """Convenience method to create a simple axis for a slab atmosphere.

    **Function-call argument** \n

    :param int num_points:   number of points for the slabatmosphere Axis [default: 1]
    :returns:               an Axis with ``axis_type='lev'`` and ``num_points=num_points``
    :rtype:                 :class:`~climlab.domain.axis.Axis`

    :Example:

        ::

            >>> import climlab
            >>> slab_atm_axis = climlab.domain.make_slabatm_axis()

            >>> print slab_atm_axis
            Axis of type lev with 1 points.

            >>> slab_atm_axis.axis_type
            'lev'

            >>> slab_atm_axis.bounds
            array([    0.,  1000.])

            >>> slab_atm_axis.units
            'mb'

    """
    depthax = Axis(axis_type='lev', num_points=num_points)
    return depthax



class SlabOcean(Ocean):
    """A class to create a SlabOcean Domain by default.

    Initializes the parent :class:`Ocean` class with a simple axis for a
    Slab Ocean created by :func:`make_slabocean_axis` which has just 1 cell
    in depth by default.

    :Example:

        Creating a SlabOcean Domain::

            >>> import climlab
            >>> slab_ocean_domain = climlab.domain.SlabOcean()

            >>> print slab_ocean_domain
            climlab Domain object with domain_type=ocean and shape=(1,)

            >>> slab_ocean_domain.axes
            {'depth': <climlab.domain.axis.Axis object at 0x7fe5c42814d0>}

            >>> slab_ocean_domain.heat_capacity
            array([ 41813000.])

    """
    def __init__(self, axes=make_slabocean_axis(), **kwargs):
        super(SlabOcean, self).__init__(axes=axes, **kwargs)

class SlabAtmosphere(Atmosphere):
    """A class to create a SlabAtmosphere Domain by default.

    Initializes the parent :class:`Atmosphere` class with a simple axis for a
    Slab Atmopshere created by :func:`make_slabatm_axis` which has just 1 cell
    in height by default.

    :Example:

        Creating a SlabAtmosphere Domain::

            >>> import climlab
            >>> slab_atm_domain = climlab.domain.SlabAtmosphere()

            >>> print slab_atm_domain
            climlab Domain object with domain_type=atm and shape=(1,)

            >>> slab_atm_domain.axes
            {'lev': <climlab.domain.axis.Axis object at 0x7fe5c4281610>}

            >>> slab_atm_domain.heat_capacity
            array([ 10244897.95918367])

    """
    def __init__(self, axes=make_slabatm_axis(), **kwargs):
        super(SlabAtmosphere, self).__init__(axes=axes, **kwargs)


def single_column(num_lev=30, water_depth=1., lev=None, **kwargs):
    """Creates domains for a single column of atmosphere overlying a slab of water.

    Can also pass a pressure array or pressure level axis object specified in ``lev``.

    If argument ``lev`` is not ``None`` then function tries to build a level axis
    and ``num_lev`` is ignored.

    **Function-call argument** \n

    :param int num_lev:         number of pressure levels
                                (evenly spaced from surface to TOA) [default: 30]
    :param float water_depth:   depth of the ocean slab [default: 1.]
    :param lev:                 specification for height axis (optional)
    :type lev:                  :class:`~climlab.domain.axis.Axis` or pressure array
    :raises: :exc:`ValueError`  if `lev` is given but neither Axis
                                nor pressure array.
    :returns:                   a list of 2 Domain objects (slab ocean, atmosphere)
    :rtype:                     :py:class:`list` of :class:`SlabOcean`, :class:`SlabAtmosphere`

    :Example:

        ::

            >>> from climlab import domain

            >>> sfc, atm = domain.single_column(num_lev=2, water_depth=10.)

            >>> print sfc
            climlab Domain object with domain_type=ocean and shape=(1,)

            >>> print atm
            climlab Domain object with domain_type=atm and shape=(2,)

    """
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
    """Creates a 1D slab ocean Domain in latitude with uniform water depth.

    Domain has a single heat capacity according to the specified water depth.

    **Function-call argument** \n

    :param int num_lat:         number of latitude points [default: 90]
    :param float water_depth:   depth of the slab ocean in meters [default: 10.]
    :param lat:                 specification for latitude axis (optional)
    :type lat:                  :class:`~climlab.domain.axis.Axis` or latitude array
    :raises: :exc:`ValueError`  if `lat` is given but neither Axis nor latitude array.
    :returns:                   surface domain
    :rtype:                     :class:`SlabOcean`

    :Example:

        ::

            >>> from climlab import domain
            >>> sfc = domain.zonal_mean_surface(num_lat=36)

            >>> print sfc
            climlab Domain object with domain_type=ocean and shape=(36, 1)

    """
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

def surface_2D(num_lat=90, num_lon=180, water_depth=10., lon=None,
               lat=None, **kwargs):
    """Creates a 2D slab ocean Domain in latitude and longitude with uniform water depth.

    Domain has a single heat capacity according to the specified water depth.

    **Function-call argument** \n

    :param int num_lat:         number of latitude points [default: 90]
    :param int num_lon:         number of longitude points [default: 180]
    :param float water_depth:   depth of the slab ocean in meters [default: 10.]
    :param lat:                 specification for latitude axis (optional)
    :type lat:                  :class:`~climlab.domain.axis.Axis` or latitude array
    :param lon:                 specification for longitude axis (optional)
    :type lon:                  :class:`~climlab.domain.axis.Axis` or longitude array
    :raises: :exc:`ValueError`  if `lat` is given but neither Axis nor latitude array.
    :raises: :exc:`ValueError`  if `lon` is given but neither Axis nor longitude array.
    :returns:                   surface domain
    :rtype:                     :class:`SlabOcean`

    :Example:

        ::

            >>> from climlab import domain
            >>> sfc = domain.surface_2D(num_lat=36, num_lat=72)

            >>> print sfc
            climlab Domain object with domain_type=ocean and shape=(36, 72, 1)

    """
    if lat is None:
        latax = Axis(axis_type='lat', num_points=num_lat)
    elif isinstance(lat, Axis):
        latax = lat
    else:
        try:
            latax = Axis(axis_type='lat', points=lat)
        except:
            raise ValueError('lat must be Axis object or latitude array')
    if lon is None:
        lonax = Axis(axis_type='lon', num_points=num_lon)
    elif isinstance(lon, Axis):
        lonax = lon
    else:
        try:
            lonax = Axis(axis_type='lon', points=lon)
        except:
            raise ValueError('lon must be Axis object or longitude array')
    depthax = Axis(axis_type='depth', bounds=[water_depth, 0.])
    axes = {'lat': latax, 'lon': lonax, 'depth': depthax}
    slab = SlabOcean(axes=axes, **kwargs)
    return slab

def zonal_mean_column(num_lat=90, num_lev=30, water_depth=10., lat=None,
                      lev=None, **kwargs):
    """Creates two Domains with one water cell, a latitude axis and
    a level/height axis.

        * SlabOcean:    one water cell and a latitude axis above
          (similar to :func:`zonal_mean_surface`)
        * Atmosphere: a latitude axis and a level/height axis (two dimensional)


    **Function-call argument** \n

    :param int num_lat:         number of latitude points on the axis
                                [default: 90]
    :param int num_lev:         number of pressure levels
                                (evenly spaced from surface to TOA) [default: 30]
    :param float water_depth:   depth of the water cell (slab ocean) [default: 10.]
    :param lat:                 specification for latitude axis (optional)
    :type lat:                  :class:`~climlab.domain.axis.Axis` or latitude array
    :param lev:                 specification for height axis (optional)
    :type lev:                  :class:`~climlab.domain.axis.Axis` or pressure array
    :raises: :exc:`ValueError`  if `lat` is given but neither Axis nor latitude array.
    :raises: :exc:`ValueError`  if `lev` is given but neither Axis nor pressure array.
    :returns:                   a list of 2 Domain objects (slab ocean, atmosphere)
    :rtype:                     :py:class:`list` of :class:`SlabOcean`, :class:`Atmosphere`

    :Example:

        ::

            >>> from climlab import domain
            >>> sfc, atm = domain.zonal_mean_column(num_lat=36,num_lev=10)

            >>> print sfc
            climlab Domain object with domain_type=ocean and shape=(36, 1)

            >>> print atm
            climlab Domain object with domain_type=atm and shape=(36, 10)


    """
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
    """Creates a box model domain (a single abstract axis).

    :param int num_points:      number of boxes [default: 2]
    :returns:                   Domain with single axis of type ``'abstract'``
                                and ``self.domain_type = 'box'``
    :rtype:                     :class:`_Domain`

    :Example:

        ::

            >>> from climlab import domain
            >>> box = domain.box_model_domain(num_points=2)

            >>> print box
            climlab Domain object with domain_type=box and shape=(2,)

    """
    ax = Axis(axis_type='abstract', num_points=num_points)
    boxes = _Domain(axes=ax, **kwargs)
    boxes.domain_type = 'box'
    return boxes
