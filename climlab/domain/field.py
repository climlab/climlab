#  Trying a new data model for state variables and domains:
#  Create a new sub-class of numpy.ndarray
#  that has as an attribute the domain itself

# Following a tutorial on subclassing ndarray here:
#
# http://docs.scipy.org/doc/numpy/user/basics.subclassing.html
from __future__ import division
import numpy as np
from climlab.domain.xarray import Field_to_xarray


class Field(np.ndarray):
    """Custom class for climlab gridded quantities, called Field.

    This class behaves exactly like :py:class:`numpy.ndarray`
    but every object has an attribute called ``self.domain``
    which is the domain associated with that field (e.g. state variables).

    **Initialization parameters** \n

    An instance of ``Field`` is initialized with the following
    arguments:

    :param array input_array:   the array which the Field object should be
                                initialized with
    :param domain:              the domain associated with that field
                                (e.g. state variables)
    :type domain:               :class:`~climlab.domain.domain._Domain`

    **Object attributes** \n

    Following object attribute is generated during initialization:

    :var domain:               the domain associated with that field
                                (e.g. state variables)
    :vartype domain:            :class:`~climlab.domain.domain._Domain`


    :Example:

        ::

            >>> import climlab
            >>> import numpy as np
            >>> from climlab import domain
            >>> from climlab.domain import field

            >>> # distribution of state
            >>> distr = np.linspace(0., 10., 30)
            >>> # domain creation
            >>> sfc, atm = domain.single_column()
            >>> # build state of type Field
            >>> s = field.Field(distr, domain=atm)

            >>> print s
            [  0.           0.34482759   0.68965517   1.03448276   1.37931034
               1.72413793   2.06896552   2.4137931    2.75862069   3.10344828
               3.44827586   3.79310345   4.13793103   4.48275862   4.82758621
               5.17241379   5.51724138   5.86206897   6.20689655   6.55172414
               6.89655172   7.24137931   7.5862069    7.93103448   8.27586207
               8.62068966   8.96551724   9.31034483   9.65517241  10.        ]

            >>> print s.domain
            climlab Domain object with domain_type=atm and shape=(30,)

            >>> # can slice this and it preserves the domain
            >>> #  a more full-featured implementation would have intelligent
            >>> #  slicing like in iris
            >>> s.shape == s.domain.shape
            True
            >>> s[:1].shape == s[:1].domain.shape
            False

            >>> #  But some things work very well. E.g. new field creation:
            >>> s2 = np.zeros_like(s)

            >>> print s2
            [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.
              0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]

            >>> print s2.domain
            climlab Domain object with domain_type=atm and shape=(30,)

    """
    def __new__(cls, input_array, domain=None, interfaces=False):
        # Input array is an already formed ndarray instance
        # We first cast to be our class type
        #obj = np.asarray(input_array).view(cls)
        # This should ensure that shape is (1,) for scalar input
        #obj = np.atleast_1d(input_array).view(cls)
        # add the new attribute to the created instance
        #  do some checking for correct dimensions

        # input argument interfaces indicates whether input_array exists
        # on cell interfaces for each dimensions
        #  It should be either a single Boolean
        #  or an array of Booleans compatible with number of dimensions
        if input_array is None:
            return None
        else:
            try:
                shape = np.array(domain.shape) + np.where(interfaces,1,0)
            except:
                raise ValueError('domain and interfaces inconsistent.')
        try:
            #assert obj.shape == domain.shape
            #  This will work if input_array is any of:
            #   - scalar
            #   - same shape as domain
            #   - broadcast-compatible with domain shape
            obj = (input_array * np.ones(shape)).view(cls)
            assert np.all(obj.shape == shape)
        except:
            try:
                # Do we get a match if we add a singleton dimension
                #  (e.g. a singleton depth axis)?
                obj = np.expand_dims(input_array, axis=-1).view(cls)
                assert np.all(obj.shape == shape)
                #obj = np.transpose(np.atleast_2d(obj))
                #if obj.shape == domain.shape:
                #    obj.domain = domain
            except:
                raise ValueError('Cannot reconcile shapes of input_array and domain.')
        obj.domain = domain
        obj.interfaces = interfaces
        #  would be nice to have some automatic domain creation here if none given

        # Finally, we must return the newly created object:
        return obj

    def __array_finalize__(self, obj):
        # ``self`` is a new object resulting from
        # ndarray.__new__(Field, ...), therefore it only has
        # attributes that the ndarray.__new__ constructor gave it -
        # i.e. those of a standard ndarray.
        #
        # We could have got to the ndarray.__new__ call in 3 ways:
        # From an explicit constructor - e.g. Field():
        #    obj is None
        #    (we're in the middle of the Field.__new__
        #    constructor, and self.domain will be set when we return to
        #    Field.__new__)
        if obj is None: return
        # From view casting - e.g arr.view(Field):
        #    obj is arr
        #    (type(obj) can be Field)
        # From new-from-template - e.g statearr[:3]
        #    type(obj) is Field
        #
        # Note that it is here, rather than in the __new__ method,
        # that we set the default value for 'domain', because this
        # method sees all creation of default objects - with the
        # Field.__new__ constructor, but also with
        # arr.view(Field).
        try:
            self.domain = obj.domain
        except:
            self.domain = None
        try:
            self.interfaces = obj.interfaces
        except:
            pass
        # We do not need to return anything

##  Loosely based on the approach in numpy.ma.core.MaskedArray
#   This determines how we slice a Field object
    def __getitem__(self, indx):
        """
        x.__getitem__(y) <==> x[y]
        Return the item described by i, as a Field.
        """
        #  create a view of just the data as np.ndarray and slice it
        dout = self.view(np.ndarray)[indx]
        try:
            #Force dout to type Field
            dout = dout.view(type(self))
            # Now slice the domain
            dout.domain = self.domain[indx]
            # Inherit attributes from self
            if hasattr(self, 'interfaces'):
                dout.interfaces = self.interfaces
        except:
            # The above will fail if we extract a single item
            # in which case we should just return the item
            pass
        return dout

    def to_xarray(self):
        """Convert Field object to xarray.DataArray"""
        return Field_to_xarray(self)


def global_mean(field):
    """Calculates the latitude weighted global mean of a field
    with latitude dependence.

    :param Field field: input field
    :raises: :exc:`ValueError` if input field has no latitude axis
    :return: latitude weighted global mean of the field
    :rtype: float

    :Example:

        initial global mean temperature of EBM model::

            >>> import climlab
            >>> model = climlab.EBM()
            >>> climlab.global_mean(model.Ts)
            Field(11.997968598413685)

    """
    try:
        lat = field.domain.lat.points
    except:
        raise ValueError('No latitude axis in input field.')
    try:
        #  Field is 2D latitude / longitude
        lon = field.domain.lon.points
        return _global_mean_latlon(field.squeeze())
    except:
        #  Field is 1D latitude only (zonal average)
        lat_radians = np.deg2rad(lat)
        return _global_mean(field.squeeze(), lat_radians)


def _global_mean(array, lat_radians):
    #  Use np.array() here to strip the Field data and return a plain array
    #  (This will be more graceful once we are using xarray.DataArray
    #  for all internal grid info instead of the Field object)
    return np.array(np.average(array, weights=np.cos(lat_radians)))


def _global_mean_latlon(field):
    dom = field.domain
    lon, lat = np.meshgrid(dom.lon.points, dom.lat.points)
    dy = np.deg2rad(np.diff(dom.lat.bounds))
    dx = np.deg2rad(np.diff(dom.lon.bounds))*np.cos(np.deg2rad(lat))
    area = dx * dy[:,np.newaxis]  # grid cell area in radians^2
    return np.array(np.average(field, weights=area))


def to_latlon(array, domain, axis = 'lon'):
    """Broadcasts a 1D axis dependent array across another axis.

    :param array input_array:   the 1D array used for broadcasting
    :param domain:              the domain associated with that
                                array
    :param axis:                the axis that the input array will
                                be broadcasted across
                                [default: 'lon']
    :return:                    Field with the same shape as the
                                domain
    :Example:

        ::

            >>> import climlab
            >>> from climlab.domain.field import to_latlon
            >>> import numpy as np

            >>> state = climlab.surface_state(num_lat=3, num_lon=4)
            >>> m = climlab.EBM_annual(state=state)
            >>> insolation = np.array([237., 417., 237.])
            >>> insolation = to_latlon(insolation, domain = m.domains['Ts'])
            >>> insolation.shape
            (3, 4, 1)
            >>> insolation
            Field([[[ 237.],   [[ 417.],   [[ 237.],
                    [ 237.],    [ 417.],    [ 237.],
                    [ 237.],    [ 417.],    [ 237.],
                    [ 237.]],   [ 417.]],   [ 237.]]])

    """
    #  if array is latitude dependent (has the same shape as lat)
    axis, array, depth = np.meshgrid(domain.axes[axis].points, array,
                                    domain.axes['depth'].points)
    if axis == 'lat':
    #  if array is longitude dependent (has the same shape as lon)
        np.swapaxes(array,1,0)
    return Field(array, domain=domain)
