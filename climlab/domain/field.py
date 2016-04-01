#  Trying a new data model for state variables and domains:
#  Create a new sub-class of numpy.ndarray
#  that has as an attribute the domain itself

# Following a tutorial on subclassing ndarray here:
#
# http://docs.scipy.org/doc/numpy/user/basics.subclassing.html

import numpy as np

'''test usage:
import numpy as np
A = np.linspace(0., 10., 30)
d = domain.single_column()
atm = d['atm']
s = field.Field(A, domain=atm)
print s
print s.domain
# can slice this and it preserves the domain
#  a more full-featured implementation would have intelligent slicing
#  like in iris
s.shape == s.domain.shape
s[:1].shape == s[:1].domain.shape
#  But some things work very well. E.g. new field creation:
s2 = np.zeros_like(s)
print s2
print s2.domain
'''


class Field(np.ndarray):
    '''Custom class for climlab gridded quantities, called Field
    This class behaves exactly like numpy.ndarray
    but every object has an attribute called domain
    which is the domain associated with that field (e.g. state variables).'''

    def __new__(cls, input_array, domain=None):
        # Input array is an already formed ndarray instance
        # We first cast to be our class type
        #obj = np.asarray(input_array).view(cls)
        # This should ensure that shape is (1,) for scalar input
        obj = np.atleast_1d(input_array).view(cls)
        # add the new attribute to the created instance
        #  do some checking for correct dimensions
        if obj.shape == domain.shape:
            obj.domain = domain
        else:
            try:
                obj = np.transpose(np.atleast_2d(obj))
                if obj.shape == domain.shape:
                    obj.domain = domain
            except:
                raise ValueError('input_array and domain have different shapes.')

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

        #self.domain = getattr(obj, 'domain', None)
        # To enable intelligent slicing of Field variables:
        domain = getattr(obj, 'domain', None)
        if hasattr(domain, 'shape'):
            if domain.shape == obj.shape:
                self.domain = domain
            else:
            #  Look at how the slicing works for the MaskedArray type
            #  We should be able to emulate that to slice heat capacity and axes
            #  For now, nothing different happens
                self.domain = domain
        else:
            self.domain = domain
        # We do not need to return anything

##  This is based on code copied from numpy.ma.core.MaskedArray
    def __getitem__(self, indx):
        """
        x.__getitem__(y) <==> x[y]
        Return the item described by i, as a Field.
        """
        #dout = self.data[indx]
        #dout = np.array(self[indx])  # just the data array as np.ndarray
        #  no this is recursive!
        #dout = np.array(self)[indx]
        #  create a view of just the data as np.ndarray and slice it
        dout = self.view(np.ndarray)[indx]

        #_mask = self._mask
        from copy import copy
        domain = copy(self.domain)
        # Did we extract a single item?
        if not getattr(dout, 'ndim', False):
            # # A record
            # if isinstance(dout, np.void):
            #     mask = _mask[indx]
            #     # We should always re-cast to mvoid, otherwise users can
            #     # change masks on rows that already have masked values, but not
            #     # on rows that have no masked values, which is inconsistent.
            #     dout = mvoid(dout, mask=mask, hardmask=self._hardmask)
            # # Just a scalar
            # elif _mask is not nomask and _mask[indx]:
            #     return masked
            return dout
        #elif self.dtype.type is np.object_ and self.dtype is not dout.dtype:
        #    # self contains an object array of arrays (yes, that happens).
        #    # If masked, turn into a MaskedArray, with everything masked.
        #    if _mask is not nomask and _mask[indx]:
        #        return MaskedArray(dout, mask=True)
        else:
            # Force dout to type Field
            dout = dout.view(type(self))
            # Inherit attributes from self
            dout.domain = domain
            # Check the fill_value ... no don't need this part
            # Update the mask if needed
            #if _mask is not nomask:
            #    dout._mask = _mask[indx]
            #    # set shape to match that of data; this is needed for matrices
            #    dout._mask.shape = dout.shape
            #    dout._sharedmask = True
            #    # Note: Don't try to check for m.any(), that'll take too long
            # Update the domain information ... at least the heat capacity
            if hasattr(domain, 'heat_capacity'):
                # in some cases (but not others) we need to slice this
                try:
                    dout.domain.heat_capacity = domain.heat_capacity[indx]
                except:
                    pass  # preserve existing heat_capacity array
            if hasattr(domain, 'shape'):
                dout.domain.shape = dout.shape
        return dout

    # def __setitem__(self, indx, value):
    #     """
    #     x.__setitem__(i, y) <==> x[i]=y
    #     Set item described by index. If value is masked, masks those
    #     locations.
    #     """
    #     if self is masked:
    #         raise MaskError('Cannot alter the masked element.')
    #     _data = self._data
    #     _mask = self._mask
    #     if isinstance(indx, basestring):
    #         _data[indx] = value
    #         if _mask is nomask:
    #             self._mask = _mask = make_mask_none(self.shape, self.dtype)
    #         _mask[indx] = getmask(value)
    #         return
    #
    #     _dtype = _data.dtype
    #     nbfields = len(_dtype.names or ())
    #
    #     if value is masked:
    #         # The mask wasn't set: create a full version.
    #         if _mask is nomask:
    #             _mask = self._mask = make_mask_none(self.shape, _dtype)
    #         # Now, set the mask to its value.
    #         if nbfields:
    #             _mask[indx] = tuple([True] * nbfields)
    #         else:
    #             _mask[indx] = True
    #         if not self._isfield:
    #             self._sharedmask = False
    #         return
    #
    #     # Get the _data part of the new value
    #     dval = value
    #     # Get the _mask part of the new value
    #     mval = getattr(value, '_mask', nomask)
    #     if nbfields and mval is nomask:
    #         mval = tuple([False] * nbfields)
    #     if _mask is nomask:
    #         # Set the data, then the mask
    #         _data[indx] = dval
    #         if mval is not nomask:
    #             _mask = self._mask = make_mask_none(self.shape, _dtype)
    #             _mask[indx] = mval
    #     elif not self._hardmask:
    #         # Unshare the mask if necessary to avoid propagation
    #         # We want to remove the unshare logic from this place in the
    #         # future. Note that _sharedmask has lots of false positives.
    #         if not self._isfield:
    #             if self._sharedmask and not (
    #                     # If no one else holds a reference (we have two
    #                     # references (_mask and self._mask) -- add one for
    #                     # getrefcount) and the array owns its own data
    #                     # copying the mask should do nothing.
    #                     (sys.getrefcount(_mask) == 3) and _mask.flags.owndata):
    #                 # 2016.01.15 -- v1.11.0
    #                 warnings.warn(
    #                    "setting an item on a masked array which has a shared "
    #                    "mask will not copy the mask and also change the "
    #                    "original mask array in the future.\n"
    #                    "Check the NumPy 1.11 release notes for more "
    #                    "information.",
    #                    MaskedArrayFutureWarning, stacklevel=2)
    #             self.unshare_mask()
    #             _mask = self._mask
    #         # Set the data, then the mask
    #         _data[indx] = dval
    #         _mask[indx] = mval
    #     elif hasattr(indx, 'dtype') and (indx.dtype == MaskType):
    #         indx = indx * umath.logical_not(_mask)
    #         _data[indx] = dval
    #     else:
    #         if nbfields:
    #             err_msg = "Flexible 'hard' masks are not yet supported."
    #             raise NotImplementedError(err_msg)
    #         mindx = mask_or(_mask[indx], mval, copy=True)
    #         dindx = self._data[indx]
    #         if dindx.size > 1:
    #             np.copyto(dindx, dval, where=~mindx)
    #         elif mindx is nomask:
    #             dindx = dval
    #         _data[indx] = dindx
    #         _mask[indx] = mindx
    #     return
    def __getslice__(self, i, j):
        """
        x.__getslice__(i, j) <==> x[i:j]
        Return the slice described by (i, j).  The use of negative indices
        is not supported.
        """
        return self.__getitem__(slice(i, j))


def global_mean(field):
    '''Calculate global mean of a field with latitude dependence.'''
    try:
        lat = field.domain.axes['lat'].points
    except:
        raise ValueError('No latitude axis in input field.')
    lat_radians = np.deg2rad(lat)
    return _global_mean(field.squeeze(), lat_radians)


def _global_mean(array, lat_radians):
    return np.sum(array * np.cos(lat_radians)) / np.sum(np.cos(lat_radians))
