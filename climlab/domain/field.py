#  Trying a new data model for state variables and domains:
#  Create a new sub-class of numpy.ndarray
#  that has as an attribute the domain itself

# Following a tutorial on subclassing ndarray here:
#
# http://docs.scipy.org/doc/numpy/user/basics.subclassing.html

import numpy as np
import copy

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
        obj = np.asarray(input_array).view(cls)
        # add the new attribute to the created instance
        #  do some checking for correct dimensions
        if obj.shape == domain.shape:
            obj.domain = domain
        else:
            raise ValueError('input_array and domain have different shapes.')
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

        self.domain = getattr(obj, 'domain', None)
        # We do not need to return anything

#  Not needed!  np.zeros_like etc work automatically
#def Field_like(field):
#    '''Return a copy of an existing Field object with the same domain.'''
#    # using copy.copy instead of copy.deepcopy should ensure that the domains
#    # are actually pointing to the same domain object.
#    return copy.copy(field)
