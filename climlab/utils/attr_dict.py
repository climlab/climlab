from collections import OrderedDict


class AttrDict(dict):
    '''
    Constructs a dict object with attribute access to data.
    '''
    def __getattr__(self, attr):
        '''
        Try to get the data. If attr is not a key, fall-back and get the attr
        '''
        if self.has_key(attr):
            return super(AttrDict, self).__getitem__(attr)
        else:
            return super(AttrDict, self).__getattr__(attr)


    def __setattr__(self, attr, value):
        '''
        Try to set the data. If attr is not a key, fall-back and set the attr
        '''
        if self.has_key(attr):
            super(AttrDict, self).__setitem__(attr, value)
        else:
            super(AttrDict, self).__setattr__(attr, value)



class OrderedAttrDict(OrderedDict):
    '''
    Constructs an collections.OrderedDict object with attribute access to data.

    Setting a NEW attribute only creates it on the instance, not the dict.
    Setting an attribute that is a key in the data will set the dict data but
    will not create a new instance attribute
    '''
    def __getattr__(self, attr):
        '''
        Try to get the data. If attr is not a key, fall-back and get the attr
        '''
        if self.has_key(attr):
            return super(OrderedAttrDict, self).__getitem__(attr)
        else:
            return super(OrderedAttrDict, self).__getattr__(attr)


    def __setattr__(self, attr, value):
        '''
        Try to set the data. If attr is not a key, fall-back and set the attr
        '''
        if self.has_key(attr):
            super(OrderedAttrDict, self).__setitem__(attr, value)
        else:
            super(OrderedAttrDict, self).__setattr__(attr, value)
