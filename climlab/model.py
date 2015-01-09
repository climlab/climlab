from netCDF4 import Dataset
import time

# New concept: every model is actually a sub-class of netCDF4.Dataset


class _Model(Dataset):
    '''An abstract parent class for all climlab models.
    Every model object is a sub-class of netCDF4.Dataset
    So we are using the netCDF conventions for storing grids,
    state variables, diagnostic quanitities, parameters, etc.'''
    def __init__(self, filename='ignored.nc', diskless=True, **kwargs):
        # Create a netCDF4.Dataset object
        super(_Model, self).__init__(filename=filename, mode="w",
                                     diskless=diskless, format='NETCDF4')
    # netCDF4 format allows for groups (and subgroups) of variables
    # useful here... keep state variables and diagnostic quantities seperate
        self.createGroup('state')
        self.createGroup('diagnostics')
        self.createGroup('params')
        self.createGroup('fixed')
        # Set some global attributes
        self.description1 = "Model object created by climlab"
        self.description2 = "Model type is " + str(type(self))
        self.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                           time.localtime())

    def param(self, name, value=None, paramType='float'):
        '''Convenience method for adding, modifying or retrieving 
        scalar model parameter.
        If value is not given, the method will return existing value.
        If name and value are given, new parameter will be created or updated.
        If parameter type is not float, it needs to be specified.'''
        if value is None:
            try:
                return self.groups['params'].variables[name][:]
            except:
                raise ValueError('No parameter %s found.' % name)
        else:
            # First check to see if this parameter already exists.
            # If not, create it.
            if name not in self.groups['params'].variables:
                # Create a new scalar variable in parameters group
                self.groups['params'].createVariable(name, paramType)
                # Assign the new value
                self.groups['params'].variables[name][:] = value

    def param_plusone(self, name):
        '''Convenience method for adding 1 (integer) to a parameter.'''
        self.groups['params'].variables[name][:] += 1
