'''
Principles of the new `climlab` API design:

- `climlab.Process` object has several dictionaries of named, gridded variables:
    - `process.state`
    - `process.diagnostic`
    - `process.input`
    - `process.param`  (which are basically just scalar input)
- `climlab` will remain (as much as possible) agnostic about the data formats
- But each dictionary will actually be a `xray.Dataset` object
- Many variables will be accessible as attributes `process.name`
    - this restricts to unique field names in the above dictionaries
- There may be other dictionaries that do have name conflicts
    - e.g. dictionary of heat capacity, with same keys as `process.state`
    - These will *not* be accessible as `process.name`
    - but *will* be accessible as `process.dict_name.name`
    (as well as regular dict interface)
    - will use `property` declarations as needed, e.g. to allow
    `process.Ts.heat_capacity` to reference `process.heat_capacity['Ts']` 
- Grid information (axes) will be accessible as attributes of variables
    - e.g. `process.Ts.lat`
    - This will be automatic if fields are stored as `xray.DataArray` objects
    - Shortcuts like `process.lat` will work where these are unambiguous
- There will be a dictionary of named subprocesses `process.subprocess`
- Each item in subprocess dict will itself be a `climlab.Process` object
- subprocesses should also be accessible as process attributes
- `process.compute()`) is a method that computes tendencies (d/dt)
    - returns a dictionary (or `xray.Dataset`) of tendencies for all state vars
    - keys for this dictionary are same as keys of state dictionary
    - tendency dictionary is the total tendency including all subprocesses
    - method only computes d/dt, does not apply changes
    - thus method is relatively independent of numerical scheme
        - may need to make exception for implicit scheme?
    - method *will* update diagnostic variables
- `process.step_forward()` updates the state variables
    - calls `process.compute()` to get current tendencies
    - implements a particular time-stepping scheme
    - user interface is agnostic about numerical scheme
- `process.integrate_years()` etc will automate time-stepping
    - also computation of time-average diagnostics.


'''
import time, copy
import numpy as np
#from climlab.domain.field import Field
#from climlab.domain.domain import _Domain
from climlab.utils import walk
import xray


#  New concept:
#   every climlab process is actually a subclass of xray.Dataset
#   which is an in-memory netcdf-like database of N-dimensional gridded data
#  So will no longer use custom Field and Domain classes

#  Scalar parameters will be stored in the attributes dictionary .attrs
#  which means they are always accessible as process.param_name

#  Each process will have several different kinds of data, aside from scalar params:
#   - state variables
#	- input data
#	- diagnostics

#  These will all be stored as xray.DataArrays in the Process object, and thus accessible
#  either through the .data_vars dict interface or directly as attributes of the Process

#  But, importantly, we will extend the Dataset interface to distinguish between different
#  types of data. Each DataArray will have an attribute 'var_type' to declare whether it is
#  state, input, diag, and maybe others

#  This will extend the Dataset.data_vars attribute. Look at the xray code and emulate it.

#  There will be an attribute like Dataset.data_vars that returns dictionaries of just
#  specific types of variables, so Process.state will return dict of state vars
#   just like it did in climlab v 0.2.x

#  Because all data is ultimately stored in a single object / dictionary,
#   there is a restriction on names. Can't store tendencies with same names as state vars
#  But that's okay and will actually clean up the interface

#  Because the compute() method for each process will take current state and input data
#   and RETURN a new Dataset with the tendencies (with same keys as state vars)
#  (which CAN be attached as attributes of the Process object, but
#   probably better and cleaner not to do this)
#  If you want to store and retain tendencies, better for the Process to
#  create a diagnostic variable with a descriptive name, e.g. Ts_tendency

#  I am also changing the logic for handling subprocesses.
#  In the spirit of modular, self-contained Processes, each Process object has a data type
#  		Process.input
#  which contains all the input and boundary values needed.
#  Often these will actually be set by a parent Process.

#  But the compute() method of each Process will iterate over all subprocesses
#  to generate the net tendency for that Process including all subprocesses

#  The step_forward() method will no longer have to 'walk the subprocess tree'
#  because the compute() method of the top-level process will automatically call all
#  the underlying subprocesses. No process needs (or should) search deeper in the tree
#  than its own immediate subprocesses.

def _make_dict(arg, argtype):
    if arg is None:
        return {}
    elif type(arg) is dict:
        return arg
    elif isinstance(arg, argtype):
        return {'default': arg}
    else:
        raise ValueError('Problem with input type')


class Process(xray.Dataset):
    '''A generic parent class for all climlab process objects.
    Every process object has a set of state variables on a spatial grid.
    '''
#    def __str__(self):
#        str1 = 'climlab Process of type {0}. \n'.format(type(self))
#        str1 += 'State variables and domain shapes: \n'
#        for varname in self.state.keys():
#            str1 += '  {0}: {1} \n'.format(varname, self.domains[varname].shape)
#        str1 += 'The subprocess tree: \n'
#        str1 += walk.process_tree(self)
#        return str1

#    def __init__(self, state=None, domains=None, subprocess=None,
#                 lat=None, lev=None, num_lat=None, num_levels=None,
#                 diagnostics=None, **kwargs):
#        # dictionary of domains. Keys are the domain names
#        self.domains = _make_dict(domains, _Domain)
#        # dictionary of state variables (all of type Field)
#        self.state = {}
#        states = _make_dict(state, Field)
#        for name, value in states.iteritems():
#            self.set_state(name, value)
#        # dictionary of model parameters
#        self.param = kwargs
#        # dictionary of diagnostic quantities
#        self.diagnostics = _make_dict(diagnostics, Field)
#        self.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
#                                           time.localtime())
#        # subprocess is a dictionary of any sub-processes
#        if subprocess is None:
#            self.subprocess = {}
#        else:
#            self.add_subprocesses(subprocess)
    def __init__(self, lat=None, lev=None, num_lat=None, num_levels=None,
                 subprocess=None, **kwargs):
        coords = {}
        if lat is not None:
            coords.update({'lat': lat})
        if lev is not None:
            coords.update({'lev': lev})
        super(Process, self).__init__(coords=coords, **kwargs)
        self.attrs['creation_date'] = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                           time.localtime())
        # subprocess is a dictionary of any sub-processes
        if subprocess is None:
            self.subprocess = {}
        else:
            self.add_subprocesses(subprocess)

    def add_subprocesses(self, procdict):
        '''Add a dictionary of subproceses to this process.
        procdict is dictionary with process names as keys.

        Can also pass a single process, which will be called \'default\'
        '''
        if isinstance(procdict, Process):
            self.add_subprocess('default', procdict)
        else:
            for name, proc in procdict.iteritems():
                self.add_subprocess(name, proc)

    def add_subprocess(self, name, proc):
        '''Add a single subprocess to this process.
        name: name of the subprocess (str)
        proc: a Process object.'''
        if isinstance(proc, Process):
            self.subprocess.update({name: proc})
            self.has_process_type_list = False
        else:
            raise ValueError('subprocess must be Process object')

    def remove_subprocess(self, name):
        '''Remove a single subprocess from this process.
        name: name of the subprocess (str)'''
        self.subprocess.pop(name, None)
        self.has_process_type_list = False

    def _subset_by_var_type(self, type):
    	'''Return a new xray.Dataset object containing just variables of specified type.'''
    	typedict = {}
    	for name, var in self.data_vars.iteritems():
            if 'var_type' in var.attrs:
                if var.var_type is type:
                    typedict[name] = var
    	#typedict = {key:value for key,value in self.data_vars.items() if value.var_type is type}
    	return xray.Dataset(typedict)

    @property
    def state(self):
        '''Returns a new xray.Dataset object containing just state variables.
        '''
        return self._subset_by_var_type('state')
    @property
    def input(self):
        '''Returns a new xray.Dataset object containing just state variables.
        '''
        return self._subset_by_var_type('input')

    @property
    def diagnostics(self):
        '''Dictionary of xray.DataArray objects corresponding to input variables
        '''
        return self._subset_by_var_type('diag')

    def set_state(self, var):
        '''Assign an xray.DataArray object as state variable of the process.'''
        self[var.name] = var
        self[var.name].attrs['var_type'] = 'state'
#==============================================================================
#     def set_state(self, name, value):
#         if isinstance(value, Field):
#             # populate domains dictionary with domains from state variables
#             self.domains.update({name: value.domain})
#         else:
#             try:
#                 thisdom = self.state[name].domain
#                 domshape = thisdom.shape
#             except:
#                 raise ValueError('State variable needs a domain.')
#             value = np.atleast_1d(value)
#             if value.shape == domshape:
#                 value = Field(value, domain=thisdom)
#             else:
#                 raise ValueError('Shape mismatch between existing domain and new state variable.')
#         # set the state dictionary
#         self.state[name] = value
#         setattr(self, name, value)
#
#     def _guess_state_domains(self):
#         for name, value in self.state.iteritems():
#             for domname, dom in self.domains.iteritems():
#                 if value.shape == dom.shape:
#                     # same shape, assume it's the right domain
#                     self.state_domain[name] = dom
#
#     # Some handy shortcuts... only really make sense when there is only
#     # a single axis of that type in the process.
#     @property
#     def lat(self):
#         try:
#             for domname, dom in self.domains.iteritems():
#                 try:
#                     thislat = dom.axes['lat'].points
#                 except:
#                     pass
#             return thislat
#         except:
#             raise ValueError('Can\'t resolve a lat axis.')
#
#     @property
#     def lat_bounds(self):
#         try:
#             for domname, dom in self.domains.iteritems():
#                 try:
#                     thislat = dom.axes['lat'].bounds
#                 except:
#                     pass
#             return thislat
#         except:
#             raise ValueError('Can\'t resolve a lat axis.')
#     @property
#     def lon(self):
#         try:
#             for domname, dom in self.domains.iteritems():
#                 try:
#                     thislon = dom.axes['lon'].points
#                 except:
#                     pass
#             return thislon
#         except:
#             raise ValueError('Can\'t resolve a lon axis.')
#     @property
#     def lon_bounds(self):
#         try:
#             for domname, dom in self.domains.iteritems():
#                 try:
#                     thislon = dom.axes['lon'].bounds
#                 except:
#                     pass
#             return thislon
#         except:
#             raise ValueError('Can\'t resolve a lon axis.')
#     @property
#     def lev(self):
#         try:
#             for domname, dom in self.domains.iteritems():
#                 try:
#                     thislev = dom.axes['lev'].points
#                 except:
#                     pass
#             return thislev
#         except:
#             raise ValueError('Can\'t resolve a lev axis.')
#     @property
#     def lev_bounds(self):
#         try:
#             for domname, dom in self.domains.iteritems():
#                 try:
#                     thislev = dom.axes['lev'].bounds
#                 except:
#                     pass
#             return thislev
#         except:
#             raise ValueError('Can\'t resolve a lev axis.')
#     @property
#     def depth(self):
#         try:
#             for domname, dom in self.domains.iteritems():
#                 try:
#                     thisdepth = dom.axes['depth'].points
#                 except:
#                     pass
#             return thisdepth
#         except:
#             raise ValueError('Can\'t resolve a depth axis.')
#     @property
#     def depth_bounds(self):
#         try:
#             for domname, dom in self.domains.iteritems():
#                 try:
#                     thisdepth = dom.axes['depth'].bounds
#                 except:
#                     pass
#             return thisdepth
#         except:
#             raise ValueError('Can\'t resolve a depth axis.')
#
#==============================================================================

def process_like(proc):
    '''Return a new process identical to the given process.
    The creation date is updated.'''
    newproc = copy.deepcopy(proc)
    newproc.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                          time.localtime())
    return newproc


def get_axes(process_or_domain):
    '''Return a dictionary of all axes in a domain or dictionary of domains.'''
    if isinstance(process_or_domain, Process):
        dom = process_or_domain.domains
    else:
        dom = process_or_domain
    if isinstance(dom, _Domain):
        return dom.axes
    elif isinstance(dom, dict):
        axes = {}
        for thisdom in dom.values():
            assert isinstance(thisdom, _Domain)
            axes.update(thisdom.axes)
        return axes
    else:
        raise TypeError('dom must be a domain or dictionary of domains.')
