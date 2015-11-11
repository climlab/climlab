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

Notes on implementation:
- Regardless of details of storage, `process.state` must give an iterable dict
over *only* the state variables
- probably best to use internal storage and properties:
    - `process._state` is of type `xray.Dataset`
    - `process.state` is a property that returns `process._state.data_vars` only
    - same for other dictionaries: input, diagnostics, etc.
    - None of the public API should be dependent on `xray.Dataset` features
    -  NEED TO CHOOSE diagnostic vs diagnostics...
    - as much as possible, use properties to re-enable the v0.2 features of
    domains and grids
- `process.param` should be stored internally as `process._state.attrs`
so they are always tied to state variables.
'''

import time, copy
import numpy as np
#from climlab.domain.field import Field
#from climlab.domain.domain import _Domain
from climlab.utils import walk, attr_dict
import xray
from xray.core.utils import is_dict_like

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

# def _make_dict(arg, argtype):
#     if arg is None:
#         return {}
#     elif type(arg) is dict:
#         return arg
#     elif isinstance(arg, argtype):
#         return {'default': arg}
#     else:
#         raise ValueError('Problem with input type')


class Process(object):
    '''A generic parent class for all climlab process objects.
    Every process object has a set of state variables on a spatial grid.
    '''

    def __str__(self):
        str1 = 'climlab Process of type {0}. \n'.format(type(self))
        str1 += 'State variables and domain shapes: \n'
        for varname in self.state.keys():
            str1 += '  {0}: {1} \n'.format(varname, self.domains[varname].shape)
        str1 += 'The subprocess tree: \n'
        str1 += walk.process_tree(self)
        return str1

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
    def __init__(self, state=None, subprocess=None,
                     lat=None, lev=None, num_lat=None, num_levels=None,
                     input=None, param=None, **kwargs):
        coords = {}
        if lat is not None:
            coords.update({'lat': lat})
        if lev is not None:
            coords.update({'lev': lev})
        super(Process, self).__init__(coords=coords, **kwargs)
        self.attrs['creation_date'] = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                           time.localtime())
        # subprocess is an ordered dictionary of sub-processes
        if subprocess is None:
            self.subprocess = attr_dict.OrderedAttrDict()
        else:
            if is_dict_like(subprocess):
                self.subprocess = attr_dict.OrderedAttrDict(subprocess)
            else:
                raise ValueError('Need a dict-like container of subproceses.')
            #self.add_subprocesses(subprocess)

        if input is None:
            self.input = attr_dict.AttrDict()
        else:
            if is_dict_like(input):
                self.input = attr_dict.AttrDict(input)
            else:
                raise ValueError('Need a dict-like container of input.')

        self.diagnostic = attr_dict.AttrDict()

    # def add_subprocesses(self, procdict):
    #     '''Add a dictionary of subproceses to this process.
    #     procdict is dictionary with process names as keys.
    #
    #     Can also pass a single process, which will be called \'default\'
    #     '''
    #     if isinstance(procdict, Process):
    #         self.add_subprocess('default', procdict)
    #     else:
    #         for name, proc in procdict.iteritems():
    #             self.add_subprocess(name, proc)
    #
    # def add_subprocess(self, name, proc):
    #     '''Add a single subprocess to this process.
    #     name: name of the subprocess (str)
    #     proc: a Process object.'''
    #     if isinstance(proc, Process):
    #         self.subprocess.update({name: proc})
    #         self.has_process_type_list = False
    #     else:
    #         raise ValueError('subprocess must be Process object')
    #
    # def remove_subprocess(self, name):
    #     '''Remove a single subprocess from this process.
    #     name: name of the subprocess (str)'''
    #     self.subprocess.pop(name, None)
    #     self.has_process_type_list = False

    # def _subset_by_var_type(self, type):
    # 	'''Return a new xray.Dataset object containing just variables of specified type.'''
    # 	typedict = {}
    # 	for name, var in self.data_vars.iteritems():
    #         if 'var_type' in var.attrs:
    #             if var.var_type is type:
    #                 typedict[name] = var
    # 	#typedict = {key:value for key,value in self.data_vars.items() if value.var_type is type}
    # 	return xray.Dataset(typedict)

    @property
    def state(self):
        '''Returns a new xray.Dataset object containing just state variables.
        '''
        #return self._subset_by_var_type('state')
        return self.data_vars

    @property
    def param(self):
        return self.attrs
    @param.setter
    def param(self, paramdict):
        if is_dict_like(paramdict):
            for name, value in paramdict.iteritems():
                self.attrs[name] = value
        else:
            raise ValueError('param must be a dict-like collection')


    # @property
    # def input(self):
    #     '''Returns a new xray.Dataset object containing just state variables.
    #     '''
    #     return self._subset_by_var_type('input')
    #
    # @property
    # def diagnostics(self):
    #     '''Dictionary of xray.DataArray objects corresponding to input variables
    #     '''
    #     return self._subset_by_var_type('diag')

    def set_state(self, var):
        '''Assign an xray.DataArray object as state variable of the process.'''
        self[var.name] = var
        #self[var.name].attrs['var_type'] = 'state'

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
