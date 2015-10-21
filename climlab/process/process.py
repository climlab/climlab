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
#
#  dimensional but fixed quantities will be stored as DataArrays just like
#   state variables but will simply not get updated

#   ...
#  The previous climlab data structure had a bunch of named dictionaries
#  of data arrays: state, tendencies, heating_rate, etc
#  Would be nice to clean and rationalize all this
#  and get back to a purer model:
#   The process object should consist of state variables, attributes
#  only what is really need to fully define the current state and compute
#  the next timestep
#
#  There should be methods to COMPUTE all diagnostic quantities
#  and probably return them in new Dataset objects
#  But don't clutter the process object with all these extra fields!

#  but... every process also needs inputs and boundary values!
#  these are not state vars but need to be provided somehow.

#  Does the compute() method of each process accept another Dataset
#  as input argument, providing all the forcing/boundary values?

#  COULD make code for complex coupled models more straightforward.
#  or it could be a mess

# Rather than storing a dictionary of tendencies,
#  can each process have a method
#  process.compute(inputData)
# that returns a Dataset object containing the tendencies (on same grid as state)?

#  OK this is what we will try:
#   the data_vars dictionary of every process contains ONLY state variables!
#  Everything else is computed by methods and returned as seperate datasets
#
#  This may prove to be too restrictive but let's try it out.

#  later... I already think this is too restrictive
#   each process object needs a state variables dict or Dataset
#   but it also needs a Dataset of input data
#  and produces a dataset of diagnostics and another of tendencies

#  I think it actually doesn't make sense to glue this all on to a parent
#  xray.Dataset object. The Process object should go back to a custom class
#  but each Process object will have several xray.Dataset members,
#  which should be accessible either through dictionary or attribute style

#  Basically a Process object should be a dict-like container of xray.Datasets
#   plus a compute method that generates tendencies and diagnostics using
#  the current state and the current input data

#  Might be useful to look at the source code for xray.Dataset to emulate it

#  ok... maybe we CAN use a single parent Dataset.
#  the key is to have a simple and robust way to differentiate between
#  different kinds of data: state, input, tendencies, diagnostics, etc.

#  But I think this should be pretty straightforward. Look at how the
#  Dataset.data_vars property is implemented. Just need to specialize this
#  so that there are distinct properties that return dicts of DataArrays
#  for each different kind of data. 
#  Then things should be nice and clean, because every field can be accessed
#   EITHER through its group dictionary, or directly as attribute of the Process
#  or through the master dictiondary Process.data_vars

#  But how will we name the fields? Because I can't use same keys for state
#  and tendency if they actually all reside in the same Dataset object!
#  could use names like 'Ts_tend' but that would get clunky.

#  So maybe we need a hybrid approach:
#   a single Dataset that contains at least three types of data:
#     - state variables
#       - input data
#       - diagnostics
#  along with properties that can distinguish between these
#   AND tendencies are handled by methods that return new Datasets
#   with same keys as state variables
#  (which CAN be attached as attributes of the Process object, but
#   probably better and cleaner not to do this)
#  If you want to store and retain tendencies, better for the Process to
#  create a diagnostic variable with a descriptive name, e.g. Ts_tendency

#  So... insist that each DataArray has an attribute 'type' that can be
#   state, diag, input

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
