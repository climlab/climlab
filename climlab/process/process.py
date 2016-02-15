'''
Principles of the new `climlab` API design:

- `climlab.Process` object has several iterable dictionaries of named,
gridded variables:
    - `process.state`
        - state variables, usually time-dependent
    - `process.input`
        - boundary conditions and other gridded quantities independent of the
        `process`
        - often set by a parent `process`
    - `process.param`  (which are basically just scalar `input`)
    - `process.tendencies`
        - iterable `dict` of time-tendencies (d/dt) for each state variable
    - `process.diagnostics`
        - any quantity derived from current state
- The `process` is fully described by contents of `state`, `input` and `param`
dictionaries. `tendencies` and `diagnostics` are always computable from current
state.
- `climlab` will remain (as much as possible) agnostic about the data formats
    - Variables within the dictionaries will behave as `numpy.ndarray` objects
    - Grid information and other domain details accessible as attributes
    of each variable
        - e.g. Tatm.lat
        - Shortcuts like `process.lat` will work where these are unambiguous
- Many variables will be accessible as process attributes `process.name`
    - this restricts to unique field names in the above dictionaries
- There may be other dictionaries that do have name conflicts
    - e.g. dictionary of tendencies, with same keys as `process.state`
    - These will *not* be accessible as `process.name`
    - but *will* be accessible as `process.dict_name.name`
    (as well as regular dict interface)
- There will be a dictionary of named subprocesses `process.subprocess`
- Each item in subprocess dict will itself be a `climlab.Process` object
- For convenience with interactive work, each subprocess should be accessible
as `process.subprocess.name` as well as `process.subprocess['name']`
- `process.compute()` is a method that computes tendencies (d/dt)
    - returns a dictionary of tendencies for all state variables
    - keys for this dictionary are same as keys of state dictionary
    - tendency dictionary is the total tendency including all subprocesses
    - method only computes d/dt, does not apply changes
    - thus method is relatively independent of numerical scheme
        - may need to make exception for implicit scheme?
    - method *will* update variables in `process.diagnostic`
        - will also *gather all diagnostics* from `subprocesses`
- `process.step_forward()` updates the state variables
    - calls `process.compute()` to get current tendencies
    - implements a particular time-stepping scheme
    - user interface is agnostic about numerical scheme
- `process.integrate_years()` etc will automate time-stepping
    - also computation of time-average diagnostics.
- Every `subprocess` should work independently of its parent `process` given
appropriate `input`.
    - investigating an individual `process` (possibly with its own
    `subprocesses`) isolated from its parent needs to be as simple as doing:
        - `newproc = climlab.process_like(procname.subprocess['subprocname'])`
        - `newproc.compute()`
        - anything in the `input` dictionary of `subprocname` will remain fixed
'''
import time, copy
import numpy as np
from climlab.domain.field import Field
from climlab.domain.domain import _Domain, zonal_mean_surface
from climlab.utils import walk, attr_dict


def _make_dict(arg, argtype):
    if arg is None:
        return {}
    elif isinstance(arg, dict):
        return arg
    elif isinstance(arg, argtype):
        return {'default': arg}
    else:
        raise ValueError('Problem with input type')


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

    def __init__(self, state=None, domains=None, subprocess=None,
                 lat=None, lev=None, num_lat=None, num_levels=None,
                 input=None, **kwargs):
        # dictionary of domains. Keys are the domain names
        self.domains = _make_dict(domains, _Domain)
        #  If lat is given, create a simple domains
        if lat is not None:
            sfc = zonal_mean_surface()
            self.domains.update({'default': sfc})
        # dictionary of state variables (all of type Field)
        self.state = attr_dict.AttrDict()
        states = _make_dict(state, Field)
        for name, value in states.iteritems():
            self.set_state(name, value)
        # dictionary of model parameters
        self.param = kwargs
        # dictionary of diagnostic quantities
        self.diagnostics = attr_dict.AttrDict()
        # dictionary of input quantities
        #self.input = _make_dict(input, Field)
        if input is None:
            self._input_vars = frozenset()
        else:
            self.add_input(input.keys())
            for name, var in input:
                self.__dict__[name] = var
        self.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                           time.localtime())
        # subprocess is a dictionary of any sub-processes
        if subprocess is None:
            #self.subprocess = {}
            # a dictionary whose items can be accessed as attributes
            self.subprocess = attr_dict.AttrDict()
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
            # make subprocess available as object attribute
            #setattr(self, name, proc)
        else:
            raise ValueError('subprocess must be Process object')

    def remove_subprocess(self, name):
        '''Remove a single subprocess from this process.
        name: name of the subprocess (str)'''
        self.subprocess.pop(name, None)
        self.has_process_type_list = False
        #  Since we made every subprocess an object attribute, we also remove
        #delattr(self, name)

    def set_state(self, name, value):
        if isinstance(value, Field):
            # populate domains dictionary with domains from state variables
            self.domains.update({name: value.domain})
        else:
            try:
                thisdom = self.state[name].domain
                domshape = thisdom.shape
            except:
                raise ValueError('State variable needs a domain.')
            value = np.atleast_1d(value)
            if value.shape == domshape:
                value = Field(value, domain=thisdom)
            else:
                raise ValueError('Shape mismatch between existing domain and new state variable.')
        # set the state dictionary
        self.state[name] = value
        setattr(self, name, value)

    def _guess_state_domains(self):
        for name, value in self.state.iteritems():
            for domname, dom in self.domains.iteritems():
                if value.shape == dom.shape:
                    # same shape, assume it's the right domain
                    self.state_domain[name] = dom

    def _set_field(self, field_type, name, value):
        '''Add a new field to a specified dictionary. The field is also added
        as a process attribute. field_type can be 'input', 'diagnostics' '''
        try:
            self.__getattribute__(field_type).update({name: value})
        except:
            raise ValueError('Problem with field_type %s'  %field_type)
        #  Note that if process has attribute name, this will trigger The
        # setter method for that attribute
        self.__setattr__(name, value)

    def init_diagnostic(self, name, value=0.):
        '''Define a new diagnostic quantity called name
        and initialize it with the given value.

        quantity is accessible and settable in two ways:
        - as a process attribute, i.e. proc.name
        - as a member of the diagnostics dictionary, i.e. proc.diagnostics['name']
        '''
        def _diag_getter(self):
            return self.diagnostics[name]
        def _diag_setter(self, value):
            self.diagnostics[name] = value
        setattr(type(self), name,
                property(fget=_diag_getter, fset=_diag_setter))
        self.__setattr__(name, value)

    def remove_diagnostic(self, name):
        '''Remove a diagnostic from the process.diagnostic dictionary
        and also delete the associated process attribute.'''
        _ = self.diagnostics.pop(name)
        delattr(type(self), name)

    def add_input(self, inputlist):
        '''Given a list of names of input variables, update the master list.'''
        self._input_vars = frozenset.union(self._input_vars, inputlist)

    # @property
    # def diagnostics(self):
    #     return { key:value for key, value in self.__dict__.items()
    #              if key in self._diag_vars }
    @property
    def input(self):
        return { key:value for key, value in self.__dict__.items()
                 if key in self._input_vars }

    # Some handy shortcuts... only really make sense when there is only
    # a single axis of that type in the process.
    @property
    def lat(self):
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislat = dom.axes['lat'].points
                except:
                    pass
            return thislat
        except:
            raise ValueError('Can\'t resolve a lat axis.')

    @property
    def lat_bounds(self):
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislat = dom.axes['lat'].bounds
                except:
                    pass
            return thislat
        except:
            raise ValueError('Can\'t resolve a lat axis.')
    @property
    def lon(self):
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislon = dom.axes['lon'].points
                except:
                    pass
            return thislon
        except:
            raise ValueError('Can\'t resolve a lon axis.')
    @property
    def lon_bounds(self):
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislon = dom.axes['lon'].bounds
                except:
                    pass
            return thislon
        except:
            raise ValueError('Can\'t resolve a lon axis.')
    @property
    def lev(self):
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislev = dom.axes['lev'].points
                except:
                    pass
            return thislev
        except:
            raise ValueError('Can\'t resolve a lev axis.')
    @property
    def lev_bounds(self):
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thislev = dom.axes['lev'].bounds
                except:
                    pass
            return thislev
        except:
            raise ValueError('Can\'t resolve a lev axis.')
    @property
    def depth(self):
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thisdepth = dom.axes['depth'].points
                except:
                    pass
            return thisdepth
        except:
            raise ValueError('Can\'t resolve a depth axis.')
    @property
    def depth_bounds(self):
        try:
            for domname, dom in self.domains.iteritems():
                try:
                    thisdepth = dom.axes['depth'].bounds
                except:
                    pass
            return thisdepth
        except:
            raise ValueError('Can\'t resolve a depth axis.')


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
