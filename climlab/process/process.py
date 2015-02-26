import time, copy
import numpy as np
from climlab.domain.field import Field
from climlab.domain.domain import _Domain
from climlab.utils import walk

#  New organizing principle:
#  processes are free to use object attributes to store whatever useful fields
# the need. If later revisions require some special action to occur upon 
# getting or setting the attribute, we can always implement a property to 
# make that happen with no change in the API
#
#  the diagnostics dictionary will instead be used expressly for the purpose 
# of passing fields up the process tree!
#  so will often only be set by a parent process (and given a name that is
#  appropriate from teh point of view of the parent process)
#
#  e.g. radiation fields in N-band models... currently would not be labeled
# in a way that lets the user know which process it came from

# TO DO:
#  go through every process code file
#  and implement process parameters as attributes, or as properties with
#  setter functions if necessary
#  it should always be possible to quickly and easily change a process 
#  parameter without messing anything up.
#
#  ALSO rationalize the use of diagnostics in all process code
#  as described above. Diagnostics are to be used FOR QUANTITIES THAT ARE TO
#  BE TIME AVERAGED or quantities that are of interest to parent processes.
#  It might often be left up to the parent process whether to make a diagnostic
#
#  FINALLY we should implement a time_average list that controls which state
# variables and diagnostics are to be time averaged by the master process.
#  It should default to the complete list, which is the current behavior.
#


def _make_dict(arg, argtype):
    if arg is None:
        return {}
    elif type(arg) is dict:
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
                 diagnostics=None, **kwargs):
        # dictionary of domains. Keys are the domain names
        self.domains = _make_dict(domains, _Domain)
        # dictionary of state variables (all of type Field)
        self.state = {}
        states = _make_dict(state, Field)
        for name, value in states.iteritems():
            self.set_state(name, value)
        # dictionary of model parameters
        self.param = kwargs
        # dictionary of diagnostic quantities
        self.diagnostics = _make_dict(diagnostics, Field)
        self.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
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
    
