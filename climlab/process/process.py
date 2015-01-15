import time
from climlab.domain.domain import _Domain
import numpy as np
import copy


# new concept:
#   every process has a list of domains
#   every state variable has a single domain
#  There should be a dictionary of domains for state variables

# need more shortcuts for easy process and domain generation

class _Process(object):
    '''A generic parent class for all climlab process objects.
    Every process object has a set of state variables on a spatial grid.
    '''
    def __init__(self, domains=None, state=None, state_domain=None, param=None, **kwargs):
        #if grid is None:
        #    self.grid = Grid()
        #else:
        #    self.grid = grid
        # dictionary of state variables
        if state is None:
            self.state = {}
        else:
            self.state = state
        # make sure all state variables are numpy arrays
        for varname, value in self.state.iteritems():
            self.state[varname] = np.atleast_1d(value)
        # dictionary of model parameters
        if param is None:
            self.param = {}
        else:
            self.param = param
        # dictionary of domains. Keys are the domain names
        # make it possible to give just a single domain:
        if type(domains) is dict:
            self.domains = domains
        elif isinstance(domains, _Domain):
            self.domains = {'default': domains}
        else:
            raise ValueError('domains needs to be _Domain object or dictionary of _Domain object')
        if state_domain is None:
            self.state_domain = {}
            self._guess_state_domains()
        else:
            self.state_domain = state_domain
        #for name, value in state.iteritems():
        #    self.set_state(name, value,)
        # dictionary of diagnostic quantities
        self.diagnostics = {}
        # dictionary of other gridded properties (usually fixed)
        # these properties should also have domains... their keys added to self.domains
        #self.properties = {}
        self.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                           time.localtime())
        # subprocess is a dictionary of any sub-processes
        self.subprocess = {}
        self.has_process_type_list = False

    def set_state(self, name, value, domain):
        self.state[name] = value
        if type(domain) is str:
            thisdomain = self.domains[domain]
        elif type(domain) is _Domain:
            thisdomain = domain
        self.state_domain[name] = thisdomain
    #def set_property(self, name, value, domain):
    
    def _guess_state_domains(self):
        for name, value in self.state.iteritems():
            for domname, dom in self.domains.iteritems():
                if value.shape == dom.shape:
                    # same shape, assume it's the right domain
                    self.state_domain[name] = dom

def process_like(proc):
    '''Return a new process identical to the given process.
    The creation date is updated.'''
    newproc = copy.deepcopy(proc)
    newproc.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                           time.localtime())
    return newproc
