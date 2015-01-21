import time
from climlab.domain.field import Field
from climlab.domain.domain import _Domain
import copy
from climlab.utils import walk


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
                 input=None, properties=None, diagnostics=None, **kwargs):

        # dictionary of state variables (all of type Field)
        self.state = _make_dict(state, Field)        
        # dictionary of model parameters
        self.param = kwargs
        
        # now domains are attached to all state and diagnostic quantities
        # dictionary of domains. Keys are the domain names
        self.domains = _make_dict(domains, _Domain)
        for varname, value in self.state.iteritems():
            self.domains.update({varname: value.domain})
        # dictionary of diagnostic quantities
        self.diagnostics = _make_dict(diagnostics, Field)
        # dictionary of other gridded properties (usually fixed)
        # these properties should also have domains... their keys added to self.domains
        self.properties = _make_dict(diagnostics, Field)
        # dictionary of input fields (usually filled by the parent process)
        self.input = _make_dict(input, Field)
        self.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                           time.localtime())
        # subprocess is a dictionary of any sub-processes
        if subprocess is None:
            self.subprocess = {}
        elif type(subprocess) is dict:
            self.subprocess = subprocess
        elif isinstance(subprocess, Process):
            self.subprocess = {'default': subprocess}
        else:
            raise ValueError('subprocess must be Process object or dictionary of Processes')
        self.has_process_type_list = False

    def set_state(self, name, value):
        self.state[name] = value
        self.domains.update({name: value.domain})
    #def set_property(self, name, value, domain):
    
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
    
