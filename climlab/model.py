import time
from grid import Grid


#  fundamental process types (very climt-like):
#    - radiation
#    - dynamics
#    - convection
#    - turbulence

class _Model(object):
    '''A generic parent class for all climlab model objects.
    Every model object has a set of state variables on a spatial grid.
    '''
    def __init__(self, **kwargs):
        self.grid = Grid()
        # dictionary of model parameters
        self.param = {}
        # dictionary of model state variables
        self.state = {}
        # dictionary of diagnostic quantities
        self.diagnostics = {}
        self.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                           time.localtime())
        # Dictionary of the fundamental process types known to climlab
        #self.processes = {'radiation': None,
        #                  'dynamics': None,
        #                  'convection': None,
        #                  'turbulence': None,
        #                  'adjustment': None}
        # Do this as a list instead
        #self.processes = []
                                           
        # No it has to be a dictionary, but the key names are flexible
        self.processes = {}
