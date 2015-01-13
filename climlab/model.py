import time
from grid import Grid


class _Model(object):
    '''A generic parent class for all climlab model objects.
    Every model object has a set of state variables on a spatial grid.
    '''
    def __init__(self, grid=None, param=None, state=None):
        if grid is None:
            self.grid = Grid()
        else:
            self.grid = grid
        # dictionary of model parameters
        if param is None:
            self.param = {}
        else:
            self.param = param
        # dictionary of model state variables
        if state is None:
            self.state = {}
        else:
            self.state = state
        # dictionary of diagnostic quantities
        self.diagnostics = {}
        self.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                           time.localtime())
                      
        myname = 'self'
        # processes is a dictionary of the model itself and any sub-processes
        self.processes = {myname: self}
