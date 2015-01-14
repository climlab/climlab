import time
from climlab.grid import Grid


class _Process(object):
    '''A generic parent class for all climlab process objects.
    Every process object has a set of state variables on a spatial grid.
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
        # dictionary of gridded properties (usually fixed)
        self.properties = {}
        self.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                           time.localtime())
        # subprocess is a dictionary of any sub-processes
        self.subprocess = {}
        self.has_process_type_list = False
