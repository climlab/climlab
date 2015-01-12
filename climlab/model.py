import numpy as np
import constants as const
import time
from grid import Grid


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
