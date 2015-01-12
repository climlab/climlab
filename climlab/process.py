# -*- coding: utf-8 -*-
"""
Created on Thu Jan  8 15:52:52 2015

@author: Brian
"""

import numpy as np


# abstract class that defines a climate process (sub-model)
class _Process(object):
    def __init__(self, grid, state, param=None, is_explicit=True, **kwargs):
        self.state = state
        self.grid = grid
        self.param = param
        self.tendencies = {}
        for varname, var in self.state.iteritems():
            self.tendencies[varname] = np.zeros_like(var)
        self.diagnostics = {}
        self.is_explicit = is_explicit
        self.is_implicit = False
        self.is_adjustment = False

    def compute():
        # compute tendencies given current state
        pass
