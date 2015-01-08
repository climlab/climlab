# -*- coding: utf-8 -*-
"""
Created on Thu Jan  8 16:06:46 2015

@author: Brian
"""
import numpy as np


# a class to hold state variables
#  each state variable has a name
#  a grid
#  and a data array


class StateVar(object):
    def __init__(self, varName, grid, data=None, long_name=None, units=None):
        self.varName = varName
        self.grid = grid
        self.long_name = long_name
        self.units = units
        if data is None:
            self.data = np.zeros(grid.stateDim)
        else:
            if grid.stateDim != data.shape:
                raise ValueError('Mismatch between grid and data.')
