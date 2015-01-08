# -*- coding: utf-8 -*-
"""
Created on Thu Jan  8 15:52:52 2015

@author: Brian
"""

import numpy as np


# abstract class that defines a climate process (sub-model)
class Process(object):
    def __init__(self, state):
        self.state = state
        self.tendencies = {}
        for varname, var in self.state.iteritems():
            self.tendencies[varname] = np.zeros_like(var)
        self.diagnostics = {}

    def compute():
        # compute tendencies given current state
        pass
