# -*- coding: utf-8 -*-
"""
Created on Thu Jan  8 15:30:13 2015

@author: Brian
"""

# in more oop climlab,
#  every model will consist of:
#   - a collection of state variables, and
#   - a collection of "processes"
#   - each "process" is a submodel
#   - each state variable exists in connection with a grid object
#   - but not all state variables need be on same grid
#   - e.g. surface temp, atm. column
#  - each process is an object that computes instantaneous rates of change on state variables
#   - and also arbitrary diagnostics
#  the model parent class just has to cycle through each process
#   and get all tendencies

#  all tendencies should have units [statevar unit] / second

import numpy as np
from climlab.process import Process
import heatCapacity

class AplusBT(Process):
    def compute(self):
        A = 200.
        B = 2.
        water_depth = 20.
        T = self.state['Ts'].data
        OLR = A + B*T
        self.tendencies['Ts'] = -OLR / heatCapacity.slab_ocean(water_depth)

#class GreyRadiation(process)
    