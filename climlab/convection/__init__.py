'''
Modules for atmospheric convection.

Currently limited to simple hard convective adjustment to prescribed lapse rate.
'''
from __future__ import absolute_import
from .convadj import ConvectiveAdjustment
from .emanuel_convection import EmanuelConvection
