'''
Modules for atmospheric convection.

For simple adjustment of temperature to a prescribed lapse rate, use :class:`~climlab.convection.ConvectiveAdjustment`

For a full convection scheme including interactive water vapor, use :class:`~climlab.convection.EmanuelConvection`
'''
from __future__ import absolute_import
from .convadj import ConvectiveAdjustment
from .emanuel_convection import EmanuelConvection
