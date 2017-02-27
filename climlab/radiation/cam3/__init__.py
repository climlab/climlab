'''
climlab wrap of the CAM3 radiation code
'''
#  the compiled fortran extension
#import _cam3
from cam3 import CAM3, CAM3_LW, CAM3_SW
