'''
climlab wrap of the CAM3 radiation code
'''
from __future__ import division
import numpy as np
import netCDF4 as nc
from climlab import constants as const
from climlab.utils.thermo import vmr_to_mmr
from climlab.radiation.radiation import _Radiation_SW, _Radiation_LW
import os
#  the compiled fortran extension
import _cam3
from cam3 import CAM3, CAM3_LW, CAM3_SW


# Initialise absorptivity / emissivity data
here = os.path.dirname(__file__)
#datadir = os.path.abspath(os.path.join(here, os.pardir, 'data', 'cam3'))
datadir = os.path.join(here, 'data')
AbsEmsDataFile = os.path.join(datadir, 'abs_ems_factors_fastvx.c030508.nc')
#  Open the absorption data file
data = nc.Dataset(AbsEmsDataFile)
#  The fortran module that holds the data
mod = _cam3.absems
#  Populate storage arrays with values from netcdf file
for field in ['ah2onw', 'eh2onw', 'ah2ow', 'ln_ah2ow', 'cn_ah2ow', 'ln_eh2ow', 'cn_eh2ow']:
    setattr(mod, field, data.variables[field][:].T)
data.close()
