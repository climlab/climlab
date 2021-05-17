'''
This chapter documents the source code of the ``climlab`` package.
The focus is on the methods and functions that the user invokes
while using the package.

Nevertheless also the underlying code of the ``climlab`` architecture
has been documented for a comprehensive understanding and traceability.
'''

__version__ = '0.7.12'

# this should ensure that we can still import constants.py as climlab.constants
from .utils import constants, thermo, legendre
# some more useful shorcuts
from .model.column import GreyRadiationModel, RadiativeConvectiveModel, BandRCModel
from .model.ebm import EBM, EBM_annual, EBM_seasonal
from .domain.field import Field, global_mean
from .domain.axis import Axis
from .domain.initial import column_state, surface_state
from .process import Process, TimeDependentProcess, ImplicitProcess, DiagnosticProcess, EnergyBudget
from .process import process_like, get_axes, couple
from .domain.xarray import to_xarray
