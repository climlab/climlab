'''
This chapter documents the source code of the ``climlab`` package.
The focus is on the methods and functions that the user invokes
while using the package.

Nevertheless also the underlying code of the ``climlab`` architecture
has been documented for a comprehensive understanding and traceability.
'''

__version__ = '0.6.2'

# this should ensure that we can still import constants.py as climlab.constants
from climlab.utils import constants
from climlab.utils import thermo, legendre
# some more useful shorcuts
from climlab.model.column import GreyRadiationModel, RadiativeConvectiveModel, BandRCModel
from climlab.model.ebm import EBM, EBM_annual, EBM_seasonal
from climlab.domain.field import Field, global_mean
from climlab.domain.axis import Axis
from climlab.domain.initial import column_state, surface_state
from climlab.process import Process, TimeDependentProcess, ImplicitProcess, DiagnosticProcess, EnergyBudget
from climlab.process import process_like, get_axes
from climlab.domain.xarray import to_xarray
