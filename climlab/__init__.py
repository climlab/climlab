__version__ = '0.4.2'

#  This list defines all the modules that will be loaded if a user invokes
#   from climLab import *

# totally out of date!

#__all__ = ["constants", "thermo", "orbital_table",
#           "long_orbital_table", "insolation", "ebm",
#           "column", "convadj"]

#from climlab import radiation
# this should ensure that we can still import constants.py as climlab.constants
from climlab.utils import constants
from climlab.utils import thermo, legendre
# some more useful shorcuts
#from climlab.model import ebm, column
from climlab.model.column import GreyRadiationModel, RadiativeConvectiveModel, BandRCModel
from climlab.model.ebm import EBM, EBM_annual, EBM_seasonal
from climlab.domain import domain
from climlab.domain.field import Field, global_mean
from climlab.domain.axis import Axis
from climlab.domain.initial import column_state, surface_state
from climlab.process import Process, TimeDependentProcess, ImplicitProcess, DiagnosticProcess, EnergyBudget
from climlab.process import process_like, get_axes
