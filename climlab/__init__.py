__version__ = '0.2'

#  This list defines all the modules that will be loaded if a user invokes
#   from climLab import *

# totally out of date!

#__all__ = ["constants", "thermo", "orbital_table",
#           "long_orbital_table", "insolation", "ebm",
#           "column", "convadj"]

# this should ensure that we can still import constants.py as climlab.constants           
from climlab.utils import constants, thermo
# some more useful shorcuts
from climlab.model import ebm, column
from climlab.domain import domain
from climlab.domain.field import Field, global_mean
from climlab.domain.axis import Axis
from climlab.process.process import Process, process_like, get_axes
from climlab.process.time_dependent_process import TimeDependentProcess
from climlab.process.implicit import ImplicitProcess
from climlab.process.diagnostic import DiagnosticProcess
from climlab.process.energy_budget import EnergyBudget
