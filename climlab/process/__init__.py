'''The base classes for all climlab processes.'''
from __future__ import absolute_import
from .process import Process, process_like, get_axes
from .time_dependent_process import TimeDependentProcess, couple
from .implicit import ImplicitProcess
from .diagnostic import DiagnosticProcess
from .energy_budget import EnergyBudget
from .external_forcing import ExternalForcing
