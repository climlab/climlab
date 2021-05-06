'''
Modules for self-describing gridded fields in climlab.
'''
__all__ = ['axis', 'domain', 'field', 'initial', 'xarray']

from climlab.domain.domain import single_column, zonal_mean_surface, surface_2D, zonal_mean_column, box_model_domain
from climlab.domain.initial import column_state, surface_state
from climlab.domain.field import Field, global_mean
from climlab.domain.axis import Axis
