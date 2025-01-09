import numpy as np
import climlab
from climlab.convection import SimplifiedBettsMiller
from climlab.tests.xarray_test import to_xarray
import pytest


@pytest.mark.compiled
@pytest.mark.fast
def test_sbm_column():
    num_lev = 30
    water_depth = 10.
    full_state = climlab.column_state(water_depth=water_depth, num_lev=num_lev)
    conv = SimplifiedBettsMiller(name='Convection',
                                 state=full_state,)
    conv.step_forward()
