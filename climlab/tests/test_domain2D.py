import numpy as np
import climlab
from climlab import domain
import pytest

def test_state():
    initialT0 = 15.
    sfc = domain.surface_2D(num_lat=90, num_lon=180)
    sfc = domain.surface_2D(lat=([-90.,0.,90.]),
                            lon=([-180.,0.,180.]))
    state = climlab.surface_state(T0 = initialT0, num_lat=90, num_lon=180)
    assert state.Ts.ndim == 3
    assert state.Ts.shape == (90, 180, 1)
    assert np.isclose(climlab.global_mean(state.Ts), initialT0, atol=1E-02)
