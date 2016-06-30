import climlab
from climlab import domain
from climlab.domain import initial
import pytest

def test_state():
    sfc = domain.surface_2D(num_lat=90, num_lon=180)
    sfc = domain.surface_2D(lat=([-90.,0.,90.]),
                            lon=([-180.,0.,180.]))
    state = initial.surface_state_2D(num_lat=90, num_lon=180)
    assert state.Ts.ndim == 3
    assert state.Ts[:,0,0].shape == (90,)
    assert state.Ts[0,:,0].shape == (180,)
    