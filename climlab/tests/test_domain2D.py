from __future__ import division
import numpy as np
import climlab
import pytest

@pytest.mark.fast
def test_state():
    initialT0 = 15.
    sfc = climlab.domain.surface_2D(num_lat=90, num_lon=180)
    sfc = climlab.domain.surface_2D(lat=([-90.,0.,90.]),
                            lon=([-180.,0.,180.]))
    state = climlab.surface_state(T0 = initialT0, num_lat=90, num_lon=180)
    assert state.Ts.ndim == 3
    assert state.Ts.shape == (90, 180, 1)
    assert np.isclose(climlab.global_mean(state.Ts), initialT0, atol=1E-02)

@pytest.mark.fast
def test_2D_EBM():
    '''Can we step forward a 2D lat/lon EBM?'''
    #state = climlab.surface_state(num_lon=4)
    #m = climlab.EBM_annual(state=state)
    m = climlab.EBM_annual(num_lon=4)
    m.step_forward()
    assert m.state.Ts.shape == (90, 4, 1)
    # Test the xarray interface
    m.to_xarray()

@pytest.mark.fast
def test_2D_EBM_seasonal():
    '''Can we step forward a 2D seasonal lat/lon EBM?'''
    #state = climlab.surface_state(num_lon=4)
    #m = climlab.EBM_annual(state=state)
    m = climlab.EBM_seasonal(num_lon=4)
    m.step_forward()
    assert m.state.Ts.shape == (90, 4, 1)
    # Test the xarray interface
    m.to_xarray()

@pytest.mark.fast
def test_2D_insolation():
    #state = climlab.surface_state(num_lon=4)
    #m = climlab.EBM_annual(state=state)
    m = climlab.EBM_annual(num_lon=4)
    #  the answers are the mean of 1D insolation arrays
    #  the mean shouldn't change from 1D to 2D...
    #  there are exactly the same amount of each number in 2D array
    assert np.mean(m.subprocess['insolation'].insolation) == pytest.approx(299.30467670961832)
    #assert np.mean(m.subprocess['insolation'].insolation) == 299.30467670961832
    sfc = m.domains['Ts']
    m.add_subprocess('insolation',
        climlab.radiation.P2Insolation(domains=sfc, **m.param))
    assert np.mean(m.subprocess['insolation'].insolation) == pytest.approx(300.34399999999999)
    #assert np.mean(m.subprocess['insolation'].insolation) == 300.34399999999999
