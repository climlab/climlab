from __future__ import division
import numpy as np
import climlab
from climlab.tests.xarray_test import to_xarray
import pytest

num_lev = 30
alb = 0.25

@pytest.fixture()
def rcm():
    # initial state (temperatures)
    state = climlab.column_state(num_lev=num_lev, num_lat=1, water_depth=5.)
    ## Create individual physical process models:
    #  fixed relative humidity
    h2o = climlab.radiation.ManabeWaterVapor(state=state, name='H2O')
    #  Hard convective adjustment
    convadj = climlab.convection.ConvectiveAdjustment(state=state, name='ConvectiveAdjustment',
                                                      adj_lapse_rate=6.5)
    # CAM3 radiation with default parameters and interactive water vapor
    rad = climlab.radiation.CAM3(state=state, albedo=alb, specific_humidity=h2o.q, name='Radiation')
    # Couple the models
    rcm = climlab.couple([h2o,convadj,rad], name='RCM')
    return rcm


@pytest.mark.compiled
@pytest.mark.fast
def test_rce(rcm):
    '''Test a single-column radiative-convective model with CAM3 radiation and
    fixed relative humidity.'''
    rcm.step_forward()
    # Did a diagnostic get properly updated?
    assert np.all(rcm.OLR == rcm.subprocess.Radiation.OLR)
    #rcm.integrate_years(5)
    #assert(np.isclose(rcm.Ts, ))
    # Test the xarray interface
    to_xarray(rcm)

@pytest.mark.compiled
@pytest.mark.slow
def test_re_radiative_forcing():
    state = climlab.column_state(num_lev=num_lev)
    rad = climlab.radiation.CAM3(state=state)
    rad.integrate_years(2)
    assert np.abs(rad.ASR - rad.OLR) < 0.1  # close to energy balance
    rad2 = climlab.process_like(rad)
    rad2.absorber_vmr['CO2'] *= 2.
    rad2.compute_diagnostics()
    assert (rad2.ASR - rad2.OLR) > 1.  # positive radiative forcing

@pytest.mark.compiled
@pytest.mark.slow
def test_rce_radiative_forcing(rcm):
    '''Run a single-column radiative-convective model with CAM3 radiation
    out to equilibrium. Clone the model, double CO2 and measure the instantaneous
    change in TOA flux. It should be positive net downward flux.'''
    rcm.integrate_years(5.)
    assert np.abs(rcm.ASR - rcm.OLR) < 0.1  # close to energy balance
    rcm2 = climlab.process_like(rcm)
    rcm2.subprocess['Radiation'].absorber_vmr['CO2'] *= 2.
    rcm2.compute_diagnostics()
    assert (rcm2.ASR - rcm2.OLR) > 1.  # positive radiative forcing

@pytest.mark.compiled
@pytest.mark.fast
def test_cam3_multidim():
    state = climlab.column_state(num_lev=40, num_lat=3, water_depth=5.)
    rad = climlab.radiation.CAM3(state=state)
    # Can we integrate?
    rad.step_forward()
    assert rad.OLR.shape == rad.Ts.shape
