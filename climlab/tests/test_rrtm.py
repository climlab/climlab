from __future__ import division
import numpy as np
import climlab
import pytest
from climlab.radiation.rrtm import _climlab_to_rrtm, _rrtm_to_climlab
from climlab.tests.xarray_test import to_xarray

num_lev = 30

@pytest.mark.fast
def test_rrtmg_lw_creation():
    state = climlab.column_state(num_lev=num_lev, water_depth=5.)
    rad = climlab.radiation.RRTMG_LW(state=state)
    #  are the transformations reversible?
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Ts)) == rad.Ts)
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Tatm)) == rad.Tatm)

@pytest.mark.fast
def test_rrtm_creation():
    # initial state (temperatures)
    state = climlab.column_state(num_lev=num_lev, num_lat=1, water_depth=5.)
    #  Create a RRTM radiation model
    rad = climlab.radiation.RRTMG(state=state)
    rad.step_forward()
    assert type(rad.subprocess['LW']) is climlab.radiation.RRTMG_LW
    assert type(rad.subprocess['SW']) is climlab.radiation.RRTMG_SW
    assert hasattr(rad, 'OLR')
    assert hasattr(rad, 'OLRclr')
    assert hasattr(rad, 'ASR')
    assert hasattr(rad, 'ASRclr')
    # Test the xarray interface
    to_xarray(rad)

@pytest.mark.fast
def test_swap_component():
    # initial state (temperatures)
    state = climlab.column_state(num_lev=num_lev, num_lat=1, water_depth=5.)
    #  Create a RRTM radiation model
    rad = climlab.radiation.RRTMG(state=state)
    rad.step_forward()
    #  Swap out the longwave model for CAM3
    rad.remove_subprocess('LW')
    rad.step_forward()
    rad.add_subprocess('LW', climlab.radiation.CAM3_LW(state=state))
    rad.step_forward()
    assert hasattr(rad, 'OLR')

@pytest.mark.fast
def test_multidim():
    state = climlab.column_state(num_lev=40, num_lat=3, water_depth=5.)
    rad = climlab.radiation.RRTMG_LW(state=state)
    #  are the transformations reversible?
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Ts)) == rad.Ts)
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Tatm)) == rad.Tatm)
    # Can we integrate?
    rad.step_forward()
    assert rad.OLR.shape == rad.Ts.shape

@pytest.mark.fast
def test_cloud():
    '''Put a high cloud layer in a radiative model.
    The all-sky ASR should be lower than clear-sky ASR.
    The all-sky OLR should be lower than clear-sky OLR.'''
    #  State variables (Air and surface temperature)
    state = climlab.column_state(num_lev=50, water_depth=1.)
    lev = state.Tatm.domain.axes['lev'].points
    #  Define some local cloud characteristics
    cldfrac = 0.5  # layer cloud fraction
    r_liq = 14.  # Cloud water drop effective radius (microns)
    clwp = 60.  # in-cloud liquid water path (g/m2)
    #  The cloud fraction is a Gaussian bump centered at level i
    i = 25
    mycloud = {'cldfrac': cldfrac*np.exp(-(lev-lev[i])**2/(2*25.)**2),
               'clwp': np.zeros_like(state.Tatm) + clwp,
               'r_liq': np.zeros_like(state.Tatm) + r_liq,}
    #  Test both RRTMG and CAM3:
    #for module in [climlab.radiation.RRTMG, climlab.radiation.CAM3]:
    #  Apparently clouds in CAM3 are not working. Save this for later
    for module in [climlab.radiation.RRTMG]:
        rad = module(state=state, **mycloud)
        rad.compute_diagnostics()
        assert(rad.ASR - rad.ASRclr < 0.)
        assert(rad.OLR - rad.OLRclr < 0.)

@pytest.mark.slow
def test_radiative_forcing():
    '''Run a single-column radiative-convective model with RRTMG radiation
    out to equilibrium. Clone the model, double CO2 and measure the instantaneous
    change in TOA flux. It should be positive net downward flux.'''
    #  State variables (Air and surface temperature)
    state = climlab.column_state(num_lev=30, water_depth=1.)
    #  Parent model process
    rcm = climlab.TimeDependentProcess(state=state)
    #  Fixed relative humidity
    h2o = climlab.radiation.ManabeWaterVapor(state=state)
    #  Couple water vapor to radiation
    #   Set icld=0 for clear-sky only (no need to call cloud overlap routine)
    rad = climlab.radiation.RRTMG(state=state, specific_humidity=h2o.q, icld=0)
    #  Convective adjustment
    conv = climlab.convection.ConvectiveAdjustment(state=state,
                                                   adj_lapse_rate=6.5)
    #  Couple everything together
    rcm.add_subprocess('Radiation', rad)
    rcm.add_subprocess('WaterVapor', h2o)
    rcm.add_subprocess('Convection', conv)

    rcm.integrate_years(5.)
    assert np.abs(rcm.ASR - rcm.OLR) < 0.1  # close to energy balance
    rcm2 = climlab.process_like(rcm)
    rcm2.subprocess['Radiation'].absorber_vmr['CO2'] *= 2.
    rcm2.compute_diagnostics()
    assert (rcm2.ASR - rcm2.OLR) > 1.  # positive radiative forcing
    #  Test the xarray interface
    to_xarray(rcm2)

@pytest.mark.slow
def test_latitude():
    '''
    Run a radiative equilibrum model with RRTMG radiation out to equilibrium
    with an annual mean insolation profile as a function of latitude.
    '''
    num_lat = 8
    #  State variables (Air and surface temperature)
    state = climlab.column_state(num_lev=30, num_lat=num_lat, water_depth=1.)
    #  Parent model process
    model = climlab.TimeDependentProcess(state=state)
    #  insolation
    sol = climlab.radiation.AnnualMeanInsolation(domains=model.Ts.domain)
    #  radiation module with insolation as input
    #   Set icld=0 for clear-sky only (no need to call cloud overlap routine)
    rad = climlab.radiation.RRTMG(state=state, icld=0,
                                  S0=sol.S0,
                                  insolation=sol.insolation,
                                  coszen=sol.coszen)
    #  Couple everything together
    model.add_subprocess('Radiation', rad)
    model.add_subprocess('Insolation', sol)
    #  Run out to equilibrium
    model.integrate_years(2.)
    #  Test for energy balance
    assert np.all(np.abs(model.ASR - model.OLR) < 0.1)
    #  Test for reasonable surface temperature gradient
    #  reversal of gradient at equator
    grad = np.diff(model.Ts, axis=0)
    assert np.all(grad[0:(int(num_lat/2)-1)] > 0.)
    assert np.all(grad[int(num_lat/2):] < 0.)
