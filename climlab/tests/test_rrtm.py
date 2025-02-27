from __future__ import division
import numpy as np
import climlab
import pytest
from climlab.radiation.rrtm import _climlab_to_rrtm, _rrtm_to_climlab
from climlab.tests.xarray_test import to_xarray

num_lev = 30

@pytest.mark.compiled
@pytest.mark.fast
def test_rrtmg_lw_creation():
    state = climlab.column_state(num_lev=num_lev, water_depth=5.)
    rad = climlab.radiation.RRTMG_LW(state=state)
    #  are the transformations reversible?
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Ts)) == rad.Ts)
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Tatm)) == rad.Tatm)

@pytest.mark.compiled
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

@pytest.mark.compiled
@pytest.mark.fast
def test_rrtm_aerosols():
    from climlab.radiation.rrtm.rrtmg_lw import nbndlw
    from climlab.radiation.rrtm.rrtmg_sw import nbndsw, naerec
    # single column version
    state = climlab.column_state(num_lev=num_lev)
    #  these value are meaningless, just testing dimensions and making sure code runs
    tauaer_lw = np.zeros((nbndlw,num_lev)) + 0.1
    tauaer_sw = np.zeros((nbndsw,num_lev)) + 0.1
    ssaaer_sw = np.zeros((nbndsw,num_lev)) + 0.1
    asmaer_sw = np.zeros((nbndsw,num_lev)) + 0.1
    ecaer_sw = np.zeros((naerec,num_lev)) + 0.1
    # ECMWF aerosols
    rad = climlab.radiation.RRTMG(state=state, iaer=6, ecaer_sw=ecaer_sw)
    #  Is the aerosol flag passed through appropriately?
    assert rad.subprocess['SW'].iaer==6
    rad.step_forward()
    # Full aerosols
    rad = climlab.radiation.RRTMG(state=state, iaer=10, 
                                  tauaer_lw=tauaer_lw,
                                  tauaer_sw=tauaer_sw,
                                  ssaaer_sw=ssaaer_sw,
                                  asmaer_sw=asmaer_sw)
    #  Is the aerosol flag passed through appropriately?
    assert rad.subprocess['SW'].iaer==10
    #  Can we step forward?
    rad.step_forward()
    # 2D version
    num_lat = 4
    state = climlab.column_state(num_lat=num_lat, num_lev=num_lev)
    tauaer_lw = np.zeros((nbndlw,num_lat,num_lev)) + 0.1
    tauaer_sw = np.zeros((nbndsw,num_lat,num_lev)) + 0.1
    ssaaer_sw = np.zeros((nbndsw,num_lat,num_lev)) + 0.1
    asmaer_sw = np.zeros((nbndsw,num_lat,num_lev)) + 0.1
    ecaer_sw = np.zeros((naerec,num_lat,num_lev)) + 0.1
    rad = climlab.radiation.RRTMG(state=state, iaer=6, ecaer_sw=ecaer_sw)
    rad.step_forward()
    rad = climlab.radiation.RRTMG(state=state, iaer=10, 
                                  tauaer_lw=tauaer_lw,
                                  tauaer_sw=tauaer_sw,
                                  ssaaer_sw=ssaaer_sw,
                                  asmaer_sw=asmaer_sw)
    rad.step_forward()

@pytest.mark.compiled
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

@pytest.mark.compiled
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

@pytest.mark.compiled
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

@pytest.mark.compiled
@pytest.mark.slow
def test_radiative_forcing():
    '''Run a single-column radiative-convective model with RRTMG radiation
    out to equilibrium. Clone the model, double CO2 and measure the instantaneous
    change in TOA flux. It should be positive net downward flux.'''
    #  State variables (Air and surface temperature)
    state = climlab.column_state(num_lev=30, water_depth=1.)
    #  Fixed relative humidity
    h2o = climlab.radiation.ManabeWaterVapor(name='WaterVapor', state=state)
    #  Couple water vapor to radiation
    #   Set icld=0 for clear-sky only (no need to call cloud overlap routine)
    rad = climlab.radiation.RRTMG(name='Radiation',
                                  state=state,
                                  specific_humidity=h2o.q,
                                  icld=0)
    #  Convective adjustment
    conv = climlab.convection.ConvectiveAdjustment(name='Convection',
                                                   state=state,
                                                   adj_lapse_rate=6.5)
    #  Couple everything together
    rcm = climlab.couple([rad,h2o,conv], name='Radiative-Convective Model')

    rcm.integrate_years(5.)
    assert np.abs(rcm.ASR - rcm.OLR) < 0.1  # close to energy balance
    rcm2 = climlab.process_like(rcm)
    rcm2.subprocess['Radiation'].absorber_vmr['CO2'] *= 2.
    rcm2.compute_diagnostics()
    assert (rcm2.ASR - rcm2.OLR) > 1.  # positive radiative forcing
    #  Test the xarray interface
    to_xarray(rcm2)

@pytest.mark.compiled
@pytest.mark.slow
def test_latitude():
    '''
    Run a radiative equilibrum model with RRTMG radiation out to equilibrium
    with an annual mean insolation profile as a function of latitude.
    '''
    num_lat = 8
    #  State variables (Air and surface temperature)
    state = climlab.column_state(num_lev=30, num_lat=num_lat, water_depth=1.)
    #  insolation
    sol = climlab.radiation.AnnualMeanInsolation(name='Insolation',
                                                 domains=state.Ts.domain)
    #  radiation module with insolation as input
    #   Set icld=0 for clear-sky only (no need to call cloud overlap routine)
    rad = climlab.radiation.RRTMG(name='Radiation', state=state, icld=0,
                                  S0=sol.S0,
                                  coszen=sol.coszen,
                                  irradiance_factor=sol.irradiance_factor,)
    #  Couple everything together
    model = rad + sol
    #  Run out to equilibrium
    model.integrate_years(2.)
    #  Test for energy balance
    assert np.all(np.abs(model.ASR - model.OLR) < 0.1)
    #  Test for reasonable surface temperature gradient
    #  reversal of gradient at equator
    grad = np.diff(model.Ts, axis=0)
    assert np.all(grad[0:(int(num_lat/2)-1)] > 0.)
    assert np.all(grad[int(num_lat/2):] < 0.)

@pytest.mark.compiled
@pytest.mark.fast
def test_cozen():
    '''
    Create 2D models with RRTMG radiation and latitudinally-varying radiation.
    Check to see that different ways of time-averaging the solar zenith angle
    result in different ASR despite same insolation
    due to zenith-angle dependence of clear-sky scattering
    '''
    num_lat = 180
    model_list = []
    for weighting in ['time', 'sunlit', 'insolation']:
        #  State variables (Air and surface temperature)
        state = climlab.column_state(num_lev=30, num_lat=num_lat, water_depth=1.)
        #  insolation using specified zenith angle weighting
        sol = climlab.radiation.DailyInsolation(name='Insolation',
                                                 domains=state.Ts.domain,
                                                 weighting=weighting)
        #  radiation module with insolation as input
        #   Set icld=0 for clear-sky only (no need to call cloud overlap routine)
        rad = climlab.radiation.RRTMG(name='Radiation', state=state, icld=0,
                                      S0=sol.S0,
                                      coszen=sol.coszen,
                                      irradiance_factor=sol.irradiance_factor)
        #  Couple everything together
        m = climlab.couple([rad, sol], name='model')
        m.compute_diagnostics()
        model_list.append(m)
    # Do all models have the same insolation?
    assert np.allclose(model_list[0].insolation, model_list[1].insolation)
    assert np.allclose(model_list[1].insolation, model_list[2].insolation)
    # Does the cosine of zenith angle increase as we weight the average towards times of day with more insolation?
    #  (either greater or about the same, to handle numerically near zero results)
    assert np.all((model_list[0].coszen < model_list[1].coszen) | (np.isclose(model_list[0].coszen, model_list[1].coszen)))
    assert np.all((model_list[1].coszen < model_list[2].coszen) | (np.isclose(model_list[1].coszen, model_list[2].coszen)))
    # Does the ASR increase for higher zenith angle (away from the poles)?
    ASR0 = climlab.to_xarray(model_list[0].ASR)
    ASR1 = climlab.to_xarray(model_list[1].ASR)
    ASR2 = climlab.to_xarray(model_list[2].ASR)
    # Using a tolerance of 0.1 W/m2 here due to some gridpoint issues
    assert np.all((ASR0<ASR1) | np.isclose(ASR0, ASR1, atol=0.1))
    assert np.all((ASR1<ASR2) | np.isclose(ASR1, ASR2, atol=0.1))

@pytest.mark.compiled
@pytest.mark.fast
def test_no_ozone():
    '''When user gives None as the ozone_file, the model is initialized
    with zero ozone. This should work on arbitrary grids.'''
    ps = 1060.
    num_lev=4000
    state = climlab.column_state(num_lev=num_lev, num_lat=1, water_depth=5.)
    lev = state.Tatm.domain.lev
    lev.bounds = np.linspace(0., ps, num_lev+1)
    lev.points = lev.bounds[:-1] + np.diff(lev.bounds)/2.
    lev.delta = np.abs(np.diff(lev.bounds))
    #  Create a RRTM radiation model
    rad = climlab.radiation.RRTMG(state=state, ozone_file=None)
    assert np.all(rad.absorber_vmr['O3']==0.)

@pytest.mark.compiled
@pytest.mark.fast
def test_fixed_insolation():
    '''Make sure that we can run a model forward with specified time-invariant insolation'''
    num_lat = 4; num_lev = 20   # grid size
    day_of_year = 80.  # days since Jan 1
    lat = np.linspace(-80., 80., num_lat)
    state = climlab.column_state(num_lev=num_lev, lat=lat)
    insolation = climlab.solar.insolation.daily_insolation(lat=lat, day=day_of_year)
    ins_array = insolation.values
    rad = climlab.radiation.RRTMG(name='Radiation', state=state, insolation=ins_array)
    rad.step_forward()

@pytest.mark.compiled
@pytest.mark.fast
def test_large_grid():
    num_lev = 50; num_lat=90
    state = climlab.column_state(num_lev=num_lev, num_lat=num_lat, water_depth=10.)
    rad1 = climlab.radiation.CAM3(state=state)
    rad1.step_forward()
    rad2 = climlab.radiation.RRTMG(state=state)
    rad2.step_forward()

    # Spectral OLR test
    # check that the spectrally decomposed TOA flux adds up to the normal OLR output
    rad3 = climlab.radiation.RRTMG(state=state, return_spectral_olr=True)
    rad3.step_forward()
    assert np.all(np.abs(rad3.OLR - rad3.OLR_spectral.sum(axis=-1))<0.1)
    #  Test the xarray interface
    to_xarray(rad3)
    
@pytest.mark.compiled
@pytest.mark.fast
def test_sw_params_propagate():
    state = climlab.column_state()
    rad = climlab.radiation.RRTMG(state=state)
    assert rad.insolation == rad.subprocess['SW'].insolation
    rad.insolation *= 1.01
    assert rad.insolation == rad.subprocess['SW'].insolation
    rad.coszen *= 1.01
    assert rad.coszen == rad.subprocess['SW'].coszen
    assert rad.insolation == rad.subprocess['SW'].insolation
    rad.irradiance_factor *= 1.01
    assert rad.irradiance_factor == rad.subprocess['SW'].irradiance_factor
    assert rad.insolation == rad.subprocess['SW'].insolation
