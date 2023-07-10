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
    m = climlab.EBM_annual(num_lon=4)
    m.step_forward()
    assert m.state.Ts.shape == (90, 4, 1)
    # Test the xarray interface
    m.to_xarray()

@pytest.mark.fast
def test_2D_EBM_seasonal():
    '''Can we step forward a 2D seasonal lat/lon EBM?'''
    m = climlab.EBM_seasonal(num_lon=4)
    m.step_forward()
    assert m.state.Ts.shape == (90, 4, 1)
    # Test the xarray interface
    m.to_xarray()

@pytest.mark.fast
def test_2D_insolation():
    m = climlab.EBM_annual(num_lon=4)
    #  the answers are the mean of 1D insolation arrays
    #  the mean shouldn't change from 1D to 2D...
    #  there are exactly the same amount of each number in 2D array
    assert np.mean(m.subprocess['insolation'].insolation) == pytest.approx(299.30467670961832)
    sfc = m.domains['Ts']
    m.add_subprocess('insolation',
        climlab.radiation.P2Insolation(domains=sfc, **m.param))
    assert np.mean(m.subprocess['insolation'].insolation) == pytest.approx(300.34399999999999)

@pytest.mark.fast
def test_diurnal_cycle():
    # We will create a model with a diurnal cycle and a longitude dimension
    # and another model with no longitude dimension that uses daily average insolation.
    # The zonal mean insolation should equal the daily average at each latitude 
    # (same time of year)
    num_lat = 30
    num_lon = 1000
    state = climlab.surface_state(num_lat=num_lat, 
                              num_lon=num_lon, 
                              water_depth=0.5)
    deltat = 3600.  # one hour timestep
    olr = climlab.radiation.AplusBT(name='OLR', 
                                    state=state, 
                                    timestep=deltat)
    ins = climlab.radiation.InstantInsolation(name='Insolation', 
                                            domains=state.Ts.domain, 
                                            timestep=deltat)
    asr = climlab.radiation.SimpleAbsorbedShortwave(name='ASR', 
                                                    state=state, 
                                                    insolation=ins.insolation, 
                                                    timestep=deltat)
    ebm = climlab.couple([olr,asr,ins], name='EBM')

    state_daily = climlab.surface_state(num_lat=num_lat, 
                              water_depth=0.5)
    deltat_daily = 3600.*12. # 12 hour timestep
    olr_daily = climlab.radiation.AplusBT(name='OLR (daily)', 
                                    state=state_daily, 
                                    timestep=deltat_daily)
    ins_daily = climlab.radiation.DailyInsolation(name='Insolation (daily)', 
                                            domains=state_daily.Ts.domain, 
                                            timestep=deltat_daily)
    asr_daily = climlab.radiation.SimpleAbsorbedShortwave(name='ASR (daily)', 
                                            state=state_daily, 
                                            insolation=ins_daily.insolation, 
                                            timestep=deltat_daily)
    ebm_daily = climlab.couple([olr_daily, asr_daily, ins_daily], name='EBM (daily)')

    for model in [ebm, ebm_daily]:
        model.compute_diagnostics()
    # zonal average diurnally-varying insolation should match daily mean insolation
    assert np.allclose(ebm.ASR.mean(axis=1), ebm_daily.ASR)

    # Now we will check to see if the average ASR over one day and all longitudes
    # is equal to ASR over a single one-day step in the daily insolation model

    # advance the daily model forward by half a day to align the calendars
    ebm_daily.step_forward()
    # and recalculate the diagnostics at the updated time
    ebm_daily.compute_diagnostics()
    # Now do one full day for the diurnal model
    ebm.integrate_days(1.)
    # Results should match within a small absolute tolerance in W/m2.
    assert np.allclose(ebm.timeave['ASR'].mean(axis=1), ebm_daily.ASR, atol=0.05)
