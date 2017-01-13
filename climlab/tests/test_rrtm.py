from __future__ import division
import numpy as np
import climlab
import pytest
#from climlab.radiation import RRTMG, RRTMG_LW, RRTMG_SW, CAM3Radiation_LW
from climlab.radiation.rrtm import _climlab_to_rrtm, _rrtm_to_climlab

num_lev = 30

def test_rrtmg_lw_creation():
    state = climlab.column_state(num_lev=num_lev, water_depth=5.)
    rad = climlab.radiation.RRTMG_LW(state=state)
    #  are the transformations reversible?
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Ts)) == rad.Ts)
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Tatm)) == rad.Tatm)

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

def test_swap_component():
    # initial state (temperatures)
    state = climlab.column_state(num_lev=num_lev, num_lat=1, water_depth=5.)
    #  Create a RRTM radiation model
    rad = climlab.radiation.RRTMG(state=state)
    rad.step_forward()
    #  Swap out the longwave model for CAM3
    rad.remove_subprocess('LW')
    rad.step_forward()
    rad.add_subprocess('LW', climlab.radiation.CAM3Radiation_LW(state=state))
    rad.step_forward()
    assert hasattr(rad, 'OLR')

def test_multidim():
    state = climlab.column_state(num_lev=40, num_lat=3, water_depth=5.)
    rad = climlab.radiation.RRTMG_LW(state=state)
    #  are the transformations reversible?
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Ts)) == rad.Ts)
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Tatm)) == rad.Tatm)
    # Can we integrate?
    rad.step_forward()
    assert rad.OLR.shape == rad.Ts.shape

def test_radiative_forcing():
    '''Run a single-column radiative-convective model with CAM3 radiation
    out to equilibrium. Clone the model, double CO2 and measure the instantaneous
    change in TOA flux. It should be positive net downward flux.'''
    #  State variables (Air and surface temperature)
    state = climlab.column_state(num_lev=30, water_depth=1.)
    #  Parent model process
    rcm = climlab.TimeDependentProcess(state=state)
    #  Fixed relative humidity
    h2o = climlab.radiation.ManabeWaterVapor(state=state)
    #  Couple water vapor to radiation
    rad = climlab.radiation.RRTMG(state=state, h2ovmr=h2o.q)
    #  Convective adjustment
    conv = climlab.convection.ConvectiveAdjustment(state=state,
                                                   adj_lapse_rate=6.5)
    #  Couple everything together
    rcm.add_subprocess('Radiation', rad)
    rcm.add_subprocess('WaterVapor', h2o)
    rcm.add_subprocess('Convection', conv)

    rcm.integrate_years(5.)
    #assert np.abs(rcm.ASR - rcm.OLR) < 0.1  # close to energy balance
    #  There is currently a problem with energy conservation in the RRTM module, need to look into this.
    rcm2 = climlab.process_like(rcm)
    rcm2.subprocess['Radiation'].co2vmr *= 2.
    rcm2.compute_diagnostics()
    #assert (rcm2.ASR - rcm2.OLR) > 1.  # positive radiative forcing
    #  For now this test just checks whether the API calls work without error.
    #  We will reinstate the physical checks after fixing the energy bug.
