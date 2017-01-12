from __future__ import division
import numpy as np
import climlab
import pytest
from climlab.radiation import RRTMG, RRTMG_LW, RRTMG_SW, CAM3Radiation_LW
from climlab.radiation.rrtm import _climlab_to_rrtm, _rrtm_to_climlab

num_lev = 30

def test_rrtmg_lw_creation():
    state = climlab.column_state(num_lev=num_lev, water_depth=5.)
    rad = RRTMG_LW(state=state)
    #  are the transformations reversible?
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Ts)) == rad.Ts)
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Tatm)) == rad.Tatm)

def test_rrtm_creation():
    # initial state (temperatures)
    state = climlab.column_state(num_lev=num_lev, num_lat=1, water_depth=5.)
    #  Create a RRTM radiation model
    rad = RRTMG(state=state)
    rad.step_forward()
    assert type(rad.subprocess['LW']) is RRTMG_LW
    assert type(rad.subprocess['SW']) is RRTMG_SW
    assert hasattr(rad, 'OLR')
    assert hasattr(rad, 'OLRclr')
    assert hasattr(rad, 'ASR')
    assert hasattr(rad, 'ASRclr')

def test_swap_component():
    # initial state (temperatures)
    state = climlab.column_state(num_lev=num_lev, num_lat=1, water_depth=5.)
    #  Create a RRTM radiation model
    rad = RRTMG(state=state)
    rad.step_forward()
    #  Swap out the longwave model for CAM3
    rad.remove_subprocess('LW')
    rad.step_forward()
    rad.add_subprocess('LW', CAM3Radiation_LW(state=state))
    rad.step_forward()
    assert hasattr(rad, 'OLR')

def test_multidim():
    state = climlab.column_state(num_lev=40, num_lat=3, water_depth=5.)
    rad = RRTMG_LW(state=state)
    #  are the transformations reversible?
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Ts)) == rad.Ts)
    assert np.all(_rrtm_to_climlab(_climlab_to_rrtm(rad.Tatm)) == rad.Tatm)
    # Can we integrate?
    rad.step_forward()
    assert rad.OLR.shape == rad.Ts.shape
