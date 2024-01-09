from __future__ import division
import numpy as np
import climlab
import pytest
from climlab import constants as const
def test_zonal_meridional_ebm_eq():
    # create control model
    ctrls2 = climlab.surface_state(num_lat=40, num_lon=40, water_depth=2.5)
    ctrlm2 = climlab.EBM(state=ctrls2)
    # create test model
    state2 = climlab.surface_state(num_lat=40, num_lon=40, water_depth=2.5)
    model2 = climlab.EBM(state=state2)
    zonald = climlab.dynamics.zonal_heat_diffusion.ZonalHeatDiffusion(state=state2, 
                                                                      timestep=model2.timestep)
    model2.add_subprocess('zonal', zonald)
    # run both models
    ctrlm2.integrate_years(2)
    model2.integrate_years(2)
    # assert
    assert all(np.abs(np.mean(ctrlm2.Ts, axis=1) - np.mean(model2.Ts, axis=1)) < 1e-6)

def test_zonal_temp():
    # create initial condition
    xy = np.tile(np.linspace(200, 300, 40), 40).reshape(40,-1)
    # create model
    state1 = climlab.surface_state(num_lat=40, num_lon=40, water_depth=2.5)
    state1.Ts[:] = xy[...,np.newaxis]
    model1 = climlab.dynamics.zonal_heat_diffusion.ZonalHeatDiffusion(state=state1, timestep=const.seconds_per_day)
    # run model
    model1.integrate_years(1)
    # assert stuff
    assert (model1.Ts < np.max(xy)).all()
    assert (model1.Ts > np.min(xy)).all()
    
