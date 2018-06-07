from __future__ import division
import numpy as np
import climlab
import pytest

@pytest.mark.compiled
@pytest.fixture()
def rcm():
    # initial state (temperatures)
    state = climlab.column_state(num_lev=40, num_lat=1, water_depth=5.)
    ## Create individual physical process models:
    #  fixed relative humidity
    h2o = climlab.radiation.ManabeWaterVapor(state=state, name='H2O')
    #  Hard convective adjustment
    convadj = climlab.convection.ConvectiveAdjustment(state=state, name='ConvectiveAdjustment',
                                                      adj_lapse_rate=6.5)
    # RRTMG radiation with default parameters and interactive water vapor
    rad = climlab.radiation.RRTMG(state=state, albedo=0.2, specific_humidity=h2o.q, name='Radiation')
    # Couple the models
    rcm = climlab.couple([h2o,convadj,rad], name='RCM')
    return rcm

@pytest.mark.compiled
@pytest.mark.fast
def test_convective_adjustment(rcm):
    rcm.step_forward()
    #  test non-scalar critical lapse rate
    num_lev = rcm.lev.size
    rcm.subprocess['ConvectiveAdjustment'].adj_lapse_rate = np.linspace(5., 8., num_lev+1)
    rcm.step_forward()
    #  Test two flags for dry adiabatic adjustment
    rcm.subprocess['ConvectiveAdjustment'].adj_lapse_rate = 'DALR'
    rcm.step_forward()
    rcm.subprocess['ConvectiveAdjustment'].adj_lapse_rate = 'dry adiabat'
    rcm.step_forward()
    #  test pseudoadiabatic critical lapse rate
    rcm.subprocess['ConvectiveAdjustment'].adj_lapse_rate = 'pseudoadiabat'
    rcm.step_forward()
    rcm.subprocess['ConvectiveAdjustment'].adj_lapse_rate = 'MALR'
    rcm.step_forward()
    rcm.subprocess['ConvectiveAdjustment'].adj_lapse_rate = 'moist adiabat'
    rcm.step_forward()
