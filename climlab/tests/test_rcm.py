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

@pytest.mark.compiled
@pytest.mark.fast
def test_coupled_rcm(rcm):
    deltat = rcm.timestep
    ocean_bounds = np.arange(0., 2010., 100.)
    depthax = climlab.Axis(axis_type='depth', bounds=ocean_bounds)
    ocean = climlab.domain.domain.Ocean(axes=depthax)
    ocean_diff = 5.E-4
    Tinitial_ocean = rcm.Ts * np.ones(ocean.shape)
    Tocean = climlab.Field(Tinitial_ocean.copy(), domain=ocean)
    Tatm = rcm.Tatm
    #  Surface temperature Ts is the upper-most grid box of the ocean
    Ts = Tocean[0:1]
    atm_state = {'Tatm': Tatm, 'Ts': Ts}
    rad = climlab.radiation.RRTMG(name='Radiation',
                                  state=atm_state,
                                  specific_humidity=rcm.specific_humidity,
                                  timestep = deltat,
                                  albedo = 0.25,
                                 )
    conv = climlab.convection.ConvectiveAdjustment(name='Convection',
                                                   state=atm_state,
                                                   adj_lapse_rate=6.5,
                                                   timestep=deltat,)

    model = rad + conv
    model.set_state('Tocean', Tocean)
    diff = climlab.dynamics.Diffusion(state={'Tocean': Tocean},
                                K=ocean_diff,
                                diffusion_axis='depth',
                                timestep=deltat * 10,)
    model.add_subprocess('Ocean Heat Uptake', diff)
    for i in range(10):
        model.step_forward()

@pytest.mark.fast
def test_convective_adjustment_multidim():
    #  can we do convective adjustment on a multidimensional grid?
    num_lev = 3
    state = climlab.column_state(num_lev=num_lev, num_lat=2)
    conv = climlab.convection.ConvectiveAdjustment(state=state)
    #  test non-scalar critical lapse rate
    conv.adj_lapse_rate = np.linspace(5., 8., num_lev+1)
    conv.step_forward()
    #  Test two flags for dry adiabatic adjustment
    conv.adj_lapse_rate = 'DALR'
    conv.step_forward()
    conv.adj_lapse_rate = 'dry adiabat'
    conv.step_forward()
    #  test pseudoadiabatic critical lapse rate
    conv.adj_lapse_rate = 'pseudoadiabat'
    conv.step_forward()
    conv.adj_lapse_rate = 'MALR'
    conv.step_forward()
    conv.adj_lapse_rate = 'moist adiabat'
    conv.step_forward()
