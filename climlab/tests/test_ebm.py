from __future__ import division
import numpy as np
import climlab
import pytest
from climlab.tests.xarray_test import to_xarray
from climlab.utils.legendre import P2

@pytest.fixture()
def EBM_seasonal():
    return climlab.EBM_seasonal(water_depth=10.)

@pytest.fixture()
def EBM_highobliquity():
    orb_highobl = {'ecc':0., 'obliquity':90., 'long_peri':0.}
    return climlab.EBM_seasonal(orb=orb_highobl, water_depth=10.)

@pytest.fixture()
def EBM_iceline():
    return climlab.EBM_annual( num_points = 180, a0=0.3, a2=0.078, ai=0.62)

# helper for a common test pattern
def _check_minmax(array, amin, amax):
    return (np.allclose(array.min(), amin) and
            np.allclose(array.max(), amax))

@pytest.mark.fast
def test_model_creation(EBM_seasonal):
    """Just make sure we can create a model."""
    assert len(EBM_seasonal.Ts)==90

#  This is not a great test.. but just sees if we reproduce the same
#  temperature as in the notebook after 1 year of integration
@pytest.mark.fast
def test_integrate_years(EBM_seasonal):
    """Check that we can integrate forward the model and get the expected
    surface temperature."""
    EBM_seasonal.step_forward()
    EBM_seasonal.integrate_years(1)
    assert _check_minmax(EBM_seasonal.Ts, -24.21414189, 31.16169215)
    # Test the xarray interface
    to_xarray(EBM_seasonal)

#  And do the same thing at high obliquity
@pytest.mark.fast
def test_high_obliquity(EBM_highobliquity):
    """Check that we can integrate forward the model and get the expected
    surface temperature."""
    EBM_highobliquity.step_forward()
    EBM_highobliquity.integrate_years(1)
    assert _check_minmax(EBM_highobliquity.Ts, -9.28717079958807, 27.37890262018116)

@pytest.mark.fast
def test_annual_iceline(EBM_iceline):
    '''Check that the annual mean EBM with interactive ice edge gives expected
    result (ice at 70degrees).'''
    EBM_iceline.integrate_years(5.)
    assert np.all(EBM_iceline.icelat == np.array([-70.,  70.]))

@pytest.mark.fast
def test_decreased_S0(EBM_iceline):
    '''Check that a decrease in solar constant to 1200 W/m2 will give a
    Snowball Earth result in the annual mean EBM.'''
    #EBM_iceline.subprocess['insolation'].S0 = 1200.
    EBM_iceline.S0 = 1200.
    EBM_iceline.integrate_years(5.)
    assert np.all(EBM_iceline.icelat == np.array([-0.,  0.]))

@pytest.mark.fast
def test_float():
    '''Check that we can step forward the model after setting the state
    variable with an ndarray of integers through 2 different methods'''
    from climlab.domain import initial
    from climlab.domain.field import Field
    state = initial.surface_state()
    sfc = climlab.domain.zonal_mean_surface(num_lat=4)
    state.Ts = Field([10,15,15,10],domain=sfc)
    m = climlab.EBM(state=state)
    m.step_forward()
    k = climlab.EBM(num_lat=4)
    k.set_state('Ts', Field([10,15,15,10], domain=k.domains['Ts']))
    k.step_forward()

@pytest.mark.fast
def test_albedo():
    '''Check that we can integrate forward a model after changing the albedo
    subprocess and get the expected icelat'''
    import numpy as np
    m = climlab.EBM()
    m.add_subprocess('albedo', climlab.surface.ConstantAlbedo(state=m.state, **m.param))
    m.subprocess['SW'].albedo = m.subprocess['albedo'].albedo
    m.integrate_years(1)
    m.add_subprocess('albedo', climlab.surface.StepFunctionAlbedo(state=m.state, **m.param))
    m.subprocess['SW'].albedo = m.subprocess['albedo'].albedo
    m.integrate_years(1)
    assert np.all(m.icelat == np.array([-70.,  70.]))
    assert np.all(m.icelat == m.subprocess['albedo'].subprocess['iceline'].icelat)
    #  What is the expected behavior if we swap out a subprocess for another
    #  and they have different diagnostics???
    #m.add_subprocess('albedo', albedo.ConstantAlbedo(state=m.state, **m.param))
    #m.integrate_years(1)
    #assert m.icelat == None

@pytest.mark.fast
def test_analytical():
    '''Check to see if the the numerical solution converges to the analytical
    steady-state solution of the simple EBM with constant albedo'''
    param = {'a0': 0.3,
             'a2': 0.,
             'ai': 0.3,
             's2': -0.48,
             'S0': 1360.,
             'A': 210.,
             'B': 2.,
             'D': 0.55,
             'Tf': -1000., # effectively makes albedo constant
            }
    m = climlab.EBM(**param)
    m.integrate_years(5)
    Tnumerical = np.squeeze(m.Ts)
    delta = param['D']/param['B']
    x = np.sin(np.deg2rad(m.lat))
    Tanalytical = ((1-param['a0'])*param['S0']/4*(1+param['s2']*P2(x)/(1+6*delta))-param['A'])/param['B']
    assert Tnumerical == pytest.approx(Tanalytical, abs=2E-2)

@pytest.mark.fast
def test_moist_EBM_creation():
    '''See if we can swap a moist diffusion module for the dry diffusion
    and just step forward once.'''
    m = climlab.EBM()
    m.remove_subprocess('diffusion')
    diff = climlab.dynamics.MeridionalMoistDiffusion(state=m.state, timestep=m.timestep)
    m.add_subprocess('diffusion', diff)
    m.step_forward()
    assert hasattr(m, 'heat_transport')
