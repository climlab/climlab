from __future__ import division
import numpy as np
import climlab
import pytest

# The fixtures are reusable pieces of code to set up the input to the tests.
# Without fixtures, we would have to do a lot of cutting and pasting
# These fixtures are based on the notebook
# Seasonal cycle and heat capacity.ipynb
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

def test_model_creation(EBM_seasonal):
    """Just make sure we can create a model."""
    assert len(EBM_seasonal.Ts)==90

#  This is not a great test.. but just sees if we reproduce the same
#  temperature as in the notebook after 1 year of integration
def test_integrate_years(EBM_seasonal):
    """Check that we can integrate forward the model and get the expected
    surface temperature."""
    EBM_seasonal.step_forward()
    EBM_seasonal.integrate_years(1)
    assert _check_minmax(EBM_seasonal.Ts, -24.21414189, 31.16169215)

#  And do the same thing at high obliquity
def test_high_obliquity(EBM_highobliquity):
    """Check that we can integrate forward the model and get the expected
    surface temperature."""
    EBM_highobliquity.step_forward()
    EBM_highobliquity.integrate_years(1)
    assert _check_minmax(EBM_highobliquity.Ts, -9.28717079958807, 27.37890262018116)

def test_annual_iceline(EBM_iceline):
    '''Check that the annual mean EBM with interactive ice edge gives expected
    result (ice at 70degrees).'''
    EBM_iceline.integrate_years(5.)
    assert np.all(EBM_iceline.icelat == np.array([-70.,  70.]))

def test_decreased_S0(EBM_iceline):
    '''Check that a decrease in solar constant to 1200 W/m2 will give a
    Snowball Earth result in the annual mean EBM.'''
    EBM_iceline.subprocess['insolation'].S0 = 1200.
    EBM_iceline.integrate_years(5.)
    assert np.all(EBM_iceline.icelat == np.array([-0.,  0.]))

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

def test_albedo():
    '''Check that we can integrate forward a model after changing the albedo
    subprocess and get the expected icelat'''
    import numpy as np
    from climlab.surface import albedo
    m = climlab.EBM()
    m.add_subprocess('albedo', albedo.ConstantAlbedo(state=m.state, **m.param))
    m.integrate_years(1)
    assert m.icelat == None
    m.add_subprocess('albedo', albedo.StepFunctionAlbedo(state=m.state, **m.param))
    m.integrate_years(1)
    assert np.all(m.icelat == np.array([-70.,  70.]))
    assert np.all(m.icelat == m.subprocess.albedo.subprocess.iceline.icelat)
    #  What is the expected behavior if we swap out a subprocess for another
    #  and they have different diagnostics???
    #m.add_subprocess('albedo', albedo.ConstantAlbedo(state=m.state, **m.param))
    #m.integrate_years(1)
    #assert m.icelat == None
