import numpy as np
import climlab
import pytest

# The fixtures are reusable pieces of code to set up the input to the tests.
# Without fixtures, we would have to do a lot of cutting and pasting
# I inferred which fixtures to use from the notebook
# Latitude-dependent grey radiation.ipynb
@pytest.fixture()
def model():
    return climlab.BandRCModel()

# helper for a common test pattern
def _check_minmax(array, amin, amax):
    return (np.allclose(array.min(), amin) and
            np.allclose(array.max(), amax))

def test_model_creation(model):
    """Just make sure we can create a model."""
    assert len(model.Tatm)==30

def test_integrate_years(model):
    """Check that we can integrate forward the model and get the expected
    surface temperature and water vapor.
    Also check the climate sensitivity to doubling CO2."""
    model.step_forward()
    model.integrate_years(2)
    Ts = model.Ts.copy()
    assert np.isclose(Ts, 275.43383753)
    assert _check_minmax(model.q, 5.E-6, 3.23764447e-03)
    model.absorber_vmr['CO2'] *= 2.
    model.integrate_years(2)
    assert np.isclose(model.Ts - Ts, 3.180993)
