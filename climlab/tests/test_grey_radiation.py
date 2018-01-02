from __future__ import division
import numpy as np
import climlab
import pytest
from climlab.tests.xarray_test import to_xarray


@pytest.fixture()
def model():
    return climlab.GreyRadiationModel(num_lev=30, num_lat=90)

@pytest.fixture()
def model_with_insolation(model):
    insolation = climlab.radiation.DailyInsolation(domains=model.Ts.domain)
    model.add_subprocess('insolation', insolation)
    model.subprocess.SW.flux_from_space = insolation.insolation
    return model

@pytest.fixture()
def rcmodel():
    model2 = climlab.RadiativeConvectiveModel(num_lev=30, num_lat=90)
    insolation = climlab.radiation.DailyInsolation(domains=model2.Ts.domain)
    model2.add_subprocess('insolation', insolation)
    model2.subprocess.SW.flux_from_space = insolation.insolation
    return model2

@pytest.fixture()
def diffmodel(rcmodel):
    diffmodel = climlab.process_like(rcmodel)
    # thermal diffusivity in W/m**2/degC
    D = 0.05
    # meridional diffusivity in 1/s
    K = D / diffmodel.Tatm.domain.heat_capacity[0]
    d = climlab.dynamics.MeridionalDiffusion(K=K,
                state={'Tatm': diffmodel.state['Tatm']},
                **diffmodel.param)
    diffmodel.add_subprocess('diffusion', d)
    return diffmodel

# helper for a common test pattern
def _check_minmax(array, amin, amax):
    return (np.allclose(array.min(), amin) and
            np.allclose(array.max(), amax))

@pytest.mark.fast
def test_model_creation(model):
    """Just make sure we can create a model."""
    assert len(model.lat)==90
    # Test the xarray interface
    to_xarray(model)

@pytest.mark.fast
def test_add_insolation(model_with_insolation):
    """"Create a model with insolation and check that SW_down_TOA has
    reasonable values."""
    model_with_insolation.step_forward()
    assert _check_minmax(model_with_insolation.SW_down_TOA, 0, 555.17111)

@pytest.mark.slow
def test_integrate_years(model_with_insolation):
    """Check that we can integrate forward the model and get the expected
    surface temperature."""
    model_with_insolation.step_forward()
    model_with_insolation.integrate_years(1)
    ts = model_with_insolation.timeave['Ts']
    assert _check_minmax(ts, 225.402329962, 301.659494398)

@pytest.mark.slow
def test_rcmodel(rcmodel):
    """Check that we can integrate forwrd the radiative convective model and
    get expected atmospheric temperature."""
    rcmodel.step_forward()
    rcmodel.integrate_years(1)
    tatm = rcmodel.timeave['Tatm']
    assert _check_minmax(tatm, 176.786517491, 292.222277112)

@pytest.mark.slow
def test_diffmodel(diffmodel):
    """Check that we can integrate the model with diffusion."""
    diffmodel.step_forward()
    diffmodel.integrate_years(1)
    tatm = diffmodel.timeave['Tatm']
    assert _check_minmax(tatm, 208.689339823, 285.16085319)
