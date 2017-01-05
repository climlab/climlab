from __future__ import division
import numpy as np
import climlab
import pytest

# The fixtures are reusable pieces of code to set up the input to the tests.
# Without fixtures, we would have to do a lot of cutting and pasting
# I inferred which fixtures to use from the notebook
# Latitude-dependent grey radiation.ipynb
@pytest.fixture()
def model():
    return climlab.GreyRadiationModel(num_lev=30, num_lat=90)

@pytest.fixture()
def model_with_insolation(model):
    insolation = climlab.radiation.insolation.DailyInsolation(
                                                        domains=model.Ts.domain)
    model.add_subprocess('insolation', insolation)
    model.subprocess.SW.flux_from_space = insolation.insolation
    return model

@pytest.fixture()
def rcmodel():
    model2 = climlab.RadiativeConvectiveModel(num_lev=30, num_lat=90)
    insolation = climlab.radiation.insolation.DailyInsolation(domains=model2.Ts.domain)
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
    d = climlab.dynamics.diffusion.MeridionalDiffusion(K=K,
                state={'Tatm': diffmodel.state['Tatm']}, **diffmodel.param)
    diffmodel.add_subprocess('diffusion', d)
    return diffmodel

@pytest.fixture()
def diffmodel_surfflux(diffmodel):
    diffmodel_surfflux = climlab.process_like(diffmodel)
    # process models for surface heat fluxes
    shf = SensibleHeatFlux(state=diffmodel_surfflux.state, Cd=0.5E-3)
    lhf = LatentHeatFlux(state=diffmodel_surfflux, Cd=0.5E-3)
    # set the water vapor input field for LHF
    lhf.q = diffmodel_surfflux.q
    diffmodel_surfflux.add_subprocess('SHF', shf)
    diffmodel_surfflux.add_subprocess('LHF', lhf)
    #  Convective adjustment for atmosphere only
    diffmodel_surfflux.remove_subprocess('convective adjustment')
    conv = ConvectiveAdjustment(state={'Tatm':diffmodel_surfflux.state['Tatm']},
                                **diffmodel_surfflux.param)
    diffmodel_surfflux.add_subprocess('convective adjustment', conv)
    return diffmodel_surfflux


# helper for a common test pattern
def _check_minmax(array, amin, amax):
    return (np.allclose(array.min(), amin) and
            np.allclose(array.max(), amax))

def test_model_creation(model):
    """Just make sure we can create a model."""
    assert len(model.lat)==90

def test_add_insolation(model_with_insolation):
    """"Create a model with insolation and check that SW_down_TOA has
    reasonable values."""
    model_with_insolation.step_forward()
    assert _check_minmax(model_with_insolation.SW_down_TOA, 0, 555.17111)

def test_integrate_years(model_with_insolation):
    """Check that we can integrate forward the model and get the expected
    surface temperature."""
    model_with_insolation.step_forward()
    model_with_insolation.integrate_years(1)
    ts = model_with_insolation.timeave['Ts']
    assert _check_minmax(ts, 225.402329962, 301.659494398)

def test_rcmodel(rcmodel):
    """Check that we can integrate forwrd the radiative convective model and
    get expected atmospheric temperature."""
    rcmodel.step_forward()
    rcmodel.integrate_years(1)
    tatm = rcmodel.timeave['Tatm']
    assert _check_minmax(tatm, 176.786517491, 292.222277112)

def test_diffmodel(diffmodel):
    """Check that we can integrate the model with diffusion."""
    diffmodel.step_forward()
    diffmodel.integrate_years(1)
    tatm = diffmodel.timeave['Tatm']
    assert _check_minmax(tatm, 208.689339823, 285.16085319)
