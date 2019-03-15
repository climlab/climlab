from __future__ import division
import numpy as np
import climlab
import pytest
from climlab.tests.xarray_test import to_xarray

# The fixtures are reusable pieces of code to set up the input to the tests.
# Without fixtures, we would have to do a lot of cutting and pasting
# I inferred which fixtures to use from the notebook
# Latitude-dependent grey radiation.ipynb
@pytest.fixture()
def model():
    return climlab.BandRCModel()

@pytest.fixture()
def diffmodel():
    ''' 2D radiative-convective model with band radiation including water vapor,
    fixed relative humidity, meridional heat transport (diffusion) and convective adjustment.
    '''
    diffmodel = climlab.BandRCModel(num_lev=30, num_lat=90)
    insolation = climlab.radiation.AnnualMeanInsolation(domains=diffmodel.Ts.domain)
    diffmodel.add_subprocess('insolation', insolation)
    diffmodel.subprocess.SW.flux_from_space = insolation.insolation
    # thermal diffusivity in W/m**2/degC
    D = 0.05
    # meridional diffusivity in 1/s
    K = D / diffmodel.Tatm.domain.heat_capacity[0]
    d = climlab.dynamics.MeridionalDiffusion(K=K,
                state={'Tatm': diffmodel.state['Tatm']},
                **diffmodel.param)
    diffmodel.add_subprocess('diffusion', d)
    return diffmodel

@pytest.fixture()
def diffmodel_surfflux(diffmodel):
    '''Explicit surface sensible and latent heat fluxes.'''
    diffmodel_surfflux = climlab.process_like(diffmodel)
    # process models for surface heat fluxes
    shf = climlab.surface.SensibleHeatFlux(state=diffmodel_surfflux.state, Cd=0.5E-3)
    lhf = climlab.surface.LatentHeatFlux(state=diffmodel_surfflux.state, Cd=0.5E-3)
    # set the water vapor input field for LHF
    lhf.q = diffmodel_surfflux.q
    diffmodel_surfflux.add_subprocess('SHF', shf)
    diffmodel_surfflux.add_subprocess('LHF', lhf)
    #  Convective adjustment for atmosphere only
    diffmodel_surfflux.remove_subprocess('convective adjustment')
    conv = climlab.convection.ConvectiveAdjustment(state={'Tatm':diffmodel_surfflux.state['Tatm']},
                                **diffmodel_surfflux.param)
    diffmodel_surfflux.add_subprocess('convective adjustment', conv)
    return diffmodel_surfflux


# helper for a common test pattern
def _check_minmax(array, amin, amax):
    return (np.allclose(array.min(), amin) and
            np.allclose(array.max(), amax))

@pytest.mark.fast
def test_model_creation(model):
    """Just make sure we can create a model."""
    assert len(model.Tatm)==30

@pytest.mark.fast
def test_diffmodel(diffmodel):
    """Check that we can integrate the model with diffusion."""
    diffmodel.step_forward()
    diffmodel.integrate_days(2)
    #  Would be better to have these tests evaluate a numerical condition
    #  Test the xarray interface
    to_xarray(diffmodel)

@pytest.mark.fast
def test_diffmodel_surfflux(diffmodel_surfflux):
    """Check that we can integrate the model with diffusion."""
    diffmodel_surfflux.step_forward()
    diffmodel_surfflux.integrate_days(2)

@pytest.mark.slow
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
