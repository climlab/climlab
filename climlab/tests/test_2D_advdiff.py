from climlab.domain import column_state
from climlab.dynamics import TwoDimensionalAdvectionDiffusion
import pytest
import numpy as np


@pytest.fixture()
def model_with_gaussian():
    '''Set up a 2D domain with a gaussian maximum of the tracer (temperature)'''
    initialstate = column_state(num_lev=30, num_lat=40, water_depth=10.)
    Tatm = initialstate['Tatm']
    Tatm_xarray = Tatm.to_xarray()
    lat = Tatm_xarray.lat
    sigma = 30.  # standard deviation in degrees latitude
    Tatm_xarray[:,:] = np.exp(-lat**2/2/sigma**2)
    Tinitial = Tatm_xarray.values
    initialstate['Tatm'][:,:] = Tinitial
    m = TwoDimensionalAdvectionDiffusion(name='Simple Diffusion', 
                                         state={'Tatm': initialstate['Tatm']},
                                         timestep = 3600,
                                         )
    return m

@pytest.mark.fast
def test_2D_advection_diffusion_null(model_with_gaussian):
    '''Can we initialize a 2D lat-pressure model and step forward with zero diffusion and advection?'''
    model_with_gaussian.step_forward()

@pytest.mark.fast
def test_2D_simple_diffusion(model_with_gaussian):
    '''Does diffusion reduce a local maximum of the tracer?'''
    model_with_gaussian.Kyy = 1E6  # set meridional diffusion in m**2 / s
    max_initial = np.max(model_with_gaussian.Tatm)
    model_with_gaussian.step_forward()
    max_final = np.max(model_with_gaussian.Tatm)
    assert max_final < max_initial

@pytest.mark.fast
def test_2D_simple_advection(model_with_gaussian):
    '''Does advection move a local maximum in the expected direction?'''
    model_with_gaussian.U = 10.  # m/s northward advection velocity
    lat = model_with_gaussian.lat
    lat_of_max_initial = lat[np.argmax(model_with_gaussian.Tatm, axis=0)]
    model_with_gaussian.step_forward()
    # The local maximum should have moved northward
    lat_of_max_final = lat[np.argmax(model_with_gaussian.Tatm, axis=0)]
    assert np.all(lat_of_max_final > lat_of_max_initial)