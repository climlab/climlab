from climlab.domain import column_state
from climlab.dynamics import TwoDimensionalAdvectionDiffusion
import pytest


@pytest.mark.fast
def test_2D_advection_diffusion_basic():
    '''Can we initialize a 2D lat-pressure model and step forward with zero diffusion and advection?'''
    state = column_state(num_lev=30, num_lat=40, water_depth=10.)
    m = TwoDimensionalAdvectionDiffusion(name='Advection-Diffusion', 
                                         state={'Tatm': state['Tatm']},
                                         Kyy=0.,
                                         Kyz=0.,
                                         Kzz=0.,
                                         U=0.,
                                         W=0.,
                                         )
    m.step_forward()
