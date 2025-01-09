import numpy as np
import climlab
from climlab.convection import SimplifiedBettsMiller
from climlab.surface import SensibleHeatFlux, LatentHeatFlux
from climlab.radiation import RRTMG
from climlab import couple
from climlab.tests.xarray_test import to_xarray
from climlab.utils import constants as const
import pytest


def make_initial_column():
    '''Create a single column state dictionary with initial conditions'''
    num_lev = 30
    water_depth = 10.
    full_state = climlab.column_state(water_depth=water_depth, num_lev=num_lev)
    # set initial conditions -- 24C at the surface, -60C at 200 hPa, isothermal stratosphere
    strat_idx = 6
    Tinitial = np.zeros(num_lev)
    Tinitial[:strat_idx] = -60. + const.tempCtoK
    Tinitial[strat_idx:] = np.linspace(-60, 22, num_lev-strat_idx) + const.tempCtoK
    Tsinitial = 24. + const.tempCtoK

    full_state = climlab.column_state(water_depth=water_depth, num_lev=num_lev)
    full_state['Tatm'][:] = Tinitial
    full_state['Ts'][:] = Tsinitial

    # Use Manabe's profile as an initial condition for specific humidity
    Q = full_state['Tatm'].domain.axes['lev'].points / const.ps
    RHprofile = 0.7 * ((Q-0.02) / (1-0.02))
    e = climlab.utils.thermo.clausius_clapeyron(full_state['Tatm']) * RHprofile
    qStrat = 5.E-6
    qinitial = np.maximum(qStrat, e/full_state['Tatm'].domain.axes['lev'].points * const.Rd / const.Rv)
    full_state['q'] = 0.*full_state.Tatm + qinitial
    return full_state

@pytest.mark.compiled
@pytest.mark.fast
def test_sbm_column():
    full_state = make_initial_column()
    conv = SimplifiedBettsMiller(name='Convection',
                                 state=full_state,)
    conv.step_forward()

@pytest.fixture()
def moist_column():
    '''Set up a complete single-column model.'''
    full_state = make_initial_column()
    short_timestep = const.seconds_per_hour * 3
    long_timestep = short_timestep*3
    insolation = 342.
    albedo = 0.18
    temperature_state = {'Tatm':full_state.Tatm,'Ts':full_state.Ts}
    #  Surface model
    shf = SensibleHeatFlux(name='Sensible Heat Flux',
                        state=temperature_state, Cd=3E-3,
                        timestep=short_timestep)
    lhf = LatentHeatFlux(name='Latent Heat Flux',
                        state=full_state, Cd=3E-3,
                        timestep=short_timestep)
    surface = couple([shf,lhf], name="Slab")
    #  Convection scheme -- water vapor is a state variable
    conv = SimplifiedBettsMiller(name='Convection',
                                state=full_state,
                                timestep=short_timestep,
                            )  
    rad = RRTMG(name='Radiation',
                    state=temperature_state,
                    specific_humidity=full_state.q,  # water vapor is an input here, not a state variable
                    albedo=albedo,
                    insolation=insolation,
                    timestep=long_timestep,
                    icld=0, # no clouds
                    )
    atm = couple([rad, conv], name='Atmosphere')
    moistmodel = couple([atm,surface], name='Moist column model')
    return moistmodel

@pytest.mark.compiled
@pytest.mark.fast
def test_moist_column(moistmodel):
    '''This test sets up a complete single-column model.
    The initial condition is completely dry.
    We test to see if the column moistens itself via surface evaporation and convection.
    
    Actually a basic test to see if we can create a single column and step forward.'''
    moistmodel.step_forward()
