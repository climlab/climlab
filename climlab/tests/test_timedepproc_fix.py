from __future__ import division
import numpy as np
import climlab
from climlab.convection import emanuel_convection
import pytest
from copy import deepcopy

# In these test we start from a model similar to the emanuel_rcm test (chosen because it has tendencies for all Ts, Tatm, q).
# We set bounds on the values of Tatm, and fix certain cells (without any physical justification).
# The test makes sure the fields remained within bounds, and that the fixed cells maintained there values
num_lev = 20
#  INPUT DATA
T = np.flipud([278.0, 273.9, 269.8, 265.7, 261.6, 257.5, 253.4, 249.3, 245.2,
    241.1, 236.9, 232.8, 228.7, 224.6, 220.5, 216.4, 212.3, 214.0, 240., 270.])
Q = np.flipud([3.768E-03, 2.812E-03, 2.078E-03, 1.519E-03, 1.099E-03,
            7.851E-04, 5.542E-04, 3.860E-04, 2.652E-04, 1.794E-04,
            1.183E-04, 7.739E-05, 4.970E-05, 3.127E-05, 1.923E-05,
            1.152E-05, 6.675E-06, 5.000E-06, 5.000E-06, 5.000E-06])
U = np.flipud([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0,
                12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0])
V = 5. * np.ones_like(U)
DELT = 60.0*10.

bound_dict = {'Tatm': (220.0, np.inf)}
fixed_cells_1d_dict = {'q': np.arange(num_lev // 2, num_lev // 2+3), 'Tatm': np.array([num_lev-2], dtype=int)}
fixed_cells_2d_dict = {k: (..., v) for k, v in fixed_cells_1d_dict.items()}

#  Set thermodynamic constants to their defaults from Emanuel's code
#   so that we get same tendencies
emanuel_convection.CPD=1005.7
emanuel_convection.CPV=1870.0
emanuel_convection.RV=461.5
emanuel_convection.RD=287.04
emanuel_convection.LV0=2.501E6
emanuel_convection.G=9.8
emanuel_convection.ROWL=1000.0

@pytest.mark.compiled
@pytest.mark.fast
def test_rcm_emanuel_bound_1col():
    num_lev = 30
    water_depth = 5.
    # Temperatures in a single column
    state = climlab.column_state(num_lev=num_lev, water_depth=water_depth)
    #  Initialize a nearly dry column (small background stratospheric humidity)
    state['q'] = np.ones_like(state.Tatm) * 5.E-6
    #  ASYNCHRONOUS COUPLING -- the radiation uses a much longer timestep
    short_timestep = climlab.constants.seconds_per_hour
    #  The top-level model
    model = climlab.TimeDependentProcess(name='Radiative-Convective Model',
                        state=state,
                        timestep=short_timestep, bound_dict=bound_dict)
    #  Radiation coupled to water vapor
    rad = climlab.radiation.RRTMG(name='Radiation',
                        state=state,
                        specific_humidity=state.q,
                        albedo=0.3,
                        timestep=24*short_timestep)
    #  Convection scheme -- water vapor is a state variable
    conv = climlab.convection.EmanuelConvection(name='Convection',
                                  state=state,
                                  timestep=short_timestep)
    #  Surface heat flux processes
    shf = climlab.surface.SensibleHeatFlux(name='SHF',
                                  state=state, Cd=0.5E-3,
                                  timestep=climlab.constants.seconds_per_hour)
    lhf = climlab.surface.LatentHeatFlux(name='LHF',
                                  state=state, Cd=0.5E-3,
                                  timestep=short_timestep)
    #  Couple all the submodels together
    for proc in [rad, conv, shf, lhf]:
        model.add_subprocess(proc.name, proc)
    model.do_fixed_cells = True
    model.fixed_cells_dict = fixed_cells_1d_dict
    model0 = deepcopy(model)
    model.integrate_years(1.0)
    for key, ind in fixed_cells_1d_dict.items():
        assert model.__dict__[key][ind] == pytest.approx(model0.__dict__[key][ind], rel=0.001)
    for key, bound in bound_dict.items():
        assert np.all(model.__dict__[key] <= bound[1])
        assert np.all(model.__dict__[key] >= bound[0])


@pytest.mark.compiled
@pytest.mark.fast
def test_rcm_emanuel_bound_2d():
    num_lev = 30
    water_depth = 5.
    # Temperatures in a single column
    state = climlab.column_state(num_lev=num_lev, num_lat=5, water_depth=water_depth)
    #  Initialize a nearly dry column (small background stratospheric humidity)
    state['q'] = np.ones_like(state.Tatm) * 5.E-6
    #  ASYNCHRONOUS COUPLING -- the radiation uses a much longer timestep
    short_timestep = climlab.constants.seconds_per_hour
    #  The top-level model
    model = climlab.TimeDependentProcess(name='Radiative-Convective Model',
                        state=state,
                        timestep=short_timestep, bound_dict=bound_dict)
    #  Radiation coupled to water vapor
    rad = climlab.radiation.RRTMG(name='Radiation',
                        state=state,
                        specific_humidity=state.q,
                        albedo=0.3,
                        timestep=24*short_timestep)
    #  Convection scheme -- water vapor is a state variable
    conv = climlab.convection.EmanuelConvection(name='Convection',
                                  state=state,
                                  timestep=short_timestep)
    #  Surface heat flux processes
    shf = climlab.surface.SensibleHeatFlux(name='SHF',
                                  state=state, Cd=0.5E-3,
                                  timestep=climlab.constants.seconds_per_hour)
    lhf = climlab.surface.LatentHeatFlux(name='LHF',
                                  state=state, Cd=0.5E-3,
                                  timestep=short_timestep)
    #  Couple all the submodels together
    for proc in [rad, conv, shf, lhf]:
        model.add_subprocess(proc.name, proc)
    model.do_fixed_cells = True
    model.fixed_cells_dict = fixed_cells_2d_dict
    model0 = deepcopy(model)
    model.integrate_years(1.0)
    for key, ind in fixed_cells_2d_dict.items():
        assert model.__dict__[key][ind] == pytest.approx(model0.__dict__[key][ind], rel=0.001)
    for key, bound in bound_dict.items():
        assert np.all(model.__dict__[key] <= bound[1])
        assert np.all(model.__dict__[key] >= bound[0])