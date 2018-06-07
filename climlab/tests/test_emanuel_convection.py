from __future__ import division
import numpy as np
import climlab
from climlab.convection import emanuel_convection
from climlab.tests.xarray_test import to_xarray
import pytest


#  These test data are based on direct single-column tests of the CONVECT43c.f
#  fortran source code. We are just checking to see if we get the right tendencies
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
#  TENDENCIES FROM FORTRAN CODE
FT = np.flipud([-1.79016788E-05,  -5.30500938E-06,  -1.31774368E-05,
        -1.52709208E-06,   2.39793881E-05,   5.00326714E-05,   5.81094064E-05,
        3.53246978E-05,   2.92667046E-05,   1.72944201E-05,  -1.29259779E-05,
        -1.95585071E-05, 0.00000000,  0.00000000,  0.00000000, 0.00000000,
        0.00000000,  0.00000000,   0.00000000,   0.00000000])
FQ = np.flipud([-1.25266510E-07,  -1.77205965E-08,   2.25621442E-08,
        1.20601991E-08,  -2.24871144E-09,  -8.65546035E-09,   1.32086608E-08,
        3.48950842E-08,   4.61437244E-09,   3.59271168E-09,   3.54269192E-09,
        1.12591925E-09,   0.00000000,       0.00000000,       0.00000000,
        0.00000000,  0.00000000,  0.00000000 ,   0.00000000,   0.00000000])
FU = np.flipud([6.96138741E-05,   2.54272982E-05,  -4.23727352E-06,
        -2.25807025E-06,   5.97735743E-06,   1.29817499E-05,  -7.07237768E-06,
        -5.06039614E-05,  -8.67366180E-06,  -1.08617351E-05,  -1.97424633E-05,
        -1.05507343E-05,   0.00000000,       0.00000000,       0.00000000,
  0.00000000,       0.00000000,       0.00000000,       0.00000000,       0.00000000])
FV = np.zeros_like(FU)


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
def test_convect_tendencies():
    # Temperatures in a single column
    state = climlab.column_state(num_lev=num_lev)
    state.Tatm[:] = T
    state['q'] = state.Tatm * 0. + Q
    state['U'] = state.Tatm * 0. + U
    state['V'] = state.Tatm * 0. + V
    assert hasattr(state, 'Tatm')
    assert hasattr(state, 'q')
    assert hasattr(state, 'U')
    assert hasattr(state, 'V')
    conv = emanuel_convection.EmanuelConvection(state=state, timestep=DELT)
    conv.step_forward()
    #  Did we get all the correct output?
    assert conv.IFLAG == 1
    #  relative tolerance for these tests ...
    tol = 1E-5
    assert conv.CBMF == pytest.approx(3.10377218E-02, rel=tol)
    tend = conv.tendencies
    assert FT == pytest.approx(tend['Tatm'], rel=tol)
    assert FQ == pytest.approx(tend['q'], rel=tol)
    assert FU == pytest.approx(tend['U'], rel=tol)
    assert FV == pytest.approx(tend['V'], rel=tol)

@pytest.mark.compiled
@pytest.mark.fast
def test_multidim_tendencies():
    # Same test just repeated in two parallel columns
    num_lat = 2
    state = climlab.column_state(num_lev=num_lev, num_lat=num_lat)
    state['q'] = state.Tatm * 0. #+ Q
    state['U'] = state.Tatm * 0. #+ U
    state['V'] = state.Tatm * 0. #+ V
    for i in range(num_lat):
        state.Tatm[i,:] = T
        state['q'][i,:] += Q
        state['U'][i,:] += U
        state['V'][i,:] += V
    assert hasattr(state, 'Tatm')
    assert hasattr(state, 'q')
    assert hasattr(state, 'U')
    assert hasattr(state, 'V')
    conv = emanuel_convection.EmanuelConvection(state=state, timestep=DELT)
    conv.step_forward()
    #  Did we get all the correct output?
    assert np.all(conv.IFLAG == 1)
    #  relative tolerance for these tests ...
    tol = 1E-5
    assert np.all(conv.CBMF == pytest.approx(3.10377218E-02, rel=tol))
    tend = conv.tendencies
    assert np.tile(FT,(num_lat,1)) == pytest.approx(tend['Tatm'], rel=tol)
    assert np.tile(FQ,(num_lat,1)) == pytest.approx(tend['q'], rel=tol)
    assert np.tile(FU,(num_lat,1)) == pytest.approx(tend['U'], rel=tol)
    assert np.tile(FV,(num_lat,1)) == pytest.approx(tend['V'], rel=tol)

@pytest.mark.compiled
@pytest.mark.fast
def test_rcm_emanuel():
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
                        timestep=short_timestep)
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
    model.step_forward()
    to_xarray(model)
