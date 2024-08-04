import numpy as np
from climlab.domain.initial import column_state
from climlab.surface.turbulent import SensibleHeatFlux, LatentHeatFlux
import pytest

num_lev = 20
#  INPUT DATA
lev = np.array([ 25.,  75., 125., 175., 225., 275., 325., 375., 425., 475., 525.,
       575., 625., 675., 725., 775., 825., 875., 925., 975.])
T = np.array([270. , 240. , 214. , 212.3, 216.4, 220.5, 224.6, 228.7, 232.8,
       236.9, 241.1, 245.2, 249.3, 253.4, 257.5, 261.6, 265.7, 269.8,
       273.9, 278. ])
Ts = np.array([288.])

rh = 0.8
DELT = 60.0*10.

Cd = 2.0e-3
p_turb_layer = 50.0

tol = 1E-5

lhf_prof_test = np.array([192.51393335])
shf_prof_test = np.array([125.83661294])

Ts_tend_lhf_test = np.array([-4.60416457e-05])
Ts_tend_shf_test = np.array([-3.00950931e-05])
Tatm_prof_tend_test = np.array([8.70029832e-13, 2.36498624e-12, 6.42869917e-12, 1.74750161e-11, 4.75020187e-11, 1.29123874e-10, 3.50995081e-10, 9.54103551e-10, 2.59352234e-09, 7.04992466e-09, 1.91636821e-08, 5.20922888e-08, 1.41601522e-07, 3.84912844e-07, 1.04630159e-06, 2.84414260e-06, 7.73118115e-06, 2.10155292e-05, 5.71261312e-05, 1.55284924e-04])
q_prof_tend_test = np.array([5.34543430e-16, 1.45303966e-15, 3.94977134e-15, 1.07365917e-14,
 2.91850820e-14, 7.93332780e-14, 2.15650208e-13, 5.86198042e-13, 1.59345149e-12, 4.33145022e-12, 1.17741024e-11, 3.20053286e-11, 8.69995033e-11, 2.36489169e-10, 6.42844210e-10, 1.74743174e-09, 4.75001193e-09, 1.29118711e-08, 3.50981046e-08, 9.54065401e-08])
q_1cell_tend_test = np.zeros((num_lev,))
q_1cell_tend_test[-1] = 1.50930924e-07
Tatm_1cell_tend_test = np.zeros((num_lev,))
Tatm_1cell_tend_test[-1] = 0.00024566


@pytest.mark.fast
def test_surf_flux_single_column():
    from copy import deepcopy
    # Temperatures in a single column
    state = column_state(num_lev=num_lev)
    state.Tatm[:] = T
    state.Ts[:] = Ts
    from climlab.utils.thermo import qsat
    Q = rh * qsat(T, state.Tatm.domain.lev.points)
    state['q'] = state.Tatm * 0. + Q
    assert hasattr(state, 'q')
    assert hasattr(state, 'Tatm')

    lhf_prof = LatentHeatFlux(state=deepcopy(state), timestep=DELT, Cd=Cd, p_turb_layer=p_turb_layer)
    shf_prof = SensibleHeatFlux(state=deepcopy(state), timestep=DELT, Cd=Cd, p_turb_layer=p_turb_layer)
    lhf_1cell = LatentHeatFlux(state=deepcopy(state), timestep=DELT, Cd=Cd)
    shf_1cell = SensibleHeatFlux(state=deepcopy(state), timestep=DELT, Cd=Cd)
    for proc in [lhf_prof, lhf_1cell]:
        proc.compute_diagnostics()
        assert proc.LHF == pytest.approx(lhf_prof_test, rel=tol)
    for proc in [shf_prof, shf_1cell]:
        proc.compute_diagnostics()
        assert proc.SHF == pytest.approx(shf_prof_test, rel=tol)
    for proc in [lhf_prof, lhf_1cell, shf_prof, shf_1cell]:
        proc.step_forward()
    assert lhf_prof.Ts == pytest.approx(Ts+Ts_tend_lhf_test*DELT, rel=tol)
    assert lhf_prof.Tatm == pytest.approx(T, rel=tol)
    assert lhf_prof.q == pytest.approx(Q+q_prof_tend_test*DELT, rel=tol)
    assert lhf_1cell.Ts == pytest.approx(Ts+Ts_tend_lhf_test*DELT, rel=tol)
    assert lhf_1cell.Tatm == pytest.approx(T, rel=tol)
    assert lhf_1cell.q == pytest.approx(Q+q_1cell_tend_test*DELT, rel=tol)

    assert shf_prof.Ts == pytest.approx(Ts+Ts_tend_shf_test*DELT, rel=tol)
    assert shf_prof.Tatm == pytest.approx(T+Tatm_prof_tend_test*DELT, rel=tol)
    assert shf_prof.q == pytest.approx(Q, rel=tol)
    assert shf_1cell.Ts == pytest.approx(Ts+Ts_tend_shf_test*DELT, rel=tol)
    assert shf_1cell.Tatm == pytest.approx(T+Tatm_1cell_tend_test*DELT, rel=tol)
    assert shf_1cell.q == pytest.approx(Q, rel=tol)