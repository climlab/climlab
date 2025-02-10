import numpy as np
from climlab.domain.initial import column_state
from climlab.convection.large_scale_condensation import LargeScaleCondensation
import pytest

num_lev = 20
#  INPUT DATA
lev = np.array([ 25.,  75., 125., 175., 225., 275., 325., 375., 425., 475., 525.,
       575., 625., 675., 725., 775., 825., 875., 925., 975.])
T = np.array([270. , 240. , 214. , 212.3, 216.4, 220.5, 224.6, 228.7, 232.8,
       236.9, 241.1, 245.2, 249.3, 253.4, 257.5, 261.6, 265.7, 269.8,
       273.9, 278. ])
Ts = np.array([288.])
rh = np.linspace(1.5, 0.2, num_lev)
DELT = 60.0*10.
pmin = 10.0
condensation_time = 3600.
RH_ref = 1.0

tol = 1E-5

latent_heating_test = np.array([ 2.64388431e+04,  4.81541867e+02,  1.34993415e+01,  6.29719694e+00,
        6.30859851e+00,  5.90859239e+00,  4.55441935e+00,  1.47017837e+00,
       -0.00000000e+00, -0.00000000e+00, -0.00000000e+00, -0.00000000e+00,
       -0.00000000e+00, -0.00000000e+00, -0.00000000e+00, -0.00000000e+00,
       -0.00000000e+00, -0.00000000e+00, -0.00000000e+00, -0.00000000e+00])
precipitation_lsc_test = np.array([0.01078337])

q_test = np.array([1.82793006e-01, 4.26194119e-03, 1.36639875e-04, 7.51032556e-05,
       9.34924128e-05, 1.19464586e-04, 1.54226244e-04, 1.99423280e-04,
       2.54847575e-04, 3.21809498e-04, 4.05769102e-04, 4.99878772e-04,
       6.06499207e-04, 7.23166732e-04, 8.45198974e-04, 9.64918546e-04,
       1.07071617e-03, 1.14593540e-03, 1.16756041e-03, 1.10468730e-03])
Tatm_test = np.array([300.9682066 , 240.56403709, 214.01581198, 212.307376  ,
       216.40738935, 220.50692082, 224.60533466, 228.70172204,
       232.8       , 236.9       , 241.1       , 245.2       ,
       249.3       , 253.4       , 257.5       , 261.6       ,
       265.7       , 269.8       , 273.9       , 278.        ])

@pytest.mark.fast
def test_lsc_single_column():
    # Temperatures in a single column
    state = column_state(num_lev=num_lev)
    state.Tatm[:] = T
    state.Ts[:] = Ts
    from climlab.utils.thermo import qsat
    Q = rh * qsat(T, state.Tatm.domain.lev.points)
    state['q'] = state.Tatm * 0. + Q
    assert hasattr(state, 'q')
    assert hasattr(state, 'Tatm')

    lsc = LargeScaleCondensation(state=state, timestep=DELT, RH_ref=RH_ref, condensation_time=condensation_time, pmin=pmin)
    lsc.compute_diagnostics()
    assert lsc.precipitation_lsc == pytest.approx(precipitation_lsc_test, rel=tol)
    assert lsc.latent_heating == pytest.approx(latent_heating_test, rel=tol)
    lsc.step_forward()
    assert lsc.q == pytest.approx(q_test, rel=tol)
    assert lsc.Ts == pytest.approx(Ts, rel=tol)
    assert lsc.Tatm == pytest.approx(Tatm_test, rel=tol)

@pytest.mark.fast
def test_lsc_multi_column():
    # Same test just repeated in two parallel columns
    num_lat = 2
    state = column_state(num_lev=num_lev, num_lat=num_lat)
    state.Tatm[:] = T
    state.Ts[:] = Ts
    from climlab.utils.thermo import qsat
    Q = rh * qsat(T, state.Tatm.domain.lev.points)
    state['q'] = state.Tatm * 0.
    for i in range(num_lat):
        state.Tatm[i,:] = T
        state['q'][i,:] += Q
    assert hasattr(state, 'Tatm')
    assert hasattr(state, 'q')
    lsc = LargeScaleCondensation(state=state, timestep=DELT, RH_ref=RH_ref, condensation_time=condensation_time, pmin=pmin)
    lsc.compute_diagnostics()
    assert lsc.precipitation_lsc == pytest.approx(np.repeat(precipitation_lsc_test[np.newaxis,...], num_lat, axis=0), rel=tol)
    assert lsc.latent_heating == pytest.approx(np.repeat(latent_heating_test[np.newaxis,...], num_lat, axis=0), rel=tol)
    lsc.step_forward()
    assert lsc.q == pytest.approx(np.repeat(q_test[np.newaxis,...], num_lat, axis=0), rel=tol)
    assert lsc.Ts == pytest.approx(np.repeat(Ts[np.newaxis,...], num_lat, axis=0), rel=tol)
    assert lsc.Tatm == pytest.approx(np.repeat(Tatm_test[np.newaxis,...], num_lat, axis=0), rel=tol)