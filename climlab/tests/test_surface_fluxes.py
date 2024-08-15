import numpy as np
from climlab.domain.initial import column_state
from climlab.surface.turbulent import SensibleHeatFlux, LatentHeatFlux
from climlab.radiation import water_vapor
from climlab import constants as const
import pytest

num_lev = 50
#  INPUT DATA
lev = np.array([ 10.,  30.,  50.,  70.,  90., 110., 130., 150., 170., 190., 210.,
       230., 250., 270., 290., 310., 330., 350., 370., 390., 410., 430.,
       450., 470., 490., 510., 530., 550., 570., 590., 610., 630., 650.,
       670., 690., 710., 730., 750., 770., 790., 810., 830., 850., 870.,
       890., 910., 930., 950., 970., 990.])
T = np.array([230.71232829, 218.31385281, 214.07055291, 211.7631994 ,
       210.72000437, 210.57182878, 210.44671195, 210.54659783,
       210.86019941, 211.51731127, 212.40754012, 214.8745677 ,
       216.32147027, 218.32122569, 219.02273417, 220.16770413,
       221.54797056, 222.80272149, 226.09345401, 229.40383742,
       232.56024174, 235.56948631, 238.43779399, 241.17101228,
       243.77455284, 246.25421595, 248.61572646, 250.86497556,
       253.00743915, 255.04967597, 256.99651925, 258.85375289,
       260.62714075, 262.32223285, 263.94399978, 265.49683998,
       266.98568886, 268.41550039, 269.78792032, 271.10794889,
       272.37965193, 273.60567898, 274.78901626, 275.93048555,
       277.03409633, 278.10233355, 279.13672359, 280.13922758,
       281.11176887, 282.05329222])
Ts = np.array([283.58570499])
albedo = 0.149
GHGs = {'CO2': 0.00042, 'CH4': 1.935e-06, 'N2O': 3.37e-07}
cldfrac = np.array([4.21489191e-10, 6.55287469e-06, 2.90036833e-05, 6.52755966e-05,
       3.86978299e-05, 2.92734702e-04, 3.82216865e-03, 1.57547569e-02,
       3.47067379e-02, 5.27498680e-02, 6.84030343e-02, 8.18007079e-02,
       9.19828963e-02, 9.77996327e-02, 1.00112179e-01, 1.00533274e-01,
       9.96270074e-02, 9.76692820e-02, 9.47642833e-02, 9.09771284e-02,
       8.68033327e-02, 8.30208209e-02, 8.01637611e-02, 7.85350573e-02,
       7.80519228e-02, 7.82564906e-02, 7.81579677e-02, 7.63163320e-02,
       7.27834474e-02, 6.92231932e-02, 6.68628774e-02, 6.62851314e-02,
       6.75484407e-02, 7.00111110e-02, 7.31708837e-02, 7.69161147e-02,
       8.13520622e-02, 8.74268521e-02, 9.70716756e-02, 1.09785954e-01,
       1.22349840e-01, 1.30997282e-01, 1.33612524e-01, 1.31086234e-01,
       1.21108973e-01, 1.00996876e-01, 7.68909055e-02, 5.47001944e-02,
       3.81864452e-02, 2.96438170e-02])
clwp = np.array([4.80895017e-23, 5.51101432e-23, 6.13824566e-23, 6.56843214e-23,
       6.97906469e-23, 7.37014331e-23, 7.74166800e-23, 6.92955484e-09,
       8.53701590e-08, 2.68630337e-07, 2.52483748e-08, 4.44685803e-06,
       3.39680578e-07, 3.74808272e-05, 5.71168334e-04, 2.47020145e-03,
       7.64498424e-03, 1.85809259e-02, 3.86616930e-02, 7.26059322e-02,
       1.34413115e-01, 2.44669938e-01, 4.23666181e-01, 6.90227123e-01,
       1.05143390e+00, 1.49735810e+00, 1.88855566e+00, 1.97081708e+00,
       1.75775745e+00, 1.54680275e+00, 1.49005412e+00, 1.55039758e+00,
       1.68545618e+00, 1.86275996e+00, 2.05870481e+00, 2.26410427e+00,
       2.47402700e+00, 2.71622684e+00, 3.13558755e+00, 3.63123017e+00,
       3.94754968e+00, 3.93996112e+00, 3.65405980e+00, 3.20396661e+00,
       2.59239277e+00, 1.82590049e+00, 1.15626387e+00, 6.93899888e-01,
       3.93203670e-01, 2.36979954e-01])
ciwp = np.array([0.00693998, 0.00795315, 0.00885833, 0.00947915, 0.01007175,
       0.01063613, 0.01117229, 0.02592792, 0.09840765, 0.2233215 ,
       0.3888991 , 0.58155817, 0.78456322, 0.96517446, 1.08563027,
       1.15481923, 1.19328834, 1.22253132, 1.23880782, 1.2264473 ,
       1.18514945, 1.12574257, 1.05729271, 0.98655897, 0.91109276,
       0.82043817, 0.71691483, 0.61569036, 0.52740048, 0.45684523,
       0.40352321, 0.36106435, 0.32559569, 0.29691966, 0.27473536,
       0.25800185, 0.24644573, 0.24141039, 0.23768282, 0.22820484,
       0.20937786, 0.18239553, 0.15185114, 0.12198203, 0.09481655,
       0.07103145, 0.0517762 , 0.03746315, 0.02687983, 0.02032815])
r_ice = r_liq = 14.0

rh = 0.8
DELT = const.seconds_per_hour
DELT_large = const.seconds_per_day
n = int(5 * const.seconds_per_day / DELT)

Cd = 2.0e-3
p_turb_layer = 10.0
water_depth = 10.0

rel_tol = 1E-5
abs_tol_q = 1e-6

lhf_prof_test1 = np.array([112.83176795])
shf_prof_test1 = np.array([70.28861968])

Ts_tend_lhf_test1 = np.array([-2.69848535e-06])
Ts_tend_shf_test1 = np.array([-1.68102312e-06])

Tatm_prof_tend_test1 = np.array([0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 7.61706405e-20,
       4.95109163e-19, 3.77044670e-18, 2.77641985e-17, 2.05089450e-16,
       1.51545298e-15, 1.11976554e-14, 8.27401678e-14, 6.11371916e-13,
       4.51746135e-12, 3.33797753e-11, 2.46645032e-10, 1.82247398e-09,
       1.34663625e-08, 9.95037079e-08, 7.35238479e-07, 5.43271837e-06,
       4.01426608e-05, 2.96616373e-04])
q_prof_tend_test1 = np.array([0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 0.00000000e+00,
       0.00000000e+00, 0.00000000e+00, 0.00000000e+00, 4.91052233e-23,
       3.19183951e-22, 2.43070855e-21, 1.78988539e-20, 1.32215814e-19,
       9.76972969e-19, 7.21883611e-18, 5.33404260e-17, 3.94135512e-16,
       2.91228939e-15, 2.15190697e-14, 1.59005613e-13, 1.17490139e-12,
       8.68141232e-12, 6.41474426e-11, 4.73989052e-10, 3.50233170e-09,
       2.58789254e-08, 1.91220831e-07])
q_1cell_tend_test1 = np.zeros((num_lev,))
q_1cell_tend_test1[-1] = 2.21150265e-07
Tatm_1cell_tend_test1 = np.zeros((num_lev,))
Tatm_1cell_tend_test1[-1] = 0.00034304

lhf_prof_test2 = np.array([68.6727717])
shf_prof_test2 = np.array([19.00624971])

@pytest.mark.fast
def test_surf_flux_single_column():
    from copy import deepcopy
    # Temperatures in a single column
    state = column_state(num_lev=num_lev, water_depth=water_depth)
    Tatm0 = np.array(state.Tatm[:])
    state.Ts[:] = Ts
    lev = state.Tatm.domain.lev.points
    ps = state.Tatm.domain.lev.bounds.max()
    h2o = water_vapor.FixedRelativeHumidity(state=state, relative_humidity=lev/ps * rh, timestep=DELT)

    state['q'] = state.Tatm * 0. + h2o.q
    Q0 = np.array(h2o.q)
    assert hasattr(state, 'q')
    assert hasattr(state, 'Tatm')

    lhf_prof = LatentHeatFlux(state=deepcopy(state), timestep=DELT, Cd=Cd, p_turb_layer=p_turb_layer)
    shf_prof = SensibleHeatFlux(state=deepcopy(state), timestep=DELT, Cd=Cd, p_turb_layer=p_turb_layer)
    lhf_1cell = LatentHeatFlux(state=deepcopy(state), timestep=DELT, Cd=Cd)
    shf_1cell = SensibleHeatFlux(state=deepcopy(state), timestep=DELT, Cd=Cd)
    for proc in [lhf_prof, lhf_1cell]:
        proc.compute_diagnostics()
        assert proc.LHF == pytest.approx(lhf_prof_test1, rel=rel_tol)
    for proc in [shf_prof, shf_1cell]:
        proc.compute_diagnostics()
        assert proc.SHF == pytest.approx(shf_prof_test1, rel=rel_tol)
    for proc in [lhf_prof, lhf_1cell, shf_prof, shf_1cell]:
        proc.step_forward()
    assert lhf_prof.Ts == pytest.approx(Ts+Ts_tend_lhf_test1*DELT, rel=rel_tol)
    assert lhf_prof.Tatm == pytest.approx(Tatm0, rel=rel_tol)
    assert lhf_prof.q == pytest.approx(Q0+q_prof_tend_test1*DELT, abs=abs_tol_q)
    assert lhf_1cell.Ts == pytest.approx(Ts+Ts_tend_lhf_test1*DELT, rel=rel_tol)
    assert lhf_1cell.Tatm == pytest.approx(Tatm0, rel=rel_tol)
    assert lhf_1cell.q == pytest.approx(Q0+q_1cell_tend_test1*DELT, abs=abs_tol_q)

    assert shf_prof.Ts == pytest.approx(Ts+Ts_tend_shf_test1*DELT, rel=rel_tol)
    assert shf_prof.Tatm == pytest.approx(Tatm0+Tatm_prof_tend_test1*DELT, rel=rel_tol)
    assert shf_prof.q == pytest.approx(Q0, abs=abs_tol_q)
    assert shf_1cell.Ts == pytest.approx(Ts+Ts_tend_shf_test1*DELT, rel=rel_tol)
    assert shf_1cell.Tatm == pytest.approx(Tatm0+Tatm_1cell_tend_test1*DELT, rel=rel_tol)
    assert shf_1cell.q == pytest.approx(Q0, abs=abs_tol_q)


@pytest.mark.fast
def test_stability_column():
    from climlab import constants as const
    from climlab import TimeDependentProcess
    from climlab.convection import ConvectiveAdjustment
    from climlab.radiation import FixedInsolation, RRTMG
    from climlab.surface import LatentHeatFlux, LatentHeatFlux
    n = 1000
    n_avg1 = 1 #in principle wanted to test instability handling by the low-pass filter, but that requires a convection scheme with q as state parameter
    state = column_state(num_lev=num_lev, water_depth=water_depth)
    state.Tatm[:] = T
    state.Ts[:] = Ts

    my_cloud = {'cldfrac': cldfrac, 'clwp': clwp, 'r_liq': r_liq, 'ciwp': ciwp, 'r_ice': r_ice}
    lev = state.Tatm.domain.lev.points
    ps = state.Tatm.domain.lev.bounds.max()
    model = TimeDependentProcess(state=state, name='Radiative-Convective Model', timestep=DELT)
    h2o = water_vapor.FixedRelativeHumidity(state=state, relative_humidity=lev/ps * rh, timestep=DELT)
    conv = ConvectiveAdjustment(state={'Tatm': model.state['Tatm']}, adj_lapse_rate='MALR', timestep=DELT)
    sun = FixedInsolation(domains=model.Ts.domain, S0=const.S0/4, timestep=DELT)
    sun.coszen = sun.insolation / const.S0
    rad = RRTMG(state=state, specific_humidity=h2o.q, albedo = albedo, insolation=sun.insolation, coszen=sun.coszen, timestep=DELT_large, **my_cloud)
    for ghg, vmr in GHGs.items():
        rad.absorber_vmr[ghg] = vmr

    model.add_subprocess('Radiation', rad)
    model.add_subprocess('Insolation', sun)
    model.add_subprocess('WaterVapor', h2o)
    model.add_subprocess('Convection', conv)

    lhf = LatentHeatFlux(state=model.state, Cd = Cd, p_turb_layer=p_turb_layer, n_avg=n_avg1, timestep=DELT)
    lhf.q = model.subprocess['WaterVapor'].q
    shf = SensibleHeatFlux(state=model.state, Cd = Cd, p_turb_layer=p_turb_layer, n_avg=n_avg1, timestep=DELT)

    model.add_subprocess('LHF', lhf)
    model.add_subprocess('SHF', shf)
    model.compute_diagnostics()

    lhf_vect = np.zeros((n,))
    shf_vect = np.zeros((n,))
    Ts_vect = np.zeros((n,))
    rad_toa_vect = np.zeros((n,))
    rad_surf_vect = np.zeros((n,))
    for k in range(n):
        lhf_vect[k] = model.subprocess['LHF'].LHF[0]
        shf_vect[k] = model.subprocess['SHF'].SHF[0]
        Ts_vect[k] = model.Ts[0]
        rad_toa_vect[k] = model.SW_flux_net[0]-model.LW_flux_net[0]
        rad_surf_vect[k] = model.SW_flux_net[-1]-model.LW_flux_net[-1]
        model.step_forward()
        model.compute_diagnostics()
    assert np.abs((rad_surf_vect-(lhf_vect+shf_vect))[-1]) < 5e-2 #test surface balance
    assert np.abs(rad_toa_vect[-1]) < 5e-2 #test toa balance
    assert rad_toa_vect.std() < 2e-3
    assert (rad_surf_vect-(lhf_vect+shf_vect)).std() < 2e-3
