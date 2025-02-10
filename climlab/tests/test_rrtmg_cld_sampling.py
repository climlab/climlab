from __future__ import division
import numpy as np
import climlab
import pytest
from copy import deepcopy

num_lev = 50
num_lat = 200

cldfrac0 = 0.5  # layer cloud fraction
r_liq = 14.  # Cloud water drop effective radius (microns)
clwp = 60.  # in-cloud liquid water path (g/m2)
ciwp = 0.  # in-cloud liquid water path (g/m2)
r_ice = r_liq
#  The cloud fraction is a Gaussian bump centered at level i
p0 = 500.0
cloud_dp = 100.0
flux_variability_expectation = 1.0

small_num = 1e-11

#  State variables (Air and surface temperature)
state = climlab.column_state(num_lev=num_lev, num_lat = num_lat, water_depth=1.)
state_1d = climlab.column_state(num_lev=num_lev, water_depth=1.)
lev = state.Tatm.domain.axes['lev'].points
lat = state.Tatm.domain.axes['lat'].points

ind_p = np.argmin(lev-p0)


cldfrac = np.repeat((cldfrac0 * np.exp(-(lev-lev[ind_p])**2/(2*cloud_dp)**2))[np.newaxis,:], num_lat, axis=0)
#  Define some local cloud characteristics
mycloud = {'cldfrac': cldfrac + np.zeros_like(state.Tatm),
            'clwp': np.zeros_like(state.Tatm) + clwp,
            'r_liq': np.zeros_like(state.Tatm) + r_liq,
            'ciwp': np.zeros_like(state.Tatm) + ciwp,
            'r_ice': np.zeros_like(state.Tatm) + r_ice}
mycloud_1d = {k: v[0,:] for k,v in mycloud.items()}

insolation = climlab.constants.S0 / 4 * np.ones((num_lat, 1))
shared_param_dict = {'state': state, **mycloud, 'insolation': insolation}
shared_param_dict_1d = {'state': state_1d, **mycloud_1d, 'insolation': insolation[0,:]}

@pytest.mark.compiled
@pytest.mark.fast
def test_cloud_sampling_approached():
    rad_no_clouds = climlab.radiation.RRTMG(**shared_param_dict, icld=0)
    rad_default = climlab.radiation.RRTMG(**shared_param_dict)
    rad_no_rand1 = climlab.radiation.RRTMG(**shared_param_dict, icld=1, do_col_by_col=True)
    rad_seed_permut = climlab.radiation.RRTMG(**shared_param_dict, do_seed_permutation=True)
    # rad_no_rand2 = climlab.radiation.RRTMG(**shared_param_dict, icld=3) # presumed to have no randomization, but that doesn't seem to be the case
    rad_avg_results = climlab.radiation.RRTMG(**shared_param_dict_1d, do_seed_permutation=True, n_rrtmg_repeat=num_lat)

    O3 = rad_no_clouds.absorber_vmr['O3']
    O3_avg = np.average(O3, weights=np.cos(np.deg2rad(lat)), axis=0)
    O3_flat = np.repeat(O3_avg[np.newaxis,:], num_lat, axis=0)
    rad_seed_permut_t1 = deepcopy(rad_seed_permut)
    rad_seed_permut_t1.time['steps'] += 1
    rad_default_t1 = deepcopy(rad_default)
    rad_default_t1.time['steps'] += 1
    rad_list = [rad_no_clouds, rad_default, rad_no_rand1, rad_seed_permut_t1, rad_default_t1]
    for rad in rad_list:
        rad.absorber_vmr['O3'] = O3_flat
        rad.compute_diagnostics()
    rad_avg_results.absorber_vmr['O3'] = O3_flat[0,:]
    rad_avg_results.compute_diagnostics()

    # radiation has no varibaility between columns without clouds
    assert(np.all(np.std(rad_no_clouds.SW_flux_net, axis=0) < small_num))
    assert(np.all(np.std(rad_no_clouds.LW_flux_net, axis=0) < small_num))

    std_sw_default = np.std(rad_default.SW_flux_net, axis=0)
    std_lw_default = np.std(rad_default.LW_flux_net, axis=0)
    mean_sw_default = np.mean(rad_default.SW_flux_net, axis=0)
    mean_lw_default = np.mean(rad_default.LW_flux_net, axis=0)

    # reflecting variability between columns due to using different seeds on the grid
    assert(np.all(std_sw_default > flux_variability_expectation))
    assert(np.all(std_lw_default > flux_variability_expectation))
    
    # not expected to randomize between differemt columns for col_by_col mode
    assert(np.all(np.std(rad_no_rand1.SW_flux_net, axis=0) < small_num))
    assert(np.all(np.std(rad_no_rand1.LW_flux_net, axis=0) < small_num))

    # not expected to randomize between time steps
    assert(np.all(rad_default_t1.SW_flux_net - rad_default.SW_flux_net == 0.0))
    assert(np.all(rad_default_t1.LW_flux_net - rad_default.LW_flux_net == 0.0))

    # after time stepping seed_permut expected to have variability between time steps
    assert(np.all(np.std(rad_seed_permut_t1.SW_flux_net - rad_seed_permut.SW_flux_net, axis=0) > flux_variability_expectation))
    assert(np.all(np.std(rad_seed_permut_t1.LW_flux_net - rad_seed_permut.LW_flux_net, axis=0) > flux_variability_expectation))

    # check variability reduction in averaged case
    assert(np.all(np.abs(rad_avg_results.SW_flux_net - mean_sw_default) < 2 / np.sqrt(num_lat) * std_sw_default))
    assert(np.all(np.abs(rad_avg_results.LW_flux_net - mean_lw_default) < 2 / np.sqrt(num_lat) * std_lw_default))