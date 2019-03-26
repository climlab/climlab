from __future__ import division
import numpy as np
from scipy.interpolate import interp1d
from climlab.utils.thermo import mmr_to_vmr


def _prepare_general_arguments(RRTMGobject):
    '''Prepare arguments needed for both RRTMG_SW and RRTMG_LW with correct dimensions.'''
    tlay = _climlab_to_rrtm(RRTMGobject.Tatm)
    tlev = _climlab_to_rrtm(interface_temperature(**RRTMGobject.state))
    play = _climlab_to_rrtm(RRTMGobject.lev * np.ones_like(tlay))
    plev = _climlab_to_rrtm(RRTMGobject.lev_bounds * np.ones_like(tlev))
    ncol, nlay = tlay.shape
    tsfc = _climlab_to_rrtm_sfc(RRTMGobject.Ts, RRTMGobject.Ts)
    # GASES -- put them in proper dimensions and units
    vapor_mixing_ratio = mmr_to_vmr(RRTMGobject.specific_humidity, gas='H2O')
    h2ovmr   = _climlab_to_rrtm(vapor_mixing_ratio * np.ones_like(RRTMGobject.Tatm))
    o3vmr    = _climlab_to_rrtm(RRTMGobject.absorber_vmr['O3'] * np.ones_like(RRTMGobject.Tatm))
    co2vmr   = _climlab_to_rrtm(RRTMGobject.absorber_vmr['CO2'] * np.ones_like(RRTMGobject.Tatm))
    ch4vmr   = _climlab_to_rrtm(RRTMGobject.absorber_vmr['CH4'] * np.ones_like(RRTMGobject.Tatm))
    n2ovmr   = _climlab_to_rrtm(RRTMGobject.absorber_vmr['N2O'] * np.ones_like(RRTMGobject.Tatm))
    o2vmr    = _climlab_to_rrtm(RRTMGobject.absorber_vmr['O2'] * np.ones_like(RRTMGobject.Tatm))
    cfc11vmr = _climlab_to_rrtm(RRTMGobject.absorber_vmr['CFC11'] * np.ones_like(RRTMGobject.Tatm))
    cfc12vmr = _climlab_to_rrtm(RRTMGobject.absorber_vmr['CFC12'] * np.ones_like(RRTMGobject.Tatm))
    cfc22vmr = _climlab_to_rrtm(RRTMGobject.absorber_vmr['CFC22'] * np.ones_like(RRTMGobject.Tatm))
    ccl4vmr  = _climlab_to_rrtm(RRTMGobject.absorber_vmr['CCL4'] * np.ones_like(RRTMGobject.Tatm))
    #  Cloud parameters
    cldfrac = _climlab_to_rrtm(RRTMGobject.cldfrac * np.ones_like(RRTMGobject.Tatm))
    ciwp = _climlab_to_rrtm(RRTMGobject.ciwp * np.ones_like(RRTMGobject.Tatm))
    clwp = _climlab_to_rrtm(RRTMGobject.clwp * np.ones_like(RRTMGobject.Tatm))
    relq = _climlab_to_rrtm(RRTMGobject.r_liq * np.ones_like(RRTMGobject.Tatm))
    reic = _climlab_to_rrtm(RRTMGobject.r_ice * np.ones_like(RRTMGobject.Tatm))

    return (ncol, nlay, play, plev, tlay, tlev, tsfc,
            h2ovmr, o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr,
            cfc12vmr, cfc12vmr, cfc22vmr, ccl4vmr,
            cldfrac, ciwp, clwp, relq, reic)



def interface_temperature(Ts, Tatm, **kwargs):
    '''Compute temperature at model layer interfaces.'''
    #  Actually it's not clear to me how the RRTM code uses these values
    lev = Tatm.domain.axes['lev'].points
    lev_bounds = Tatm.domain.axes['lev'].bounds
    #  Interpolate to layer interfaces
    f = interp1d(lev, Tatm, axis=-1)  # interpolation function
    Tinterp = f(lev_bounds[1:-1])
    #  add TOA value, Assume surface temperature at bottom boundary
    Ttoa = Tatm[...,0]
    Tinterp = np.concatenate((Ttoa[..., np.newaxis], Tinterp, Ts), axis=-1)
    return Tinterp

def _climlab_to_rrtm(field):
    '''Prepare field with proper dimension order.
    RRTM code expects arrays with (ncol, nlay)
    and with pressure decreasing from surface at element 0

    climlab grid dimensions are any of:
        - (num_lev,) --> (1, num_lev)
        - (num_lat, num_lev)  --> (num_lat, num_lev)
        - (num_lat, num_lon, num_lev)  -->  (num_lat*num_lon, num_lev)

        But lat-lon grids not yet supported here!

    Case single column
    '''
    # Make this work just with 1D (KM,) arrays
    #  (KM,)  -->  (1, nlay)
    try:
        #  Flip along the last axis to reverse the pressure order
        field = field[..., ::-1]
    except:
        if np.isscalar(field):
            return field
        else:
            raise ValueError('field must be array_like or scalar.')
    shape = field.shape
    if len(shape)==1:  #  (num_lev)
        #  Need to append an extra dimension for singleton horizontal ncol
        return field[np.newaxis, ...]
    elif len(shape)==2:  # (num_lat, num_lev)
        return field
    elif len(shape) > 2:
        raise ValueError('lat-lon grids not yet supported here.')
    #elif len(shape)==3:  # (num_lat, num_lon, num_lev)
        #  Need to reshape this array

def _rrtm_to_climlab(field):
    try:
        #  Flip along the last axis to reverse the pressure order
        field = field[..., ::-1]
    except:
        if np.isscalar(field):
            return field
        else:
            raise ValueError('field must be array_like or scalar.')
    return np.squeeze(field)

def _climlab_to_rrtm_sfc(field, Ts):
    '''Return an array of size np.squeeze(Ts) to remove the singleton depth dimension'''
    fieldsqueeze = np.squeeze(field)
    Tsqueeze = np.squeeze(Ts)
    return fieldsqueeze * np.ones_like(Tsqueeze)
