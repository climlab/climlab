'''Convenience routines for setting up initial conditions.'''
import numpy as np
from climlab.domain import domain
from climlab.domain.field import Field
from climlab.utils.attr_dict import AttrDict
from climlab.utils import legendre


def column_state(num_lev=30,
                 num_lat=1,
                 lev=None,
                 lat=None,
                 water_depth=1.0):
    '''Set up a state variable dictionary consisting of temperatures
    for atmospheric column (`Tatm`) and surface mixed layer (`Ts`).
    '''
    if lat is not None:
        num_lat = np.array(lat).size
    if lev is not None:
        num_lev = np.array(lev).size

    if num_lat is 1:
        sfc, atm = domain.single_column(water_depth=water_depth,
                                        num_lev=num_lev,
                                        lev=lev)
    else:
        sfc, atm = domain.zonal_mean_column(water_depth=water_depth,
                                            num_lev=num_lev,
                                            lev=lev,
                                            num_lat=num_lat,
                                            lat=lat)
    num_lev = atm.lev.num_points
    Ts = Field(288.*np.ones(sfc.shape), domain=sfc)
    Tinitial = np.tile(np.linspace(200., 288.-10., num_lev), sfc.shape)
    Tatm = Field(Tinitial, domain=atm)
    state = AttrDict()
    state['Ts'] = Ts
    state['Tatm'] = Tatm
    return state


def surface_state(num_lat=90,
                  water_depth=10.,
                  T0=12.,
                  T2=-40.):
    '''Set up a state variable dictionary for a zonal-mean surface model
    (e.g. basic EBM). There is a single state variable `Ts`, the temperature
    of the surface mixed layer.
    '''
    sfc = domain.zonal_mean_surface(num_lat=num_lat,
                                    water_depth=water_depth)
    sinphi = np.sin(np.deg2rad(sfc.axes['lat'].points))
    initial = T0 + T2 * legendre.P2(sinphi)
    Ts = Field(initial, domain=sfc)
    state = AttrDict()
    state['Ts'] = Ts
    return state
