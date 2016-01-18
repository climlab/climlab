'''Convenience routines for setting up initial conditions.'''
import numpy as np
from climlab.domain import domain
from climlab.domain.field import Field


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
    state = {'Ts': Ts, 'Tatm': Tatm}
    return state
