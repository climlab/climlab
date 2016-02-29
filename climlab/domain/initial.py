"""Convenience routines for setting up initial conditions."""
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
    """Sets up a state variable dictionary consisting of temperatures
    for atmospheric column (``Tatm``) and surface mixed layer (``Ts``).
    
    Surface temperature is always 288 K. Atmospheric temperature is initialized 
    between 278 K at lowest altitude and 200 at top of atmosphere according to
    the number of levels given.
    
    **Function-call arguments** \n        
        
    :param int num_lev:         number of pressure levels
                                (evenly spaced from surface to TOA) 
                                [default: 30]
    :param int num_lat:         number of latitude points on the axis
                                [default: 1]
    :param lev:                 specification for height axis (optional)
    :type lev:                  :class:`~climlab.domain.axis.Axis` or pressure array
    :param array lat:           size of array determines dimension of latitude
    :param float water_depth:   *irrelevant*
    
    :returns:                   dictionary with two temperature 
                                :class:`~climlab.domain.field.Field` 
                                for atmospheric column ``Tatm`` and 
                                surface mixed layer ``Ts``
    :rtype:                     dict
    
    """
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
    """Sets up a state variable dictionary for a zonal-mean surface model
    (e.g. basic EBM). 
    
    Returns a single state variable `Ts`, the temperature of the surface 
    mixed layer, initialized by a basic temperature and the second Legendre
    polynomial.
    
    **Function-call arguments** \n        
        
    :param int num_lat:         number of latitude points on the axis
                                [default: 90]
    :param float water_depth:   *irrelevant*
    :param float T0:            base value for initial temperature          \n
                                - unit :math:`^{\circ} \\textrm{C}`         \n
                                - default value: ``12``
    :param float T2:            factor for 2nd Legendre polynomial 
                                :class:`~climlab.utils.legendre.P2` 
                                to calculate initial temperature            \n
                                - unit: dimensionless
                                - default value: ``40``

    :returns:                   dictionary with temperature 
                                :class:`~climlab.domain.field.Field` 
                                for surface mixed layer ``Ts``
    :rtype:                     dict
    
    """
    sfc = domain.zonal_mean_surface(num_lat=num_lat,
                                    water_depth=water_depth)
    sinphi = np.sin(np.deg2rad(sfc.axes['lat'].points))
    initial = T0 + T2 * legendre.P2(sinphi)
    Ts = Field(initial, domain=sfc)
    state = AttrDict()
    state['Ts'] = Ts
    return state
