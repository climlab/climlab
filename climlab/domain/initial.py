"""Convenience routines for setting up initial conditions."""
from __future__ import division
import numpy as np
from climlab.domain import domain
from climlab.domain.field import Field
from attrdict import AttrDict
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
                                (evenly spaced from surface to top of atmosphere)
                                [default: 30]
    :param int num_lat:         number of latitude points on the axis
                                [default: 1]
    :param lev:                 specification for height axis (optional)
    :type lev:                  :class:`~climlab.domain.axis.Axis`
                                or pressure array
    :param array lat:           size of array determines dimension of latitude
                                (optional)
    :param float water_depth:   *irrelevant*

    :returns:                   dictionary with two temperature
                                :class:`~climlab.domain.field.Field`
                                for atmospheric column ``Tatm`` and
                                surface mixed layer ``Ts``
    :rtype:                     dict

    :Example:

        ::

            >>> from climlab.domain import initial
            >>> T_dict = initial.column_state()

            >>> print T_dict
            {'Tatm': Field([ 200.        ,  202.68965517,  205.37931034,  208.06896552,
                    210.75862069,  213.44827586,  216.13793103,  218.82758621,
                    221.51724138,  224.20689655,  226.89655172,  229.5862069 ,
                    232.27586207,  234.96551724,  237.65517241,  240.34482759,
                    243.03448276,  245.72413793,  248.4137931 ,  251.10344828,
                    253.79310345,  256.48275862,  259.17241379,  261.86206897,
                    264.55172414,  267.24137931,  269.93103448,  272.62068966,
                    275.31034483,  278.        ]), 'Ts': Field([ 288.])}

    """
    if lat is not None:
        num_lat = np.array(lat).size
    if lev is not None:
        num_lev = np.array(lev).size

    if num_lat == 1:
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
                  num_lon=None,
                  water_depth=10.,
                  T0=12.,
                  T2=-40.):
    """Sets up a state variable dictionary for a surface model
    (e.g. :class:`~climlab.model.ebm.EBM`) with a uniform slab ocean depth.

    The domain is either 1D (latitude) or 2D (latitude, longitude)
    depending on whether the input argument num_lon is supplied.

    Returns a single state variable `Ts`, the temperature of the surface
    mixed layer (slab ocean).

    The temperature is initialized to a smooth equator-to-pole shape given by

    .. math::

        T(\phi) = T_0 + T_2 P_2(\sin\phi)

    where :math:`\phi` is latitude, and :math:`P_2` is the second Legendre
    polynomial :class:`~climlab.utils.legendre.P2`.

    **Function-call arguments** \n

    :param int num_lat:         number of latitude points [default: 90]
    :param int num_lat:         (optional) number of longitude points [default: None]
    :param float water_depth:   depth of the slab ocean in meters [default: 10.]
    :param float T0:            global-mean initial temperature in :math:`^{\circ} \\textrm{C}` [default: 12.]
    :param float T2:            2nd Legendre coefficient for equator-to-pole gradient in
                                initial temperature, in :math:`^{\circ} \\textrm{C}` [default: -40.]

    :returns:                   dictionary with temperature
                                :class:`~climlab.domain.field.Field`
                                for surface mixed layer ``Ts``
    :rtype:                     dict


    :Example:

        ::

            >>> from climlab.domain import initial
            >>> import numpy as np

            >>> T_dict = initial.surface_state(num_lat=36)

            >>> print np.squeeze(T_dict['Ts'])
            [-27.88584094 -26.97777479 -25.18923361 -22.57456133 -19.21320344
             -15.20729309 -10.67854785  -5.76457135  -0.61467228   4.61467228
               9.76457135  14.67854785  19.20729309  23.21320344  26.57456133
              29.18923361  30.97777479  31.88584094  31.88584094  30.97777479
              29.18923361  26.57456133  23.21320344  19.20729309  14.67854785
               9.76457135   4.61467228  -0.61467228  -5.76457135 -10.67854785
             -15.20729309 -19.21320344 -22.57456133 -25.18923361 -26.97777479
             -27.88584094]

    """
    if num_lon is None:
        sfc = domain.zonal_mean_surface(num_lat=num_lat,
                                        water_depth=water_depth)
    else:
        sfc = domain.surface_2D(num_lat=num_lat,
                                num_lon=num_lon,
                                water_depth=water_depth)
    if 'lon' in sfc.axes:
        lon, lat = np.meshgrid(sfc.axes['lon'].points, sfc.axes['lat'].points)
    else:
        lat = sfc.axes['lat'].points
    sinphi = np.sin(np.deg2rad(lat))
    initial = T0 + T2 * legendre.P2(sinphi)
    Ts = Field(initial, domain=sfc)
    #if num_lon is None:
    #    Ts = Field(initial, domain=sfc)
    #else:
    #    Ts = Field([[initial for k in range(num_lon)]], domain=sfc)
    state = AttrDict()
    state['Ts'] = Ts
    return state
