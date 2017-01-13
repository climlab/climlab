"""Routines for calculating heat capacities for grid boxes."""

from __future__ import division
from climlab import constants as const


def atmosphere(dp):
    """Returns heat capacity of a unit area of atmosphere, in units J /m**2 / K.

    .. math::

        C_a = \\frac{c_p \\cdot dp \\cdot f_{\\textrm{mb-to-Pa}}}{g}

    where

    ==============================  ==============  ==================================================  ===============================================
    variable                        value           unit                                                description
    ==============================  ==============  ==================================================  ===============================================
    :math:`C_a`                     *output*        :math:`\\textrm{J} / \\textrm{m}^2 / \\textrm{K}`      heat capacity for atmospheric cell
    :math:`c_p`                     :math:`1004.`   :math:`\\textrm{J} / \\textrm{kg} / \\textrm{K}`       specific heat at constant pressure for dry air
    :math:`dp`                      *input*         :math:`\\textrm{mb}`                                 pressure for atmospheric cell
    :math:`f_{\\textrm{mb-to-Pa}}`   :math:`100`     :math:`\\textrm{Pa} / \\textrm{mb}`                   conversion factor from mb to Pa
    :math:`g`                       :math:`9.8`     :math:`\\textrm{m} / \\textrm{s}^2`                   gravitational acceleration
    ==============================  ==============  ==================================================  ===============================================

    **Function-call argument** \n

    :param array dp:    pressure intervals (*unit:* mb)
    :returns:           the heat capacity for atmosphere cells correspoding to
                        pressure input (*unit:* J /m**2 / K)
    :rtype:             array

    :Example:

        Calculate atmospheric heat capacity for pressure intervals of
        1, 10, 100 mb::

            >>> from climlab.utils import heat_capacity

            >>> pressure_interval = array([1,10,100]) # in mb
            >>> heat_capacity.atmosphere(pressure_interval) # in J /m**2 / K
            array([   10244.89795918,   102448.97959184,  1024489.79591837])

    """
    return const.cp * dp * const.mb_to_Pa / const.g


def ocean(dz):
    """Returns heat capacity of a unit area of water, in units J /m**2 / K.

    .. math::

        C_o = \\rho_w \\cdot c_w \\cdot dz

    where

    ==============================  ==============  ==================================================  ==================================
    variable                        value           unit                                                description
    ==============================  ==============  ==================================================  ==================================
    :math:`C_o`                     *output*        :math:`\\textrm{J} / \\textrm{m}^2 / \\textrm{K}`      heat capacity for oceanic cell
    :math:`c_w`                     :math:`4181.3`  :math:`\\textrm{J} / \\textrm{kg} / \\textrm{K}`       specific heat of liquid water
    :math:`dz`                      *input*         :math:`\\textrm{m}`                                  water depth of oceanic cell
    :math:`\\rho_w`                  :math:`1000.`   :math:`\\textrm{kg} / \\textrm{m}^3`                   density of water
    ==============================  ==============  ==================================================  ==================================

    **Function-call argument** \n

    :param array dz:    water depth of ocean cells (*unit:* m)
    :returns:           the heat capacity for ocean cells correspoding to
                        depth input (*unit:* J /m**2 / K)
    :rtype:             array

    :Example:

        Calculate atmospheric heat capacity for pressure intervals of
        1, 10, 100 m::

            >>> from climlab.utils import heat_capacity

            >>> pressure_interval = array([1,10,100]) # in m
            >>> heat_capacity.ocean(pressure_interval) # in J /m**2 / K
            array([  4.18130000e+06,   4.18130000e+07,   4.18130000e+08])

    """
    return const.rho_w * const.cw * dz


def slab_ocean(water_depth):
    """Returns heat capacity of a unit area slab of water, in units of J / m**2 / K.

    Takes input argument ``water_depth`` and calls :func:`ocean()`

    **Function-call argument** \n

    :param float:   water depth of slab ocean (*unit:* m)
    :returns:       the heat capacity for slab ocean cell (*unit:* J / m**2 / K)
    :rtype:         float

    """
    return ocean(water_depth)
