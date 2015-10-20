"""
Routines for calculating heat capacities for grid boxes
in units of J / m**2 / K
"""
from climlab import constants as const


def atmosphere(dp):
    '''Heat capacity of a unit area of atmosphere, in units of J / m**2 / K
    Input is pressure intervals in units of mb.'''
    return const.cp * dp * const.mb_to_Pa / const.g


def ocean(dz):
    '''Heat capacity of a unit area of water, in units of J / m**2 / K
    Input dz is water depth intervals in meters'''
    return const.rho_w * const.cw * dz


def slab_ocean(water_depth):
    '''Heat capacity of a unit area slab of water, in units of J / m**2 / K
    Input is depth of water in meters.'''
    return ocean(water_depth)
