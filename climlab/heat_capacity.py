"""
Routines for calculating heat capacities for grid boxes
in units of J / m**2 / K
"""
import constants


def slab_ocean(water_depth):
    '''Heat capacity of a unit area slab of water, in units of J / m**2 / K
    Input is depth of water in meters.'''
    return constants.rho_w * constants.cw * water_depth


def atmosphere(dp):
    '''Heat capacity of a unit area of atmosphere, in units of J / m**2 / K
    Input is pressure intervals in units of mb.'''
    return constants.cp * dp * constants.mb_to_Pa / constants.g
