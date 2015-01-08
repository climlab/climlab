# -*- coding: utf-8 -*-
"""
Created on Thu Jan  8 17:11:15 2015

@author: Brian
"""
import constants as const


def slab_ocean(water_depth):
    return const.rho_w * const.cw * water_depth


def atmosphere(grid):
    return const.cp * grid.dp * const.mb_to_Pa / const.g
