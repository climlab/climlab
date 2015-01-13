# -*- coding: utf-8 -*-
"""
Created on Tue Jan 13 16:45:24 2015

@author: Brian
"""

def _make_meridional_diffusion_matrix(K, grid):
    lataxis = grid['lat']
    phi_stag = np.deg2rad(lataxis.bounds)
    phi = np.deg2rad(lataxis.points)
    weight1 = np.cos(phi_stag)
    weight2 = np.cos(phi)
    diag = _make_diffusion_matrix(K, weight1, weight2)
    return diag

