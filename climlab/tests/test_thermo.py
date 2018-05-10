from __future__ import division
import numpy as np
import climlab
from climlab.utils import thermo
import pytest


@pytest.mark.fast
def test_thermo():
    '''Basic single value tests for the thermodynamic routines.'''
    assert np.isclose(thermo.potential_temperature(250., 500.), 304.783)
    assert np.isclose(thermo.theta(250., 500.), 304.783)

    assert np.isclose(thermo.temperature_from_potential(300., 500.), 246.076)
    assert np.isclose(thermo.T(300., 500.), 246.076)

    assert np.isclose(thermo.clausius_clapeyron(300.), 35.345)

    assert np.isclose(thermo.qsat(300., 1000.), 0.02227839)

    assert np.isclose(thermo.estimated_inversion_strength(300., 290.), 5.3605345)
    assert np.isclose(thermo.EIS(300., 290.), 5.3605345)

    assert np.isclose(thermo.blackbody_emission(300.), 459.3)

@pytest.mark.fast
def test_thermo_domain():
    '''Can we call qsat, etc on a multi-dim climlab state temperature object?'''
    state = climlab.column_state(num_lev = 30, num_lat=3)
    T = state.Tatm
    p = T.domain.lev.points
    thermo.clausius_clapeyron(T)
    thermo.qsat(T, p)
    thermo.pseudoadiabat(T, p)
    thermo.blackbody_emission(T)
