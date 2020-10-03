from __future__ import division, print_function, absolute_import
import numpy as np
from climlab import constants as const
from climlab.solar.insolation import daily_insolation
from climlab.solar.orbital import OrbitalTable
from climlab.solar.orbital.long import OrbitalTable as LongOrbitalTable
from climlab import EBM_seasonal
from climlab.solar.orbital_cycles import OrbitalCycles
from climlab.surface import StepFunctionAlbedo
import pytest


@pytest.mark.fast
def test_daily_insolation():
    lat = np.linspace(-90., 90., 500)
    days = np.linspace(0, const.days_per_year, 365)
    Q = daily_insolation(lat, days)

    # check the range of Q
    np.testing.assert_almost_equal(Q.max(), 562.0333475)
    np.testing.assert_almost_equal(Q.min(), 0.0)

    # check the area integral
    Q_area_int = (np.sum(np.mean(Q, axis=1) * np.cos(np.deg2rad(lat))) /
                  np.sum(np.cos(np.deg2rad(lat))) )
    np.testing.assert_almost_equal(Q_area_int, 341.384184481)

@pytest.mark.fast
def test_orbital_parameters():
    kyears = np.arange( -1000., 1.)
    orb = OrbitalTable.interp(kyear=kyears)

    # check that orb has the right dictionary keys
    # key: (min, max)
    orb_expected = {'ecc': (0.004, 0.057),
                    #'long_peri': (2.3, 360),
                    'obliquity': (22, 24.5) }

    for k in orb_expected:
        assert orb[k].min() > orb_expected[k][0]
        assert orb[k].max() < orb_expected[k][1]

@pytest.mark.fast
def test_orbital_interpolation():
    # check to see if we get smooth results when we interpolate
    # orbital parameters at high temporal frequency
    kyears = np.linspace(-11, 0, 1001)
    orb = OrbitalTable.interp(kyear=kyears)
    S30 = daily_insolation(lat=30, day=172, orb=orb)
    # there should be no abrupt changes
    # test if insolation varies by more than 0.1 W/m2 per year
    assert S30.diff(dim='kyear').max() < 0.1


@pytest.mark.slow
def test_long_orbital_parameters():
    kyears = np.arange( -1000., +500.)
    orb = LongOrbitalTable.interp(kyear=kyears)

    # check that orb has the right dictionary keys
    # key: (min, max)
    orb_expected = {'ecc': (0.0018, 0.0579),
                    #'long_peri': (0.182, 360),
                    'obliquity': (22, 24.5) }

    for k in orb_expected:
        assert orb[k].min() > orb_expected[k][0]
        assert orb[k].max() < orb_expected[k][1]

#  Tests of automatic orbital cycles with EBM
@pytest.mark.slow
def test_orbital_cycles():
    ebm = EBM_seasonal()
    #  add an albedo feedback
    alb = StepFunctionAlbedo(state=ebm.state, **ebm.param)
    ebm.add_subprocess('albedo', alb)
    ebm.subprocess['SW'].albedo = alb.albedo
    #  run for 1,000 orbital years, but only 100 model years
    experiment = OrbitalCycles(ebm, kyear_start=-20, kyear_stop=-19,
                               orbital_year_factor=10.)
    assert experiment.orb_kyear == -20.
    np.testing.assert_almost_equal(experiment.T_segments_global, 11.48520525)
