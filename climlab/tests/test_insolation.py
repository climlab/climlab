import numpy as np
from climlab import constants as const
from climlab.solar.insolation import daily_insolation, daily_insolation_factors, instant_insolation, solar_longitude
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
def test_solar_longitude():
    lat = np.linspace(-90., 90., 500)
    days = np.linspace(0, const.days_per_year, 365)
    Q1 = daily_insolation(lat, days, day_type=1)
    Q2 = daily_insolation(lat, solar_longitude(days), day_type=2)
    np.testing.assert_allclose(Q1, Q2)

@pytest.mark.fast    
def test_instant_insolation():
    lats = np.linspace(-90., 90., 181)
    days = np.arange(0.5, 1.5, 0.001)
    Q    = daily_insolation(lats, 1.0)
    Qs   = instant_insolation(lats, days)
    # small error tolerance (in W/m2)
    error = np.abs(Qs.mean(dim='day') - Q)
    assert np.all(error < 0.01)

@pytest.mark.fast
def test_coszen_averages():
    lat = np.linspace(-90., 90., 500)
    days = np.linspace(0, const.days_per_year, 365)
    coszen_insolation, irradiance_factor_insolation = daily_insolation_factors(lat, days, weighting='insolation')
    coszen_sunlit, irradiance_factor_sunlit = daily_insolation_factors(lat, days, weighting='sunlit')
    Q = daily_insolation(lat, days)
    S0 = const.S0
    np.testing.assert_allclose(Q, S0*coszen_insolation*irradiance_factor_insolation)
    np.testing.assert_allclose(Q, S0*coszen_sunlit*irradiance_factor_sunlit)

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
