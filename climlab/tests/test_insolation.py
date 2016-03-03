import numpy as np
from climlab import constants as const
from climlab.solar.insolation import daily_insolation
from climlab.solar.orbital import OrbitalTable

# mostly copied from Insolation notebook
def test_daily_insolation():
    lat = np.linspace( -90., 90., 500. )
    days = np.linspace(0, const.days_per_year, 365. )
    Q = daily_insolation(lat, days)

    # check the range of Q
    np.testing.assert_almost_equal(Q.max(), 562.0333475)
    np.testing.assert_almost_equal(Q.min(), 0.0)

    # check the area integral
    Q_area_int = (np.sum(np.mean(Q, axis=1) * np.cos(np.deg2rad(lat))) /
                  np.sum(np.cos(np.deg2rad(lat))) )
    np.testing.assert_almost_equal(Q_area_int, 341.384184481)

def test_orbital_parameters():
    kyears = np.arange( -1000., 1.)
    table = OrbitalTable()
    orb = table.lookup_parameters(kyears)

    # check that orb has the right dictionary keys
    # key: (min, max)
    orb_expected = {'ecc': (0.004, 0.057),
                    'long_peri': (2.3, 360),
                    'obliquity': (22, 24.5) }

    for k in orb_expected:
        orb[k].min() > orb_expected[k][0]
        orb[k].max() > orb_expected[k][0]
