import numpy as np
import pytest


@pytest.mark.long_orbital
def test_long_orbital_parameters():
    from climlab.solar.orbital.long import OrbitalTable as LongOrbitalTable
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

