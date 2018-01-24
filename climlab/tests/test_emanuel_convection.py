from __future__ import division
import numpy as np
import climlab
import pytest

num_lev = 20

@pytest.mark.fast
def test_convect_tendencies():
    #  These test data are based on direct single-column tests of the CONVECT43c.f
    #  fortran source code. We are just checking to see if we get the right tendencies

    #  INPUT DATA
    T = np.flipud([278.0, 273.9, 269.8, 265.7, 261.6, 257.5, 253.4, 249.3, 245.2,
        241.1, 236.9, 232.8, 228.7, 224.6, 220.5, 216.4, 212.3, 214.0, 240., 270.])
    Q = np.flipud([3.768E-03, 2.812E-03, 2.078E-03, 1.519E-03, 1.099E-03,
                7.851E-04, 5.542E-04, 3.860E-04, 2.652E-04, 1.794E-04,
                1.183E-04, 7.739E-05, 4.970E-05, 3.127E-05, 1.923E-05,
                1.152E-05, 6.675E-06, 5.000E-06, 5.000E-06, 5.000E-06])
    U = np.flipud([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0,
                    12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0])
    V = 5. * np.ones_like(U)
    DELT = 60.0*10.

    #  TENDENCIES FROM FORTRAN CODE
    FT = np.flipud([ -1.79028630E-05, -5.30782881E-06, -1.31800425E-05,
            -1.52185385E-06, 2.39886958E-05, 5.00537462E-05, 5.81255517E-05,
            3.53323558E-05, 2.92723980E-05, 1.72980563E-05,  -1.29287255E-05,
            -1.95621360E-05, 0.00000000,  0.00000000,  0.00000000, 0.00000000,
            0.00000000,  0.00000000,   0.00000000,   0.00000000])
    FQ = np.flipud([-1.25298214E-07,  -1.77202448E-08,   2.25614087E-08,
        1.20568053E-08,  -2.25126184E-09,  -8.66014460E-09,   1.32199425E-08,
        3.49015110E-08,   4.61532412E-09,   3.59331809E-09,   3.54343976E-09,
        1.12605347E-09,   0.00000000,       0.00000000,       0.00000000,
         0.00000000,  0.00000000,  0.00000000 ,   0.00000000,   0.00000000])
    FU = np.flipud([6.96283823E-05,   2.54282313E-05,  -4.23648498E-06,
      -2.25508029E-06,   5.98046836E-06,   1.29878290E-05,  -7.08503649E-06,
      -5.06099132E-05,  -8.67537074E-06,  -1.08638551E-05,  -1.97463705E-05,
      -1.05528088E-05,   0.00000000,       0.00000000,       0.00000000,
      0.00000000,       0.00000000,       0.00000000,       0.00000000,       0.00000000])
    FV = np.zeros_like(FU)

    # Temperatures in a single column
    state = climlab.column_state(num_lev=num_lev)
    state['Tatm'][:] = T
    state['q'] = state.Tatm * 0. + Q
    state['U'] = state.Tatm * 0. + U
    state['V'] = state.Tatm * 0. + V
    conv = climlab.convection.EmanuelConvection(state=state, timestep=DELT)
    conv.step_forward()
    #  Did we get all the correct output?
    assert conv.IFLAG == 1
    #  relative tolerance for these tests ...
    tol = 1E-3
    assert conv.CBMF == pytest.approx(3.10438257E-02, rel=tol)
    tend = conv.tendencies
    assert FT == pytest.approx(tend['Tatm'], rel=tol)
    assert FQ == pytest.approx(tend['q'], rel=tol)
    assert FU == pytest.approx(tend['U'], rel=tol)
    assert FV == pytest.approx(tend['V'], rel=tol)
