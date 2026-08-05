r"""Unit tests for the 2D advection-diffusion numerical routines.

Tests the pure numerical functions in two_d_adv_diff_numerics module.
"""
import numpy as np
import pytest
from climlab.dynamics import two_d_adv_diff_numerics as numerics


class TestExtendToBoundaries:
    """Tests for the extend_to_boundaries function."""

    def test_1d_array(self):
        """Test extending a 1D array."""
        arr = np.array([1.0, 2.0, 3.0, 4.0])
        extended = numerics.extend_to_boundaries(arr)

        assert extended.shape == (6,)
        assert extended[0] == arr[0]  # First value repeated
        assert extended[-1] == arr[-1]  # Last value repeated
        np.testing.assert_array_equal(extended[1:-1], arr)

    def test_2d_array_last_axis(self):
        """Test extending a 2D array along last axis."""
        arr = np.array([[1.0, 2.0, 3.0],
                        [4.0, 5.0, 6.0]])
        extended = numerics.extend_to_boundaries(arr, axis=-1)

        assert extended.shape == (2, 5)
        np.testing.assert_array_equal(extended[:, 0], arr[:, 0])
        np.testing.assert_array_equal(extended[:, -1], arr[:, -1])

    def test_2d_array_first_axis(self):
        """Test extending a 2D array along first axis."""
        arr = np.array([[1.0, 2.0, 3.0],
                        [4.0, 5.0, 6.0]])
        extended = numerics.extend_to_boundaries(arr, axis=0)

        assert extended.shape == (4, 3)
        np.testing.assert_array_equal(extended[0, :], arr[0, :])
        np.testing.assert_array_equal(extended[-1, :], arr[-1, :])


class TestCalcIntegral:
    """Tests for the calc_integral function."""

    def test_uniform_spacing(self):
        """Test integral with uniform spacing."""
        tracer = np.array([1.0, 2.0, 3.0, 4.0])
        dbounds = np.ones(4) * 2.0  # uniform spacing of 2

        integral = numerics.calc_integral(tracer, dbounds)

        assert integral.shape == (5,)
        assert integral[0] == 0.0  # First element is always 0
        assert integral[-1] == pytest.approx(2.0 * (1 + 2 + 3 + 4))
        # Cumulative sum check
        assert integral[1] == pytest.approx(2.0)
        assert integral[2] == pytest.approx(6.0)
        assert integral[3] == pytest.approx(12.0)

    def test_nonuniform_spacing(self):
        """Test integral with non-uniform spacing."""
        tracer = np.array([1.0, 1.0, 1.0])
        dbounds = np.array([1.0, 2.0, 3.0])

        integral = numerics.calc_integral(tracer, dbounds)

        assert integral[0] == 0.0
        assert integral[-1] == pytest.approx(6.0)  # 1+2+3

    def test_2d_tracer(self):
        """Test integral with 2D tracer (operates on last axis)."""
        tracer = np.array([[1.0, 2.0],
                           [3.0, 4.0]])
        dbounds = np.ones(2)

        integral = numerics.calc_integral(tracer, dbounds)

        assert integral.shape == (2, 3)
        assert integral[0, 0] == 0.0
        assert integral[1, 0] == 0.0
        assert integral[0, -1] == pytest.approx(3.0)  # 1+2
        assert integral[1, -1] == pytest.approx(7.0)  # 3+4


class TestLinearInterp1d:
    """Tests for the linear_interp_1d function."""

    def test_linear_data(self):
        """Linear interpolation should be exact for linear data."""
        xvec = np.array([0.0, 1.0, 2.0, 3.0])
        vec = np.array([0.0, 2.0, 4.0, 6.0])  # y = 2x
        location = np.array([0.5, 1.5, 2.5])

        result = numerics.linear_interp_1d(xvec, vec, location)

        expected = np.array([1.0, 3.0, 5.0])  # 2 * location
        np.testing.assert_allclose(result, expected)

    def test_quadratic_data(self):
        """Linear interpolation of quadratic data."""
        xvec = np.array([0.0, 1.0, 2.0])
        vec = np.array([0.0, 1.0, 4.0])  # y = x^2
        location = np.array([0.5, 1.5])

        result = numerics.linear_interp_1d(xvec, vec, location)

        # Linear interp at 0.5: 0 + (1-0) * 0.5 = 0.5
        # Linear interp at 1.5: 1 + (4-1) * 0.5 = 2.5
        expected = np.array([0.5, 2.5])
        np.testing.assert_allclose(result, expected)


class TestComputeAdvectiveFlux:
    """Tests for the compute_advective_flux function."""

    def test_positive_flux(self):
        """Test positive flux calculation."""
        integral = np.array([0.0, 1.0, 3.0, 6.0])
        istar = np.array([0.0, 0.5, 2.0, 4.5])
        dt = 1.0

        flux = numerics.compute_advective_flux(integral, istar, dt)

        expected = (integral - istar) / dt
        np.testing.assert_allclose(flux, expected)

    def test_with_sign(self):
        """Test flux with negative sign (for pressure coordinates)."""
        integral = np.array([0.0, 1.0, 3.0])
        istar = np.array([0.0, 0.5, 2.0])
        dt = 2.0
        sign = -1.0

        flux = numerics.compute_advective_flux(integral, istar, dt, sign=sign)

        expected = -1.0 * (integral - istar) / dt
        np.testing.assert_allclose(flux, expected)


class TestApplyFluxConvergence:
    """Tests for the apply_flux_convergence function."""

    def test_uniform_flux(self):
        """Uniform flux should give zero convergence."""
        flux = np.array([1.0, 1.0, 1.0, 1.0])
        dbounds = np.ones(3)
        dt = 1.0

        update = numerics.apply_flux_convergence(flux, dbounds, dt)

        np.testing.assert_allclose(update, np.zeros(3))

    def test_diverging_flux(self):
        """Diverging flux should decrease tracer."""
        flux = np.array([0.0, 1.0, 2.0, 3.0])  # Increasing flux
        dbounds = np.ones(3)
        dt = 1.0

        update = numerics.apply_flux_convergence(flux, dbounds, dt)

        # Convergence = (flux_in - flux_out) / dx * dt
        # Cell 0: (0 - 1) / 1 * 1 = -1
        # Cell 1: (1 - 2) / 1 * 1 = -1
        # Cell 2: (2 - 3) / 1 * 1 = -1
        expected = np.array([-1.0, -1.0, -1.0])
        np.testing.assert_allclose(update, expected)


class TestComputeDiffusiveFlux:
    """Tests for the compute_diffusive_flux function."""

    def test_constant_tracer(self):
        """Constant tracer should give zero diffusive flux."""
        tracer_ext = np.array([1.0, 1.0, 1.0, 1.0])  # Extended tracer
        dbounds_ext = np.ones(4)
        diffusivity = np.ones(3) * 10.0

        flux = numerics.compute_diffusive_flux(tracer_ext, dbounds_ext, diffusivity)

        np.testing.assert_allclose(flux, np.zeros(3))

    def test_linear_gradient(self):
        """Linear gradient should give constant diffusive flux."""
        tracer_ext = np.array([1.0, 1.0, 2.0, 3.0, 3.0])  # Extended tracer
        dbounds_ext = np.ones(5)  # Uniform spacing
        diffusivity = np.ones(4) * 2.0

        flux = numerics.compute_diffusive_flux(tracer_ext, dbounds_ext, diffusivity)

        # Gradient at each interface is 1/1 = 1
        # Flux = -K * gradient = -2 * 1 = -2
        # (except at boundaries where gradient is 0)
        assert flux[0] == pytest.approx(0.0)
        assert flux[-1] == pytest.approx(0.0)
        assert flux[1] == pytest.approx(-2.0)
        assert flux[2] == pytest.approx(-2.0)


class TestParabolicInterp:
    """Tests for the parabolic_interp_istar function."""

    def test_zero_velocity(self):
        """With zero velocity, I* should equal I."""
        integral = np.array([0.0, 1.0, 3.0, 6.0])
        bounds = np.array([0.0, 1.0, 2.0, 3.0])
        tracer_ext = np.array([1.0, 1.0, 2.0, 3.0, 3.0])
        dbounds_ext = np.ones(5)
        velocity = np.zeros(4)
        dt = 1.0

        istar = numerics.parabolic_interp_istar(
            bounds, dbounds_ext, integral, tracer_ext, velocity, dt
        )

        np.testing.assert_allclose(istar, integral)


class TestLinearInterpIstar:
    """Tests for the linear_interp_istar function."""

    def test_zero_velocity(self):
        """With zero velocity, I* should equal I."""
        bounds = np.array([0.0, 1.0, 2.0, 3.0])
        integral = np.array([0.0, 1.0, 3.0, 6.0])
        velocity = np.zeros(4)
        dt = 1.0

        istar = numerics.linear_interp_istar(bounds, integral, velocity, dt)

        np.testing.assert_allclose(istar, integral)


# Run tests when executed directly
if __name__ == '__main__':
    pytest.main([__file__, '-v'])
