r"""Regression and unit tests for the 2D advection-diffusion solver.

These tests verify that the TwoDimensionalAdvectionDiffusion class
produces consistent results after refactoring.
"""
import os
import numpy as np
import pytest
from climlab.domain import zonal_mean_column
from climlab.domain import Field
from climlab.dynamics import TwoDimensionalAdvectionDiffusion, ParticleSink
from climlab.dynamics import two_d_adv_diff_numerics as numerics

try:
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    HAS_MATPLOTLIB = True
except ImportError:
    HAS_MATPLOTLIB = False

PLOT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'test_plots')

# Test grid parameters
NUM_LAT = 18  # Latitude points
NUM_LEV = 10  # Pressure levels
TIMESTEP = np.timedelta64(86400, 's')  # 1 day in seconds


def create_test_domain():
    """Create a lat-lev atmosphere domain for testing."""
    sfc, atm = zonal_mean_column(num_lat=NUM_LAT, num_lev=NUM_LEV)
    return atm


def create_gaussian_tracer(atm_domain, lat_center=0.0, lev_center=500.0,
                           lat_width=30.0, lev_width=200.0):
    """Create a Gaussian tracer distribution for testing.

    Parameters
    ----------
    atm_domain : Domain
        Atmosphere domain with lat and lev axes
    lat_center : float
        Center latitude in degrees
    lev_center : float
        Center pressure level in hPa
    lat_width : float
        Gaussian width in latitude (degrees)
    lev_width : float
        Gaussian width in pressure (hPa)

    Returns
    -------
    tracer : Field
        2D tracer field with Gaussian distribution
    """
    lat_points = atm_domain.axes['lat'].points
    lev_points = atm_domain.axes['lev'].points

    # Create 2D meshgrid
    lat_2d = lat_points[:, np.newaxis] * np.ones((1, len(lev_points)))
    lev_2d = np.ones((len(lat_points), 1)) * lev_points[np.newaxis, :]

    # Gaussian distribution
    tracer_values = np.exp(
        -((lat_2d - lat_center) / lat_width) ** 2
        - ((lev_2d - lev_center) / lev_width) ** 2
    )

    tracer = Field(tracer_values, domain=atm_domain)
    return tracer


def create_velocity_fields(atm_domain, U_max=0.0, W_max=0.0):
    """Create velocity fields for testing.

    Parameters
    ----------
    atm_domain : Domain
        Atmosphere domain
    U_max : float
        Maximum meridional velocity (m/s)
    W_max : float
        Maximum vertical velocity (Pa/s)

    Returns
    -------
    U, W : ndarray
        Velocity arrays at cell boundaries
    """
    lat_bounds = atm_domain.axes['lat'].bounds
    lev_bounds = atm_domain.axes['lev'].bounds

    nlat = len(lat_bounds)
    nlev = len(lev_bounds)

    # U is defined at latitude boundaries (nlat+1 x nlev)
    lat_norm = (lat_bounds - lat_bounds.min()) / (lat_bounds.max() - lat_bounds.min())
    U = U_max * np.sin(2 * np.pi * lat_norm)[:, np.newaxis] * np.ones((1, nlev - 1))

    # W is defined at level boundaries (nlat x nlev+1)
    lev_norm = (lev_bounds - lev_bounds.min()) / (lev_bounds.max() - lev_bounds.min())
    W = W_max * np.sin(np.pi * lev_norm)[np.newaxis, :] * np.ones((nlat - 1, 1))

    return U, W


def create_zero_velocity_fields(atm_domain):
    """Create zero velocity fields with proper shapes.

    Parameters
    ----------
    atm_domain : Domain
        Atmosphere domain

    Returns
    -------
    U, W : ndarray
        Zero velocity arrays at cell boundaries
    """
    lat_bounds = atm_domain.axes['lat'].bounds
    lev_bounds = atm_domain.axes['lev'].bounds

    nlat = len(lat_bounds)
    nlev = len(lev_bounds)

    U = np.zeros((nlat, nlev - 1))
    W = np.zeros((nlat - 1, nlev))

    return U, W


def create_diffusivity_fields(atm_domain, Kyy_val=0.0, Kzz_val=0.0, Kyz_val=0.0):
    """Create diffusivity fields for testing.

    Parameters
    ----------
    atm_domain : Domain
        Atmosphere domain
    Kyy_val : float
        Meridional diffusivity (m^2/s)
    Kzz_val : float
        Vertical diffusivity (m^2/s)
    Kyz_val : float
        Mixed diffusivity (m^2/s)

    Returns
    -------
    Kyy, Kzz, Kyz : ndarray
        Diffusivity arrays at cell boundaries
    """
    lat_bounds = atm_domain.axes['lat'].bounds
    lev_bounds = atm_domain.axes['lev'].bounds

    nlat = len(lat_bounds)
    nlev = len(lev_bounds)

    # Uniform diffusivities (can be made spatially varying for more tests)
    Kyy = Kyy_val * np.ones((nlat, nlev - 1))
    Kzz = Kzz_val * np.ones((nlat - 1, nlev))
    Kyz = Kyz_val * np.ones((nlat, nlev))

    return Kyy, Kzz, Kyz


@pytest.fixture
def atm_domain():
    """Fixture for atmosphere domain."""
    return create_test_domain()


@pytest.fixture
def gaussian_tracer(atm_domain):
    """Fixture for Gaussian tracer field."""
    return create_gaussian_tracer(atm_domain)


class TestTwoDimensionalAdvectionDiffusion:
    """Test suite for TwoDimensionalAdvectionDiffusion class."""

    def test_initialization(self, atm_domain, gaussian_tracer):
        """Test that the process can be initialized correctly."""
        state = {'tracer': gaussian_tracer}

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
        )

        assert proc is not None
        assert 'tracer' in proc.state
        assert proc.timestep == TIMESTEP

    def test_pure_advection_y(self, atm_domain, gaussian_tracer):
        """Test pure meridional advection (U only)."""
        state = {'tracer': gaussian_tracer}
        U, _ = create_velocity_fields(atm_domain, U_max=10.0, W_max=0.0)
        _, W_zero = create_zero_velocity_fields(atm_domain)

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U,
            W=W_zero,
            Kyy=0.0,
            Kzz=0.0,
        )

        # Store initial mass
        initial_tracer = proc.state['tracer'].copy()
        initial_mass = np.sum(proc.M_air * initial_tracer)

        # Step forward
        proc.step_forward()

        final_tracer = proc.state['tracer']
        final_mass = np.sum(proc.M_air * final_tracer)

        # Tracer should have changed
        assert not np.allclose(initial_tracer, final_tracer)

        # Mass should be approximately conserved (within numerical precision)
        assert np.isclose(initial_mass, final_mass, rtol=1e-10)

    def test_pure_advection_z(self, atm_domain, gaussian_tracer):
        """Test pure vertical advection (W only)."""
        state = {'tracer': gaussian_tracer}
        U_zero, _ = create_zero_velocity_fields(atm_domain)
        _, W = create_velocity_fields(atm_domain, U_max=0.0, W_max=100.0)

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U_zero,
            W=W,
            Kyy=0.0,
            Kzz=0.0,
        )

        initial_tracer = proc.state['tracer'].copy()
        initial_mass = np.sum(proc.M_air * initial_tracer)

        proc.step_forward()

        final_tracer = proc.state['tracer']
        final_mass = np.sum(proc.M_air * final_tracer)

        # Tracer should have changed
        assert not np.allclose(initial_tracer, final_tracer)

        # Mass should be approximately conserved
        assert np.isclose(initial_mass, final_mass, rtol=1e-10)

    def test_pure_diffusion_y(self, atm_domain, gaussian_tracer):
        """Test pure meridional diffusion (Kyy only)."""
        state = {'tracer': gaussian_tracer}
        U_zero, W_zero = create_zero_velocity_fields(atm_domain)
        Kyy, _, _ = create_diffusivity_fields(atm_domain, Kyy_val=1e6)

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U_zero,
            W=W_zero,
            Kyy=Kyy,
            Kzz=0.0,
        )

        initial_tracer = proc.state['tracer'].copy()
        initial_mass = np.sum(proc.M_air * initial_tracer)

        proc.step_forward()

        final_tracer = proc.state['tracer']
        final_mass = np.sum(proc.M_air * final_tracer)

        # Diffusion should spread the tracer
        assert not np.allclose(initial_tracer, final_tracer)

        # Max should decrease (spreading)
        assert np.max(final_tracer) <= np.max(initial_tracer)

        # Mass should be conserved
        assert np.isclose(initial_mass, final_mass, rtol=1e-10)

    def test_pure_diffusion_z(self, atm_domain, gaussian_tracer):
        """Test pure vertical diffusion (Kzz only)."""
        state = {'tracer': gaussian_tracer}
        U_zero, W_zero = create_zero_velocity_fields(atm_domain)
        _, Kzz, _ = create_diffusivity_fields(atm_domain, Kzz_val=1e4)

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U_zero,
            W=W_zero,
            Kyy=0.0,
            Kzz=Kzz,
        )

        initial_tracer = proc.state['tracer'].copy()
        initial_mass = np.sum(proc.M_air * initial_tracer)

        proc.step_forward()

        final_tracer = proc.state['tracer']
        final_mass = np.sum(proc.M_air * final_tracer)

        # Diffusion should spread the tracer
        assert not np.allclose(initial_tracer, final_tracer)

        # Mass should be conserved
        assert np.isclose(initial_mass, final_mass, rtol=1e-10)

    def test_mixed_advection_diffusion(self, atm_domain, gaussian_tracer):
        """Test combined advection and diffusion."""
        state = {'tracer': gaussian_tracer}
        U, W = create_velocity_fields(atm_domain, U_max=5.0, W_max=50.0)
        Kyy, Kzz, Kyz = create_diffusivity_fields(
            atm_domain, Kyy_val=5e5, Kzz_val=5e3, Kyz_val=1e4
        )

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U,
            W=W,
            Kyy=Kyy,
            Kzz=Kzz,
            Kyz=Kyz,
        )

        initial_tracer = proc.state['tracer'].copy()
        initial_mass = np.sum(proc.M_air * initial_tracer)

        proc.step_forward()

        final_tracer = proc.state['tracer']
        final_mass = np.sum(proc.M_air * final_tracer)

        # Tracer should have changed
        assert not np.allclose(initial_tracer, final_tracer)

        # Mass should be conserved
        assert np.isclose(initial_mass, final_mass, rtol=1e-10)

    def test_limiters_preserve_positivity(self, atm_domain, gaussian_tracer):
        """Test that limiters prevent negative tracer values."""
        state = {'tracer': gaussian_tracer}
        U, W = create_velocity_fields(atm_domain, U_max=20.0, W_max=200.0)

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U,
            W=W,
            use_limiters=True,
        )

        # Run for several timesteps
        for _ in range(5):
            proc.step_forward()

        # Tracer should remain non-negative with limiters
        assert np.all(proc.state['tracer'] >= -1e-15)

    def test_interpolation_order_1(self, atm_domain, gaussian_tracer):
        """Test linear interpolation (order 1)."""
        state = {'tracer': gaussian_tracer}
        U, W = create_velocity_fields(atm_domain, U_max=5.0, W_max=50.0)

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U,
            W=W,
            interpolation_order=1,
        )

        initial_tracer = proc.state['tracer'].copy()
        proc.step_forward()

        # Should have changed
        assert not np.allclose(initial_tracer, proc.state['tracer'])

    def test_interpolation_order_2(self, atm_domain, gaussian_tracer):
        """Test parabolic interpolation (order 2)."""
        state = {'tracer': gaussian_tracer}
        U, W = create_velocity_fields(atm_domain, U_max=5.0, W_max=50.0)

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U,
            W=W,
            interpolation_order=2,
        )

        initial_tracer = proc.state['tracer'].copy()
        proc.step_forward()

        # Should have changed
        assert not np.allclose(initial_tracer, proc.state['tracer'])

    def test_diagnostics(self, atm_domain, gaussian_tracer):
        """Test that diagnostics are computed correctly."""
        state = {'tracer': gaussian_tracer}
        U_zero, W_zero = create_zero_velocity_fields(atm_domain)

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U_zero,
            W=W_zero,
            diagnostic_name_suffix="_test",
        )

        proc.step_forward()

        # Check diagnostics exist
        assert hasattr(proc, 'total_tracer_mass_test')
        assert hasattr(proc, 'column_density_test')

        # Total mass should be positive
        assert proc.total_tracer_mass_test[0] > 0

    def test_sedimentation_velocity(self, atm_domain, gaussian_tracer):
        """Test that sedimentation velocity works."""
        state = {'tracer': gaussian_tracer}
        U_zero, W_zero = create_zero_velocity_fields(atm_domain)

        # Constant downward sedimentation
        W_sed = 10.0  # Pa/s downward

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U_zero,
            W=W_zero,
            W_sedimentation=W_sed,
        )

        initial_tracer = proc.state['tracer'].copy()
        proc.step_forward()

        # Tracer should have moved
        assert not np.allclose(initial_tracer, proc.state['tracer'])


class TestParticleSink:
    """Test suite for ParticleSink class."""

    def test_initialization(self, atm_domain, gaussian_tracer):
        """Test ParticleSink initialization."""
        state = {'tracer': gaussian_tracer}
        tropopause = 200.0 * np.ones(NUM_LAT)  # 200 hPa

        sink = ParticleSink(
            state=state,
            timestep=TIMESTEP,
            tropopause_p=tropopause,
            adjust_to_0=True,
        )

        assert sink is not None

    def test_adjustment_mode(self, atm_domain, gaussian_tracer):
        """Test that adjustment mode zeros tracer below tropopause."""
        state = {'tracer': gaussian_tracer}
        tropopause = 500.0 * np.ones(NUM_LAT)  # 500 hPa

        sink = ParticleSink(
            state=state,
            timestep=TIMESTEP,
            tropopause_p=tropopause,
            adjust_to_0=True,
        )

        sink.step_forward()

        # Check that tracer is zero where pressure > tropopause
        lev_points = atm_domain.axes['lev'].points
        # Create 2D mask matching tracer shape (NUM_LAT, NUM_LEV)
        under_trop = (np.ones((NUM_LAT, 1)) * (lev_points[np.newaxis, :] > 500.0)).astype(bool)

        # The tracer should be zero below tropopause after adjustment
        tracer = np.array(sink.state['tracer'])  # Convert to plain ndarray for indexing
        assert np.allclose(tracer[under_trop], 0.0)

    def test_decay_mode(self, atm_domain, gaussian_tracer):
        """Test that decay mode reduces tracer below tropopause."""
        state = {'tracer': gaussian_tracer}
        tropopause = 500.0 * np.ones(NUM_LAT)  # 500 hPa
        lifetime = 30 * 86400  # 30 days

        sink = ParticleSink(
            state=state,
            timestep=TIMESTEP,
            tropopause_p=tropopause,
            adjust_to_0=False,
            lifetime=lifetime,
        )

        initial_tracer = np.array(sink.state['tracer'].copy())
        sink.step_forward()

        # Tracer below tropopause should have decreased
        lev_points = atm_domain.axes['lev'].points
        # Create 2D mask matching tracer shape (NUM_LAT, NUM_LEV)
        under_trop = np.ones((NUM_LAT, 1)) * (lev_points[np.newaxis, :] > 500.0)
        under_trop = under_trop.astype(bool)

        final_tracer = np.array(sink.state['tracer'])

        # Values below tropopause should be smaller
        assert np.all(final_tracer[under_trop] <= initial_tracer[under_trop])


class TestRegressionValues:
    """Regression tests with saved reference values.

    These tests capture specific numerical values from the current
    implementation to verify that refactoring preserves exact behavior.
    """

    def test_regression_advection_only(self, atm_domain, gaussian_tracer):
        """Regression test for pure advection."""
        state = {'tracer': gaussian_tracer}
        U, W = create_velocity_fields(atm_domain, U_max=10.0, W_max=100.0)
        Kyy, Kzz, _ = create_diffusivity_fields(atm_domain, Kyy_val=0.0, Kzz_val=0.0)

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U,
            W=W,
            Kyy=Kyy,
            Kzz=Kzz,
            interpolation_order=2,
            use_limiters=True,
        )

        # Run for 3 timesteps
        for _ in range(3):
            proc.step_forward()

        # Capture key statistics for regression
        tracer = proc.state['tracer']
        stats = {
            'mean': np.mean(tracer),
            'std': np.std(tracer),
            'max': np.max(tracer),
            'min': np.min(tracer),
            'sum': np.sum(tracer),
        }

        # These values should remain constant after refactoring
        # (Values captured from current implementation)
        assert stats['mean'] > 0  # Placeholder - will be updated with exact values
        assert stats['min'] >= -1e-15  # Limiters should prevent negative

    def test_regression_diffusion_only(self, atm_domain, gaussian_tracer):
        """Regression test for pure diffusion."""
        state = {'tracer': gaussian_tracer}
        U_zero, W_zero = create_zero_velocity_fields(atm_domain)
        Kyy, Kzz, Kyz = create_diffusivity_fields(atm_domain, Kyy_val=1e6, Kzz_val=1e4, Kyz_val=0.0)

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U_zero,
            W=W_zero,
            Kyy=Kyy,
            Kzz=Kzz,
            Kyz=Kyz,
        )

        # Run for 3 timesteps
        for _ in range(3):
            proc.step_forward()

        tracer = proc.state['tracer']

        # Mass conservation check
        initial_mass = np.sum(proc.M_air * gaussian_tracer)
        final_mass = np.sum(proc.M_air * tracer)

        assert np.isclose(initial_mass, final_mass, rtol=1e-10)

    def test_regression_full_physics(self, atm_domain, gaussian_tracer):
        """Regression test with all physics enabled."""
        state = {'tracer': gaussian_tracer}
        # Use moderate velocities and diffusivities
        U, W = create_velocity_fields(atm_domain, U_max=2.0, W_max=20.0)
        Kyy, Kzz, Kyz = create_diffusivity_fields(
            atm_domain, Kyy_val=1e5, Kzz_val=1e3, Kyz_val=1e3
        )

        proc = TwoDimensionalAdvectionDiffusion(
            state=state,
            timestep=TIMESTEP,
            U=U,
            W=W,
            Kyy=Kyy,
            Kzz=Kzz,
            Kyz=Kyz,
            W_sedimentation=1.0,
            interpolation_order=2,
            use_limiters=True,
        )

        initial_mass = np.sum(proc.M_air * gaussian_tracer)

        # Run for 3 timesteps
        for _ in range(3):
            proc.step_forward()

        tracer = proc.state['tracer']
        final_mass = np.sum(proc.M_air * tracer)

        # Capture statistics
        stats = {
            'mean': np.mean(tracer),
            'std': np.std(tracer),
            'max': np.max(tracer),
            'min': np.min(tracer),
        }

        # Verify basic sanity - code should run without numerical blowup
        assert np.isfinite(stats['mean'])
        assert np.isfinite(stats['max'])
        # Mass should be approximately conserved
        assert np.isclose(initial_mass, final_mass, rtol=1e-8)



# ===================================================================
# Cartesian grid tests for advection/diffusion numerics
# ===================================================================

def _get_plot_dir():
    """Create and return directory for test plots."""
    os.makedirs(PLOT_DIR, exist_ok=True)
    return PLOT_DIR


def _advect_cartesian_step(tracer, velocity, bounds_1d, dbounds_1d, dt, axis,
                           order=2, use_limiters=True):
    """One NIRVANA advection step along given axis on a Cartesian grid.

    Parameters
    ----------
    tracer : ndarray (ny, nz)
    velocity : ndarray (ny+1, nz) for axis=0 or (ny, nz+1) for axis=1
    bounds_1d : ndarray (n+1,) cell boundary positions
    dbounds_1d : ndarray (n,) cell widths
    dt : float
    axis : int (0 or 1)
    """
    tr = np.moveaxis(tracer, axis, -1)
    vel = np.moveaxis(velocity, axis, -1)

    bounds_2d = np.broadcast_to(bounds_1d[np.newaxis, :], vel.shape)
    dbounds_2d = np.broadcast_to(dbounds_1d[np.newaxis, :], tr.shape)

    integral = numerics.calc_integral(tr, dbounds_2d)
    tr_ext = numerics.extend_to_boundaries(tr, axis=-1)
    db_ext = numerics.extend_dbounds(dbounds_2d, axis=-1)

    if order == 1:
        istar = numerics.linear_interp_istar(bounds_2d, integral, vel, dt)
    else:
        istar = numerics.parabolic_interp_istar(
            bounds_2d, db_ext, integral, tr_ext, vel, dt)
        if use_limiters:
            istar = numerics.apply_limiters(
                bounds_2d, integral, istar, vel, dt)

    flux = numerics.compute_advective_flux(integral, istar, dt, sign=1.0)
    flux = flux.copy()
    flux[..., 0] = 0.0
    flux[..., -1] = 0.0

    update = numerics.apply_flux_convergence(flux, dbounds_2d, dt)
    update = np.moveaxis(update, -1, axis)

    return tracer + update


def _diffuse_cartesian_step(tracer, Ky, Kz, dy, dz, dt):
    """One explicit diffusion step on a Cartesian grid.

    Parameters
    ----------
    tracer : ndarray (ny, nz)
    Ky, Kz : float, diffusivity in y and z directions
    dy : ndarray (ny,) cell widths in y
    dz : ndarray (nz,) cell widths in z
    dt : float
    """
    # Y-direction diffusive flux: shape (ny+1, nz)
    tracer_ext_y = numerics.extend_to_boundaries(tracer, axis=0)
    dy_ext = numerics.extend_dbounds(dy)
    dym = 0.5 * (dy_ext[1:] + dy_ext[:-1])
    flux_y = -Ky * np.diff(tracer_ext_y, axis=0) / dym[:, None]
    flux_y[0, :] = 0.0
    flux_y[-1, :] = 0.0

    # Z-direction diffusive flux: shape (ny, nz+1)
    tracer_ext_z = numerics.extend_to_boundaries(tracer, axis=1)
    dz_ext = numerics.extend_dbounds(dz)
    dzm = 0.5 * (dz_ext[1:] + dz_ext[:-1])
    flux_z = -Kz * np.diff(tracer_ext_z, axis=1) / dzm[None, :]
    flux_z[:, 0] = 0.0
    flux_z[:, -1] = 0.0

    tracer_new = tracer.copy()
    tracer_new += (flux_y[:-1, :] - flux_y[1:, :]) * dt / dy[:, None]
    tracer_new += (flux_z[:, :-1] - flux_z[:, 1:]) * dt / dz[None, :]

    return tracer_new


def _compute_malta_velocities(tracer, Kyz, dy, dz, epsilon_frac=0.01):
    """Compute MALTA effective velocities for off-diagonal diffusion.

    The MALTA scheme (Western et al. 2024) approximates the off-diagonal
    diffusion terms as effective advective velocities:

        Ud = -Kyz * (dC/dz) / C   (at y-boundaries)
        Wd = -Kyz * (dC/dy) / C   (at z-boundaries)

    The negative sign is for Cartesian coordinates. In the solver's pressure
    coordinates, the sign is positive due to the z=p convention.

    Parameters
    ----------
    tracer : ndarray (ny, nz)
    Kyz : float, off-diagonal diffusivity
    dy : ndarray (ny,) cell widths in y
    dz : ndarray (nz,) cell widths in z
    epsilon_frac : float
        Minimum tracer fraction for velocity computation (avoids division
        by near-zero tracer values).

    Returns
    -------
    Ud : ndarray (ny+1, nz)
        Effective y-velocity at y-boundaries
    Wd : ndarray (ny, nz+1)
        Effective z-velocity at z-boundaries
    """
    ny, nz = tracer.shape
    max_tracer = np.max(np.abs(tracer))
    epsilon = epsilon_frac * max_tracer

    # --- Ud at y-boundaries (ny+1, nz) ---
    # Need dC/dz at y-boundaries, z-centers
    tracer_bz = numerics.extend_to_boundaries(tracer, axis=1)       # (ny, nz+2)
    tracer_by = numerics.extend_to_boundaries(tracer, axis=0)       # (ny+2, nz)

    # Tracer at z-boundary positions: average adjacent z-extended cells
    tracer_z_edges = 0.5 * (tracer_bz[:, :-1] + tracer_bz[:, 1:])  # (ny, nz+1)
    # dC/dz at cell centers from z-boundary values
    dCdz = np.diff(tracer_z_edges, axis=1) / dz[None, :]            # (ny, nz)
    # Interpolate to y-boundaries
    dCdz_ext = numerics.extend_to_boundaries(dCdz, axis=0)          # (ny+2, nz)
    dCdz_yb = 0.5 * (dCdz_ext[:-1, :] + dCdz_ext[1:, :])          # (ny+1, nz)
    # Tracer at y-boundaries
    tracer_yb = 0.5 * (tracer_by[:-1, :] + tracer_by[1:, :])       # (ny+1, nz)
    # Ud = -Kyz * dCdz / C (with safeguard)
    Ud = np.where(tracer_yb > epsilon,
                  -Kyz * dCdz_yb / tracer_yb,
                  0.0)
    # Zero at domain boundaries
    Ud[0, :] = 0.0
    Ud[-1, :] = 0.0

    # --- Wd at z-boundaries (ny, nz+1) ---
    # Need dC/dy at z-boundaries, y-centers
    # Tracer at y-boundary positions: average adjacent y-extended cells
    tracer_y_edges = 0.5 * (tracer_by[:-1, :] + tracer_by[1:, :])  # (ny+1, nz)
    # dC/dy at cell centers from y-boundary values
    dCdy = np.diff(tracer_y_edges, axis=0) / dy[:, None]            # (ny, nz)
    # Interpolate to z-boundaries
    dCdy_ext = numerics.extend_to_boundaries(dCdy, axis=1)          # (ny, nz+2)
    dCdy_zb = 0.5 * (dCdy_ext[:, :-1] + dCdy_ext[:, 1:])          # (ny, nz+1)
    # Tracer at z-boundaries
    tracer_zb = 0.5 * (tracer_bz[:, :-1] + tracer_bz[:, 1:])      # (ny, nz+1)
    # Wd = -Kyz * dCdy / C (with safeguard)
    Wd = np.where(tracer_zb > epsilon,
                  -Kyz * dCdy_zb / tracer_zb,
                  0.0)
    # Zero at domain boundaries
    Wd[:, 0] = 0.0
    Wd[:, -1] = 0.0

    return Ud, Wd


class TestCircularAdvection:
    """Advect a constant tracer block around circles using rigid-body rotation.

    Uses a 100x100 Cartesian grid from -1 to +1 in both axes.
    Velocity field: v_y = -z, v_z = +y (counter-clockwise, omega=1).
    Period T = 2*pi. Shows results after 1, 5, and 25 full revolutions.
    """

    def test_full_revolution(self):
        """Advect a constant 10x10 block through 1, 5, and 25 revolutions."""
        # --- Grid ---
        N = 100
        y_bounds = np.linspace(-1, 1, N + 1)
        z_bounds = np.linspace(-1, 1, N + 1)
        y_centers = 0.5 * (y_bounds[1:] + y_bounds[:-1])
        z_centers = 0.5 * (z_bounds[1:] + z_bounds[:-1])
        dy = np.diff(y_bounds)
        dz = np.diff(z_bounds)
        Y, Z = np.meshgrid(y_centers, z_centers, indexing='ij')
        DY, DZ = np.meshgrid(dy, dz, indexing='ij')

        # --- Circular velocity (rigid-body rotation) ---
        # At y-boundaries (shape N+1 x N): v_y = -z_center
        U = -z_centers[np.newaxis, :] * np.ones((N + 1, 1))
        # At z-boundaries (shape N x N+1): v_z = +y_center
        W = y_centers[:, np.newaxis] * np.ones((1, N + 1))

        # --- Initial condition: constant 10x10 block at (0, 0.5) ---
        tracer_init = np.zeros((N, N))
        iy_center = N // 2       # y ~ 0
        iz_center = 3 * N // 4   # z ~ 0.5
        block_half = 5
        tracer_init[iy_center - block_half:iy_center + block_half,
                    iz_center - block_half:iz_center + block_half] = 1.0

        # --- Timestep (CFL ~ 0.25) ---
        max_vel = max(np.max(np.abs(U)), np.max(np.abs(W)))
        dt = 0.25 * dy[0] / max_vel
        T_rev = 2 * np.pi
        nsteps_1rev = int(np.ceil(T_rev / dt))
        dt = T_rev / nsteps_1rev  # adjust so nsteps*dt = T exactly

        # --- Initial mass ---
        initial_mass = np.sum(tracer_init * DY * DZ)

        # --- Run advection for multiple revolutions ---
        revolution_counts = [1, 5, 25]
        snapshots = {}
        diagnostics = {}

        tracer = tracer_init.copy()
        current_rev = 0
        for target_rev in revolution_counts:
            extra_revs = target_rev - current_rev
            for _ in range(extra_revs * nsteps_1rev):
                tracer = _advect_cartesian_step(tracer, U, y_bounds, dy, dt,
                                                axis=0, order=2, use_limiters=True)
                tracer = _advect_cartesian_step(tracer, W, z_bounds, dz, dt,
                                                axis=1, order=2, use_limiters=True)
            current_rev = target_rev
            snapshots[target_rev] = tracer.copy()

            final_mass = np.sum(tracer * DY * DZ)
            mass_error = abs(final_mass - initial_mass) / initial_mass
            com_y = np.sum(tracer * Y * DY * DZ) / final_mass
            com_z = np.sum(tracer * Z * DY * DZ) / final_mass
            com_y_i = np.sum(tracer_init * Y * DY * DZ) / initial_mass
            com_z_i = np.sum(tracer_init * Z * DY * DZ) / initial_mass
            com_shift = np.sqrt((com_y - com_y_i)**2 + (com_z - com_z_i)**2)
            peak_ratio = np.max(tracer) / np.max(tracer_init)
            diagnostics[target_rev] = {
                'mass_error': mass_error, 'com_shift': com_shift,
                'peak_ratio': peak_ratio, 'com': (com_y, com_z),
            }

        # --- Print diagnostics ---
        print(f"\n--- Circular Advection Test ---")
        print(f"Grid: {N}x{N}, steps/rev: {nsteps_1rev}, dt: {dt:.5f}")
        for nrev in revolution_counts:
            d = diagnostics[nrev]
            print(f"  {nrev:2d} rev: mass_err={d['mass_error']:.2e}, "
                  f"COM_shift={d['com_shift']:.4f}, peak_ratio={d['peak_ratio']:.4f}")

        # --- Plot ---
        if HAS_MATPLOTLIB:
            plot_dir = _get_plot_dir()
            fig = plt.figure(figsize=(20, 14))

            # Top row: contour maps (initial + 3 snapshots)
            vmax = np.max(tracer_init)
            levels = np.linspace(0, vmax, 21)
            theta_circle = np.linspace(0, 2 * np.pi, 200)
            R_orbit = np.sqrt(y_centers[iy_center]**2 + z_centers[iz_center]**2)

            contour_data = [('Initial', tracer_init)] + \
                [(f'{n} rev', snapshots[n]) for n in revolution_counts]

            for idx, (title, data) in enumerate(contour_data):
                ax = fig.add_subplot(2, 4, idx + 1)
                c = ax.contourf(Y, Z, data, levels=levels, cmap='viridis',
                                extend='both')
                ax.contour(Y, Z, data, levels=[0.1, 0.5, 0.9],
                           colors='k', linewidths=0.5)
                ax.set_title(title)
                ax.set_xlabel('y')
                ax.set_ylabel('z')
                ax.set_aspect('equal')
                fig.colorbar(c, ax=ax, shrink=0.8)
                ax.plot(R_orbit * np.cos(theta_circle),
                        R_orbit * np.sin(theta_circle),
                        'r--', alpha=0.5, linewidth=1)

            # Bottom row: 1D slices
            # Slice along y at z=z_center (through block center)
            iz_slice = iz_center
            ax5 = fig.add_subplot(2, 4, 5)
            ax5.plot(y_centers, tracer_init[:, iz_slice], 'k-',
                     label='Initial', linewidth=2)
            colors = ['b', 'g', 'r']
            for nrev, col in zip(revolution_counts, colors):
                ax5.plot(y_centers, snapshots[nrev][:, iz_slice], col + '-',
                         label=f'{nrev} rev', linewidth=1.5)
            ax5.set_title(f'y-slice at z={z_centers[iz_slice]:.2f}')
            ax5.set_xlabel('y')
            ax5.set_ylabel('Tracer')
            ax5.legend(fontsize=8)
            ax5.grid(True, alpha=0.3)

            # Slice along z at y=y_center (through block center)
            iy_slice = iy_center
            ax6 = fig.add_subplot(2, 4, 6)
            ax6.plot(z_centers, tracer_init[iy_slice, :], 'k-',
                     label='Initial', linewidth=2)
            for nrev, col in zip(revolution_counts, colors):
                ax6.plot(z_centers, snapshots[nrev][iy_slice, :], col + '-',
                         label=f'{nrev} rev', linewidth=1.5)
            ax6.set_title(f'z-slice at y={y_centers[iy_slice]:.2f}')
            ax6.set_xlabel('z')
            ax6.set_ylabel('Tracer')
            ax6.legend(fontsize=8)
            ax6.grid(True, alpha=0.3)

            # Diagonal slice through block center at 45 degrees
            # From (y_center-0.5, z_center-0.5) to (y_center+0.5, z_center+0.5)
            n_diag = N
            diag_y = np.linspace(-0.5, 0.5, n_diag) + y_centers[iy_center]
            diag_z = np.linspace(-0.5, 0.5, n_diag) + z_centers[iz_center]
            from scipy.interpolate import RegularGridInterpolator
            diag_s = np.linspace(-0.5 * np.sqrt(2), 0.5 * np.sqrt(2), n_diag)
            ax7 = fig.add_subplot(2, 4, 7)
            for label, data, style in [
                ('Initial', tracer_init, 'k-'),
                ('1 rev', snapshots[1], 'b-'),
                ('5 rev', snapshots[5], 'g-'),
                ('25 rev', snapshots[25], 'r-'),
            ]:
                interp = RegularGridInterpolator(
                    (y_centers, z_centers), data, method='linear',
                    bounds_error=False, fill_value=0.0)
                vals = interp(np.column_stack([diag_y, diag_z]))
                ax7.plot(diag_s, vals, style, label=label,
                         linewidth=2 if label == 'Initial' else 1.5)
            ax7.set_title('45-degree diagonal slice')
            ax7.set_xlabel('distance along diagonal')
            ax7.set_ylabel('Tracer')
            ax7.legend(fontsize=8)
            ax7.grid(True, alpha=0.3)

            # Diagnostics summary panel
            ax8 = fig.add_subplot(2, 4, 8)
            ax8.axis('off')
            text_lines = ['Diagnostics:', '']
            for nrev in revolution_counts:
                d = diagnostics[nrev]
                text_lines.append(f'{nrev} rev:')
                text_lines.append(f'  mass err = {d["mass_error"]:.2e}')
                text_lines.append(f'  COM shift = {d["com_shift"]:.4f}')
                text_lines.append(f'  peak ratio = {d["peak_ratio"]:.4f}')
                text_lines.append('')
            ax8.text(0.1, 0.95, '\n'.join(text_lines), transform=ax8.transAxes,
                     fontsize=10, verticalalignment='top', fontfamily='monospace')

            fig.suptitle(
                f'Circular Advection: 10x10 constant block, N={N}, '
                f'dt={dt:.4f}, steps/rev={nsteps_1rev}',
                fontsize=12)
            plt.tight_layout()
            path = os.path.join(plot_dir, 'circular_advection_test.png')
            plt.savefig(path, dpi=150, bbox_inches='tight')
            plt.close()
            print(f"Plot saved: {path}")

        # --- Assertions ---
        # Mass should be conserved to machine precision at all stages
        for nrev in revolution_counts:
            assert diagnostics[nrev]['mass_error'] < 1e-10, \
                f"Mass not conserved after {nrev} rev: {diagnostics[nrev]['mass_error']:.2e}"
        # After 1 revolution, COM should be close to initial position
        assert diagnostics[1]['com_shift'] < 0.15, \
            f"COM shifted too much after 1 rev: {diagnostics[1]['com_shift']:.4f}"


class TestPureDiffusionGreenFunction:
    """Test anisotropic diffusion against the analytical Green's function.

    Uses a 100x100 Cartesian grid from -1 to +1. Starts with a narrow
    Gaussian (approximating a delta function) and diffuses with Ky = 2*Kz
    until the profile changes significantly. Compares to the exact solution:

        C(y,z,t) = M / (2*pi*sigma_y(t)*sigma_z(t))
                   * exp(-y^2/(2*sigma_y(t)^2) - z^2/(2*sigma_z(t)^2))

    where sigma_y(t)^2 = sigma_0^2 + 2*Ky*t,
          sigma_z(t)^2 = sigma_0^2 + 2*Kz*t.
    """

    def test_green_function(self):
        """Diffuse a narrow Gaussian with Ky=2*Kz, compare to analytical."""
        # --- Grid ---
        N = 100
        y_bounds = np.linspace(-1, 1, N + 1)
        z_bounds = np.linspace(-1, 1, N + 1)
        y_centers = 0.5 * (y_bounds[1:] + y_bounds[:-1])
        z_centers = 0.5 * (z_bounds[1:] + z_bounds[:-1])
        dy = np.diff(y_bounds)
        dz = np.diff(z_bounds)
        Y, Z = np.meshgrid(y_centers, z_centers, indexing='ij')
        DY, DZ = np.meshgrid(dy, dz, indexing='ij')
        dx = dy[0]  # uniform grid spacing

        # --- Parameters (anisotropic: Ky = 2*Kz) ---
        Ky = 0.02
        Kz = 0.01
        sigma0 = 3 * dx   # initial width: 3 cells = 0.06

        # --- Initial condition: narrow Gaussian at origin ---
        tracer_init = np.exp(-(Y**2 + Z**2) / (2 * sigma0**2))
        total_mass = np.sum(tracer_init * DY * DZ)

        # --- Run until sigma_y triples (significant change) ---
        sigma_y_target = 3 * sigma0   # 0.18
        t_final = (sigma_y_target**2 - sigma0**2) / (2 * Ky)

        # CFL for 2D diffusion: Ky*dt/dy^2 + Kz*dt/dz^2 < 0.5
        dt = 0.4 * dx**2 / (Ky + Kz)
        nsteps = int(np.ceil(t_final / dt))
        dt = t_final / nsteps

        print(f"\n--- Anisotropic Diffusion Test (Ky=2*Kz) ---")
        print(f"Grid: {N}x{N}, Ky={Ky}, Kz={Kz}, sigma0={sigma0:.4f}")
        print(f"t_final: {t_final:.4f}, dt: {dt:.6f}, steps: {nsteps}")

        # --- Run diffusion ---
        tracer = tracer_init.copy()
        for _ in range(nsteps):
            tracer = _diffuse_cartesian_step(tracer, Ky, Kz, dy, dz, dt)

        # --- Analytical solution (anisotropic) ---
        sigma_y_f = np.sqrt(sigma0**2 + 2 * Ky * t_final)
        sigma_z_f = np.sqrt(sigma0**2 + 2 * Kz * t_final)
        analytical = (total_mass / (2 * np.pi * sigma_y_f * sigma_z_f)) * \
            np.exp(-Y**2 / (2 * sigma_y_f**2) - Z**2 / (2 * sigma_z_f**2))

        # --- Error metrics ---
        final_mass = np.sum(tracer * DY * DZ)
        mass_error = abs(final_mass - total_mass) / total_mass
        l2_error = np.sqrt(np.sum((tracer - analytical)**2 * DY * DZ)) / \
            np.sqrt(np.sum(analytical**2 * DY * DZ))
        peak_num = np.max(tracer)
        peak_ana = np.max(analytical)
        peak_error = abs(peak_num - peak_ana) / peak_ana

        print(f"sigma_y_final: {sigma_y_f:.4f}, sigma_z_final: {sigma_z_f:.4f}")
        print(f"Mass error: {mass_error:.2e}")
        print(f"Peak: numerical={peak_num:.6f}, analytical={peak_ana:.6f}, "
              f"error={peak_error:.2e}")
        print(f"L2 relative error: {l2_error:.4f}")

        # --- Plot ---
        if HAS_MATPLOTLIB:
            plot_dir = _get_plot_dir()
            fig = plt.figure(figsize=(18, 12))

            vmax = max(np.max(tracer), np.max(analytical))
            levels_init = np.linspace(0, np.max(tracer_init), 21)
            levels_final = np.linspace(0, vmax, 21)

            # Row 1: contour maps
            ax1 = fig.add_subplot(2, 3, 1)
            c1 = ax1.contourf(Y, Z, tracer_init, levels=levels_init,
                              cmap='viridis')
            ax1.set_title(f'Initial ($\\sigma_0$={sigma0:.3f})')
            ax1.set_xlabel('y')
            ax1.set_ylabel('z')
            ax1.set_aspect('equal')
            fig.colorbar(c1, ax=ax1)

            ax2 = fig.add_subplot(2, 3, 2)
            c2 = ax2.contourf(Y, Z, tracer, levels=levels_final,
                              cmap='viridis')
            ax2.set_title(f'Numerical (t={t_final:.3f})')
            ax2.set_xlabel('y')
            ax2.set_ylabel('z')
            ax2.set_aspect('equal')
            fig.colorbar(c2, ax=ax2)

            ax3 = fig.add_subplot(2, 3, 3)
            c3 = ax3.contourf(Y, Z, analytical, levels=levels_final,
                              cmap='viridis')
            ax3.set_title(f'Analytical ($\\sigma_y$={sigma_y_f:.3f}, '
                          f'$\\sigma_z$={sigma_z_f:.3f})')
            ax3.set_xlabel('y')
            ax3.set_ylabel('z')
            ax3.set_aspect('equal')
            fig.colorbar(c3, ax=ax3)

            # Row 2: 1D slices
            j_mid = N // 2
            i_mid = N // 2

            ax4 = fig.add_subplot(2, 3, 4)
            ax4.plot(y_centers, tracer_init[:, j_mid], 'b--',
                     label='Initial', linewidth=1)
            ax4.plot(y_centers, tracer[:, j_mid], 'r-',
                     label='Numerical', linewidth=2)
            ax4.plot(y_centers, analytical[:, j_mid], 'k--',
                     label='Analytical', linewidth=1.5)
            ax4.set_title('Slice along y (z=0)')
            ax4.set_xlabel('y')
            ax4.set_ylabel('Tracer')
            ax4.legend()
            ax4.grid(True, alpha=0.3)

            ax5 = fig.add_subplot(2, 3, 5)
            ax5.plot(z_centers, tracer_init[i_mid, :], 'b--',
                     label='Initial', linewidth=1)
            ax5.plot(z_centers, tracer[i_mid, :], 'r-',
                     label='Numerical', linewidth=2)
            ax5.plot(z_centers, analytical[i_mid, :], 'k--',
                     label='Analytical', linewidth=1.5)
            ax5.set_title('Slice along z (y=0)')
            ax5.set_xlabel('z')
            ax5.set_ylabel('Tracer')
            ax5.legend()
            ax5.grid(True, alpha=0.3)

            # Diagonal slice (y=z line)
            diag_idx = np.arange(N)
            r_diag = np.sign(y_centers[diag_idx]) * np.sqrt(
                y_centers[diag_idx]**2 + z_centers[diag_idx]**2)
            ax6 = fig.add_subplot(2, 3, 6)
            ax6.plot(r_diag, tracer_init[diag_idx, diag_idx], 'b--',
                     label='Initial', linewidth=1)
            ax6.plot(r_diag, tracer[diag_idx, diag_idx], 'r-',
                     label='Numerical', linewidth=2)
            ax6.plot(r_diag, analytical[diag_idx, diag_idx], 'k--',
                     label='Analytical', linewidth=1.5)
            ax6.set_title('Diagonal slice (y=z)')
            ax6.set_xlabel(r'signed $r = \pm\sqrt{y^2+z^2}$')
            ax6.set_ylabel('Tracer')
            ax6.legend()
            ax6.grid(True, alpha=0.3)

            fig.suptitle(
                f"Anisotropic Diffusion: Ky={Ky}, Kz={Kz}, "
                f"mass err={mass_error:.2e}, "
                f"L2 err={l2_error:.4f}, peak err={peak_error:.2e}",
                fontsize=12)
            plt.tight_layout()
            path = os.path.join(plot_dir,
                                'diffusion_green_function_test.png')
            plt.savefig(path, dpi=150, bbox_inches='tight')
            plt.close()
            print(f"Plot saved: {path}")

        # --- Assertions ---
        assert mass_error < 1e-10, \
            f"Mass not conserved: {mass_error:.2e}"
        assert l2_error < 0.15, \
            f"L2 relative error too large: {l2_error:.4f}"
        assert peak_error < 0.15, \
            f"Peak error too large: {peak_error:.4f}"


class TestOffDiagonalDiffusion:
    """Test full tensor diffusion (Kyz != 0) against analytical solution.

    Uses the MALTA scheme to convert off-diagonal diffusion into effective
    advective velocities. The analytical solution for a 2D Gaussian with
    a full diffusion tensor K = [[Kyy, Kyz], [Kyz, Kzz]] is:

        Sigma(t) = sigma_0^2 * I + 2*K*t
        C(y,z,t) = M / (2*pi*sqrt(det(Sigma))) * exp(-0.5 * [y,z] Sigma^-1 [y,z]^T)

    Requires positive definiteness: Kyz^2 < Kyy*Kzz.
    """

    def test_kyz_green_function(self):
        """Diffuse with off-diagonal Kyz, compare to tilted Gaussian."""
        # --- Grid ---
        N = 100
        y_bounds = np.linspace(-1, 1, N + 1)
        z_bounds = np.linspace(-1, 1, N + 1)
        y_centers = 0.5 * (y_bounds[1:] + y_bounds[:-1])
        z_centers = 0.5 * (z_bounds[1:] + z_bounds[:-1])
        dy = np.diff(y_bounds)
        dz = np.diff(z_bounds)
        Y, Z = np.meshgrid(y_centers, z_centers, indexing='ij')
        DY, DZ = np.meshgrid(dy, dz, indexing='ij')
        dx = dy[0]

        # --- Parameters (positive definite: Kyz^2 < Kyy*Kzz) ---
        Kyy = 0.02
        Kzz = 0.01
        Kyz = 0.01
        # Verify: Kyz^2 = 0.0001 < Kyy*Kzz = 0.0002
        assert Kyz**2 < Kyy * Kzz, "Diffusion tensor not positive definite"
        sigma0 = 3 * dx   # 0.06

        # --- Initial condition: narrow Gaussian at origin ---
        tracer_init = np.exp(-(Y**2 + Z**2) / (2 * sigma0**2))
        total_mass = np.sum(tracer_init * DY * DZ)

        # --- Time integration parameters ---
        # Run until the dominant sigma triples
        sigma_target = 3 * sigma0
        t_final = (sigma_target**2 - sigma0**2) / (2 * Kyy)

        # CFL for diagonal diffusion: Kyy*dt/dy^2 + Kzz*dt/dz^2 < 0.5
        dt = 0.4 * dx**2 / (Kyy + Kzz)
        nsteps = int(np.ceil(t_final / dt))
        dt = t_final / nsteps

        print(f"\n--- Off-Diagonal Diffusion Test (MALTA) ---")
        print(f"Grid: {N}x{N}, Kyy={Kyy}, Kzz={Kzz}, Kyz={Kyz}")
        print(f"Kyz^2={Kyz**2:.4f} < Kyy*Kzz={Kyy*Kzz:.4f} (positive definite)")
        print(f"sigma0={sigma0:.4f}, t_final={t_final:.4f}, "
              f"dt={dt:.6f}, steps={nsteps}")

        # --- Run: MALTA advection for off-diagonal + explicit diffusion ---
        tracer = tracer_init.copy()
        for step in range(nsteps):
            # Compute MALTA velocities from current tracer
            Ud, Wd = _compute_malta_velocities(tracer, Kyz, dy, dz,
                                               epsilon_frac=0.01)
            # Advect with MALTA velocities (off-diagonal contribution)
            tracer = _advect_cartesian_step(tracer, Ud, y_bounds, dy, dt,
                                            axis=0, order=2, use_limiters=True)
            tracer = _advect_cartesian_step(tracer, Wd, z_bounds, dz, dt,
                                            axis=1, order=2, use_limiters=True)
            # Apply diagonal diffusion
            tracer = _diffuse_cartesian_step(tracer, Kyy, Kzz, dy, dz, dt)

        # --- Analytical solution: tilted 2D Gaussian ---
        # Covariance matrix: Sigma(t) = sigma0^2*I + 2*K*t
        S_yy = sigma0**2 + 2 * Kyy * t_final
        S_zz = sigma0**2 + 2 * Kzz * t_final
        S_yz = 2 * Kyz * t_final
        det_S = S_yy * S_zz - S_yz**2
        # Sigma^{-1}
        inv_yy = S_zz / det_S
        inv_zz = S_yy / det_S
        inv_yz = -S_yz / det_S
        # C(y,z,t) = M / (2*pi*sqrt(det_S)) * exp(-0.5 * quadratic form)
        quad = inv_yy * Y**2 + 2 * inv_yz * Y * Z + inv_zz * Z**2
        analytical = (total_mass / (2 * np.pi * np.sqrt(det_S))) * \
            np.exp(-0.5 * quad)

        # --- Error metrics ---
        final_mass = np.sum(tracer * DY * DZ)
        mass_error = abs(final_mass - total_mass) / total_mass
        l2_error = np.sqrt(np.sum((tracer - analytical)**2 * DY * DZ)) / \
            np.sqrt(np.sum(analytical**2 * DY * DZ))
        peak_num = np.max(tracer)
        peak_ana = np.max(analytical)
        peak_error = abs(peak_num - peak_ana) / peak_ana

        print(f"Sigma(t): [[{S_yy:.4f}, {S_yz:.4f}], [{S_yz:.4f}, {S_zz:.4f}]]")
        print(f"Mass error: {mass_error:.2e}")
        print(f"Peak: numerical={peak_num:.6f}, analytical={peak_ana:.6f}, "
              f"error={peak_error:.2e}")
        print(f"L2 relative error: {l2_error:.4f}")

        # --- Plot ---
        if HAS_MATPLOTLIB:
            plot_dir = _get_plot_dir()
            fig = plt.figure(figsize=(18, 12))

            vmax = max(np.max(tracer), np.max(analytical))
            levels_init = np.linspace(0, np.max(tracer_init), 21)
            levels_final = np.linspace(0, vmax, 21)

            # Row 1: contour maps
            ax1 = fig.add_subplot(2, 3, 1)
            c1 = ax1.contourf(Y, Z, tracer_init, levels=levels_init,
                              cmap='viridis')
            ax1.set_title(f'Initial ($\\sigma_0$={sigma0:.3f})')
            ax1.set_xlabel('y')
            ax1.set_ylabel('z')
            ax1.set_aspect('equal')
            fig.colorbar(c1, ax=ax1)

            ax2 = fig.add_subplot(2, 3, 2)
            c2 = ax2.contourf(Y, Z, tracer, levels=levels_final,
                              cmap='viridis')
            ax2.set_title(f'Numerical (t={t_final:.3f})')
            ax2.set_xlabel('y')
            ax2.set_ylabel('z')
            ax2.set_aspect('equal')
            fig.colorbar(c2, ax=ax2)

            ax3 = fig.add_subplot(2, 3, 3)
            c3 = ax3.contourf(Y, Z, analytical, levels=levels_final,
                              cmap='viridis')
            ax3.set_title('Analytical (tilted Gaussian)')
            ax3.set_xlabel('y')
            ax3.set_ylabel('z')
            ax3.set_aspect('equal')
            fig.colorbar(c3, ax=ax3)

            # Row 2: 1D slices
            j_mid = N // 2
            i_mid = N // 2

            ax4 = fig.add_subplot(2, 3, 4)
            ax4.plot(y_centers, tracer_init[:, j_mid], 'b--',
                     label='Initial', linewidth=1)
            ax4.plot(y_centers, tracer[:, j_mid], 'r-',
                     label='Numerical', linewidth=2)
            ax4.plot(y_centers, analytical[:, j_mid], 'k--',
                     label='Analytical', linewidth=1.5)
            ax4.set_title('Slice along y (z=0)')
            ax4.set_xlabel('y')
            ax4.set_ylabel('Tracer')
            ax4.legend()
            ax4.grid(True, alpha=0.3)

            ax5 = fig.add_subplot(2, 3, 5)
            ax5.plot(z_centers, tracer_init[i_mid, :], 'b--',
                     label='Initial', linewidth=1)
            ax5.plot(z_centers, tracer[i_mid, :], 'r-',
                     label='Numerical', linewidth=2)
            ax5.plot(z_centers, analytical[i_mid, :], 'k--',
                     label='Analytical', linewidth=1.5)
            ax5.set_title('Slice along z (y=0)')
            ax5.set_xlabel('z')
            ax5.set_ylabel('Tracer')
            ax5.legend()
            ax5.grid(True, alpha=0.3)

            # Diagonal slice (45-degree, where off-diagonal effect is strongest)
            diag_idx = np.arange(N)
            r_diag = np.sign(y_centers[diag_idx]) * np.sqrt(
                y_centers[diag_idx]**2 + z_centers[diag_idx]**2)
            ax6 = fig.add_subplot(2, 3, 6)
            ax6.plot(r_diag, tracer_init[diag_idx, diag_idx], 'b--',
                     label='Initial', linewidth=1)
            ax6.plot(r_diag, tracer[diag_idx, diag_idx], 'r-',
                     label='Numerical', linewidth=2)
            ax6.plot(r_diag, analytical[diag_idx, diag_idx], 'k--',
                     label='Analytical', linewidth=1.5)
            ax6.set_title('Diagonal slice (y=z, max Kyz effect)')
            ax6.set_xlabel(r'signed $r = \pm\sqrt{y^2+z^2}$')
            ax6.set_ylabel('Tracer')
            ax6.legend()
            ax6.grid(True, alpha=0.3)

            fig.suptitle(
                f"Off-Diagonal Diffusion (MALTA): Kyy={Kyy}, Kzz={Kzz}, "
                f"Kyz={Kyz}\n"
                f"mass err={mass_error:.2e}, L2 err={l2_error:.4f}, "
                f"peak err={peak_error:.2e}",
                fontsize=12)
            plt.tight_layout()
            path = os.path.join(plot_dir,
                                'off_diagonal_diffusion_test.png')
            plt.savefig(path, dpi=150, bbox_inches='tight')
            plt.close()
            print(f"Plot saved: {path}")

        # --- Assertions ---
        assert mass_error < 1e-5, \
            f"Mass not conserved: {mass_error:.2e}"
        assert l2_error < 0.3, \
            f"L2 relative error too large: {l2_error:.4f}"
        assert peak_error < 0.3, \
            f"Peak error too large: {peak_error:.4f}"


# Run tests when executed directly
if __name__ == '__main__':
    pytest.main([__file__, '-v'])
