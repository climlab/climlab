r"""NIRVANA advection scheme numerical routines for 2D tracer transport.

This module provides numerical routines for the NIRVANA advection scheme
(Leonard 1995, Gregory 2002) used in two-dimensional tracer transport.

All functions in this module operate on the **final dimension** of the input
arrays. Use ``np.moveaxis()`` to position the axis of interest as the last
axis before calling these functions.

The NIRVANA advection scheme
----------------------------

The advection equation in 1D is:

.. math::

    \frac{\partial \chi}{\partial t} + u \frac{\partial \chi}{\partial x} = 0

The scheme works by computing a cumulative tracer integral at cell boundaries:

.. math::

    I_{k+1/2} = \sum_{i=1}^{k} \chi_i \Delta x_i

Then interpolating the integral to the upstream position:

.. math::

    I^*_{k+1/2} = I(x_{k+1/2} - u_{k+1/2} \Delta t)

The advective flux is computed as:

.. math::

    F_{k+1/2} = \frac{I_{k+1/2} - I^*_{k+1/2}}{\Delta t}

And the tracer is updated by flux convergence:

.. math::

    \chi_k^+ = \chi_k + \frac{F_{k-1/2} - F_{k+1/2}}{\Delta x_k}

Two interpolation methods are provided:
- Linear (1st order): First-order upwind scheme
- Parabolic (2nd order): Second-order scheme with optional monotonicity limiters

References
----------
.. [1] Leonard, B.P. (1995). "Order of accuracy of QUICK and related
       convection-diffusion schemes." Applied Mathematical Modelling.
.. [2] Gregory, D. (2002). "Sensitivity of cloud microphysics to choice
       of advection scheme." Q. J. R. Meteorol. Soc.
"""
import numpy as np


def calc_integral(tracer, dbounds):
    r"""Compute cumulative tracer integral along the last axis.

    Computes:

    .. math::

        I_{k+1/2} = \sum_{i=1}^{k} \chi_i \Delta x_i

    (Equation A10 in Gregory 2002)

    Parameters
    ----------
    tracer : ndarray (..., J)
        Tracer mixing ratio at cell centers
    dbounds : ndarray (..., J)
        Cell spacings, must be broadcastable to tracer shape

    Returns
    -------
    integral : ndarray (..., J+1)
        Cumulative tracer integral at cell boundaries.
        First element is 0 (boundary condition).
    """
    J = tracer.shape[-1]
    integral = np.zeros(tracer.shape[:-1] + (J + 1,))
    integral[..., 1:] = np.cumsum(tracer * dbounds, axis=-1)
    return integral


def linear_interp_1d(xvec, vec, location):
    r"""Linear interpolation along the last axis.

    Interpolates values from ``vec`` defined at positions ``xvec`` to
    new positions ``location``.

    Parameters
    ----------
    xvec : ndarray (..., J+1)
        Positions where vec is defined
    vec : ndarray (..., J+1)
        Values at xvec positions
    location : ndarray (..., J)
        New positions for interpolation (must be between consecutive xvec points)

    Returns
    -------
    result : ndarray (..., J)
        Interpolated values at location positions
    """
    # Linear interpolation: vec[i] + (vec[i+1] - vec[i]) * (loc - x[i]) / (x[i+1] - x[i])
    result = vec[..., :-1] + (np.diff(vec, axis=-1) *
                              (location - xvec[..., :-1]) / np.diff(xvec, axis=-1))
    return result


def linear_interp_istar(bounds, integral, velocity, dt):
    r"""Compute I* using linear interpolation (first-order NIRVANA).

    For positive velocity (flow to the right), interpolates from the left cell.
    For negative velocity (flow to the left), interpolates from the right cell.

    Parameters
    ----------
    bounds : ndarray (..., J+1)
        Cell boundary positions
    integral : ndarray (..., J+1)
        Cumulative tracer integral at boundaries
    velocity : ndarray (..., J+1)
        Advecting velocity at boundaries
    dt : float
        Timestep

    Returns
    -------
    istar : ndarray (..., J+1)
        Interpolated integral at upstream positions
    """
    J = integral.shape[-1] - 1

    # Left-going interpolation (for positive velocity)
    # Upstream position is bounds[1:] - |u| * dt
    pos_left = bounds[..., 1:] - np.abs(velocity[..., 1:]) * dt
    istarleft = linear_interp_1d(bounds, integral, pos_left)
    # Pad with zero at left boundary
    istarleft = np.concatenate([np.zeros(istarleft.shape[:-1] + (1,)), istarleft], axis=-1)

    # Right-going interpolation (for negative velocity)
    # Upstream position is bounds[:-1] + |u| * dt
    pos_right = bounds[..., :-1] + np.abs(velocity[..., :-1]) * dt
    istarright = linear_interp_1d(bounds, integral, pos_right)
    # Pad with boundary value at right boundary
    istarright = np.concatenate([istarright, integral[..., -1:]], axis=-1)

    # Select based on flow direction
    return np.where(velocity > 0, istarleft, istarright)


def parabolic_interp_istar(bounds, dbounds_extended, integral, tracer_extended, velocity, dt):
    r"""Compute I* using parabolic interpolation (second-order NIRVANA).

    Uses a quadratic reconstruction of the tracer integral based on
    linear interpolation of tracer values (Leonard 1995, eq. 29).

    The formula is:

    .. math::

        I^*_{k+1/2} = I_{k+1/2} - \frac{\chi_k \Delta x_{k+1} + \chi_{k+1} \Delta x_k}
                                       {\Delta x_k + \Delta x_{k+1}} u \Delta t
                    + \frac{\chi_{k+1} - \chi_k}{\Delta x_k + \Delta x_{k+1}} (u \Delta t)^2

    Parameters
    ----------
    bounds : ndarray (..., J+1)
        Cell boundary positions (length J+1 along last axis)
    dbounds_extended : ndarray (..., J+2)
        Cell spacings with boundary values repeated at edges (length J+2)
    integral : ndarray (..., J+1)
        Cumulative tracer integral at boundaries
    tracer_extended : ndarray (..., J+2)
        Tracer values with boundary cells repeated at edges (length J+2)
    velocity : ndarray (..., J+1)
        Advecting velocity at boundaries
    dt : float
        Timestep

    Returns
    -------
    istar : ndarray (..., J+1)
        Interpolated integral at upstream positions
    """
    # dbounds_extended has shape (..., J+2): [db[0], db[0], db[1], ..., db[J-1], db[J-1]]
    # tracer_extended has shape (..., J+2): [t[0], t[0], t[1], ..., t[J-1], t[J-1]]

    # dx_k corresponds to dbounds_extended[..., :-1] (left cell width)
    # dx_{k+1} corresponds to dbounds_extended[..., 1:] (right cell width)
    dx_left = dbounds_extended[..., :-1]   # (..., J+1)
    dx_right = dbounds_extended[..., 1:]   # (..., J+1)
    dx_sum = dx_left + dx_right

    # chi_k corresponds to tracer_extended[..., :-1]
    # chi_{k+1} corresponds to tracer_extended[..., 1:]
    chi_left = tracer_extended[..., :-1]   # (..., J+1)
    chi_right = tracer_extended[..., 1:]   # (..., J+1)

    # Compute I* using the parabolic formula
    u_dt = velocity * dt
    weighted_chi = (chi_left * dx_right + chi_right * dx_left) / dx_sum
    chi_diff = (chi_right - chi_left) / dx_sum

    istar = integral - weighted_chi * u_dt + chi_diff * (u_dt ** 2)

    return istar


def apply_limiters(bounds, integral, istar, velocity, dt):
    r"""Apply monotonicity limiters to I* values.

    Implements the limiting from Gregory 2002 equations A12-A13:

    .. math::

        I^* = \min\{I_{max}, \max(I^*, I_{min})\}

    where:

    .. math::

        I_{min} = \min\{I_c, \max(I_l, I_r)\}
        I_{max} = \max\{I_c, \min(I_l, I_r)\}

    and :math:`I_c, I_l, I_r` are linearly interpolated/extrapolated values.

    Parameters
    ----------
    bounds : ndarray (..., J+1)
        Cell boundary positions
    integral : ndarray (..., J+1)
        Cumulative tracer integral at boundaries
    istar : ndarray (..., J+1)
        Parabolic interpolated I* values
    velocity : ndarray (..., J+1)
        Advecting velocity at boundaries
    dt : float
        Timestep

    Returns
    -------
    istar_limited : ndarray (..., J+1)
        Limited I* values
    """
    J = integral.shape[-1] - 1

    # --- For positive velocity (leftward interpolation) ---
    # Position for icenter: bounds[1:] - |u[1:]| * dt
    pos_center_left = bounds[..., 1:] - np.abs(velocity[..., 1:]) * dt
    icenter_left = linear_interp_1d(bounds, integral, pos_center_left)

    # iright: interpolate using FULL grid at bounds[:-1] - |u[:-1]|*dt, then shift
    pos_right_left = bounds[..., :-1] - np.abs(velocity[..., :-1]) * dt
    iright_left = linear_interp_1d(bounds, integral, pos_right_left)
    # Shift: remove first element, duplicate last
    iright_left = np.concatenate([iright_left[..., 1:], iright_left[..., -1:]], axis=-1)

    # ileft: interpolate using shifted grid (bounds[:-1], integral[:-1])
    # at position bounds[2:] - |u[2:]|*dt
    pos_left_left = bounds[..., 2:] - np.abs(velocity[..., 2:]) * dt
    ileft_left = linear_interp_1d(bounds[..., :-1], integral[..., :-1], pos_left_left)
    # Pad: prepend first value
    ileft_left = np.concatenate([ileft_left[..., :1], ileft_left], axis=-1)

    # Compute limits
    imin_left = np.minimum(icenter_left, np.maximum(ileft_left, iright_left))
    imax_left = np.maximum(icenter_left, np.minimum(ileft_left, iright_left))

    # Apply limits to istar[1:] for positive velocity
    istarleft = np.minimum(imax_left, np.maximum(istar[..., 1:], imin_left))
    istarleft = np.concatenate([np.zeros(istarleft.shape[:-1] + (1,)), istarleft], axis=-1)

    # --- For negative velocity (rightward interpolation) ---
    # Position for icenter: bounds[:-1] + |u[:-1]| * dt
    pos_center_right = bounds[..., :-1] + np.abs(velocity[..., :-1]) * dt
    icenter_right = linear_interp_1d(bounds, integral, pos_center_right)

    # iright: interpolate using shifted grid (bounds[1:], integral[1:])
    # at position bounds[:-2] + |u[:-2]|*dt
    pos_right_right = bounds[..., :-2] + np.abs(velocity[..., :-2]) * dt
    iright_right = linear_interp_1d(bounds[..., 1:], integral[..., 1:], pos_right_right)
    # Pad: append last value
    iright_right = np.concatenate([iright_right, iright_right[..., -1:]], axis=-1)

    # ileft: interpolate using FULL grid at bounds[1:] + |u[1:]|*dt, then shift
    pos_left_right = bounds[..., 1:] + np.abs(velocity[..., 1:]) * dt
    ileft_right = linear_interp_1d(bounds, integral, pos_left_right)
    # Shift: prepend first value, remove last
    ileft_right = np.concatenate([ileft_right[..., :1], ileft_right[..., :-1]], axis=-1)

    # Compute limits
    imin_right = np.minimum(icenter_right, np.maximum(ileft_right, iright_right))
    imax_right = np.maximum(icenter_right, np.minimum(ileft_right, iright_right))

    # Apply limits to istar[:-1] for negative velocity
    istarright = np.minimum(imax_right, np.maximum(istar[..., :-1], imin_right))
    istarright = np.concatenate([istarright, istarright[..., -1:]], axis=-1)

    # Select based on flow direction
    return np.where(velocity > 0, istarleft, istarright)


def compute_advective_flux(integral, istar, dt, sign=1.0):
    r"""Compute advective flux from integral difference.

    .. math::

        F_{k+1/2} = \text{sign} \cdot \frac{I_{k+1/2} - I^*_{k+1/2}}{\Delta t}

    Parameters
    ----------
    integral : ndarray (..., J+1)
        Cumulative tracer integral at boundaries
    istar : ndarray (..., J+1)
        Interpolated integral at upstream positions
    dt : float
        Timestep
    sign : float, optional
        Sign convention (+1 for lat, -1 for pressure). Default is 1.0.

    Returns
    -------
    flux : ndarray (..., J+1)
        Advective flux at cell boundaries
    """
    return sign * (integral - istar) / dt


def apply_flux_convergence(flux, dbounds, dt, tend_fac=1.0):
    r"""Apply flux convergence to update tracer field.

    Computes the tracer update from flux divergence:

    .. math::

        \Delta \chi_k = \text{tend\_fac} \cdot \frac{F_{k-1/2} - F_{k+1/2}}{\Delta x_k} \Delta t

    Parameters
    ----------
    flux : ndarray (..., J+1)
        Flux at cell boundaries
    dbounds : ndarray (..., J)
        Cell spacings
    dt : float
        Timestep
    tend_fac : float, optional
        Tendency factor (for diagnostic purposes). Default is 1.0.

    Returns
    -------
    tracer_update : ndarray (..., J)
        Change in tracer due to flux convergence
    """
    # flux[:-1] - flux[1:] is the flux convergence (inflow - outflow)
    return tend_fac * (flux[..., :-1] - flux[..., 1:]) / dbounds * dt


def compute_diffusive_flux(tracer_extended, dbounds_extended, diffusivity):
    r"""Compute diffusive flux at cell boundaries.

    Uses centered differences:

    .. math::

        F^K_{k+1/2} = -K_{k+1/2} \frac{\chi_{k+1} - \chi_k}{(\Delta x_k + \Delta x_{k+1})/2}

    Parameters
    ----------
    tracer_extended : ndarray (..., J+2)
        Tracer values with boundary cells repeated at edges
    dbounds_extended : ndarray (..., J+2)
        Cell spacings with boundary values repeated at edges
    diffusivity : ndarray (..., J+1)
        Diffusivity at cell boundaries

    Returns
    -------
    flux : ndarray (..., J+1)
        Diffusive flux at cell boundaries
    """
    # Mean spacing at interfaces
    dxm = 0.5 * (dbounds_extended[..., 1:] + dbounds_extended[..., :-1])  # (..., J+1)

    # Tracer gradient at interfaces
    dchi = np.diff(tracer_extended, axis=-1)  # (..., J+1)

    flux = -diffusivity * dchi / dxm

    return flux


def extend_to_boundaries(arr, axis=-1):
    r"""Extend array by repeating boundary values.

    Adds ghost cells at both ends by repeating the first and last values.

    Parameters
    ----------
    arr : ndarray
        Array to extend
    axis : int, optional
        Axis along which to extend. Default is -1 (last axis).

    Returns
    -------
    extended : ndarray
        Array with shape increased by 2 along specified axis
    """
    # Get slices for first and last elements
    first_slice = [slice(None)] * arr.ndim
    first_slice[axis] = slice(0, 1)
    last_slice = [slice(None)] * arr.ndim
    last_slice[axis] = slice(-1, None)

    first = arr[tuple(first_slice)]
    last = arr[tuple(last_slice)]

    return np.concatenate([first, arr, last], axis=axis)


def extend_dbounds(dbounds, axis=-1):
    r"""Extend cell spacing array for boundary treatment.

    Creates array of length J+2 from length J by repeating first and last values.

    Parameters
    ----------
    dbounds : ndarray (..., J)
        Cell spacings
    axis : int, optional
        Axis along which to extend. Default is -1 (last axis).

    Returns
    -------
    extended : ndarray (..., J+2)
        Extended cell spacings
    """
    return extend_to_boundaries(dbounds, axis=axis)
