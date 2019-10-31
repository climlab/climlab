r'''
The 1D advection-diffusion problem
----------------------------------

The equation to be solved is

.. math::

    \frac{\partial}{\partial t} \psi(x,t) &= -\frac{1}{w(x)} \frac{\partial}{\partial x} \left[ w(x) ~ \mathcal{F}(x,t) \right] + \dot{\psi}\\
    \mathcal{F} &= U(x) \psi(x) -K(x) ~ \frac{\partial \psi}{\partial x} + F(x)

for the following quantities:

- state variable :math:`\psi(x,t)`
- diffusivity :math:`K(x)` in units of :math:`x^2 ~ t^{-1}`
- advecting velocity :math:`U(x)` in units of :math:`x ~ t^{-1}`
- a prescribed flux :math:`F(x)` (including boundary conditions) in units of :math:`\psi ~ x ~ t^{-1}`
- a scalar source/sink :math:`\dot{\psi}(x)` in units of :math:`\psi ~ t^{-1}`
- weighting function :math:`w(x)` for the divergence operator on curvilinear grids.

The boundary condition is a flux condition at the end points:

.. math::
    \begin{align}  \label{eq:fluxcondition}
    \mathcal{F}(x_0) &= F(x_0)   &    \mathcal{F}(x_J) &= F(x_J)
    \end{align}

which requires that the advecting velocity :math:`u(x) = 0` at the end points :math:`x_0, x_J`

The solver is implemented on a 1D staggered grid, with J+1 flux points
and J scalar points located somewhere between the flux points.

The solver does **not** assume the gridpoints are evenly spaced in :math:`x`.

Routines are provided to compute the following:

- Advective, diffusive, and total fluxes (the terms of :math:`\mathcal{F}`)
- Tridiagonal matrix operator for the flux convergence
- The actual flux convergence, or instantaneous scalar tendency given a current value of :math:`\psi(x)`
- Future value of :math:`\psi(x)` for an implicit timestep

Some details of the solver formulas are laid out below for reference.

Spatial discretization
----------------------

We use a non-uniform staggered spatial grid with scalar :math:`\psi` evaluated at :math:`J` points,
and flux :math:`\mathcal{F}` evaluated at :math:`J+1` flux points.
The indexing will run from :math:`j=0` to :math:`j=J` for the flux points,
and :math:`i=0` to :math:`i=J-1` for the scalar points.
This notation is consistent with zero-indexed Python arrays.

We define the following arrays:

- :math:`\mathcal{X}_b[j]` is a length J+1 array defining the location of the flux points.
- :math:`\mathcal{X}[i]` is a length J array defining the location of the scalar points, where point :math:`\mathcal{X}[j]` is somewhere between :math:`\mathcal{X}_b[j]` and :math:`\mathcal{X}_b[j+1]` for all :math:`j<J`.
- :math:`\psi[i], \dot{\psi}[i]` are length J arrays defined on :math:`\mathcal{X}`.
- :math:`U[j], K[j], F[j]` are all arrays of length J+1 defined on :math:`\mathcal{X}_b`.
- The grid weights are similarly in arrays :math:`W_b[j], W[i]` respectively on :math:`\mathcal{X}_b`` and :math:`\mathcal{X}`.

Centered difference formulas for the flux
-----------------------------------------

We use centered differences in :math:`x` to discretize the spatial derivatives.
The diffusive component of the flux is thus

.. math::

    \begin{align*}
    \mathcal{F}_{diff}[j] &= - K[j] \frac{ \left( \psi[i] - \psi[i-1] \right) }{\left( \mathcal{X}[i] - \mathcal{X}[i-1] \right)}  &  j&=i=1,2,...,J-1
    \end{align*}

The diffusive flux is assumed to be zero at the boundaries.

The advective term requires an additional approximation since the scalar :math:`\psi` is not defined at the flux points.
We use a linear interpolation to the flux points:

.. math::

    \begin{align*}
    \psi_b[j] &\equiv \psi[i-1]  \left( \frac{\mathcal{X}[i] - \mathcal{X}_b[j]}{\mathcal{X}[i] - \mathcal{X}[i-1]} \right)  +   \psi[i]  \left(  \frac{ \mathcal{X}_b[j] - \mathcal{X}[i-1] }{\mathcal{X}[i] - \mathcal{X}[i-1]} \right)   &   j&=i=1,2,...,J-1
    \end{align*}

Note that for an evenly spaced grid, this reduces to the simple average :math:`\frac{1}{2} \left(  \psi[i-1] +  \psi[i] \right)`.

With this interpolation, the advective flux is approximated by

.. math::

    \begin{align*}
    \mathcal{F}_{adv}[j] &= \frac{U[j] }{\mathcal{X}[i] - \mathcal{X}[i-1]} \left(  \psi[i-1] (\mathcal{X}[i] - \mathcal{X}_b[j])   +   \psi[i] (\mathcal{X}_b[j] - \mathcal{X}[i-1])   \right) &   j&=i=1,2,...,J-1
    \end{align*}

The total flux away from the boundaries (after some recombining terms) is thus:

.. math::

    \mathcal{F}[j] = F[j] + \psi[i-1] \left( \frac{K[j] + U[j] (\mathcal{X}[i] - \mathcal{X}_b[j]) }{ \mathcal{X}[i] - \mathcal{X}[i-1] }   \right)  - \psi[i]  \left(  \frac{K[j] - U[j] (\mathcal{X}_b[j] - \mathcal{X}[i-1]) }{\mathcal{X}[i] - \mathcal{X}[i-1] } \right)

which is valid for j=i=1,2,...,J-1.

Centered difference formulas for the flux convergence
-----------------------------------------------------

Centered difference approximation of the flux convergence gives

.. math::

    \begin{align*}
    \frac{\partial }{\partial t} \psi[i] &= -\frac{ W_b[j+1] \mathcal{F}[j+1] - W_b[j] \mathcal{F}[j] }{W[i] ( \mathcal{X}_b[j+1] - \mathcal{X}_b[j] )}  + \dot{\psi}[i] & i&=j=0,1,...,J-1
    \end{align*}

The flux convergences are best expressed together in matrix form:

.. math::

    \begin{equation}
    \frac{\partial \boldsymbol{\psi}}{\partial t} = \boldsymbol{T} ~ \boldsymbol{\psi} + \boldsymbol{S}
    \end{equation}

where :math:`\boldsymbol{\psi}` is the :math:`J\times1` column vector,
:math:`\boldsymbol{S}` is a :math:`J\times1` column vector
representing the prescribed flux convergence and source terms, whose elements are

.. math::

    \begin{align}
    S[i] &= \frac{-W_b[j+1] F[j+1] + W_b[j] F[j]}{W[i] ( \mathcal{X}_b[j+1] - \mathcal{X}_b[j] )}  + \dot{\psi}[i]  &  i&=j=0,1,...,J-1
    \end{align}

and :math:`\boldsymbol{T}` is a :math:`J\times J` tridiagonal matrix:

.. math::

    \begin{equation}
    \boldsymbol{T} ~ \boldsymbol{\psi} = \left[\begin{array}{ccccccc} T_{m0} & T_{u1} & 0 & ... & 0 & 0 & 0 \\T_{l0} & T_{m1} & T_{u2} & ... & 0 & 0 & 0 \\ 0 & T_{l1} & T_{m2} & ... & 0 & 0 & 0 \\... & ... & ... & ... & ... & ... & ... \\0 & 0 & 0 & ... & T_{m(J-3)} & T_{u(J-2)} & 0 \\0 & 0 & 0 & ... & T_{l(J-3)} & T_{m(J-2)} & T_{u(J-1)} \\0 & 0 & 0 & ... & 0 & T_{l(J-2)} & T_{m(J-1)}\end{array}\right] \left[\begin{array}{c} \psi_0 \\ \psi_1 \\ \psi_2 \\... \\ \psi_{J-3} \\ \psi_{J-2} \\ \psi_{J-1} \end{array}\right]
    \end{equation}

with vectors :math:`T_l, T_m, T_u` representing respectively the lower, main, and upper diagonals of :math:`\boldsymbol{T}`.
We will treat all three vectors as length J;
the 0th element of :math:`T_u` is ignored while the (J-1)th element of :math:`T_l` is ignored
(this is consistent with the expected inputs for the Python module scipy.linalg.solve_banded).

The instantanous tendency is then easily computed by matrix multiplication.

The elements of the main diagonal of :math:`\boldsymbol{\psi}` can be computed from

.. math::

    \begin{align}  \label{eq:maindiag}
    \begin{split}
    T_m[i] &= -\left( \frac{ W_b[j+1]  \big( K[j+1] + U[j+1] (\mathcal{X}[i+1] - \mathcal{X}_b[j+1]) \big) }{ W[i] ( \mathcal{X}_b[j+1] - \mathcal{X}_b[j] )(\mathcal{X}[i+1] - \mathcal{X}[i]) }   \right)  \\
    & \qquad - \left(  \frac{W_b[j] \big( K[j] - U[j] (\mathcal{X}_b[j] - \mathcal{X}[i-1]) \big) }{W[i] ( \mathcal{X}_b[j+1] - \mathcal{X}_b[j] )(\mathcal{X}[i] - \mathcal{X}[i-1]) } \right)  \\
    i &=j=0,2,...,J-1
    \end{split}
    \end{align}

which is valid at the boundaries so long as we set :math:`W_b[0] = W_b[J] = 0`.

The lower diagonal (including the right boundary condition) is computed from

.. math::

    \begin{align}  \label{eq:lowerdiag}
    \begin{split}
    T_l[i-1] &= \left( \frac{W_b[j]}{W[i] } \right) \left( \frac{ K[j] + U[j] (\mathcal{X}[i] - \mathcal{X}_b[j]) }{( \mathcal{X}_b[j+1] - \mathcal{X}_b[j] ) (\mathcal{X}[i] - \mathcal{X}[i-1] )}   \right) \\
     i &=j =1,2,...,J-2, J-1
     \end{split}
    \end{align}

Finally the upper diagonal (including the left boundary condition) is computed from

.. math::

    \begin{align}  \label{eq:upperdiag}
    \begin{split}
    T_u[i+1] &= \left( \frac{W_b[j+1]}{W[i]} \right) \left( \frac{K[j+1] - U[j+1] (\mathcal{X}_b[j+1] - \mathcal{X}[i]) }{( \mathcal{X}_b[j+1] - \mathcal{X}_b[j] )(\mathcal{X}[i+1] - \mathcal{X}[i] ) }  \right)  \\
    i &= j=0,...,J-2
    \end{split}
    \end{align}

Implicit time discretization
----------------------------

The forward-time finite difference approximation to LHS of the flux-convergence equation is simply

.. math::
    \begin{equation}
    \frac{\partial \psi[i]}{\partial t} \approx \frac{\psi^{n+1}[i]- \psi^{n}[i]}{\Delta t}
    \end{equation}

where the superscript :math:`n` indicates the time index.

We use the implicit-time method, in which the RHS is evaluated at the future time :math:`n+1`.
Applying this to the matrix equation above
and moving all the terms at time :math:`n+1` over to the LHS yields

.. math::

    \begin{equation}  \label{eq:implicit_tridiagonal}
    \left( \boldsymbol{I} - \boldsymbol{T} \Delta t \right) \boldsymbol{\psi}^{n+1} = \boldsymbol{\psi}^{n} + \boldsymbol{S} \Delta t
    \end{equation}

where :math:`\boldsymbol{I}` is the :math:`J\times J` identity matrix.

Solving for the future value :math:`\boldsymbol{\psi}^{n+1}` is then accomplished
by solving the :math:`J \times J` tridiagonal linear system using standard routines.

Analytical benchmark
--------------------

Here is an analytical case to be used for testing purposes to validate the numerical code.
This is implemented in the CLIMLAB test suite.

- :math:`K=K_0` is constant
- :math:`w(x) = 1` everywhere (Cartesian coordinates)
- :math:`F = 0` everywhere
- :math:`\psi(x,0) = \psi_0 \sin^2\left(\frac{\pi x}{L}\right)`
- :math:`u(x) = U_0 \sin\left(\frac{\pi x}{L}\right)`
for a domain with endpoints at :math:`x=0` and :math:`x=L`.

The analytical solution is

.. math::

    \begin{align}
    \mathcal{F} &= \psi_0 \sin\left(\frac{\pi x}{L}\right) \left[U_0 \sin^2\left(\frac{\pi x}{L}\right) - 2K \frac{\pi}{L} \cos\left(\frac{\pi x}{L}\right) \right]  \\
    \frac{\partial \psi}{\partial t} &= -\psi_0 \frac{\pi}{L} \left\{ 3 U_0 \sin^2\left(\frac{\pi x}{L}\right) \cos\left(\frac{\pi x}{L}\right) -2K\frac{\pi}{L} \left[\cos^2\left(\frac{\pi x}{L}\right) -\sin^2\left(\frac{\pi x}{L}\right) \right] \right\}
    \end{align}

which satisfies the boundary condition :math:`\mathcal{F} = 0` at :math:`x=0` and :math:`x=L`.


Module function reference
-------------------------

All the functions in ``climlab.dynamics.adv_diff_numerics`` are vectorized
to handle multidimensional input. The key assumption is that
**advection-diffusion operates along the final dimension**.

Inputs should be reshaped appropriately (e.g. with ``numpy.moveaxis()``)
before calling these functions.
'''
from __future__ import division
from numpy import zeros, ones, zeros_like, ones_like, matmul, diag, diag_indices, diff, newaxis
from numpy.linalg import solve
from scipy.linalg import solve_banded

def diffusive_flux(X, Xb, K, field):
    '''Return the diffusive flux on cell boundaries (length J+1)'''
    flux = zeros_like(K)
    flux[...,1:-1] += field[...,:-1]*K[...,1:-1]/diff(X,axis=-1)
    flux[...,1:-1] -= field[...,1:]*K[...,1:-1]/diff(X,axis=-1)
    return flux

def advective_flux(X, Xb, U, field):
    '''Return the advective flux on cell boundaries (length J+1)'''
    flux = zeros_like(U)
    flux[...,1:-1] += field[...,:-1]*(U[...,1:-1]*(X[...,1:]-Xb[...,1:-1]))/diff(X,axis=-1)
    flux[...,1:-1] -= field[...,1:]*(-U[...,1:-1]*(Xb[...,1:-1]-X[...,:-1]))/diff(X,axis=-1)
    return flux

def total_flux(X, Xb, K, U, field, prescribed_flux=None):
    '''Return the total (advective + diffusive + prescribed) flux
       on cell boundaries (length J+1)'''
    if prescribed_flux is None:
        prescribed_flux = zeros_like(U)
    return advective_flux(X, Xb, U, field) + diffusive_flux(X, Xb, K, field) + prescribed_flux

def advdiff_tridiag(X, Xb, K, U, W=None, Wb=None, use_banded_solver=False):
    r'''Compute the tridiagonal matrix operator for the advective-diffusive
       flux convergence.

       Input arrays of length J+1:
         Xb, Wb, K, U
       Input arrays of length J:
         X, W

       The 0th and Jth (i.e. first and last) elements of Wb are ignored;
       assuming boundary condition is a prescribed flux.

       The return value depends on input flag ``use_banded_solver``

       If ``use_banded_solver==True``, return a 3xJ array containing the elements of the tridiagonal.
       This version is restricted to 1D input arrays,
       but is suitable for use with the efficient banded solver.

       If ``use_banded_solver=False`` (which it must be for multidimensional input),
       return an array (...,J,J) with the full tridiagonal matrix.
    '''
    J = X.shape[-1]
    if (W is None):
        W = ones_like(X)
    if (Wb is None):
        Wb = ones_like(Xb)
    #  These are all length (J-1) in the last axis
    lower_diagonal = (Wb[...,1:-1]/W[...,1:] *
        (K[...,1:-1]+U[...,1:-1]*(X[...,1:]-Xb[...,1:-1])) /
        ((Xb[...,2:]-Xb[...,1:-1])*(X[...,1:]-X[...,:-1])))
    upper_diagonal = (Wb[...,1:-1]/W[...,:-1] *
        (K[...,1:-1]-U[...,1:-1]*(Xb[...,1:-1]-X[...,:-1])) /
        ((Xb[...,1:-1]-Xb[...,:-2])*(X[...,1:]-X[...,:-1])))
    main_diagonal_term1 = (-Wb[...,1:-1]/W[...,:-1] *
        (K[...,1:-1]+U[...,1:-1]*(X[...,1:]-Xb[...,1:-1])) /
        ((Xb[...,1:-1]-Xb[...,:-2])*(X[...,1:]-X[...,:-1])))
    main_diagonal_term2 = (-Wb[...,1:-1]/W[...,1:] *
        (K[...,1:-1]-U[...,1:-1]*(Xb[...,1:-1]-X[...,:-1])) /
        ((Xb[...,2:]-Xb[...,1:-1])*(X[...,1:]-X[...,:-1])))
    if use_banded_solver:
        # Pack the diagonals into a 3xJ array
        tridiag_banded = zeros((3,J))
        # Lower diagonal (last element ignored)
        tridiag_banded[2,:-1] = lower_diagonal
        # Upper diagonal (first element ignored)
        tridiag_banded[0,1:] = upper_diagonal
        # Main diagonal, term 1, length J-1
        tridiag_banded[1,:-1] += main_diagonal_term1
        # Main diagonal, term 2, length J-1
        tridiag_banded[1, 1:] += main_diagonal_term2
        return tridiag_banded
    else:
        #  If X.size is (...,J), then the tridiagonal operator is (...,J,J)
        sizeJJ = tuple([n for n in X.shape[:-1]] + [J,J])
        tridiag = zeros(sizeJJ)
        #  indices for main, upper, and lower diagonals of a JxJ matrix
        inds_main = diag_indices(J)
        inds_upper = (inds_main[0][:-1], inds_main[1][1:])
        inds_lower = (inds_main[0][1:], inds_main[1][:-1])
        # Lower diagonal (length J-1)
        tridiag[...,inds_lower[0],inds_lower[1]] = lower_diagonal
        # Upper diagonal (length J-1)
        tridiag[...,inds_upper[0],inds_upper[1]] = upper_diagonal
        # Main diagonal, term 1, length J-1
        tridiag[...,inds_main[0][:-1],inds_main[1][:-1]] += main_diagonal_term1
        # Main diagonal, term 2, length J-1
        tridiag[...,inds_main[0][1:],inds_main[1][1:]] += main_diagonal_term2
        return tridiag

def make_the_actual_tridiagonal_matrix(tridiag_banded):
    '''Convert a (3xJ) banded array into full (JxJ) tridiagonal matrix form.'''
    return (diag(tridiag_banded[1,:], k=0) +
            diag(tridiag_banded[0,1:], k=1) +
            diag(tridiag_banded[2,:-1], k=-1))

def compute_source(X, Xb, prescribed_flux=None, prescribed_source=None,
                   W=None, Wb=None):
    '''Return the source array S consisting of the convergence of the prescribed flux
    plus the prescribed scalar source.'''
    if (W is None):
        W = ones_like(X)
    if (Wb is None):
        Wb = ones_like(Xb)
    if prescribed_flux is None:
        prescribed_flux = zeros_like(Xb)
    if prescribed_source is None:
        prescribed_source = zeros_like(X)
    F = prescribed_flux
    return ((-Wb[...,1:]*F[...,1:]+Wb[...,:-1]*F[...,:-1]) /
            (W*(Xb[...,1:]-Xb[...,:-1])) + prescribed_source)

def compute_tendency(field, tridiag, source, use_banded_solver=False):
    r'''Return the instantaneous scalar tendency.

    This is the sum of the convergence of advective+diffusive flux plus any
    prescribed convergence or scalar sources.

    The convergence is computed by matrix multiplication:

    .. math::

        \frac{\partial \psi}{\partial t} = T \times \psi + S

    where :math:`T` is the tridiagonal flux convergence matrix.
    '''
    if use_banded_solver:
        tridiag = make_the_actual_tridiagonal_matrix(tridiag)
    #  np.matmul expects the final 2 dims of each array to be matrices
    #  add a singleton dimension to field so we get (J,J)x(J,1)->(J,1)
    result = matmul(tridiag, field[...,newaxis]) + source[...,newaxis]
    #  Now strip the extra dim
    return result[...,0]

def implicit_step_forward(initial_field, tridiag, source, timestep,
                          use_banded_solver=False):
    r'''Return the field at future time using an implicit timestep.

    The matrix problem is

    .. math::

        (I - T \Delta t) \psi^{n+1} = \psi^n + S \Delta t

    where :math:`T` is the tridiagonal matrix for the flux convergence, :math:`psi` is the
    state variable, the superscript :math:`n` refers to the time index, and :math:`S \Delta t`
    is the accumulated source over the timestep :math:`\Delta t`.

    Input arguments:

    - ``initial_field``: the current state variable :math:`\psi^n`, dimensions (...,J)
    - ``tridiag``: the tridiagonal matrix :math:`T`, dimensions (...,J,J) or (...,3,J) depending on the value of ``use_banded_solver``
    - ``source``: prescribed sources/sinks of :math:`\psi`, dimensions (...,J)
    - ``timestep``: the discrete timestep in time units
    - ``use_banded_solver``: switch to use the optional efficient banded solver (see below)

    Returns the updated value of the state variable :math:`\psi^{n+1}`, dimensions (...,J)

    The expected shape of ``tridiag`` depends on the switch ``use_banded_solver``,
    which should be consistent with that used in the call to ``advdiff_tridiag()``.
    If ``True``, we use the efficient banded matrix solver
    ``scipy.linalg.solve_banded()``.
    However this will probably only work for a 1D state variable.

    The default is to use the general linear system solver ``numpy.linalg.solve()``.
    '''
    RHS = initial_field + source*timestep
    I = 0.*tridiag
    J = initial_field.shape[-1]
    if use_banded_solver:
        I[1,:] = 1.  # identity matrix in banded form
        IminusTdt = I-tridiag*timestep
        return solve_banded((1, 1), IminusTdt, RHS)
    else:
        #  indices for main, upper, and lower diagonals of a JxJ matrix
        inds_main = diag_indices(J)
        I = 0.*tridiag
        I[...,inds_main[0],inds_main[1]] = 1.  # stacked identity matrix
        IminusTdt = I-tridiag*timestep
        return solve(IminusTdt, RHS)
