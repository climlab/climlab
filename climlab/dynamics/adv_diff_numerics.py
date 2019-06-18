from __future__ import division
from numpy import zeros, ones, zeros_like, ones_like, matmul, diag, diag_indices, diff, newaxis
from numpy.linalg import solve
from scipy.linalg import solve_banded


#  In this module we assume that the diffusion always acts on the last axis

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
    '''Compute the tridiagonal matrix operator for the advective-diffusive
       flux convergence.

       Input arrays of length J+1:
         Xb, Wb, K, U
       Input arrays of length J:
         X, W

       The 0th and Jth (i.e. first and last) elements of Wb are ignored;
       assuming boundary condition is a prescribed flux

       The return value depends on input flag use_banded_solver

       If use_banded_solver=True, return a 3xJ array containing the elements of the tridiagonal.
       This version is restricted to 1D input arrays,
       but is suitable for use with the efficient banded solver.

       If use_banded_solver=False (which it must be for multidimensional input),
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
    '''Convert the (3xJ) array into full (JxJ) tridiagonal matrix form.'''
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
    """Return the field at future time using an implicit timestep.

    The matrix problem is

    $$ (I - T \Delta t) \psi^{n+1} = \psi^n + S \Delta t $$

    where $T$ is the tridiagonal matrix for the flux convergence.

    We use the banded matrix solver `scipy.linalg.solve_banded()`.
    """
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
