'''Tests for the 1D advection-diffusion solver against an analytical benchmark.'''
from __future__ import division
import numpy as np
import pytest
from climlab.dynamics import adv_diff_numerics

L = 1E7  # meters
J = 50
x0 = 0.
xJ = L
timestep = 3600.*24.
Psi0 = 1.  # some psi units
U0 = 10.  # m/s
Uscale_mixing = 10.  # m/s
Lscale_mixing = 1E5  # meters
Kconst = Uscale_mixing * Lscale_mixing
offset = 2E6

def flux_analytical(Xb, Psi0, U0, Kconst, L):
    return Psi0*np.sin(np.pi*Xb/L)*(U0*np.sin(np.pi*Xb/L)**2-2*Kconst*np.pi/L*np.cos(np.pi*Xb/L))

def tendency_analytical(X, Psi0, U0, Kconst, L):
    return (-Psi0*np.pi/L*(3*U0*np.sin(np.pi*X/L)**2*np.cos(np.pi*X/L)
                - 2*Kconst*np.pi/L*(np.cos(np.pi*X/L)**2-np.sin(np.pi*X/L)**2)))

@pytest.mark.fast
def test_uniform_grid():
    Xb = np.linspace(x0, xJ, J+1)
    X = np.linspace(x0+(Xb[1]-Xb[0])/2, xJ - (Xb[-1]-Xb[-2])/2, J)
    prescribed_flux = np.zeros_like(Xb)
    prescribed_source = np.zeros_like(X)
    Wb = np.ones_like(Xb)
    W = np.ones_like(X)
    Psi = Psi0 * (np.sin(np.pi*X/L))**2
    U = U0 * np.sin(np.pi*Xb/L)
    K = np.ones_like(Xb) * Kconst
    source = adv_diff_numerics.compute_source(X,Xb,prescribed_flux,prescribed_source,W,Wb)
    tridiag = adv_diff_numerics.advdiff_tridiag(X, Xb, K, U, W, Wb, use_banded_solver=True)
    F_analytical = flux_analytical(Xb, Psi0, U0, Kconst, L)
    F_numerical = adv_diff_numerics.total_flux(X, Xb, K, U, Psi, prescribed_flux)
    dPsidt_analytical = tendency_analytical(X, Psi0, U0, Kconst, L)
    dPsidt_numerical = adv_diff_numerics.compute_tendency(Psi, tridiag, source, use_banded_solver=True)

    assert F_numerical == pytest.approx(F_analytical, abs=0.01, rel=0.001)
    assert dPsidt_numerical == pytest.approx(dPsidt_analytical, abs=0.01, rel=0.001)
    adv_diff_numerics.implicit_step_forward(Psi, tridiag, source, timestep, use_banded_solver=True)

@pytest.mark.fast
def test_nonuniform_grid():
    Xb = np.geomspace(x0+offset, xJ+offset, J+1)-offset
    Xb[0] = 0.; Xb[-1] = xJ
    X = Xb[:-1] + (Xb[1:]-Xb[:-1])/2
    prescribed_flux = np.zeros_like(Xb)
    prescribed_source = np.zeros_like(X)
    Wb = np.ones_like(Xb)
    W = np.ones_like(X)
    Psi = Psi0 * (np.sin(np.pi*X/L))**2
    U = U0 * np.sin(np.pi*Xb/L)
    K = np.ones_like(Xb) * Kconst
    source = adv_diff_numerics.compute_source(X,Xb,prescribed_flux,prescribed_source,W,Wb)
    tridiag = adv_diff_numerics.advdiff_tridiag(X, Xb, K, U, W, Wb, use_banded_solver=False)
    F_analytical = flux_analytical(Xb, Psi0, U0, Kconst, L)
    F_numerical = adv_diff_numerics.total_flux(X, Xb, K, U, Psi, prescribed_flux)
    dPsidt_analytical = tendency_analytical(X, Psi0, U0, Kconst, L)
    dPsidt_numerical = adv_diff_numerics.compute_tendency(Psi, tridiag, source, use_banded_solver=False)
    assert F_numerical == pytest.approx(F_analytical, abs=0.01, rel=0.01)
    assert dPsidt_numerical == pytest.approx(dPsidt_analytical, abs=0.01, rel=0.01)
    adv_diff_numerics.implicit_step_forward(Psi, tridiag, source, timestep, use_banded_solver=False)

@pytest.mark.fast
def test_nonuniform_multidim():
    P=7; N = 4; M = 5  # arbitrary
    X = np.zeros((P,N,M,J))
    Xb = np.zeros((P,N,M,J+1))
    for p in range(P):
        for n in range(N):
            for m in range(M):
                Xb[p,n,m,:] = np.geomspace(x0+offset, xJ+offset, J+1)-offset
                Xb[p,n,m,0] = 0.; Xb[p,n,m,-1] = xJ
                X[p,n,m,:] = Xb[p,n,m,:-1] + (Xb[p,n,m,1:]-Xb[p,n,m,:-1])/2
    Psi = Psi0 * (np.sin(np.pi*X/L))**2
    K = np.ones_like(Xb) * Kconst
    U = U0 * np.sin(np.pi*Xb/L)
    prescribed_flux = np.zeros_like(U)
    prescribed_source = np.zeros_like(Psi)
    source = adv_diff_numerics.compute_source(X,Xb)
    tridiag = adv_diff_numerics.advdiff_tridiag(X, Xb, K, U)
    F_analytical = flux_analytical(Xb, Psi0, U0, Kconst, L)
    F_numerical = adv_diff_numerics.total_flux(X, Xb, K, U, Psi)
    dPsidt_analytical = tendency_analytical(X, Psi0, U0, Kconst, L)
    dPsidt_numerical = adv_diff_numerics.compute_tendency(Psi, tridiag, source)
    assert F_numerical == pytest.approx(F_analytical, abs=0.01, rel=0.01)
    assert dPsidt_numerical == pytest.approx(dPsidt_analytical, abs=0.01, rel=0.01)
    adv_diff_numerics.implicit_step_forward(Psi, tridiag, source, timestep, use_banded_solver=False)
