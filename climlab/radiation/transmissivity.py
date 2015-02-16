import numpy as np


class Transmissivity(object):
    '''Class for calculating and store transmissivity between levels, 
    and computing radiative fluxes between levels.
    
    Input: 1-dimensional numpy array of absorptivities for each level.
    Attributes: (all stored as numpy arrays):
        N: number of levels
        absorptivity: level absorptivity (N)
        transmissivity: level transmissivity (N)
        Tup: transmissivity matrix for upwelling beam (N+1, N+1)
        Tdown: transmissivity matrix for downwelling beam (N+1, N+1)

    Example for N = 3 atmospheric layers:

    tau is a vector of transmissivities
        tau = [1, tau0, tau1, tau2]    
    A is a matrix
        A = [[   1,    1,    1,    1],
             [tau0,    1,    1,    1],
             [tau1, tau1,    1,    1],
             [tau2, tau2, tau2,    1]]
    We then take the cumulative product along columns,
        and finally take the lower triangle of the result to get
    Tup = [[             1,         0,    0,  0],
           [          tau0,         1,    0,  0],
           [     tau1*tau0,      tau0,    1,  0],
           [tau2*tau1*tau0, tau2*tau1, tau2,  1]]
           
    and Tdown = transpose(Tup)
    
    Construct an emission vector for the downwelling beam:
        Edown = [E0, E1, E2, fromspace]
    Now we can get the downwelling beam by matrix multiplication:
        D = Tdown * Edown
    
    For the upwelling beam, we start by adding the reflected part 
    at the surface to the surface emissions:
        Eup = [emit_sfc + albedo_sfc*D[0], E0, E1, E2]
    So that the upwelling flux is 
        U = Tup * Eup
    The total flux, positive up is thus
        F = U - D
    The absorbed radiation at the surface is then -F[0]
    The absorbed radiation in the atmosphere is the flux convergence:
        -diff(F)
    '''
    def __init__(self, absorptivity):
        if absorptivity.ndim is not 1:
            raise ValueError('absorptivity argument must be a vector')
        self.absorptivity = absorptivity
        self.transmissivity = 1 - absorptivity
        N = self.absorptivity.size
        self.N = N
        # fully vectorized version
        tau = np.concatenate((np.atleast_1d(1.), self.transmissivity))
        B = np.tile(tau, (N+1,1)).transpose()    
        A = np.tril(B,k=-1) + np.tri(N+1).transpose()
        self.Tup = np.tril(np.cumprod(A, axis=0))
        self.Tdown = np.transpose(self.Tup)

    def flux_down(self, fluxDownTop, emission=None):
        '''Compute downwelling radiative flux at interfaces between layers.
        
        Inputs:
            fluxDownTop: flux down at top
            emission: emission from atmospheric levels (N)
                defaults to zero if not given
        Returns:
            vector of downwelling radiative flux between levels (N+1)
            element 0 is the flux down to the surface.'''
        if emission is None:
            emission = np.zeros_like(self.absorptivity)
        E = np.append(emission, fluxDownTop)
        return np.dot(self.Tdown, E)

    def flux_up(self, fluxUpBottom, emission=None):
        '''Compute upwelling radiative flux at interfaces between layers.
        
        Inputs:
            fluxUpBottom: flux up from bottom
            emission: emission from atmospheric levels (N)
                defaults to zero if not given
        Returns:
            vector of downwelling radiative flux between levels (N+1)
            element N is the flux up to space.'''        
        if emission is None:
            emission = np.zeros_like(self.absorptivity)
        E = np.flipud(np.append(np.flipud(emission), fluxUpBottom))
        return np.dot(self.Tup, E)
    
