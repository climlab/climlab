import numpy as np


class Transmissivity(object):
    '''Calculate and store transmissivity matrices between each level.
    Input: 1-dimensional numpy array of absorptivities for each level.
    Attributes: (all stored as numpy arrays):
        N: number of levels
        absorptivity: level absorptivity (N)
        trans: level transmissivity (N)
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
        self.trans = 1 - absorptivity
        N = self.absorptivity.size
        self.N = N
        # fully vectorized version
        tau = np.concatenate((np.atleast_1d(1.), self.trans))
        B = np.tile(tau, (N+1,1)).transpose()    
        A = np.tril(B,k=-1) + np.tri(N+1).transpose()
        self.Tup = np.tril(np.cumprod(A, axis=0))
        self.Tdown = np.transpose(self.Tup)
        

#
#class Transmissivity(object):
#    '''Calculate and store transmissivity matrices between each level.
#    Input: 1-dimensional numpy array of absorptivities for each level.
#    Attributes: (all stored as numpy arrays):
#        absorptivity: level absorptivity (N)
#        trans: level transmissivity (N)
#        sfc2atm: transmissivity between surface and each level (N)
#        sfc2space: transmissivity between surface and space (TOA) (1)
#        atm2space: transmissivity between each level and space (N)
#        atm2atm: transmissivity matrix between atmospheric levels (N,N)
#    '''
#    def __init__(self, absorptivity):
#        if absorptivity.ndim is not 1:
#            raise ValueError('absorptivity argument must be a vector')
#        self.absorptivity = absorptivity
#        self.trans = 1 - absorptivity
#        N = self.absorptivity.size
#        # a matrix containing the transmission between atmospheric layers
#        #  multiply this matrix by vector of emissions to get the
#        # total incident beam at each layer.
#        self.atm2atm = np.diag(np.ones(N-1), 1)
#        for n in range(N):
#            self.atm2atm[n, n+2:N] = np.cumprod(self.trans[n+1:N-1])
#        self.atm2atm += self.atm2atm.transpose()
#        # the transmissivity between surface and layer k
#        self.sfc2atm = np.concatenate(([1.], np.cumprod(self.trans[:N-1])))
#        # the transmissivity between layer k and space
#        self.atm2space = np.flipud(np.cumprod(np.concatenate(([1.],
#                                    np.flipud(self.trans[1:N])))))
#        #  the transmissivity between surface and space
#        self.sfc2space = np.prod(self.trans)
