import numpy as np


class Transmissivity(object):
    '''Calculate and store transmissivity matrices between each level.
    Input: 1-dimensional numpy array of absorptivities for each level.
    Attributes: (all stored as numpy arrays):
        absorb: level absorptivity (N)
        trans: level transmissivity (N)
        sfc2atm: transmissivity between surface and each level (N)
        sfc2space: transmissivity between surface and space (TOA) (1)
        atm2space: transmissivity between each level and space (N)
        atm2atm: transmissivity matrix between atmospheric levels (N,N)
    '''
    def __init__(self, absorb):
        if absorb.ndim is not 1:
            raise ValueError('absorb argument must be a vector')
        self.absorb = absorb
        self.trans = 1 - self.absorb
        N = absorb.size
        # a matrix containing the transmission between atmospheric layers
        #  multiply this matrix by vector of emissions to get the
        # total incident beam at each layer.
        self.atm2atm = np.diag(np.ones(N-1), 1)
        for n in range(N):
            self.atm2atm[n, n+2:N] = np.cumprod(self.trans[n+1:N-1])
        self.atm2atm += self.atm2atm.transpose()
        # the transmissivity between surface and layer k
        self.sfc2atm = np.concatenate(([1.], np.cumprod(self.trans[:N-1])))
        # the transmissivity between layer k and space
        self.atm2space = np.flipud(np.cumprod(np.concatenate(([1.],
                                   np.flipud(self.trans[1:N])))))
        #  the transmissivity between surface and space
        self.sfc2space = np.prod(self.trans)
