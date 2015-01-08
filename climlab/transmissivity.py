# moving Transmissivity to its own module
import numpy as np


class Transmissivity:
    '''Need to write something here.'''
    def __init__(self, absorb):
        if absorb.ndim is not 1:
            raise ValueError('absorb argument must be a vector')
        self.absorb = absorb
        self.trans = 1 - self.absorb
        N = self.absorb.size
        # a matrix containing the transmission between atmospheric layers
        #  multiply this matrix by vector of emissions to get the
        # total incident beam at each layer.
        self.atm2atm = np.diag(np.ones(N-1), 1)
        for n in range(N):
            self.atm2atm[n, n+2:N] = np.cumprod(self.trans[n+1:N-1])
        self.atm2atm += self.atm2atm.transpose()

        # the transmissivity between surface and layer k
        self.surf2atm = np.concatenate(([1.], np.cumprod(self.trans[:N-1])))
        # the transmissivity between layer k and space
        self.atm2space = np.flipud(np.cumprod(np.concatenate(([1.],
                                              np.flipud(self.trans[1:N])))))
        #  the transmissivity between surface and space
        self.surf2space = np.prod(self.trans)
