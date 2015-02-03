import numpy as np


def set_transmissivity(absorb):
    '''Calculate transmissivity matrices between each level.
    Input: 1-dimensional numpy array of absorbptivities for each level.

    Returns a dictionary of numpy arrays:
        absorb: level absorptivity (N)
        trans: level transmissivity (N)
        sfc2atm: transmissivity between surface and each level (N)
        sfc2space: transmissivity between surface and space (TOA) (1)
        atm2space: transmissivity between each level and space (N)
        atm2atm: transmissivity matrix between atmospheric levels (N,N)
    '''
    if absorb.ndim is not 1:
        raise ValueError('absorb argument must be a vector')
    trans = 1 - absorb
    N = absorb.size
    # a matrix containing the transmission between atmospheric layers
    #  multiply this matrix by vector of emissions to get the
    # total incident beam at each layer.
    atm2atm = np.diag(np.ones(N-1), 1)
    for n in range(N):
        atm2atm[n, n+2:N] = np.cumprod(trans[n+1:N-1])
    atm2atm += atm2atm.transpose()
    # the transmissivity between surface and layer k
    sfc2atm = np.concatenate(([1.], np.cumprod(trans[:N-1])))
    # the transmissivity between layer k and space
    atm2space = np.flipud(np.cumprod(np.concatenate(([1.],
                          np.flipud(trans[1:N])))))
    #  the transmissivity between surface and space
    sfc2space = np.prod(trans)
    # Assemble all fields in a dictionary
    transdict = {'absorb': absorb, 'trans': trans, 'atm2atm': atm2atm,
                 'sfc2atm': sfc2atm, 'atm2space': atm2space,
                 'sfc2space': sfc2space}
    return transdict
