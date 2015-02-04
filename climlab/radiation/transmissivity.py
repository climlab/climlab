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

    @property
    def absorb(self):
        return self._absorb
    @absorb.setter
    def absorb(self, value):
        self._absorb = value
        self._trans = 1 - value
        N = self._absorb.size
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

    @property
    def trans(self):
        return 1 - self.absorb
    @trans.setter
    def trans(self, value):
        self.absorb = 1 - value

#
#class NbandFlux(object):
#    def __init__(self, absorb):
#        self.absorb = absorb
#
#    def compute_flux(self,fromspace, albedo_sfc, emit_sfc, emit_atm):
#        absorbed, flux = NbandFluxCompute(fromspace, albedo_sfc, emit_sfc, emit_atm, self._trans)
#        self.absorbed = absorbed
#        self.flux = flux
#    
#    @property
#    def absorb(self):
#        return self._trans.absorb
#    @absorb.setter
#    def absorb(self, value):
#        self._trans = Transmissivity(value)
#    @property
#    def transmissivity(self):
#        return self._trans.trans
#    @transmissivity.setter
#    def transmissivity(self, value):
#        self.absorb = 1 - value
#

def NbandFluxCompute(fromspace, albedo_sfc, emit_sfc, emit_atm, trans):
    flux = {}  # fluxes in W / m**2
    absorbed = {}  # absorbed radiation (flux convergence) in W / m**2
    flux['space2sfc'] = fromspace * trans.sfc2space
    flux['space2atm'] = fromspace * trans.atm2space
    flux['atm2sfc'] = np.dot(trans.sfc2atm, emit_atm)
    flux['atm2atm'] = np.dot(trans.atm2atm, emit_atm)
    flux['incident_sfc'] = flux['space2sfc'] + np.sum(flux['atm2sfc'])
    flux['up_sfc'] = albedo_sfc * flux['incident_sfc'] + emit_sfc
    flux['sfc2atm'] = flux['up_sfc'] * trans.sfc2atm
    flux['sfc2space'] = flux['up_sfc'] * trans.sfc2space
    flux['atm2space'] = emit_atm * trans.atm2space
    absorbed['sfc'] = flux['incident_sfc'] - flux['up_sfc']
    absorbed['atm'] = ((flux['atm2atm'] + flux['sfc2atm'] + flux['space2atm'])
                       * trans.absorb - 2*emit_atm)
    absorbed['total'] = absorbed['sfc'] + np.sum(absorbed['atm'])
    flux['up2space'] = flux['sfc2space'] + np.sum(flux['atm2space'])
    flux['net2sfc'] = flux['incident_sfc'] - flux['up_sfc']
    return absorbed, flux
