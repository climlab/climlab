import numpy as np
from transmissivity import set_transmissivity

# module to compute radiative transfer in a column
# output: flux convergence (absorbed energy) at surface and each atm box
# output: flux out at TOA
# maybe also some diagnostic quantities

class _NbandFlux(object):
    def __init__(self, absorb):
        self.trans = set_transmissivity(absorb)
    def compute_flux(self,fromspace, albedo_sfc, emit_sfc, emit_atm):
        absorbed, flux = _NbandFluxCompute(fromspace, albedo_sfc, emit_sfc, emit_atm, self.trans)
        self.absorbed = absorbed
        self.flux = flux


def _NbandFluxCompute(fromspace, albedo_sfc, emit_sfc, emit_atm, trans):
    flux = {}  # fluxes in W / m**2
    absorbed = {}  # absorbed radiation (flux convergence) in W / m**2
    flux['space2sfc'] = fromspace * trans['sfc2space']
    flux['space2atm'] = fromspace * trans['atm2space']
    flux['atm2sfc'] = np.dot(trans['sfc2atm'], emit_atm)
    flux['atm2atm'] = np.dot(trans['atm2atm'], emit_atm)
    flux['incident_sfc'] = flux['space2sfc'] + np.sum(flux['atm2sfc'])
    flux['up_sfc'] = albedo_sfc * flux['incident_sfc'] + emit_sfc
    flux['sfc2atm'] = flux['up_sfc'] * trans['sfc2atm']
    flux['sfc2space'] = flux['up_sfc'] * trans['sfc2space']
    flux['atm2space'] = emit_atm * trans['atm2space']
    absorbed['sfc'] = flux['incident_sfc'] - flux['up_sfc']
    absorbed['atm'] = (flux['atm2atm'] + flux['sfc2atm'] + flux['space2atm']) * trans['absorb'] - 2*emit_atm
    absorbed['total'] = absorbed['sfc'] + np.sum(absorbed['atm'])
    flux['up2space'] = flux['sfc2space'] + np.sum(flux['atm2space'])
    flux['net2sfc'] = flux['incident_sfc'] - flux['up_sfc']
    return absorbed, flux

#==============================================================================
# 
# def set_transmissitivity(absorb):
#     '''Calculate transmissivity matrices between each level.
#     Input: 1-dimensional numpy array of absorbptivities for each level.
# 
#     Returns a dictionary of numpy arrays:
#         absorb: level absorptivity (N)
#         trans: level transmissivity (N)
#         sfc2atm: transmissivity between surface and each level (N)
#         sfc2space: transmissivity between surface and space (TOA) (1)
#         atm2space: transmissivity between each level and space (N)
#         atm2atm: transmissivity matrix between atmospheric levels (N,N)
#     '''
#     if absorb.ndim is not 1:
#         raise ValueError('absorb argument must be a vector')
#     trans = 1 - absorb
#     N = absorb.size
#     # a matrix containing the transmission between atmospheric layers
#     #  multiply this matrix by vector of emissions to get the
#     # total incident beam at each layer.
#     atm2atm = np.diag(np.ones(N-1), 1)
#     for n in range(N):
#         atm2atm[n, n+2:N] = np.cumprod(trans[n+1:N-1])
#     atm2atm += atm2atm.transpose()
#     # the transmissivity between surface and layer k
#     sfc2atm = np.concatenate(([1.], np.cumprod(trans[:N-1])))
#     # the transmissivity between layer k and space
#     atm2space = np.flipud(np.cumprod(np.concatenate(([1.],
#                           np.flipud(trans[1:N])))))
#     #  the transmissivity between surface and space
#     sfc2space = np.prod(trans)
#     # Assemble all fields in a dictionary
#     transdict = {'absorb': absorb, 'trans': trans, 'atm2atm': atm2atm,
#                  'sfc2atm': sfc2atm, 'atm2space': atm2space,
#                 'sfc2space': sfc2space}
#    return transdict
#==============================================================================

