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
