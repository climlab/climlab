import numpy as np


# module to compute radiative transfer in a column
# output: flux convergence (absorbed energy) at surface and each atm box
# output: flux out at TOA
# maybe also some diagnostic quantities

def _NbandFlux(fromspace, albedo_sfc, emit_sfc, emit_atm, trans):
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
# 
# def SWflux(Q, albedo_sfc, trans):
#     emit_atm = np.zeros_like(trans['absorb'])
#     emit_sfc = np.zeros_like(albedo_sfc)
#     absorbed_sfc, absorbed_atm, absorbed_total, up2space, net2sfc, sfc2space, atm2space, incident_sfc, up_sfc =  \
#         _NbandFlux(Q, albedo_sfc, emit_sfc, emit_atm, trans)
#     planetary_albedo = up2space / Q
#     return absorbed_sfc, absorbed_atm, absorbed_total, incident_sfc, up2space, planetary_albedo
# 
# 
# def LWflux(emit_atm, emit_sfc, trans):
#     albedo_sfc = np.zeros_like(emit_sfc)
#     fromspace = np.zeros_like(emit_sfc)
#     absorbed_sfc, absorbed_atm, absorbed_total, up2space, net2sfc, sfc2space, atm2space, incident_sfc, up_sfc =  \
#         _NbandFlux(fromspace, albedo_sfc, emit_sfc, emit_atm, trans)
#     OLR_sfc = sfc2space
#     OLR_atm = atm2space
#     OLR = up2space
#     LWdown_sfc = incident_sfc
#     return absorbed_sfc, absorbed_atm, OLR, LWdown_sfc, OLR_sfc, OLR_atm
#==============================================================================
