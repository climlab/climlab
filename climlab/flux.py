import numpy as np


# module to compute radiative transfer in a column
# output: flux convergence (absorbed energy) at surface and each atm box
# output: flux out at TOA
# maybe also some diagnostic quantities

def _flux(fromspace, albedo_sfc, emit_sfc, emit_atm, trans):
    space2sfc = fromspace * trans['sfc2space']
    space2atm = fromspace * trans['atm2space']
    atm2sfc = np.dot(trans['sfc2atm'], emit_atm)
    atm2atm = np.dot(trans['atm2atm'], emit_atm)
    incident_sfc = space2sfc + np.sum(atm2sfc)
    up_sfc = albedo_sfc * incident_sfc + emit_sfc
    sfc2atm = up_sfc * trans['sfc2atm']
    sfc2space = up_sfc * trans['sfc2space']
    atm2space = emit_atm * trans['atm2space']
    absorbed_sfc = incident_sfc - up_sfc
    absorbed_atm = (atm2atm + sfc2atm + space2atm) * trans['absorb'] - 2*emit_atm
    absorbed_total = absorbed_sfc + np.sum(absorbed_atm)
    up2space = sfc2space + np.sum(atm2space)
    net2sfc = incident_sfc - up_sfc
    return absorbed_sfc, absorbed_atm, absorbed_total, up2space, net2sfc, \
        sfc2space, atm2space, incident_sfc, up_sfc


def SWflux(Q, albedo_sfc, trans):
    emit_atm = np.zeros_like(trans['absorb'])
    emit_sfc = np.zeros_like(albedo_sfc)
    absorbed_sfc, absorbed_atm, absorbed_total, up2space, net2sfc, sfc2space, atm2space, incident_sfc, up_sfc =  \
        _flux(Q, albedo_sfc, emit_sfc, emit_atm, trans)
    planetary_albedo = up2space / Q
    return absorbed_sfc, absorbed_atm, absorbed_total, incident_sfc, up2space, planetary_albedo


def LWflux(emit_atm, emit_sfc, trans):
    albedo_sfc = np.zeros_like(emit_sfc)
    fromspace = np.zeros_like(emit_sfc)
    absorbed_sfc, absorbed_atm, absorbed_total, up2space, net2sfc, sfc2space, atm2space, incident_sfc, up_sfc =  \
        _flux(fromspace, albedo_sfc, emit_sfc, emit_atm, trans)
    OLR_sfc = sfc2space
    OLR_atm = atm2space
    OLR = up2space
    LWdown_sfc = incident_sfc
    return absorbed_sfc, absorbed_atm, OLR, LWdown_sfc, OLR_sfc, OLR_atm
