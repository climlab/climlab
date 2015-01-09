import numpy as np
from transmissivity import Transmissivity


# module to compute radiative transfer in a column
# output: flux convergence (absorbed energy) at surface and each atm box
# output: flux out at TOA
# maybe also some diagnostic quantities

def _flux(fromspace, albedo_sfc, emit_sfc, emit_atm, eps):
    trans = Transmissivity(eps)
    space2sfc = TOA_down * trans.sfc2space
    space2atm = TOA_down * trans.atm2space
    atm2sfc = np.dot(trans.sfc2atm, emit_atm)
    atm2atm = np.dot(trans.atm2atm, emit_atm)
    incident_sfc = space2sfc + atm2sfc
    up_sfc = albedo_sfc * incident_sfc + emit_sfc
    sfc2atm = up_sfc * trans.sfc2atm
    sfc2space = up_sfc * trans.sfc2space
    atm2space = emit_atm * trans.atm2space
    absorbed_sfc = incident_sfc - up_sfc
    absorbed_atm = (atm2atm + sfc2atm + space2atm) * trans.absorb - 2*emit_atm
    net2space = sfc2space + np.sum(atm2space)
    return absorbed_sfc, absorbed_atm, net2space, sfc2space, atm2space, up_sfc

def SWflux(Q, albedo_sfc, eps):
    emit_atm = np.zeros_like(eps)
    emit_sfc = np.zeros_like(albedo_sfc)
    absorbed_sfc, absorbed_atm, net2space, sfc2space, atm2space, up_sfc =  \
        _flux(fromspace=Q, albedo_sfc=albedo_sfc, emit_sfc, emit_atm, eps)
    planetary_albedo = net2space / Q
    absorbed_total = absorbed_sfc + np.sum(absorbed_atm)
    return absorbed_sfc, absorbed_atm, absorbed_total, planetary_albedo

def LWflux(emit_atm, emit_sfc, eps):
    albedo_sfc = np.zeros_like(emit_sfc)
    fromspace = np.zeros_like(emit_sfc)
    absorbed_sfc, absorbed_atm, net2space, sfc2space, atm2space, up_sfc =  \
        _flux(fromspace, albedo_sfc, emit_sfc, emit_atm, eps)
    OLR_sfc = sfc2space
    OLR_atm = atm2space
    OLR = net2space

#==============================================================================
#         self.SWdown_TOA = self.Q
#         SW_incident_fromabove = self.SWdown_TOA * self.SWtrans.atm2space
#         self.SWdown_sfc = self.SWdown_TOA * self.SWtrans.surf2space
#         self.SWup_sfc = self.albedo * self.SWdown_sfc
#         self.SW_absorbed_sfc = self.SWdown_sfc - self.SWup_sfc
#         SW_incident_frombelow = self.SWup_sfc * self.SWtrans.surf2atm
#         self.SW_absorbed_atm = ((SW_incident_fromabove + SW_incident_frombelow)
#                                 * self.SWtrans.absorb)
#         self.SWup_TOA = self.SWup_sfc * self.SWtrans.surf2space
#         self.SW_absorbed_total = (self.SW_absorbed_sfc +
#                                   np.sum(self.SW_absorbed_atm))
#         self.planetary_albedo = self.SWup_TOA / self.SWdown_TOA
# 
# 
# 
#         eps = self.LWtrans.absorb
#         # emissions from surface and each layer
#         self.emit_sfc = const.sigma * self.Ts**4.
#         self.emit_atm = eps * const.sigma * self.Tatm**4.
# 
#         self.LW_down_sfc = np.dot(self.LWtrans.surf2atm, self.emit_atm)
#         self.OLR_sfc = self.LWtrans.surf2space * self.emit_sfc
#         self.OLR_atm = self.LWtrans.atm2space * self.emit_atm
#         self.OLR = self.OLR_sfc + np.sum(self.OLR_atm)
#         self.LW_absorbed_sfc = self.LW_down_sfc - self.emit_sfc
# 
#         incident_fromsfc = self.emit_sfc * self.LWtrans.surf2atm
#         incident_fromatm = np.dot(self.LWtrans.atm2atm, self.emit_atm)
#         self.LW_absorbed_atm = ((incident_fromatm + incident_fromsfc) * eps
#==============================================================================
                                - 2 * self.emit_atm)

