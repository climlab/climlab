import numpy as np
from climlab import constants as const
from climlab.radiation.transmissivity import Transmissivity#, NbandFluxCompute
from climlab.radiation.radiation import _Radiation


# should revise this
# make the class strictly an atmospheric radiative transfer
# and couple it to a surface radiation class


class NbandModel(_Radiation):
    '''Parent class for all band radiation models,
    including grey and semi-grey model.
    
    Input argument absorptivity is band absorptivity
    (should same size as grid).
    
    By default emissivity = absorptivity.
    Subclasses can override this is necessary (e.g. for shortwave model)'''
    def __init__(self, absorptivity=None, albedo_sfc=0, **kwargs):
        super(NbandModel, self).__init__(**kwargs)
        self.absorptivity = absorptivity
        self.emissivity_sfc = np.zeros_like(self.state['Ts'])
        # self.emissivity_atm is set by setting self.absorptivity
        self.albedo_sfc = albedo_sfc

    @property
    def absorptivity(self):
        return self._trans.absorptivity
    @absorptivity.setter
    def absorptivity(self, value):
        self._trans = Transmissivity(np.array(value))
    @property
    def emissivity_atm(self):
        # This ensures that emissivity = absorptivity at all times
    #  needs to be overridden for shortwave classes
        return self.absorptivity
    @property
    def transmissivity(self):
        return self._trans.trans
    @transmissivity.setter
    def transmissivity(self, value):
        self.absorptivity = 1 - value

    def blackbody_emission(self):
        self.blackbody_emission_sfc = const.sigma * self.state['Ts']**4
        self.blackbody_emission_atm = const.sigma * self.state['Tatm']**4

    def emission(self):
        self.blackbody_emission()
        self.diagnostics['emit_sfc'] = (self.emissivity_sfc *
                                        self.blackbody_emission_sfc)
        self.diagnostics['emit_atm'] = (self.emissivity_atm *
                                        self.blackbody_emission_atm)

    def radiative_heating(self):
        self.emission()
        try:
            fromspace = self.from_space

        except:
            fromspace = np.zeros_like(self.state['Ts'])
        albedo_sfc = self.albedo_sfc
        #absorbed, flux = flux_compute(fromspace, albedo_sfc, 
        #                                    self.diagnostics['emit_sfc'], 
        #                                    self.diagnostics['emit_atm'],
        #                                    self._trans)
        F = self._trans.flux_compute(fromspace, albedo_sfc, 
                                        self.diagnostics['emit_sfc'], 
                                        self.diagnostics['emit_atm'])
        absorbed = {}  # absorbed radiation (flux convergence) in W / m**2
        absorbed['atm'] = -np.diff(F)
        absorbed['sfc'] = -F[0]
        absorbed['total'] = absorbed['sfc'] + np.sum(absorbed['atm'])
        self.absorbed = absorbed
        #  These are mostly just placeholders at the moment
        N = self._trans.N
        flux = {}  # fluxes in W / m**2
        flux['space2sfc'] = 0.
        flux['space2atm'] = np.zeros(N)
        flux['atm2sfc'] = np.zeros(N)
        flux['atm2atm'] = np.zeros(N)
        flux['incident_sfc'] = 0.
        flux['up_sfc'] = 0.
        flux['sfc2atm'] = np.zeros(N)
        flux['sfc2space'] = 0.
        flux['atm2space'] = np.zeros(N)
        flux['up2space'] = F[N]
        flux['net2sfc'] = -F[0]

        self.flux = flux
        self.diagnostics['absorbed_sfc'] = absorbed['sfc']
        self.diagnostics['absorbed_atm'] = absorbed['atm']
        self.heating_rate['Ts'] = absorbed['sfc']
        self.heating_rate['Tatm'] = absorbed['atm']


def flux_compute(fromspace, albedo_sfc, emit_sfc, emit_atm, trans):
    flux = {}  # fluxes in W / m**2
    absorbed = {}  # absorbed radiation (flux convergence) in W / m**2
    #trans = 1-absorptivity
    N = trans.N
    # it's convenient to define a N+2 vector of level emissions, including
    # the surface and outer space
    E = np.concatenate((np.atleast_1d(emit_sfc), emit_atm, np.atleast_1d(fromspace)))
        
    # fully vectorized version
    # downwelling beam
    D = np.dot(trans.Tdown, E[1:])
    #  add in the reflected part at the surface
    E[0] += albedo_sfc * D[0]
    U = np.dot(trans.Tup, E[:N+1])

    # total upwelling flux
    F = U - D
    # absorbed radiation is just flux convergence
    absorbed['atm'] = -np.diff(F)
    absorbed['sfc'] = -F[0]
    absorbed['total'] = absorbed['sfc'] + np.sum(absorbed['atm'])
    
    
    #  These are mostly just placeholders at the moment
    flux['space2sfc'] = 0.
    flux['space2atm'] = np.zeros(N)
    flux['atm2sfc'] = np.zeros(N)
    flux['atm2atm'] = np.zeros(N)
    flux['incident_sfc'] = 0.
    flux['up_sfc'] = 0.
    flux['sfc2atm'] = np.zeros(N)
    flux['sfc2space'] = 0.
    flux['atm2space'] = np.zeros(N)
    flux['up2space'] = F[N]
    flux['net2sfc'] = -F[0]
    return absorbed, flux


#
#def flux_compute(fromspace, albedo_sfc, emit_sfc, emit_atm, trans):
#    flux = {}  # fluxes in W / m**2
#    absorbed = {}  # absorbed radiation (flux convergence) in W / m**2
#    flux['space2sfc'] = fromspace * trans.sfc2space
#    flux['space2atm'] = fromspace * trans.atm2space
#    #flux['atm2sfc'] = np.dot(trans.sfc2atm, emit_atm)
#    # I think that was a bug... this should give a vector
#    flux['atm2sfc'] = trans.sfc2atm * emit_atm
#    flux['atm2atm'] = np.dot(trans.atm2atm, emit_atm)
#    flux['incident_sfc'] = flux['space2sfc'] + np.sum(flux['atm2sfc'])
#    flux['up_sfc'] = albedo_sfc * flux['incident_sfc'] + emit_sfc
#    flux['sfc2atm'] = flux['up_sfc'] * trans.sfc2atm
#    flux['sfc2space'] = flux['up_sfc'] * trans.sfc2space
#    flux['atm2space'] = emit_atm * trans.atm2space
#    absorbed['sfc'] = flux['incident_sfc'] - flux['up_sfc']
#    absorbed['atm'] = ((flux['atm2atm'] + flux['sfc2atm'] + flux['space2atm'])
#                       * trans.absorptivity - 2*emit_atm)
#    absorbed['total'] = absorbed['sfc'] + np.sum(absorbed['atm'])
#    flux['up2space'] = flux['sfc2space'] + np.sum(flux['atm2space'])
#    flux['net2sfc'] = flux['incident_sfc'] - flux['up_sfc']
#    return absorbed, flux
