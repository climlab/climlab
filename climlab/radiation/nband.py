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
        # This sets self.emissivity_atm to point to self.absorptivity
        #  gets automatically updated if absorptivity changes
        self.emissivity_atm = self.absorptivity
        self.albedo_sfc = albedo_sfc

    @property
    def absorptivity(self):
        return self._trans.absorptivity
    @absorptivity.setter
    def absorptivity(self, value):
        self._trans = Transmissivity(value)
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
        absorbed, flux = flux_compute(fromspace, albedo_sfc, 
                                            self.diagnostics['emit_sfc'], 
                                            self.diagnostics['emit_atm'],
                                            self._trans)
        self.absorbed = absorbed
        self.flux = flux
        self.diagnostics['absorbed_sfc'] = absorbed['sfc']
        self.diagnostics['absorbed_atm'] = absorbed['atm']
        self.heating_rate['Ts'] = absorbed['sfc']
        self.heating_rate['Tatm'] = absorbed['atm']


def flux_compute(fromspace, albedo_sfc, emit_sfc, emit_atm, trans):
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
                       * trans.absorptivity - 2*emit_atm)
    absorbed['total'] = absorbed['sfc'] + np.sum(absorbed['atm'])
    flux['up2space'] = flux['sfc2space'] + np.sum(flux['atm2space'])
    flux['net2sfc'] = flux['incident_sfc'] - flux['up_sfc']
    return absorbed, flux
