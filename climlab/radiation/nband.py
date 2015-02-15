import numpy as np
from climlab import constants as const
from climlab.radiation.transmissivity import Transmissivity
from climlab.process.energy_budget import EnergyBudget

# should revise this
# make the class strictly an atmospheric radiative transfer
# and couple it to a surface radiation class


class NbandModel(EnergyBudget):
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
        flux = {}  # fluxes in W / m**2
        flux_down = self._trans.flux_down(fromspace, self.diagnostics['emit_atm'])
        flux['incident_sfc'] = flux_down[0]        
        #F = self._trans.flux_compute(fromspace, albedo_sfc, 
        #                                self.diagnostics['emit_sfc'], 
        #                                self.diagnostics['emit_atm'])
        flux_up = self._trans.flux_up(self.diagnostics['emit_sfc'] + 
                                      albedo_sfc*flux['incident_sfc'],
                                      self.diagnostics['emit_atm'])
        flux_net = flux_up - flux_down
        
        absorbed = {}  # absorbed radiation (flux convergence) in W / m**2
        absorbed['atm'] = -np.diff(flux_net)
        absorbed['sfc'] = -flux_net[0]
        absorbed['total'] = absorbed['sfc'] + np.sum(absorbed['atm'])
        self.absorbed = absorbed
        #  These are mostly just placeholders at the moment
        N = self._trans.N
        flux['space2sfc'] = 0.
        flux['space2atm'] = np.zeros(N)
        flux['atm2sfc'] = np.zeros(N)
        flux['atm2atm'] = np.zeros(N)
        flux['incident_sfc'] = 0.
        flux['up_sfc'] = 0.
        flux['sfc2atm'] = np.zeros(N)
        flux['sfc2space'] = 0.
        flux['atm2space'] = np.zeros(N)
        flux['up2space'] = flux_net[N]
        flux['net2sfc'] = -flux_net[0]

        self.flux = flux
        self.diagnostics['absorbed_sfc'] = absorbed['sfc']
        self.diagnostics['absorbed_atm'] = absorbed['atm']
        self.heating_rate['Ts'] = absorbed['sfc']
        self.heating_rate['Tatm'] = absorbed['atm']

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
        self.radiative_heating()        

