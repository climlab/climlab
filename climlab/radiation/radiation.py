import numpy as np
#from climlab import constants as const
from climlab import thermo
from climlab.radiation.transmissivity import Transmissivity
from climlab.process.energy_budget import EnergyBudget

#  Renamed from NbandModel to Radiation
#  Now strictly atmospheric process
#  must be coupled to a surface model

class Radiation(EnergyBudget):
    '''Base class for all band radiation models,
    including grey and semi-grey model.
    
    Input argument absorptivity is band absorptivity
    (should same size as grid).
    
    By default emissivity = absorptivity.
    Subclasses can override this is necessary (e.g. for shortwave model)'''
    def __init__(self, absorptivity=None, albedo_sfc=0, **kwargs):
        super(Radiation, self).__init__(**kwargs)
        self.absorptivity = absorptivity
        self.albedo_sfc = albedo_sfc
        self.flux_from_space = np.zeros_like(self.Ts)
        self.flux_to_sfc = np.zeros_like(self.Ts)
        self.flux_from_sfc = np.zeros_like(self.Ts)
        self.heating_rate['Ts'] = np.zeros_like(self.Ts)
        
    @property
    def absorptivity(self):
        return self.trans.absorptivity
    @absorptivity.setter
    def absorptivity(self, value):
        self.trans = Transmissivity(np.array(value))
    @property
    def emissivity(self):
        # This ensures that emissivity = absorptivity at all times
    #  needs to be overridden for shortwave classes
        return self.absorptivity
    @property
    def transmissivity(self):
        return self.trans.transmissivity
    @transmissivity.setter
    def transmissivity(self, value):
        self.absorptivity = 1 - value

#    def blackbody_emission(self):
#        #self.blackbody_emission_sfc = const.sigma * self.state['Ts']**4
#        #self.blackbody_emission_atm = const.sigma * self.state['Tatm']**4
#        return const.sigma * self.Tatm**4

    def emission(self):
        #self.blackbody_emission()
        #self.diagnostics['emit_sfc'] = (self.emissivity_sfc *
        #                                self.blackbody_emission_sfc)
        return self.emissivity * thermo.blackbody_emission(self.Tatm)
        #self.diagnostics['emit_atm'] = (self.emissivity_atm *
        #                                self.blackbody_emission_atm)

    def radiative_heating(self):
        emission = self.emission()
        self.diagnostics['emission'] = emission
        try:
            fromspace = self.flux_from_space

        except:
            fromspace = np.zeros_like(self.state['Ts'])
        
        #flux = {}  # fluxes in W / m**2
        self.flux_down = self.trans.flux_down(fromspace, emission)
        #flux['incident_sfc'] = flux_down[0]
        #F = self._trans.flux_compute(fromspace, albedo_sfc, 
        #                                self.diagnostics['emit_sfc'], 
        #                                self.diagnostics['emit_atm'])
        flux_up_bottom = self.flux_from_sfc + self.albedo_sfc*self.flux_down[0]
        self.flux_up = self.trans.flux_up(flux_up_bottom, emission)
        self.flux_net = self.flux_up - self.flux_down
        # absorbed radiation (flux convergence) in W / m**2
        self.absorbed = -np.diff(self.flux_net)
        self.absorbed_total = np.sum(self.absorbed)
        self.diagnostics['absorbed_atm'] = self.absorbed
        self.heating_rate['Tatm'] = self.absorbed
        self.flux_to_sfc = self.flux_down[0]
        #absorbed = {}  # absorbed radiation (flux convergence) in W / m**2
        #absorbed['atm'] = -np.diff(flux_net)
        #absorbed['sfc'] = -flux_net[0]
        #absorbed['total'] = absorbed['sfc'] + np.sum(absorbed['atm'])
        #self.absorbed = absorbed
        #  These are mostly just placeholders at the moment
        #N = self._trans.N
        #flux['space2sfc'] = 0.
        #flux['space2atm'] = np.zeros(N)
        #flux['atm2sfc'] = np.zeros(N)
        #flux['atm2atm'] = np.zeros(N)
        #flux['incident_sfc'] = 0.
        #flux['up_sfc'] = 0.
        #flux['sfc2atm'] = np.zeros(N)
        #flux['sfc2space'] = 0.
        #flux['atm2space'] = np.zeros(N)
        #flux['up2space'] = flux_net[N]
        #flux['net2sfc'] = -flux_net[0]

        #self.flux = flux
        #self.diagnostics['absorbed_sfc'] = absorbed['sfc']
        #self.diagnostics['absorbed_atm'] = absorbed['atm']
        #self.heating_rate['Ts'] = absorbed['sfc']
        #self.heating_rate['Tatm'] = absorbed['atm']

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.'''
        self.radiative_heating()        

