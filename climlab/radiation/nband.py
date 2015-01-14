import numpy as np
import climlab.utils.constants as const
import climlab.radiation.nbandflux as nbandflux
from climlab.radiation.radiation import _Radiation


class NbandModel(_Radiation):
    '''Parent class for all band radiation models,
    including grey and semi-grey model.
    
    Input argument eps is band absorptivity
    (should same size as grid).'''
    def __init__(self, eps=None, albedo_sfc=0, **kwargs):
        super(NbandModel, self).__init__(**kwargs)
        self.set_absorptivity(eps)
        self.set_emissivity(np.zeros_like(self.state['Ts']), np.zeros_like(self.state['Tatm']))
        self.albedo_sfc = albedo_sfc

    def set_absorptivity(self, eps):
        '''Set or change the band absorptivity. 
        Input: eps
        must be same size as grid.'''
        self.trans = nbandflux.set_transmissitivity(eps)
        
    def set_emissivity(self, emissivity_sfc, emissivity_atm):
        '''emissivity should be a fraction 0-1
        of blackbody emission in that band.'''
        self.diagnostics['emissivity_atm'] = emissivity_atm
        self.diagnostics['emissivity_sfc'] = emissivity_sfc
    
    def blackbody_emission(self):
        self.diagnostics['blackbody_emission_sfc'] = const.sigma * self.state['Ts']**4
        self.diagnostics['blackbody_emission_atm'] = const.sigma * self.state['Tatm']**4
    
    def emission(self):
        self.blackbody_emission()
        self.diagnostics['emit_sfc'] = (self.diagnostics['emissivity_sfc'] * 
                                    self.diagnostics['blackbody_emission_sfc'])
        self.diagnostics['emit_atm'] = (self.diagnostics['emissivity_atm'] * 
                                    self.diagnostics['blackbody_emission_atm'])
        
    def radiative_heating(self):
        self.emission()
        try:
            fromspace = self.flux_from_space
        except:
            fromspace = np.zeros_like(self.state['Ts'])
        albedo_sfc = self.albedo_sfc
        absorbed, flux = nbandflux._NbandFluxCompute(fromspace, albedo_sfc, 
                                            self.diagnostics['emit_sfc'], 
                                            self.diagnostics['emit_atm'], 
                                            self.trans)
        self.absorbed = absorbed
        self.flux = flux
        self.diagnostics['absorbed_sfc'] = absorbed['sfc']
        self.diagnostics['absorbed_atm'] = absorbed['atm']
        self.heating_rate['Ts'] = absorbed['sfc']
        self.heating_rate['Tatm'] = absorbed['atm']
#==============================================================================
# 
#     def radiative_tendencies(self):
#         """Compute net radiative heating everywhere in the Column (in W/m^2),
#         and the resulting temperature change over a specified timestep (in K).
#         """
#         # compute longwave heating rates
#         self.radiative_heating()
#         #  Need heat capacities (J/m**2/K) to compute temperature tendencies from fluxes
#         c_atm = heat_capacity.atmosphere(self.grid['lev'].delta)
#         c_sfc = heat_capacity.slab_ocean(self.param['water_depth'])
# 
#         self.tendencies['Ts'] = (self.diagnostics['absorbed_sfc'] *
#                                  self.param['timestep'] / c_sfc)
#         self.tendencies['Tatm'] = (self.diagnostics['absorbed_atm'] *
#                                     self.param['timestep'] / c_atm)
#==============================================================================
