import numpy as np
import constants as const
from timestepping_model import _TimeSteppingModel
import heat_capacity
from transmissivity import set_transmissitivity
from flux import _NbandFlux


#==============================================================================
# def AplusBT(T, param):
#     tendencies = {}
#     diagnostics = {}
#     A = param['A']
#     B = param['B']
#     OLR = A + B*T
#     tendencies['Ts'] = -OLR / heat_capacity.slab_ocean(param['water_depth'])
#     diagnostics['OLR'] = OLR
#==============================================================================


class _Radiation(_TimeSteppingModel):
    '''Abstract parent class for all radiation modules.'''
    def __init__(self, **kwargs):
        super(_Radiation, self).__init__(**kwargs)
        #  heat capacity of atmospheric layers
        self.c_atm = heat_capacity.atmosphere(self.grid['lev'].delta)
        #  heat capacity of surface in J / m**2 / K
        self.c_sfc = heat_capacity.slab_ocean(self.param['water_depth'])
        self.process_type = 'explicit'
    
    def emission(self):
        '''No emission unless overwritten by daughter class.'''
        emit_sfc = np.zeros_like(self.state['Ts'])
        emit_atm = np.zeros_like(self.state['Tatm'])
        self.diagnostics['emit_sfc'] = emit_sfc
        self.diagnostics['emit_atm'] = emit_atm

    def radiative_tendencies(self):
        pass

    def compute(self):
        self.radiative_tendencies()


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
        self.trans = set_transmissitivity(eps)
        
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
        self.diagnostics['emit_sfc'] = self.diagnostics['emissivity_sfc'] * self.diagnostics['blackbody_emission_sfc']
        self.diagnostics['emit_atm'] = self.diagnostics['emissivity_atm'] * self.diagnostics['blackbody_emission_atm']
        
    def radiative_heating(self):
        self.emission()
        try:
            fromspace = self.flux_from_space
        except:
            fromspace = np.zeros_like(self.state['Ts'])
        albedo_sfc = self.albedo_sfc
        absorbed, flux = _NbandFlux(fromspace, albedo_sfc, self.diagnostics['emit_sfc'], self.diagnostics['emit_atm'], self.trans)
        self.absorbed = absorbed
        self.flux = flux
        self.diagnostics['absorbed_sfc'] = absorbed['sfc']
        self.diagnostics['absorbed_atm'] = absorbed['atm']

    def radiative_tendencies(self):
        """Compute net radiative heating everywhere in the Column (in W/m^2),
        and the resulting temperature change over a specified timestep (in K).
        """
        # compute longwave heating rates
        self.radiative_heating()
        #  temperature tendencies due only to radiation
        self.tendencies['Ts'] = (self.diagnostics['absorbed_sfc'] *
                                 self.param['timestep'] / self.c_sfc)
        self.tendencies['Tatm'] = (self.diagnostics['absorbed_atm'] *
                                    self.param['timestep'] / self.c_atm)


class GreyRadiation_LW(NbandModel):
    '''Grey Radiation model: a single band for all longwave.
    Absorptivity = Emissivity in all atmospheric layers.
    Surface albedo is zero.'''
    def __init__(self, eps=None, **kwargs):
        super(GreyRadiation_LW, self).__init__(eps=eps, **kwargs)
        self.set_absorptivity(eps)
        self.set_emissivity(np.ones_like(self.state['Ts']), eps)
        self.albedo_sfc = np.zeros_like(self.state['Ts'])
        self.flux_from_space = np.zeros_like(self.state['Ts'])
        
    def radiative_heating(self):
        super(GreyRadiation_LW, self).radiative_heating()
        self.diagnostics['LW_down_sfc'] = self.flux['incident_sfc']
        self.diagnostics['OLR_sfc'] = self.flux['sfc2space']
        self.diagnostics['OLR_atm'] = self.flux['atm2space']
        self.diagnostics['OLR'] = self.flux['up2space']
        self.diagnostics['LW_absorbed_sfc'] = self.absorbed['sfc']
        self.diagnostics['LW_absorbed_atm'] = self.absorbed['atm']
        
        
class GreyRadiation_SW(NbandModel):
    def __init__(self, Q=341.5, eps=None, **kwargs):
        super(GreyRadiation_SW, self).__init__(eps=eps, **kwargs)
        self.set_absorptivity(eps)
        self.set_emissivity(np.zeros_like(self.state['Ts']), np.zeros_like(self.state['Tatm']))
        self.flux_from_space = Q * np.ones_like(self.state['Ts'])
        
    def radiative_heating(self):
        super(GreyRadiation_SW, self).radiative_heating()
        self.diagnostics['SW_absorbed_sfc'] = self.absorbed['sfc']
        self.diagnostics['SW_absorbed_atm'] = self.absorbed['atm']
        self.diagnostics['SWdown_sfc'] = self.flux['incident_sfc']
        self.diagnostics['SWup_TOA'] = self.flux['up2space']
        self.diagnostics['SW_absorbed_total'] = self.absorbed['total']
        self.diagnostics['planetary_albedo'] = self.flux['up2space'] / self.flux_from_space


def compute_layer_absorptivity(abs_coeff, grid):
    '''Compute layer absorptivity from a constant absorption coefficient.'''
    return (2. / (1 + 2. * const.g / abs_coeff /
                         (grid['lev'].delta * const.mb_to_Pa)))

    
#==============================================================================
# 
# class GreyRadiation(_TimeSteppingModel):
#     def __init__(self, grid=None, state=None, param=None, eps=None, abs_coeff=None, **kwargs):
#         super(GreyRadiation, self).__init__(grid=grid, state=state, param=param, **kwargs)
#         self.set_LW_emissivity(eps=eps, abs_coeff=self.param['abs_coeff'])
#         self.set_SW_absorptivity(eps)
#         #  heat capacity of atmospheric layers
#         self.c_atm = heat_capacity.atmosphere(self.grid['lev'].delta)
#         #  heat capacity of surface in J / m**2 / K
#         self.c_sfc = heat_capacity.slab_ocean(self.param['water_depth'])
# 
#     def set_LW_emissivity(self, eps=None, abs_coeff=None):
#         """Set the longwave emissivity / absorptivity eps for the Column."""
#         if eps is None:
#             # default is to set eps equal at every level
#             # use value consistent with the absorption coefficient parameter
#             self.eps = (2. / (1 + 2. * const.g / abs_coeff /
#                         (self.grid['lev'].delta * const.mb_to_Pa)))
#         elif (np.isscalar(eps) or eps.size == self.param['num_levels']):
#             self.eps = eps
#         else:
#             raise ValueError('eps must be scalar or have exactly ' +
#                              self.num_levels + ' elements.')
# 
#         if np.isscalar(self.eps):
#             self.LWtrans = set_transmissitivity(self.eps * np.ones_like(self.grid['lev'].points))
#         else:
#             self.LWtrans = set_transmissitivity(self.eps)
# 
#     def set_SW_absorptivity(self, eps=None):
#         """Set the shortwave absorptivity eps for the Column."""
#         if eps is None:
#             # default is no shortwave absorption
#             self.SWtrans = set_transmissitivity(np.zeros_like(self.grid['lev'].points))
#         elif np.isscalar(eps):
#             # passing a single scalar sets eps equal to this number everywhere
#             self.SWtrans = set_transmissitivity(self.eps * np.ones_like(self.grid['lev'].points))
#         elif np.size(eps) == self.param['num_levels']:
#             self.SWtrans = set_transmissitivity(eps)
#         else:
#             raise ValueError('eps must be scalar or have exactly ' +
#                              self.param['num_levels'] + ' elements.')
#     
#     def emission(self):
#         emit_sfc = const.sigma * self.state['Ts']**4
#         emit_atm = const.sigma * self.state['Tatm']**4
#         self.diagnostics['emit_sfc'] = emit_sfc
#         self.diagnostics['emit_atm'] = emit_atm
#     
#     def longwave_heating(self):
#         """Compute the net longwave radiative heating at every level
#         and the surface. Also store the upwelling longwave radiation at the top
#         (OLR), and the downwelling longwave radiation at the surface.
#         """
#         eps = self.LWtrans['absorb']
#         # emissions from surface and each layer
#         self.emission()
#         absorbed_sfc, absorbed_atm, OLR, LWdown_sfc, OLR_sfc, OLR_atm = \
#             flux.LWflux(self.diagnostics['emit_atm'], 
#                         self.diagnostics['emit_sfc'], 
#                         self.LWtrans)
#         self.diagnostics['LW_down_sfc'] = LWdown_sfc
#         self.diagnostics['OLR_sfc'] = OLR_sfc
#         self.diagnostics['OLR_atm'] = OLR_atm
#         self.diagnostics['OLR'] = OLR
#         self.diagnostics['LW_absorbed_sfc'] = absorbed_sfc
#         self.diagnostics['LW_absorbed_atm'] = absorbed_atm
#     
#     def shortwave_heating(self):
#         '''Net shortwave heating at each level.'''
#         self.diagnostics['SWdown_TOA'] = self.param['Q']
#         absorbed_sfc, absorbed_atm, absorbed_total, incident_sfc, up2space, planetary_albedo = \
#             flux.SWflux(self.diagnostics['SWdown_TOA'],
#                         self.param['albedo'],
#                         self.SWtrans)
#         self.diagnostics['SWdown_sfc'] = incident_sfc
#         self.diagnostics['SW_absorbed_sfc'] = absorbed_sfc
#         self.diagnostics['SW_absorbed_atm'] = absorbed_atm
#         self.diagnostics['SWup_TOA'] = up2space
#         self.diagnostics['SW_absorbed_total'] = absorbed_total
#         self.diagnostics['planetary_albedo'] = planetary_albedo
#     
#     def rad_temperature_tendency(self):
#         """Compute net radiative heating everywhere in the Column (in W/m^2),
#         and the resulting temperature change over a specified timestep (in K).
#         """
#         # compute longwave heating rates
#         self.longwave_heating()
#         self.shortwave_heating()
#         # net radiative forcing
#         self.diagnostics['rad_heating_sfc'] = (self.diagnostics['SW_absorbed_sfc'] 
#                                         + self.diagnostics['LW_absorbed_sfc'])
#         self.diagnostics['rad_heating_atm'] = (self.diagnostics['SW_absorbed_atm'] 
#                                         + self.diagnostics['LW_absorbed_atm'])
#         #  temperature tendencies due only to radiation
#         self.diagnostics['rad_temp_tendency_sfc'] = (self.diagnostics['rad_heating_sfc']
#                                     * self.param['timestep'] / self.c_sfc)
#         self.diagnostics['rad_temp_tendency_atm'] = (self.diagnostics['rad_heating_atm']
#                                     * self.param['timestep'] / self.c_atm)
#     
#     def compute(self):
#       self.rad_temperature_tendency()
#       self.tendencies['Ts'] = self.diagnostics['rad_temp_tendency_sfc']
#       self.tendencies['Tatm'] = self.diagnostics['rad_temp_tendency_atm']
#==============================================================================
