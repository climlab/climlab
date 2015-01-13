import numpy as np
import constants as const
from timestepping_model import _TimeSteppingModel
import heat_capacity
from transmissivity import set_transmissitivity
import flux


def AplusBT(T, param):
    tendencies = {}
    diagnostics = {}
    A = param['A']
    B = param['B']
    OLR = A + B*T
    tendencies['Ts'] = -OLR / heat_capacity.slab_ocean(param['water_depth'])
    diagnostics['OLR'] = OLR


class GreyRadiation(_TimeSteppingModel):
    def __init__(self, grid=None, state=None, param=None, eps=None, abs_coeff=None, **kwargs):
        super(GreyRadiation, self).__init__(grid=grid, state=state, param=param, **kwargs)
        self.set_LW_emissivity(eps=eps, abs_coeff=self.param['abs_coeff'])
        self.set_SW_absorptivity(eps)
        #  heat capacity of atmospheric layers
        self.c_atm = heat_capacity.atmosphere(self.grid['lev'].delta)
        #  heat capacity of surface in J / m**2 / K
        self.c_sfc = heat_capacity.slab_ocean(self.param['water_depth'])

    def set_LW_emissivity(self, eps=None, abs_coeff=None):
        """Set the longwave emissivity / absorptivity eps for the Column."""
        if eps is None:
            # default is to set eps equal at every level
            # use value consistent with the absorption coefficient parameter
            self.eps = (2. / (1 + 2. * const.g / abs_coeff /
                        (self.grid['lev'].delta * const.mb_to_Pa)))
        elif (np.isscalar(eps) or eps.size == self.param['num_levels']):
            self.eps = eps
        else:
            raise ValueError('eps must be scalar or have exactly ' +
                             self.num_levels + ' elements.')

        if np.isscalar(self.eps):
            self.LWtrans = set_transmissitivity(self.eps * np.ones_like(self.grid['lev'].points))
        else:
            self.LWtrans = set_transmissitivity(self.eps)

    def set_SW_absorptivity(self, eps=None):
        """Set the shortwave absorptivity eps for the Column."""
        if eps is None:
            # default is no shortwave absorption
            self.SWtrans = set_transmissitivity(np.zeros_like(self.grid['lev'].points))
        elif np.isscalar(eps):
            # passing a single scalar sets eps equal to this number everywhere
            self.SWtrans = set_transmissitivity(self.eps * np.ones_like(self.grid['lev'].points))
        elif np.size(eps) == self.param['num_levels']:
            self.SWtrans = set_transmissitivity(eps)
        else:
            raise ValueError('eps must be scalar or have exactly ' +
                             self.param['num_levels'] + ' elements.')
    
    def emission(self):
        emit_sfc = const.sigma * self.state['Ts']**4
        emit_atm = const.sigma * self.state['Tatm']**4
        self.diagnostics['emit_sfc'] = emit_sfc
        self.diagnostics['emit_atm'] = emit_atm
    
    def longwave_heating(self):
        """Compute the net longwave radiative heating at every level
        and the surface. Also store the upwelling longwave radiation at the top
        (OLR), and the downwelling longwave radiation at the surface.
        """
        eps = self.LWtrans['absorb']
        # emissions from surface and each layer
        self.emission()
        absorbed_sfc, absorbed_atm, OLR, LWdown_sfc, OLR_sfc, OLR_atm = \
            flux.LWflux(self.diagnostics['emit_atm'], 
                        self.diagnostics['emit_sfc'], 
                        self.LWtrans)
        self.diagnostics['LW_down_sfc'] = LWdown_sfc
        self.diagnostics['OLR_sfc'] = OLR_sfc
        self.diagnostics['OLR_atm'] = OLR_atm
        self.diagnostics['OLR'] = OLR
        self.diagnostics['LW_absorbed_sfc'] = absorbed_sfc
        self.diagnostics['LW_absorbed_atm'] = absorbed_atm
    
    def shortwave_heating(self):
        '''Net shortwave heating at each level.'''
        self.diagnostics['SWdown_TOA'] = self.param['Q']
        absorbed_sfc, absorbed_atm, absorbed_total, incident_sfc, up2space, planetary_albedo = \
            flux.SWflux(self.diagnostics['SWdown_TOA'],
                        self.param['albedo'],
                        self.SWtrans)
        self.diagnostics['SWdown_sfc'] = incident_sfc
        self.diagnostics['SW_absorbed_sfc'] = absorbed_sfc
        self.diagnostics['SW_absorbed_atm'] = absorbed_atm
        self.diagnostics['SWup_TOA'] = up2space
        self.diagnostics['SW_absorbed_total'] = absorbed_total
        self.diagnostics['planetary_albedo'] = planetary_albedo
    
    def rad_temperature_tendency(self):
        """Compute net radiative heating everywhere in the Column (in W/m^2),
        and the resulting temperature change over a specified timestep (in K).
        """
        # compute longwave heating rates
        self.longwave_heating()
        self.shortwave_heating()
        # net radiative forcing
        self.diagnostics['rad_heating_sfc'] = (self.diagnostics['SW_absorbed_sfc'] 
                                        + self.diagnostics['LW_absorbed_sfc'])
        self.diagnostics['rad_heating_atm'] = (self.diagnostics['SW_absorbed_atm'] 
                                        + self.diagnostics['LW_absorbed_atm'])
        #  temperature tendencies due only to radiation
        self.diagnostics['rad_temp_tendency_sfc'] = (self.diagnostics['rad_heating_sfc']
                                    * self.param['timestep'] / self.c_sfc)
        self.diagnostics['rad_temp_tendency_atm'] = (self.diagnostics['rad_heating_atm']
                                    * self.param['timestep'] / self.c_atm)
    
    def compute(self):
        self.rad_temperature_tendency()
        self.tendencies['Ts'] = self.diagnostics['rad_temp_tendency_sfc']
        self.tendencies['Tatm'] = self.diagnostics['rad_temp_tendency_atm']
