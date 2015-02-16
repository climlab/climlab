import numpy as np
from climlab import constants as const
from climlab.radiation.radiation import Radiation


class GreyRadiation_LW(Radiation):
    '''Grey Radiation model: a single band for all longwave.
    Absorptivity = Emissivity in all atmospheric layers.
    Surface albedo is zero.'''
    def __init__(self, absorptivity=None, **kwargs):
        super(GreyRadiation_LW, self).__init__(absorptivity=absorptivity, **kwargs)
        #self.emissivity_sfc = np.ones_like(self.state['Ts'])
        #self.emissivity_atm = self.absorptivity
        #self.albedo_sfc = np.zeros_like(self.state['Ts'])
        self.albedo_sfc = 0.  #  need to vectorize this        
        #self.flux_from_space = np.zeros_like(self.state['Ts'])

    def radiative_heating(self):
        super(GreyRadiation_LW, self).radiative_heating()
        self.diagnostics['OLR'] = self.flux_net[-1]     
        #self.diagnostics['LW_down_sfc'] = self.flux['incident_sfc']
        #self.diagnostics['OLR_sfc'] = self.flux['sfc2space']
        #self.diagnostics['OLR_atm'] = self.flux['atm2space']
        #self.diagnostics['OLR'] = self.flux['up2space']
        #self.diagnostics['LW_absorbed_sfc'] = self.absorbed['sfc']
        #self.diagnostics['LW_absorbed_atm'] = self.absorbed['atm']
        

# use insolation class to calculate Q
# implement a setter for SW class
# the coupling process needs to look after exchanging between insolation and column

class GreyRadiation_SW(Radiation):
    def __init__(self, absorptivity=None, **kwargs):
        super(GreyRadiation_SW, self).__init__(absorptivity=absorptivity, **kwargs)
        #self.emissivity_sfc = np.zeros_like(self.state['Ts'])
        #self.emissivity_atm = np.zeros_like(self.state['Tatm'])
        #self.flux_from_space = np.ones_like(self.state['Ts'])

    @property
    def emissivity(self):
        return np.zeros_like(self.Tatm)

    def radiative_heating(self):
        super(GreyRadiation_SW, self).radiative_heating()
                
        #self.diagnostics['SW_absorbed_sfc'] = self.absorbed['sfc']
        #self.diagnostics['SW_absorbed_atm'] = self.absorbed['atm']
        #self.diagnostics['SWdown_sfc'] = self.flux['incident_sfc']
        #self.diagnostics['SWup_TOA'] = self.flux['up2space']
        #self.diagnostics['SW_absorbed_total'] = self.absorbed['total']
        #self.diagnostics['planetary_albedo'] = (self.flux['up2space'] / 
        #                                        self.from_space)


def compute_layer_absorptivity(abs_coeff, dp):
    '''Compute layer absorptivity from a constant absorption coefficient.'''
    return (2. / (1 + 2. * const.g / abs_coeff /
                         (dp * const.mb_to_Pa)))
