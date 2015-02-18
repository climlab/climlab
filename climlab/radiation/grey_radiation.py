import numpy as np
from climlab import constants as const
from climlab.radiation.radiation import Radiation


class GreyRadiation_LW(Radiation):
    '''Grey Radiation model: a single band for all longwave.
    Absorptivity = Emissivity in all atmospheric layers.
    Surface albedo is zero.'''
    def __init__(self, absorptivity=None, **kwargs):
        super(GreyRadiation_LW, self).__init__(absorptivity=absorptivity, **kwargs)
        self.albedo_sfc = np.zeros_like(self.Ts)

    def radiative_heating(self):
        super(GreyRadiation_LW, self).radiative_heating()
        self.diagnostics['OLR'] = self.flux_net[-1]     
        self.diagnostics['LW_down_sfc'] = self.flux_to_sfc
        self.diagnostics['LW_up_sfc'] = self.flux_from_sfc
        #  need to compute contributions to OLR
        #self.diagnostics['OLR_sfc'] = self.flux['sfc2space']
        #self.diagnostics['OLR_atm'] = self.flux['atm2space']
        self.diagnostics['OLR'] = self.flux_to_space
        self.diagnostics['LW_absorbed_sfc'] = -self.flux_net[0]
        self.diagnostics['LW_absorbed_atm'] = self.absorbed
        self.diagnostics['LW_emission'] = self.emission

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
        self.diagnostics['ASR'] = self.flux_from_space - self.flux_to_space
        self.diagnostics['SW_absorbed_sfc'] = -self.flux_net[0]
        self.diagnostics['SW_absorbed_atm'] = self.absorbed
        self.diagnostics['SW_down_sfc'] = self.flux_to_sfc
        self.diagnostics['SW_up_sfc'] = self.flux_from_sfc
        self.diagnostics['SW_up_TOA'] = self.flux_to_space
        self.diagnostics['SW_down_TOA'] = self.flux_from_space
        self.diagnostics['SW_absorbed_total'] = self.absorbed_total - self.flux_net[0]
        self.diagnostics['planetary_albedo'] = (self.flux_to_space / 
                                                self.flux_from_space)
        self.diagnostics['SW_emission'] = self.emission

def compute_layer_absorptivity(abs_coeff, dp):
    '''Compute layer absorptivity from a constant absorption coefficient.'''
    return (2. / (1 + 2. * const.g / abs_coeff /
                         (dp * const.mb_to_Pa)))
