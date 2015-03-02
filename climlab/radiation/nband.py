import numpy as np
from climlab.radiation.radiation import Radiation
from climlab import constants as const
from climlab.domain import domain, axis, field
from copy import copy


class NbandRadiation(Radiation):
    '''Process for radiative transfer.
    Solves the discretized Schwarschild two-stream equations
    with the spectrum divided into N spectral bands.
    
    Every NbandRadiation object has an attribute
        self.band_fraction
    with sum(self.band_fraction) == 1
    that gives the fraction of the total beam in each band
    
    Also a dictionary 
        self.absorber_vmr
    that gives the volumetric mixing ratio of every absorbing gas
    on the same grid as temperature
    
    and a dictionary
        self.absorption_cross_section
    that gives the absorption cross-section per unit mass for each gas
    in every spectral band
    '''
    def __init__(self, **kwargs):
        super(NbandRadiation, self).__init__(**kwargs)
        #self.numSWchannels = 3
        # this should be overridden by daughter classes
        self.band_fraction = np.array(1.)
        ##  a dictionary of absorbing gases, in volumetric mixing ratios
        #  each item should have dimensions of self.Tatm
        self.absorber_vmr = {}
        #self.CO2vmr = 380.E-6 * np.ones_like(self.lev)
        #self.O3vmr = np.zeros_like(self.lev)
        
        # a dictionary of absorption cross-sections in m**2 / kg
        # each item should have dimension...  (num_channels, 1)
        self.absorption_cross_section = {}
        #self.sigmaH2O = np.reshape(np.array([0.002, 0.002, 0.002]),
        #                           (self.numSWchannels, 1))
        #self.sigmaO3 = np.reshape(np.array([200.E-24, 0.285E-24, 0.]) * 
        #    const.Rd / const.kBoltzmann, (self.numSWchannels, 1))
        self.cosZen = 1.  # cosine of the average zenith angle
        dp = self.Tatm.domain.lev.delta
        self.mass_per_layer = dp * const.mb_to_Pa / const.g
        self.flux_from_space = np.zeros_like(self.Ts)
        self.flux_from_sfc = np.zeros_like(self.Ts)
        self.albedo_sfc = np.ones_like(self.band_fraction)*self.albedo_sfc
        self.compute_absorptivity()
    
    @property 
    def band_fraction(self):
        return self._band_fraction
    @band_fraction.setter
    def band_fraction(self, value):
        self.num_channels = value.size        
        # abstract axis for channels
        ax = axis.Axis(num_points=self.num_channels)
        self.channel_ax = {'channel': ax}
        dom = domain._Domain(axes=self.channel_ax)
        #   fraction of the total solar flux in each band:
        self._band_fraction = field.Field(self.band_fraction, domain=dom)    

    def compute_optical_path(self):
        # this will cause a problrm for a model without CO2
        tau = np.zeros_like(self.absorber_vmr['CO2']*
                            self.absorption_cross_section['CO2'])
        for gas, vmr in self.absorber_vmr.iteritems:
            # convert to mass of absorber per unit total mass
            q = vmr / (1.+vmr)
            kappa = self.absorption_cross_section[gas]
            tau += q * kappa
        tau *= self.mass_per_layer / self.cosZen
        return tau

    def compute_absorptivity(self):
        #  assume that the water vapor etc is current
        optical_path = self.compute_optical_path()
        #  account for finite layer depth
        absorptivity = 1. - np.exp(-optical_path)
        axes = copy(self.Tatm.domain.axes)
        # add these to the dictionary of axes
        axes.update(self.channel_ax)
        dom = domain.Atmosphere(axes=axes)
        self.absorptivity = field.Field(absorptivity, domain=dom)
    
    def radiative_heating(self):
        #  need to recompute transmissivities each time because 
        # water vapor is changing
        self.compute_absorptivity()
        self.emission = self.compute_emission()
        try:
            fromspace = self.split_channels(self.flux_from_space)
        except:
            fromspace = self.split_channels(np.zeros_like(self.Ts))
        
        self.flux_down = self.trans.flux_down(fromspace, self.emission)
        # this ensure same dimensions as other fields
        flux_down_sfc = self.flux_down[..., 0, np.newaxis]
        #flux_down_sfc = self.flux_down[..., 0]
        self.flux_to_sfc = np.sum(flux_down_sfc, axis=0)

        flux_from_sfc = self.split_channels(self.flux_from_sfc)
        flux_up_bottom = flux_from_sfc + self.albedo_sfc*flux_down_sfc
        self.flux_up = self.trans.flux_up(flux_up_bottom, self.emission)
        self.flux_net = self.flux_up - self.flux_down
        flux_up_top = self.flux_up[..., -1, np.newaxis]
        # absorbed radiation (flux convergence) in W / m**2
        self.absorbed = -np.diff(self.flux_net, axis=1)
        self.absorbed_total = np.sum(self.absorbed)
        self.heating_rate['Tatm'] = np.sum(self.absorbed, axis=0)
        self.flux_to_space = np.sum(flux_up_top, axis=0)
    
    def split_channels(self, flux):
        return (self.band_fraction*flux)[..., np.newaxis]


def ThreeBandSW(NbandRadiation):
    def __init__(self, **kwargs):
        super(ThreeBandSW, self).__init__(**kwargs)
        #  Three SW channels:
        # channel 0 is Hartley and Huggins band (UV, 1%, 200 - 340 nm)
        # channel 1 is Chappuis band (27%, 450 - 800 nm)
        # channel 2 is remaining radiation (72%)
        #   fraction of the total solar flux in each band:
        self.band_fraction = np.array([0.01, 0.27, 0.72])
        self.absorber_vmr['CO2'] = 380.E-6 * np.ones_like(self.Tatm)
        self.absorber_vmr['O3'] = np.zeros_like(self.Tatm)
        ##  absorption cross-sections in m**2 / kg
        O3 = np.array([200.E-24, 0.285E-24, 0.]) * const.Rd / const.kBoltzmann
        self.absorption_cross_section['O3'] = np.reshape(O3,
            (self.numSWchannels, 1))
        H2O = np.array([0.002, 0.002, 0.002]
        self.absorption_cross_section['H2O'] = np.reshape(H2O,
            (self.numSWchannels, 1))
        self.cosZen = 0.5  # cosine of the average solar zenith angle
