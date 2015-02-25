import numpy as np
from climlab.radiation.radiation import RadiationSW
from climlab import constants as const
from climlab.utils.thermo import clausius_clapeyron
from climlab.domain import domain, axis, field
from copy import copy


class ThreeBandSW(RadiationSW):
    '''A three-band mdoel for shortwave radiation.
    
    The spectral decomposition used here is largely based on the
    "Moist Radiative-Convective Model" by Aarnout van Delden, Utrecht University
    a.j.vandelden@uu.nl
    http://www.staff.science.uu.nl/~delde102/RCM.htm

    Three SW channels:
        channel 0 is Hartley and Huggins band (UV, 1%, 200 - 340 nm)
        channel 1 is Chappuis band (27%, 450 - 800 nm)
        channel 2 is remaining radiation (72%)
    '''
    def __init__(self, **kwargs):
        super(ThreeBandSW, self).__init__(**kwargs)
        self.numSWchannels = 3
        #  Three SW channels:
        # channel 0 is Hartley and Huggins band (UV, 1%, 200 - 340 nm)
        # channel 1 is Chappuis band (27%, 450 - 800 nm)
        # channel 2 is remaining radiation (72%)
        #   fraction of the total solar flux in each band:
        #self.band_fraction = np.array([0.01, 0.27, 0.72])
        # abstract axis for channels
        ax = axis.Axis(num_points=self.numSWchannels)
        self.channel_ax = {'channel': ax}
        dom = domain._Domain(axes=self.channel_ax)
        #   fraction of the total solar flux in each band:
        self.band_fraction = field.Field([0.01, 0.27, 0.72], domain=dom)
        ##  absorption cross-sections in m**2 / kg
        self.sigmaH2O = np.reshape(np.array([0.002, 0.002, 0.002]),
                                   (self.numSWchannels, 1))
        self.sigmaO3 = np.reshape(np.array([200.E-24, 0.285E-24, 0.]) * 
            const.Rd / const.kBoltzmann, (self.numSWchannels, 1))
        self.cosZen = 0.5  # cosine of the average solar zenith angle
        self.CO2vmr = 380.E-6 * np.ones_like(self.lev)
        self.O3vmr = np.zeros_like(self.lev)
        self.p = self.lev
        dp = self.Tatm.domain.lev.delta
        self.mass_per_layer = dp * const.mb_to_Pa / const.g
        self.relative_humidity = 0.77  # fixed relative humidity at surface
        self.qStrat = 5.E-6  # minimum specific humidity for stratosphere
        self.flux_from_space = self.band_fraction*np.ones_like(self.Ts)
        self.flux_from_sfc = self.band_fraction*np.zeros_like(self.Ts)
        self.albedo_sfc = np.ones_like(self.band_fraction)*self.albedo_sfc
        self.compute_absorptivity()

    def Manabe_water_vapor(self):
        '''Compute water vapor mixing ratio profile following 
        Manabe and Wetherald JAS 1967
        Fixed surface relative humidity and a specified fractional profile.'''
        Q = self.p / const.ps
        h = self.relative_humidity * ( (Q - 0.02) / (1-0.02) )
        es = clausius_clapeyron( self.Tatm )
        e = h * es
        # convert to specific humidity (assume dilute)
        qH2O = e / self.p * const.Rd / const.Rv 
        #  mixing ratio can't be smaller than qStrat (need some water in the stratosphere!)
        return np.maximum( self.qStrat, qH2O )
    
    def compute_optical_path(self,O3vmr, H2Ovmr, cosZen ):
        '''O3vmr and H2Ovmr are mixing ratios (dimensionless arrays)
        cosZen is the solar zenith angle.'''
        qO3 = O3vmr / (1+O3vmr)
        qH2O = H2Ovmr / (1+H2Ovmr)
        return ((qO3*self.sigmaO3 + qH2O*self.sigmaH2O) / cosZen * 
                self.mass_per_layer)

    def compute_absorptivity(self):
        #  need to recompute transmissivities each time because 
        # water vapor is changing
        self.H2Ovmr = self.Manabe_water_vapor()
        opticalpath = self.compute_optical_path(self.O3vmr, self.H2Ovmr,
                                                     self.cosZen )
        epsSW = 1. - np.exp(-opticalpath)
        axes = copy(self.Tatm.domain.axes)
        # add these to the dictionary of axes
        axes.update(self.channel_ax)
        dom = domain.Atmosphere(axes=axes)
        self.absorptivity = field.Field(epsSW, domain=dom)
    
    def radiative_heating(self):
        #  need to recompute transmissivities each time because 
        # water vapor is changing
        self.compute_absorptivity()
        super(ThreeBandSW, self).radiative_heating()

#  Still some dimension mismatch issues in the flux calculations. Not working yet.