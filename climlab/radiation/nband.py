from __future__ import division
from builtins import range
import numpy as np
from climlab.radiation.greygas import GreyGas
from climlab import constants as const
from climlab.domain import domain, axis, field
from copy import copy


class NbandRadiation(GreyGas):
    '''Process for radiative transfer.
    Solves the discretized Schwarschild two-stream equations
    with the spectrum divided into N spectral bands.

    Every NbandRadiation object has an attribute
    ``self.band_fraction``
    with sum(self.band_fraction) == 1
    that gives the fraction of the total beam in each band

    Also a dictionary
    ``self.absorber_vmr``
    that gives the volumetric mixing ratio of every absorbing gas
    on the same grid as temperature

    and a dictionary
    ``self.absorption_cross_section``
    that gives the absorption cross-section per unit mass for each gas
    in every spectral band
    '''
    def __init__(self, absorber_vmr=None, **kwargs):
        super(NbandRadiation, self).__init__(**kwargs)
        newinput = ['band_fraction',
                    'absorber_vmr',
                    'absorption_cross_section',
                    'cosZen',]
        self.declare_input(newinput)
        # this should be overridden by daughter classes
        self.band_fraction = np.array(1.)
        ##  a dictionary of absorbing gases, in volumetric mixing ratios
        #  each item should have dimensions of self.Tatm
        #  Can be passed as input argument
        if absorber_vmr is None:
            absorber_vmr = {}
        self.absorber_vmr = absorber_vmr
        # a dictionary of absorption cross-sections in m**2 / kg
        # each item should have dimension...  (num_channels, 1)
        self.absorption_cross_section = {}
        self.cosZen = 1.  # cosine of the average zenith angle
        dp = self.Tatm.domain.lev.delta
        self.mass_per_layer = dp * const.mb_to_Pa / const.g
        self.albedo_sfc = np.ones_like(self.band_fraction) * self.albedo_sfc

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
        self._band_fraction = field.Field(value, domain=dom)

    def _compute_optical_path(self):
        # this will cause a problem for a model without CO2
        tau = np.zeros_like(self.absorber_vmr['CO2']*
                            self.absorption_cross_section['CO2'])
        for gas, vmr in self.absorber_vmr.items():
            # convert to mass of absorber per unit total mass
            if gas == 'H2O':  # H2O is stored as specific humidity, not VMR
                q = vmr
            else:
                q = vmr / (1.+vmr)
            try:
                # if this gas isn't present in absorption dictionary
                # the assumption is that there is no absorption!
                kappa = self.absorption_cross_section[gas]
                tau += q * kappa
            except: pass
        tau *= self.mass_per_layer / self.cosZen
        return tau

    def _compute_absorptivity(self):
        #  assume that the water vapor etc is current
        optical_path = self._compute_optical_path()
        #  account for finite layer depth
        absorptivity = 1. - np.exp(-optical_path)
        axes = copy(self.Tatm.domain.axes)
        # add these to the dictionary of axes
        axes.update(self.channel_ax)
        dom = domain.Atmosphere(axes=axes)
        self.absorptivity = field.Field(absorptivity, domain=dom)

    def _compute_emission_sfc(self):
        #  need to split the total emission across the bands
        total_emission = super(NbandRadiation, self)._compute_emission_sfc()
        return self._split_channels(total_emission)

    def _compute_emission(self):
        #  need to split the total emission across the bands
        total_emission = super(NbandRadiation, self)._compute_emission()
        band_fraction = self.band_fraction
        for n in range(self.Tatm.domain.numdims):
            band_fraction = band_fraction[:, np.newaxis]
        return total_emission * band_fraction

    def _compute_radiative_heating(self):
        #  need to recompute transmissivities each time because
        # water vapor is changing
        self._compute_absorptivity()
        super(NbandRadiation, self)._compute_radiative_heating()

    def _split_channels(self, flux):
        split = np.outer(self.band_fraction, flux)
        # make sure there's a singleton dimension at the last axis (level)
        if np.size(split, axis=-1) != 1:
            split = split[..., np.newaxis]
        return split

    def _join_channels(self, flux):
        return np.sum(flux, axis=0)


class ThreeBandSW(NbandRadiation):
    def __init__(self, emissivity_sfc=0., **kwargs):
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
        super(ThreeBandSW, self).__init__(emissivity_sfc=emissivity_sfc, **kwargs)
        #   fraction of the total solar flux in each band:
        self.band_fraction = np.array([0.01, 0.27, 0.72])
        if 'CO2' not in self.absorber_vmr:
            self.absorber_vmr['CO2'] = 380.E-6 * np.ones_like(self.Tatm)
        if 'O3' not in self.absorber_vmr:
            self.absorber_vmr['O3'] = np.zeros_like(self.Tatm)
        if 'H2O' not in self.absorber_vmr:
            self.absorber_vmr['H2O'] = self.q
        ##  absorption cross-sections in m**2 / kg
        O3 = np.array([200.E-24, 0.285E-24, 0.]) * const.Rd / const.kBoltzmann
        #self.absorption_cross_section['O3'] = np.reshape(O3,
        #    (self.num_channels, 1))
        #H2O = np.array([0.002, 0.002, 0.002])
        H2O = np.array([0., 0., 0.001])
        for n in range(self.Tatm.domain.numdims):
            H2O = H2O[:, np.newaxis]
            O3 = O3[:, np.newaxis]
        self.absorption_cross_section['O3'] = O3
        self.absorption_cross_section['H2O'] = H2O
        #self.absorption_cross_section['H2O'] = np.reshape(H2O,
        #    (self.num_channels, 1))
        self.absorption_cross_section['CO2'] = \
            np.zeros_like(self.absorption_cross_section['O3'])
        self.cosZen = 0.5  # cosine of the average solar zenith angle

    @property
    def emissivity(self):
        # This ensures that emissivity is always zero for shortwave classes
        return np.zeros_like(self.absorptivity)


class FourBandSW(NbandRadiation):
    #  The most recent RMCM program uses a four-channel SW model
    #  this is probably a better way to do it... distinguishes between
    #  visible band with no absorption and near-infrared with weak H2O absorption
    #   But this needs some tuning and better documentation
    def __init__(self, emissivity_sfc=0., **kwargs):
        '''A four-band mdoel for shortwave radiation.

        The spectral decomposition used here is largely based on the
        "Moist Radiative-Convective Model" by Aarnout van Delden, Utrecht University
        a.j.vandelden@uu.nl
        http://www.staff.science.uu.nl/~delde102/RCM.htm

        Four SW channels:
            channel 0 is Hartley and Huggins band (UV, 6%, <340 nm)
            channel 1 is part of visible with no O3 absorption (14%, 340 - 500 nm)
            channel 2 is Chappuis band (27%, 500 - 700 nm)
            channel 3 is near-infrared (53%, > 700 nm)
        '''
        super(FourBandSW, self).__init__(emissivity_sfc=emissivity_sfc, **kwargs)
        #   fraction of the total solar flux in each band:
        self.band_fraction = np.array([0.06, 0.14, 0.27, 0.53])
        if 'CO2' not in self.absorber_vmr:
            self.absorber_vmr['CO2'] = 380.E-6 * np.ones_like(self.Tatm)
        if 'O3' not in self.absorber_vmr:
            self.absorber_vmr['O3'] = np.zeros_like(self.Tatm)
        if 'H2O' not in self.absorber_vmr:
            self.absorber_vmr['H2O'] = self.q
        ##  absorption cross-sections in m**2 / kg
        O3 = np.array([200.E-24, 0., 0.285E-24, 0.]) * const.Rd / const.kBoltzmann
        #self.absorption_cross_section['O3'] = np.reshape(O3,
        #    (self.num_channels, 1))
        H2O = np.array([0., 0., 0., 0.0012])
        for n in range(self.Tatm.domain.numdims):
            H2O = H2O[:, np.newaxis]
            O3 = O3[:, np.newaxis]
        self.absorption_cross_section['O3'] = O3
        self.absorption_cross_section['H2O'] = H2O
        #self.absorption_cross_section['H2O'] = np.reshape(H2O,
        #    (self.num_channels, 1))
        self.absorption_cross_section['CO2'] = \
            np.zeros_like(self.absorption_cross_section['O3'])
        self.cosZen = 0.5  # cosine of the average solar zenith angle

    @property
    def emissivity(self):
        # This ensures that emissivity is always zero for shortwave classes
        return np.zeros_like(self.absorptivity)



class FourBandLW(NbandRadiation):
    def __init__(self, **kwargs):
        '''Closely following SPEEDY / MITgcm longwave model
        band 0 is window region (between 8.5 and 11 microns)
        band 1 is CO2 channel (the band of strong absorption by CO2 around 15 microns)
        band 2 is weak H2O channel (aggregation of spectral regions with weak to moderate absorption by H2O)
        band 3 is strong H2O channel (aggregation of regions with strong absorption by H2O)
        '''
        super(FourBandLW, self).__init__(**kwargs)
        #  SPEEDY uses an approximation to the Planck function
        #  and the band fraction for every emission is calculated from
        #  its current temperature
        #  Here for simoplicity we'll just set an average band_fraction
        #  and hold it fixed
        Tarray = np.linspace(-30, 30) + 273.15
        self.band_fraction = np.mean(SPEEDY_band_fraction(Tarray), axis=1)

        # defaults from MITgcm/aim:
        # these are layer absorptivities per dp = 10^5 Pa
        # the water vapor terms are expressed for dq = 1 g/kg
        ABLWIN = 0.7
        ABLCO2 = 4.0
        ABLWV1 = 0.7
        ABLWV2 = 50.0
        #  I'm going to assume that the absorption in window region is by O3.
        #   not sure if this makes any sense... maybe it should be zero
        O3 = np.array([ABLWIN, 0., 0., 0.]) / 1E5 * const.g / 5E-6
        self.absorption_cross_section['O3'] = np.reshape(O3,
            (self.num_channels, 1))
        # the CO2 mixing ratio for which SPEEDY / MITgcm is tuned...
        #   not clear what this number should be
        AIMCO2 = 380E-6
        CO2 = np.array([0., ABLCO2, 0., 0.]) / 1E5 * const.g / AIMCO2
        #self.absorption_cross_section['CO2'] = np.reshape(CO2,
        #    (self.num_channels, 1))
        # Need to multiply by 1E3 for H2O fields because we use kg/kg for mixing ratio
        H2O = np.array([0., 0., ABLWV1, ABLWV2]) / 1E5 * const.g * 1E3
        #self.absorption_cross_section['H2O'] = np.reshape(H2O,
        #    (self.num_channels, 1))
        for n in range(self.Tatm.domain.numdims):
            CO2 = CO2[:, np.newaxis]
            O3 = O3[:, np.newaxis]
            H2O = H2O[:, np.newaxis]
        self.absorption_cross_section.update({'CO2': CO2, 'H2O': H2O, 'O3': O3})
        if 'CO2' not in self.absorber_vmr:
            self.absorber_vmr['CO2'] = 380.E-6 * np.ones_like(self.Tatm)
        if 'O3' not in self.absorber_vmr:
            self.absorber_vmr['O3'] = np.zeros_like(self.Tatm)
        if 'H2O' not in self.absorber_vmr:
            self.absorber_vmr['H2O'] = self.q


def SPEEDY_band_fraction(T):
    '''Python / numpy implementation of the formula used by SPEEDY and MITgcm
    to partition longwave emissions into 4 spectral bands.

    Input: temperature in Kelvin

    returns: a four-element array of band fraction

    Reproducing here the FORTRAN code from MITgcm/pkg/aim_v23/phy_radiat.F

    .. code-block:: fortran

    	      EPS3=0.95 _d 0

    	      DO JTEMP=200,320
    	        FBAND(JTEMP,0)= EPSLW
    	        FBAND(JTEMP,2)= 0.148 _d 0 - 3.0 _d -6 *(JTEMP-247)**2
    	        FBAND(JTEMP,3)=(0.375 _d 0 - 5.5 _d -6 *(JTEMP-282)**2)*EPS3
    	        FBAND(JTEMP,4)= 0.314 _d 0 + 1.0 _d -5 *(JTEMP-315)**2
    	        FBAND(JTEMP,1)= 1. _d 0 -(FBAND(JTEMP,0)+FBAND(JTEMP,2)
    	     &                           +FBAND(JTEMP,3)+FBAND(JTEMP,4))
    	      ENDDO

    	      DO JB=0,NBAND
    	        DO JTEMP=lwTemp1,199
    	          FBAND(JTEMP,JB)=FBAND(200,JB)
    	        ENDDO
    	        DO JTEMP=321,lwTemp2
    	          FBAND(JTEMP,JB)=FBAND(320,JB)
    	        ENDDO
    	      ENDDO

    '''
    # EPSLW is the fraction of longwave emission that goes directly to space
    #  It is set to zero by default in MITgcm code. We won't use it here.
    Tarray = np.array(T)
    Tarray = np.minimum(Tarray, 320.)
    Tarray = np.maximum(Tarray, 200.)
    num_band = 4
    dims = [num_band]
    dims.extend(Tarray.shape)
    FBAND = np.zeros(dims)

    EPS2=0.95
    FBAND[1,:] = 0.148 - 3.0E-6 *(T-247.)**2
    FBAND[2,:] = (0.375 - 5.5E-6 *(T-282.)**2)*EPS2
    FBAND[3,:] = 0.314 + 1.0E-5 *(T-315.)**2
    FBAND[0,:] = 1. - np.sum(FBAND, axis=0)
    return FBAND
