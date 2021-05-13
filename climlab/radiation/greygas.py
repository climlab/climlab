from __future__ import division
from builtins import range
import numpy as np
from climlab.utils.thermo import blackbody_emission
from climlab.radiation.transmissivity import Transmissivity
from climlab.process import EnergyBudget


class GreyGas(EnergyBudget):
    '''Base class for all band radiation models,
    including grey and semi-grey model.

    Input argument absorptivity is band absorptivity
    (should same size as grid).

    By default emissivity = absorptivity.
    Subclasses can override this is necessary (e.g. for shortwave model).

    The following boundary and input values need to be specified by
    user or parent process:
    - albedo_sfc (default is zero)
    - flux_from_space
    - absorptivity
    - reflectivity (default is zero)
    These are accessible (and settable) as process attributes
    Also stored in process.input dictionary

    The following values are computed are stored in the .diagnostics dictionary:
    - flux_from_sfc
    - flux_to_sfc
    - flux_to_space
    - absorbed
    - absorbed_total
    - emission
    - emission_sfc
    - flux_reflected_up
    (all in W/m2)
    '''
    def __init__(self, absorptivity=None, reflectivity=None, emissivity_sfc=1.,
                 albedo_sfc=0., **kwargs):
        super(GreyGas, self).__init__(**kwargs)
        self.add_input('flux_from_space', 0. * self.Ts)
        #  initialize all diagnostics to zero
        self.add_diagnostic('flux_from_sfc', 0. * self.Ts)
        self.add_diagnostic('flux_to_sfc', 0. * self.Ts)
        self.add_diagnostic('flux_to_space', 0. * self.Ts)
        self.add_diagnostic('absorbed', 0. * self.Tatm)
        self.add_diagnostic('absorbed_total', 0. * self.Ts)

        newinput = ['reflectivity',
                    'absorptivity',
                    'emissivity_sfc',
                    'albedo_sfc',]
        self.declare_input(newinput)
        if reflectivity is None:
            reflectivity = np.zeros_like(self.Tatm)
        if absorptivity is None:
            absorptivity = np.zeros_like(self.Tatm)
        self.absorptivity = absorptivity
        self.reflectivity = reflectivity
        self.emissivity_sfc = emissivity_sfc * np.ones_like(self.Ts)
        self.albedo_sfc = albedo_sfc * np.ones_like(self.Ts)
        #  Initialize diagnostics
        self.add_diagnostic('emission', 0. * self.Tatm)
        self.add_diagnostic('emission_sfc', 0. * self.Ts)
        self.add_diagnostic('flux_reflected_up')

    @property
    def absorptivity(self):
        return self.trans.absorptivity
    @absorptivity.setter
    def absorptivity(self, value):
        #  value should be a Field,
         #  or numpy array of same size as self.Tatm
        try:
            axis = value.domain.axis_index['lev']
        except:
            axis = self.Tatm.domain.axis_index['lev']
            # if a single scalar is given, broadcast that to all levels
            if len(np.shape(np.array(value))) == 0:
                value = np.ones_like(self.Tatm) * value
            elif value.shape != self.Tatm.shape:
                raise ValueError('absorptivity must be a Field, a scalar, or match atm grid dimensions')
        try:
            self.trans = Transmissivity(absorptivity=value,
                                    reflectivity=self.reflectivity,
                                    axis=axis)
        except:
            self.trans = Transmissivity(absorptivity=value, axis=axis)
        self.input['absorptivity'] = value
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
    @property
    def reflectivity(self):
        return self.trans.reflectivity
    @reflectivity.setter
    def reflectivity(self, value):
        #  value should be a Field,
         #  or numpy array of same size as self.Tatm
        try:
            axis = value.domain.axis_index['lev']
        except:
            axis = self.Tatm.domain.axis_index['lev']
            # if a single scalar is given, broadcast that to all levels
            if len(np.shape(np.array(value))) == 0:
                value = np.ones_like(self.Tatm) * value
            elif value.shape != self.Tatm.shape:
                raise ValueError('reflectivity must be a Field, a scalar, or match atm grid dimensions')
        self.trans = Transmissivity(absorptivity=self.absorptivity,
                                    reflectivity=value,
                                    axis=axis)
        #self.input['reflectivity'] = value

    def _compute_emission_sfc(self):
        return self.emissivity_sfc * blackbody_emission(self.Ts)

    def _compute_emission(self):
        return self.emissivity * blackbody_emission(self.Tatm)

    def _compute_fluxes(self):
        ''' All fluxes are band by band'''
        self.emission = self._compute_emission()
        self.emission_sfc = self._compute_emission_sfc()
        fromspace = self._from_space()
        self.flux_down = self.trans.flux_down(fromspace, self.emission)
        self.flux_reflected_up = self.trans.flux_reflected_up(self.flux_down, self.albedo_sfc)
        # this ensure same dimensions as other fields
        self.flux_to_sfc = self.flux_down[..., -1, np.newaxis]
        self.flux_from_sfc = (self.emission_sfc +
                              self.flux_reflected_up[..., -1, np.newaxis])
        self.flux_up = self.trans.flux_up(self.flux_from_sfc,
                            self.emission + self.flux_reflected_up[...,0:-1])
        self.flux_net = self.flux_up - self.flux_down
        # absorbed radiation (flux convergence) in W / m**2 (per band)
        self.absorbed = np.diff(self.flux_net, axis=-1)
        self.absorbed_total = np.sum(self.absorbed, axis=-1)
        self.flux_to_space = self._compute_flux_top()

    def _compute_flux_top(self):
        bandflux = self.flux_up[..., 0, np.newaxis]
        return self._join_channels(bandflux)

    def _flux_convergence_atm(self):
        return self.absorbed

    def _flux_convergence_sfc(self):
        return ( self.flux_to_sfc - self.flux_from_sfc )

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in :math:`W/m^2`.'''
        self._compute_radiative_heating()

    def _compute_radiative_heating(self):
        self._compute_fluxes()
        self.heating_rate['Tatm'] = self._join_channels(self.absorbed)
        self.heating_rate['Ts'] = self._join_channels( self.flux_to_sfc -
                                                       self.flux_from_sfc )

    def _from_space(self):
        try:
            fromspace = self.flux_from_space
        except:
            fromspace = np.zeros_like(self.Ts)
        return self._split_channels(fromspace)

    def _split_channels(self, flux):
        '''Single channel for Grey Gas model.'''
        return flux

    def _join_channels(self, flux):
        '''Single channel for Grey Gas model.'''
        return flux


#####  These routines may need fixing with flipped vertical grid...
    def flux_components_top(self):
        '''Compute the contributions to the outgoing flux to space due to
        emissions from each level and the surface.'''
        N = self.lev.size
        flux_up_bottom = self.flux_from_sfc
        emission = np.zeros_like(self.emission)
        this_flux_up = (np.ones_like(self.Ts) *
                        self.trans.flux_up(flux_up_bottom, emission))
        sfcComponent = this_flux_up[..., -1]
        atmComponents = np.zeros_like(self.Tatm)
        flux_up_bottom = np.zeros_like(self.Ts)
        # I'm sure there's a way to write this as a vectorized operation
        #  but the speed doesn't really matter if it's just for diagnostic
        #  and we are not calling it every timestep
        for n in range(N):
            emission = np.zeros_like(self.emission)
            emission[..., n] = self.emission[..., n]
            this_flux_up = self.trans.flux_up(flux_up_bottom, emission)
            atmComponents[..., n] = this_flux_up[..., -1]
        return sfcComponent, atmComponents

    def flux_components_bottom(self):
        '''Compute the contributions to the downwelling flux to surface due to
        emissions from each level.'''
        N = self.lev.size
        atmComponents = np.zeros_like(self.Tatm)
        flux_down_top = np.zeros_like(self.Ts)
        #  same comment as above... would be nice to vectorize
        for n in range(N):
            emission = np.zeros_like(self.emission)
            emission[..., n] = self.emission[..., n]
            this_flux_down = self.trans.flux_down(flux_down_top, emission)
            atmComponents[..., n] = this_flux_down[..., 0]
        return atmComponents


class GreyGasSW(GreyGas):
    '''Emissivity is always set to zero for shortwave classes.'''
    def __init__(self, albedo_sfc=0.33, emissivity_sfc=0., **kwargs):
        super(GreyGasSW, self).__init__(albedo_sfc=albedo_sfc,
                                        emissivity_sfc=emissivity_sfc,
                                        **kwargs)
    @property
    def emissivity(self):
        # This ensures that emissivity is always zero for shortwave classes
        return np.zeros_like(self.absorptivity)
