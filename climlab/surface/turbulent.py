'''Processes for surface turbulent heat and moisture fluxes

:class:`~climlab.surface.SensibleHeatFlux` and
:class:`~climlab.surface.LatentHeatFlux` implement standard bulk formulae
for the turbulent heat fluxes, assuming that the heating or moistening
occurs in the lowest atmospheric model level.

        :Example:

            Here is an example of setting up a single-column
            Radiative-Convective model with interactive water vapor
            and surface latent and sensible heat fluxes.

            This example also demonstrates *asynchronous coupling*:
            the radiation uses a longer timestep than the other model components::

                import numpy as np
                import climlab
                from climlab import constants as const
                # Temperatures in a single column
                full_state = climlab.column_state(num_lev=30, water_depth=2.5)
                temperature_state = {'Tatm':full_state.Tatm,'Ts':full_state.Ts}
                #  Initialize a nearly dry column (small background stratospheric humidity)
                q = np.ones_like(full_state.Tatm) * 5.E-6
                #  Add specific_humidity to the state dictionary
                full_state['q'] = q
                #  ASYNCHRONOUS COUPLING -- the radiation uses a much longer timestep
                #  The top-level model
                model = climlab.TimeDependentProcess(state=full_state,
                                              timestep=const.seconds_per_hour)
                #  Radiation coupled to water vapor
                rad = climlab.radiation.RRTMG(state=temperature_state,
                                              specific_humidity=full_state.q,
                                              albedo=0.3,
                                              timestep=const.seconds_per_day
                                              )
                #  Convection scheme -- water vapor is a state variable
                conv = climlab.convection.EmanuelConvection(state=full_state,
                                              timestep=const.seconds_per_hour)
                #  Surface heat flux processes
                shf = climlab.surface.SensibleHeatFlux(state=temperature_state, Cd=0.5E-3,
                                              timestep=const.seconds_per_hour)
                lhf = climlab.surface.LatentHeatFlux(state=full_state, Cd=0.5E-3,
                                              timestep=const.seconds_per_hour)
                #  Couple all the submodels together
                model.add_subprocess('Radiation', rad)
                model.add_subprocess('Convection', conv)
                model.add_subprocess('SHF', shf)
                model.add_subprocess('LHF', lhf)
                print(model)

                #  Run the model
                model.integrate_years(1)
                #  Check for energy balance
                print(model.ASR - model.OLR)
'''
from __future__ import division
import numpy as np
from climlab.utils.thermo import qsat
from climlab import constants as const
from climlab.process.energy_budget import EnergyBudget
from climlab.domain.field import Field


class _SurfaceFlux(EnergyBudget):
    '''Abstract parent class for SensibleHeatFlux and LatentHeatFlux'''
    def __init__(self, Cd=3E-3, resistance=1., **kwargs):
        super(_SurfaceFlux, self).__init__(**kwargs)
        self.Cd = Cd
        self.add_input('resistance', resistance)
        self.heating_rate['Tatm'] = np.zeros_like(self.Tatm)
        #  fixed wind speed (for now)
        self.add_input('U', 5. * np.ones_like(self.Ts))
        #  retrieving surface pressure from model grid
        self.ps = self.lev_bounds[-1]

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in :math:`W/m^2`.'''
        self._compute_flux()
        self.heating_rate['Ts'] = -self._flux
        # Modify only the lowest model level
        self.heating_rate['Tatm'][..., -1, np.newaxis] = self._flux

    def _air_density(self, Ta):
        return self.ps * const.mb_to_Pa / const.Rd / Ta


class SensibleHeatFlux(_SurfaceFlux):
    r'''Surface turbulent sensible heat flux implemented through a bulk aerodynamic formula.

    The flux is computed from

    .. math::

        SH = r ~ c_p ~\rho ~ C_D ~ U \left( T_s - T_a \right)


    where:

    - :math:`c_p` and :math:`\rho` are the specific heat and density of air
    - :math:`C_D` is a drag coefficient (stored as ``self.Cd``, default value is 3E-3)
    - :math:`U` is the near-surface wind speed, stored as ``self.U``, default value is 5 m/s
    - :math:`r` is an optional resistance parameter (stored as ``self.resistance``, default value = 1)

    The surface temperature :math:`T_s` is taken directly from ``self.state['Ts']``,
    while the near-surface air temperature :math:`T_a` is taken as the lowest model
    level in ``self.state['Tatm']``

    Diagnostic quantity ``self.SHF`` gives the sensible heat flux in W/m2.

    Temperature tendencies associated with this flux are computed for
    ``Ts`` and for the lowest model level in ``Tatm``. All other tendencies
    (including air temperature tendencies at other levels) are set to zero.
    '''
    def __init__(self, Cd=3E-3, **kwargs):
        super(SensibleHeatFlux, self).__init__(Cd=Cd, **kwargs)
        self.add_diagnostic('SHF', 0.*self.Ts)

    def _compute_flux(self):
        # this ensure same dimensions as Ts
        #  (and use only the lowest model level)
        Ta = Field(self.Tatm[..., -1, np.newaxis], domain=self.Ts.domain)
        Ts = self.Ts
        DeltaT = Ts - Ta
        rho = self._air_density(Ta)
        #  flux from bulk formula
        self._flux = self.resistance * const.cp * rho * self.Cd * self.U * DeltaT
        self.SHF = self._flux



class LatentHeatFlux(_SurfaceFlux):
    r'''Surface turbulent latent heat flux implemented through a bulk aerodynamic formula.

    The flux is computed from

    .. math::

        LH = r ~ L ~\rho ~ C_D ~ U \left( q_s - q_a \right)


    where:

    - :math:`L` and :math:`\rho` are the latent heat of vaporization and density of air
    - :math:`C_D` is a drag coefficient (stored as ``self.Cd``, default value is 3E-3)
    - :math:`U` is the near-surface wind speed, stored as ``self.U``, default value is 5 m/s
    - :math:`r` is an optional resistance parameter (stored as ``self.resistance``, default value = 1)

    The surface specific humidity :math:`q_s` is computed as the saturation specific
    humidity at the surface temperature ``self.state['Ts']`` and surface pressure
    ``self.ps``, while the near-surface specific humidity :math:`q_a` is taken as the lowest model
    level in the field ``self.q`` (which must be provided either as a state variable or as input).

    Two diagnostics are computed:

    - ``self.LHF`` gives the sensible heat flux in W/m2.
    - ``self.evaporation`` gives the evaporation rate in kg/m2/s (or mm/s)

    How the tendencies are computed depends on whether specific humidity ``q``
    is a state variable (i.e. is present in ``self.state``):

    - If ``q`` is in ``self.state`` then the evaporation determines the specific humidity tendency ``self.tendencies['q']``. The water vapor is added to the lowest model level only. Evaporation cools the surface through the surface tendency ``self.tendencies['Ts']``. Air temperature tendencies are zero everywhere.
    - If ``q`` is not in ``self.state`` then we compute an equivalent air temperature tendency for the lowest model layer instead of a specific humidity tendency (i.e. the latent heat flux is applied in the same way as a sensible heat flux).

    This process does not apply a tendency to the surface water amount.
    In the absence of other water processes this implies an infinite water source at the surface (slab ocean).
    '''
    def __init__(self, Cd=3E-3, **kwargs):
        super(LatentHeatFlux, self).__init__(Cd=Cd, **kwargs)
        self.add_diagnostic('LHF', 0.*self.Ts)
        self.add_diagnostic('evaporation', 0.*self.Ts)  # in kg/m2/s or mm/s

    def _compute_flux(self):
        #  specific humidity at lowest model level
        #  assumes pressure is the last axis
        q = Field(self.q[..., -1, np.newaxis], domain=self.Ts.domain)
        Ta = Field(self.Tatm[..., -1, np.newaxis], domain=self.Ts.domain)
        qs = qsat(self.Ts, self.ps)
        Deltaq = Field(qs - q, domain=self.Ts.domain)
        rho = self._air_density(Ta)
        #  flux from bulk formula
        self._flux = self.resistance * const.Lhvap * rho * self.Cd * self.U * Deltaq
        self.LHF[:] = self._flux
        # evporation rate, convert from W/m2 to kg/m2/s (or mm/s)
        self.evaporation[:] = self.LHF/const.Lhvap

    def _compute(self):
        '''Overides the _compute method of EnergyBudget'''
        tendencies = self._temperature_tendencies()
        if 'q' in self.state:
            # in a model with active water vapor, this flux should affect
            #  water vapor tendency, NOT air temperature tendency!
            tendencies['Tatm'] *= 0.
            Pa_per_hPa = 100.
            air_mass_per_area = self.Tatm.domain.lev.delta[...,-1] * Pa_per_hPa / const.g
            specific_humidity_tendency = 0.*self.q
            specific_humidity_tendency[...,-1,np.newaxis] = self.LHF/const.Lhvap / air_mass_per_area
            tendencies['q'] = specific_humidity_tendency
        return tendencies
