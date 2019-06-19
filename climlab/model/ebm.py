r"""Convenience classes for pre-made Energy Balance Models in CLIMLAB.

These models all solve some form of the equation

.. math::

    C \frac{\partial}{\partial t} T_s(\phi,t) = (1-\alpha)S(\phi,t) - \left[A + B T_s \right] + \frac{1}{\cos\phi} \frac{\partial}{\partial \phi} \left[ \cos\phi ~ D ~ \frac{\partial T_s}{\partial \phi} \right]

where

- :math:`\phi` is latitude
- :math:`T_s` is a zonally averaged surface temperature
- :math:`C` is a depth-integrated heat capacity
- :math:`\alpha` is an albedo (which may depend on latitude and/or temperature)
- :math:`S(\phi, t)` is the insolation
- :math:`\left[A + B T_s \right]` is a parameterization of the Outgoing Longwave Radiation to space
- the last term on the right hand side is a diffusive heat transport convergence with thermal diffusivity :math:`D` in the same units as :math:`B`

Three classes are provided, which differ in the type of insolation :math:`S`:

- ``climlab.EBM`` uses a steady idealized annual insolation (second Legendre polynomial form)
- ``climlab.EBM_annual`` uses realistic steady annual-mean insolation
- ``climlab.EBM_seasonal`` uses realistic seasonally varying insolation

The ``__init__`` method of class ``EBM`` shows how these models are assembled
from subprocesses representing each term in the above equation.


Building the Moist EBM
----------------------

There is currently no ready-made convenience class for the **moist EBM**,
but it can be readily built by swapping out the dry heat diffusion process ``climlab.dynamics.MeridionalHeatDiffusion``
with the moist equivalent ``climlab.dynamics.MeridionalMoistDiffusion``.

This sort of mixing and matching of model components is at the heart of CLIMLAB
design and functionality.

    :Example:

        .. code-block:: python

            import climlab
            #  create and display a 1D Energy Balance Model
            dry = climlab.EBM()
            print(dry)
            #  clone this model and swap out the diffusion subprocess
            moist = climlab.process_like(dry)
            diff = climlab.dynamics.MeridionalMoistDiffusion(state=moist.state, timestep=moist.timestep)
            moist.add_subprocess('diffusion', diff)
            print(moist)

We can run both models out to equilibrium and compare the results as follows:

    :Example:

        .. code-block:: python

            #  Run both models out to quasi-equilibrium
            #   print out the global mean planetary energy budget -- should be very small
            for m in [dry, moist]:
                m.integrate_years(10)
                print(climlab.global_mean(m.net_radiation))
            #  plot and compare the temperatures
            import matplotlib.pyplot as plt
            plt.figure()
            plt.plot(dry.lat, dry.Ts, label='Dry')
            plt.plot(moist.lat, moist.Ts, label='Moist')
            plt.legend()
            plt.show()
            #  plot and compare the heat transport
            plt.figure()
            plt.plot(dry.lat_bounds, dry.heat_transport, label='Dry')
            plt.plot(moist.lat_bounds, moist.heat_transport, label='Moist')
            plt.legend()
            plt.show()

"""
from __future__ import division
import numpy as np
from climlab import constants as const
from climlab.domain.field import Field, global_mean
from climlab.process import EnergyBudget, TimeDependentProcess
from climlab.utils import legendre
from climlab.domain import domain
from climlab.radiation import AplusBT, P2Insolation, AnnualMeanInsolation, DailyInsolation, SimpleAbsorbedShortwave
from climlab.surface import albedo
from climlab.dynamics import MeridionalHeatDiffusion
from climlab.domain.initial import surface_state
from scipy import integrate

#  A lot of this should be re-written / simplified
#  using more up-to-date climlab APIs for coupling processes together
#  Making sure that each subprocess properly declares inputs and diagnostics

#  For example, the basic EBM should be created with something like
#  ebm = climlab.couple([asr,olr,diff])


class EBM(TimeDependentProcess):
    """A parent class for all Energy-Balance-Model classes.

    This class sets up a typical EnergyBalance Model with following subprocesses:

    * Outgoing Longwave Radiation (OLR) parametrization through
      :class:`~climlab.radiation.AplusBT`
    * Absorbed Shortwave Radiation (ASR) through
      :class:`~climlab.radiation.SimpleAbsorbedShortwave`
    * solar insolation paramtrization through
      :class:`~climlab.radiation.P2Insolation`
    * albedo parametrization in dependence of temperature through
      :class:`~climlab.surface.StepFunctionAlbedo`
    * energy diffusion through
      :class:`~climlab.dynamics.MeridionalHeatDiffusion`

    **Initialization parameters** \n

    An instance of ``EBM`` is initialized with the following
    arguments *(for detailed information see Object attributes below)*:

    :param int num_lat:         number of equally spaced points for the
                                latitue grid. Used for domain intialization of
                                :class:`~climlab.domain.domain.zonal_mean_surface`
                                                                            \n
                                - default value: ``90``
    :param int num_lon:         number of equally spaced points in longitude
                                                                            \n
                                - default value: ``None``
    :param float S0:            solar constant                              \n
                                - unit: :math:`\\frac{\\textrm{W}}{\\textrm{m}^2}`   \n
                                - default value: ``1365.2``
    :param float A:             parameter for linear OLR parametrization
                                :class:`~climlab.radiation.AplusBT.AplusBT` \n
                                - unit: :math:`\\frac{\\textrm{W}}{\\textrm{m}^2}`   \n
                                - default value: ``210.0``
    :param float B:             parameter for linear OLR parametrization
                                :class:`~climlab.radiation.AplusBT.AplusBT` \n
                                - unit: :math:`\\frac{\\textrm{W}}{\\textrm{m}^2 \\ ^{\circ} \\textrm{C}}`   \n
                                - default value: ``2.0``
    :param float D:             diffusion parameter for Meridional Energy Diffusion
                                :class:`~climlab.dynamics.diffusion.MeridionalDiffusion`
                                                                            \n
                                - unit: :math:`\\frac{\\textrm{W}}{\\textrm{m}^2 \\ ^{\circ} \\textrm{C}}`   \n
                                - default value: ``0.555``
    :param float water_depth:   depth of :class:`~climlab.domain.domain.zonal_mean_surface`
                                domain, which the heat capacity is dependent on
                                                                            \n
                                - unit: meters                              \n
                                - default value: ``10.0``
    :param float Tf:            freezing temperature                        \n
                                - unit: :math:`^{\circ} \\textrm{C}`                    \n
                                - default value: ``-10.0``
    :param float a0:            base value for planetary albedo parametrization
                                :class:`~climlab.surface.albedo.StepFunctionAlbedo`
                                                                            \n
                                - unit: dimensionless
                                - default value: ``0.3``
    :param float a2:            parabolic value for planetary  albedo parametrization
                                :class:`~climlab.surface.albedo.StepFunctionAlbedo`
                                                                            \n
                                - unit: dimensionless
                                - default value: ``0.078``
    :param float ai:            value for ice albedo paramerization in
                                :class:`~climlab.surface.albedo.StepFunctionAlbedo`
                                                                            \n
                                - unit: dimensionless
                                - default value: ``0.62``
    :param float timestep:      specifies the EBM's timestep                \n
                                - unit: seconds
                                - default value: (365.2422 * 24 * 60 * 60 ) / 90 \n
                                  -> (90 timesteps per year)
    :param float T0:            base value for initial temperature          \n
                                - unit :math:`^{\circ} \\textrm{C}`         \n
                                - default value: ``12``
    :param float T2:            factor for 2nd Legendre polynomial
                                :class:`~climlab.utils.legendre.P2`
                                to calculate initial temperature            \n
                                - unit: dimensionless
                                - default value: ``40``




    **Object attributes** \n

    Additional to the parent class :class:`~climlab.process.EnergyBudget`
    following object attributes are generated and updated during initialization:

    :ivar dict param:       The parameter dictionary is updated with a couple
                            of the initatilzation input arguments, namely
                            ``'S0'``, ``'A'``, ``'B'``, ``'D'``, ``'Tf'``,
                            ``'water_depth'``, ``'a0'``, ``'a2'`` and ``'ai'``.
    :ivar dict domains:     If the object's ``domains`` and the ``state``
                            dictionaries are empty during initialization
                            a domain ``sfc`` is created through
                            :func:`~climlab.domain.domain.zonal_mean_surface`.
                            In the meantime the object's ``domains`` and
                            ``state`` dictionaries are updated.
    :ivar dict subprocess:  Several subprocesses are created (see above)
                            through calling
                            :func:`~climlab.process.process.Process.add_subprocess`
                            and therefore the subprocess dictionary is updated.
    :ivar bool topdown:     is set to ``False`` to call subprocess compute
                            methods first.
                            See also
                            :class:`~climlab.process.time_dependent_process.TimeDependentProcess`.
    :ivar dict diagnostics: is initialized with keys: ``'OLR'``, ``'ASR'``,
                            ``'net_radiation'``, ``'albedo'``, ``'icelat'`` and
                            ``'ice_area'`` through
                            :func:`~climlab.process.process.Process.add_diagnostic`.

    :Example:

        Creation and integration of the preconfigured Energy Balance Model::

            >>> import climlab
            >>> model = climlab.EBM()

            >>> model.integrate_years(2.)
            Integrating for 180 steps, 730.4844 days, or 2.0 years.
            Total elapsed time is 2.0 years.

        For more information how to use the EBM class, see the :ref:`Tutorial`
        chapter.

    """
    def __init__(self,
                 num_lat=90,
                 num_lon=None,
                 S0=const.S0,
                 s2=-0.48,
                 A=210.,
                 B=2.,
                 D=0.555,  # in W / m^2 / degC, same as B
                 water_depth=10.0,
                 Tf=-10.,
                 a0=0.3,
                 a2=0.078,
                 ai=0.62,
                 timestep=const.seconds_per_year/90.,
                 T0 = 12.,  # initial temperature parameters
                 T2 = -40.,  #  (2nd Legendre polynomial)
                 **kwargs):
        # Check to see if an initial state is already provided
        #  If not, make one
        if 'state' not in kwargs:
            state = surface_state(num_lat=num_lat, num_lon=num_lon,
                                  water_depth=water_depth, T0=T0, T2=T2)
            sfc = state.Ts.domain
            kwargs.update({'state': state, 'domains':{'sfc':sfc}})
        super(EBM, self).__init__(timestep=timestep, **kwargs)
        sfc = self.Ts.domain
        self.param['S0'] = S0
        self.param['s2'] = s2
        self.param['A'] = A
        self.param['B'] = B
        self.param['D'] = D
        self.param['Tf'] = Tf
        self.param['water_depth'] = water_depth
        self.param['a0'] = a0
        self.param['a2'] = a2
        self.param['ai'] = ai
        # create sub-models
        lw = AplusBT(state=self.state, **self.param)
        ins = P2Insolation(domains=sfc, **self.param)
        alb = albedo.StepFunctionAlbedo(state=self.state, **self.param)
        sw = SimpleAbsorbedShortwave(state=self.state,
                                     insolation=ins.insolation,
                                     albedo=alb.albedo,
                                     **self.param)
        diff = MeridionalHeatDiffusion(state=self.state, use_banded_solver=False, **self.param)
        self.add_subprocess('LW', lw)
        self.add_subprocess('insolation', ins)
        self.add_subprocess('albedo', alb)
        self.add_subprocess('SW', sw)
        self.add_subprocess('diffusion', diff)
        self.topdown = False  # call subprocess compute methods first
        self.add_diagnostic('net_radiation', 0.*self.Ts)

    @property
    def S0(self):
        return self.subprocess['insolation'].S0
    @S0.setter
    def S0(self, value):
        self.param['S0'] = value
        self.subprocess['insolation'].S0 = value

    def _compute(self):
        self.net_radiation = self.subprocess['SW'].ASR - self.subprocess['LW'].OLR
        return super(EBM, self)._compute()

    def global_mean_temperature(self):
        """Convenience method to compute global mean surface temperature.

        Calls :func:`~climlab.domain.field.global_mean` method which
        for the object attriute ``Ts`` which calculates the latitude weighted
        global mean of a field.

        :Example:

            Calculating the global mean temperature of initial EBM temperature::

                >>> import climlab
                >>> model = climlab.EBM(T0=14., T2=-25)

                >>> model.global_mean_temperature()
                Field(13.99873037400856)

        """
        return global_mean(self.Ts)

    def inferred_heat_transport(self):
        """Calculates the inferred heat transport by integrating the TOA
        energy imbalance from pole to pole.

        The method is calculating

        .. math::

            H(\\varphi) = 2 \pi R^2 \int_{-\pi/2}^{\\varphi} cos\phi \ R_{TOA} d\phi

        where :math:`R_{TOA}` is the net radiation at top of atmosphere.


        :return: total heat transport on the latitude grid in unit :math:`\\textrm{PW}`
        :rtype: array of size ``np.size(self.lat_lat)``

        :Example:

            .. plot:: code_input_manual/example_EBM_inferred_heat_transport.py
                :include-source:

        """
        phi = np.deg2rad(self.lat)
        energy_in = np.squeeze(self.net_radiation)
        return (1E-15 * 2 * np.math.pi * const.a**2 *
                integrate.cumtrapz(np.cos(phi)*energy_in, x=phi, initial=0.))

    # def heat_transport(self):
    #     """Returns instantaneous heat transport in unit :math:`\\textrm{PW}`
    #     on the staggered grid (bounds) through calling
    #     :func:`diffusive_heat_transport`.
    #
    #     :Example:
    #
    #         .. plot:: code_input_manual/example_EBM_heat_transport.py
    #             :include-source:
    #
    #     """
    #     return self.diffusive_heat_transport()
    #
    def diffusive_heat_transport(self):
        """Compute instantaneous diffusive heat transport in unit :math:`\\textrm{PW}`
        on the staggered grid (bounds) through calculating:

        .. math::

            H(\\varphi) = - 2 \pi R^2 cos(\\varphi) D \\frac{dT}{d\\varphi}
                        \\approx - 2 \pi R^2 cos(\\varphi) D \\frac{\Delta T}{\Delta \\varphi}

        :rtype: array of size ``np.size(self.lat_bounds)``

        THIS IS DEPRECATED AND WILL BE REMOVED IN THE FUTURE. Use the diagnostic
        ``heat_transport`` instead, which implements the same calculation.
        """
        phi = np.deg2rad(self.lat)
        phi_stag = np.deg2rad(self.lat_bounds)
        D = self.param['D']
        T = np.squeeze(self.Ts)
        dTdphi = np.diff(T) / np.diff(phi)
        dTdphi = np.append(dTdphi, 0.)
        dTdphi = np.insert(dTdphi, 0, 0.)
        return (1E-15*-2*np.math.pi*np.cos(phi_stag)*const.a**2*D*dTdphi)

    # def heat_transport_convergence(self):
    #     """Returns instantaneous convergence of heat transport.
    #
    #     .. math::
    #
    #         h(\\varphi) = - \\frac{1}{2 \pi R^2 cos(\\varphi)} \\frac{dH}{d\\varphi}
    #                     \\approx - \\frac{1}{2 \pi R^2 cos(\\varphi)} \\frac{\Delta H}{\Delta \\varphi}
    #
    #     h is the *dynamical heating rate* in unit :math:`\\textrm{W}/ \\textrm{m}^2`
    #     which is the convergence of energy transport into each latitude band,
    #     namely the difference between what's coming in and what's going out.
    #
    #     :Example:
    #
    #         .. plot:: code_input_manual/example_EBM_heat_transport_convergence.py
    #             :include-source:
    #
    #     """
    #     phi = np.deg2rad(self.lat)
    #     phi_stag = np.deg2rad(self.lat_bounds)
    #     H = 1.E15*self.heat_transport()
    #     return (-1./(2*np.math.pi*const.a**2*np.cos(phi)) *
    #             np.diff(H)/np.diff(phi_stag))


class EBM_seasonal(EBM):
    def __init__(self, a0=0.33, a2=0.25, ai=None, **kwargs):
        """A class that implements Energy Balance Models with realistic
        daily insolation.

        This class is inherited from the general :class:`~climlab.EBM`
        class and uses the insolation subprocess
        :class:`~climlab.radiation.DailyInsolation` instead of
        :class:`~climlab.radiation.P2Insolation` to compute a
        realisitc distribution of solar radiation on a daily basis.

        If argument for ice albedo ``'ai'`` is not given, the model will not
        have an albedo feedback.

        An instance of ``EBM_seasonal`` is initialized with the following
        arguments:

        :param float a0:            base value for planetary albedo parametrization
                                    :class:`~climlab.surface.albedo.StepFunctionAlbedo`
                                    [default: 0.33]
        :param float a2:            parabolic value for planetary  albedo parametrization
                                    :class:`~climlab.surface.albedo.StepFunctionAlbedo`
                                    [default: 0.25]
        :param float ai:            value for ice albedo paramerization in
                                    :class:`~climlab.surface.albedo.StepFunctionAlbedo`
                                    (optional)


        **Object attributes** \n

        Following object attributes are updated during initialization: \n

        :ivar dict param:       The parameter dictionary is updated with
                                ``'a0'`` and ``'a2'``.
        :ivar dict subprocess:  suprocess ``'insolation'`` is overwritten by
                                :class:`~climlab.radiation.insolation.DailyInsolation`.

        *if* ``'ai'`` *is not given*:

        :ivar dict param:       ``'ai'`` and ``'Tf'`` are removed from the
                                parameter dictionary (initialized by parent class
                                :class:`~climlab.model.ebm.EBM`)
        :ivar dict subprocess:  suprocess ``'albedo'`` is overwritten by
                                :class:`~climlab.surface.albedo.P2Albedo`.

        *if* ``'ai'`` *is given*:

        :ivar dict param:       The parameter dictionary is updated with
                                ``'ai'``.
        :ivar dict subprocess:  suprocess ``'albedo'`` is overwritten by
                                :class:`~climlab.surface.albedo.StepFunctionAlbedo`
                                (which basically has been there before but now is
                                updated with the new albedo parameter values).
        :Example:

            The annual distribution of solar insolation:

            .. plot:: code_input_manual/example_EBM_seasonal.py
                :include-source:

        """
        if ai is None:
            no_albedo_feedback = True
            ai = 0. # ignored but need to set a number
        else:
            no_albedo_feedback = False
        super(EBM_seasonal, self).__init__(a0=a0, a2=a2, ai=ai, **kwargs)
        self.param['a0'] = a0
        self.param['a2'] = a2
        sfc = self.domains['Ts']
        ins = DailyInsolation(domains=sfc, **self.param)
        if no_albedo_feedback:
            # Remove unused parameters here for clarity
            _ = self.param.pop('ai')
            _ = self.param.pop('Tf')
            alb = albedo.P2Albedo(domains=sfc, **self.param)
        else:
            self.param['ai'] = ai
            alb = albedo.StepFunctionAlbedo(state=self.state, **self.param)
        sw = SimpleAbsorbedShortwave(state=self.state,
                                     insolation=ins.insolation,
                                     albedo=alb.albedo,
                                     **self.param)
        self.add_subprocess('insolation', ins)
        self.add_subprocess('albedo', alb)
        self.add_subprocess('SW', sw)


class EBM_annual(EBM_seasonal):
    def __init__(self, **kwargs):
        """A class that implements Energy Balance Models with annual mean insolation.

        The annual solar distribution is calculated through averaging the
        :class:`~climlab.radiation.insolation.DailyInsolation` over time
        which has been used in used in the parent class
        :class:`~climlab.EBM_seasonal`. That is done by the subprocess
        :class:`~climlab.radiation.AnnualMeanInsolation` which is
        more realistic than the :class:`~climlab.radiation.P2Insolation`
        module used in the classical :class:`~climlab.EBM` class.

        According to the parent class :class:`~climlab.EBM_seasonal`
        the model will not have an ice-albedo feedback, if albedo ice parameter
        ``'ai'`` is not given. For details see there.


        **Object attributes** \n

        Following object attributes are updated during initialization: \n

        :ivar dict subprocess:  suprocess ``'insolation'`` is overwritten by
                                :class:`~climlab.radiation.AnnualMeanInsolation`

        :Example:

            The :class:`~climlab.EBM_annual` class uses a different
            insolation subprocess than the :class:`~climlab.EBM` class::

                >>> import climlab
                >>> model_annual = climlab.EBM_annual()

                >>> print model_annual

            .. code-block:: none
               :emphasize-lines: 9

                climlab Process of type <class 'climlab.model.ebm.EBM_annual'>.
                State variables and domain shapes:
                  Ts: (90, 1)
                The subprocess tree:
                top: <class 'climlab.EBM_annual'>
                   diffusion: <class 'climlab.dynamics.MeridionalHeatDiffusion'>
                   LW: <class 'climlab.radiation.AplusBT'>
                   albedo: <class 'climlab.surface.P2Albedo'>
                   insolation: <class 'climlab.radiation.AnnualMeanInsolation'>

        """
        super(EBM_annual, self).__init__(**kwargs)
        sfc = self.domains['Ts']
        ins = AnnualMeanInsolation(domains=sfc, **self.param)
        self.add_subprocess('insolation', ins)
        self.subprocess['SW'].insolation = ins.insolation

# an EBM that computes degree-days has an additional state variable.
#  Need to implement that
#  could make a good working example to document creating a new model class




#==============================================================================
#
# class _EBM(TimeDependentProcess):
#     def __init__(self, num_points=90, K=0.555, **kwargs):
#         # first create the model domains
#         doms = domain.zonal_mean_surface(num_points=num_points)
#         # initial surface temperature
#         lat = doms['sfc'].grid['lat'].points
#         initial = {}
#         initial['Ts'] = 12. - 40. * legendre.P2(np.sin(np.deg2rad(lat)))
#          #  Create process data structures
#         super(_EBM, self).__init__(domains=doms, state=initial, **kwargs)
#         #  first set all parameters to sensible default values
#         #self.num_points = num_points
#         # self.K = 2.2E6  # in m^2 / s
#         self.K = 0.555  # in W / m^2 / degC, same as B
#         self.A = 210.
#         self.B = 2.
#         # self.water_depth =  10.0
#         self.Tf = 0.0
#         self.S0 = const.S0
#         self.make_grid()
#         # self.albedo_noice = 0.303 + 0.0779 * P2( np.sin( self.phi ) )
#         self.albedo_noice = 0.33 + 0.25 * legendre.P2(np.sin(self.phi))
#         # self.albedo_ice = 0.62 * np.ones_like( self.phi )
#         self.albedo_ice = self.albedo_noice  # default to no albedo feedback
#         self.T = 12. - 40. * legendre.P2(np.sin(self.phi))
#         #  A dictionary of the model state variables
#         self.state = {'T': self.T}
#         self.positive_degree_days = np.zeros_like(self.phi)
#         # self.make_insolation_array()  # now called from inside set_timestep()
#         self.external_heat_source = np.zeros_like(self.phi)
#         self.set_timestep()
#
#     def make_grid(self):
#         '''Build the grid for the computation, evenly spaced in latitude.'''
#         #   dlat will be our grid spacing
#         #   lat will be our temperature grid:
#         #     an array with exactly num_points evenly spaced points
#         #   lat_stag will be a staggered grid with numpoints+1 points,
#         #     where the end points are the North and South poles
#         #  Then we convert these all to radians for the computation.
#         self.dlat = 180. / self.num_points
#         self.lat = np.linspace(-90. + self.dlat/2,
#                                90. - self.dlat/2, self.num_points)
#         self.lat_stag = np.linspace(-90., 90., self.num_points+1)
#         self.dphi = np.deg2rad(self.dlat)
#         self.phi = np.deg2rad(self.lat)
#         self.phi_stag = np.deg2rad(self.lat_stag)
#
#     def set_timestep(self, num_steps_per_year=90):
#         '''Change the timestep, given a number of steps per calendar year.'''
#         super(_EBM, self).set_timestep(num_steps_per_year)
#         self.set_water_depth()
#         self.make_insolation_array()
#
#     def set_water_depth(self, water_depth=10.):
#         '''Method for changing the water depth (heat capacity) with depth in m.
#         Also recomputes the tridiagonal diffusion matrix.'''
#         if water_depth is None:
#             try:
#                 water_depth = self.water_depth
#             except:
#                 ValueError("water_depth parameter is not specified.")
#         self.water_depth = water_depth
#         self.C = const.cw * const.rho_w * self.water_depth
#         self.delta_time_over_C = self.timestep / self.C
#         self.set_diffusivity(self.K)
#
#     def set_diffusivity(self, K=None):
#         '''Method for changing the diffusivity, with K in W/m^2/degC.
#         Recomputes the tridiagonal diffusion matrix.'''
#         if K is None:
#             try:
#                 K = self.K
#             except:
#                 ValueError("Diffusivity parameter K is not specified.")
#         self.K = K
#         self.diffTriDiag = self._make_diffusion_matrix()
#
#     def _make_diffusion_matrix(self):
#         J = self.num_points
#         # Ka = (const.cp * const.ps * const.mb_to_Pa / const.g / const.a**2 *
#         #        self.K * np.ones_like(self.phi_stag))
#         # cosKa = np.cos(self.phi_stag) * Ka
#         cosKa = np.cos(self.phi_stag) * self.K
#         Ka1 = (cosKa[0:J] / np.cos(self.phi) *
#                self.delta_time_over_C / self.dphi**2)
#         Ka3 = (cosKa[1:J+1] / np.cos(self.phi) *
#                self.delta_time_over_C / self.dphi**2)
#         Ka2 = np.insert(Ka1[1:J], 0, 0) + np.append(Ka3[0:J-1], 0)
#         #  Atmosphere tridiagonal matrix
#         diag = np.empty((3, J))
#         diag[0, 1:] = -Ka3[0:J-1]
#         diag[1, :] = 1 + Ka2
#         diag[2, 0:J-1] = -Ka1[1:J]
#         return diag
#
#     def compute_OLR(self):
#         return self.A + self.B * self.T
#
#     def make_insolation_array(self):
#         # will be overridden by daughter classes
#         raise NotImplementedError("Subclasses of _EBM must implement a method for computing insolation.")
#
#     def compute_insolation(self):
#         return self.insolation_array[:, self.day_of_year_index]
#
#     def compute_albedo(self):
#         '''Simple step-function albedo based on ice line at temperature Tf.'''
#         return np.where(self.T >= self.Tf, self.albedo_noice, self.albedo_ice)
#
#     def compute_radiation(self):
#         self.ASR = (1 - self.compute_albedo()) * self.compute_insolation()
#         self.OLR = self.compute_OLR()
#         self.net_radiation = self.ASR - self.OLR
#
#     def step_forward(self):
#         self.compute_radiation()
#         #  updated temperature due to radiation:
#         Trad = (self.T + (self.net_radiation + self.external_heat_source) *
#                 self.delta_time_over_C)
#         # Time-stepping the diffusion is just inverting this matrix problem:
#         # self.T = np.linalg.solve( self.diffTriDiag, Trad )
#         self.T = solve_banded((1, 1), self.diffTriDiag, Trad)
#         self.positive_degree_days += self.compute_degree_days()
#         super(_EBM, self).step_forward()
#
#     def compute_degree_days(self, threshold=0.):
#         """Return temperature*time in degree-days,
#         wherever temperature is above the threshold, otherwise zero."""
#         return np.where(self.T > threshold, self.T * self.timestep /
#                         const.seconds_per_day, np.zeros_like(self.T))
#
#     def do_new_calendar_year(self):
#         """This function is called once at the end of every calendar year."""
#         super(_EBM, self).do_new_calendar_year()
#         self.previous_positive_degree_days = self.positive_degree_days
#         self.positive_degree_days = np.zeros_like(self.phi)
#
#     def heat_transport(self):
#         '''Returns instantaneous heat transport in units on PW,
#         on the staggered grid.'''
#         return self.diffusive_heat_transport()
#
#     def diffusive_heat_transport( self ):
#         '''Compute instantaneous diffusive heat transport in units of PW, on the staggered grid.'''
#         #return ( 1E-15 * -2 * np.math.pi * np.cos(self.phi_stag) * const.cp * const.ps * const.mb_to_Pa / const.g * self.K *
#         #    np.append( np.append( 0., np.diff( self.T ) ), 0.) / self.dphi )
#         return ( 1E-15 * -2 * np.math.pi * np.cos(self.phi_stag) * const.a**2 * self.K *
#             np.append( np.append( 0., np.diff( self.T ) ), 0.) / self.dphi )
#
#     def heat_transport_convergence( self ):
#         '''Returns instantaneous convergence of heat transport in units of W / m^2.'''
#         return ( -1./(2*np.math.pi*const.a**2*np.cos(self.phi)) * np.diff( 1.E15*self.heat_transport() )
#             / np.diff(self.phi_stag) )
#
#     def inferred_heat_transport( self ):
#         '''Returns the inferred heat transport (in PW) by integrating the TOA energy imbalance from pole to pole.'''
#         return ( 1E-15 * 2 * np.math.pi * const.a**2 * integrate.cumtrapz( np.cos(self.phi)*self.net_radiation,
#             x=self.phi, initial=0. ) )
#
#     def find_icelines( self ):
#         '''Returns the instantaneous latitudes of any ice edges.'''
#         # This probably won't work in cases with multiple ice lines per hemisphere!
#         #  Revise!
#         iceindices = np.squeeze( np.where( self.T < self.Tf ) )
#         if iceindices.size == 0:
#             return 90.
#         elif iceindices.size == self.lat.size:
#             return 0.
#         else:
#             icelines = np.squeeze( np.where( np.diff(iceindices)>1) )
#             icelat1 = self.lat_stag[ iceindices[icelines]+1 ]
#             icelat2 = self.lat_stag[ iceindices[icelines+1] ]
#             return icelat1, icelat2
#
#     def global_mean( self, field ):
#         '''Compute the area-weighted global mean of a vector field on the latitude grid.'''
#       #return np.sum( field * np.cos( self.phi ) ) / np.sum( np.cos( self.phi ) )
#       return global_mean( field, self.phi )
#
#   def global_mean_temperature( self ):
#       '''Convenience method to compute global mean temperature.'''
#       return self.global_mean( self.T )
#==============================================================================



#==============================================================================
# class EBM_landocean( EBM_seasonal ):
#     '''A model with both land and ocean, based on North and Coakley (1979)
#     Essentially just invokes two different EBM_seasonal objects, one for ocean, one for land.
#     '''
#     def __str__(self):
#         return ( "Instance of EBM_landocean class with " +  str(self.num_points) + " latitude points." )
#
#     def __init__( self, num_points = 90 ):
#         super(EBM_landocean,self).__init__( num_points )
#         self.land_ocean_exchange_parameter = 1.0  # in W/m2/K
#
#         self.land = EBM_seasonal( num_points )
#         self.land.make_insolation_array( self.orb )
#         self.land.Tf = 0.
#         self.land.set_timestep( timestep = self.timestep )
#         self.land.set_water_depth( water_depth = 2. )
#
#         self.ocean = EBM_seasonal( num_points )
#         self.ocean.make_insolation_array( self.orb )
#         self.ocean.Tf = -2.
#         self.ocean.set_timestep( timestep = self.timestep )
#         self.ocean.set_water_depth( water_depth = 75. )
#
#         self.land_fraction = 0.3 * np.ones_like( self.land.phi )
#         self.C_ratio = self.land.water_depth / self.ocean.water_depth
#         self.T = self.zonal_mean_temperature()
#
#     def zonal_mean_temperature( self ):
#         return self.land.T * self.land_fraction + self.ocean.T * (1-self.land_fraction)
#
#     def step_forward( self ):
#         #  note.. this simple implementation is possibly problematic
#         # because the exchange should really occur simultaneously with radiation
#         # and before the implicit heat diffusion
#         self.exchange = (self.ocean.T - self.land.T) * self.land_ocean_exchange_parameter
#         self.land.step_forward()
#         self.ocean.step_forward()
#         self.land.T += self.exchange / self.land_fraction * self.land.delta_time_over_C
#         self.ocean.T -= self.exchange / (1-self.land_fraction) * self.ocean.delta_time_over_C
#         self.T = self.zonal_mean_temperature()
#         self.update_time()
#
#     #   This code should be more accurate, but it's ungainly and seems to produce just about the same result.
#     #def step_forward( self ):
#     #    self.exchange = (self.ocean.T - self.land.T) * self.land_ocean_exchange_parameter
#     #    self.land.compute_radiation( )
#     #    self.ocean.compute_radiation( )
#     #    Trad_land = ( self.land.T + ( self.land.net_radiation + self.exchange / self.land_fraction )
#     #        * self.land.delta_time_over_C )
#     #    Trad_ocean = ( self.ocean.T + ( self.ocean.net_radiation - self.exchange / (1-self.land_fraction) )
#     #        * self.ocean.delta_time_over_C )
#     #    self.land.T = solve_banded((1,1), self.land.diffTriDiag, Trad_land )
#     #    self.ocean.T = solve_banded((1,1), self.ocean.diffTriDiag, Trad_ocean )
#     #    self.T = self.zonal_mean_temperature()
#     #    self.land.update_time()
#     #    self.ocean.update_time()
#     #    self.update_time()
#
#     def integrate_years(self, years=1.0, verbose=True ):
#         #  Here we make sure that both sub-models have the current insolation.
#         self.land.make_insolation_array( self.orb )
#         self.ocean.make_insolation_array( self.orb )
#        super(EBM_landocean,self).integrate_years( years, verbose )
#==============================================================================


#  To do:
#    -  use integrated positive degree days to calculate implicit ice sheet melt potential
#    - also use these to set up a version of the model with vegetation-albedo feedback
#   - Create option to have specified extra ocean heat transport in the ocean component
#   - Create a default land-fraction that looks more like reality for the land-ocean model
#    - add diffusion of moist static energy
#  (would require re-computing the diffusion operator at each timestep, probably somewhat slower)

#==============================================================================
#
# class EBM_annual_moist( EBM_annual ):
#     def __str__(self):
#         return ( "Instance of EBM_annual_moist class with " +  str(self.num_points) + " latitude points  \n" +
#           "and global mean temperature " + str(self.global_mean_temperature()) + " degrees C.")
#
#     def __init__( self, num_points = 90 ):
#         _EBM.__init__( self, num_points )
#         self.K0 = self.K  # constant
#         self.Kperdegree = self.K0/20. # 5% increase per degree
#         self.Tref = 15.
#         self.set_diffusivity( K = self.compute_K() )
#
#     def compute_K(self):
#         # formula to compute diffusivity, linear in global mean temperature
#         return self.K0 + self.Kperdegree * (self.global_mean_temperature()-self.Tref)
#
#     def step_forward( self ):
#         # set the diffusivity, depends on global mean temperature
#         self.set_diffusivity( K = self.compute_K() )
#         _EBM.step_forward(self)
#==============================================================================
