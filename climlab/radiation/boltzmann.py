from __future__ import division
from climlab import constants as const
from climlab.process.energy_budget import EnergyBudget


class Boltzmann(EnergyBudget):
    r"""A class for black body radiation.

    Implements a radiation subprocess which computes longwave radiation
    with the Stefan-Boltzmann law for black/grey body radiation.

    According to the Stefan Boltzmann law the total power radiated from an
    object with surface area :math:`A` and temperature :math:`T` (in unit Kelvin)
    can be written as

    .. math::

        P = A \varepsilon \sigma T^4

    where :math:`\varepsilon` is the emissivity of the body.

    As the :class:`~climlab.process.energy_budget.EnergyBudget` of the
    Energy Balance Model is accounted in unit :math:`\textrm{energy} / \textrm{area}`
    (:math:`\textrm{W}/ \textrm{m}^2`)
    the energy budget equation looks like this:

    .. math::

        C \frac{dT}{dt} = R\downarrow - R\uparrow - H  \n

    The :class:`Boltzmann` radiation subprocess represents the outgoing radiation
    :math:`R\uparrow` which then can be written as

    .. math::

        R\uparrow = \varepsilon \sigma T^4

    with state variable :math:`T`.

    **Initialization parameters** \n

    An instance of ``Boltzmann`` is initialized with the following
    arguments:

    :param float eps:   emissivity of the planet's surface which is the
                        effectiveness in emitting energy as thermal radiation
                        [default: 0.65]
    :param float tau:   transmissivity of the planet's atmosphere which is the
                        effectiveness in transmitting the longwave radiation
                        emitted from the surface [default: 0.95]

    **Object attributes** \n

    During initialization both arguments described above are created as object
    attributes which calls their setter function (see below).

    :ivar float eps:            calls the setter function of :func:`eps`
    :ivar float tau:            calls the setter function of :func:`tau`
    :ivar dict diagnostics:     the subprocess's diagnostic dictionary
                                ``self.diagnostic`` is initialized
                                through calling
                                ``self.add_diagnostic('OLR', 0. * self.Ts)``
    :ivar Field OLR:            the subprocess attribute ``self.OLR`` is
                                created with correct dimensions

    :Example:

        Replacing an the regular AplusBT subprocess in an energy balance model::

            >>> import climlab
            >>> from climlab.radiation.Boltzmann import Boltzmann

            >>> # creating EBM model
            >>> model = climlab.EBM()

            >>> print model

        .. code-block:: none
            :emphasize-lines: 7

            climlab Process of type <class 'climlab.model.ebm.EBM'>.
            State variables and domain shapes:
              Ts: (90, 1)
            The subprocess tree:
            top: <class 'climlab.model.ebm.EBM'>
               diffusion: <class 'climlab.dynamics.diffusion.MeridionalDiffusion'>
               LW: <class 'climlab.radiation.AplusBT.AplusBT'>
               albedo: <class 'climlab.surface.albedo.StepFunctionAlbedo'>
                  iceline: <class 'climlab.surface.albedo.Iceline'>
                  cold_albedo: <class 'climlab.surface.albedo.ConstantAlbedo'>
                  warm_albedo: <class 'climlab.surface.albedo.P2Albedo'>
               insolation: <class 'climlab.radiation.insolation.P2Insolation'>

        ::

            >>> #  creating and adding albedo feedback subprocess
            >>> LW_boltz = Boltzmann(eps=0.69, tau=0.98, state=model.state, **model.param)

            >>> # overwriting old 'LW' subprocess with same name
            >>> model.add_subprocess('LW', LW_boltz)

            >>> print model

        .. code-block:: none
            :emphasize-lines: 7

            climlab Process of type <class 'climlab.model.ebm.EBM'>.
            State variables and domain shapes:
              Ts: (90, 1)
            The subprocess tree:
            top: <class 'climlab.model.ebm.EBM'>
               diffusion: <class 'climlab.dynamics.diffusion.MeridionalDiffusion'>
               LW: <class 'climlab.radiation.Boltzmann.Boltzmann'>
               albedo: <class 'climlab.surface.albedo.StepFunctionAlbedo'>
                  iceline: <class 'climlab.surface.albedo.Iceline'>
                  cold_albedo: <class 'climlab.surface.albedo.ConstantAlbedo'>
                  warm_albedo: <class 'climlab.surface.albedo.P2Albedo'>
               insolation: <class 'climlab.radiation.insolation.P2Insolation'>

    """
    # implemented by m-kreuzer
    def __init__(self, eps= 0.65, tau=0.95, **kwargs):
        super(Boltzmann, self).__init__(**kwargs)
        self.eps = eps
        self.tau = tau
        self.add_diagnostic('OLR', 0. * self.Ts)

    @property
    def eps(self):
        """Property of emissivity parameter.

        :getter:    Returns the albedo value which is stored in attribute
                    ``self._eps``
        :setter:    * sets the emissivity which is addressed as ``self._eps``
                      to the new value
                    * updates the parameter dictionary ``self.param['eps']``
        :type:      float

        """
        return self._eps
    @eps.setter
    def eps(self, value):
        self._eps = value
        self.param['eps'] = value

    @property
    def tau(self):
        """Property of the transmissivity parameter.

        :getter:    Returns the albedo value which is stored in attribute
                    ``self._tau``
        :setter:    * sets the emissivity which is addressed as ``self._tau``
                      to the new value
                    * updates the parameter dictionary ``self.param['tau']``
        :type:      float

        """
        return self._tau
    @tau.setter
    def tau(self, value):
        self._tau = value
        self.param['tau'] = value

#    def emission(self):
#        """Calculates the Outgoing Longwave Radiation (OLR) of the Boltzmann
#        radiation subprocess.
#
#        **Object attributes** \n
#
#        During method execution following object attribute is modified:
#
#        :ivar float OLR:            the described formula is calculated and the
#                                    result stored in the project attribute ``self.OLR``
#        :ivar dict diagnostics:     the same result is written in ``diagnostics``
#                                    dictionary with the key ``'OLR'``
#
#        .. warning::
#
#            This currently works only for a single state variable!
#
#        """
#        for varname, value in self.state.iteritems():
#            flux = self.eps * self.tau * const.sigma * (value + const.tempCtoK)**4.
#            self.OLR = flux
#            self.diagnostics['OLR'] = self.OLR

    def _compute_emission(self):
        for varname, value in self.state.items():
            flux = self.eps * self.tau * const.sigma * (value + const.tempCtoK)**4.
            self.OLR[:] = flux


    def _compute_heating_rates(self):
        """Computes energy flux convergences to get heating rates in :math:`W/m^2`.

        """
        self._compute_emission()
        for varname, value in self.state.items():
            self.heating_rate[varname] = -self.OLR
