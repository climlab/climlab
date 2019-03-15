from __future__ import division
from climlab.process.energy_budget import EnergyBudget
from climlab.utils import constants as const
import numpy as np


class AplusBT(EnergyBudget):
    r"""The simplest linear longwave radiation module.

    Calculates the Outgoing Longwave Radation (OLR) :math:`R\uparrow` as

    .. math::

        R\uparrow = A + B \cdot T

    where :math:`T` is the state variable.

    Should be invoked with a single temperature state variable only.


    **Initialization parameters** \n

    An instance of ``AplusBT`` is initialized with the following
    arguments:

    :param float A:             parameter for linear OLR parametrization   \n
                                - unit: :math:`\frac{\textrm{W}}{\textrm{m}^2}` \n
                                - default value: ``200.0``
    :param float B:             parameter for linear OLR parametrization   \n
                                - unit: :math:`\frac{\textrm{W}} {\textrm{m}^2 ^{\circ}\textrm{C}}`  \n
                                - default value: ``2.0``

    **Object attributes** \n

    Additional to the parent class :class:`~climlab.process.energy_budget.EnergyBudget`
    following object attributes are generated or modified during initialization:

    :ivar float A:              calls the setter function of :func:`A`
    :ivar float B:              calls the setter function of :func:`B`
    :ivar dict diagnostics:     key ``'OLR'`` initialized with value:
                                :class:`~climlab.domain.field.Field` of zeros
                                in size of ``self.Ts``
    :ivar Field OLR:            the subprocess attribute ``self.OLR`` is
                                created with correct dimensions


    .. warning::

        This module currently works only for a single state variable!

    :Example:

        Simple linear radiation module (stand alone)::

            >>> import climlab

            >>> # create a column atmosphere and scalar surface
            >>> sfc, atm = climlab.domain.single_column()

            >>> # Create a state variable
            >>> Ts = climlab.Field(15., domain=sfc)

            >>> # Make a dictionary of state variables
            >>> s = {'Ts': Ts}

            >>> # create process
            >>> olr = climlab.radiation.AplusBT(state=s)

            >>> print olr
            climlab Process of type <class 'climlab.radiation.AplusBT.AplusBT'>.
            State variables and domain shapes:
              Ts: (1,)
            The subprocess tree:
            top: <class 'climlab.radiation.AplusBT.AplusBT'>

            >>> # to compute tendencies and diagnostics
            >>> olr.compute()

            >>> #  or to actually update the temperature
            >>> olr.step_forward()

            >>> print olr.state
            {'Ts': Field([ 5.69123176])}

    """
    def __init__(self, A=200., B=2., **kwargs):
        super(AplusBT, self).__init__(**kwargs)
        self.A = A
        self.B = B
        self.add_diagnostic('OLR', 0. * self.Ts)

    @property
    def A(self):
        """Property of AplusBT parameter A.

        :getter:    Returns the parameter A which is stored in attribute
                    ``self._A``
        :setter:    * sets parameter A which is addressed as ``self._A``
                      to the new value
                    * updates the parameter dictionary ``self.param['A']``
        :type:      float

        :Example:

            ::

                >>> import climlab
                >>> model = climlab.EBM()

                >>> # getter
                >>> model.subprocess['LW'].A
                210.0
                >>> # setter
                >>> model.subprocess['LW'].A = 220
                >>> # getter again
                >>> model.subprocess['LW'].A
                220

                >>> # subprocess parameter dictionary
                >>> model.subprocess['LW'].param['A']
                220

        """
        return self._A
    @A.setter
    def A(self, value):
        self._A = value
        self.param['A'] = value
    @property
    def B(self):
        """Property of AplusBT parameter B.

        :getter:    Returns the parameter B which is stored in attribute
                    ``self._B``
        :setter:    * sets parameter B which is addressed as ``self._B``
                      to the new value
                    * updates the parameter dictionary ``self.param['B']``
        :type:      float

        """
        return self._B
    @B.setter
    def B(self, value):
        self._B = value
        self.param['B'] = value

    def _compute_emission(self):
        for varname, value in self.state.items():
            self.OLR[:] = self.A + self.B * value

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in :math:`W/m^2`,'''
        self._compute_emission()
        for varname, value in self.state.items():
            self.heating_rate[varname] = -self.OLR


class AplusBT_CO2(EnergyBudget):
    """Linear longwave radiation module considering CO2 concentration.

    This radiation subprocess is based in the idea to linearize the Outgoing
    Longwave Radiation (OLR) emitted to space according to the surface temperature
    (see :class:`AplusBT`).

    To consider a the change of the greenhouse effect through range of
    :math:`CO_2` in the atmosphere, the parameters A and B are computed like
    the following:

    .. math::

        A(c) = -326.4 + 9.161 c - 3.164 c^2 + 0.5468 c^3            \n
        B(c) =  1.953 - 0.04866 c + 0.01309 c^2 - 0.002577 c^3

    where :math:`c=\\log \\frac{p}{300}` and :math:`p` represents
    the concentration of :math:`CO_2` in the atmosphere.

    For further reading see :cite:`Caldeira_1992`.


    **Initialization parameters** \n

    An instance of ``AplusBT_CO2`` is initialized with the following
    argument:

    :param float CO2:   The concentration of :math:`CO_2` in the atmosphere.
                        Referred to as :math:`p` in the above given formulas.\n
                        - unit: :math:`\\textrm{ppm}` (parts per million)   \n
                        - default value: ``300.0``


    **Object attributes** \n

    Additional to the parent class :class:`~climlab.process.energy_budget.EnergyBudget`
    following object attributes are generated or updated during initialization:

    :ivar float CO2:                calls the setter function of :func:`CO2`
    :ivar dict diagnostics:         the subprocess's diagnostic dictionary
                                    ``self.diagnostic`` is initialized
                                    through calling
                                    ``self.add_diagnostic('OLR', 0. * self.Ts)``
    :ivar Field OLR:                the subprocess attribute ``self.OLR`` is
                                    created with correct dimensions

    :Example:

        Replacing an the regular AplusBT subprocess in an energy balance model::

            >>> import climlab
            >>> from climlab.radiation.AplusBT import AplusBT_CO2

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
            >>> LW_CO2 = AplusBT_CO2(CO2=400, state=model.state, **model.param)

            >>> # overwriting old 'LW' subprocess with same name
            >>> model.add_subprocess('LW', LW_CO2)

            >>> print model

        .. code-block:: none
            :emphasize-lines: 7

            climlab Process of type <class 'climlab.model.ebm.EBM'>.
            State variables and domain shapes:
              Ts: (90, 1)
            The subprocess tree:
            top: <class 'climlab.model.ebm.EBM'>
               diffusion: <class 'climlab.dynamics.diffusion.MeridionalDiffusion'>
               LW: <class 'climlab.radiation.AplusBT.AplusBT_CO2'>
               albedo: <class 'climlab.surface.albedo.StepFunctionAlbedo'>
                  iceline: <class 'climlab.surface.albedo.Iceline'>
                  cold_albedo: <class 'climlab.surface.albedo.ConstantAlbedo'>
                  warm_albedo: <class 'climlab.surface.albedo.P2Albedo'>
               insolation: <class 'climlab.radiation.insolation.P2Insolation'>

    """
    # implemented by m-kreuzer
    def __init__(self, CO2=300., **kwargs):
        super(AplusBT_CO2, self).__init__(**kwargs)
        self.CO2 = CO2
        #newdiags = ['OLR',]
        #self.add_diagnostics(newdiags)
        self.add_diagnostic('OLR', 0. * self.Ts)

    @property
    def CO2(self):
        """Property of AplusBT_CO2 parameter CO2.

        :getter:    Returns the CO2 concentration which is stored in attribute
                    ``self._CO2``
        :setter:    * sets the CO2 concentration which is addressed as ``self._CO2``
                      to the new value
                    * updates the parameter dictionary ``self.param['CO2']``
        :type:      float

        """
        return self._CO2
    @CO2.setter
    def CO2(self, value):
        self._CO2 = value
        self.param['CO2'] = value

#    def emission(self):
#        """Calculates the Outgoing Longwave Radiation (OLR) of the AplusBT_CO2
#        subprocess.
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
#            This method currently works only for a single state variable!
#
#        """
#        l = np.log(self.CO2/300.)
#        A = -326.400 + 9.16100*l - 3.16400*l**2 + 0.546800*l**3
#        B =    1.953 - 0.04866*l + 0.01309*l**2 - 0.002577*l**3
#        for varname, value in self.state.iteritems():
#            flux = A + B * (value + const.tempCtoK)
#            self.OLR = flux
#            self.diagnostics['OLR'] = self.OLR

    def _compute_emission(self):
        l = np.log(self.CO2/300.)
        self.A = -326.400 + 9.16100*l - 3.16400*l**2 + 0.546800*l**3
        self.B =    1.953 - 0.04866*l + 0.01309*l**2 - 0.002577*l**3
        for varname, value in self.state.items():
            self.OLR[:] = self.A + self.B * (value + const.tempCtoK)

    def _compute_heating_rates(self):
        """Computes energy flux convergences to get heating rates in :math:`W/m^2`."""
        self._compute_emission()
        for varname, value in self.state.items():
            self.heating_rate[varname] = -self.OLR
