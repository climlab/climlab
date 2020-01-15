r"""
Classes to provide insolation as input for other CLIMLAB processes.

Options include

- ``climlab.radiation.P2Insolation`` (idealized 2nd Legendre polynomial form)
- ``climlab.radiation.FixedInsolation`` (generic steady-state insolation)
- ``climlab.radiation.AnnualMeanInsolation`` (steady-state annual-mean insolation computed from orbital parameters and latitude)
- ``climlab.radiation.DailyInsolation`` (time-varying daily-mean insolation computed from orbital parameters, latitude and time of year)

All are subclasses of ``climlab.process.DiagnosticProcess``
and do add any tendencies to any state variables.

At least two diagnostics are provided:

- ``insolation``, the incoming solar radiation in :math:`\textrm{W}}{\textrm{m}^2}``
- ``coszen``, cosine of the solar zenith angle
"""
from __future__ import division
import numpy as np
from climlab.process.diagnostic import DiagnosticProcess
from climlab.domain.field import Field
from climlab.domain.field import to_latlon
from climlab.utils.legendre import P2
from climlab import constants as const
from climlab.solar.insolation import daily_insolation

# REVISE TO MAKE ALL OF THESE CALLABLE WITH NO ARGUMENTS.
# SET SOME SENSIBLE DEFAULTS FOR DOMAINS

#  the diagnostic self.insolation is set with correct dimensions at
#  creation time. After that, make sure to always modify it though
#  self.insolation[:] = ...
#  so that links to the insolation in other processes will work

#  REALLY NEED TO CHANGE THE WAY DIAGNOSTIC PROCESSES ARE CREATED
#  CAN'T RELY ON NAMES OF DOMAINS
#  should be easy to pass a state variable object or something with the right shape
#  and have the process gracefully set the correct dimensions


class _Insolation(DiagnosticProcess):
    """A private parent class for insolation processes.

    Calling compute() will update self.insolation with current values.

    **Initialization parameters** \n

    An instance of ``_Insolation`` is initialized with the following
    arguments *(for detailed information see Object attributes below)*:

    :param float S0:        solar constant                              \n
                            - unit: :math:`\\frac{\\textrm{W}}{\\textrm{m}^2}`   \n
                            - default value: ``1365.2``

    **Object attributes** \n

    Additional to the parent class :class:`~climlab.process.diagnostic.DiagnosticProcess`
    following object attributes are generated and updated during initialization:

    :ivar array insolation: the array is initialized with zeros of the size of
                            ``self.domains['sfc']`` or ``self.domains['default']``.
    :ivar array coszen:     cosine of the solar zenith angle
    :ivar float S0:         initialized with given argument ``S0``
    :ivar dict diagnostics: key ``'insolation'`` initialized with value:
                            :class:`~climlab.domain.field.Field` of zeros
                            in size of ``self.domains['sfc']`` or
                            ``self.domains['default']``
    :ivar Field insolation: the subprocess attribute ``self.insolation`` is
                            created with correct dimensions

    .. note::

        ``self.insolation`` should always be modified with
        ``self.insolation[:] = ...`` so that links to the insolation in other
        processes will work.

    """
    # parameter S0 is now stored using a python property
    # can be changed through self.S0 = newvalue
    # which will also update the parameter dictionary
    # CAUTION: changing self.param['S0'] will not work!
    def __init__(self, S0=const.S0, **kwargs):
        super(_Insolation, self).__init__(**kwargs)
        #  initialize diagnostics with correct shape
        self.add_diagnostic('insolation')
        self.add_diagnostic('coszen')
        try:
            domain = self.domains['sfc']
        except:
            domain = self.domains['default']
        self.insolation = Field(np.zeros(domain.shape), domain=domain)
        self.coszen = Field(np.zeros(domain.shape), domain=domain)
        self.declare_diagnostics(['insolation','coszen'])
        self.S0 = S0
        #  Now that we have a value for self.S0 we can compute the correct coszen
        self.coszen = self._coszen_from_insolation()
        self.declare_input(['S0'])

    @property
    def S0(self):
        """Property of solar constant S0.

        The parameter S0 is stored using a python property and can be changed through
        ``self.S0 = newvalue`` which will also update the parameter dictionary.

        .. warning::

            changing ``self.param['S0']`` will not work!

        :getter:    Returns the S0 parameter which is stored in attribute ``self._S0``.
        :setter:    * sets S0 which is addressed as ``self._S0`` to the new value
                    * updates the parameter dictionary ``self.param['S0']`` and
                    * calls method :func:`_compute_fixed`
        :type:      float

        """
        return self._S0
    @S0.setter
    def S0(self, value):
        self._S0 = value
        self.param['S0'] = value
        self._compute_fixed()

    def _coszen_from_insolation(self):
        return self.insolation / self.S0

    def _compute_fixed(self):
        '''Recompute any fixed quantities after a change in parameters'''
        pass

    def _get_current_insolation(self):
        pass

    def _compute(self):
        self._get_current_insolation()
        return {}

class FixedInsolation(_Insolation):
    """A class for fixed insolation at each point of latitude off the domain.

    The solar distribution for the whole domain is constant and specified by
    a parameter.

    **Initialization parameters** \n

    :param float S0:        solar constant                              \n
                            - unit: :math:`\\frac{\\textrm{W}}{\\textrm{m}^2}`   \n
                            - default value: ``const.S0/4 = 341.2``

    :Example:

        ::

            >>> import climlab
            >>> from climlab.radiation.insolation import FixedInsolation

            >>> model = climlab.EBM()
            >>> sfc = model.Ts.domain

            >>> fixed_ins = FixedInsolation(S0=340.0, domains=sfc)

            >>> print fixed_ins
            climlab Process of type <class 'climlab.radiation.insolation.FixedInsolation'>.
            State variables and domain shapes:
            The subprocess tree:
            top: <class 'climlab.radiation.insolation.FixedInsolation'>

    """
    def __init__(self, S0=const.S0/4, **kwargs):
        super(FixedInsolation, self).__init__(S0=S0, **kwargs)

    def _compute_fixed(self):
        #ins_adjustment = self.S0 - self.insolation
        #self.insolation += ins_adjustment
        self.insolation[:] = self.S0
        self.coszen[:] = self._coszen_from_insolation()


class P2Insolation(_Insolation):
    """A class for parabolic solar distribution over the domain's latitude
    on the basis of the second order Legendre Polynomial.

    Calculates the latitude dependent solar distribution as

    .. math::

        S(\\varphi) = \\frac{S_0}{4} \\left( 1 + s_2 P_2(x) \\right)

    where :math:`P_2(x) = \\frac{1}{2} (3x^2 - 1)` is the second order Legendre Polynomial
    and :math:`x=sin(\\varphi)`.

    **Initialization parameters** \n

    :param float S0:        solar constant                              \n
                            - unit: :math:`\\frac{\\textrm{W}}{\\textrm{m}^2}`   \n
                            - default value: ``1365.2``
    :param floar s2:        factor for second legendre polynominal term \n
                            - default value: ``-0.48``

    :Example:

        ::

            >>> import climlab
            >>> from climlab.radiation.insolation import P2Insolation

            >>> model = climlab.EBM()
            >>> sfc = model.Ts.domain

            >>> p2_ins = P2Insolation(S0=340.0, s2=-0.5, domains=sfc)

            >>> print p2_ins
            climlab Process of type <class 'climlab.radiation.insolation.P2Insolation'>.
            State variables and domain shapes:
            The subprocess tree:
            top: <class 'climlab.radiation.insolation.P2Insolation'>

    """
    def __init__(self, S0=const.S0, s2=-0.48, **kwargs):
        super(P2Insolation, self).__init__(S0=S0, **kwargs)
        self.s2 = s2

    @property
    def s2(self):
        """Property of second legendre polynomial factor s2.

        s2 in following equation:

        .. math::

            S(\\varphi) = \\frac{S_0}{4} \\left( 1 + s_2 P_2(x) \\right)

        :getter:    Returns the s2 parameter which is stored in attribute ``self._s2``.
        :setter:    * sets s2 which is addressed as ``self._S0`` to the new value
                    * updates the parameter dictionary ``self.param['s2']`` and
                    * calls method :func:`_compute_fixed`
        :type:      float

        """
        return self._s2
    @s2.setter
    def s2(self, value):
        self._s2 = value
        self.param['s2'] = value
        self._compute_fixed()

    def _compute_fixed(self):
        phi = np.deg2rad(self.lat)
        #  Why is there a silent fail here? Should get rid of this.
        try:
            insolation = self.S0 / 4 * (1. + self.s2 * P2(np.sin(phi)))
            dom = self.domains['default']
            try:
                insolation = to_latlon(insolation, domain=dom)
                self.insolation[:] = insolation
            except:
                self.insolation[:] = Field(insolation, domain=dom)
            # make sure that the diagnostic has the correct field dimensions.
            #self.insolation = Field(insolation, domain=dom)
            self.insolation[:] = Field(insolation, domain=dom)
            self.coszen[:] = self._coszen_from_insolation()
        #  Silent fail only for attribute error: _s2 is not an attribute of self
        #  but s2 parameter is being stored in self._s2
        except AttributeError:
            pass


# These classes calculate insolation based on orbital parameters
#  and astronomical formulas

class AnnualMeanInsolation(_Insolation):
    """A class for latitudewise solar insolation averaged over a year.

    This class computes the solar insolation for each day of the year and
    latitude specified in the domain on the basis of orbital parameters and
    astronomical formulas.

    Therefore it uses the method :func:`~climlab.solar.insolation.daily_insolation`.
    For details how the solar distribution is dependend on orbital parameters
    see there.

    The mean over the year is calculated from data given by
    :func:`~climlab.solar.insolation.daily_insolation` and stored in the
    object's attribute ``self.insolation``

    **Initialization parameters** \n

    :param float S0:    solar constant                                       \n
                        - unit: :math:`\\frac{\\textrm{W}}{\\textrm{m}^2}`   \n
                        - default value: ``1365.2``

    :param dict orb:    a dictionary with three orbital parameters (as provided by
                        :class:`~climlab.solar.orbital.OrbitalTable`):

                        * ``'ecc'`` - eccentricity

                            * unit: dimensionless
                            * default value: ``0.017236``

                        * ``'long_peri'`` - longitude of perihelion (precession angle)

                            * unit: degrees
                            * default value: ``281.37``

                        * ``'obliquity'`` - obliquity angle

                            * unit: degrees
                            * default value: ``23.446``

    **Object attributes** \n

    Additional to the parent class :class:`~climlab.radiation.insolation._Insolation`
    following object attributes are generated and updated during initialization:

    :ivar insolation:       Current insolation in W/m2
    :vartype insolation:    Field

    :ivar coszen:           Cosine of the current solar zenith angle
    :vartype coszen:    Field

    :ivar dict orb:         initialized with given argument ``orb``

    :Example:

        Create regular EBM and replace standard insolation subprocess by
        :class:`~climlab.radation.AnnualMeanInsolation`::

            >>> import climlab
            >>> from climlab.radiation import AnnualMeanInsolation

            >>> # model creation
            >>> model = climlab.EBM()

            >>> print model

        .. code-block:: none
            :emphasize-lines: 12

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

            >>> # catch model domain for subprocess creation
            >>> sfc = model.domains['Ts']

            >>> # create AnnualMeanInsolation subprocess
            >>> new_insol = AnnualMeanInsolation(domains=sfc, **model.param)

            >>> # add it to the model
            >>> model.add_subprocess('insolation',new_insol)

            >>> print model

        .. code-block:: none
            :emphasize-lines: 12

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
               insolation: <class 'climlab.radiation.insolation.AnnualMeanInsolation'>

    """
    def __init__(self, S0=const.S0, orb=const.orb_present, **kwargs):
        super(AnnualMeanInsolation, self).__init__(S0=S0, **kwargs)
        #self.param['orb'] = orb
        self.orb = orb
        self._compute_fixed()

    @property
    def orb(self):
        """Property of dictionary for orbital parameters.

        orb contains: (for more information see :class:`~climlab.solar.orbital.OrbitalTable`)

        * ``'ecc'`` - eccentricity [unit: dimensionless]
        * ``'long_peri'`` - longitude of perihelion (precession angle) [unit: degrees]
        * ``'obliquity'`` - obliquity angle [unit: degrees]

        :getter:    Returns the orbital dictionary which is stored in attribute
                    ``self._orb``.
        :setter:    * sets orb which is addressed as ``self._orb`` to the new value
                    * updates the parameter dictionary ``self.param['orb']`` and
                    * calls method :func:`_compute_fixed`
        :type:      dict

        """
        return self._orb
    @orb.setter
    def orb(self, value):
        self._orb = value
        self.param['orb'] = value
        self._compute_fixed()

    def _daily_insolation_array(self):
        lat = self.lat
        days_of_year = self.time['days_of_year']
        orb = self.orb
        S0 = self.S0
        return daily_insolation(lat, days_of_year, orb=orb, S0=S0)

    def _compute_fixed(self):
        try:
            temp_array = self._daily_insolation_array()
            insolation = np.mean(temp_array, axis=1)
            # make sure that the diagnostic has the correct field dimensions.
            dom = self.domains['default']
            try:
                insolation = to_latlon(insolation, domain=dom)
                self.insolation[:] = insolation
            except:
                self.insolation[:] = Field(insolation, domain=dom)
            self.coszen[:] = self._coszen_from_insolation()
        #  Silent fail only for attribute error: _orb is not an attribute of self
        #  but orb parameter is being stored in self._orb
        except AttributeError:
            pass


class DailyInsolation(AnnualMeanInsolation):
    """A class to compute latitudewise daily solar insolation for specific
    days of the year.

    This class computes the solar insolation on basis of orbital parameters and
    astronomical formulas.

    Therefore it uses the method :func:`~climlab.solar.insolation.daily_insolation`.
    For details how the solar distribution is dependend on orbital parameters
    see there.

    **Initialization parameters** \n

    :param float S0:    solar constant                              \n
                        - unit: :math:`\\frac{\\textrm{W}}{\\textrm{m}^2}`   \n
                        - default value: ``1365.2``

    :param dict orb:    a dictionary with orbital parameters:

                        * ``'ecc'`` - eccentricity

                            * unit: dimensionless
                            * default value: ``0.017236``

                        * ``'long_peri'`` - longitude of perihelion (precession angle)

                            * unit: degrees
                            * default value: ``281.37``

                        * ``'obliquity'`` - obliquity angle

                            * unit: degrees
                            * default value: ``23.446``

    **Object attributes** \n

    Additional to the parent class :class:`~climlab.radiation.insolation._Insolation`
    following object attributes are generated and updated during initialization:

    :ivar insolation:       Current insolation in W/m2
    :vartype insolation:    Field

    :ivar coszen:           Cosine of the current solar zenith angle
    :vartype coszen:    Field

    :ivar dict orb:         initialized with given argument ``orb``


    :Example:

        Create regular EBM and replace standard insolation subprocess by
        :class:`~climlab.radation.DailyInsolation`::

            >>> import climlab
            >>> from climlab.radiation import DailyInsolation

            >>> # model creation
            >>> model = climlab.EBM()

            >>> print model

        .. code-block:: none
            :emphasize-lines: 12

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

            >>> # catch model domain for subprocess creation
            >>> sfc = model.domains['Ts']

            >>> # create DailyInsolation subprocess and add it to the model
            >>> model.add_subprocess('insolation',DailyInsolation(domains=sfc, **model.param))

            >>> print model

        .. code-block:: none
            :emphasize-lines: 12

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
               insolation: <class 'climlab.radiation.insolation.DailyInsolation'>

    """

    def _compute_fixed(self):
        try:
            self.insolation_array = self._daily_insolation_array()
        except AttributeError:
            pass

    def _get_current_insolation(self):
        insolation_array = self.insolation_array
        # make sure that the diagnostic has the correct field dimensions.
        dom = self.domains['default']
        time_index = self.time['day_of_year_index']   # THIS ONLY WORKS IF self IS THE MASTER PROCESS
        insolation = insolation_array[..., time_index]
        if 'lon' in dom.axes:
            # insolation is latitude-only, need to broadcast across longitude
            #  assumption is axes are ordered (lat, lon, depth)
            #  NOTE this is a clunky hack and all this will go away
            #  when we use xarray structures for these internals
            insolation = np.tile(insolation[...,np.newaxis], dom.axes['lon'].num_points)
        self.insolation[:] = Field(insolation, domain=dom)
        self.coszen[:] = self._coszen_from_insolation()
