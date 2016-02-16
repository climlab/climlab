import numpy as np
from climlab.process.diagnostic import DiagnosticProcess
from climlab.domain.field import Field
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
                            
    :ivar float S0:         initialized with given argument ``S0``
    :ivar frozenset _diag_vars:  
                            extended by string ``'insolation'``
    
    """
    # parameter S0 is now stored using a python property
    # can be changed through self.S0 = newvalue
    # which will also update the parameter dictionary
    # CAUTION: changing self.param['S0'] will not work!
    def __init__(self, S0=const.S0, **kwargs):
        super(_Insolation, self).__init__(**kwargs)
        #  initialize diagnostic with correct shape
        try:
            self.insolation = np.zeros(self.domains['sfc'].shape)
        except:
            self.insolation = np.zeros(self.domains['default'].shape)
        self.S0 = S0
        self.add_diagnostics(['insolation'])

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

    def _compute_fixed(self):
        '''Recompute any fixed quantities after a change in parameters'''
        pass

    def _get_current_insolation(self):
        pass

    def _compute(self):
        self._get_current_insolation()
        return {}

class FixedInsolation(_Insolation):
    """A class for fixed insolation.
    
    Defines a constant solar distribution for the whole domain.
    
    **Initialization parameters** \n

    :param float S0:        solar constant                              \n
                            - unit: :math:`\\frac{\\textrm{W}}{\\textrm{m}^2}`   \n
                            - default value: ``const.S0/4 = 341.2``    
    
    :Example:
        
        .. code::

            import climlab
            
            model = climlab.EBM()
            sfc = model.Ts.domain
            fixed_ins = climlab.radiation.insolation.FixedInsolation(S0=340.0, domains=sfc)
    
    """
    def __init__(self, S0=const.S0/4, **kwargs):
        super(FixedInsolation, self).__init__(S0=S0, **kwargs)

    def _compute_fixed(self):
        ins_adjustment = self.S0 - self.insolation
        self.insolation += ins_adjustment


class P2Insolation(_Insolation):  
    """A class for parabolic solar distribution on basis of the second order
    Legendre Polynomial.
    
    Calculates the latitude dependent solar distribution as
    
    .. math::
    
        S(\\varphi) = \\frac{S_0}{4} \\left( 1 + s_2 P_2(x) \\right)
    
    where :math:`P_2(x) = \\frac{1}{2} (3x^2 - 1)` is the second order Legendre Polynomial
    and :math:`x=sin(\\varphi)`.
    
    **Initialization parameters** \n

    :param float S0:        solar constant                              \n
                            - unit: :math:`\\frac{\\textrm{W}}{\\textrm{m}^2}`   \n
                            - default value: ``1365.2``     
    :param floar s2:        factor for second legendre polynominal term
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
            # make sure that the diagnostic has the correct field dimensions.
            dom = self.domains['default']
            self.insolation[:] = Field(insolation, domain=dom)
        except:
            pass


# These classes calculate insolation based on orbital parameters
#  and astronomical formulas

class AnnualMeanInsolation(_Insolation):
    """A class for annual mean solar insolation.
    
    This class computes the solar insolation on basis of orbital parameters and 
    astronomical formulas.
    
    Therefor it uses the method :func:`~climlab.solar.insolation.daily_insolation`.
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
        
    :ivar Field insolation: the solar distribution is calculated as a Field on 
                            the basis of the ``self.domains['default']`` domain
                            and stored in the attribute ``self.insolation``.
                            
    :ivar dict orb:         initialized with given argument ``orb``
    
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
            self.insolation[:] = Field(insolation, domain=dom)
        except:
            pass


class DailyInsolation(AnnualMeanInsolation):
    """A class for daily solar insolation.
    
    This class computes the solar insolation on basis of orbital parameters and 
    astronomical formulas.
    
    Therefor it uses the method :func:`~climlab.solar.insolation.daily_insolation`.
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
        
    :ivar Field insolation: the solar distribution is calculated as a Field on 
                            the basis of the ``self.domains['default']`` domain
                            and stored in the attribute ``self.insolation``.
                            
    :ivar dict orb:         initialized with given argument ``orb``
    
    """          
    
    def _compute_fixed(self):
        try:
            self.insolation_array = self._daily_insolation_array()
        except:
            pass

    def _get_current_insolation(self):
        #  this probably only works for 1D (latitude) domains
        insolation_array = self.insolation_array
        # make sure that the diagnostic has the correct field dimensions.
        dom = self.domains['default']
        time_index = self.time['day_of_year_index']   # THIS ONLY WORKS IF self IS THE MASTER PROCESS
        insolation = insolation_array[..., time_index]
        self.insolation[:] = Field(insolation, domain=dom)
