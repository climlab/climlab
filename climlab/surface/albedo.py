import numpy as np
from climlab.process.diagnostic import DiagnosticProcess
from climlab.utils.legendre import P2
from climlab.domain.field import Field


class ConstantAlbedo(DiagnosticProcess):
    """A class for constant albedo values.
    
    Defines constant albedo values for the whole domain.
    
    **Initialization parameters** \n

    :param float albedo:    albedo values                                   \n
                            - unit: dimensionless                           \n
                            - default value: ``0.33``
                            
    **Object attributes** \n
    
    Additional to the parent class 
    :class:`~climlab.process.diagnostic.DiagnosticProcess`
    following object attributes are generated and updated during initialization:
        
    :ivar Field albedo:             attribute to store the albedo value. 
                                    During initialization the 
                                    :func:`albedo` setter is called.
    :ivar frozenset _diag_vars:     extended by the list ``['albedo',]``
    
    
    """        
    def __init__(self, albedo=0.33, **kwargs):
        '''Uniform prescribed albedo.'''
        super(ConstantAlbedo, self).__init__(**kwargs)
        self.albedo = albedo
        newdiags = ['albedo',]
        self.add_diagnostics(newdiags)

    @property
    def albedo(self):
        """Property of albedo value.
        
        :getter:    Returns the albedo value which is stored in attribute 
                    ``self._albedo``
        :setter:    * sets albedo which is addressed as ``self._albedo`` to the new value
                      through creating a Field on the basis of domain ``self.domain['default']``
                    * updates the parameter dictionary ``self.param['albedo']``
        :type:      Field
        
        """
        return self._albedo
    @albedo.setter
    def albedo(self, value):
        dom = self.domains['default']
        self._albedo = Field(value, domain=dom)
        self.param['albedo'] = value


class P2Albedo(DiagnosticProcess):
    """A class for parabolic defined albedo values on basis of the second order
    Legendre Polynomial.
    
    Defines parabolic shaped albedo values across the domain range.
    
    Calculates the latitude dependent albedo values as
    
    .. math::
    
         \\alpha(\\varphi) = a_0 + a_2 P_2(x)
    
    where :math:`P_2(x) = \\frac{1}{2} (3x^2 - 1)` is the second order Legendre Polynomial
    and :math:`x=sin(\\varphi)`.
    
    **Initialization parameters** \n

    :param float a0:    basic parameter for albedo function             \n
                        - unit: dimensionless                           \n
                        - default value: ``0.33``
    :param float a2:    factor for second legendre polynominal term in albedo function
                                                                        \n
                        - unit: dimensionless                           \n
                        - default value: ``0.25``
                            
    **Object attributes** \n
    
    Additional to the parent class 
    :class:`~climlab.process.diagnostic.DiagnosticProcess`
    following object attributes are generated and updated during initialization:
        
    :ivar float a0:                 attribute to store the albedo parameter a0. 
                                    During initialization the 
                                    :func:`a0` setter is called.
    :ivar float a2:                 attribute to store the albedo parameter a2. 
                                    During initialization the 
                                    :func:`a2` setter is called.
    :ivar frozenset _diag_vars:     extended by the list ``['albedo',]``
    
    """    

    def __init__(self, a0=0.33, a2=0.25, **kwargs):
        super(P2Albedo, self).__init__(**kwargs)
        self.a0 = a0
        self.a2 = a2
        newdiags = ['albedo',]
        self.add_diagnostics(newdiags)

    @property
    def a0(self):
        """Property of albedo parameter a0.
        
        :getter:    Returns the albedo parameter value which is stored in attribute 
                    ``self._a0``
        :setter:    * sets albedo parameter which is addressed as ``self._a0`` 
                      to the new value
                    * updates the parameter dictionary ``self.param['a0']``
                    * calls method :func:`_compute_fixed`
        :type:      float
        
        """
        return self._a0
    @a0.setter
    def a0(self, value):
        self._a0 = value
        self.param['a0'] = value
        self._compute_fixed()
    @property
    def a2(self):
        """Property of albedo parameter a2.
        
        :getter:    Returns the albedo parameter value which is stored in attribute 
                    ``self._a2``
        :setter:    * sets albedo parameter which is addressed as ``self._a2`` 
                      to the new value
                    * updates the parameter dictionary ``self.param['a2']``
                    * calls method :func:`_compute_fixed`
        :type:      float
        
        """
        return self._a2
    @a2.setter
    def a2(self, value):
        self._a2 = value
        self.param['a2'] = value
        self._compute_fixed()
    def _compute_fixed(self):
        '''Recompute any fixed quantities after a change in parameters'''
        phi = np.deg2rad(self.lat)
        try:
            albedo = self.a0 + self.a2 * P2(np.sin(phi))
        except:
            albedo = np.zeros_like(phi)
        # make sure that the diagnostic has the correct field dimensions.
        dom = self.domains['default']
        self.albedo = Field(albedo, domain=dom)



class Iceline(DiagnosticProcess):
    """A class for an Iceline subprocess.
    
    Depending on a freezing temperature it calculates where on the domain the
    surface is covered with ice, where there is no ice and on which latitude the ice-edge 
    is placed.
    
    **Initialization parameters** \n

    :param float Tf:    freezing temperature where sea water freezes and
                        surface is covered with ice                     \n
                        - unit: :math:`^{\circ} \\textrm{C}`            \n
                        - default value: ``-10``
                            
    **Object attributes** \n
    
    Additional to the parent class 
    :class:`~climlab.process.diagnostic.DiagnosticProcess`
    following object attributes are generated and updated during initialization:
        
    :ivar dict param:               The parameter dictionary is updated with the 
                                    input argument ``'Tf'``.
    :ivar frozenset _diag_vars:     extended by the list ``['noice','ice','icelat']``
    

    """    
    def __init__(self, Tf=-10., **kwargs):
        super(DiagnosticProcess, self).__init__(**kwargs)
        self.param['Tf'] = Tf
        newdiags = ['noice',
                    'ice',
                    'icelat']
        self.add_diagnostics(newdiags)

    def find_icelines(self):
        """Finds iceline according to the surface temperature.
        
        This method is called by the private function 
        :func:`~climlab.surface.albedo.Iceline._compute`
        and updates following attributes according to the freezing temperature
        ``self.param['Tf']`` and the surface temperature ``self.param['Ts']``:
        
        **Object attributes** \n
        
        :ivar Field noice:      a Field of booleans which are ``True`` where
                                :math:`T_s \\ge T_f`
        :ivar Field ice:        a Field of booleans which are ``True`` where
                                :math:`T_s < T_f`
        :ivar array icelat:     an array with two elements indicating the 
                                ice-edge latitudes
        
        
        """
        Tf = self.param['Tf']
        Ts = self.state['Ts']
        lat_bounds = self.domains['Ts'].axes['lat'].bounds
        self.noice = np.where(Ts >= Tf, True, False)
        self.ice = np.where(Ts < Tf, True, False)
        if self.ice.all():
            # 100% ice cover
            self.icelat = np.array([-0., 0.])
        elif self.noice.all():
            # zero ice cover
            self.icelat = np.array([-90., 90.])
        else:  # there is some ice edge
            # Taking np.diff of a boolean array gives True at the boundaries between True and False
            boundary_indices = np.where(np.diff(self.ice.squeeze()))[0] + 1
            self.icelat = lat_bounds[boundary_indices]  # an array of boundary latitudes

    def _compute(self):
        self.find_icelines()
        return {}


class StepFunctionAlbedo(DiagnosticProcess):
    """A step function albedo suprocess.
    
    This class itself defines three subprocesses that are created during 
    initialization:
        * ``'iceline'`` - :class:`Iceline`
        * ``'warm_albedo'`` - :class:`P2Albedo`
        * ``'cold_albedo'`` - :class:`ConstantAlbedo`

    **Initialization parameters** \n

    :param float Tf:    freezing temperature for Iceline subprocess     \n
                        - unit: :math:`^{\circ} \\textrm{C}`            \n
                        - default value: ``-10``    
    :param float a0:    basic parameter for P2Albedo subprocess         \n
                        - unit: dimensionless                           \n
                        - default value: ``0.3``
    :param float a2:    factor for second legendre polynominal term in P2Albedo subprocess 
                                                                        \n
                        - unit: dimensionless                           \n
                        - default value: ``0.078``
    :param float ai:    ice albedo value for ConstantAlbedo subprocess  \n
                        - unit: dimensionless                           \n
                        - default value: ``0.62``
                                                        
    Additional to the parent class 
    :class:`~climlab.process.diagnostic.DiagnosticProcess`
    following object attributes are generated/updated during initialization:
        
    :ivar dict param:               The parameter dictionary is updated with 
                                    a couple of the initatilzation input 
                                    arguments, namely ``'Tf'``, ``'a0'``, 
                                    ``'a2'`` and ``'ai'``.  
    :ivar bool topdown:             is set to ``False`` to call subprocess
                                    compute method first
    :ivar frozenset _diag_vars:     extended by the list ``['albedo',]``
    
    """
    def __init__(self, Tf=-10., a0=0.3, a2=0.078, ai=0.62, **kwargs):
        super(DiagnosticProcess, self).__init__(**kwargs)
        self.param['Tf'] = Tf
        self.param['a0'] = a0
        self.param['a2'] = a2
        self.param['ai'] = ai
        sfc = self.domains['Ts']
        self.add_subprocess('iceline', Iceline(Tf=Tf, state=self.state))
        self.add_subprocess('warm_albedo', P2Albedo(a0=a0, a2=a2, domains=sfc))
        self.add_subprocess('cold_albedo', ConstantAlbedo(albedo=ai, domains=sfc))
        self.topdown = False  # call subprocess compute methods first
        newdiags = ['albedo',]
        self.add_diagnostics(newdiags)

    def _get_current_albedo(self):
        '''Simple step-function albedo based on ice line at temperature Tf.'''
        ice = self.subprocess['iceline'].ice
        # noice = self.subprocess['iceline'].diagnostics['noice']
        cold_albedo = self.subprocess['cold_albedo'].albedo
        warm_albedo = self.subprocess['warm_albedo'].albedo
        albedo = Field(np.where(ice, cold_albedo, warm_albedo), domain=self.domains['Ts'])
        return albedo

    def _compute(self):
        self.albedo = self._get_current_albedo()
        return {}
