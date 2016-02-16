from climlab.process.energy_budget import EnergyBudget
from climlab.domain.field import global_mean


class BudykoTransport(EnergyBudget):
    """calculates the 1 dimensional heat transport as the difference 
    between the local temperature and the global mean temperature.
    
    :ivar float b:    budyko transport parameter
    
    In a global Energy Balance Model 
    
    .. math::

        C \\frac{dT}{dt} = R\downarrow - R\uparrow - H
    
    with model state :math:`T`, the energy transport term :math:`H` 
    can be described as

    .. math::
    
        H = b [T - \\bar{T}]

    where :math:`T` is a vector of the model temperature and :math:`\\bar{T}`
    describes the mean value of :math:`T`.
    
    For further information see [Budyko1969]_.

    """
    # implemented by m-kreuzer
    def __init__(self, b=3.81, **kwargs):
        super(BudykoTransport, self).__init__(**kwargs)
        self.b = b        
        
    @property
    def b(self):
        """the budyko transport parameter in unit 
        :math:`\\frac{\\textrm{W}}{\\textrm{m}^2 \\textrm{K}}`
    
        :getter: returns the budyko transport parameter
        :setter: sets the budyko transport parameter
        :type: float
        
        """    
        return self._b
    @b.setter
    def b(self, value):
        self._b = value
        self.param['b'] = value
        
    def _compute_heating_rates(self):
        """Computes energy flux convergences to get heating rates in :math:`W/m^2`.
        
        """       
        for varname, value in self.state.iteritems():
            self.heating_rate[varname] = - self.b * (value - global_mean(value))

