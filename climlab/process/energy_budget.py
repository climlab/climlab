import numpy as np
from climlab.process.time_dependent_process import TimeDependentProcess


class EnergyBudget(TimeDependentProcess):
    """A parent class for explicit energy budget processes.
    
    This class solves equations that include a heat capacitiy term like
    :math:`C \\frac{dT}{dt} = \\textrm{flux convergence}`
    
    In an Energy Balance Model with model state :math:`T` this equation 
    will look like this:
    
    .. math::
    
        C \\frac{dT}{dt} = R\downarrow - R\uparrow - H  \n
        \\frac{dT}{dt} = \\frac{R\downarrow}{C} - \\frac{R\uparrow}{C} - \\frac{H}{C}

    Every EnergyBudget object has a ``heating_rate`` dictionary with items 
    corresponding to each state variable. The heating rate accounts the actual 
    heating of a subprocess, namely the contribution to the energy budget 
    of :math:`R\\downarrow, R\\uparrow` and :math:`H` in this case.
    The temperature tendencies for each subprocess are then calculated 
    through dividing the heating rate by the heat capacitiy :math:`C`.
    
    **Initialization parameters** \n
        
    An instance of ``EnergyBudget`` is initialized with the forwarded 
    keyword arguments ``**kwargs`` of the corresponding children classes.
    
    **Object attributes** \n
    
    Additional to the parent class 
    :class:`~climlab.process.timedependentprocess.TimeDependentProcess`
    following object attributes are generated or modified during initialization:
    
    :ivar str time_type:        is set to ``'explicit'``
    :ivar dict heating_rate:    energy share for given subprocess in unit 
                                :math:`\\textrm{W}/ \\textrm{m}^2` stored 
                                in a dictionary sorted by model states
    
    """    
    def __init__(self, **kwargs):
        super(EnergyBudget, self).__init__(**kwargs)
        self.time_type = 'explicit'
        self.heating_rate = {}

    def _compute_heating_rates(self):
        """Computes energy flux convergences to get heating rates in unit
        :math:`\\textrm{W}/ \\textrm{m}^2`.
        
        This method should be over-ridden by daughter classes.
        
        """
        for varname in self.state.keys():
            self.heating_rate[varname] = self.state[varname] * 0.

    def _temperature_tendencies(self):
        self._compute_heating_rates()
        tendencies = {}
        for varname, value in self.state.iteritems():
            #C = self.state_domain[varname].heat_capacity
            C = value.domain.heat_capacity
            try:  # there may be state variables without heating rates
                tendencies[varname] = (self.heating_rate[varname] / C)
            except:
                pass
        return tendencies

    def _compute(self):
        tendencies = self._temperature_tendencies()
        return tendencies


class ExternalEnergySource(EnergyBudget):
    """A fixed energy source or sink to be specified by the user.
    
    **Initialization parameters** \n
        
    An instance of ``ExternalEnergySource`` is initialized with the forwarded 
    keyword arguments ``**kwargs`` of hypthetical corresponding children classes
    (which are not existing in this case).
    
    **Object attributes** \n
    
    Additional to the parent class :class:`~climlab.process.energy_budget.EnergyBudget`
    the following object attribute is modified during initialization:
    
    :ivar dict heating_rate:    energy share dictionary for this subprocess
                                is set to zero for every model state.
                                
    After initialization the user should modify the fields in the 
    ``heating_rate`` dictionary, which contain heating rates in 
    unit :math:`\\textrm{W}/ \\textrm{m}^2` for all state variables.
    
    """
    def __init__(self, **kwargs):
        super(ExternalEnergySource, self).__init__(**kwargs)
        for varname in self.state.keys():
            self.heating_rate[varname] = self.state[varname] * 0.

    def _compute_heating_rates(self):
        pass
