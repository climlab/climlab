from climlab.process.time_dependent_process import TimeDependentProcess


class ImplicitProcess(TimeDependentProcess):
    """A parent class for modules that use implicit time discretization.

    During initialization following attributes are intitialized:

    :ivar time_type:        is set to ``'implicit'``
    :vartype time_type:     str
    
    :ivar adjustment:       the model state adjustments due to this implicit 
                            subprocess                            
    :vartype adjustment:    dict   
    
    Calculating the model state adjustments through solving the matrix problem 
    already includes the multiplication with the timestep. The adjustment is
    divided by the timestep to calculate the implicit subprocess tendencies,
    which can be handeled by the 
    :func:`~climlab.process.time_dependent_process.TimeDependentProcess.compute`
    method of the parent 
    :class:`~climlab.process.time_dependent_process.TimeDependentProcess` class.
    
    """
    def __init__(self, **kwargs):
        super(ImplicitProcess, self).__init__(**kwargs)
        self.time_type = 'implicit'
        self.adjustment = {}

    def _compute(self):
        # Time-stepping the diffusion is just inverting this matrix problem:
        # self.T = np.linalg.solve( self.diffTriDiag, Trad )
        newstate = self._implicit_solver()
        #adjustment = newstate.copy()
        adjustment = {}
        tendencies = {}
        for name, var in self.state.iteritems():
            adjustment[name] = newstate[name] - var
            tendencies[name] = adjustment[name] / self.param['timestep']
        # express the adjustment (already accounting for the finite time step)
        #  as a tendency per unit time, so that it can be applied along with explicit
        self.adjustment = adjustment
        return tendencies
