from climlab.process.time_dependent_process import TimeDependentProcess


class ImplicitProcess(TimeDependentProcess):
    '''Parent class for modules that use implicit time discretization.'''
    def __init__(self, **kwargs):
        super(ImplicitProcess, self).__init__(**kwargs)
        self.process_type = 'implicit'
        #self.adjustment = {}

    def _compute(self):
        # Time-stepping the diffusion is just inverting this matrix problem:
        # self.T = np.linalg.solve( self.diffTriDiag, Trad )
        newstate = self._implicit_solver()
        #adjustment = newstate.copy()
        adjustment = {}
        tendency = {}
        for name, var in self.state.iteritems():
            adjustment[name] = newstate[name] - var
            tendency[name] = adjustment[name] / self.timestep
        # express the adjustment (already accounting for the finite time step)
        #  as a tendency per unit time, so that it can be applied along with explicit
        return tendency
