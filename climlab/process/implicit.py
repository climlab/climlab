from climlab.process.time_dependent_process import TimeDependentProcess


class ImplicitProcess(TimeDependentProcess):
    '''Parent class for modules that use implicit time discretization.'''
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
