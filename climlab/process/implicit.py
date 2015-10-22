from climlab.process.time_dependent_process import TimeDependentProcess


class ImplicitProcess(TimeDependentProcess):
    '''Parent class for modules that use implicit time discretization.'''
    def __init__(self, **kwargs):
        super(ImplicitProcess, self).__init__(**kwargs)
        self.attrs['process_type'] = 'implicit'
        #self.adjustment = {}

    def _compute(self):
        # Time-stepping the diffusion is just inverting this matrix problem:
        # self.T = np.linalg.solve( self.diffTriDiag, Trad )
        newstate = self._implicit_solver()
        adjustment = newstate.copy()
        for name, var in self.state.data_vars.iteritems():
            adjustment[name] = newstate[name] - var
        # express the adjustment (already accounting for the finite time step)
        #  as a tendency per unit time, so that it can be applied along with explicit
        tendency = adjustment / self.timestep
        return tendency
