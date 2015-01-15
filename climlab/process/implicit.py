from climlab.process.time_dependent_process import TimeDependentProcess


class ImplicitProcess(TimeDependentProcess):
    '''Parent class for modules that use implicit time discretization.'''
    def __init__(self, **kwargs):
        super(ImplicitProcess, self).__init__(**kwargs)
        self.process_type = 'implicit'
        self.adjusted_state = {}

    def compute(self):
        # Time-stepping the diffusion is just inverting this matrix problem:
        # self.T = np.linalg.solve( self.diffTriDiag, Trad )
        newstate = self._implicit_solver()
        for varname in self.state.keys():
            self.adjusted_state[varname] = newstate[varname]
