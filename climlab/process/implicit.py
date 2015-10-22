from climlab.process.time_dependent_process import TimeDependentProcess


class ImplicitProcess(TimeDependentProcess):
    '''Parent class for modules that use implicit time discretization.'''
    def __init__(self, **kwargs):
        super(ImplicitProcess, self).__init__(**kwargs)
        self.attrs['process_type'] = 'implicit'
        self.adjustment = {}

    def compute(self):
        # Time-stepping the diffusion is just inverting this matrix problem:
        # self.T = np.linalg.solve( self.diffTriDiag, Trad )
        newstate = self._implicit_solver()
        for varname, value in self.state.iteritems():
            self.adjustment[varname] = newstate[varname] - value
