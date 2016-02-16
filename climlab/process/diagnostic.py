from climlab.process.time_dependent_process import TimeDependentProcess


class DiagnosticProcess(TimeDependentProcess):
    '''parent class for all processes that are strictly diagnostic,
    i.e. no time dependence.'''
    def __init__(self, **kwargs):
        super(DiagnosticProcess, self).__init__(**kwargs)
        self.time_type = 'diagnostic'

    def _compute(self):
        '''Diagnotic process returns no tendencies.'''
        return {}
