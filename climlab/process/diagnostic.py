from climlab.process.process import _Process


# haven't really used this for anything yet.

class _DiagnosticProcess(_Process):
    '''parent class for all processes that are strictly diagnostic,
    i.e. no time dependence.'''
    def __init__(self, **kwargs):
        super(_DiagnosticProcess, self).__init__(**kwargs)
        self.time_type = 'diagnostic'

    def compute():
        '''Update all diagnostic quantities using current model state.'''
        pass
