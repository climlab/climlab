from climlab.process.diagnostic import _DiagnosticProcess


# not sure if I need this anymore

class _EnergyFlux(_DiagnosticProcess):
    '''parent class for diagnostic energy flux processes.'''
    def __init__(self, **kwargs):
        super(_EnergyFlux, self).__init__(**kwargs)
    

    def compute():
        '''Update all diagnostic quantities using current model state.'''
        pass
