import numpy as np
from climlab.process import TimeDependentProcess


class Limiter(TimeDependentProcess):
    '''A process that implements strict bounds on the allowable range of values of state variables.
    Values outside the given bounds are adjusted back to the bounding value at each timestep.

    Bounding values are stored in a dictionary ``.bounds`` which has identical keys to ``.state``

    Each item in the ``.bounds`` dict is another dict containing the keys ``'minimum'`` and ``'maximum'``.
    By default these are initialized to ``None`` and ``np.inf`` respectively,
    which means the process produces zero adjustment.

    The user needs to specify desired minimum and/or maximum values for each state variable.
    These can be specified at process creation time using the keyword argument ``bounds``, 
    or modified in-place (see example below).

    For diagnostic purposes, we can always access the adjustments (in state variable units) 
    and the tendencies (in state variable units per second) produced by the Limiter
    just like any other process (see example below) 

    Example use: an EBM with surface temperature limited to <= 25 degrees C::

        import climlab
        ebm = climlab.EBM()
        #  Create the Limiter process, and make sure it has a matching timestep
        mylimiter = climlab.process.Limiter(state=ebm.state, timestep=ebm.timestep)
        #  Now set our desired upper bound on the temperature
        mylimiter.bounds['Ts']['maximum'] = 25.
        #  And couple it to the rest of the model
        ebm.add_subprocess('TempLimiter', mylimiter)
        #  Take a step forward and verify that surface temperatures do not exceed 25 degrees C
        ebm.step_forward()
        assert np.all(ebm.Ts<=25)
        #  Examine the tendencies (in degrees C / second) produced by the Limiter:
        #  They should be zero everywhere the temperaure is less than 25 degrees: 
        print(ebm.subprocess['TempLimiter'].tendencies)
    '''
    def __init__(self, bounds={}, **kwargs):
        super(Limiter, self).__init__(**kwargs)
        #  Initialize bounds for all state variables. `None` means no bounds
        #  By default the process should produce zero adjustment
        #  Note that in numpy 2.0 and above, we can do this by setting `None` on both bounds
        #  But in numpy < 2.0 that's not allowed, so we use `np.inf` as upper bound instead
        self.bounds = {}
        for name in self.state:
            self.bounds[name] = {'minimum': None, 'maximum': np.inf}
        #  Now override with any user-specified values
        for name, thisbounddict in bounds.items():
            if 'minimum' in thisbounddict:
                self.bounds[name]['minimum'] = thisbounddict['minimum']
            if 'maximum' in thisbounddict:
                self.bounds[name]['maximum'] = thisbounddict['maximum']
        self.time_type = 'adjustment'
        self.adjustment = {}

    def _compute(self):
        for name, value in self.state.items():
            min = self.bounds[name]['minimum']
            max = self.bounds[name]['maximum']
            clipped = np.clip(value, a_min=min, a_max=max)
            self.adjustment[name] = clipped - value
        return self.adjustment