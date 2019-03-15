from __future__ import absolute_import, division, print_function
from .time_dependent_process import TimeDependentProcess

class ExternalForcing(TimeDependentProcess):
    """A Process class for user-defined tendencies of state variables.
    Useful for combining some prescribed external forcing with an interactive model.

    :Example:
        The user can invoke the process on a dicionary of state variables ``mystate`` like this::

            myforcing = climlab.process.ExternalForcing(state=mystate)

        and then set the desired tendencies in the dictionary ``myforcing.forcing_tendencies``,
        in units of [state variable unit] per second.
    """
    def __init__(self,**kwargs):
        super(ExternalForcing, self).__init__(**kwargs)
        self.forcing_tendencies = {}
        for var in self.state:
            self.forcing_tendencies[var] = 0. * self.state[var]

    def _compute(self):
        return self.forcing_tendencies
