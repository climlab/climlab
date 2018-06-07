from __future__ import division
from climlab.process.time_dependent_process import TimeDependentProcess


class ImplicitProcess(TimeDependentProcess):
    """A parent class for modules that use implicit time discretization.

    During initialization following attributes are intitialized:

    :ivar time_type:        is set to ``'implicit'``
    :vartype time_type:     str

    :ivar adjustment:       the model state adjustments due to this implicit
                            subprocess
    :vartype adjustment:    dict

    """
    def __init__(self, **kwargs):
        super(ImplicitProcess, self).__init__(**kwargs)
        self.time_type = 'implicit'
        self.adjustment = {}

    def _compute(self):
        """Computes the state variable tendencies in time for implicit processes.

        To calculate the new state the :func:`_implicit_solver()` method is
        called for daughter classes. This however returns the new state of the
        variables, not just the tendencies. Therefore, the adjustment is
        calculated which is the difference between the new and the old state
        and stored in the object's attribute adjustment.

        Calculating the new model states through solving the matrix problem
        already includes the multiplication with the timestep. The derived
        adjustment is divided by the timestep to calculate the implicit
        subprocess tendencies, which can be handeled by the
        :func:`~climlab.process.time_dependent_process.TimeDependentProcess.compute`
        method of the parent
        :class:`~climlab.process.time_dependent_process.TimeDependentProcess` class.

        :ivar dict adjustment:  holding all state variables' adjustments
                                of the implicit process which are the
                                differences between the new states (which have
                                been solved through matrix inversion) and the
                                old states.

        """
        newstate = self._implicit_solver()
        adjustment = {}
        tendencies = {}
        for name, var in self.state.items():
            adjustment[name] = newstate[name] - var
            tendencies[name] = adjustment[name] / self.timestep
        # express the adjustment (already accounting for the finite time step)
        #  as a tendency per unit time, so that it can be applied along with explicit
        self.adjustment = adjustment
        self._update_diagnostics(newstate)
        return tendencies

    def _update_diagnostics(self, newstate):
        '''This method is called each timestep after the new state is computed
        with the implicit solver. Daughter classes can implement this method to
        compute any diagnostic quantities using the new state.'''
        pass
