from __future__ import division
from climlab.process.time_dependent_process import TimeDependentProcess


class DiagnosticProcess(TimeDependentProcess):
    """A parent class for all processes that are strictly diagnostic,
    namely that do **not** contribute directly to tendencies of state variables.

    During initialization following attribute is set:

    :ivar time_type:        is set to ``'diagnostic'``
    :vartype time_type:     str

    """
    def __init__(self, **kwargs):
        super(DiagnosticProcess, self).__init__(**kwargs)
        self.time_type = 'diagnostic'

    def _compute(self):
        """Diagnotic process returns no tendencies.

        """
        return {}
