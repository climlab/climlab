from __future__ import division
from climlab.process.energy_budget import EnergyBudget
from climlab.domain.field import global_mean


class BudykoTransport(EnergyBudget):
    r"""calculates the 1 dimensional heat transport as the difference
    between the local temperature and the global mean temperature.

    :param float b:     budyko transport parameter                      \n
                        - unit: :math:`\\textrm{W} / \\left( \\textrm{m}^2 \\ ^{\circ} \\textrm{C} \\right)`   \n
                        - default value: ``3.81``

    As BudykoTransport is a :class:`~climlab.process.process.Process` it needs
    a state do be defined on. See example for details.

    **Computation Details:** \n

    In a global Energy Balance Model

    .. math::

        C \\frac{dT}{dt} = R\downarrow - R\uparrow - H

    with model state :math:`T`, the energy transport term :math:`H`
    can be described as

    .. math::

        H = b [T - \\bar{T}]

    where :math:`T` is a vector of the model temperature and :math:`\\bar{T}`
    describes the mean value of :math:`T`.

    For further information see :cite:`Budyko_1969`.

    :Example:

        Budyko Transport as a standalone process:

        .. plot:: code_input_manual/example_budyko_transport.py
           :include-source:

    """
    # implemented by m-kreuzer
    def __init__(self, b=3.81, **kwargs):
        super(BudykoTransport, self).__init__(**kwargs)
        self.b = b

    @property
    def b(self):
        r"""the budyko transport parameter in unit
        :math:`\\frac{\\textrm{W}}{\\textrm{m}^2 \\textrm{K}}`

        :getter: returns the budyko transport parameter
        :setter: sets the budyko transport parameter
        :type: float

        """
        return self._b
    @b.setter
    def b(self, value):
        self._b = value
        self.param['b'] = value

    def _compute_heating_rates(self):
        """Computes energy flux convergences to get heating rates in :math:`W/m^2`.

        """
        for varname, value in self.state.items():
            self.heating_rate[varname] = - self.b * (value - global_mean(value))
