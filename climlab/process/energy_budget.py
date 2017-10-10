from __future__ import division
import numpy as np
from climlab.process.time_dependent_process import TimeDependentProcess


class EnergyBudget(TimeDependentProcess):
    r"""A parent class for explicit energy budget processes.

    This class solves equations that include a heat capacitiy term like
    :math:`C \\frac{dT}{dt} = \\textrm{flux convergence}`

    In an Energy Balance Model with model state :math:`T` this equation
    will look like this:

    .. math::

        C \\frac{dT}{dt} = R\downarrow - R\uparrow - H  \n
        \\frac{dT}{dt} = \\frac{R\downarrow}{C} - \\frac{R\uparrow}{C} - \\frac{H}{C}

    Every EnergyBudget object has a ``heating_rate`` dictionary with items
    corresponding to each state variable. The heating rate accounts the actual
    heating of a subprocess, namely the contribution to the energy budget
    of :math:`R\\downarrow, R\\uparrow` and :math:`H` in this case.
    The temperature tendencies for each subprocess are then calculated
    through dividing the heating rate by the heat capacitiy :math:`C`.

    **Initialization parameters** \n

    An instance of ``EnergyBudget`` is initialized with the forwarded
    keyword arguments ``**kwargs`` of the corresponding children classes.

    **Object attributes** \n

    Additional to the parent class
    :class:`~climlab.process.timedependentprocess.TimeDependentProcess`
    following object attributes are generated or modified during initialization:

    :ivar str time_type:        is set to ``'explicit'``
    :ivar dict heating_rate:    energy share for given subprocess in unit
                                :math:`\\textrm{W}/ \\textrm{m}^2` stored
                                in a dictionary sorted by model states

    """
    def __init__(self, **kwargs):
        super(EnergyBudget, self).__init__(**kwargs)
        self.time_type = 'explicit'
        self.heating_rate = {}

    def _compute_heating_rates(self):
        """Computes energy flux convergences to get heating rates in unit
        :math:`\\textrm{W}/ \\textrm{m}^2`.

        This method should be over-ridden by daughter classes.

        """
        for varname in list(self.state.keys()):
            self.heating_rate[varname] = self.state[varname] * 0.

    def _temperature_tendencies(self):
        self._compute_heating_rates()
        tendencies = {}
        for varname, value in self.state.items():
            #C = self.state_domain[varname].heat_capacity
            C = value.domain.heat_capacity
            try:  # there may be state variables without heating rates
                tendencies[varname] = (self.heating_rate[varname] / C)
            except:
                pass
        return tendencies

    def _compute(self):
        tendencies = self._temperature_tendencies()
        return tendencies


class ExternalEnergySource(EnergyBudget):
    """A fixed energy source or sink to be specified by the user.

    **Object attributes** \n

    Additional to the parent class :class:`~climlab.process.energy_budget.EnergyBudget`
    the following object attribute is modified during initialization:

    :ivar dict heating_rate:    energy share dictionary for this subprocess
                                is set to zero for every model state.

    After initialization the user should modify the fields in the
    ``heating_rate`` dictionary, which contain heating rates in
    unit :math:`\\textrm{W}/ \\textrm{m}^2` for all state variables.

    :Example:

        Creating an Energy Balance Model with a uniform external energy source
        of :math:`10 \\ \\textrm{W}/ \\textrm{m}^2` for all latitudes::

            >>> import climlab
            >>> from climlab.process.energy_budget import ExternalEnergySource
            >>> import numpy as np

            >>> # create model & external energy subprocess
            >>> model = climlab.EBM(num_lat=36)
            >>> ext_en = ExternalEnergySource(state= model.state,**model.param)

            >>> # modify external energy rate
            >>> ext_en.heating_rate.keys()
            ['Ts']

            >>> np.squeeze(ext_en.heating_rate['Ts'])
            Field([-0., -0., -0., -0., -0., -0., -0., -0., -0.,  0.,  0.,  0.,  0.,
                    0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,
                    0., -0., -0., -0., -0., -0., -0., -0., -0., -0.])

            >>> ext_en.heating_rate['Ts'][:]=10

            >>> np.squeeze(ext_en.heating_rate['Ts'])
            Field([ 10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,
                    10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,
                    10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,  10.,
                    10.,  10.,  10.])

            >>> # add subprocess to model
            >>> model.add_subprocess('ext_energy',ext_en)

            >>> print model
            climlab Process of type <class 'climlab.model.ebm.EBM'>.
            State variables and domain shapes:
              Ts: (36, 1)
            The subprocess tree:
            top: <class 'climlab.model.ebm.EBM'>
               diffusion: <class 'climlab.dynamics.diffusion.MeridionalDiffusion'>
               LW: <class 'climlab.radiation.AplusBT.AplusBT'>
               ext_energy: <class 'climlab.process.energy_budget.ExternalEnergySource'>
               albedo: <class 'climlab.surface.albedo.StepFunctionAlbedo'>
                  iceline: <class 'climlab.surface.albedo.Iceline'>
                  cold_albedo: <class 'climlab.surface.albedo.ConstantAlbedo'>
                  warm_albedo: <class 'climlab.surface.albedo.P2Albedo'>
               insolation: <class 'climlab.radiation.insolation.P2Insolation'>

    """
    def __init__(self, **kwargs):
        super(ExternalEnergySource, self).__init__(**kwargs)
        for varname in list(self.state.keys()):
            self.heating_rate[varname] = self.state[varname] * 0.

    def _compute_heating_rates(self):
        pass
