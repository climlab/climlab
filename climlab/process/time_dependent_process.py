from __future__ import division
from __future__ import print_function
from builtins import str
from builtins import range
import numpy as np
import copy
from climlab import constants as const
from .process import Process
from climlab.utils import walk
from attrdict import AttrDict


def couple(proclist, name='Parent'):
    #  Union of the two state dictionaries
    new_state = AttrDict()
    new_input = AttrDict()
    all_input = {}
    all_diagnotics_list = []
    timestep = const.seconds_per_year * 1E6  # very long!
    for proc in proclist:
        timestep = np.minimum(timestep, proc.timestep)
        for key in proc.state:
            new_state[key] = proc.state[key]
        all_diagnotics_list += list(proc.diagnostics.keys())
        for key in proc.input:
            all_input[key] = proc.input[key]
    for key in all_input:
        if (key not in new_state) and (key not in all_diagnotics_list):
            # This quantity is still a necessary input for the parent process
            new_input[key] = all_input[key]
    # The newly created parent process has the minimum timestep
    coupled = TimeDependentProcess(state=new_state, timestep=timestep, name=name)
    for proc in proclist:
        coupled.add_subprocess(proc.name, proc)
    for key in new_input:
        coupled.add_input(key, new_input[key])
    return coupled


class TimeDependentProcess(Process):
    """A generic parent class for all time-dependent processes.

    ``TimeDependentProcess`` is a child of the
    :class:`~climlab.process.process.Process` class and therefore inherits
    all those attributes.

    **Initialization parameters** \n

    An instance of ``TimeDependentProcess`` is initialized with the following
    arguments *(for detailed information see Object attributes below)*:

    :param float timestep:  specifies the timestep of the object (optional)
    :param str time_type:   how time-dependent-process should be computed
                            [default: 'explicit']
    :param bool topdown:    whether geneterate *process_types* in regular or
                            in reverse order [default: True]

    **Object attributes** \n

    Additional to the parent class :class:`~climlab.process.process.Process`
    following object attributes are generated during initialization:

    :ivar bool has_process_type_list:
                            information whether attribute *process_types*
                            (which is needed for :func:`compute` and build in
                            :func:`_build_process_type_list`)
                            exists or not. Attribute is set to ``'False'``
                            during initialization.
    :ivar bool topdown:     information whether the list *process_types* (which
                            contains all processes and sub-processes) should be
                            generated in regular or in reverse order.
                            See :func:`_build_process_type_list`.
    :ivar dict timeave:     a time averaged collection of all states and diagnostic
                            processes over the timeperiod that
                            :func:`integrate_years` has been called for last.
    :ivar dict tendencies:  computed difference in a timestep for each state.
                            See :func:`compute` for details.
    :ivar str time_type:    how time-dependent-process should be computed.
                            Possible values are: ``'explicit'``, ``'implicit'``,
                            ``'diagnostic'``, ``'adjustment'``.
    :ivar dict time:        a collection of all time-related attributes of the process.
                            The dictionary contains following items:

        * ``'timestep'``: see initialization parameter
        * ``'num_steps_per_year'``: see :func:`set_timestep` and :func:`timestep` for details
        * ``'day_of_year_index'``: counter how many steps have been integrated in current year
        * ``'steps'``: counter how many steps have been integrated in total
        * ``'days_elapsed'``: time counter for days
        * ``'years_elapsed'``: time counter for years
        * ``'days_of_year'``: array which holds the number of numerical steps per year, expressed in days

    """
    def __init__(self, time_type='explicit', timestep=None, topdown=True, **kwargs):
        # Create the state dataset
        self.tendencies = {}
        super(TimeDependentProcess, self).__init__(**kwargs)
        for name, var in self.state.items():
            self.tendencies[name] = var * 0.
        self.timeave = {}
        if timestep is None:
            self.set_timestep()
        else:
            self.set_timestep(timestep=timestep)
        self.time_type = time_type
        self.topdown = topdown
        self.has_process_type_list = False

    def __add__(self, other):
        newparent = couple([self,other])
        return newparent

    @property
    def timestep(self):
        """The amount of time over which :func:`step_forward` is integrating in unit seconds.

        :getter: Returns the object timestep which is stored in ``self.param['timestep']``.
        :setter: Sets the timestep to the given input. See also :func:`set_timestep`.
        :type: float

        """
        return self.param['timestep']
    @timestep.setter
    def timestep(self, value):
        num_steps_per_year = const.seconds_per_year / value
        timestep_days = value / const.seconds_per_day
        days_of_year = np.arange(0., const.days_per_year, timestep_days)
        self.time = {'timestep': value,
                     'num_steps_per_year': num_steps_per_year,
                     'day_of_year_index': 0,
                     'steps': 0,
                     'days_elapsed': 0,
                     'years_elapsed': 0,
                     'days_of_year': days_of_year,
                     'active_now': True}
        self.param['timestep'] = value

    def set_state(self, name, value):
        super(TimeDependentProcess, self).set_state(name,value)
        # Make sure that the new state variable is added to the tendencies dict
        self.tendencies[name] = value * 0.

    def set_timestep(self, timestep=const.seconds_per_day, num_steps_per_year=None):
        """Calculates the timestep in unit seconds
        and calls the setter function of :func:`timestep`

        :param float timestep:              the amount of time over which
                                            :func:`step_forward` is integrating
                                            in unit seconds [default: 24*60*60]
        :param float num_steps_per_year:    a number of steps per calendar year
                                            (optional)

        If the parameter *num_steps_per_year* is specified and not ``None``,
        the timestep is calculated accordingly and therefore the given input
        parameter *timestep* is ignored.

        """
        if num_steps_per_year is not None:
            timestep = const.seconds_per_year / num_steps_per_year
        # Need a more sensible approach for annual cycle stuff
        self.timestep = timestep

    def compute(self):
        """Computes the tendencies for all state variables given current state
        and specified input.

        The function first computes all diagnostic processes. They don't produce
        any tendencies directly but they may affect the other processes (such as
        change in solar distribution). Subsequently, all tendencies and
        diagnostics for all explicit processes are computed.

        Tendencies due to implicit and adjustment processes need to be
        calculated from a state that is already adjusted after explicit
        alteration. For that reason the explicit tendencies are applied to the
        states temporarily. Now all tendencies from implicit processes are
        calculated by matrix inversions and similar to the explicit tendencies,
        the implicit ones are applied to the states temporarily. Subsequently,
        all instantaneous adjustments are computed.

        Then the changes that were made to the states from explicit and implicit
        processes are removed again as this
        :class:`~climlab.process.time_dependent_process.TimeDependentProcess.compute()`
        function is supposed to calculate only tendencies and not apply them
        to the states.

        Finally, all calculated tendencies from all processes are collected
        for each state, summed up and stored in the dictionary
        ``self.tendencies``, which is an attribute of the time-dependent-process
        object, for which the
        :class:`~climlab.process.time_dependent_process.TimeDependentProcess.compute()`
        method has been called.


        **Object attributes** \n

        During method execution following object attributes are modified:

        :ivar dict tendencies:  dictionary that holds tendencies for all states
                                is calculated for current timestep through
                                adding up tendencies from explicit, implicit and
                                adjustment processes.
        :ivar dict diagnostics: process diagnostic dictionary is updated
                                by diagnostic dictionaries of subprocesses
                                after computation of tendencies.

        """
        #  First reset tendencies to zero -- recomputing them is the point of this method
        for varname in self.tendencies:
            self.tendencies[varname] *= 0.
        if not self.has_process_type_list:
            self._build_process_type_list()
        tendencies = {}
        ignored = self._compute_type('diagnostic')
        tendencies['explicit'] = self._compute_type('explicit')
        #  Tendencies due to implicit and adjustment processes need to be
        #  calculated from a state that is already adjusted after explicit stuff
        #  So apply the tendencies temporarily and then remove them again
        for name, var in self.state.items():
            var += tendencies['explicit'][name] * self.timestep
        # Now compute all implicit processes -- matrix inversions
        tendencies['implicit'] = self._compute_type('implicit')
        #  Same deal ... temporarily apply tendencies from implicit step
        for name, var in self.state.items():
            var += tendencies['implicit'][name] * self.timestep
        # Finally compute all instantaneous adjustments -- expressed as explicit forward step
        tendencies['adjustment'] = self._compute_type('adjustment')
        #  Now remove the changes from the model state
        for name, var in self.state.items():
            var -= ( (tendencies['implicit'][name] + tendencies['explicit'][name]) *
                    self.timestep)
        #  Sum up all subprocess tendencies
        for proctype in ['explicit', 'implicit', 'adjustment']:
            for varname, tend in tendencies[proctype].items():
                self.tendencies[varname] += tend
        # Finally compute my own tendencies, if any
        self_tend = self._compute()
        #  Adjustment processes _compute method returns absolute adjustment
        #  Needs to be converted to rate of change
        if self.time_type == 'adjustment':
            for varname, adj in self_tend.items():
                self_tend[varname] /= self.timestep
        for varname, tend in self_tend.items():
            self.tendencies[varname] += tend
        return self.tendencies

    def _compute_type(self, proctype):
        """Computes tendencies due to all subprocesses of given type
        ``'proctype'``. Also pass all diagnostics up to parent process."""
        tendencies = {}
        for varname in self.state:
            tendencies[varname] = 0. * self.state[varname]
        for proc in self.process_types[proctype]:
            #  Asynchronous coupling
            #  if subprocess has longer timestep than parent
            #  We compute subprocess tendencies once
            #   and apply the same tendency at each substep
            step_ratio = int(proc.timestep / self.timestep)
            #  Does the number of parent steps divide evenly by the ratio?
            #  If so, it's time to do a subprocess step.
            if self.time['steps'] % step_ratio == 0:
                proc.time['active_now'] = True
                tenddict = proc.compute()
            else:
                # proc.tendencies is unchanged from last subprocess timestep if we didn't recompute it above
                proc.time['active_now'] = False
                tenddict = proc.tendencies
            for name, tend in tenddict.items():
                tendencies[name] += tend
            for diagname, value in proc.diagnostics.items():
                self.__setattr__(diagname, value)
        return tendencies

    def _compute(self):
        """Where the tendencies are actually computed...

        Needs to be implemented for each daughter class

        Returns a dictionary with same keys as self.state"""
        tendencies = {}
        for name, value in self.state.items():
            tendencies[name] = value * 0.
        return tendencies

    def _build_process_type_list(self):
        """Generates lists of processes organized by process type.

        Following object attributes are generated or updated:

        :ivar dict process_types:   a dictionary with entries:
                                    ``'diagnostic'``, ``'explicit'``,
                                    ``'implicit'`` and ``'adjustment'`` which
                                    point to a list of processes according to
                                    the process types.

        The ``process_types`` dictionary is created while walking
        through the processes with :func:`~climlab.utils.walk.walk_processes`

        CHANGING THIS TO REFER ONLY TO THE CURRENT LEVEL IN SUBPROCESS TREE

        """
        self.process_types = {'diagnostic': [], 'explicit': [], 'implicit': [], 'adjustment': []}
        #for name, proc, level in walk.walk_processes(self, topdown=self.topdown):
        #    self.process_types[proc.time_type].append(proc)
        for name, proc in self.subprocess.items():
            self.process_types[proc.time_type].append(proc)
        self.has_process_type_list = True

    def step_forward(self):
        """Updates state variables with computed tendencies.

        Calls the :func:`compute` method to get current tendencies for all
        process states. Multiplied with the timestep and added up to the state
        variables is updating all model states.

        :Example:

            ::

                >>> import climlab
                >>> model = climlab.EBM()

                >>> # checking time step counter
                >>> model.time['steps']
                0

                >>> # stepping the model forward
                >>> model.step_forward()

                >>> # step counter increased
                >>> model.time['steps']
                1

        """
        tenddict = self.compute()
        #  Total tendency is applied as an explicit forward timestep
        # (already accounting properly for order of operations in compute() )
        for varname, tend in tenddict.items():
            self.state[varname] += tend * self.timestep
        # Update all time counters for this and all subprocesses in the tree
        #  Also pass diagnostics up the process tree
        for name, proc, level in walk.walk_processes(self, ignoreFlag=True):
            if proc.time['active_now']:
                proc._update_time()

    def compute_diagnostics(self, num_iter=3):
        """Compute all tendencies and diagnostics, but don't update model state.
        By default it will call compute() 3 times to make sure all
        subprocess coupling is accounted for. The number of iterations can
        be changed with the input argument.

        """
        for n in range(num_iter):
            ignored = self.compute()

    def _update_time(self):
        """Increments the timestep counter by one.

        Furthermore ``self.time['days_elapsed']`` and
        ``self.time['num_steps_per_year']`` are updated.

        The function is called by the time stepping methods.

        """
        self.time['steps'] += 1
        # time in days since beginning
        self.time['days_elapsed'] += self.time['timestep'] / const.seconds_per_day
        if self.time['day_of_year_index'] >= self.time['num_steps_per_year']-1:
            self._do_new_calendar_year()
        else:
            self.time['day_of_year_index'] += 1

    def _do_new_calendar_year(self):
        """This function is called once at the end of every calendar year.

        It updates ``self.time['years_elapsed']`` and
        ``self.time['day_of_year_index']``
        """
        self.time['day_of_year_index'] = 0  # back to Jan. 1
        self.time['years_elapsed'] += 1

    def integrate_years(self, years=1.0, verbose=True):
        """Integrates the model by a given number of years.

        :param float years:     integration time for the model in years
                                [default: 1.0]
        :param bool verbose:    information whether model time details
                                should be printed [default: True]

        It calls :func:`step_forward` repetitively and calculates a time
        averaged value over the integrated period for every model state and all
        diagnostics processes.

        :Example:

            ::

                >>> import climlab
                >>> model = climlab.EBM()

                >>> model.global_mean_temperature()
                Field(11.997968598413685)

                >>> model.integrate_years(2.)
                Integrating for 180 steps, 730.4844 days, or 2.0 years.
                Total elapsed time is 2.0 years.

                >>> model.global_mean_temperature()
                Field(13.531055349437258)


        """
        days = years * const.days_per_year
        numsteps = int(self.time['num_steps_per_year'] * years)
        if verbose:
            print("Integrating for " + str(numsteps) + " steps, "
                  + str(days) + " days, or " + str(years) + " years.")
        #  begin time loop
        for count in range(numsteps):
            # Compute the timestep
            self.step_forward()
            if count == 0:
                # on first step only...
                #  This implements a generic time-averaging feature
                # using the list of model state variables
                self.timeave = self.state.copy()
                # add any new diagnostics to the timeave dictionary
                self.timeave.update(self.diagnostics)
                # reset all values to zero
                for varname, value in self.timeave.items():
                # moves on to the next varname if value is None
                # this preserves NoneType diagnostics
                    if value is None:
                        continue
                    self.timeave[varname] = 0*value
            # adding up all values for each timestep
            for varname in list(self.timeave.keys()):
                try:
                    self.timeave[varname] += self.state[varname]
                except:
                    try:
                        self.timeave[varname] += self.diagnostics[varname]
                    except: pass
        # calculating mean values through dividing the sum by number of steps
        for varname, value in self.timeave.items():
            if value is None:
                continue
            self.timeave[varname] /= numsteps
        if verbose:
            print("Total elapsed time is %s years."
                  % str(self.time['days_elapsed']/const.days_per_year))

    def integrate_days(self, days=1.0, verbose=True):
        """Integrates the model forward for a specified number of days.

        It convertes the given number of days into years and calls
        :func:`integrate_years`.

        :param float days:      integration time for the model in days
                                [default: 1.0]
        :param bool verbose:    information whether model time details
                                should be printed [default: True]

        :Example:

            ::

                >>> import climlab
                >>> model = climlab.EBM()

                >>> model.global_mean_temperature()
                Field(11.997968598413685)

                >>> model.integrate_days(80.)
                Integrating for 19 steps, 80.0 days, or 0.219032740466 years.
                Total elapsed time is 0.211111111111 years.

                >>> model.global_mean_temperature()
                Field(11.873680783355553)

        """
        years = days / const.days_per_year
        self.integrate_years(years=years, verbose=verbose)

    def integrate_converge(self, crit=1e-4, verbose=True):
        """Integrates the model until model states are converging.

        :param crit:            exit criteria for difference of iterated
                                solutions [default: 0.0001]
        :type crit:             float
        :param bool verbose:    information whether total elapsed time
                                should be printed [default: True]

        :Example:

            ::

                >>> import climlab
                >>> model = climlab.EBM()

                >>> model.global_mean_temperature()
                Field(11.997968598413685)

                >>> model.integrate_converge()
                Total elapsed time is 10.0 years.

                >>> model.global_mean_temperature()
                Field(14.288155406577301)

        """
        # implemented by m-kreuzer
        for varname, value in self.state.items():
            value_old = copy.deepcopy(value)
            self.integrate_years(1,verbose=False)
            while np.max(np.abs(value_old-value)) > crit :
                value_old = copy.deepcopy(value)
                self.integrate_years(1,verbose=False)
        if verbose == True:
            print("Total elapsed time is %s years."
                  % str(self.time['days_elapsed']/const.days_per_year))
