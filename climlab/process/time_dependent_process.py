import numpy as np
import copy
from climlab import constants as const
from climlab.process.process import Process
from climlab.utils.walk import walk_processes


class TimeDependentProcess(Process):
    '''A generic parent class for all time-dependent processes.'''
    def __init__(self, time_type='explicit', timestep=None, topdown=True, **kwargs):
        # Create the state dataset
        super(TimeDependentProcess, self).__init__(**kwargs)
        self.tendencies = {}
        for name, var in self.state.iteritems():
            self.tendencies[name] = var * 0.
        self.timeave = {}
        if timestep is None:
            self.set_timestep()
        else:
            self.set_timestep(timestep=timestep)
        self.time_type = time_type
        self.topdown = topdown
        self.has_process_type_list = False

    @property
    def timestep(self):
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
                     'days_of_year': days_of_year}
        self.param['timestep'] = value

    def set_timestep(self, timestep=const.seconds_per_day, num_steps_per_year=None):
        '''Change the timestep.
        Input is either timestep in seconds,
        or
        num_steps_per_year: a number of steps per calendar year.'''
        if num_steps_per_year is not None:
            timestep = const.seconds_per_year / num_steps_per_year
        # Need a more sensible approach for annual cycle stuff
        self.timestep = timestep

    def compute(self):
        '''Compute tendencies for all state variables given
        current state and specified input.'''
        if not self.has_process_type_list:
            self._build_process_type_list()
        # First compute all strictly diagnostic processes
        ignored = self._compute_type('diagnostic')
        # Compute tendencies and diagnostics for all explicit processes
        tendencies_explicit = self._compute_type('explicit')
        #  Tendencies due to implicit and adjustment processes need to be
        #  calculated from a state that is already adjusted after explicit stuff
        #  So apply the tendencies temporarily and then remove them again
        for name, var in self.state.iteritems():
            var += tendencies_explicit[name] * self.timestep
        # Now compute all implicit processes -- matrix inversions
        tendencies_implicit = self._compute_type('implicit')
        for name, var in self.state.iteritems():
            var += tendencies_implicit[name] * self.timestep
        # Finally compute all instantaneous adjustments
        #  and express in terms of discrete timestep
        adjustments = self._compute_type('adjustment')
        tendencies_adjustment = {}
        for name in adjustments:
            tendencies_adjustment[name] = adjustments[name] / self.timestep
        #  Now remove the changes from the model state
        for name, var in self.state.iteritems():
            var -= ( (tendencies_implicit[name] + tendencies_explicit[name]) *
                    self.timestep)
        # Finally sum up all the tendencies from all processes
        self.tendencies = {}
        for varname in self.state:
            self.tendencies[varname] = 0. * self.state[varname]
        for tend_dict in [tendencies_explicit,
                          tendencies_implicit,
                          tendencies_adjustment]:
            for name in tend_dict:
                self.tendencies[name] += tend_dict[name]
        #  pass diagnostics up the process tree
        for name, proc in self.subprocess.iteritems():
            #self.add_diagnostics(proc.diagnostics.keys())
            self.diagnostics.update(proc.diagnostics)

    def _compute_type(self, proctype):
        '''Compute tendencies due to all subprocesses of given type.'''
        tendencies = {}
        for varname in self.state:
            tendencies[varname] = 0. * self.state[varname]
        for proc in self.process_types[proctype]:
            proctend = proc._compute()
            for varname, tend in proctend.iteritems():
                tendencies[varname] += tend
        return tendencies

    def _compute(self):
        # where the tendencies are actually computed...
        #  needs to be implemented for each daughter class
        #  needs to return a dictionary with same keys as self.state
        tendencies = {}
        for name, value in self.state.iteritems():
            tendencies[name] = value * 0.
        return tendencies

    def _build_process_type_list(self):
        '''Generate lists of processes organized by process type
        Currently, this can be 'diagnostic', 'explicit', 'implicit', or 'adjustment'.'''
        self.process_types = {'diagnostic': [], 'explicit': [], 'implicit': [], 'adjustment': []}
        for name, proc, level in walk_processes(self, topdown=self.topdown):
            self.process_types[proc.time_type].append(proc)
        self.has_process_type_list = True

    def step_forward(self):
        '''Call the compute() method to get current tendencies, then
        apply them to update state variables.'''
        self.compute()
        #  Total tendency is applied as an explicit forward timestep
        # (already accounting properly for order of operations in compute() )
        for name, var in self.state.iteritems():
            var += self.tendencies[name] * self.param['timestep']

        # Update all time counters for this and all subprocesses in the tree
        for name, proc, level in walk_processes(self):
            proc._update_time()

    def compute_diagnostics(self, num_iter=3):
        '''Compute all tendencies and diagnostics, but don't update model state.
        By default it will call compute() 3 times to make sure all
        subprocess coupling is accounted for. The number of iterations can
        be changed with the input argument.'''
        for n in range(num_iter):
            self.compute()

    def _update_time(self):
        '''Increment the timestep counter by one.
        This function is called by the timestepping routines.'''
        self.time['steps'] += 1
        # time in days since beginning
        self.time['days_elapsed'] += self.time['timestep'] / const.seconds_per_day
        if self.time['day_of_year_index'] >= self.time['num_steps_per_year']-1:
            self._do_new_calendar_year()
        else:
            self.time['day_of_year_index'] += 1

    def _do_new_calendar_year(self):
        '''This function is called once at the end of every calendar year.'''
        self.time['day_of_year_index'] = 0  # back to Jan. 1
        self.time['years_elapsed'] += 1

    def integrate_years(self, years=1.0, verbose=True):
        '''Timestep the model forward a specified number of years.'''
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
                for varname, value in self.timeave.iteritems():
                    self.timeave[varname] = 0*value
            for varname in self.timeave.keys():
                try:
                    self.timeave[varname] += self.state[varname]
                except:
                    try:
                        self.timeave[varname] += self.diagnostics[varname]
                    except: pass
        for varname in self.timeave.keys():
            self.timeave[varname] /= numsteps
        if verbose:
            print("Total elapsed time is %s years."
                  % str(self.time['days_elapsed']/const.days_per_year))

    def integrate_days(self, days=1.0, verbose=True):
        '''Timestep the model forward a specified number of days.'''
        years = days / const.days_per_year
        self.integrate_years(years=years, verbose=verbose)

    def integrate_converge(self, crit=1e-4, verbose=True):
        '''integrate until solution is converging
        param:  crit    - exit criteria for difference of iterated solutions
        '''
        for varname, value in self.state.iteritems():
            value_old = copy.deepcopy(value)
            self.integrate_years(1,verbose=False)
            while np.max(np.abs(value_old-value)) > crit :
                value_old = copy.deepcopy(value)
                self.integrate_years(1,verbose=False)
        if verbose == True:
            print("Total elapsed time is %s years."
                  % str(self.time['days_elapsed']/const.days_per_year))
