import numpy as np
import climlab.utils.constants as const
from climlab.process.process import _Process
from climlab.utils.walk import walk_processes


# Have to fix the way I handle time!!
# totally inappropriate for e.g. box models

class _TimeDependentProcess(_Process):
    '''A generic parent class for all time-dependent processes.'''
    def __init__(self, time_type='explicit', **kwargs):
        # Create the state dataset
        super(_TimeDependentProcess, self).__init__(**kwargs)
        self.tendencies = {}
        self.timeave = {}
        self.set_timestep()
        self.time_type = time_type

    def set_timestep(self, num_steps_per_year=90):
        '''Change the timestep, given a number of steps per calendar year.'''
        timestep = const.seconds_per_year / num_steps_per_year
        timestep_days = timestep / const.seconds_per_day
        days_of_year = np.arange(0., const.days_per_year, timestep_days)
        self.time = {'timestep': timestep,
                     'num_steps_per_year': num_steps_per_year,
                     'day_of_year_index': 0,
                     'steps': 0,
                     'days_elapsed': 0,
                     'years_elapsed': 0,
                     'days_of_year': days_of_year}
        self.param['timestep'] = timestep

    def compute(self):
        '''By default, the tendency is zero.'''
        for varname in self.state.keys():
            self.tendencies[varname] = np.zeros_like(self.state[varname])

    def _build_process_type_list(self):
        '''Generate lists of processes organized by process type
        Currently, this can be 'explicit', 'implicit', or 'adjustment'.'''
        self.process_types = {'explicit': [], 'implicit': [], 'adjustment': []}        
        for proc in walk_processes(self):
            self.process_types[proc.time_type].append(proc)
        self.has_process_type_list = True
        
    def step_forward(self):
        '''new oop climlab... just loop through processes
        and add up the tendencies'''
        if not self.has_process_type_list:
            self._build_process_type_list()
        # Compute tendencies and diagnostics for all explicit processes
        for proc in self.process_types['explicit']:
            proc.compute()
        # Update state variables using all explicit tendencies
        for proc in self.process_types['explicit']:
            for varname in proc.state.keys():
                try: proc.state[varname] += proc.tendencies[varname]
                except: pass
        # Now compute all implicit processes -- matrix inversions
        for proc in self.process_types['implicit']:
            proc.compute()
            for varname in proc.state.keys():
                try: proc.state[varname] += proc.tendencies[varname]
                except: pass
        # Adjustment processes change the state instantaneously
        for proc in self.process_types['adjustment']:
            proc.compute()
            for varname, value in proc.state.iteritems():
                proc.set_state(varname, proc.adjusted_state[varname])
        # Gather all diagnostics
        for procs in walk_processes(self):
            self.diagnostics.update(procs.diagnostics)
        self._update_time()

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
        #  This implements a generic time-averaging feature
        # using the list of model state variables
        self.timeave = dict(self.state.items() + self.diagnostics.items())
        for varname, value in self.timeave.items():
            self.timeave[varname] = np.zeros_like(value)
        #  begin time loop
        for count in range(numsteps):
            self.step_forward()
            for varname, value in self.timeave.iteritems():
                self.timeave[varname] += value
        for varname, value in self.timeave.iteritems():
            self.timeave[varname] /= numsteps
        if verbose:
            print("Total elapsed time is %s years." 
                  % str(self.time['days_elapsed']/const.days_per_year))

    def integrate_days(self, days=1.0, verbose=True):
        '''Timestep the model forward a specified number of days.'''
        years = days / const.days_per_year
        self.integrate_years(years=years, verbose=verbose)
