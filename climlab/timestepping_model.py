import numpy as np
import constants as const
from model import _Model


class _TimeSteppingModel(_Model):
    '''A generic parent class for all time-dependent models that use a
    discete forward timestep.'''
    def __init__(self, **kwargs):
        # Create the state dataset
        super(_TimeSteppingModel, self).__init__(**kwargs)
        self.timeave = {}
        self.set_timestep()
        #  Daughter classes will need to do a bunch of other initialization

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

    def step_forward(self):
        '''new oop climlab... just loop through processes
        and add up the tendencies'''
        #for proc in processes:
        #    proc.compute()
        #for proc in processes:
        pass    
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
