import numpy as np
import constants as const
import netCDF4 as nc
import time


class _Model(object):
    '''A generic parent class for all climlab model objects.
    Every model object has a set of state variables on a spatial grid,
    stored as a netCDF4 dataset.'''
    def __init__(self, filename='ignored.nc', diskless=True, **kwargs):
        # Create a netCDF4.Dataset object
        self.state = nc.Dataset(filename=filename, mode="w",
                                diskless=diskless, format='NETCDF4')
        # Set some global attributes
        self.state.description1 = "Model object created by climlab"
        self.state.description2 = "Model type is " + str(type(self))
        self.state.creation_date = time.strftime("%a, %d %b %Y %H:%M:%S %z",
                                                 time.localtime())
        # This dictionary will hold all model parameters
        self.param = {}


class _TimeSteppingModel(_Model):
    '''A generic parent class for all time-dependent models that use a
    discete forward timestep.'''
    def __init__(self, **kwargs):
        # Create the state dataset
        super(_TimeSteppingModel, self).__init__(**kwargs)
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
        '''Daughter classes need to implement details
        for changes in model state.'''
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
        numsteps = int(self.time['num_steps_per_year'] * years)
        if verbose:
            print("Integrating for " + str(numsteps) + " steps or "
                  + str(years) + " years.")
        #  This implements a generic time-averaging feature
        # using the list of model state variables
        #self.state_timeave = {}
        #for varname, value in self.state.items():
        #    self.state_timeave[varname] = np.zeros_like(value)
        #  begin time loop
        for count in range(numsteps):
            self.step_forward()
        #    for varname, value in self.state.items():
        #        self.state_timeave[varname] += value
        #for varname, value in self.state.items():
        #    self.state_timeave[varname] /= numsteps
        if verbose:
            print("Total elapsed time is " +
                  str(self.time['days_elapsed']/const.days_per_year) + " years.")

    def integrate_days(self, days=1.0, verbose=True):
        '''Timestep the model forward a specified number of days.'''
        numsteps = int(self.time['num_steps_per_year'] / const.days_per_year * days)
        if verbose:
            print("Integrating for " + str(numsteps) + " steps or " +
                  str(days) + " days.")
        #  begin time loop
        for count in range(numsteps):
            self.step_forward()
        if verbose:
            print("Total elapsed time is " +
                  str(self.time['days_elapsed']/const.days_per_year) + " years.")
