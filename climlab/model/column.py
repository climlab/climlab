"""column.py

Object-oriented code for one-dimensional radiative-convective models.

Code developed by Brian Rose, University at Albany
brose@albany.edu
"""
import numpy as np
import climlab.utils.constants as const
from climlab.process.time_dependent_process import _TimeDependentProcess
from climlab.axis import Axis
from climlab.grid import Grid
import climlab.radiation.grey_radiation as grey_radiation
from climlab.convection.convadj import ConvectiveAdjustment


#  Need to modify this to use Domains
# and then test example notebooks again


class SingleColumnModel(_TimeDependentProcess):
    '''SingleColumnModel '''
    def __init__(self,
                 grid=None,
                 p=None,
                 #  first set all parameters to sensible default values
                 num_levels=30,
                 water_depth=1.0,
                 albedo_sfc=0.299,
                 Q=341.3,
                 timestep=1. * const.seconds_per_day,
                 # absorption coefficient in m**2 / kg
                 abs_coeff=1.229E-4,
                 **kwargs):
        #  First create the dataset
        super(SingleColumnModel, self).__init__(**kwargs)
        #  Attach all parameters to the object
        self.grid = grid
        self.param['water_depth'] = water_depth
        self.param['albedo_sfc'] = albedo_sfc
        self.param['Q'] = Q
        #self.abs_coeff = abs_coeff

        if grid is None:
            # Make a new grid using given number of levels
            pAxis = Axis(axisType='lev', num_points=num_levels)
            self.grid = Grid(lev=pAxis)
        if p is not None:
            # Make a new grid using the p array
            pAxis = Axis(axisType='lev', points=p)
            self.grid = Grid(lev=pAxis)
        self.param['num_levels'] = self.grid['lev'].num_points
        self.param['abs_coeff'] = abs_coeff

        # initial surface temperature
        self.state['Ts'] = 288.
        # intitial column temperature
        self.state['Tatm'] = np.linspace(self.state['Ts']-10., 200., self.param['num_levels'])
        
        #  heat capacity of atmospheric layers
        #self.c_atm = heat_capacity.atmosphere(self.grid['lev'].delta)
        #  heat capacity of surface in J / m**2 / K
        #self.c_sfc = heat_capacity.slab_ocean(self.param['water_depth'])

        # create sub-modesl for longwave and shortwave radiation
        epsLW = grey_radiation.compute_layer_absorptivity(self.param['abs_coeff'], self.grid)
        epsSW = np.zeros_like(epsLW)
        longwave = grey_radiation.GreyRadiation_LW(grid=self.grid, state=self.state, param=self.param, eps=epsLW)
        shortwave = grey_radiation.GreyRadiation_SW(grid=self.grid, state=self.state, param=self.param, eps=epsSW, albedo_sfc=self.param['albedo_sfc'], Q=Q)
        self.subprocess['LW'] = longwave
        self.subprocess['SW'] = shortwave
        
#        self.processes['convective adjustment'] = ConvectiveAdjustment(grid=self.grid,
#                state=self.state, adj_lapse_rate=adj_lapse_rate, param=self.param)
        self.set_timestep(num_steps_per_year=const.seconds_per_year /
                          timestep)
        

class RadiativeConvectiveModel(SingleColumnModel):
    def __init__(self,                  
                 # lapse rate for convective adjustment, in K / km
                 adj_lapse_rate=None,
                 **kwargs):
        super(RadiativeConvectiveModel, self).__init__(**kwargs)
        self.param['adj_lapse_rate'] = adj_lapse_rate
        self.processes['convective adjustment'] = ConvectiveAdjustment(grid=self.grid,
                state=self.state, adj_lapse_rate=adj_lapse_rate, param=self.param)

