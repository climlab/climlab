"""column.py

Object-oriented code for one-dimensional radiative-convective models.

Code developed by Brian Rose, University at Albany
brose@albany.edu
"""
import numpy as np
import climlab.utils.constants as const
from climlab.process.time_dependent_process import TimeDependentProcess
#from climlab.axis import Axis
#from climlab.grid import Grid
import climlab.domain.domain as domain
import climlab.domain.field as field
import climlab.radiation.grey_radiation as grey_radiation
from climlab.convection.convadj import ConvectiveAdjustment


# this should really subclass EnergyBudget
class SingleColumnModel(TimeDependentProcess):
    '''SingleColumnModel has an atmospheric column with radiative transfer
    and a single slab ocean point.'''
    def __init__(self,
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
        # first create the model domains
        if p is not None:
            doms = domain.single_column(water_depth=water_depth,
                                        lev=p)
        else:
            doms = domain.single_column(num_points=num_levels, 
                                        water_depth=water_depth)
        atm = doms['atm']
        sfc = doms['sfc']
        num_levels = atm.axes['lev'].num_points
       # initial surface temperature
        initial = {}
        initial['Ts'] = field.Field(288., domain=sfc)
        # intitial column temperature
        initial['Tatm'] = field.Field(np.linspace(initial['Ts']-10., 200., num_levels), domain=atm)
         #  Create process data structures
        super(SingleColumnModel, self).__init__(state=initial, timestep=timestep, **kwargs)
        #  Attach all parameters to the object
        #self.grid = grid
        self.param['water_depth'] = water_depth
        self.param['albedo_sfc'] = albedo_sfc
        self.param['Q'] = Q
        #self.abs_coeff = abs_coeff

       #if grid is None:
        #    # Make a new grid using given number of levels
        #    pAxis = Axis(axisType='lev', num_points=num_levels)
        #    self.grid = Grid(lev=pAxis)
        #if p is not None:
        #    # Make a new grid using the p array
        #    pAxis = Axis(axisType='lev', points=p)
        #    self.grid = Grid(lev=pAxis)
        self.param['num_levels'] = num_levels
        self.param['abs_coeff'] = abs_coeff

        # create sub-modesl for longwave and shortwave radiation
        dp = self.state['Tatm'].domain.axes['lev'].delta
        epsLW = grey_radiation.compute_layer_absorptivity(self.param['abs_coeff'], dp)
        epsSW = np.zeros_like(epsLW)
        #longwave = grey_radiation.GreyRadiation_LW(state=self.state,
        #                                           param=self.param,
        #                                           eps=epsLW)
        longwave = grey_radiation.GreyRadiation_LW(state=self.state,
                                                   eps=epsLW,
                                                   **self.param)
        #shortwave = grey_radiation.GreyRadiation_SW(state=self.state,
        #                                            param=self.param,
        #                                            eps=epsSW,
        #                                            albedo_sfc=self.param['albedo_sfc'],
        #                                            Q=Q)
        shortwave = grey_radiation.GreyRadiation_SW(state=self.state,
                                                    eps=epsSW,
                                                    **self.param)
        self.subprocess['LW'] = longwave
        self.subprocess['SW'] = shortwave
        
        #self.set_timestep(num_steps_per_year=const.seconds_per_year /
        #                  timestep)
        

class RadiativeConvectiveModel(SingleColumnModel):
    def __init__(self,                  
                 # lapse rate for convective adjustment, in K / km
                 adj_lapse_rate=None,
                 **kwargs):
        super(RadiativeConvectiveModel, self).__init__(**kwargs)
        self.param['adj_lapse_rate'] = adj_lapse_rate
        self.subprocess['convective adjustment'] = \
            ConvectiveAdjustment(state=self.state, **self.param)

