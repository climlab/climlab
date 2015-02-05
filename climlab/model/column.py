"""column.py

Object-oriented code for one-dimensional radiative-convective models.

Code developed by Brian Rose, University at Albany
brose@albany.edu
"""
import numpy as np
from climlab import constants as const
from climlab.process.time_dependent_process import TimeDependentProcess
from climlab.domain import domain
from climlab.domain.field import Field
from climlab.radiation import insolation, grey_radiation
from climlab.convection.convadj import ConvectiveAdjustment


class SingleColumnModel(TimeDependentProcess):
    '''SingleColumnModel has an atmospheric column with radiative transfer
    and a single slab ocean point.'''
    def __init__(self,
                 lev=None,
                 num_levels=30,
                 water_depth=1.0,
                 albedo_sfc=0.299,
                 Q=341.3,
                 timestep=1. * const.seconds_per_day,
                 # absorption coefficient in m**2 / kg
                 abs_coeff=1.229E-4,
                 **kwargs):
        super(SingleColumnModel, self).__init__(timestep=timestep, **kwargs)
        if not self.domains and not self.state:  # no state vars or domains yet
            # first create the model domains
            if lev is not None:
                sfc, atm = domain.single_column(water_depth=water_depth,
                                                lev=lev)
            else:
                sfc, atm = domain.single_column(num_points=num_levels, 
                                                water_depth=water_depth)
            num_levels = atm.axes['lev'].num_points
            # initial surface temperature
            self.set_state('Ts', Field(288., domain=sfc))
            # intitial column temperature
            self.set_state('Tatm', Field(np.linspace(288.-10., 200., 
                                             num_levels), domain=atm))
        self.param['water_depth'] = water_depth
        self.param['albedo_sfc'] = albedo_sfc
        self.param['Q'] = Q
        self.param['num_levels'] = num_levels
        self.param['abs_coeff'] = abs_coeff

        # create sub-modesl for longwave and shortwave radiation
        dp = self.state['Tatm'].domain.axes['lev'].delta
        absorbLW = grey_radiation.compute_layer_absorptivity(self.param['abs_coeff'], dp)
        absorbLW = Field(absorbLW, domain=self.state['Tatm'].domain)        
        absorbSW = np.zeros_like(absorbLW)
        longwave = grey_radiation.GreyRadiation_LW(state=self.state,
                                                   absorptivity=absorbLW,
                                                   **self.param)
        shortwave = grey_radiation.GreyRadiation_SW(state=self.state,
                                                    absorptivity=absorbSW,
                                                    **self.param)
        Q = insolation.FixedInsolation(S0=self.param['Q'], **self.param)
        self.add_subprocess('LW', longwave)
        self.add_subprocess('SW', shortwave)
        self.add_subprocess('insolation', Q)
    
    # This process has to handle the coupling between insolation and column radiation
    def compute(self):
        self.subprocess['SW'].from_space = \
            self.subprocess['insolation'].diagnostics['insolation']
        

class RadiativeConvectiveModel(SingleColumnModel):
    def __init__(self,                  
                 # lapse rate for convective adjustment, in K / km
                 adj_lapse_rate=None,
                 **kwargs):
        super(RadiativeConvectiveModel, self).__init__(**kwargs)
        self.param['adj_lapse_rate'] = adj_lapse_rate
        self.add_subprocess('convective adjustment', \
            ConvectiveAdjustment(state=self.state, **self.param))

