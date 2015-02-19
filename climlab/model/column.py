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
from climlab.surface.surface_radiation import SurfaceRadiation


#class GreyRadiationModel(TimeDependentProcess):
#    def __init__(self,
#                 lev=None,
#                 num_levels=30,
#                 lat=None,
#                 num_lat=1,
#                 water_depth=1.0,
#                 timestep=1. * const.seconds_per_day,
#                 abs_coeff=1.229E-4,
#                 **kwargs):
#        super(GreyRadiationModel, self).__init__(timestep=timestep, **kwargs)
#        if not self.domains and not self.state:  # no state vars or domains yet
#            # first create the model domains
#            if lev is not None:
#                sfc, atm = domain.single_column(water_depth=water_depth,
#                                                lev=lev)
#            else:
#                sfc, atm = domain.single_column(num_points=num_levels, 
#                                                water_depth=water_depth)
#            num_levels = atm.axes['lev'].num_points
#        

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
                 num_lat=1,
                 **kwargs):
        super(SingleColumnModel, self).__init__(timestep=timestep, **kwargs)
        if not self.domains and not self.state:  # no state vars or domains yet
            # first create the model domains
            #if lev is not None:
            #    sfc, atm = domain.single_column(water_depth=water_depth,
            #                                    lev=lev)
            #else:
            #    sfc, atm = domain.single_column(num_points=num_levels, 
            #                                    water_depth=water_depth)
            # need to re-integrate the above code to allow domains and state
            # vars as arguments            
            if num_lat > 1:
                sfc, atm = domain.zonal_mean_column(num_lat=num_lat, 
                                                    num_lev=num_levels,
                                                    water_depth=water_depth)
            else:
                sfc, atm = domain.single_column(num_points=num_levels, 
                                                water_depth=water_depth)
            num_levels = atm.axes['lev'].num_points
            # initial surface temperature
            self.set_state('Ts', Field(288.*np.ones(sfc.shape), domain=sfc))
            # intitial column temperature
            Tinitial = np.tile(np.linspace(288.-10., 200., num_levels),
                               sfc.shape)
            self.set_state('Tatm', Field(Tinitial, domain=atm))
        self.param['water_depth'] = water_depth
        self.param['albedo_sfc'] = albedo_sfc
        self.param['Q'] = Q
        self.param['num_levels'] = num_levels
        self.param['abs_coeff'] = abs_coeff

        # create sub-modesl for longwave and shortwave radiation
        dp = self.Tatm.domain.axes['lev'].delta
        absorbLW = grey_radiation.compute_layer_absorptivity(self.param['abs_coeff'], dp)
        absorbLW = Field(np.tile(absorbLW, sfc.shape), domain=self.Tatm.domain)
        absorbSW = np.zeros_like(absorbLW)
        longwave = grey_radiation.GreyRadiation_LW(state=self.state,
                                                   absorptivity=absorbLW,
                                                   **self.param)
        shortwave = grey_radiation.GreyRadiation_SW(state=self.state,
                                                    absorptivity=absorbSW,
                                                    **self.param)
        thisQ = self.param['Q']*np.ones_like(self.Ts)
        Q = insolation.FixedInsolation(S0=thisQ, domain=sfc, **self.param)
        surface = SurfaceRadiation(state=self.state, **self.param)
        self.add_subprocess('LW', longwave)
        self.add_subprocess('SW', shortwave)
        self.add_subprocess('insolation', Q)
        self.add_subprocess('surface', surface)
    
    # This process has to handle the coupling between insolation and column radiation
    def compute(self):
        self.subprocess['SW'].flux_from_space = \
            self.subprocess['insolation'].diagnostics['insolation']
        self.subprocess['SW'].albedo_sfc = self.subprocess['surface'].albedo_sfc
        self.subprocess['surface'].LW_from_atm = self.subprocess['LW'].flux_to_sfc
        self.subprocess['surface'].SW_from_atm = self.subprocess['SW'].flux_to_sfc
        self.subprocess['LW'].flux_from_sfc = self.subprocess['surface'].LW_to_atm
        

class RadiativeConvectiveModel(SingleColumnModel):
    def __init__(self,                  
                 # lapse rate for convective adjustment, in K / km
                 adj_lapse_rate=None,
                 **kwargs):
        super(RadiativeConvectiveModel, self).__init__(**kwargs)
        self.param['adj_lapse_rate'] = adj_lapse_rate
        self.add_subprocess('convective adjustment', \
            ConvectiveAdjustment(state=self.state, **self.param))

