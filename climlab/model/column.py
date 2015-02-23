"""column.py

Object-oriented code for radiative-convective models with grey-gas radiation.

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


class GreyRadiationModel(TimeDependentProcess):
    def __init__(self,
                 num_lev=30,
                 num_lat=1,
                 lev=None,
                 lat=None,
                 water_depth=1.0,
                 albedo_sfc=0.299,
                 timestep=1. * const.seconds_per_day,
                 Q=341.3,
                 # absorption coefficient in m**2 / kg
                 abs_coeff=1.229E-4,
                 **kwargs):
        # Check to see if an initial state is already provided
        #  If not, make one
        if 'state' not in kwargs:
            state = self.initial_state(num_lev, num_lat, lev, lat, water_depth)
            kwargs.update({'state': state})
        super(GreyRadiationModel, self).__init__(timestep=timestep, **kwargs)
        self.param['water_depth'] = water_depth
        self.param['albedo_sfc'] = albedo_sfc
        self.param['Q'] = Q
        self.param['abs_coeff'] = abs_coeff

        sfc = self.Ts.domain
        atm = self.Tatm.domain
        # create sub-modesl for longwave and shortwave radiation
        dp = self.Tatm.domain.lev.delta
        absorbLW = grey_radiation.compute_layer_absorptivity(self.param['abs_coeff'], dp)
        absorbLW = Field(np.tile(absorbLW, sfc.shape), domain=atm)
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
    
    def initial_state(self, num_lev, num_lat, lev, lat, water_depth):
        if num_lat is 1:
            sfc, atm = domain.single_column(water_depth=water_depth,
                                            num_lev=num_lev,
                                            lev=lev)
        else:
            sfc, atm = domain.zonal_mean_column(water_depth=water_depth,
                                                num_lev=num_lev,
                                                lev=lev,
                                                num_lat=num_lat,
                                                lat=lat)
        num_lev = atm.lev.num_points
        Ts = Field(288.*np.ones(sfc.shape), domain=sfc)
        Tinitial = np.tile(np.linspace(288.-10., 200., num_lev), sfc.shape)
        Tatm = Field(Tinitial, domain=atm)
        state = {'Ts': Ts, 'Tatm': Tatm}
        return state
        
    # This process has to handle the coupling between insolation and column radiation
    def compute(self):
        self.subprocess['SW'].flux_from_space = \
            self.subprocess['insolation'].diagnostics['insolation']
        self.subprocess['SW'].albedo_sfc = self.subprocess['surface'].albedo_sfc
        self.subprocess['surface'].LW_from_atm = self.subprocess['LW'].flux_to_sfc
        self.subprocess['surface'].SW_from_atm = self.subprocess['SW'].flux_to_sfc
        self.subprocess['LW'].flux_from_sfc = self.subprocess['surface'].LW_to_atm
      

class RadiativeConvectiveModel(GreyRadiationModel):
    def __init__(self,                  
                 # lapse rate for convective adjustment, in K / km
                 adj_lapse_rate=6.5,
                 **kwargs):
        super(RadiativeConvectiveModel, self).__init__(**kwargs)
        self.param['adj_lapse_rate'] = adj_lapse_rate
        self.add_subprocess('convective adjustment', \
            ConvectiveAdjustment(state=self.state, **self.param))

