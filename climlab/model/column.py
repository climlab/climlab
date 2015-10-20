"""column.py

Object-oriented code for radiative-convective models with grey-gas radiation.

Code developed by Brian Rose, University at Albany
brose@albany.edu

Note that the column models by default represent global, time averages.
Thus the insolation is a prescribed constant.

Here is an example to implement seasonal insolation at 45 degrees North

import climlab
#  create the column model object
col = climlab.GreyRadiationModel()
#  create a new latitude axis with a single point
lat = climlab.domain.Axis(axis_type='lat', points=45.)
#  add this new axis to the surface domain
col.Ts.domain.axes['lat'] = lat
#  create a new insolation process using this domain
Q = climlab.radiation.insolation.DailyInsolation(domains=col.Ts.domain, **col.param)
#  replace the fixed insolation subprocess in the column model
col.add_subprocess('insolation', Q)

This model is now a single column with seasonally varying insolation 
calculated for 45N.
"""
import numpy as np
from climlab import constants as const
from climlab.process.time_dependent_process import TimeDependentProcess
from climlab.domain import domain
from climlab.domain.field import Field
from climlab.radiation.insolation import FixedInsolation
from climlab.radiation.radiation import Radiation, RadiationSW
from climlab.convection.convadj import ConvectiveAdjustment
from climlab.surface.surface_radiation import SurfaceRadiation
from climlab.radiation.nband import ThreeBandSW, FourBandLW, FourBandSW
from climlab.radiation.water_vapor import ManabeWaterVapor

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
        if lat is not None:
            num_lat = np.array(lat).size
        if lev is not None:
            num_lev = np.array(lev).size
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
        # create sub-models for longwave and shortwave radiation
        dp = self.Tatm.domain.lev.delta
        absorbLW = compute_layer_absorptivity(self.param['abs_coeff'], dp)
        absorbLW = Field(np.tile(absorbLW, sfc.shape), domain=atm)
        absorbSW = np.zeros_like(absorbLW)
        longwave = Radiation(state=self.state, absorptivity=absorbLW,
                             albedo_sfc=0)
        shortwave = RadiationSW(state=self.state, absorptivity=absorbSW,
                                albedo_sfc=self.param['albedo_sfc'])
        # sub-model for insolation ... here we just set constant Q
        thisQ = self.param['Q']*np.ones_like(self.Ts)
        Q = FixedInsolation(S0=thisQ, domain=sfc, **self.param)
        #  surface sub-model
        surface = SurfaceRadiation(state=self.state, **self.param)
        self.add_subprocess('LW', longwave)
        self.add_subprocess('SW', shortwave)
        self.add_subprocess('insolation', Q)
        self.add_subprocess('surface', surface)
    
    def initial_state(self, num_lev, num_lat, lev, lat, water_depth):
        return initial_state(num_lev, num_lat, lev, lat, water_depth)
        
    # This process has to handle the coupling between insolation and column radiation
    def compute(self):
        # some handy nicknames for subprocesses
        LW = self.subprocess['LW']
        SW = self.subprocess['SW']
        insol = self.subprocess['insolation']
        surf = self.subprocess['surface']
        # Do the coupling
        SW.flux_from_space = insol.diagnostics['insolation']
        SW.albedo_sfc = surf.albedo_sfc
        surf.LW_from_atm = LW.flux_to_sfc
        surf.SW_from_atm = SW.flux_to_sfc
        LW.flux_from_sfc = surf.LW_to_atm
        # set diagnostics
        self.do_diagnostics()
    
    def do_diagnostics(self):
        '''Set all the diagnostics from long and shortwave radiation.'''
        LW = self.subprocess['LW']
        SW = self.subprocess['SW']
        surf = self.subprocess['surface']
        try: self.diagnostics['OLR'] = LW.flux_to_space
        except: pass
        try: self.diagnostics['LW_down_sfc'] = LW.flux_to_sfc
        except: pass
        try: self.diagnostics['LW_up_sfc'] = surf.LW_to_atm
        except: pass
        try: self.diagnostics['LW_absorbed_sfc'] = (surf.LW_from_atm -
                                                    surf.LW_to_atm)
        except: pass
        try: self.diagnostics['LW_absorbed_atm'] = LW.absorbed
        except: pass
        try: self.diagnostics['LW_emission'] = LW.emission
        except: pass
            #  contributions to OLR from surface and atm. levels
            #self.diagnostics['OLR_sfc'] = self.flux['sfc2space']
            #self.diagnostics['OLR_atm'] = self.flux['atm2space']
        try: self.diagnostics['ASR'] = SW.flux_from_space - SW.flux_to_space
        except: pass
        try: 
            self.diagnostics['SW_absorbed_sfc'] = (surf.SW_from_atm - 
                                                    surf.SW_to_atm)
        except: pass
        try: self.diagnostics['SW_absorbed_atm'] = SW.absorbed
        except: pass
        try: self.diagnostics['SW_down_sfc'] = SW.flux_to_sfc
        except: pass
        try: self.diagnostics['SW_up_sfc'] = SW.flux_from_sfc
        except: pass
        try: self.diagnostics['SW_up_TOA'] = SW.flux_to_space
        except: pass
        try: self.diagnostics['SW_down_TOA'] = SW.flux_from_space
        except: pass
        try: self.diagnostics['SW_absorbed_total'] = (SW.absorbed_total - 
                                                      SW.flux_net[0])
        except: pass
        try: self.diagnostics['planetary_albedo'] = (SW.flux_to_space / 
                                                     SW.flux_from_space)
        except: pass
        try: self.diagnostics['SW_emission'] = SW.emission
        except: pass


def initial_state(num_lev, num_lat, lev, lat, water_depth):
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


class RadiativeConvectiveModel(GreyRadiationModel):
    def __init__(self,                  
                 # lapse rate for convective adjustment, in K / km
                 adj_lapse_rate=6.5,
                 **kwargs):
        super(RadiativeConvectiveModel, self).__init__(**kwargs)
        self.param['adj_lapse_rate'] = adj_lapse_rate
        self.add_subprocess('convective adjustment', \
            ConvectiveAdjustment(state=self.state, **self.param))


class BandRCModel(RadiativeConvectiveModel):
    def __init__(self, **kwargs):
        super(BandRCModel, self).__init__(**kwargs)
        #  Initialize specific humidity
        if 'q' not in self.state:
            q = np.zeros_like(self.Tatm)
            self.set_state('q', q)
        h2o = ManabeWaterVapor(state=self.state, **self.param)
        self.add_subprocess('H2O', h2o)
        
        #  initialize radiatively active gas inventories
        self.absorber_vmr = {}
        self.absorber_vmr['CO2'] = 380.E-6 * np.ones_like(self.Tatm)
        self.absorber_vmr['O3'] = np.zeros_like(self.Tatm)
        # water vapor is actually specific humidity, not VMR.
        self.absorber_vmr['H2O'] = self.q

        longwave = FourBandLW(state=self.state, 
                              absorber_vmr=self.absorber_vmr,
                              albedo_sfc=0.)
        shortwave = ThreeBandSW(state=self.state, 
                                absorber_vmr=self.absorber_vmr,
                                albedo_sfc=self.param['albedo_sfc'])
        self.add_subprocess('LW', longwave)
        self.add_subprocess('SW', shortwave)


def compute_layer_absorptivity(abs_coeff, dp):
    '''Compute layer absorptivity from a constant absorption coefficient.'''
    return (2. / (1 + 2. * const.g / abs_coeff /
                         (dp * const.mb_to_Pa)))
