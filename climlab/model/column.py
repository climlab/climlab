"""Object-oriented code for radiative-convective models with grey-gas radiation.

Code developed by Brian Rose, University at Albany
brose@albany.edu

Note that the column models by default represent global, time averages.
Thus the insolation is a prescribed constant.

Here is an example to implement seasonal insolation at 45 degrees North

    :Example:

        .. code-block:: python

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
from __future__ import division
import numpy as np
from climlab import constants as const
from climlab.process.time_dependent_process import TimeDependentProcess
from climlab.domain.initial import column_state
from climlab.domain.field import Field
from climlab.radiation.insolation import FixedInsolation
from climlab.radiation.greygas import GreyGas, GreyGasSW
from climlab.convection.convadj import ConvectiveAdjustment
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
        # Check to see if an initial state is already provided
        #  If not, make one
        if 'state' not in kwargs:
            state = column_state(num_lev, num_lat, lev, lat, water_depth)
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
        longwave = GreyGas(state=self.state, absorptivity=absorbLW,
                             albedo_sfc=0)
        shortwave = GreyGasSW(state=self.state, absorptivity=absorbSW,
                                albedo_sfc=self.param['albedo_sfc'])
        # sub-model for insolation ... here we just set constant Q
        thisQ = self.param['Q']*np.ones_like(self.Ts)
        Q = FixedInsolation(S0=thisQ, domains=sfc, **self.param)
        self.add_subprocess('LW', longwave)
        self.add_subprocess('SW', shortwave)
        self.add_subprocess('insolation', Q)
        newdiags = ['OLR',
                    'LW_down_sfc',
                    'LW_up_sfc',
                    'LW_absorbed_sfc',
                    'LW_absorbed_atm',
                    'LW_emission',
                    'ASR',
                    'SW_absorbed_sfc',
                    'SW_absorbed_atm',
                    'SW_up_sfc',
                    'SW_up_TOA',
                    'SW_down_TOA',
                    'planetary_albedo']
        for name in newdiags:
            self.add_diagnostic(name)
        # This process has to handle the coupling between
        # insolation and column radiation
        self.subprocess['SW'].flux_from_space = \
            self.subprocess['insolation'].diagnostics['insolation']

    def _compute(self):
        # set diagnostics
        self.do_diagnostics()
        # no tendencies for the parent process
        tendencies = {}
        for name, var in self.state.items():
            tendencies[name] = var * 0.
        return tendencies

    def do_diagnostics(self):
        '''Set all the diagnostics from long and shortwave radiation.'''
        self.OLR = self.subprocess['LW'].flux_to_space
        self.LW_down_sfc = self.subprocess['LW'].flux_to_sfc
        self.LW_up_sfc = self.subprocess['LW'].flux_from_sfc
        self.LW_absorbed_sfc = self.LW_down_sfc - self.LW_up_sfc
        self.LW_absorbed_atm = self.subprocess['LW'].absorbed
        self.LW_emission = self.subprocess['LW'].emission
            #  contributions to OLR from surface and atm. levels
            #self.diagnostics['OLR_sfc'] = self.flux['sfc2space']
            #self.diagnostics['OLR_atm'] = self.flux['atm2space']
        self.ASR = (self.subprocess['SW'].flux_from_space -
                    self.subprocess['SW'].flux_to_space)
        #self.SW_absorbed_sfc = (self.subprocess['surface'].SW_from_atm -
        #                            self.subprocess['surface'].SW_to_atm)
        self.SW_absorbed_atm = self.subprocess['SW'].absorbed
        self.SW_down_sfc = self.subprocess['SW'].flux_to_sfc
        self.SW_up_sfc = self.subprocess['SW'].flux_from_sfc
        self.SW_absorbed_sfc = self.SW_down_sfc - self.SW_up_sfc
        self.SW_up_TOA = self.subprocess['SW'].flux_to_space
        self.SW_down_TOA = self.subprocess['SW'].flux_from_space
        self.planetary_albedo = (self.subprocess['SW'].flux_to_space /
                                 self.subprocess['SW'].flux_from_space)


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
        h2o = ManabeWaterVapor(state=self.state, **self.param)
        self.add_subprocess('H2O', h2o)
        # q is an input field for this process, which is set by subproc
        #  (though in this sense it is actually diagnostic...)
        newinput = ['q']
        self.add_input('q')
        self.q = self.subprocess['H2O'].q

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
                                emissivity_sfc=0.,
                                albedo_sfc=self.param['albedo_sfc'])
        self.add_subprocess('LW', longwave)
        self.add_subprocess('SW', shortwave)
        # This process has to handle the coupling between
        # insolation and column radiation
        self.subprocess['SW'].flux_from_space = \
            self.subprocess['insolation'].diagnostics['insolation']


def compute_layer_absorptivity(abs_coeff, dp):
    '''Compute layer absorptivity from a constant absorption coefficient.'''
    return (2. / (1 + 2. * const.g / abs_coeff /
                         (dp * const.mb_to_Pa)))
