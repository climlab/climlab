"""column.py

Object-oriented code for one-dimensional radiative-convective models.

Code developed by Brian Rose, University at Albany
brose@albany.edu
"""


import numpy as np
import constants as const
from convadj import convective_adjustment
from model import _TimeSteppingModel
from transmissivity import set_transmissitivity
import flux


class Column(_TimeSteppingModel):
    """The Column object represents surface temperature and air temperature
    on grid of pressure levels."""
#    def __str__(self):
        #return ("Instance of Column class with surface temperature " +
        #        str(self.Ts) + " K, and " + str(self.num_levels) +
        #        " atmospheric levels.")
            #  WILL WANT TO MAKE SOME BETTER OUTPUT HERE.
#        return ("Instance of Column class with surface temperature " +
#                str(self.Ts) + " K, " + str(self.num_levels) +
#                " atmospheric levels, and parameters: \n" + str(self.params))

    def __init__(self,
                 p=None,
                 #  first set all parameters to sensible default values
                 num_levels=30,
                 water_depth=1.0,
                 albedo=0.299,
                 Q=341.3,
                 timestep=1. * const.seconds_per_day,
                 # absorption coefficient in m**2 / kg
                 abs_coeff=1.229E-4,
                 # lapse rate for convective adjustment, in K / km
                 adj_lapse_rate=None,
                 **kwargs):
        #  First create the dataset
        super(Column, self).__init__(**kwargs)
        #  Attach all parameters to the object
        self.p = p
        
        self.param['num_levels'] = num_levels
        self.param['water_depth'] = water_depth
        self.param['albedo'] = albedo
        self.param['Q'] = Q
        #self.timestep = timestep
        self.param['abs_coeff'] = abs_coeff
        self.param['adj_lapse_rate'] = adj_lapse_rate

        if p is None:
            #  pressure interval in hPa or mb
            self.dp = const.ps / self.param['num_levels']
            #  pressure at each level, in hPa or mb
            self.p = np.linspace(const.ps - self.dp/2,
                                 self.dp/2, self.param['num_levels'])
        else:
            # if pressure levels are provided:
            #  assume layer boundaries occur halfway between the given levels
            self.p = np.flipud(np.sort(p))  # ensures that pressure decreases
            N = self.p.size
            self.pbounds = np.concatenate(([const.ps], (self.p[0:N-1] +
                                          self.p[1:N])/2, [0.]))
            self.dp = np.flipud(np.diff(np.flipud(self.pbounds)))
            self.param['num_levels'] = N

        # initial surface temperature
        self.Ts = 288.
        # intitial column temperature
        self.Tatm = np.linspace(self.Ts-10., 200., self.param['num_levels'])
        #  heat capacity of atmospheric layers
        self.c_atm = const.cp * self.dp * const.mb_to_Pa / const.g
        #  heat capacity of surface in J / m**2 / K
        self.c_sfc = const.cw * const.rho_w * self.param['water_depth']
        self.set_LW_emissivity()
        self.set_SW_absorptivity()

        self.set_timestep(num_steps_per_year=const.seconds_per_year /
                          timestep)
        #  A dictionary of the model state variables
        #self.state = {'Ts': self.Ts, 'Tatm': self.Tatm}

    def set_LW_emissivity(self, eps=None):
        """Set the longwave emissivity / absorptivity eps for the Column."""
        if eps is None:
            # default is to set eps equal at every level
            # use value consistent with the absorption coefficient parameter
            self.eps = (2. / (1 + 2. * const.g / self.param['abs_coeff'] /
                        (self.dp * const.mb_to_Pa)))
        elif (np.isscalar(eps) or eps.size == self.param['num_levels']):
            self.eps = eps
        else:
            raise ValueError('eps must be scalar or have exactly ' +
                             self.num_levels + ' elements.')

        if np.isscalar(self.eps):
            self.LWtrans = set_transmissitivity(self.eps * np.ones_like(self.p))
        else:
            self.LWtrans = set_transmissitivity(self.eps)

    def set_SW_absorptivity(self, eps=None):
        """Set the shortwave absorptivity eps for the Column."""
        if eps is None:
            # default is no shortwave absorption
            self.SWtrans = set_transmissitivity(np.zeros_like(self.p))
        elif np.isscalar(eps):
            # passing a single scalar sets eps equal to this number everywhere
            self.SWtrans = set_transmissitivity(self.eps * np.ones_like(self.p))
        elif np.size(eps) == self.num_levels:
            self.SWtrans = set_transmissitivity(eps)
        else:
            raise ValueError('eps must be scalar or have exactly ' +
                             self.num_levels + ' elements.')

    def longwave_heating(self):
        """Compute the net longwave radiative heating at every level
        and the surface. Also store the upwelling longwave radiation at the top
        (OLR), and the downwelling longwave radiation at the surface.
        """
        eps = self.LWtrans['absorb']
        # emissions from surface and each layer
        self.emit_sfc = const.sigma * self.Ts**4.
        self.emit_atm = eps * const.sigma * self.Tatm**4.
        absorbed_sfc, absorbed_atm, OLR, LWdown_sfc, OLR_sfc, OLR_atm = \
            flux.LWflux(self.emit_atm, self.emit_sfc, self.LWtrans)
        self.LW_down_sfc = LWdown_sfc
        self.OLR_sfc = OLR_sfc
        self.OLR_atm = OLR_atm
        self.OLR = OLR
        self.LW_absorbed_sfc = absorbed_sfc
        self.LW_absorbed_atm = absorbed_atm

    def shortwave_heating(self):
        '''Net shortwave heating at each level.'''
        self.SWdown_TOA = self.param['Q']
        absorbed_sfc, absorbed_atm, absorbed_total, incident_sfc, up2space, planetary_albedo = \
            flux.SWflux(self.SWdown_TOA, self.param['albedo'], self.SWtrans)
        self.SWdown_sfc = incident_sfc
        self.SW_absorbed_sfc = absorbed_sfc
        self.SW_absorbed_atm = absorbed_atm
        self.SWup_TOA = up2space
        self.SW_absorbed_total = absorbed_total
        self.planetary_albedo = planetary_albedo

    def rad_temperature_tendency(self):
        """Compute net radiative heating everywhere in the Column (in W/m^2),
        and the resulting temperature change over a specified timestep (in K).
        """
        # compute longwave heating rates
        self.longwave_heating()
        self.shortwave_heating()
        # net radiative forcing
        self.rad_heating_sfc = self.SW_absorbed_sfc + self.LW_absorbed_sfc
        self.rad_heating_atm = self.SW_absorbed_atm + self.LW_absorbed_atm
        #  temperature tendencies due only to radiation
        self.rad_temp_tendency_sfc = (self.rad_heating_sfc * self.time['timestep'] /
                                      self.c_sfc)
        self.rad_temp_tendency_atm = (self.rad_heating_atm * self.time['timestep'] /
                                      self.c_atm)

    def step_forward(self):
        """Update the Column temperature. 
        
        Calls rad_temperature_tendency() to compute radiative tendencies,
        and if a lapse rate is specified in params['adj_lapse_rate'], also calls convective_adjustment().
        """
        self.rad_temperature_tendency()
        self.Ts += self.rad_temp_tendency_sfc
        self.Tatm += self.rad_temp_tendency_atm
        if self.param['adj_lapse_rate'] is not None:
            self.unstable_Ts = self.Ts
            self.unstable_Tatm = self.Tatm
            Tcol = np.concatenate(([self.Ts], self.Tatm))
            pnew = np.concatenate(([const.ps], self.p))
            cnew = np.concatenate(([self.c_sfc], self.c_atm *
                                  np.ones_like(self.p)))
            Tadj = convective_adjustment(pnew, Tcol, cnew,
                                         lapserate=self.param['adj_lapse_rate'])
            self.Ts = Tadj[0]
            self.Tatm = Tadj[1:self.num_levels+1]
        super(Column, self).step_forward()
