"""column.py

Object-oriented code for one-dimensional radiative-convective models.

Code developed by Brian Rose, University at Albany
brose@albany.edu
"""


import numpy as np
import constants as const
from convadj import convective_adjustment
from timestepping_model import _TimeSteppingModel
from transmissivity import Transmissivity

# experiment to see if we can use a nc.Dataset object to hold the state variables etc
#  seems to work just fine.
#  but here I've just glued it on to my existing code
#  what we really want to do is define the top-class model object
#  as a sub-class of nc.Dataset!
#   That way every aspect of the model and its run is stored inside a netCDF object

class Column(_TimeSteppingModel):
    """The Column object represents surface temperature and air temperature
    on grid of pressure levels."""
#    def __str__(self):
#        return ("Instance of Column class with surface temperature " +
#                str(self.Ts) + " K, and " + str(self.num_levels) +
#                " atmospheric levels.")
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
        # groups to hold transmissivity data
        self.createGroup('LWtrans')
        self.createGroup('SWtrans')
        #  Attach all parameters to the object
        # self.p = p
        self.param('num_levels', num_levels)
        self.param('water_depth', water_depth)
        self.param('albedo', albedo)
        self.param('Q', Q)
        self.param('abs_coeff', abs_coeff)
        if adj_lapse_rate is not None:
            self.param('adj_lapse_rate', adj_lapse_rate)

        if p is None:
            #  Here trying out a netCDF structure            
            lev = self.createDimension('lev', num_levels)
            levbounds = self.createDimension('lev_bounds', num_levels+1)
            # coordinate variable
            levels = self.createVariable('level','float',('lev',))
            lev_bounds = self.createVariable('lev_bounds','float',('lev_bounds',))
            dp = self.createVariable('dp', 'float', ('lev',))
            dp_constant = const.ps / num_levels
            levels[:] = np.linspace(const.ps - dp_constant/2, dp_constant/2, num_levels)
            lev_bounds[:] = np.concatenate(([const.ps], (levels[0:num_levels-1] +
                                          levels[1:num_levels])/2, [0.]))
            dp[:] = const.ps / num_levels * np.ones_like(levels[:])            
            self.param('num_levels', num_levels)            
        else:
            #  THIS WILL NOT WORK FOR NOW with new dataset structure
            # if pressure levels are provided:
            #  assume layer boundaries occur halfway between the given levels
            self.p = np.flipud(np.sort(p))  # ensures that pressure decreases
            N = self.p.size
            self.pbounds = np.concatenate(([const.ps], (self.p[0:N-1] +
                                          self.p[1:N])/2, [0.]))
            self.dp = np.flipud(np.diff(np.flipud(self.pbounds)))
            self.num_levels = N

        # Create and initialize the state variables
        Ts = self.groups['state'].createVariable('Ts', 'float')
        Tatm = self.groups['state'].createVariable('Tatm', 'float', ('lev',))
        Ts[:] = 288.
        Tatm[:] = np.linspace(Ts[:]-10., 200., num_levels)

        #  heat capacity of atmospheric layers
        c_atm = self.groups['fixed'].createVariable('c_atm', 'float', ('lev',))
        c_atm[:] = const.cp * const.mb_to_Pa / const.g * self.variables['dp'][:]
        #  heat capacity of surface in J / m**2 / K
        c_sfc = self.groups['fixed'].createVariable('c_sfc', 'float')
        c_sfc[:] = const.cw * const.rho_w * self.param('water_depth')
        
        self.set_LW_emissivity()
        self.set_SW_absorptivity()

        self.set_timestep(num_steps_per_year=const.seconds_per_year/timestep)
        #  A dictionary of the model state variables
                          # not needed anymore
        #self.state = {'Ts': self.Ts, 'Tatm': self.Tatm}

    def set_LW_emissivity(self, eps=None):
        """Set the longwave emissivity / absorptivity eps for the Column."""
        if eps is None:
            # default is to set eps equal at every level
            # use value consistent with the absorption coefficient parameter
            eps = (2. / (1 + 2. * const.g / self.param('abs_coeff') /
                        (self.variables['dp'][:] * const.mb_to_Pa)))
        elif (np.isscalar(eps) or eps.size == self.param('num_levels')):
            pass
        else:
            raise ValueError('eps must be scalar or have exactly ' +
                             self.param('num_levels') + ' elements.')
        if np.isscalar(eps):
            p = self.variables['level'][:]
            eps = eps*np.ones_like(p)
        self.set_transmissivity('LWtrans', eps)

    def set_SW_absorptivity(self, eps=None):
        """Set the shortwave absorptivity eps for the Column."""
        if eps is None:
            # default is no shortwave absorption
            eps = np.zeros_like(self.variables['dp'][:])
        elif np.isscalar(eps):
            # passing a single scalar sets eps equal to this number everywhere
            p = self.variables['level'][:]
            eps = eps*np.ones_like(p)
        elif np.size(eps) == self.param('num_levels'):
            pass
        else:
            raise ValueError('eps must be scalar or have exactly ' +
                             self.num_levels + ' elements.')
        self.set_transmissivity('SWtrans', eps)

    def set_transmissivity(self, group, eps):
        trans = Transmissivity(eps)
        thisgroup = self.groups[group]
        thisgroup.createVariable('eps', 'float', ('lev',))
        thisgroup.variables['eps'][:] = eps
        thisgroup.createVariable('absorb', 'float', ('lev',))
        thisgroup.variables['absorb'][:] = trans.absorb
        thisgroup.createVariable('trans', 'float', ('lev',))
        thisgroup.variables['trans'][:] = trans.trans
        thisgroup.createVariable('surf2atm', 'float', ('lev',))
        thisgroup.variables['surf2atm'][:] = trans.surf2atm
        thisgroup.createVariable('atm2space', 'float', ('lev',))
        thisgroup.variables['atm2space'][:] = trans.atm2space
        thisgroup.createVariable('surf2space', 'float')
        thisgroup.variables['atm2space'][:] = trans.surf2space
        thisgroup.createVariable('atm2atm', 'float', ('lev', 'lev',))
        thisgroup.variables['atm2atm'][:] = trans.atm2atm


    def longwave_heating(self):
        """Compute the net longwave radiative heating at every level
        and the surface. Also store the upwelling longwave radiation at the top
        (OLR), and the downwelling longwave radiation at the surface.
        """
        eps = self.groups['LWtrans'].variables['absorb'][:]
        # emissions from surface and each layer
        self.emit_sfc = const.sigma * self.Ts**4.
        self.emit_atm = eps * const.sigma * self.Tatm**4.

        self.LW_down_sfc = np.dot(self.LWtrans.surf2atm, self.emit_atm)
        self.OLR_sfc = self.LWtrans.surf2space * self.emit_sfc
        self.OLR_atm = self.LWtrans.atm2space * self.emit_atm
        self.OLR = self.OLR_sfc + np.sum(self.OLR_atm)
        self.LW_absorbed_sfc = self.LW_down_sfc - self.emit_sfc

        incident_fromsfc = self.emit_sfc * self.LWtrans.surf2atm
        incident_fromatm = np.dot(self.LWtrans.atm2atm, self.emit_atm)
        self.LW_absorbed_atm = ((incident_fromatm + incident_fromsfc) * eps
                                - 2 * self.emit_atm)

    def shortwave_heating(self):
        '''Net shortwave heating at each level.'''
        self.SWdown_TOA = self.Q
        SW_incident_fromabove = self.SWdown_TOA * self.SWtrans.atm2space
        self.SWdown_sfc = self.SWdown_TOA * self.SWtrans.surf2space
        self.SWup_sfc = self.albedo * self.SWdown_sfc
        self.SW_absorbed_sfc = self.SWdown_sfc - self.SWup_sfc
        SW_incident_frombelow = self.SWup_sfc * self.SWtrans.surf2atm
        self.SW_absorbed_atm = ((SW_incident_fromabove + SW_incident_frombelow)
                                * self.SWtrans.absorb)
        self.SWup_TOA = self.SWup_sfc * self.SWtrans.surf2space
        self.SW_absorbed_total = (self.SW_absorbed_sfc +
                                  np.sum(self.SW_absorbed_atm))
        self.planetary_albedo = self.SWup_TOA / self.SWdown_TOA

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
        self.rad_temp_tendency_sfc = (self.rad_heating_sfc * self.timestep /
                                      self.c_sfc)
        self.rad_temp_tendency_atm = (self.rad_heating_atm * self.timestep /
                                      self.c_atm)

    def step_forward(self):
        '''Update the Column temperature. If optional argument num_steps is given, 
        the timestepping will repeat the specifed number of times.
        
        Calls rad_temperature_tendency() to compute radiative tendencies,
        and if a lapse rate is specified in params['adj_lapse_rate'], also calls convective_adjustment().
        '''
        self.rad_temperature_tendency()
        self.Ts += self.rad_temp_tendency_sfc
        self.Tatm += self.rad_temp_tendency_atm
        if self.adj_lapse_rate is not None:
            self.unstable_Ts = self.Ts
            self.unstable_Tatm = self.Tatm
            # Tadj = self.convective_adjustment( lapserate = self.params['adj_lapse_rate'] )
            Tcol = np.concatenate(([self.Ts], self.Tatm))
            pnew = np.concatenate(([const.ps], self.p))
            cnew = np.concatenate(([self.c_sfc], self.c_atm *
                                  np.ones_like(self.p)))
            Tadj = convective_adjustment(pnew, Tcol, cnew,
                                         lapserate=self.adj_lapse_rate)
            self.Ts = Tadj[0]
            self.Tatm = Tadj[1:self.num_levels+1]
        super(Column, self).step_forward()
