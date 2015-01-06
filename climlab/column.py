"""column.py

Object-oriented code for one-dimensional radiative-convective models.

Code developed by Brian Rose, University at Albany
brose@albany.edu
"""


import numpy as np
import constants as const
from convadj import convective_adjustment
from model import _TimeSteppingModel

# Need to make this more consistent with ebm.py -- don't use a dictionary for parameters

class Column(_TimeSteppingModel):
    """The Column object represents surface temperature and air temperature on grid of pressure levels."""
    def __str__(self):
        return ( "Instance of Column class with surface temperature " + str(self.Ts) + " K, " + 
            str(self.params['num_levels']) + " atmospheric levels, and parameters: \n" + str(self.params) )
    
    def __init__(self, p=None, params=dict(nul=None)):
        #  first set all parameters to sensible default values
        params.setdefault( 'num_levels', 30 )
        params.setdefault( 'water_depth', 1.0 )
        params.setdefault( 'albedo', 0.299 )
        params.setdefault( 'Q', 341.3 )
        params.setdefault( 'timestep', 1. * const.seconds_per_day )
        params.setdefault( 'abs_coeff', 1.229E-4 )  # absorption coefficient in m**2 / kg
        params.setdefault( 'adj_lapse_rate', None )  # lapse rate for convective adjustment, in K / km
                
        self.params = params
        
        if p is None:
            self.dp = const.ps / self.params['num_levels']  #  pressure interval in hPa or mb
            self.p = np.linspace( const.ps - self.dp/2, self.dp/2, params['num_levels'] )  #  pressure at each level, in hPa or mb
        else:
            # if pressure levels are provided, assume that boundaries between layers occur halfway between the given levels
            self.p = np.flipud( np.sort( p ) )  # ensures that pressure decreases
            N = self.p.size
            self.pbounds = np.concatenate(([const.ps], (self.p[0:N-1] + self.p[1:N])/2, [0.]))
            self.dp = np.flipud( np.diff( np.flipud( self.pbounds ) ) )
            self.params['num_levels'] = N

        self.Ts = 288.  # initial surface temperature
        self.Tatm = np.linspace( self.Ts-10., 200., params['num_levels'] ) # intitial temperature
        self.c_atm = const.cp * self.dp * const.mb_to_Pa / const.g  #  heat capacity of atmospheric layers
        self.c_sfc = const.cw * const.rho_w * params['water_depth']   #  heat capacity of surface in J / m**2 / K
        self.set_LW_emissivity()
        self.set_SW_absorptivity()
        
        #self.steps = 0
        #self.days_elapsed = 0.
        self.set_timestep( num_steps_per_year = const.seconds_per_year / params['timestep'])
        #  A dictionary of the model state variables
        self.state = {'Ts':self.Ts, 'Tatm':self.Tatm}

        
    def set_LW_emissivity(self, eps = None ):
        """Set the longwave emissivity / absorptivity eps for the Column."""
        if eps is None:
            # default is to set eps equal at every level, and use the value consistent with the absorption coefficient parameter 
            self.eps = 2. / ( 1 + 2. * const.g / self.params['abs_coeff'] / (self.dp * const.mb_to_Pa) )
        elif ( np.isscalar(eps) or eps.size == self.params['num_levels'] ):
            self.eps = eps
        else:
            raise ValueError('eps must be scalar or have exactly ' + self.params['num_levels'] + ' elements.' )    
        
        if np.isscalar( self.eps ):
            self.LWtrans = Transmissivity( self.eps * np.ones_like( self.p ) )
        else:
            self.LWtrans = Transmissivity( self.eps )
    
    def set_SW_absorptivity(self, eps = None ):
        """Set the shortwave absorptivity eps for the Column."""
        if eps is None:
            # default is no shortwave absorption
            self.SWtrans = Transmissivity( np.zeros_like( self.p ) )
        elif np.isscalar(eps):
            # passing a single scalar sets eps equal to this number everywhere
            self.SWtrans = Transmissivity( self.eps * np.ones_like( self.p ) )
        elif np.size(eps) == self.params['num_levels']:
            self.SWtrans = Transmissivity( eps )
        else:
            raise ValueError('eps must be scalar or have exactly ' + self.params['num_levels'] + ' elements.' )        
                              
    def longwave_heating(self):
        """Compute the net longwave radiative heating at every level and the surface.
        Also store the upwelling longwave radiation at the top (OLR), and the downwelling 
        longwave radiation at the surface.
        """

        eps = self.LWtrans.absorb
        # emissions from surface and each layer
        self.emit_sfc = const.sigma * self.Ts**4.
        self.emit_atm = eps * const.sigma * self.Tatm**4.
        
        self.LW_down_sfc = np.dot( self.LWtrans.surf2atm, self.emit_atm )
        self.OLR_sfc = self.LWtrans.surf2space * self.emit_sfc
        self.OLR_atm = self.LWtrans.atm2space * self.emit_atm
        self.OLR = self.OLR_sfc + np.sum( self.OLR_atm )
        self.LW_absorbed_sfc = self.LW_down_sfc - self.emit_sfc
        
        incident_fromsfc = self.emit_sfc * self.LWtrans.surf2atm
        incident_fromatm = np.dot( self.LWtrans.atm2atm, self.emit_atm )
        self.LW_absorbed_atm = (incident_fromatm + incident_fromsfc) * eps - 2 * self.emit_atm
    
    def shortwave_heating(self):
        '''Net shortwave heating at each level.'''
         
        self.SWdown_TOA = self.params['Q']
        SW_incident_fromabove = self.SWdown_TOA * self.SWtrans.atm2space
        self.SWdown_sfc = self.SWdown_TOA * self.SWtrans.surf2space
        self.SWup_sfc = self.params['albedo'] * self.SWdown_sfc
        self.SW_absorbed_sfc = self.SWdown_sfc - self.SWup_sfc
        SW_incident_frombelow = self.SWup_sfc * self.SWtrans.surf2atm
        self.SW_absorbed_atm = ( SW_incident_fromabove + SW_incident_frombelow ) * self.SWtrans.absorb
        self.SWup_TOA = self.SWup_sfc * self.SWtrans.surf2space
        self.SW_absorbed_total =  self.SW_absorbed_sfc + np.sum( self.SW_absorbed_atm )
        self.planetary_albedo = self.SWup_TOA / self.SWdown_TOA

    def rad_temperature_tendency(self):
        """Compute the net radiative heating everywhere in the Column (in W/m^2), 
        and the resulting temperature change over a specified timestep (in K).
        """
        
        # compute longwave heating rates
        self.longwave_heating()
        self.shortwave_heating()
        # net radiative forcing
        self.rad_heating_sfc = self.SW_absorbed_sfc + self.LW_absorbed_sfc
        self.rad_heating_atm = self.SW_absorbed_atm + self.LW_absorbed_atm
        #  temperature tendencies due only to radiation
        self.rad_temp_tendency_sfc = self.rad_heating_sfc * self.params['timestep'] / self.c_sfc
        self.rad_temp_tendency_atm = self.rad_heating_atm * self.params['timestep'] / self.c_atm

    def step_forward(self):
        self.rad_temperature_tendency()
        self.Ts += self.rad_temp_tendency_sfc
        self.Tatm += self.rad_temp_tendency_atm
        if self.params['adj_lapse_rate'] is not None:
            self.unstable_Ts = self.Ts
            self.unstable_Tatm = self.Tatm
            # Tadj = self.convective_adjustment( lapserate = self.params['adj_lapse_rate'] )
            Tcol = np.concatenate(([self.Ts], self.Tatm))
            pnew = np.concatenate(([const.ps], self.p))
            cnew = np.concatenate(([self.c_sfc], self.c_atm *
                                  np.ones_like(self.p)))
            Tadj = convective_adjustment(pnew, Tcol, cnew,
                            lapserate=self.params['adj_lapse_rate'])
            self.Ts = Tadj[0]
            self.Tatm = Tadj[1:self.params['num_levels']+1]
        super(Column,self).step_forward()

    #def step_forward(self, num_steps=1 ):
    #    """Update the Column temperature. If optional argument num_steps is given, 
    #    the timestepping will repeat the specifed number of times.
    #    
    #    Calls rad_temperature_tendency() to compute radiative tendencies,
    #    and if a lapse rate is specified in params['adj_lapse_rate'], also calls convective_adjustment().
    #    """
    #    
    #    for n in range(num_steps):
    #        self.rad_temperature_tendency()
    #        self.Ts += self.rad_temp_tendency_sfc
    #        self.Tatm += self.rad_temp_tendency_atm
    #        if self.params['adj_lapse_rate'] is not None:
    #            self.unstable_Ts = self.Ts
    #            self.unstable_Tatm = self.Tatm
    #            Tadj = self.convective_adjustment( lapserate = self.params['adj_lapse_rate'] )
    #            self.Ts = Tadj[0]
    #            self.Tatm = Tadj[1:self.params['num_levels']+1]
    #        self.update_time()
            
        
class Transmissivity:
    def __init__(self, absorb ):
        if absorb.ndim is not 1:
            raise ValueError('absorb argument must be a vector')        
        self.absorb = absorb
        self.trans = 1 - self.absorb
        N = self.absorb.size
        # a matrix containing the transmission between atmospheric layers
        #  multiply this matrix by vector of emissions to the total incident beam
        #  at each layer. 
        self.atm2atm = np.diag( np.ones(N-1), 1 )
        for n in range(N):
            self.atm2atm[n,n+2:N] = np.cumprod( self.trans[n+1:N-1] )
        self.atm2atm += self.atm2atm.transpose()
                
        # the transmissivity between surface and layer k
        self.surf2atm = np.concatenate( ( [1.], np.cumprod( self.trans[:N-1] ) ) ) 
        # the transmissivity between layer k and space
        self.atm2space = np.flipud( np.cumprod( np.concatenate( ([1.], np.flipud(self.trans[1:N])) ) ) )
        #  the transmissivity between surface and space
        self.surf2space = np.prod( self.trans )