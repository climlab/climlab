"""ebm.py

Object-oriented code for diffusive energy balance models

Code developed by Brian Rose, University at Albany
brose@albany.edu
"""

import numpy as np
from scipy.linalg import solve_banded
from scipy import integrate
import climlab.utils.constants as const
from climlab.process.time_dependent_process import TimeDependentProcess
import climlab.solar.insolation as insolation
from climlab.solar.orbital import OrbitalTable

from climlab.domain.field import Field
from climlab.process.energy_budget import EnergyBudget
import climlab.utils.legendre as legendre
import climlab.domain.domain as domain
from climlab.radiation.AplusBT import AplusBT
from climlab.radiation.insolation import P2Insolation
from climlab.surface.albedo import StepFunctionAlbedo
from climlab.dynamics.diffusion import MeridionalDiffusion

def global_mean(field, lat_radians):
    '''Compute the area-weighted global mean.
    Field must be a vector of values on a latitude grid.'''
    return np.sum(field * np.cos(lat_radians)) / np.sum(np.cos(lat_radians))


class NewEBM(EnergyBudget):
    def __init__(self,
                 num_points=90,
                 A=210.,
                 B=2.,
                 D=0.555,  # in W / m^2 / degC, same as B
                 Tf=-10.0,
                 water_depth=10.0,
                 timestep=1. * const.seconds_per_day,
                 **kwargs):
        super(NewEBM, self).__init__(timestep=timestep, **kwargs)
        if not self.domains and not self.state:  # no state vars or domains yet
            sfc = domain.zonal_mean_surface(num_points=num_points,
                                            water_depth=water_depth)
            lat = sfc.axes['lat'].points
            initial = 12. - 40. * legendre.P2(np.sin(np.deg2rad(lat)))
            self.set_state('Ts', Field(initial, domain=sfc))
        self.param['A'] = A
        self.param['B'] = B
        self.param['D'] = D
        self.param['Tf'] = Tf
        self.param['water_depth'] = water_depth
        # create sub-models
        self.subprocess['LW'] = AplusBT(state=self.state, **self.param)
        self.subprocess['insolation'] = P2Insolation(domains=sfc, **self.param)
        self.subprocess['albedo'] = StepFunctionAlbedo(state=self.state,
                                                       **self.param)
        # diffusivity in units of 1/s
        K = self.param['D'] / self.domains['Ts'].heat_capacity
        self.subprocess['diffusion'] = MeridionalDiffusion(state=self.state,
                                                           K=K,
                                                           **self.param)

    def _compute_heating_rates(self):
        '''Compute energy flux convergences to get heating rates in W / m**2.
        This method should be over-ridden by daughter classes.'''
        insolation = self.subprocess['insolation'].diagnostics['insolation']
        albedo = self.subprocess['albedo'].diagnostics['albedo']
        ASR = (1-albedo) * insolation
        self.heating_rate['Ts'] = ASR
        self.diagnostics['ASR'] = ASR

# lots of work to do here.
#  need to figure out a better way to deal with params
# and **kwargs

class _EBM(TimeDependentProcess):
    def __init__(self, num_points=90, K=0.555, **kwargs):
        # first create the model domains
        doms = domain.zonal_mean_surface(num_points=num_points)
        # initial surface temperature
        lat = doms['sfc'].grid['lat'].points
        initial = {}
        initial['Ts'] = 12. - 40. * legendre.P2(np.sin(np.deg2rad(lat)))
         #  Create process data structures
        super(_EBM, self).__init__(domains=doms, state=initial, **kwargs)
        #  first set all parameters to sensible default values
        #self.num_points = num_points
        # self.K = 2.2E6  # in m^2 / s
        self.K = 0.555  # in W / m^2 / degC, same as B
        self.A = 210.
        self.B = 2.
        # self.water_depth =  10.0
        self.Tf = 0.0
        self.S0 = const.S0
        self.make_grid()
        # self.albedo_noice = 0.303 + 0.0779 * P2( np.sin( self.phi ) )
        self.albedo_noice = 0.33 + 0.25 * legendre.P2(np.sin(self.phi))
        # self.albedo_ice = 0.62 * np.ones_like( self.phi )
        self.albedo_ice = self.albedo_noice  # default to no albedo feedback
        self.T = 12. - 40. * legendre.P2(np.sin(self.phi))
        #  A dictionary of the model state variables
        self.state = {'T': self.T}
        self.positive_degree_days = np.zeros_like(self.phi)
        # self.make_insolation_array()  # now called from inside set_timestep()
        self.external_heat_source = np.zeros_like(self.phi)
        self.set_timestep()

    def make_grid(self):
        '''Build the grid for the computation, evenly spaced in latitude.'''
        #   dlat will be our grid spacing
        #   lat will be our temperature grid:
        #     an array with exactly num_points evenly spaced points
        #   lat_stag will be a staggered grid with numpoints+1 points,
        #     where the end points are the North and South poles
        #  Then we convert these all to radians for the computation.
        self.dlat = 180. / self.num_points
        self.lat = np.linspace(-90. + self.dlat/2,
                               90. - self.dlat/2, self.num_points)
        self.lat_stag = np.linspace(-90., 90., self.num_points+1)
        self.dphi = np.deg2rad(self.dlat)
        self.phi = np.deg2rad(self.lat)
        self.phi_stag = np.deg2rad(self.lat_stag)

    def set_timestep(self, num_steps_per_year=90):
        '''Change the timestep, given a number of steps per calendar year.'''
        super(_EBM, self).set_timestep(num_steps_per_year)
        self.set_water_depth()
        self.make_insolation_array()

    def set_water_depth(self, water_depth=10.):
        '''Method for changing the water depth (heat capacity) with depth in m.
        Also recomputes the tridiagonal diffusion matrix.'''
        if water_depth is None:
            try:
                water_depth = self.water_depth
            except:
                ValueError("water_depth parameter is not specified.")
        self.water_depth = water_depth
        self.C = const.cw * const.rho_w * self.water_depth
        self.delta_time_over_C = self.timestep / self.C
        self.set_diffusivity(self.K)

    def set_diffusivity(self, K=None):
        '''Method for changing the diffusivity, with K in W/m^2/degC.
        Recomputes the tridiagonal diffusion matrix.'''
        if K is None:
            try:
                K = self.K
            except:
                ValueError("Diffusivity parameter K is not specified.")
        self.K = K
        self.diffTriDiag = self._make_diffusion_matrix()

    def _make_diffusion_matrix(self):
        J = self.num_points
        # Ka = (const.cp * const.ps * const.mb_to_Pa / const.g / const.a**2 *
        #        self.K * np.ones_like(self.phi_stag))
        # cosKa = np.cos(self.phi_stag) * Ka
        cosKa = np.cos(self.phi_stag) * self.K
        Ka1 = (cosKa[0:J] / np.cos(self.phi) *
               self.delta_time_over_C / self.dphi**2)
        Ka3 = (cosKa[1:J+1] / np.cos(self.phi) *
               self.delta_time_over_C / self.dphi**2)
        Ka2 = np.insert(Ka1[1:J], 0, 0) + np.append(Ka3[0:J-1], 0)
        #  Atmosphere tridiagonal matrix
        diag = np.empty((3, J))
        diag[0, 1:] = -Ka3[0:J-1]
        diag[1, :] = 1 + Ka2
        diag[2, 0:J-1] = -Ka1[1:J]
        return diag

    def compute_OLR(self):
        return self.A + self.B * self.T

    def make_insolation_array(self):
        # will be overridden by daughter classes
        raise NotImplementedError("Subclasses of _EBM must implement a method for computing insolation.")

    def compute_insolation(self):
        return self.insolation_array[:, self.day_of_year_index]

    def compute_albedo(self):
        '''Simple step-function albedo based on ice line at temperature Tf.'''
        return np.where(self.T >= self.Tf, self.albedo_noice, self.albedo_ice)

    def compute_radiation(self):
        self.ASR = (1 - self.compute_albedo()) * self.compute_insolation()
        self.OLR = self.compute_OLR()
        self.net_radiation = self.ASR - self.OLR

    def step_forward(self):
        self.compute_radiation()
        #  updated temperature due to radiation:
        Trad = (self.T + (self.net_radiation + self.external_heat_source) *
                self.delta_time_over_C)
        # Time-stepping the diffusion is just inverting this matrix problem:
        # self.T = np.linalg.solve( self.diffTriDiag, Trad )
        self.T = solve_banded((1, 1), self.diffTriDiag, Trad)
        self.positive_degree_days += self.compute_degree_days()
        super(_EBM, self).step_forward()

    def compute_degree_days(self, threshold=0.):
        """Return temperature*time in degree-days,
        wherever temperature is above the threshold, otherwise zero."""
        return np.where(self.T > threshold, self.T * self.timestep /
                        const.seconds_per_day, np.zeros_like(self.T))

    def do_new_calendar_year(self):
        """This function is called once at the end of every calendar year."""
        super(_EBM, self).do_new_calendar_year()
        self.previous_positive_degree_days = self.positive_degree_days
        self.positive_degree_days = np.zeros_like(self.phi)

    def heat_transport(self):
        '''Returns instantaneous heat transport in units on PW,
        on the staggered grid.'''
        return self.diffusive_heat_transport()

    def diffusive_heat_transport( self ):
        '''Compute instantaneous diffusive heat transport in units of PW, on the staggered grid.'''
        #return ( 1E-15 * -2 * np.math.pi * np.cos(self.phi_stag) * const.cp * const.ps * const.mb_to_Pa / const.g * self.K * 
        #    np.append( np.append( 0., np.diff( self.T ) ), 0.) / self.dphi )
        return ( 1E-15 * -2 * np.math.pi * np.cos(self.phi_stag) * const.a**2 * self.K * 
            np.append( np.append( 0., np.diff( self.T ) ), 0.) / self.dphi )
    
    def heat_transport_convergence( self ):
        '''Returns instantaneous convergence of heat transport in units of W / m^2.'''
        return ( -1./(2*np.math.pi*const.a**2*np.cos(self.phi)) * np.diff( 1.E15*self.heat_transport() )
            / np.diff(self.phi_stag) )
    
    def inferred_heat_transport( self ):
        '''Returns the inferred heat transport (in PW) by integrating the TOA energy imbalance from pole to pole.'''
        return ( 1E-15 * 2 * np.math.pi * const.a**2 * integrate.cumtrapz( np.cos(self.phi)*self.net_radiation,
            x=self.phi, initial=0. ) )
            
    def find_icelines( self ):
        '''Returns the instantaneous latitudes of any ice edges.'''
        # This probably won't work in cases with multiple ice lines per hemisphere!
        #  Revise!
        iceindices = np.squeeze( np.where( self.T < self.Tf ) )
        if iceindices.size == 0:
            return 90.
        elif iceindices.size == self.lat.size:
            return 0.
        else:
            icelines = np.squeeze( np.where( np.diff(iceindices)>1) )
            icelat1 = self.lat_stag[ iceindices[icelines]+1 ]
            icelat2 = self.lat_stag[ iceindices[icelines+1] ]
            return icelat1, icelat2

    def global_mean( self, field ):
        '''Compute the area-weighted global mean of a vector field on the latitude grid.'''
        #return np.sum( field * np.cos( self.phi ) ) / np.sum( np.cos( self.phi ) )     
        return global_mean( field, self.phi ) 
    
    def global_mean_temperature( self ):
        '''Convenience method to compute global mean temperature.'''
        return self.global_mean( self.T )


class EBM_simple( _EBM ):
    '''The simplest diffusive energy balance model, uses constant prescribed insolation (2nd Legendre polynomial).'''
    def __str__(self):
        return ( "Instance of EBM_simple class with " +  str(self.num_points) + " latitude points  \n" +
          "and global mean temperature " + str(self.global_mean_temperature()) + " degrees C.")
    
    def __init__( self, num_points = 90 ):
        self.s2 = -0.48    
        super(EBM_simple,self).__init__( num_points )
        self.Tf = -10.
        
    def make_insolation_array( self ):
        self.constant_insolation = self.S0 / 4 * (1 + self.s2 * 
                legendre.P2( np.sin( self.phi ) ) )
        self.insolation_array = np.tile( np.expand_dims(self.constant_insolation,axis=1), [1, self.num_steps_per_year] )


class EBM_annual( EBM_simple ):
    '''EBM with steady annual-mean insolation computed using real orbital parameters (depends on obliquity).'''
    def __str__(self):
        return ( "Instance of EBM_annual class with " +  str(self.num_points) + " latitude points  \n" +
          "and global mean temperature " + str(self.global_mean_temperature()) + " degrees C.")
    
    def make_insolation_array( self, orb = const.orb_present ):
        self.orb = orb
        temp_array = insolation.daily_insolation(self.lat, self.days_of_year, orb=self.orb, S0=self.S0)
        self.constant_insolation = np.mean( temp_array, axis=1 )
        self.insolation_array = np.tile( np.expand_dims(self.constant_insolation,axis=1), [1, self.num_steps_per_year] )
    
    
class EBM_seasonal( _EBM ):
    '''EBM with realistic time-varying seasonal insolation computed using real orbital parameters.'''
    def __str__(self):
        return ( "Instance of EBM_seasonal class with " +  str(self.num_points) + " latitude points  \n" +
          "and global mean temperature " + str(self.global_mean_temperature()) + " degrees C.")
          
    def __init__( self, num_points = 90 ):
        super(EBM_seasonal,self).__init__( num_points )
        self.Tf = 0.
                
    def make_insolation_array( self, orb = const.orb_present ):
        self.orb = orb
        self.insolation_array = insolation.daily_insolation(self.lat, self.days_of_year, 
            orb=self.orb, S0=self.S0)
            
    
class EBM_landocean( EBM_seasonal ):
    '''A model with both land and ocean, based on North and Coakley (1979)
    Essentially just invokes two different EBM_seasonal objects, one for ocean, one for land.
    '''
    def __str__(self):
        return ( "Instance of EBM_landocean class with " +  str(self.num_points) + " latitude points." )
    
    def __init__( self, num_points = 90 ):
        super(EBM_landocean,self).__init__( num_points )
        self.land_ocean_exchange_parameter = 1.0  # in W/m2/K

        self.land = EBM_seasonal( num_points )
        self.land.make_insolation_array( self.orb )
        self.land.Tf = 0.
        self.land.set_timestep( timestep = self.timestep )
        self.land.set_water_depth( water_depth = 2. )
        
        self.ocean = EBM_seasonal( num_points )
        self.ocean.make_insolation_array( self.orb )
        self.ocean.Tf = -2.
        self.ocean.set_timestep( timestep = self.timestep )
        self.ocean.set_water_depth( water_depth = 75. )

        self.land_fraction = 0.3 * np.ones_like( self.land.phi )        
        self.C_ratio = self.land.water_depth / self.ocean.water_depth
        self.T = self.zonal_mean_temperature()
        
    def zonal_mean_temperature( self ):
        return self.land.T * self.land_fraction + self.ocean.T * (1-self.land_fraction)

    def step_forward( self ):
        #  note.. this simple implementation is possibly problematic
        # because the exchange should really occur simultaneously with radiation
        # and before the implicit heat diffusion
        self.exchange = (self.ocean.T - self.land.T) * self.land_ocean_exchange_parameter
        self.land.step_forward()
        self.ocean.step_forward()
        self.land.T += self.exchange / self.land_fraction * self.land.delta_time_over_C
        self.ocean.T -= self.exchange / (1-self.land_fraction) * self.ocean.delta_time_over_C
        self.T = self.zonal_mean_temperature()
        self.update_time()
    
    #   This code should be more accurate, but it's ungainly and seems to produce just about the same result.
    #def step_forward( self ):
    #    self.exchange = (self.ocean.T - self.land.T) * self.land_ocean_exchange_parameter
    #    self.land.compute_radiation( )
    #    self.ocean.compute_radiation( )
    #    Trad_land = ( self.land.T + ( self.land.net_radiation + self.exchange / self.land_fraction )
    #        * self.land.delta_time_over_C )
    #    Trad_ocean = ( self.ocean.T + ( self.ocean.net_radiation - self.exchange / (1-self.land_fraction) )
    #        * self.ocean.delta_time_over_C )
    #    self.land.T = solve_banded((1,1), self.land.diffTriDiag, Trad_land )
    #    self.ocean.T = solve_banded((1,1), self.ocean.diffTriDiag, Trad_ocean )
    #    self.T = self.zonal_mean_temperature()
    #    self.land.update_time()
    #    self.ocean.update_time()
    #    self.update_time()

    def integrate_years(self, years=1.0, verbose=True ):
        #  Here we make sure that both sub-models have the current insolation.
        self.land.make_insolation_array( self.orb )
        self.ocean.make_insolation_array( self.orb )
        super(EBM_landocean,self).integrate_years( years, verbose )


class OrbitalCycles:
    def __init__( self, model, kyear_start = 20., kyear_stop = 0., 
                  segment_length_years = 100., orbital_year_factor = 1., verbose=True ):
        """Automatically integrate an Energy Balance Model through changes in orbital parameters.
        
        model is an object of class EBM or its daughters.
        
        segment_length_years is the length of each integration with fixed orbital parameters.
        orbital_year_factor is an optional speed-up to the orbital cycles.
        """
        
        self.model = model
        self.kyear_start = kyear_start
        self.kyear_stop = kyear_stop
        self.segment_length_years = segment_length_years
        self.orbital_year_factor = orbital_year_factor
        self.verbose = verbose
        self.num_segments = int( (kyear_start - kyear_stop) * 1000. / segment_length_years / orbital_year_factor )
        
        kyear_before_present = kyear_start
        
        if verbose:
            print( "---------  OrbitalCycles  START ----------" )
            print( "Beginning integration for the model from " + str(kyear_start) + " to " + 
                str(kyear_stop) + " kyears before present." )
            print( "Integration time for each set of orbital parameters is " + 
                str(segment_length_years) + " years." )
            print( "Orbital cycles will be sped up by a factor " + str(orbital_year_factor) )
            print( "Total number of segments is " + str(self.num_segments) )
        
        # initialize storage arrays
        self.T_segments_global = np.empty( self.num_segments )
        self.T_segments = np.empty( (self.model.num_points, self.num_segments) )
        self.T_segments_annual = np.empty_like( self.T_segments )
        self.orb_kyear = np.empty( self.num_segments )
        
        # Get orbital data table
        orbtable = OrbitalTable()
        
        for n in range(self.num_segments):
            if verbose:
                print("-------------------------")
                print("Segment " + str(n) + " out of " + str(self.num_segments) )
                print( "Using orbital parameters from " + str(kyear_before_present) + " kyears before present." )
            orb = orbtable.lookup_parameters( kyear_before_present )
            self.model.make_insolation_array( orb )
            self.model.integrate_years( segment_length_years-1., verbose=False )
            #  Run one final year to characterize the current equilibrated state
            self.model.integrate_years( 1.0, verbose=False )
            self.T_segments_annual[:,n] = self.model.T_timeave
            self.T_segments[:,n] = self.model.T
            self.T_segments_global[n] = self.model.global_mean( self.model.T_timeave )
            self.orb_kyear[n] = kyear_before_present
            kyear_before_present -= segment_length_years / 1000. * orbital_year_factor
            if verbose:
                print( "Global mean temperature from the final year of integration is " + 
                    str(self.T_segments_global[n]) + " degrees C." )
        if verbose:
            print( "---------  OrbitalCycles  END ----------" )


#  To do:
#    -  use integrated positive degree days to calculate implicit ice sheet melt potential
#    - also use these to set up a version of the model with vegetation-albedo feedback
#   - Create option to have specified extra ocean heat transport in the ocean component
#   - Create a default land-fraction that looks more like reality for the land-ocean model
#    - add diffusion of moist static energy
#  (would require re-computing the diffusion operator at each timestep, probably somewhat slower)


class EBM_annual_moist( EBM_annual ):
    def __str__(self):
        return ( "Instance of EBM_annual_moist class with " +  str(self.num_points) + " latitude points  \n" +
          "and global mean temperature " + str(self.global_mean_temperature()) + " degrees C.")
    
    def __init__( self, num_points = 90 ):
        _EBM.__init__( self, num_points )
        self.K0 = self.K  # constant
        self.Kperdegree = self.K0/20. # 5% increase per degree
        self.Tref = 15.
        self.set_diffusivity( K = self.compute_K() )
        
    def compute_K(self):
        # formula to compute diffusivity, linear in global mean temperature
        return self.K0 + self.Kperdegree * (self.global_mean_temperature()-self.Tref)
        
    def step_forward( self ):
        # set the diffusivity, depends on global mean temperature
        self.set_diffusivity( K = self.compute_K() )
        _EBM.step_forward(self)