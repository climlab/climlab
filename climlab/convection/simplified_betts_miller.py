'''
A climlab process for the Frierson Simplified Betts Miller convection scheme

        :Example:

            Here is an example of setting up a complete single-column
            Radiative-Convective model with interactive water vapor.
            The model includes the following processes:
            
            - Constant insolation
            - Longwave and Shortwave radiation
            - Surface turbulent fluxes of sensible and latent heat
            - Moist convection using the Simplified Betts Miller scheme

            The state variables for this model will be surface temperature, 
            air temperature, and specific humidity. 
            This model has a simple but self-contained hydrological cycle: 
            water is evaporated from the surface and transported aloft by 
            the moist convection scheme.

            The vertical distribution of temperature and humidity at 
            equilibrium will be determined by the interactions between 
            moist convection, radiation, and surface fluxes::

                import numpy as np
                import climlab
                from climlab.utils import constants as const

                num_lev = 30
                water_depth = 10.
                short_timestep = const.seconds_per_hour * 3
                long_timestep = short_timestep*3
                insolation = 342.
                albedo = 0.18

                # set initial conditions -- 24C at the surface, -60C at 200 hPa, isothermal stratosphere
                strat_idx = 6
                Tinitial = np.zeros(num_lev)
                Tinitial[:strat_idx] = -60. + const.tempCtoK
                Tinitial[strat_idx:] = np.linspace(-60, 22, num_lev-strat_idx) + const.tempCtoK
                Tsinitial = 24. + const.tempCtoK

                full_state = climlab.column_state(water_depth=water_depth, num_lev=num_lev)
                full_state['Tatm'][:] = Tinitial
                full_state['Ts'][:] = Tsinitial

                # Initialize the model with a nearly dry atmosphere
                qStrat = 5.E-6   #  a very small background specific humidity value
                full_state['q'] = 0.*full_state.Tatm + qStrat

                temperature_state = {'Tatm':full_state.Tatm,'Ts':full_state.Ts}
                #  Surface model
                shf = climlab.surface.SensibleHeatFlux(name='Sensible Heat Flux',
                                    state=temperature_state, Cd=3E-3,
                                    timestep=short_timestep)
                lhf = climlab.surface.LatentHeatFlux(name='Latent Heat Flux',
                                    state=full_state, Cd=3E-3,
                                    timestep=short_timestep)
                surface = climlab.couple([shf,lhf], name="Slab")
                #  Convection scheme -- water vapor is a state variable
                conv = climlab.convection.SimplifiedBettsMiller(name='Convection',
                                            state=full_state,
                                            timestep=short_timestep,
                                        )  
                rad = climlab.radiation.RRTMG(name='Radiation',
                                state=temperature_state,
                                specific_humidity=full_state.q,  # water vapor is an input here, not a state variable
                                albedo=albedo,
                                insolation=insolation,
                                timestep=long_timestep,
                                icld=0, # no clouds
                                )
                atm = climlab.couple([rad, conv], name='Atmosphere')
                moistmodel = climlab.couple([atm,surface], name='Moist column model')

                print(moistmodel)
            
            Try running this model and verifying that the atmosphere moistens
            itself via convection, e.g::

                moistmodel.integrate_years(1)
                moistmodel.q

            which should produce something like::

                Field([5.00000000e-06, 5.00000000e-06, 5.00000000e-06, 5.00000000e-06,
                    5.00000000e-06, 5.00000000e-06, 8.55725020e-05, 2.02525334e-04,
                    4.03568410e-04, 6.98905819e-04, 1.08494727e-03, 1.54761989e-03,
                    2.06592591e-03, 2.62545894e-03, 3.22046387e-03, 3.84210271e-03,
                    4.48057560e-03, 5.12535633e-03, 5.76585382e-03, 6.39443880e-03,
                    7.00456365e-03, 7.47003956e-03, 8.02017591e-03, 8.57294739e-03,
                    9.10816435e-03, 9.63014344e-03, 1.01386863e-02, 1.06365703e-02,
                    1.11337461e-02, 1.51187832e-02])
            
            showing that humidity is now penetrating up to tropopause.
'''
import numpy as np
import warnings
from climlab.process import TimeDependentProcess
from climlab.utils.thermo import qsat
from climlab import constants as const
from climlab.domain.field import Field
from climlab.domain import zonal_mean_column
# The array conversion routines
#from climlab.radiation.rrtm.utils import _climlab_to_rrtm as _climlab_to_convect
#from climlab.radiation.rrtm.utils import _rrtm_to_climlab as _convect_to_climlab
try:
    from climlab_sbm_convection import betts_miller
except:
    warnings.warn('Cannot import SimplifiedBettsMiller fortran extension, this module will not be functional.')

HLv = const.Lhvap
Cp_air = const.cp
Grav = const.g
rdgas = const.Rd
rvgas = const.Rv
kappa = const.kappa
es0 = 1.0


class SimplifiedBettsMiller(TimeDependentProcess):
    '''
    The climlab wrapper for Dargan Frierson's Simplified Betts Miller moist 
    convection scheme (Frierson 2007, J. Atmos. Sci. 64, doi:10.1175/JAS3935.1)

    Basic characteristics:

    State:

    - Tatm (air temperature in K)
    - q (specific humidity in kg/kg)

    Input arguments and default values:

    - ``tau_bm = 7200.``: Betts-Miller relaxation timescale (seconds)
    - ``rhbm = 0.8``: relative humidity profile to which the scheme is relaxing
    - ``do_simp = False``: do the simple method where you adjust timescales to make precip continuous always.    
    - ``do_shallower = True``: do the shallow convection scheme where it chooses a smaller depth such that precipitation is zero.
    - ``do_changeqref = True``: do the shallow convection scheme where it changes the profile of both q and T in order make precip zero.
    - ``do_envsat = True``: reference profile is rhbm times saturated wrt environment (if false, it's rhbm times parcel).
    - ``do_taucape = False``: scheme where taubm is proportional to CAPE**-1/2
    - ``capetaubm = 900.``: for the above scheme, the value of CAPE (J/kg) for which tau = tau_bm. Ignored unless ``do_taucape == True``.
    - ``tau_min = 2400.``: for the above scheme, the minimum relaxation time allowed (seconds). Ignored unless ``do_taucape == True``.

    See Frierson (2007) for more details.
    '''
    def __init__(self,
                tau_bm=7200.,
                rhbm=0.8,
                do_simp=False,
                do_shallower=True,
                do_changeqref=True,
                do_envsat=True,
                do_taucape=False,
                capetaubm=900.,  # only used if do_taucape == True
                tau_min=2400.,   # only used if do_taucape == True
                **kwargs):
        super(SimplifiedBettsMiller, self).__init__(**kwargs)
        self.time_type = 'explicit'
        #  Define inputs and diagnostics
        surface_shape = self.state['Tatm'][...,0].shape
        #  Hack to handle single column and multicolumn
        if surface_shape == ():
            init = np.atleast_1d(np.zeros(surface_shape))
            self.multidim=False
        else:
            init = np.zeros(surface_shape)[...,np.newaxis]
            self.multidim=True
        init = Field(init, domain=self.state.Ts.domain)
        self.add_diagnostic('precipitation', init*0.) # Precip rate (kg/m2/s)
        self.add_diagnostic('cape', init*0.)
        self.add_diagnostic('cin', init*0.)
        self.add_input('tau_bm', tau_bm)
        self.add_input('rhbm', rhbm)
        self.add_input('capetaubm', capetaubm)
        self.add_input('tau_min', tau_min)
        self.add_input('do_simp', do_simp)
        self.add_input('do_shallower', do_shallower)
        self.add_input('do_changeqref', do_changeqref)
        self.add_input('do_envsat', do_envsat)
        self.add_input('do_taucape', do_taucape)
        if hasattr(rhbm, 'shape'):
            assert np.all(rhbm.shape == self.Tatm.shape), f'rhbm {rhbm.shape} has to have same shape as Tatm {self.Tatm.shape}'
            self.rhbm = rhbm
        else:
            self.rhbm = rhbm * np.ones_like(self.Tatm)

        self._KX = self.lev.size
        try:
            self._JX = self.lat.size
        except:
            self._JX = 1
        try:
            self._IX = self.lon.size
        except:
            self._IX = 1

    def _climlab_to_sbm(self, field):
        '''Prepare field with proper dimension order.
        Betts-Miller code expects 3D arrays with (IX, JX, KX)
        and 2D arrays with (IX, JX).
        climlab grid dimensions are any of:
            - (KX,)
            - (JX, KX)
            - (JX, IX, KX)
        '''
        if np.isscalar(field):
            return field
        else:
            num_dims = len(field.shape)
            if num_dims==1:  #  (num_lev only)
                return np.tile(field, [self._IX, self._JX, 1])
            elif num_dims==2:  # (num_lat, num_lev)
                return np.tile(field, [self._IX, 1, 1])
            else:  # assume we have (num_lon, num_lat, num_lev)
                return field
    
    def _sbm_to_climlab(self, field):
        ''' Output is either (IX, JX, KX) or (IX, JX).
        Transform this to...
            - (KX,) or (1,)  if IX==1 and JX==1
            - (IX,KX) or (IX, 1)   if IX>1 and JX==1
            - no change if IX>1, JX>1
        '''
        return np.squeeze(field)

    def _compute(self):
        #  Convection code expects that first element on pressure axis is TOA
        #  which is the same as climlab convention.
        #  All we have to do is ensure the input fields are (num_lat, num_lon, num_lev)
        T = self._climlab_to_sbm(self.state['Tatm'])
        RHBM = self._climlab_to_sbm(self.rhbm)
        dom = self.state['Tatm'].domain
        P = self._climlab_to_sbm(dom.lev.points) * 100. # convert to Pascals
        PH = self._climlab_to_sbm(dom.lev.bounds) * 100.
        Q = self._climlab_to_sbm(self.state['q'])
        dt = self.timestep

        (rain, tdel, qdel, q_ref, bmflag, klzbs, cape, cin, t_ref, \
        invtau_bm_t, invtau_bm_q, capeflag) = \
            betts_miller(dt, T, Q, RHBM, P, PH,
                         HLv,Cp_air,Grav,rdgas,rvgas,kappa, es0,
                         self.tau_bm, self.do_simp, self.do_shallower,
                         self.do_changeqref, self.do_envsat, self.do_taucape,
                         self.capetaubm, self.tau_min,self._IX, self._JX, self._KX, )

        # Routine returns adjustments rather than tendencies
        dTdt = tdel / dt
        dQdt = qdel / dt
        tendencies = {'Tatm': self._sbm_to_climlab(dTdt)*np.ones_like(self.state['Tatm']),
                      'q': self._sbm_to_climlab(dQdt)*np.ones_like(self.state['q'])}
        if 'Ts' in self.state:
            tendencies['Ts'] = 0. * self.state['Ts']
        #  Need to convert from kg/m2 (mm) to kg/m2/s (mm/s)
        #  Hack to handle single column and multicolumn
        if self.multidim:
            self.precipitation[:,0] = self._sbm_to_climlab(rain)/dt
            self.cape[:,0] = self._sbm_to_climlab(cape)
            self.cin[:,0] = self._sbm_to_climlab(cin)
        else:
            self.precipitation[:] = self._sbm_to_climlab(rain)/dt
            self.cape[:] = self._sbm_to_climlab(cape)
            self.cin[:] = self._sbm_to_climlab(cin)
        return tendencies