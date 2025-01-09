'''
A climlab process for the Frierson Simplified Betts Miller convection scheme
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
    to do
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