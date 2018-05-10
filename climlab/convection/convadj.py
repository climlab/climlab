from __future__ import division
from builtins import range
import numpy as np
from climlab import constants as const
from climlab.utils.thermo import rho_moist, pseudoadiabat
from climlab.process.time_dependent_process import TimeDependentProcess
from climlab.domain.field import Field
from .akmaev_adjustment import convective_adjustment_direct


class ConvectiveAdjustment(TimeDependentProcess):
    '''Hard Convective Adjustment to a prescribed lapse rate.

    This process computes the instantaneous adjustment to conservatively
    remove any instabilities in each column.

    Instability is defined as a temperature decrease with height that exceeds
    the prescribed critical lapse rate. This critical rate is set by input argument
    ``adj_lapse_rate``, which can be either a numerical or string value.

    Numerical values for ``adj_lapse_rate`` are given in units of K / km. Both
    array and scalar values are valid. For scalar values, the assumption is that
    the critical lapse rate is the same at every level.

    If an array is given, it is assumed to represent the in-situ critical lapse
    rate (in K/km) at every grid point.

    Alternatively, string arguments can be given as follows:

        - ``'DALR'`` or ``'dry adiabat'``: critical lapse rate is set to g/cp = 9.8 K / km
        - ``'MALR'`` or ``'moist adiabat'`` or ``'pseudoadiabat'``: critical lapse rate follows the in-situ moist pseudoadiabat at every level

    Adjustment includes the surface if ``'Ts'`` is included in the ``state``
    dictionary. This implicitly accounts for turbulent surface fluxes.
    Otherwise only the atmospheric temperature is adjusted.

    If ``adj_lapse_rate`` is an array, its size must match the number of vertical
    levels of the adjustment. This is number of pressure levels if the surface is
    not adjusted, or number of pressure levels + 1 if the surface is adjusted.

    This process implements the conservative adjustment algorithm described in
    Akmaev (1991) Monthly Weather Review.
    '''
    def __init__(self, adj_lapse_rate=None, **kwargs):
        super(ConvectiveAdjustment, self).__init__(**kwargs)
        # lapse rate for convective adjustment, in K / km
        self.adj_lapse_rate = adj_lapse_rate
        self.param['adj_lapse_rate'] = adj_lapse_rate
        self.time_type = 'adjustment'
        self.adjustment = {}
    @property
    def pcol(self):
        patm = self.lev
        if 'Ts' in self.state:
            #  surface pressure should correspond to model domain!
            ps = self.lev_bounds[-1]
            return np.append(patm, ps)
        else:
            return patm
    @property
    def ccol(self):
        c_atm = self.Tatm.domain.heat_capacity
        if 'Ts' in self.state:
            c_sfc = self.Ts.domain.heat_capacity
            return np.append(c_atm, c_sfc)
        else:
            return c_atm
    @property
    def Tcol(self):
        #  For now, let's assume that the vertical axis is the last axis
        Tatm = self.Tatm
        if 'Ts' in self.state:
            Ts = np.atleast_1d(self.Ts)
            return np.concatenate((Tatm, Ts),axis=-1)
        else:
            return Tatm
    @property
    def adj_lapse_rate(self):
        lapserate = self._adj_lapse_rate
        if type(lapserate) is str:
            if lapserate in ['DALR', 'dry adiabat']:
                return const.g / const.cp * 1.E3
            elif lapserate in ['MALR', 'moist adiabat', 'pseudoadiabat']:
                # critical lapse rate at each level is set by pseudoadiabat
                dTdp = pseudoadiabat(self.Tcol,self.pcol) / 100.  # K / Pa
                #  Could include water vapor effect on density here ...
                #  Replace Tcol with virtual temperature
                rho = self.pcol*100./const.Rd/self.Tcol  # in kg/m**3
                return dTdp * const.g * rho * 1000.  # K / km
            else:
                raise ValueError('adj_lapse_rate must be either numeric or any of \'DALR\', \'dry adiabat\', \'MALR\', \'moist adiabat\', \'pseudoadiabat\'.')
        else:
            return lapserate
    @adj_lapse_rate.setter
    def adj_lapse_rate(self, lapserate):
        self._adj_lapse_rate = lapserate
        self.param['adj_lapse_rate'] = lapserate

    def _compute(self):
        if self.adj_lapse_rate is None:
            self.adjustment['Ts'] = self.Ts * 0.
            self.adjustment['Tatm'] = self.Tatm * 0.
        else:
            #  convective adjustment routine expect reversered vertical axis
            pflip = self.pcol[..., ::-1]
            Tflip = self.Tcol[..., ::-1]
            cflip = self.ccol[..., ::-1]
            lapseflip = np.atleast_1d(self.adj_lapse_rate)[..., ::-1]
            Tadj_flip = convective_adjustment_direct(pflip, Tflip, cflip, lapserate=lapseflip)
            Tadj = Tadj_flip[..., ::-1]
            if 'Ts' in self.state:
                Ts = Field(Tadj[...,-1], domain=self.Ts.domain)
                Tatm = Field(Tadj[...,:-1], domain=self.Tatm.domain)
                self.adjustment['Ts'] = Ts - self.Ts
            else:
                Tatm = Field(Tadj, domain=self.Tatm.domain)
            self.adjustment['Tatm'] = Tatm - self.Tatm
        #  return the adjustment, independent of timestep
        #  because the parent process might have set a different timestep!
        return self.adjustment
