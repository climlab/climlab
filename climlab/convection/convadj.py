from __future__ import division
from builtins import range
import numpy as np
from climlab import constants as const
from climlab.process.time_dependent_process import TimeDependentProcess
from climlab.domain.field import Field
from .akmaev_adjustment import convective_adjustment_direct


class ConvectiveAdjustment(TimeDependentProcess):
    '''Convective adjustment process
    
    Instantly returns column to neutral lapse rate

    Adjustment includes the surface IF 'Ts' is included in the state
    dictionary. Otherwise only the atmopsheric temperature is adjusted.

    Implements the conservative adjustment algorithm from Akmaev (1991) Monthly Weather Review.
    '''
    def __init__(self, adj_lapse_rate=None, **kwargs):
        super(ConvectiveAdjustment, self).__init__(**kwargs)
        # lapse rate for convective adjustment, in K / km
        self.adj_lapse_rate = adj_lapse_rate
        self.param['adj_lapse_rate'] = adj_lapse_rate
        self.time_type = 'adjustment'
        self.adjustment = {}
        patm = self.lev
        c_atm = self.Tatm.domain.heat_capacity
        if 'Ts' in self.state:
            c_sfc = self.Ts.domain.heat_capacity
            #self.pnew = np.append(patm, const.ps)
            #  surface pressure should correspond to model domain!
            ps = self.lev_bounds[-1]
            self.pnew = np.append(patm, ps)
            self.cnew = np.append(c_atm, c_sfc)
        else:
            self.pnew = patm
            self.cnew = c_atm
    @property
    def adj_lapse_rate(self):
        return self._adj_lapse_rate
    @adj_lapse_rate.setter
    def adj_lapse_rate(self, lapserate):
        if lapserate is 'DALR':
            self._adj_lapse_rate = const.g / const.cp * 1.E3
        else:
            self._adj_lapse_rate = lapserate
        self.param['adj_lapse_rate'] = self._adj_lapse_rate

    def _compute(self):
        #lapse_rate = self.param['adj_lapse_rate']
        if self.adj_lapse_rate is None:
            #self.adjustment = self.state * 0.
            self.adjustment['Ts'] = self.Ts * 0.
            self.adjustment['Tatm'] = self.Tatm * 0.
        else:
            #  For now, let's assume that the vertical axis is the last axis
            unstable_Tatm = self.Tatm
            if 'Ts' in self.state:
                unstable_Ts = np.atleast_1d(self.Ts)
                #Tcol = np.concatenate((unstable_Ts, unstable_Tatm),axis=-1)
                Tcol = np.concatenate((unstable_Tatm, unstable_Ts),axis=-1)
            else:
                Tcol = unstable_Tatm
            #  convective adjustment routine expect reversered vertical axis
            pflip = self.pnew[..., ::-1]
            Tflip = Tcol[..., ::-1]
            cflip = self.cnew[..., ::-1]
            Tadj_flip = convective_adjustment_direct(pflip, Tflip, cflip, lapserate=self.adj_lapse_rate)
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
