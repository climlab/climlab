'''
A climlab process for the Emanuel convection scheme
'''
from __future__ import absolute_import

import numpy as np
from climlab.process import TimeDependentProcess
from climlab.utils.thermo import qsat
from ._emanuel_convection import emanuel_convection as convect
# The array conversion routines we are borrowing from the RRTMG wrapper
from climlab.radiation.rrtm.utils import _climlab_to_rrtm as _climlab_to_convect
from climlab.radiation.rrtm.utils import _rrtm_to_climlab as _convect_to_climlab


class EmanuelConvection(TimeDependentProcess):
    def __init__(self, **kwargs):
        super(EmanuelConvection, self).__init__(**kwargs)
        self.time_type = 'explicit'
        #  Define inputs and diagnostics
        self.add_diagnostic('CBMF', np.zeros(self.Tatm[...,0].shape))  # cloud base mass flux
        self.add_diagnostic('PRECIP', np.zeros(self.Tatm[...,0].shape)) # Precip rate (mm/day)

    def _compute(self):
        #  Invert arrays so the first element is the bottom of column
        T = _climlab_to_convect(self.Tatm)
        dom = self.Tatm.domain
        P = _climlab_to_convect(dom.lev.points)
        PH = _climlab_to_convect(dom.lev.bounds)
        Q = _climlab_to_convect(self.q)
        QS = qsat(T,P)
        ND = np.size(T, axis=1)
        NCOL = np.size(T, axis=0)
        NL = ND-1
        try:
            U = _climlab_to_convect(self.U)
        except:
            U = np.zeros_like(T)
        try:
            V = _climlab_to_convect(self.V)
        except:
            V = np.zeros_like(T)
        NTRA = 1
        TRA = np.zeros((NCOL,ND,NTRA), order='F')  # tracers ignored
        DELT = float(self.timestep)
        CBMF = self.CBMF
        (IFLAG, FT, FQ, FU, FV, FTRA, PRECIP, WD, TPRIME, QPRIME, CBMFnew) = \
            convect(T, Q, QS, U, V, TRA, P, PH, NCOL, ND, NL, NTRA, DELT, CBMF)

        tendencies = {'Tatm': _convect_to_climlab(FT),
                      'q': _convect_to_climlab(FQ)}
        if 'Ts' in self.state:
            tendencies['Ts'] = 0. * self.Ts
        if 'U' in self.state:
            tendencies['U'] = _convect_to_climlab(FU) * np.ones_like(self.U)
        if 'V' in self.state:
            tendencies['V'] = _convect_to_climlab(FV) * np.ones_like(self.V)
        self.CBMF = CBMFnew
        self.PRECIP = PRECIP
        self.IFLAG = IFLAG
        return tendencies

    # def _climlab_to_convect(self, field):
    #     #  Invert arrays so the first element is the bottom of column
    #     #return np.flipud(field)
    #     newfield = _climlab_to_rrtm(field)
    #     return _climlab_to_rrtm(field)
    #
    # def _convect_to_climlab(self, field):
    #     #return np.flipud(field) * np.ones_like(self.Tatm)
    #     return _rrtm_to_climlab(field)
