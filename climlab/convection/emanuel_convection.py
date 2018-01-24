'''
A climlab process for the Emanuel convection scheme
'''
from __future__ import absolute_import

import numpy as np
from climlab.process import TimeDependentProcess
from climlab.utils.thermo import qsat
from ._emanuel_convection import emanuel_convection as convect


class EmanuelConvection(TimeDependentProcess):
    def __init__(self, **kwargs):
        super(EmanuelConvection, self).__init__(**kwargs)
        self.time_type = 'explicit'
        #  Define inputs and diagnostics
        self.add_diagnostic('CBMF', 0.)  # cloud base mass flux
        self.add_diagnostic('PRECIP', 0.) # Precip rate (mm/day)

    def _compute(self):
        #  Invert arrays so the first element is the bottom of column
        T = self._climlab_to_convect(self.Tatm)
        dom = self.Tatm.domain
        P = self._climlab_to_convect(dom.lev.points)
        PH = self._climlab_to_convect(dom.lev.bounds)
        Q = self._climlab_to_convect(self.q)
        QS = qsat(T,P)
        ND = T.size
        NL = ND-1
        try:
            U = self._climlab_to_convect(self.U)
        except:
            U = np.zeros(ND)
        try:
            V = self._climlab_to_convect(self.V)
        except:
            V = np.zeros(ND)
        NTRA = 1
        TRA = np.zeros((ND,NTRA), order='F')  # tracers ignored
        NTRA = np.size(TRA,axis=1)
        DELT = float(self.timestep)
        CBMF = float(self.CBMF)
        (IFLAG, FT, FQ, FU, FV, FTRA, PRECIP, WD, TPRIME, QPRIME, CBMFnew) = \
            convect(T, Q, QS, U, V, TRA, P, PH, ND, NL, NTRA, DELT, CBMF)

        tendencies = {'Tatm': self._convect_to_climlab(FT),
                      'q': self._convect_to_climlab(FQ)}
        if 'Ts' in self.state:
            tendencies['Ts'] = np.zeros_like(self.Ts)
        if 'U' in self.state:
            tendencies['U'] = self._convect_to_climlab(FU)
        if 'V' in self.state:
            tendencies['V'] = self._convect_to_climlab(FV)
        self.CBMF = CBMFnew
        self.PRECIP = PRECIP
        self.IFLAG = IFLAG
        return tendencies

    def _climlab_to_convect(self, field):
        #  Invert arrays so the first element is the bottom of column
        return np.flipud(field)

    def _convect_to_climlab(self, field):
        return np.flipud(field) * np.ones_like(self.Tatm)
