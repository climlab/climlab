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
        T = np.array(np.flipud(self.Tatm))
        dom = self.Tatm.domain
        P = np.flipud(dom.lev.points)
        PH = np.flipud(dom.lev.bounds)
        Q = np.array(np.flipud(self.q))
        QS = qsat(T,P)
        ND = T.size
        NL = ND-1
        U = np.zeros(ND)  # velocities ignored for now
        V = np.zeros(ND)
        NTRA = 1
        TRA = np.zeros((ND,NTRA), order='F')  # tracers ignored
        NTRA = np.size(TRA,axis=1)
        DELT = float(self.timestep)
        CBMF = float(self.CBMF)
        (IFLAG, FT, FQ, FU, FV, FTRA, PRECIP, WD, TPRIME, QPRIME, CBMFnew) = \
            convect(T, Q, QS, U, V, TRA, P, PH, ND, NL, NTRA, DELT, CBMF)

        tendencies = {'Tatm': np.flipud(FT) * np.ones_like(self.Tatm),
                      'q': np.flipud(FQ) * np.ones_like(self.q)}
        self.CBMF = CBMFnew
        self.PRECIP = PRECIP
        return tendencies
