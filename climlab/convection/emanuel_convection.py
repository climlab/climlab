'''
A climlab process for the Emanuel convection scheme
'''
from __future__ import absolute_import

import numpy as np
from climlab.process import TimeDependentProcess
from climlab.utils.thermo import qsat
from climlab import constants as const
from ._emanuel_convection import emanuel_convection as convect
# The array conversion routines we are borrowing from the RRTMG wrapper
from climlab.radiation.rrtm.utils import _climlab_to_rrtm as _climlab_to_convect
from climlab.radiation.rrtm.utils import _rrtm_to_climlab as _convect_to_climlab


# Thermodynamic constants
CPD = const.cp
CPV = const.cpv
RV = const.Rv
RD = const.Rd
LV0 = const.Lhvap
G = const.g
ROWL = const.rho_w
# specific heat of liquid water -- artifically small!
#   Kerry Emanuel's notes say this is intentional, do not change this.
CL=2500.0
#CPV = CPD  #  try neglecting effect of water vapor on heat capacity

class EmanuelConvection(TimeDependentProcess):
    def __init__(self,
            MINORIG = 0,  # index of lowest level from which convection may originate (zero means lowest)
            #  Default parameter values taken from convect43c.f  fortran source
            ELCRIT=.0011,
            TLCRIT=-55.0,
            ENTP=1.5,
            SIGD=0.05,
            SIGS=0.12,
            OMTRAIN=50.0,
            OMTSNOW=5.5,
            COEFFR=1.0,
            COEFFS=0.8,
            CU=0.7,
            BETA=10.0,
            DTMAX=0.9,
            ALPHA=0.2,
            DAMP=0.1,
            **kwargs):
        super(EmanuelConvection, self).__init__(**kwargs)
        self.time_type = 'explicit'
        #  Define inputs and diagnostics
        self.add_diagnostic('CBMF', np.zeros(self.Tatm[...,0].shape))  # cloud base mass flux
        self.add_diagnostic('PRECIP', np.zeros(self.Tatm[...,0].shape)) # Precip rate (mm/day)
        self.add_input('MINORIG', MINORIG)
        self.add_input('ELCRIT', ELCRIT)
        self.add_input('TLCRIT', TLCRIT)
        self.add_input('ENTP', ENTP)
        self.add_input('SIGD', SIGD)
        self.add_input('SIGS', SIGS)
        self.add_input('OMTRAIN', OMTRAIN)
        self.add_input('OMTSNOW', OMTSNOW)
        self.add_input('COEFFR', COEFFR)
        self.add_input('COEFFS', COEFFS)
        self.add_input('CU', CU)
        self.add_input('BETA', BETA)
        self.add_input('DTMAX', DTMAX)
        self.add_input('ALPHA', ALPHA)
        self.add_input('DAMP', DAMP)

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
            convect(T, Q, QS, U, V, TRA, P, PH, NCOL, ND, NL, NTRA, DELT, CBMF,
                    CPD, CPV, CL, RV, RD, LV0, G, ROWL, self.MINORIG,
                    self.ELCRIT, self.TLCRIT, self.ENTP, self.SIGD, self.SIGS,
                    self.OMTRAIN, self.OMTSNOW, self.COEFFR, self.COEFFS,
                    self.CU, self.BETA, self.DTMAX, self.ALPHA, self.DAMP
                    )
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
