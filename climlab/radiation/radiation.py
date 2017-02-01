'''
Radiation is the base class for radiation processes
currently CAM3 and RRTMG

Basic characteristics:

State:
- Ts (surface radiative temperature)
- Tatm (air temperature)

Input (specified or provided by parent process):
- (fix this up)

Shortave processes should define these diagnostics (minimum):
- ASR (net absorbed shortwave radiation)
- SW_down_TOA
- SW_up_TOA
- SW_net_from_space
- SW_flux_to_sfc (downwelling flux to surface)
- SW_flux_from_sfc (upwelling flux from surface)
- SW_net_to_sfc (net absorbed at surface)
- TdotSW

May also have all the same diagnostics for clear-sky, e.g. ASR_clr
 ....

 work in progress

Longwave processes should define
'''
from __future__ import division
import numpy as np
from climlab.process import EnergyBudget


class Radiation(EnergyBudget):
    '''Base class for radiation models (currently CAM3 and RRTMG).
    '''
    def __init__(self, **kwargs):
        super(Radiation, self).__init__(**kwargs)
        #  Define inputs, default values, diagnostics
        #  Mechanism for setting up sensible ozone distribution
        #  Or any other prescribed gas.
