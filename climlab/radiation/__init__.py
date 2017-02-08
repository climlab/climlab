from aplusbt import AplusBT, AplusBT_CO2
from boltzmann import Boltzmann
from insolation import FixedInsolation, P2Insolation, AnnualMeanInsolation, DailyInsolation
from nband import NbandRadiation, ThreeBandSW
from water_vapor import ManabeWaterVapor
#from radiation import Radiation, Radiation_SW, Radiation_LW
#  CAM3 requires netCDF4
try:
    from cam3 import CAM3, CAM3_LW, CAM3_SW
except:
    print 'CAM3 Radiation module not available.'
try:
    from rrtm import RRTMG, RRTMG_LW, RRTMG_SW
except:
    print 'RRTM module not available.'
