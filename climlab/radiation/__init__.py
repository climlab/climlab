from aplusbt import AplusBT, AplusBT_CO2
from boltzmann import Boltzmann
from insolation import FixedInsolation, P2Insolation, AnnualMeanInsolation, DailyInsolation
from radiation import Radiation
#from three_band import ThreeBandSW
from nband import NbandRadiation, ThreeBandSW
from water_vapor import ManabeWaterVapor
#  CAM3Radiation requires netCDF4
try:
    from cam3rad import CAM3Radiation, CAM3Radiation_LW, CAM3Radiation_SW
except:
    print 'CAM3Radiation module not available.'
try:
    from rrtm import RRTMG, RRTMG_LW, RRTMG_SW
except:
    print 'RRTM module not available.'
