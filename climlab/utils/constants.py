# constants.py

#Part of the climlab package
#Brian Rose, University at Albany
#brose@albany.edu

"""Contains a collection of physical constants for the atmosphere and ocean.

    .. literalinclude:: ../code_input_manual/constants.py

"""

from __future__ import division
from numpy import pi

a = 6.373E6      # Radius of Earth (m)
Lhvap = 2.5E6    # Latent heat of vaporization (J / kg)
Lhsub = 2.834E6   # Latent heat of sublimation (J / kg)
Lhfus = Lhsub - Lhvap  # Latent heat of fusion (J / kg)
cp = 1004.     # specific heat at constant pressure for dry air (J / kg / K)
Rd = 287.         # gas constant for dry air (J / kg / K)
kappa = Rd / cp
Rv = 461.5       # gas constant for water vapor (J / kg / K)
cpv = 1875.   # specific heat at constant pressure for water vapor (J / kg / K)
eps = Rd / Rv
Omega = 2 * pi / 24. / 3600.  # Earth's rotation rate, (s**(-1))
g = 9.8          # gravitational acceleration (m / s**2)
kBoltzmann = 1.3806488E-23  # the Boltzmann constant (J / K)
c_light = 2.99792458E8   # speed of light (m/s)
hPlanck = 6.62606957E-34  # Planck's constant (J s)
#  Stefan-Boltzmann constant (W / m**2 / K**4) derived from fundamental constants
sigma = (2*pi**5 * kBoltzmann**4) / (15 * c_light**2 * hPlanck**3)

S0 = 1365.2       # solar constant (W / m**2)
# value is consistent with Trenberth and Fasullo, Surveys of Geophysics 2012

ps = 1000.       # approximate surface pressure (mb or hPa)

rho_w = 1000.    # density of water (kg / m**3)
cw = 4181.3      # specific heat of liquid water (J / kg / K)

tempCtoK = 273.15   # 0degC in Kelvin
tempKtoC = -tempCtoK  # 0 K in degC
mb_to_Pa = 100.  # conversion factor from mb to Pa

#  Some useful time conversion factors
seconds_per_minute = 60.
minutes_per_hour = 60.
hours_per_day = 24.

# the length of the "tropical year" -- time between vernal equinoxes
# This value is consistent with Berger (1978)
# "Long-Term Variations of Daily Insolation and Quaternary Climatic Changes"
days_per_year = 365.2422
seconds_per_hour = minutes_per_hour * seconds_per_minute
minutes_per_day = hours_per_day * minutes_per_hour
seconds_per_day = hours_per_day * seconds_per_hour
seconds_per_year = seconds_per_day * days_per_year
minutes_per_year = seconds_per_year / seconds_per_minute
hours_per_year = seconds_per_year / seconds_per_hour
#  average lenghts of months based on dividing the year into 12 equal parts
months_per_year = 12.
seconds_per_month = seconds_per_year / months_per_year
minutes_per_month = minutes_per_year / months_per_year
hours_per_month = hours_per_year / months_per_year
days_per_month = days_per_year / months_per_year

area_earth = 4 * pi * a**2

# present-day orbital parameters in dictionary form
orb_present = {'ecc': 0.017236, 'long_peri': 281.37, 'obliquity': 23.446}
#  usage should be indentical to
    #from climlab.solar.orbital import OrbitalTable
    #orb_present = OrbitalTable.sel(kyear=-0)
# But avoids reading the data file unnecessarily

# Molecular weights in g/mol
molecular_weight = {'N2': 28.,
                    'O2': 32.,
                    'H2O': 18.01528,
                    'CO2': 44.,
                    'N2O': 44.,
                    'CH4': 16.,
                    'CFC11': 136.,
                    'CFC12': 120.,
                    'O3': 48.,
                    'SO2': 64.,
                    'SO4': 96.,
                    'H2O2': 34.,
                    'dry air': 28.97,
                   }
