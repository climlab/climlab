# constants.py

#Part of the climlab package
#Brian Rose, University at Albany
#brose@albany.edu

"""Contains a collection of physical constants for the atmosphere and ocean.

    .. literalinclude:: ../code_input_manual/constants.py

"""

from __future__ import division
from numpy import pi
from attrdict import AttrDict

const_dict = AttrDict()

const_dict['a'] = 6.373E6      # Radius of Earth (m)
const_dict['Lhvap'] = 2.5E6    # Latent heat of vaporization (J / kg)
const_dict['Lhsub'] = 2.834E6   # Latent heat of sublimation (J / kg)
const_dict['Lhfus'] = const_dict['Lhsub'] - const_dict['Lhvap']  # Latent heat of fusion (J / kg)
const_dict['cp'] = 1004.     # specific heat at constant pressure for dry air (J / kg / K)
const_dict['Rd'] = 287.         # gas constant for dry air (J / kg / K)
const_dict['kappa'] = const_dict['Rd'] / const_dict['cp']
const_dict['Rv'] = 461.5       # gas constant for water vapor (J / kg / K)
const_dict['cpv'] = 1875.   # specific heat at constant pressure for water vapor (J / kg / K)
const_dict['eps'] = const_dict['Rd'] / const_dict['Rv']
const_dict['Omega'] = 2 * pi / 24. / 3600.  # Earth's rotation rate, (s**(-1))
const_dict['g'] = 9.8          # gravitational acceleration (m / s**2)
const_dict['kBoltzmann'] = 1.3806488E-23  # the Boltzmann constant (J / K)
const_dict['c_light'] = 2.99792458E8   # speed of light (m/s)
const_dict['hPlanck'] = 6.62606957E-34  # Planck's constant (J s)
#  Stefan-Boltzmann constant (W / m**2 / K**4) derived from fundamental constants
const_dict['sigma'] = (2*pi**5 * const_dict['kBoltzmann']**4) / (15 * const_dict['c_light']**2 * const_dict['hPlanck']**3)

const_dict['S0'] = 1365.2       # solar constant (W / m**2)
# value is consistent with Trenberth and Fasullo, Surveys of Geophysics 2012

const_dict['ps'] = 1000.       # approximate surface pressure (mb or hPa)

const_dict['rho_w'] = 1000.    # density of water (kg / m**3)
const_dict['cw'] = 4181.3      # specific heat of liquid water (J / kg / K)

const_dict['tempCtoK'] = 273.15   # 0degC in Kelvin
const_dict['tempKtoC'] = -const_dict['tempCtoK']  # 0 K in degC
const_dict['mb_to_Pa'] = 100.  # conversion factor from mb to Pa

#  Some useful time conversion factors
const_dict['seconds_per_minute'] = 60.
const_dict['minutes_per_hour'] = 60.
const_dict['hours_per_day'] = 24.

# the length of the "tropical year" -- time between vernal equinoxes
# This value is consistent with Berger (1978)
# "Long-Term Variations of Daily Insolation and Quaternary Climatic Changes"
const_dict['days_per_year'] = 365.2422
const_dict['seconds_per_hour'] = const_dict['minutes_per_hour'] * const_dict['seconds_per_minute']
const_dict['minutes_per_day'] = const_dict['hours_per_day'] * const_dict['minutes_per_hour']
const_dict['seconds_per_day'] = const_dict['hours_per_day'] * const_dict['seconds_per_hour']
const_dict['seconds_per_year'] = const_dict['seconds_per_day'] * const_dict['days_per_year']
const_dict['minutes_per_year'] = const_dict['seconds_per_year'] / const_dict['seconds_per_minute']
const_dict['hours_per_year'] = const_dict['seconds_per_year'] / const_dict['seconds_per_hour']
#  average lenghts of months based on dividing the year into 12 equal parts
const_dict['months_per_year'] = 12.
const_dict['seconds_per_month'] = const_dict['seconds_per_year'] / const_dict['months_per_year']
const_dict['minutes_per_month'] = const_dict['minutes_per_year'] / const_dict['months_per_year']
const_dict['hours_per_month'] = const_dict['hours_per_year'] / const_dict['months_per_year']
const_dict['days_per_month'] = const_dict['days_per_year'] / const_dict['months_per_year']

const_dict['area_earth'] = 4 * pi * const_dict['a']**2

# present-day orbital parameters in dictionary form
const_dict['orb_present'] = {'ecc': 0.017236, 'long_peri': 281.37, 'obliquity': 23.446}
#  usage should be indentical to
    #from climlab.solar.orbital import OrbitalTable
    #orb_present = OrbitalTable.sel(kyear=-0)
# But avoids reading the data file unnecessarily

# Molecular weights in g/mol
const_dict['molecular_weight'] = {'N2': 28.,
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
