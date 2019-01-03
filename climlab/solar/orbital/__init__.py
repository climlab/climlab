"""This module defines the class :class:`OrbitalTable` which holds orbital data,
and includes a method :func:`~OrbitalTable.lookup_parameters`
which interpolates the orbital data for a specific year
(- works equally well for arrays of years).

The base class :class:`OrbitalTable()` is designed to work with 5 Myears of orbital data
(**eccentricity, obliquity, and longitude of perihelion**) from :cite:`Berger_1991`.

Data will be read from the file orbit91, which was originally obtained from
ftp://ftp.ncdc.noaa.gov/pub/data/paleo/insolation/
If the file isn't found locally, the module will attempt to read it remotely
from the above URL.

A subclass :class:`LongOrbitalTable()` works with La2004 orbital data for
-51 to +21 Myears as calculated by :cite:`Laskar_2004`.
See http://vo.imcce.fr/insola/earth/online/earth/La2004/README.TXT

"""
from __future__ import division, print_function
import numpy as np
import pandas as pd
import xarray as xr
from .table import _get_Berger_data

OrbitalTable = _get_Berger_data()
