"""
The object ``climlab.solar.orbital.OrbitalTable`` is an ``xarray.Dataset``
holding orbital data (**eccentricity**, **obliquity**, and **longitude of perihelion**)
for the past 5 Myears. The data are from :cite:`Berger_1991`.

Data are read from the file ``orbit91``, which was originally obtained from
<https://www1.ncdc.noaa.gov/pub/data/paleo/climate_forcing/orbital_variations/insolation/>
If the file isn't found locally, the module will attempt to read it remotely
from the above URL.

A subclass ``climlab.solar.orbital.long.OrbitalTable``
works with La2004 orbital data for
-51 to +21 Myears as calculated by :cite:`Laskar_2004`.
See <http://vo.imcce.fr/insola/earth/online/earth/La2004/README.TXT>

(Breaking change from climlab 0.7.0 and previous)

    :Example:
        Load orbital data from the past 5 Myears::

            # Load the data
            from climlab.solar.orbital import OrbitalTable

            # Examine the xarray object
            print(OrbitalTable)

            # Get a timeseries of obliquity
            print(OrbitalTable.obliquity)

            # Get the orbital data for a specific year, 10 kyear before present:
            print(OrbitalTable.interp(kyear=-10))

            # Get the long orbital table data
            from climlab.solar.orbital.long import OrbitalTable as LongTable
            print(LongTable)
"""
from __future__ import division, print_function, absolute_import
import numpy as np
import pandas as pd
import xarray as xr
from .table import _get_Berger_data

OrbitalTable = _get_Berger_data()
