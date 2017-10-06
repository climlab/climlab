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
from future import standard_library
standard_library.install_aliases()
from builtins import zip, range, object
import numpy as np
from scipy import interpolate
import os
try:
    # This should work in Python 3.x
    from urllib.request import urlopen
except:
    # Fallback for Python 2.7
    from urllib2 import urlopen

class OrbitalTable(object):
    """Invoking OrbitalTable() will load 5 million years of orbital data
    from :cite:`Berger_1991` and compute linear interpolants.

    The data can be accessed through the method :func:`lookup_parameters()`.


    **Object attributes** \n

    Following object attributes are generated during initialization:

    :ivar array kyear:          time table with negative values are before present
                                (*unit:* kyears)
    :ivar array ecc:            eccentricity over time (*unit:* dimensionless)
    :ivar array long_peri:      longitude of perihelion (precession angle) (*unit:* degrees)
    :ivar array obliquity:      obliquity angle (*unit:* degrees)
    :ivar float kyear_min:      minimum value of time table (*unit:* kyears)
    :ivar float kyear_max:      maximum value of time table (*unit:* kyears)

    """
    def __init__(self):
        self.kyear = None
        self.ecc = None
        self.long_peri = None
        self.obliquity = None
        #  call a method that reads data from a file and populates the arrays
        self._get_data()
        self._compute_interpolants()
        # find and store min and max years. lookup_parameters should throw an exception
        # if you ask for something outside this range -- not yet implemented.
        self.kyear_max = np.max(self.kyear)
        self.kyear_min = np.min(self.kyear)

    def lookup_parameters( self, kyear = 0 ):
        """Look up orbital parameters for given kyear measured from present.

        .. note::

            Input ``kyear`` is thousands of years after present.
            For years before present, use ``kyear < 0``.

        **Function-call argument** \n

        :param array kyear:     Time for which oribtal parameters should be given.
                                Will handle scalar or vector input (for multiple years).
                                [default: 0]

        :returns:               a three-member dictionary of orbital parameters:

                                    * ``'ecc'``: eccentricity (dimensionless)
                                    * ``'long_peri'``: longitude of perihelion
                                      relative to vernal equinox (degrees)
                                    * ``'obliquity'``: obliquity angle or axial tilt (degrees).

                                Each member is an array of same size as kyear.
        :rtype:                 dict

        """
        #  linear interpolation:
        this_ecc = self.f_ecc(kyear)
        this_obliquity = self.f_obliquity(kyear)
        this_long_peri = self.f_long_peri(kyear)
        #  convert long_peri to an angle (in degrees) between 0 and 360
        long_peri_converted = this_long_peri % 360.
        # Build a dictionary of all the parameters
        orb = {'ecc':this_ecc, 'long_peri':long_peri_converted, 'obliquity':this_obliquity}
        return orb

    def _get_data(self):
        past_file = 'orbit91'
        base_url = 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/insolation/'
        #  This gives the full path to the data file, assuming it's in the same directory
        local_path = os.path.dirname(__file__)
        fullfilename = os.path.join(local_path, past_file)

        num_lines_past = 5001
        self.kyear = np.empty(num_lines_past)
        self.ecc = np.empty_like(self.kyear)
        self.long_peri = np.empty_like(self.kyear)
        self.obliquity = np.empty_like(self.kyear)

        #  This gives the full path to the data file, assuming it's in the same directory
        fullfilename = os.path.join(os.path.dirname(__file__), past_file)
        try:
            record = open(fullfilename,'r')
            print('Loading Berger and Loutre (1991) orbital parameter data from file ' + fullfilename)
        except:
            print('Failed to load orbital locally, trying to access it via remote ftp.')
            try:
                record = urlopen( base_url + past_file )
                print('Accessing Berger and Loutre (1991) orbital data from ' + base_url)
                print('Reading file ' + past_file)
            except:
                raise Exception('Failed to load the data via remote ftp.')

        #  loop through each line of the file, read it into numpy array
        #  skip first three lines of header
        toskip = 3
        for i in range(toskip):
            record.readline()
        for index,line in enumerate(record):
            str1 = line.rstrip()  # remove newline character
            thisdata = np.fromstring(str1, sep=' ')
            # ignore after the 4th column
            self.kyear[index] = thisdata[0]
            self.ecc[index] = thisdata[1]
            self.long_peri[index] = thisdata[2]
            self.obliquity[index] = thisdata[3]
        record.close()

    def _compute_interpolants(self):
        # add 180 degrees to long_peri (see lambda definition, Berger 1978 Appendix)
        long_peri0rad = np.deg2rad(self.long_peri + 180.)
        long_peri0 = np.rad2deg( np.unwrap( long_peri0rad ) ) # remove discontinuities (360 degree jumps)
        #  calculate linear interpolants
        self.f_ecc = interpolate.interp1d(self.kyear, self.ecc)
        self.f_long_peri = interpolate.interp1d(self.kyear, long_peri0)
        self.f_obliquity = interpolate.interp1d(self.kyear, self.obliquity)


class LongOrbitalTable(OrbitalTable):
    """Loads orbital parameter tables for -51 to +21 Myears.

    Based on calculations by :cite:`Laskar_2004`
        http://vo.imcce.fr/insola/earth/online/earth/La2004/README.TXT

    Usage is identical to parent class :class:`OrbitalTable()`.

    """
    def _get_data(self):
        base_url = 'http://vo.imcce.fr/insola/earth/online/earth/La2004/'
        past_file = 'INSOLN.LA2004.BTL.ASC'
        future_file = 'INSOLP.LA2004.BTL.ASC'

        num_lines_past = 51001
        num_lines_future = 21001
        num_columns = 4
        data_past = np.empty((num_lines_past,num_columns))
        data_future = np.empty((num_lines_future, num_columns))

        print('Attempting to access La2004 orbital data from ' + base_url)
        #  loop through each line of the file, read it into numpy array
        for (data,filename) in zip((data_past,data_future),
                            (past_file,future_file)):
            print('Reading file ' + filename)
            record = urlopen( base_url + filename )
            for index,line in enumerate(record):
                str1 = line.rstrip()  # remove newline character
                try:
                    str2 = str1.replace('D','E')  # put string into numpy format
                except:
                    #  in Python 3 we need to convert from bytes object first
                    str2 = (str(str1,'utf-8')).replace('D','E')
                data[index,:] = np.fromstring(str2, sep=' ')
            record.close()
        #  need to flip it so the data runs from past to present
        data_past = np.flipud(data_past)
        # and expunge the first line of the future data because it repeats year 0
        data = np.concatenate((data_past,data_future[1:,:]), axis=0)
        self.kyear = data[:,0]
        self.ecc = data[:,1]
        self.obliquity = np.rad2deg(data[:,2])
        self.long_peri = np.rad2deg(data[:,3])
