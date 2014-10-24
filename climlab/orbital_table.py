import numpy as np
from scipy import interpolate
import os
import urllib2

class OrbitalTable:
    def __init__(self):
        self.kyear = None
        self.ecc = None
        self.long_peri = None
        self.obliquity = None
        #  call a method that reads data from a file and populates the arrays
        self.get_data()
        self.compute_interpolants()
        # find and store min and max years. lookup_parameters should throw an exception
        # if you ask for something outside this range -- not yet implemented.
        self.kyear_max = np.max(self.kyear)
        self.kyear_min = np.min(self.kyear)

    def lookup_parameters( self, kyear = 0 ):
        """Look up orbital parameters for given kyear after present.
    
        Input kyear is thousands of years after present.
        Negative numbers imply years before present!
    
        Returns a dictionary of three values: 
        ecc = eccentricity (dimensionless)
        long_peri = longitude of perihelion (precession angle) (degrees)
        obliquity = obliquity angle or axial tilt (degrees).
        """
        #  linear interpolation:
        this_ecc = self.f_ecc(kyear)
        this_obliquity = self.f_obliquity(kyear)
        this_long_peri = self.f_long_peri(kyear)
        #  convert long_peri to an angle (in degrees) between 0 and 360
        long_peri_converted = this_long_peri % 360.   
        # Build a dictionary of all the parameters
        orb = {'ecc':this_ecc, 'long_peri':long_peri_converted, 'obliquity':this_obliquity, 'kyear':kyear}
    
        return orb

    def get_data(self):
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
            print 'Loading Berger and Loutre (1991) orbital parameter data from file ' + fullfilename
        except:
            record = urllib2.urlopen( base_url + past_file )
            print 'Accessing Berger and Loutre (1991) orbital data from ' + base_url
            print 'Reading file ' + past_file
    
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
    
    def compute_interpolants(self):
        # add 180 degrees to long_peri (see lambda definition, Berger 1978 Appendix)
        long_peri0rad = np.deg2rad(self.long_peri + 180.) 
        long_peri0 = np.rad2deg( np.unwrap( long_peri0rad ) ) # remove discontinuities (360 degree jumps)
        #  calculate linear interpolants
        self.f_ecc = interpolate.interp1d(self.kyear, self.ecc)
        self.f_long_peri = interpolate.interp1d(self.kyear, long_peri0)
        self.f_obliquity = interpolate.interp1d(self.kyear, self.obliquity)

#  Will then sub-class this to create a "Long Orbital Table" that uses the LA2004 data
