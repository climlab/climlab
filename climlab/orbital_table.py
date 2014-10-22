import numpy as np
from scipy import interpolate
import constants as const


#  A new class will hold orbital table data in numpy arrays
# as well as an associated method to load the data.

#  OrbitalTable should have fields called
#   kyear
#   ecc
#   long_peri  (degrees)
#   obliquity   (degrees)
#  Each is a 1D column array of same length
#  Also will have min and max kyears loaded with the data, so that the lookup 
#  can throw an exception if you give a year outside this range
class OrbitalTable:
    def __init__(self):
        self.kyear = None
        self.ecc = None
        self.long_peri = None
        self.obliquity = None
        #  call a method that reads data from a file and populates the arrays
        self.get_data()
        
        #  calculate linear interpolants
        self.f_ecc = interpolate.interp1d(self.kyear, self.ecc)
        self.f_long_peri = interpolate.interp1d(self.kyear, self.long_peri)
        self.f_obliquity = interpolate.interp1d(self.kyear, self.obliquity)

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
        this_long_peri = self.f_long_peri(kyear)
        this_obliquity = self.f_obliquity(kyear)
        orb = {'ecc':this_ecc, 'long_peri':this_long_peri, 'obliquity':this_obliquity, 'kyear':kyear}
    
        return orb

    def get_data(self):
        pass
        #  need to read the BergerLoutre data here

#  Will then sub-class this to create a "Long Orbital Table" that uses the LA2004 data