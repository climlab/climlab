'''Reads the orbital parameter tables for -51 to +21 Myears
as calculated by Laskar et al. 2004
http://www.imcce.fr/Equipes/ASD/insola/earth/La2004/README.TXT

This module is hard-coded to fetch the data from the above website
and return the data in a 4-column numpy array.

The data format is
Column 0: Time from J2000  in 1000 years
Column 1: eccentricity
Column 2: obliquity (radians)
Column 3: longitude of perihelion from moving equinox (radians)
'''

import numpy as np
import urllib2

def read_La2004():
    base_url = 'http://www.imcce.fr/Equipes/ASD/insola/earth/La2004/'
    past_file = 'INSOLN.LA2004.BTL.ASC'
    future_file = 'INSOLP.LA2004.BTL.ASC'
    
    num_lines_past = 51001
    num_lines_future = 21001
    num_columns = 4
    data_past = np.empty((num_lines_past,num_columns))
    data_future = np.empty((num_lines_future, num_columns))
    
    print 'Accessing orbital data from ' + base_url
    #  loop through each line of the file, read it into numpy array
    for (data,filename) in zip((data_past,data_future), 
                            (past_file,future_file)):
        print 'Reading file ' + filename
        record = urllib2.urlopen( base_url + filename )
        for index,line in enumerate(record):
            str1 = line.rstrip()  # remove newline character
            str2 = str1.replace('D','E')  # put string into numpy format
            data[index,:] = np.fromstring(str2, sep=' ')
        record.close()
    
    #  need to flip it so the data runs from past to present
    data_past = np.flipud(data_past)
    # and expunge the first line of the future data because it repeats year 0
    data = np.concatenate((data_past,data_future[1:,:]), axis=0)
    return(data)
    #  This now returns an identical array to my old La2004_orbital.py