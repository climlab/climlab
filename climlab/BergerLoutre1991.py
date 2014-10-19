'''Reads the orbital parameter tables for -51 to +21 Myears
as calculated by Berger and Loutre 1991
http://hurricane.ncdc.noaa.gov/pls/paleox/f?p=519:1:0::::P1_STUDY_ID:5776
or
ftp://ftp.ncdc.noaa.gov/pub/data/paleo/insolation/

This module is hard-coded to fetch the data from the above website
and return the data in a 4-column numpy array.

The data format is
Column 0: is kyr from present (negative because backward in time)
Column 1: is eccentricity (dimensionless)
Column 2: is longitude of perihelion (degrees)
Column 3: obliquity angle (degrees)
'''

import numpy as np
import urllib2

def get_data():
    base_url = 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/insolation/'
    past_file = 'orbit91'
    
    num_lines_past = 5001
    num_columns = 4
    data = np.empty((num_lines_past,num_columns))
    
    print 'Accessing orbital data from ' + base_url
    #  loop through each line of the file, read it into numpy array
    print 'Reading file ' + past_file
    record = urllib2.urlopen( base_url + past_file )
    #  skip first three lines of header
    toskip = 3
    for i in range(toskip):
        record.readline()
    for index,line in enumerate(record):
        str1 = line.rstrip()  # remove newline character
        thisdata = np.fromstring(str1, sep=' ')
        # ignore after the 4th column
        data[index,:] = thisdata[0:4]
    record.close()
    
    #  need to flip it so the data runs from past to present
    #data_past = np.flipud(data_past)
    return(data)
    #  This now returns an identical array to my old La2004_orbital.py