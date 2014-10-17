#  experimental code to read the La2004 orbital data tables remotely
# so that we don't have to distribute the data tables with the code

import numpy as np
import urllib

La2004_url = 'http://www.imcce.fr/Equipes/ASD/insola/earth/La2004/INSOLN.LA2004.BTL.ASC'

response = urllib.urlopen( La2004_url )

num_lines = 72001
num_columns = 4

data = np.empty((num_lines,num_columns))
i=0

#  This is simple and WOULD work except that
#  numpy doesn't recognize string literals in fortran format e.g.
# 0.1716138713254424D-01
#   because it wants E instead of D!
#  What's the elegant way to swap all D's for E's in the file?
data = np.fromstring(response)

#  This doesn't work either and is uglier
#for line in response:
#    data[i,:] = line.rstrip()
#    i+=1
    