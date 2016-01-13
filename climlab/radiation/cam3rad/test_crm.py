#!/usr/bin/env python

# This script compares CliMT's version of NCAR's CRM with the original.
# To do the comparision:
# first build and run CRM out-of-the-box:
# - go to CliMT/src/radiation/ccm3/src/crm-2.1.2-ccm-3.6/bld and build CRM (you may need to modify Makefile)
# - go to ../bin and do ./crm < ../data/mls_clr.in
# Now set up CliMT for comparison:
# - in CliMT/src/radiation/ccm3/crm.F, change crm_printout to .true.
# - recompile with km=18 in CliMT/src/Makefile
# - run this script

from numpy import *

p    = zeros(18,'d')
T    = zeros(18,'d')
q    = zeros(18,'d')
o3   = zeros(18,'d')
cldf = zeros(18,'d')
clwp = zeros(18,'d')

i=0
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) =   2.026, 267.03, 4.000e-06, 7.500e-06, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) =   5.470, 248.22, 4.000e-06, 9.800e-06, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) =  15.296, 231.98, 4.000e-06, 9.000e-06, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) =  33.936, 222.56, 4.000e-06, 6.200e-06, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) =  60.780, 218.01, 4.000e-06, 3.500e-06, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 103.225, 215.75, 4.000e-06, 1.400e-06, 0.0, 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 161.270, 215.86, 4.244e-06, 6.500e-07, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 234.510, 227.25, 4.603e-05, 3.000e-07, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 323.046, 241.61, 2.502e-04, 1.850e-07, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 420.091, 253.99, 6.079e-04, 1.300e-07, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 516.833, 263.79, 1.121e-03, 1.000e-07, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 613.473, 271.92, 2.137e-03, 8.000e-08, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 709.910, 279.01, 3.863e-03, 7.000e-08, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 799.156, 284.84, 5.895e-03, 6.000e-08, 0., 0.
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 873.003, 288.74, 7.802e-03, 5.700e-08, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 931.555, 291.10, 9.394e-03, 5.300e-08, 0.0, 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) = 974.810, 292.67, 1.060e-02, 5.100e-08, 0., 0. 
i+=1
(p[i],T[i],q[i],o3[i],cldf[i],clwp[i]) =1002.769, 293.65, 1.138e-02, 5.000e-08, 0., 0. 

par={}
par['ps']       = 1013.
par['Ts']       = 294.
par['aldif']    = 0.1
par['aldir']    = 0.1
par['asdif']    = 0.1
par['asdir']    = 0.1
par['co2']      = 355.e-6  * 1.e6 
par['n2o']      = 0.311e-6 * 1.e6 
par['ch4']      = 1.714e-6 * 1.e6 
par['cfc11']    = 0.280e-9 * 1.e6 
par['cfc12']    = 0.503e-9 * 1.e6 
par['tauvis']   = 0. #0.14
par['scon']     = 1367.
par['orb_year'] = 1950
par['avg']      = 'inst'
par['calday']   =  83.333333
par['lat']      = 0.
par['lon']      = 0.
par['Cpd']      = 1004.64
par['epsilon']  = 0.622
par['g']        = 9.80616
par['p']        = p
par['T']        = T
par['q']        = q * 1.e3
par['o3']       = o3        
par['cldf']     = cldf      
par['clwp']     = clwp      

import climt

def compute(scheme='cam3'):
    r=climt.radiation(scheme=scheme)
    r(**par)


    print 'Zenith angle: %f' % r['zen']
    print 'Insolation:   %f' % r['solin']
    print 'TOA SW CRF:  %6.2f %6.2f' % (r['SwToa'],r['SwToaCf'])
    print 'TOA LW CRF:  %6.2f %6.2f' % (r['LwToa'],r['LwToaCf'])
    # output
    print "lev    p     T       q      O3          lwflx         lwhr       swflx         swhr"
    for i in range(r.nlev):
        print "%3i %6.1f %6.1f %6.3f %10.3e %12.5f %12.5f %12.5f %12.5f" % \
              (i, r['p'][i], r['T'][i], r['q'][i], r['o3'][i], \
               r['lwflx'][i], r['lwhr'][i], r['swflx'][i], r['swhr'][i]) 

#--------------------
if __name__ == '__main__':
    compute()
    #compute('ccm3')

