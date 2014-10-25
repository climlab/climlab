'''The first several Legendre polynomials, along with 
(some of) their first derivatives.'''

def Pn(x):
    '''Calculate lots of Legendre polyomials and return them in a dictionary.'''
    Pn = {}
    Pn['0'] = P0(x)
    Pn['1'] = P1(x)
    Pn['2'] = P2(x)
    Pn['3'] = P3(x)
    Pn['4'] = P4(x)
    Pn['5'] = P5(x)
    Pn['6'] = P6(x)
    Pn['8'] = P8(x)
    Pn['10'] = P10(x)
    Pn['12'] = P12(x)
    Pn['14'] = P14(x)
    return Pn
    
def Pnprime(x):
    '''First derivatives of Legendre polynomials.'''
    Pnprime = {}
    Pnprime['0'] = 0
    Pnprime['1'] = P1prime(x)
    Pnprime['2'] = P2prime(x)
    Pnprime['3'] = P3prime(x)
    Pnprime['4'] = P4prime(x)
    Pnprime['6'] = P6prime(x)
    Pnprime['8'] = P8prime(x)
    Pnprime['10'] = P10prime(x)
    Pnprime['12'] = P12prime(x)
    Pnprime['14'] = P14prime(x)
    return Pnprime

def P0( x ):
    return 1.

def P1( x ):
    return x

def P2( x ):
    '''The second Legendre polynomial.'''
    return (3. * x**2 - 1. ) / 2.

def P3( x ):
    return (5.* x**3 - 3.*x) / 2.
    
def P4( x ):
    return (35*x**4 -30*x**2 + 3)/8.
    
def P5(x):
    return (63.*x**5 - 70.*x**3 + 15.*x)/8.
    
def P6(x):
    return (231*x**6-315*x**4+105*x**2-5)/16.
    
def P8(x):
    return (6435*x**8-12012*x**6+6930*x**4-1260*x**2+35)/128.

def P10(x):
    return (46189*x**10-109395*x**8+90090*x**6-30030*x**4+
            3465*x**2-63)/256.
    
def P12(x):
    return (676039*x**12-1939938*x**10+2078505*x**8-1021020*x**6+
        225225*x**4-18018*x**2+231)/1024.

def P14(x):
    return (5014575*x**14-16900975*x**12+22309287*x**10-
        14549535*x**8+4849845*x**6-765765*x**4+45045*x**2-429)/2048.

def P1prime(x):
    return 1.

def P2prime(x):
    return 3.*x
    
def P3prime(x):
    return (15*x**2 - 3)/2.

def P4prime(x):
    return (140*x**3-60*x)/8.
    
def P6prime(x):
    return (1386*x**5-1260*x**3+210*x)/16.

def P8prime(x):
    return (51480*x**7-72072*x**5+27720*x**3-2520*x)/128.
    
def P10prime(x):
    return (461890*x**9-875160*x**7+540540*x**5-
        120120*x**3+6930*x)/256.

def P12prime(x):
    return (8112468*x**11-19399380*x**9+16628040*x**7-
        6126120*x**5+900900*x**3-36036*x)/1024.

def P14prime(x):
    return (70204050*x**13-202811700*x**11+223092870*x**9-
        116396280*x**7+29099070*x**5-3063060*x**3+90090*x)/2048.
