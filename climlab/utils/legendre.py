"""Can calculate the first several Legendre polynomials, along with
(some of) their first derivatives."""

from __future__ import division

def Pn(x):
    """Calculate Legendre polyomials P0 to P28 and returns them
    in a dictionary ``Pn``.

    :param float x:     argument to calculate Legendre polynomials
    :return Pn:         dictionary which contains order of Legendre polynomials
                        (from 0 to 28) as keys and the corresponding evaluation
                        of Legendre polynomials as values.
    :rtype:             dict

    """
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
    Pn['16'] = P16(x)
    Pn['18'] = P18(x)
    Pn['20'] = P20(x)
    Pn['22'] = P22(x)
    Pn['24'] = P24(x)
    Pn['26'] = P26(x)
    Pn['28'] = P28(x)
    return Pn

def Pnprime(x):
    """Calculates first derivatives of Legendre polynomials and returns them
    in a dictionary ``Pnprime``.

    :param float x:     argument to calculate first derivate of Legendre polynomials
    :return Pn:         dictionary which contains order of Legendre polynomials
                        (from 0 to 4 and even numbers until 14) as keys and
                        the corresponding evaluation of first derivative of
                        Legendre polynomials as values.
    :rtype:             dict

    """
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
    """
    .. math::
        P_0 (x) = 1

    """
    return 1.

def P1( x ):
    """
    .. math::
        P_1 (x) = 1

    """
    return x

def P2( x ):
    """The second Legendre polynomial.

    .. math::
        P_2(x) = \\frac{1}{2} (3x^2 - 1)

    """
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

def P16(x):
    return (6435 - 875160*x**2 + 19399380*x**4 - 162954792*x**6 + 669278610*x**8 - 1487285800*x**10 +
        1825305300*x**12 - 1163381400*x**14 + 300540195*x**16)/32768.

def P18(x):
    return (-12155 + 2078505*x**2 - 58198140*x**4 + 624660036*x**6 - 3346393050*x**8 +
        10039179150*x**10 - 17644617900*x**12 +18032411700*x**14 - 9917826435*x**16 +
        2268783825*x**18)/65536.

def P20(x):
    return (1/262144.)*(46189 - 9699690*x**2 + 334639305*x**4 - 4461857400*x**6 +
        30117537450*x**8 - 116454478140*x**10 + 273491577450*x**12 -
        396713057400*x**14 + 347123925225*x**16 - 167890003050*x**18 + 34461632205*x**20)

def P22(x):
    return (1/524288.)*(-88179 + 22309287*x**2 - 929553625*x**4 + 15058768725*x**6 -
        124772655150*x**8 + 601681470390*x**10 - 1805044411170*x**12 + 3471239252250*x**14 -
        4281195077775*x**16 + 3273855059475*x**18 - 1412926920405*x**20 +
        263012370465*x**22)

def P24(x):
    return (1/4194304.)*(676039 - 202811700*x**2 + 10039179150*x**4 - 194090796900*x**6 +
        1933976154825*x**8 - 11345993441640*x**10 + 42117702927300*x**12 -
        102748681866600*x**14 + 166966608033225*x**16 - 178970743251300*x**18 +
        121511715154830*x**20 - 47342226683700*x**22 + 8061900920775*x**24)

def P26(x):
    return (1/8388608.)*(-1300075 + 456326325*x**2 - 26466926850*x**4 + 601681470390*x**6 -
        7091245901025*x**8 + 49638721307175*x**10 - 222622144044300*x**12 +
        667866432132900*x**14 - 1369126185872445*x**16 + 1923935489951475*x**18 -
        1822675727322450*x**20 + 1112542327066950*x**22 - 395033145117975*x**24 +
        61989816618513*x**26)

def P28(x):
    return (1/33554432.)*(5014575 - 2035917450*x**2 + 136745788725*x**4 -
        3610088822340*x**6 + 49638721307175*x**8 - 408140597414550*x**10 +
        2170565904431925*x**12 - 7823578204985400*x**14 + 19624141997505045*x**16 -
        34630838819126550*x**18 + 42832879592077575*x**20 - 36343049350853700*x**22 +
        20146690401016725*x**24 - 6570920561562378*x**26 + 956086325095055*x**28)

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
