'''The first several Legendre polynomials'''

def P0( x ):
    return 1.

def P1( x ):
    return x

def P2( x ):
    '''The second Legendre polynomial.'''
    return 1. / 2. * (3. * x**2 - 1. )

def P3( x ):
    return 1. / 2. * (5.* x**3 - 3.*x)
    
def P4( x ):
    return 1./8. * (35*x**4 -30*x**2 + 3)