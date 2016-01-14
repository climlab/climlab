import math

class Parameters:
    '''
    '''
    def __init__(self,**kwargs):

        # Set default param values
        self._setDefaults()

        # Used items will be popped from kwargs
        self.unused_kwargs = kwargs

        # Update params requested in kwargs
        for key in self.value:
            if key in kwargs and kwargs[key] is not None:
                self.value[key] = kwargs.pop(key)

    def _setDefaults(self):
        self.value = {}
        self.units = {}
        self.long_name = {}
        
        # Physical constants
        self.append('Cpd',     1005.7,  'J K-1 kg-1', 'Spec. heat dry air at const. press.') 
        self.append('Cvd',     718.,    'J K-1 kg-1', 'Spec. heat dry air at const. volume') 
        self.append('Cpv',     1870.,   'J K-1 kg-1', 'Specific heat of water vapour at const. press') 
        self.append('Cl',      4190.,   'J K-1 kg-1', 'Specific heat pure liquid water')
        self.append('Csw',     3985.,   'J K-1 kg-1', 'Specific heat sea water')
        self.append('Rv',      461.5,   'J K-1 kg-1', 'Gas constant for water vapour')
        self.append('Rd',      287.04,  'J K-1 kg-1', 'Gas constant dry air')
        self.append('Lv',      2.501e6,          ' ', 'Latent heat vaporisation')
        self.append('Lf',      3.336e5,          ' ', 'Lat heat of fusion')
        self.append('rowl',    1000.,       'kg m-3', 'Density of liquid water')
        self.append('stebol',  5.67e-8,          ' ', 'Stefan Boltzmann constant' )
        self.append('epsilon', 287.04/461.5,     '-', 'Rd/Rv')

        # Orbital and planetary parameters
        self.append('eccen',          0.016715,       '-', 'Eccentricity of orbit')
        self.append('obliq',           23.4441,     'deg', 'Planetary obliquity')
        self.append('prece',             102.7,     'deg', 'Precession of vernal equinox')
        self.append('orb_year',           1995,       '-', 'Earth year for which orbital params computed')
        self.append('lod',              86400.,       's', 'Length of day' ) 
        self.append('daysperyear',      365.25,       '-', 'Number of days in a year' )
        self.append('calday',             80.5,    'days', 'Julian calendar day' )
        self.append('r',              6374000.,       'm', 'Planetary radius' ) 
        self.append('g',                   9.8,   'm s-2', 'Gravitational acceleration')
        self.append('omega', 2.*math.pi/86400., 'rad s-1', 'Planetary rotation rate' ) 
        self.append('radius',               1.,      'AU', 'Mean orbital radius' )
        self.append('scon',              1367.,   'W m-2', 'Solar irradiance at mean orbital radius')

        # Parameters for axisymmetric dynamics
        self.append('Newt', 1.e9, 'days', 'Newtonian cooling timescale' )
        self.append('delh', 100.,    'K', 'Eq-pole gradient of target temp' )
        self.append('delv', 40.,     'K', 'Surface-trop pot temp increase' )

        # Parameters for simple turbulence
        self.append('nuv',             1., 'm^2/s',  'Kinematic viscosity' )
        self.append('Pr',              1., '-',      'Prandtl number (visc/cond)' )
        self.append('do_srf_mom_flx',   1, '-',      'Momentum flux thru surface: 1=Yes, 0=No' )
        self.append('do_srf_sen_flx',   1, '-',      'Heat flux thru surface: 1=Yes, 0=No' )
        self.append('do_srf_lat_flx',   1, '-',      'Moist flux thru surface: 1=Yes, 0=No' )
        self.append('Cd',          1.3e-3, '-',      'Surface flux coeff' )
        self.append('u0',              5., 'm s-1',  'surface wind for bulk surface flux' )

        # Parameters for radiative transfer
        self.append('do_sw',   1,     '-',      '1=do, 0=do not compute SW' )
        self.append('do_lw',   1,     '-',      '1=do, 0=do not compute LW' )
        self.append('in_cld',  0,     '-',      '1=in-cloud, 0=grid avg cloud water path' )
        self.append('co2',     380.,  'ppmv',   'CO2 conc.' )
        self.append('n2o',     1.e-9, 'ppmv',   'N2O conc.' )
        self.append('ch4',     1.e-9, 'ppmv',   'CH4 conc.' )
        self.append('cfc11',   1.e-9, 'ppmv',   'CFC11 conc.' )
        self.append('cfc12',   1.e-9, 'ppmv',   'CFC12 conc.' )
        self.append('cfc22',   1.e-9, 'ppmv',   'CFC22 conc.' ) 
        self.append('o2', .2095, '-', 'O2 volumetric mixing ratio')
        self.append('ccl4', 0., '-', 'volume mixing ratio')
        self.append('tauaer_sw', 0., '-', 'Aerosol optical depth')
        self.append('ssaaer_sw', 0., '-', 'Aerosol single scattering albedo')
        self.append('asmaer_sw', 0., '-', 'Aerosol asymmetry parameter')
        self.append('tauaer_lw', 0., '-', 'Aerosol optical depth')
        self.append('lw_surface_emissivity', 1., '-', 'lw surface emissivity')
        self.append('tauvis',  0.,    '-',      'Aerosol opt. depth, CCM3 scheme' )    
        self.append('tau_inf', 1.,    '-',      'Total optical depth, greygas scheme')
        self.append('alpha_greygas', 1., '-',    'tau profile shape parameter, greygas scheme' )
        self.append('beta_greygas', 1., '-',    'bandwith, greygas scheme' )

        # Parameters for Emanuel's convection scheme
        self.append('elcrit',   0.0011,   '-',      ' ' )
        self.append('tlcrit',   -55.,     '-',      ' ' )
        self.append('entp',     1.5,      '-',      ' ' )
        self.append('sigd',     0.05,     '-',      ' ' )
        self.append('sigs',     0.12,     '-',      ' ' )
        self.append('omtrain',  50.0,     '-',      ' ' )
        self.append('omtsnow',  5.5,     '-',      ' ' )
        self.append('coeffr',   1.0,     '-',      ' ' )
        self.append('coeffs',   0.8,     '-',      ' ' )
        self.append('cu',       0.7,     '-',      ' ' )
        self.append('beta',     10.0,     '-',      ' ' )
        self.append('dtmax',    0.9,     '-',      ' ' )
        self.append('alpha',    0.2,     '-',      ' ' )
        self.append('damp',     0.1,     '-',      ' ' )

        # Parameters for hard convective adjustment scheme
        self.append('lapse',   9.8/1005.7,  'K m-1',    'Specified lapse rate')

        # Parameters for simplified Betts-Miller convection scheme
        self.append('tau_bm',  7200.,    's',   'B-M relaxation timescale')
        self.append('es0',      611.,  'kPa',   'reference pressure for C-C relation')
        self.append('T00',    273.15,    'K',   'reference temperature for C-C relation')
        self.append('rhbm',      0.8,    '-',   'relative humidity of relaxation')
        self.append('Mv',        18.,  'AMU',   'molecular weight of condensible')
        self.append('Ma',        29.,  'AMU',   'mean molecular weight of dry air')
        
        # Parameters for ocean
        self.append('Hslab',   50.,      'm',  'Depth of slab ocean' )

        # Parameters for timestepping
        self.append('dt',   60*15.,    's',  'Time step' )
        self.append('afc',    0.05,    '-',  'Asselin filter coefficient' )

    def append(self,*arg):
        key = arg[0]
        self.value[key] = arg[1]
        self.units[key] = arg[2]
        self.long_name[key] = arg[3]
        
    # The following methods allow a Parameters instance to be treated as a dictionary
    def __getitem__(self,key):
        try: return self.value[key]
        except: raise IndexError,'\n\n CliMT.Params: %s not a known parameter name' % str(key)
        
    def __setitem__(self,key,value):
        self.value[key] = value
        
    def keys(self):
        return self.value.keys()

    def __iter__(self):
        return self.value.__iter__()
