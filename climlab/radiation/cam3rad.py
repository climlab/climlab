'''
climlab wrap of the CAM3 radiation code
'''
import numpy as np
from climlab import constants as const
from climlab.radiation.radiation import Radiation
import os, sys, subprocess, glob, string
from numpy.distutils import fcompiler
from distutils.dep_util import newer
import netCDF4 as nc


class CAM3Radiation(Radiation):
    def __init__(self, **kwargs):
        super(CAM3Radiation, self).__init__(**kwargs)
        self.KM = self.lev.size
        try:
            self.JM = self.lat.size
        except:
            self.JM = 1
        try:
            self.IM = self.lon.size
        except:
            self.IM = 1
        self._build_extension()
        self._init_extension()
        self.ToExtension    = ['do_sw','do_lw','p','dp','ps','Tatm','Ts','q','o3','cldf','clwp','ciwp',
                               'in_cld','aldif','aldir','asdif','asdir','zen','solin','flus','r_liq','r_ice',
                               'co2','n2o','ch4','cfc11','cfc12','g','Cpd',
                               'epsilon','stebol','dt']
        self.FromExtension  = ['Tinc','TdotRad','SrfRadFlx','swhr','lwhr','swflx','lwflx','SwToaCf',
                               'SwSrfCf','LwToaCf','LwSrfCf','LwToa','LwSrf','SwToa','SwSrf','lwuflx','lwdflx']
        self.do_sw = 1  # '1=do, 0=do not compute SW'
        self.do_lw = 1  # '1=do, 0=do not compute LW'
        self.in_cld = 0 # '1=in-cloud, 0=grid avg cloud water path'
        #  Set up some useful defaults, mostly following climt/state.py
        self.shape3D = (self.KM, self.JM, self.IM)
        self.shape2D = self.shape3D[1:]
        self.ps = const.ps * np.ones(self.shape2D)
        #lev = (np.arange(self.KM)+0.5) * const.ps/self.KM
        #self.p = np.transpose(np.resize(lev,self.shape3D[::-1]))
        self.p = self.lev
        self.dp = np.zeros_like(self.p) - 99. # set as missing
        self.q = 1.e-5*np.ones_like(self.p)
        self.o3 = np.zeros_like(self.p) + 1.E-9
        # Cloud frac
        self.cldf = np.zeros_like(self.p)
        # Cloud water path
        self.clwp = np.zeros_like(self.p)
        self.ciwp = np.zeros_like(self.p)
        # Effective radius cloud drops
        self.r_liq = np.zeros_like(self.p) + 10.
        self.r_ice = np.zeros_like(self.p) + 30.
        # Surface upwelling LW
        self.flus = np.zeros_like(self.Ts) - 99. # set to missing as default
        # Albedos
        self.asdir = np.zeros_like(self.Ts) + 0.07
        self.asdif = np.zeros_like(self.Ts) + 0.07
        self.aldir = np.zeros_like(self.Ts) + 0.07
        self.aldif = np.zeros_like(self.Ts) + 0.07
        # Solar zenith angle
        self.zen = 0.5
        # Insolation
        self.solin = 341.5 * np.ones_like(self.Ts)
        # Absorbing gases in ppmv
        self.co2 = 380.
        self.n2o = 1.E-9
        self.ch4 = 1.E-9
        self.cfc11 = 1.E-9
        self.cfc12 = 1.E-9
        # physical constants
        self.g = const.g
        self.Cpd = const.cp
        self.epsilon = const.Rd / const.Rv
        self.stebol = const.sigma
        self.dt = self.timestep

    def _climlab_to_cam3(self, field):
        '''Prepare field wit proper dimension order.
        CAM3 code expects 3D arrays with (KM, JM, IM)
        and 2D arrays with (JM, IM).'''
        #   THIS NEEDS TO BE GENERALIZED TO MULTIPLE DIMS
        return np.rollaxis(np.atleast_3d(np.flipud(field)),1)

    def _cam3_to_climlab(self, field):
        #   THIS NEEDS TO BE GENERALIZED TO MULTIPLE DIMS
        try:
            return np.flipud(np.squeeze(field))
        except:
            return np.squeeze(field)

    def _compute_radiative_heating(self):
        # List of arguments to be passed to extension
        #args = [ getattr(self,key) for key in self.ToExtension ]
        args = []
        for key in self.ToExtension:
            value = getattr(self, key)
            if np.isscalar(value):
                args.append(value)
            else:
                args.append(self._climlab_to_cam3(value))
        OutputValues = self.extension.driver(*args)
        Output = dict( zip(self.FromExtension, OutputValues ))
        self.Output = Output
        #for name, value in Output.iteritems():
        #    setattr(self, name, value)
        #  SrfRadFlx is net downward flux at surface
        self.heating_rate['Ts'] = self._cam3_to_climlab(Output['SrfRadFlx'])
        # lwhr and swhr are heating rates in J/kg/day from climt interface to CAM3
        #  (qrl and qrs in CAM3 code)
        #  Need to set to W/m2
        Catm = self.Tatm.domain.heat_capacity
        self.heating_rate['Tatm'] = (self._cam3_to_climlab(Output['TdotRad']) /
                                     const.seconds_per_day *
                                     Catm / const.cp)

    def _build_extension(self):
        name = 'cam3_radiation'
        #  Need to make this more robust...
        thisdir = os.path.dirname(__file__)
        dir = thisdir + '/../../src/radiation/cam3'
        # figure out which compiler we're goint to use
        compiler = fcompiler.get_default_fcompiler()
        # set some fortran compiler-dependent flags
        if compiler == 'gnu95':
            f77flags='-ffixed-line-length-132 -fdefault-real-8'
            f90flags='-fdefault-real-8 -fno-range-check -ffree-form'
        elif compiler == 'intel' or compiler == 'intelem':
            f77flags='-132 -r8'
            f90flags='-132 -r8'
        elif compiler == 'ibm':
            f77flags='-qautodbl=dbl4 -qsuffix=f=f:cpp=F -qfixed=132'
            f90flags='-qautodbl=dbl4 -qsuffix=f=f90:cpp=F90 -qfree=f90'
        else:
            print 'Sorry, compiler %s not supported' % compiler

        #cppflags = '-DPLEV=%i -DIM=%i -DJM=%i -DKM=%i' % (self.KM,self.IM,self.JM,self.KM)
        # only the vertical dimension needs to be set by pre-processor
        cppflags = '-DPLEV=%i' %self.KM

        def getSources(dir, source_file_name='sources_in_order_of_compilation'):
            #Gets list of source files for extensions
            SrcFile = os.path.join(dir, source_file_name)
            if os.path.exists(SrcFile):
                Sources = open(SrcFile).readlines()
                Sources = [os.path.join(dir,s[:-1]) for s in Sources]
            else:
                Sources = []
                w = os.walk(dir)
                for ww in w:
                    if 'ignore' not in ww[0]:
                        for pattern in ['*.f','*.F','*.f90','*.F90']:
                            Sources += glob.glob(os.path.join(ww[0],pattern))
            return Sources

        def buildNeeded(target,src):
            #Checks if source code is newer than extension, so extension needs to be rebuilt
            #  Will need to modify this to check for grid dimensions!
            #target = os.path.join('lib/climt',target)
            if not os.path.exists(target):
                return True
            for file in src:
                if newer(file,target):
                    return True
            print 'Extension %s is up to date' % os.path.basename(target)
            return False

        src = getSources(dir)
        target = '_%s.so' % name
        driver = glob.glob(os.path.join(dir,'Driver.f*'))[0]
        f77flags = '"%s %s"' % (cppflags,f77flags)
        f90flags = '"%s %s"' % (cppflags,f90flags)
        if buildNeeded(target,src):
            print '\n Building %s ... \n' % os.path.basename(target)
            # generate signature file
            if os.path.exists(os.path.join(dir, 'sources_signature_file')):
                src_pyf = getSources(dir, source_file_name='sources_signature_file')
                src_pyf_str = string.join(src_pyf)
            else:
                src_pyf_str = driver
            subprocess.call('f2py --quiet --overwrite-signature %s -m _%s -h _%s.pyf'%(src_pyf_str,name,name), shell=True)

            # compile extension
            F2pyCommand = []
            F2pyCommand.append('f2py -c -m _%s' % name)
            F2pyCommand.append('--fcompiler=%s --noopt' % compiler)
            #F2pyCommand.append('-I%s' % dir)
            #F2pyCommand.append('-I%s' % os.path.join(dir,'include'))
            F2pyCommand.append('-I%s' % os.path.join(dir,'src'))
            #F2pyCommand.append('-I%s' % os.path.join(dir,'src','include'))
            F2pyCommand.append('--f77flags=%s' % f77flags)
            F2pyCommand.append('--f90flags=%s' % f90flags)
            F2pyCommand.append('_%s.pyf' % name)
            F2pyCommand.append('%s' % string.join(src))
            F2pyCommand.append('--quiet')
            F2pyCommand = string.join(F2pyCommand)
            print F2pyCommand
            if subprocess.call(F2pyCommand, shell=True) > 0:
                raise StandardError('+++ Compilation failed')
            subprocess.call('rm -f _%s.pyf' % name, shell=True)

    def _init_extension(self):
        import _cam3_radiation
        # Initialise abs/ems.
        dir = os.path.dirname(__file__) + '/../data/cam3rad'
        AbsEmsDataFile = os.path.join(dir, 'abs_ems_factors_fastvx.c030508.nc')
        #  Open the absorption data file
        data = nc.Dataset(AbsEmsDataFile)
        #  The fortran module that holds the data
        mod = _cam3_radiation.absems
        #  initialize storage arrays
        mod.initialize_radbuffer()
        #  Populate storage arrays with values from netcdf file
        for field in ['ah2onw', 'eh2onw', 'ah2ow', 'ln_ah2ow', 'cn_ah2ow', 'ln_eh2ow', 'cn_eh2ow']:
            setattr(mod, field, data.variables[field][:].T)
        self.extension = _cam3_radiation
