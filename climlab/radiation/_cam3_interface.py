'''Interface between climlab and the CAM3 radiation code.'''

import os, sys, subprocess, glob, string
from numpy.distutils import fcompiler
from distutils.dep_util import newer
import netCDF4 as nc


ToExtension = ['do_sw','do_lw','p','dp','ps','Tatm','Ts','q','o3','cldf','clwp','ciwp',
                               'in_cld','aldif','aldir','asdif','asdir','zen','solin','flus','r_liq','r_ice',
                               'co2','n2o','ch4','cfc11','cfc12','g','Cpd','epsilon','stebol']
                               
FromExtension = ['TdotRad','SrfRadFlx','swhr','lwhr','swflx','lwflx','SwToaCf',
                               'SwSrfCf','LwToaCf','LwSrfCf','LwToa','LwSrf','SwToa','SwSrf','lwuflx','lwdflx']


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


def buildNeeded(src, KM):
    #Checks if source code is newer than extension, so extension needs to be rebuilt
    try:
        import _cam3_radiation
    except:
        return True
    #  Rebuild if number of vertical levels has changed.
    if (_cam3_radiation.get_nlev() != KM):
        return True
    for file in src:
        if newer(file, '_cam3_radiation.so'):
            return True
    print 'Extension _cam3_radiation is up to date'
    return False


def _build_extension(KM):
    name = 'cam3_radiation'
    #  Temporarily move to the source directory
    here = os.getcwd()
    pydir = os.path.dirname(__file__)
    srcdir = pydir + '/../../src/radiation/cam3'
    os.chdir(srcdir)
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
    cppflags = '-DPLEV=%i' %KM

    src = getSources(srcdir)
    target = '_%s.so' % name
    #target = '_cam3_radiation.so'
    driver = glob.glob(os.path.join(srcdir,'Driver.f*'))[0]
    f77flags = '"%s %s"' % (cppflags,f77flags)
    f90flags = '"%s %s"' % (cppflags,f90flags)
    if buildNeeded(src, KM):
        print '\n Building %s ... \n' % os.path.basename(target)
        # generate signature file
        if os.path.exists(os.path.join(srcdir, 'sources_signature_file')):
            src_pyf = getSources(srcdir, source_file_name='sources_signature_file')
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
        F2pyCommand.append('-I%s' % os.path.join(srcdir,'src'))
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
        # delete signature file
        subprocess.call('rm -f _%s.pyf' % name, shell=True)
        # Move shared object file to pydir
        subprocess.call('mv %s %s'%(target,pydir), shell=True)
    #  Switch back to original working directory
    os.chdir(here)


def _init_extension(module):
    import _cam3_radiation
    # Initialise abs/ems.
    datadir = os.path.dirname(__file__) + '/../data/cam3rad'
    AbsEmsDataFile = os.path.join(datadir, 'abs_ems_factors_fastvx.c030508.nc')
    #  Open the absorption data file
    data = nc.Dataset(AbsEmsDataFile)
    #  The fortran module that holds the data
    mod = _cam3_radiation.absems
    #  initialize storage arrays
    mod.initialize_radbuffer()
    #  Populate storage arrays with values from netcdf file
    for field in ['ah2onw', 'eh2onw', 'ah2ow', 'ln_ah2ow', 'cn_ah2ow', 'ln_eh2ow', 'cn_eh2ow']:
        setattr(mod, field, data.variables[field][:].T)
    module.extension = _cam3_radiation
