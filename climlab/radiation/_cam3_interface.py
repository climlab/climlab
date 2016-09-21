'''Interface between climlab and the CAM3 radiation code.'''

import os, sys, subprocess, glob, string, importlib
from numpy.distutils import fcompiler
from distutils.dep_util import newer
import netCDF4 as nc


ToExtension = ['do_sw','do_lw','p','dp','ps','Tatm','Ts','q','O3','cldf','clwp',
               'ciwp', 'in_cld','aldif','aldir','asdif','asdir','cosZen',
               'insolation','flus','r_liq','r_ice','CO2','N2O','CH4','CFC11',
               'CFC12','g','Cpd','epsilon','stebol']

FromExtension = ['TdotRad','SrfRadFlx','swhr','lwhr','swflx','lwflx','SwToaCf',
                 'SwSrfCf','LwToaCf','LwSrfCf','LwToa','LwSrf','SwToa','SwSrf',
                 'lwuflx','lwdflx']

def _modify_extname(extname):
    '''Check to see if the supplied extension name already exists.
    If so, append _n to the extension name, where n is the first available
    integer.
    This avoids all kinds of unstable behavior when an extension with the same
    name has already been imported in a current Python session.'''
    filelist = glob.glob(extname + '*.so')
    if filelist:  # only executes in list is not empty
        # start from 0 and count up until we find a name
        #  extname_n.so that doesn't exist yet, and use that.
        done = False
        n = 0
        while not done:
            modname = extname + '_' + str(n)
            if modname + '.so' not in filelist:
                done = True
            n += 1
        extname = modname
    return extname


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


def buildNeeded(src, KM, extname='_cam3_radiation'):
    '''Check extension needs to be rebuilt.
    Returns True if any of the following are True:
        - the shared object [extname].so cannot be imported
        - the shared object does not have the correct number of vertical levels
        - any of the Fortran source code is newer than the shared object.

    Currently ... we are renaming the extension every time, so this should
    always return True -- the extension will be rebuilt every time.
    '''
    try:
        _cam3_radiation = importlib.import_module(extname)
    except:
        return True
    rebuild = False
    #  Rebuild if number of vertical levels has changed.
    if (_cam3_radiation.get_nlev() != KM):
        rebuild = True
    # or if source code has changed
    for file in src:
        if newer(file, extname +'.so'):
            rebuild=True
    return rebuild


def _build_extension(KM,JM,IM, extname='_cam3_radiation'):
    #name = 'cam3_radiation'
    name = extname
    #  current working directory
    here = os.getcwd()
    #  directory where this file lives
    pydir = os.path.dirname(os.path.abspath(__file__))
    #  where the fortran source lives
    srcdir = os.path.join(pydir, 'src', 'cam3')
    src = getSources(srcdir)

    if buildNeeded(src, KM, name):
    #if True:
        #  need to change to srcdir so f2py sees the .f2py_f2cmap file
        os.chdir(srcdir)
        # figure out which compiler we're going to use
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

        #cppflags = '-DPLEV=%i -DIM=%i -DJM=%i -DKM=%i' % (KM,IM,JM,KM)
        # only the vertical dimension needs to be set by pre-processor
        cppflags = '-DPLEV=%i' %KM

        target = '%s.so' % name
        driver = glob.glob(os.path.join(srcdir,'Driver.f*'))[0]
        f77flags = '"%s %s"' % (cppflags,f77flags)
        f90flags = '"%s %s"' % (cppflags,f90flags)
        print '\n Building %s ... \n' % os.path.basename(target)
        # generate signature file
        if os.path.exists(os.path.join(srcdir, 'sources_signature_file')):
            src_pyf = getSources(srcdir, source_file_name='sources_signature_file')
            src_pyf_str = string.join(src_pyf)
        else:
            src_pyf_str = driver
        subprocess.call('f2py --quiet --overwrite-signature %s -m %s -h %s.pyf'%(src_pyf_str,name,name), shell=True)
        # compile extension
        F2pyCommand = []
        F2pyCommand.append('f2py -c -m %s' % name)
        F2pyCommand.append('--fcompiler=%s --noopt' % compiler)
        #F2pyCommand.append('-I%s' % dir)
        #F2pyCommand.append('-I%s' % os.path.join(dir,'include'))
        F2pyCommand.append('-I%s' % os.path.join(srcdir,'src'))
        #F2pyCommand.append('-I%s' % os.path.join(dir,'src','include'))
        F2pyCommand.append('--f77flags=%s' % f77flags)
        F2pyCommand.append('--f90flags=%s' % f90flags)
        F2pyCommand.append('%s.pyf' % name)
        F2pyCommand.append('%s' % string.join(src))
        F2pyCommand.append('--quiet')
        F2pyCommand = string.join(F2pyCommand)
        print F2pyCommand
        if subprocess.call(F2pyCommand, shell=True) > 0:
            raise StandardError('+++ Compilation failed')
        # Check to see if we are using Mac OSX
        #  which might need this hack to make the linking work if using
        #  gcc compilers installed with conda
        if sys.platform == 'darwin':
            _patch_extension_rpath(target)
        # delete signature file
        subprocess.call('rm -f %s.pyf' % name, shell=True)
        # Move shared object file to working directory
        subprocess.call('mv %s %s'%(target, here), shell=True)
        #  Switch back to original working directory
        os.chdir(here)
    else:
        print 'Extension %s is up to date' %name

def _patch_extension_rpath(extname, verbose=False):
        ####   This is a little hack to get the build to work on OSX
        ####   with gcc compilers installed by conda
        ####   see https://github.com/ContinuumIO/anaconda-issues/issues/739#issuecomment-238076905
    patch_rpath = []
    #patch_rpath.append('install_name_tool -change @rpath/./libgfortran.3.dylib ${CONDA_PREFIX}/lib/libgfortran.3.dylib -change @rpath/./libquadmath.0.dylib ${CONDA_PREFIX}/lib/libquadmath.0.dylib')
    patch_rpath.append('install_name_tool -change @rpath/./libgfortran.3.dylib ${CONDA_PREFIX}/lib/libgfortran.3.dylib')
    patch_rpath.append('-change @rpath/./libquadmath.0.dylib ${CONDA_PREFIX}/lib/libquadmath.0.dylib %s' %extname)
    patch_rpath = string.join(patch_rpath)
    if verbose:
        print patch_rpath
    if subprocess.call(patch_rpath, shell=True) > 0:
        raise StandardError('+++ rpath change failed')
    if verbose:
        print 'rpath patch successful'

def _init_extension(module, extname='_cam3_radiation'):
    #  Import the extension module using the supplied name
    _cam3_radiation = importlib.import_module(extname)
    # Initialise abs/ems.
    datadir = os.path.abspath(os.path.dirname(__file__) + '/../data/cam3rad')
    #datadir = os.path.dirname(__file__) + '/../data/cam3rad'
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
