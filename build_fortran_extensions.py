'''
Script to manually build signature files and .so extensions
for the fortran components
(currently CAM3, RRTMG_LW and RRTGM_SW)

Currently this is called automatically when you run
```
python setup.py install
```

Hopefully in the future we will get the build procedure fully automated
with setuptools
'''
import os, sys, subprocess, string
from numpy.distutils import fcompiler

def _get_sources(dir, source_file_name='sources_in_order_of_compilation'):
    '''Gets list of source files for extensions'''
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

def _build_extension(name=None, srcdir=None, targetdir=None,
        cppflags='', f77flags='', f90flags='', lib='', libdir='', incdir=''):
    #  current working directory
    here = os.getcwd()
    #  need to change to src dir so f2py sees the .f2py_f2cmap file
    os.chdir(srcdir)
    #  Get list of sources
    src = _get_sources(srcdir)
    print src
    target = '{}.so'.format(name)
    #driver = glob.glob(os.path.join(srcdir,'Driver.f*'))[0]
    f77flags = '"{} {}"'.format(cppflags,f77flags)
    f90flags = '"{} {}"'.format(cppflags,f90flags)
    print '\n Building {} ... \n'.format(os.path.basename(target))
    # generate signature file
    if os.path.exists(os.path.join(srcdir, 'sources_signature_file')):
        src_pyf = _get_sources(srcdir, source_file_name='sources_signature_file')
        src_pyf_str = string.join(src_pyf)
    else:
        src_pyf_str = 'Driver.f90'
    subprocess.call('f2py --quiet --overwrite-signature {} -m {} -h {}.pyf'.format(src_pyf_str,name,name), shell=True)
    # compile extension
    F2pyCommand = []
    F2pyCommand.append('f2py -c -m {}'.format(name))
    F2pyCommand.append('--fcompiler={} --noopt'.format(compiler))
    F2pyCommand.append('-I{}'.format(srcdir))
    #F2pyCommand.append('-I{}'.format(os.path.join(srcdir,'include')))
    F2pyCommand.append('-I{}'.format(os.path.join(srcdir,'src')))
    #F2pyCommand.append('-I{}'.format(os.path.join(srcdir,'src','include')))
    F2pyCommand.append('--f77flags={}'.format(f77flags))
    F2pyCommand.append('--f90flags={}'.format(f90flags))
    F2pyCommand.append('{}.pyf'.format(name))
    F2pyCommand.append('{}'.format(string.join(src)))
    F2pyCommand.append('--quiet')
    F2pyCommand = string.join(F2pyCommand)
    print F2pyCommand
    if subprocess.call(F2pyCommand, shell=True) > 0:
        raise StandardError('+++ Compilation failed')
    # delete signature file
    #os.remove('{}.pyf'.format(name))
    #  No, actually leave the signature file in place
    # Move shared object file to target directory
    print targetdir
    os.rename(target, os.path.join(targetdir, target))
    #  Switch back to original working directory
    os.chdir(here)

###  Begin climlab install script  ###

#  Root directory of climlab code
climlab_root = os.path.abspath(os.path.dirname(__file__))
#  List of all fortran extensions to be compiled
Extensions = [
    {'name': '_cam3',
     'srcdir': os.path.join('climlab','radiation','cam3'),
     'targetdir': os.path.join('climlab','radiation','cam3')},
    #{'name': '_rrtmg_lw',
    # 'srcdir': os.path.join('climlab','radiation','rrtm','_rrtmg_lw'),
    # 'targetdir': os.path.join('climlab','radiation','rrtm','_rrtmg_lw')},
    #{'name': '_rrtmg_sw',
    # 'srcdir': os.path.join('climlab','radiation','rrtm','_rrtmg_sw'),
    # 'targetdir': os.path.join('climlab','radiation','rrtm','_rrtmg_sw')}

    ]

#  Set default compiler flags
cppflags = ''
f77flags = ''
f90flags = ''
# figure out which compiler we're going to use
try:
    compiler = fcompiler.get_default_fcompiler()
except:
    compiler = None
    print 'Something went wrong with finding a Fortran compiler.'
# set some fortran compiler-dependent flags (following CliMT code here)
if compiler == 'gnu95':
    # The flag -Wl,-rpath,$(CONDA_PREFIX)/lib
    # makes sure that the gfortran runtime libraries are linked properly
    # see https://github.com/ContinuumIO/anaconda-issues/issues/739#issuecomment-238076905
    f77flags='-ffixed-line-length-132 -fdefault-real-8 -Wl,-rpath,$(CONDA_PREFIX)/lib'
    f90flags='-fdefault-real-8 -fno-range-check -ffree-form -Wl,-rpath,$(CONDA_PREFIX)/lib'
elif compiler == 'intel' or compiler == 'intelem':
    f77flags='-132 -r8'
    f90flags='-132 -r8'
#  Cannot test ibm compiler
#elif compiler == 'ibm':
#    f77flags='-qautodbl=dbl4 -qsuffix=f=f:cpp=F -qfixed=132'
#    f90flags='-qautodbl=dbl4 -qsuffix=f=f90:cpp=F90 -qfree=f90'
else:
    print 'Compiler {} not supported, proceed at your own risk!'.format(compiler)

# Build all extensions
here = os.getcwd()
for ext in Extensions:
    try:
        # relative to absolute path
        ext['srcdir'] = os.path.join(climlab_root, ext['srcdir'])
        ext['targetdir'] = os.path.join(climlab_root, ext['targetdir'])
        _build_extension(cppflags=cppflags,
                         f77flags=f77flags,
                         f90flags=f90flags,
                         **ext)
    except:
        print 'Something went wrong building Fortran extension:'
        print ext['name']
        #  Make sure we are back to original working directory
        os.chdir(here)
