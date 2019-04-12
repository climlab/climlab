from __future__ import print_function
from os.path import join, abspath


def configuration(parent_package='', top_path=None):
    global config
    from numpy.distutils.misc_util import Configuration
    from numpy.distutils.fcompiler import get_default_fcompiler, CompilerNotFound

    build = True
    try:
        # figure out which compiler we're going to use
        compiler = get_default_fcompiler()
        # set some fortran compiler-dependent flags
        f90flags = []
        if compiler == 'gnu95':
            f90flags.append('-fdefault-real-8')
        elif compiler == 'intel' or compiler == 'intelem':
            f90flags.append('-132')
            f90flags.append('-r8')
        #  Set aggressive optimization level
        f90flags.append('-O3')
        #  Suppress all compiler warnings (avoid huge CI log files)
        f90flags.append('-w')
    except CompilerNotFound:
        print('No Fortran compiler found, not building the CAM3 radiation module!')
        build = False

    config = Configuration(package_name='cam3', parent_name=parent_package, top_path=top_path)
    config.add_data_files(join('data', 'README'))
    if build:
        config.add_extension(name='_cam3',
                         sources=[cam3_gen_source],
                         extra_f90_compile_args=f90flags,
                         f2py_options=['--quiet'])
        config.add_include_dirs('src')
    return config

def cam3_gen_source(ext, build_dir):
    '''Add CAM3 fortran source if Fortran 90 compiler available,
    if no compiler is found do not try to build the extension.'''
    #  Fortran 90 sources in order of compilation
    fort90source = ['pmgrid.F90',
                'prescribed_aerosols.F90',
                'shr_kind_mod.F90',
                'quicksort.F90',
                'abortutils.F90',
                'absems.F90',
                'wv_saturation.F90',
                'aer_optics.F90',
                'cmparray_mod.F90',
                'shr_const_mod.F90',
                'physconst.F90',
                'pkg_cldoptics.F90',
                'gffgch.F90',
                'chem_surfvals.F90',
                'volcrad.F90',
                'radae.F90',
                'radlw.F90',
                'radsw.F90',
                'crm.F90',]
    #thispath = abspath(config.local_path)
    thispath = config.local_path
    sourcelist = []
    sourcelist.append(join(thispath,'_cam3.pyf'))
    for item in fort90source:
        sourcelist.append(join(thispath, 'src', item))
    sourcelist.append(join(thispath,'Driver.f90'))
    try:
        config.have_f90c()
        return sourcelist
    except:
        print('No Fortran 90 compiler found, not building CAM3 extension!')
        return None

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(configuration=configuration)
