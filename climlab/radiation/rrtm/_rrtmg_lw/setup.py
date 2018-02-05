from __future__ import print_function
from os.path import join, abspath

modules = ['parkind.f90',
           'parrrtm.f90',
           'rrlw_cld.f90',
           'rrlw_con.f90',
           'rrlw_kg01.f90',
           'rrlw_kg02.f90',
           'rrlw_kg03.f90',
           'rrlw_kg04.f90',
           'rrlw_kg05.f90',
           'rrlw_kg06.f90',
           'rrlw_kg07.f90',
           'rrlw_kg08.f90',
           'rrlw_kg09.f90',
           'rrlw_kg10.f90',
           'rrlw_kg11.f90',
           'rrlw_kg12.f90',
           'rrlw_kg13.f90',
           'rrlw_kg14.f90',
           'rrlw_kg15.f90',
           'rrlw_kg16.f90',
           'rrlw_ncpar.f90',
           'rrlw_ref.f90',
           'rrlw_tbl.f90',
           'rrlw_vsn.f90',
           'rrlw_wvn.f90',]
src = ['rrtmg_lw_k_g.f90',
       'rrtmg_lw_taumol.f90',
       'rrtmg_lw_setcoef.f90',
       'rrtmg_lw_rtrnmc.f90',
       'rrtmg_lw_cldprmc.f90',
       'mcica_random_numbers.f90',
       'mcica_subcol_gen_lw.f90',
       'rrtmg_lw_init.f90',
       'rrtmg_lw_rad.f90',]
unoptimized_src = ['rrtmg_lw_k_g.f90']
mod_src = ['rrtmg_lw_setcoef.f90',]

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
            f90flags.append('-fno-range-check')
            f90flags.append('-ffree-form')
            f90flags.append('-fPIC')
        elif compiler == 'intel' or compiler == 'intelem':
            f90flags.append('-132')
        #  Need zero-level optimization to avoid build problems with rrtmg_lw_k_g.f90
        #f90flags.append('-O2')
        #  Suppress all compiler warnings (avoid huge CI log files)
        f90flags.append('-w')
    except CompilerNotFound:
        print('No Fortran compiler found, not building the RRTMG_LW radiation module!')
        build = False

    config = Configuration(package_name='_rrtmg_lw', parent_name=parent_package, top_path=top_path)
    if build:
        link_args_list = build_extra_objects(f90flags)
        config.add_extension(name='_rrtmg_lw',
                             sources=[rrtmg_lw_gen_source],
                             extra_f90_compile_args=f90flags + ['-O3'],
                             f2py_options=['--quiet'],
                             extra_link_args=link_args_list,
                             )
    return config

def build_extra_objects(f90flags):
    import subprocess, glob
    thispath = config.local_path
    object_file_list = []
    #  Compile all source to object .o files
    gfortran_call = ['gfortran', '-c', ] + f90flags
    try:
        for item in modules:
            optflag = '-O3'
            output_file = item[:-3]+'o'
            object_file_list.append(output_file)
            fullname = join(thispath,'rrtmg_lw_v4.85','gcm_model','modules',item)
            subprocess.call(gfortran_call + [fullname] + [optflag] + ['-o'] + [output_file])
        for item in src:
            if item in mod_src:
                fullname = join(thispath,'sourcemods',item)
            else:
                fullname = join(thispath,'rrtmg_lw_v4.85','gcm_model','src',item)
            if item in unoptimized_src:
                optflag = '-O0'
            else:
                optflag = '-O3'
            output_file = item[:-3]+'o'
            object_file_list.append(output_file)
            subprocess.call(gfortran_call + [fullname] + [optflag] + ['-o'] + [output_file])
        return object_file_list
    except:
        print('There was a problem with compiling rrtmg_sw objects.')
        return None

def rrtmg_lw_gen_source(ext, build_dir):
    '''Add RRTMG_LW fortran source if Fortran 90 compiler available,
    if no compiler is found do not try to build the extension.'''
    thispath = config.local_path
    sourcelist = [join(thispath,'_rrtmg_lw.pyf'),
                  join(thispath,'Driver.f90')]
    try:
        config.have_f90c()
        return sourcelist
    except:
        print('No Fortran 90 compiler found, not building RRTMG_LW extension!')
        return None

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(configuration=configuration)
