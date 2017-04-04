from os.path import join


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
        elif compiler == 'intel' or compiler == 'intelem':
            f90flags.append('-132')
        #  Need zero-level optimization to avoid build problems with rrtmg_lw_k_g.f90
        f90flags.append('-O0')
        #  Suppress all compiler warnings (avoid huge CI log files)
        f90flags.append('-w')
    except CompilerNotFound:
        print 'No Fortran compiler found, not building the RRTMG_LW radiation module!'
        build = False

    config = Configuration(package_name='_rrtmg_lw', parent_name=parent_package, top_path=top_path)
    if build:
        config.add_extension(name='_rrtmg_lw',
                             sources=[rrtmg_lw_gen_source],
                             extra_f90_compile_args=f90flags,
                             f2py_options=['--quiet'],
                             )
    #  Not currently initializing from nc data file, so there's no reason to include it
    #config.add_data_files(join('rrtmg_lw_v4.85', 'gcm_model', 'data', 'rrtmg_lw.nc'))
    return config

def rrtmg_lw_gen_source(ext, build_dir):
    '''Add RRTMG_LW fortran source if Fortran 90 compiler available,
    if no compiler is found do not try to build the extension.'''
    #  Fortran 90 sources in order of compilation
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
    sourcelist = []
    sourcelist.append(join(config.local_path,'_rrtmg_lw.pyf'))
    for item in modules:
        sourcelist.append(join(config.local_path,'rrtmg_lw_v4.85','gcm_model','modules',item))
    for item in src:
        if item == 'rrtmg_lw_setcoef.f90':
            sourcelist.append(join(config.local_path,'sourcemods',item))
        else:
            sourcelist.append(join(config.local_path,'rrtmg_lw_v4.85','gcm_model','src',item))
    sourcelist.append(join(config.local_path,'Driver.f90'))
    try:
        config.have_f90c()
        return sourcelist
    except:
        print 'No Fortran 90 compiler found, not building RRTMG_LW extension!'
        return None

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(configuration=configuration)
