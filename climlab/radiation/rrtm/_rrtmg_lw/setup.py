def configuration(parent_package='', top_path=None):
    import os
    from numpy.distutils.misc_util import Configuration
    from numpy.distutils import fcompiler

    # figure out which compiler we're going to use
    compiler = fcompiler.get_default_fcompiler()
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

    sourcelist = ['_rrtmg_lw.pyf',
                  'rrtmg_lw_v4.85/gcm_model/modules/parkind.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_cld.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_con.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_ref.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_tbl.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_vsn.f90',
                  'rrtmg_lw_v4.85/gcm_model/modules/rrlw_wvn.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_k_g.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_taumol.f90',
                  'sourcemods/rrtmg_lw_setcoef.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rtrnmc.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_cldprmc.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/mcica_random_numbers.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90',
                  'Driver.f90']
    config = Configuration(package_name='_rrtmg_lw', parent_name=parent_package, top_path=top_path)
    config.add_extension(name='_rrtmg_lw',
                         sources=sourcelist,
                         extra_f90_compile_args=f90flags,
                         f2py_options=['--quiet'],
                         )
    #  Not currently initializing from nc data file, so there's no reason to include it
    #config.add_data_files(os.path.join('rrtmg_lw_v4.85', 'gcm_model', 'data', 'rrtmg_lw.nc'))
    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(configuration=configuration)
