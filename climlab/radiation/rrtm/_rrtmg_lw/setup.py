def configuration(parent_package='', top_path=None):
    import os
    from numpy.distutils.misc_util import Configuration
    from numpy.distutils import fcompiler

    #  Set default compiler flags
    cppflags = ''
    f77flags = ''
    f90flags = ''
    # figure out which compiler we're going to use
    compiler = fcompiler.get_default_fcompiler()
    # set some fortran compiler-dependent flags (following CliMT code here)
    if compiler == 'gnu95':
        f77flags=['-ffixed-line-length-132', '-fdefault-real-8']
        f90flags=['-fdefault-real-8', '-fno-range-check', '-ffree-form']
        #f90flags=['-fdefault-real-8']
    elif compiler == 'intel' or compiler == 'intelem':
        f77flags='-132 -r8'
        f90flags='-132 -r8'
    print f90flags

    #here = os.path.abspath(os.path.dirname(__file__))
    #sourcelistfile = os.path.join(here, 'sources_in_order_of_compilation')
    #sourcelist = [line.rstrip() for line in open(sourcelistfile)]
    #sourcelist.append('_rrtmg_lw.pyf')
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
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_taumol.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_setcoef.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rtrnmc.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_cldprmc.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/mcica_random_numbers.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90',
                  'rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90',
                  'f90wrap_parrrtm.f90',
                  'f90wrap_rrlw_kg01.f90',
                  'f90wrap_rrlw_kg02.f90',
                  'f90wrap_rrlw_kg03.f90',
                  'f90wrap_rrlw_kg04.f90',
                  'f90wrap_rrlw_kg05.f90',
                  'f90wrap_rrlw_kg06.f90',
                  'f90wrap_rrlw_kg07.f90',
                  'f90wrap_rrlw_kg08.f90',
                  'f90wrap_rrlw_kg09.f90',
                  'f90wrap_rrlw_kg10.f90',
                  'f90wrap_rrlw_kg11.f90',
                  'f90wrap_rrlw_kg12.f90',
                  'f90wrap_rrlw_kg13.f90',
                  'f90wrap_rrlw_kg14.f90',
                  'f90wrap_rrlw_kg15.f90',
                  'f90wrap_rrlw_kg16.f90',
                  'f90wrap_rrlw_ncpar.f90',
                  'f90wrap_mcica_subcol_gen_lw.f90',
                  'f90wrap_rrtmg_lw_init.f90',
                  'f90wrap_rrtmg_lw_rad.f90']

    config = Configuration(package_name='_rrtmg_lw', parent_name=parent_package, top_path=top_path)
    config.add_extension(name='_rrtmg_lw',
                         sources=sourcelist,
                         extra_f90_compile_args=f90flags,
                         #f2py_options=['-c',],
                         )
    config.add_data_files(os.path.join('rrtmg_lw_v4.85', 'gcm_model', 'data', 'rrtmg_lw.nc'))
    return config

if __name__ == '__main__':
    from numpy.distutils.core import setup
    setup(configuration=configuration)
