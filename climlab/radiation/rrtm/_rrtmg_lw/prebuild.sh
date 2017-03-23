f90wrap -k .f2py_f2cmap -m rrtmg_lw rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90 rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90 rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_init.f90 rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90
f2py --overwrite-signature f90wrap_parrrtm.f90 f90wrap_mcica_subcol_gen_lw.f90 f90wrap_rrtmg_lw_init.f90 f90wrap_rrtmg_lw_rad.f90 -m _rrtmg_lw -h _rrtmg_lw.pyf
