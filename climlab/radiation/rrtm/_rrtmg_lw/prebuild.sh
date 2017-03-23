f90wrap -k .f2py_f2cmap -m parrrtm rrtmg_lw_v4.85/gcm_model/modules/parrrtm.f90
f90wrap -k .f2py_f2cmap -m mcica_subcol_gen_lw rrtmg_lw_v4.85/gcm_model/src/mcica_subcol_gen_lw.f90
f90wrap -k .f2py_f2cmap -m rrtmg_lw_rad rrtmg_lw_v4.85/gcm_model/src/rrtmg_lw_rad.f90
f2py --overwrite-signature f90wrap_mcica_subcol_gen_lw.f90 -m _mcica_subcol_gen_lw -h _mcica_subcol_gen_lw.pyf
f2py --overwrite-signature f90wrap_rrtmg_lw_rad.f90 -m _rrtmg_lw_rad -h _rrtmg_lw_rad.pyf
