#  Run this script to re-generate the signature file _cam3.pyf
f2py --overwrite-signature src/shr_kind_mod.F90 src/absems.F90 Driver.f90 -m _cam3 -h _cam3.pyf
