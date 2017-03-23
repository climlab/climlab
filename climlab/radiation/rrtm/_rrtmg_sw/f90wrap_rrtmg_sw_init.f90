! Module rrtmg_sw_init defined in file rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90

subroutine f90wrap_rrtmg_sw_ini(cpdair)
    use rrtmg_sw_init, only: rrtmg_sw_ini
    implicit none
    
    real(8), intent(in) :: cpdair
    call rrtmg_sw_ini(cpdair=cpdair)
end subroutine f90wrap_rrtmg_sw_ini

subroutine f90wrap_swdatinit(cpdair)
    use rrtmg_sw_init, only: swdatinit
    implicit none
    
    real(8), intent(in) :: cpdair
    call swdatinit(cpdair=cpdair)
end subroutine f90wrap_swdatinit

subroutine f90wrap_swcmbdat
    use rrtmg_sw_init, only: swcmbdat
    implicit none
    
    call swcmbdat()
end subroutine f90wrap_swcmbdat

subroutine f90wrap_swaerpr
    use rrtmg_sw_init, only: swaerpr
    implicit none
    
    call swaerpr()
end subroutine f90wrap_swaerpr

subroutine f90wrap_cmbgb16s
    use rrtmg_sw_init, only: cmbgb16s
    implicit none
    
    call cmbgb16s()
end subroutine f90wrap_cmbgb16s

subroutine f90wrap_cmbgb17
    use rrtmg_sw_init, only: cmbgb17
    implicit none
    
    call cmbgb17()
end subroutine f90wrap_cmbgb17

subroutine f90wrap_cmbgb18
    use rrtmg_sw_init, only: cmbgb18
    implicit none
    
    call cmbgb18()
end subroutine f90wrap_cmbgb18

subroutine f90wrap_cmbgb19
    use rrtmg_sw_init, only: cmbgb19
    implicit none
    
    call cmbgb19()
end subroutine f90wrap_cmbgb19

subroutine f90wrap_cmbgb20
    use rrtmg_sw_init, only: cmbgb20
    implicit none
    
    call cmbgb20()
end subroutine f90wrap_cmbgb20

subroutine f90wrap_cmbgb21
    use rrtmg_sw_init, only: cmbgb21
    implicit none
    
    call cmbgb21()
end subroutine f90wrap_cmbgb21

subroutine f90wrap_cmbgb22
    use rrtmg_sw_init, only: cmbgb22
    implicit none
    
    call cmbgb22()
end subroutine f90wrap_cmbgb22

subroutine f90wrap_cmbgb23
    use rrtmg_sw_init, only: cmbgb23
    implicit none
    
    call cmbgb23()
end subroutine f90wrap_cmbgb23

subroutine f90wrap_cmbgb24
    use rrtmg_sw_init, only: cmbgb24
    implicit none
    
    call cmbgb24()
end subroutine f90wrap_cmbgb24

subroutine f90wrap_cmbgb25
    use rrtmg_sw_init, only: cmbgb25
    implicit none
    
    call cmbgb25()
end subroutine f90wrap_cmbgb25

subroutine f90wrap_cmbgb26
    use rrtmg_sw_init, only: cmbgb26
    implicit none
    
    call cmbgb26()
end subroutine f90wrap_cmbgb26

subroutine f90wrap_cmbgb27
    use rrtmg_sw_init, only: cmbgb27
    implicit none
    
    call cmbgb27()
end subroutine f90wrap_cmbgb27

subroutine f90wrap_cmbgb28
    use rrtmg_sw_init, only: cmbgb28
    implicit none
    
    call cmbgb28()
end subroutine f90wrap_cmbgb28

subroutine f90wrap_cmbgb29
    use rrtmg_sw_init, only: cmbgb29
    implicit none
    
    call cmbgb29()
end subroutine f90wrap_cmbgb29

subroutine f90wrap_swcldpr
    use rrtmg_sw_init, only: swcldpr
    implicit none
    
    call swcldpr()
end subroutine f90wrap_swcldpr

! End of module rrtmg_sw_init defined in file rrtmg_sw_v4.0/gcm_model/src/rrtmg_sw_init.f90

