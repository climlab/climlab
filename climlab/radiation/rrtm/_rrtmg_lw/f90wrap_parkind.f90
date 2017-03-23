! Module parkind defined in file rrtmg_lw_v4.85/gcm_model/modules/parkind.f90

subroutine f90wrap_parkind__get__kind_ib(f90wrap_kind_ib)
    use parkind, only: parkind_kind_ib => kind_ib
    implicit none
    integer, intent(out) :: f90wrap_kind_ib
    
    f90wrap_kind_ib = parkind_kind_ib
end subroutine f90wrap_parkind__get__kind_ib

subroutine f90wrap_parkind__get__kind_im(f90wrap_kind_im)
    use parkind, only: parkind_kind_im => kind_im
    implicit none
    integer, intent(out) :: f90wrap_kind_im
    
    f90wrap_kind_im = parkind_kind_im
end subroutine f90wrap_parkind__get__kind_im

subroutine f90wrap_parkind__get__kind_in(f90wrap_kind_in)
    use parkind, only: parkind_kind_in => kind_in
    implicit none
    integer, intent(out) :: f90wrap_kind_in
    
    f90wrap_kind_in = parkind_kind_in
end subroutine f90wrap_parkind__get__kind_in

subroutine f90wrap_parkind__get__kind_rb(f90wrap_kind_rb)
    use parkind, only: parkind_kind_rb => kind_rb
    implicit none
    integer, intent(out) :: f90wrap_kind_rb
    
    f90wrap_kind_rb = parkind_kind_rb
end subroutine f90wrap_parkind__get__kind_rb

subroutine f90wrap_parkind__get__kind_rm(f90wrap_kind_rm)
    use parkind, only: parkind_kind_rm => kind_rm
    implicit none
    integer, intent(out) :: f90wrap_kind_rm
    
    f90wrap_kind_rm = parkind_kind_rm
end subroutine f90wrap_parkind__get__kind_rm

subroutine f90wrap_parkind__get__kind_rn(f90wrap_kind_rn)
    use parkind, only: parkind_kind_rn => kind_rn
    implicit none
    integer, intent(out) :: f90wrap_kind_rn
    
    f90wrap_kind_rn = parkind_kind_rn
end subroutine f90wrap_parkind__get__kind_rn

! End of module parkind defined in file rrtmg_lw_v4.85/gcm_model/modules/parkind.f90

