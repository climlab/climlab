! Module rrlw_ncpar defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90

subroutine f90wrap_getabsorberindex(absorbername, absorberindex)
    use rrlw_ncpar, only: getabsorberindex
    implicit none
    
    character(1024), intent(in) :: absorbername
    integer(4), intent(out) :: absorberindex
    call getabsorberindex(AbsorberName=absorbername, AbsorberIndex=absorberindex)
end subroutine f90wrap_getabsorberindex

subroutine f90wrap_rrlw_ncpar__get__cpdair(f90wrap_cpdair)
    use rrlw_ncpar, only: rrlw_ncpar_cpdair => cpdair
    implicit none
    real(8), intent(out) :: f90wrap_cpdair
    
    f90wrap_cpdair = rrlw_ncpar_cpdair
end subroutine f90wrap_rrlw_ncpar__get__cpdair

subroutine &
    f90wrap_rrlw_ncpar__get__maxabsorbernamelength(f90wrap_maxabsorbernamelength)
    use rrlw_ncpar, only: rrlw_ncpar_maxabsorbernamelength => maxabsorbernamelength
    implicit none
    integer(4), intent(out) :: f90wrap_maxabsorbernamelength
    
    f90wrap_maxabsorbernamelength = rrlw_ncpar_maxabsorbernamelength
end subroutine f90wrap_rrlw_ncpar__get__maxabsorbernamelength

subroutine f90wrap_rrlw_ncpar__get__absorber(f90wrap_absorber)
    use rrlw_ncpar, only: rrlw_ncpar_absorber => absorber
    implicit none
    integer(4), intent(out) :: f90wrap_absorber
    
    f90wrap_absorber = rrlw_ncpar_absorber
end subroutine f90wrap_rrlw_ncpar__get__absorber

subroutine f90wrap_rrlw_ncpar__array__status(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_ncpar, only: rrlw_ncpar_status => status
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 5
    dshape(1:1) = shape(rrlw_ncpar_status)
    dloc = loc(rrlw_ncpar_status)
end subroutine f90wrap_rrlw_ncpar__array__status

subroutine f90wrap_rrlw_ncpar__get__i(f90wrap_i)
    use rrlw_ncpar, only: rrlw_ncpar_i => i
    implicit none
    integer(4), intent(out) :: f90wrap_i
    
    f90wrap_i = rrlw_ncpar_i
end subroutine f90wrap_rrlw_ncpar__get__i

subroutine f90wrap_rrlw_ncpar__set__i(f90wrap_i)
    use rrlw_ncpar, only: rrlw_ncpar_i => i
    implicit none
    integer(4), intent(in) :: f90wrap_i
    
    rrlw_ncpar_i = f90wrap_i
end subroutine f90wrap_rrlw_ncpar__set__i

subroutine f90wrap_rrlw_ncpar__get__keylower(f90wrap_keylower)
    use rrlw_ncpar, only: rrlw_ncpar_keylower => keylower
    implicit none
    integer(4), intent(out) :: f90wrap_keylower
    
    f90wrap_keylower = rrlw_ncpar_keylower
end subroutine f90wrap_rrlw_ncpar__get__keylower

subroutine f90wrap_rrlw_ncpar__get__keyupper(f90wrap_keyupper)
    use rrlw_ncpar, only: rrlw_ncpar_keyupper => keyupper
    implicit none
    integer(4), intent(out) :: f90wrap_keyupper
    
    f90wrap_keyupper = rrlw_ncpar_keyupper
end subroutine f90wrap_rrlw_ncpar__get__keyupper

subroutine f90wrap_rrlw_ncpar__get__tdiff(f90wrap_tdiff)
    use rrlw_ncpar, only: rrlw_ncpar_tdiff => tdiff
    implicit none
    integer(4), intent(out) :: f90wrap_tdiff
    
    f90wrap_tdiff = rrlw_ncpar_tdiff
end subroutine f90wrap_rrlw_ncpar__get__tdiff

subroutine f90wrap_rrlw_ncpar__get__ps(f90wrap_ps)
    use rrlw_ncpar, only: rrlw_ncpar_ps => ps
    implicit none
    integer(4), intent(out) :: f90wrap_ps
    
    f90wrap_ps = rrlw_ncpar_ps
end subroutine f90wrap_rrlw_ncpar__get__ps

subroutine f90wrap_rrlw_ncpar__get__plower(f90wrap_plower)
    use rrlw_ncpar, only: rrlw_ncpar_plower => plower
    implicit none
    integer(4), intent(out) :: f90wrap_plower
    
    f90wrap_plower = rrlw_ncpar_plower
end subroutine f90wrap_rrlw_ncpar__get__plower

subroutine f90wrap_rrlw_ncpar__get__pupper(f90wrap_pupper)
    use rrlw_ncpar, only: rrlw_ncpar_pupper => pupper
    implicit none
    integer(4), intent(out) :: f90wrap_pupper
    
    f90wrap_pupper = rrlw_ncpar_pupper
end subroutine f90wrap_rrlw_ncpar__get__pupper

subroutine f90wrap_rrlw_ncpar__get__tself(f90wrap_tself)
    use rrlw_ncpar, only: rrlw_ncpar_tself => tself
    implicit none
    integer(4), intent(out) :: f90wrap_tself
    
    f90wrap_tself = rrlw_ncpar_tself
end subroutine f90wrap_rrlw_ncpar__get__tself

subroutine f90wrap_rrlw_ncpar__get__tforeign(f90wrap_tforeign)
    use rrlw_ncpar, only: rrlw_ncpar_tforeign => tforeign
    implicit none
    integer(4), intent(out) :: f90wrap_tforeign
    
    f90wrap_tforeign = rrlw_ncpar_tforeign
end subroutine f90wrap_rrlw_ncpar__get__tforeign

subroutine f90wrap_rrlw_ncpar__get__pforeign(f90wrap_pforeign)
    use rrlw_ncpar, only: rrlw_ncpar_pforeign => pforeign
    implicit none
    integer(4), intent(out) :: f90wrap_pforeign
    
    f90wrap_pforeign = rrlw_ncpar_pforeign
end subroutine f90wrap_rrlw_ncpar__get__pforeign

subroutine f90wrap_rrlw_ncpar__get__t(f90wrap_t)
    use rrlw_ncpar, only: rrlw_ncpar_t => t
    implicit none
    integer(4), intent(out) :: f90wrap_t
    
    f90wrap_t = rrlw_ncpar_t
end subroutine f90wrap_rrlw_ncpar__get__t

subroutine f90wrap_rrlw_ncpar__get__tplanck(f90wrap_tplanck)
    use rrlw_ncpar, only: rrlw_ncpar_tplanck => tplanck
    implicit none
    integer(4), intent(out) :: f90wrap_tplanck
    
    f90wrap_tplanck = rrlw_ncpar_tplanck
end subroutine f90wrap_rrlw_ncpar__get__tplanck

subroutine f90wrap_rrlw_ncpar__get__band(f90wrap_band)
    use rrlw_ncpar, only: rrlw_ncpar_band => band
    implicit none
    integer(4), intent(out) :: f90wrap_band
    
    f90wrap_band = rrlw_ncpar_band
end subroutine f90wrap_rrlw_ncpar__get__band

subroutine f90wrap_rrlw_ncpar__get__gpoint(f90wrap_gpoint)
    use rrlw_ncpar, only: rrlw_ncpar_gpoint => gpoint
    implicit none
    integer(4), intent(out) :: f90wrap_gpoint
    
    f90wrap_gpoint = rrlw_ncpar_gpoint
end subroutine f90wrap_rrlw_ncpar__get__gpoint

subroutine f90wrap_rrlw_ncpar__get__gpointset(f90wrap_gpointset)
    use rrlw_ncpar, only: rrlw_ncpar_gpointset => gpointset
    implicit none
    integer(4), intent(out) :: f90wrap_gpointset
    
    f90wrap_gpointset = rrlw_ncpar_gpointset
end subroutine f90wrap_rrlw_ncpar__get__gpointset

! End of module rrlw_ncpar defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_ncpar.f90

