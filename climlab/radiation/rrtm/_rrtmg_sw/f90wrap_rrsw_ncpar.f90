! Module rrsw_ncpar defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90

subroutine f90wrap_getabsorberindex(absorbername, absorberindex)
    use rrsw_ncpar, only: getabsorberindex
    implicit none
    
    character(1024), intent(in) :: absorbername
    integer(4), intent(out) :: absorberindex
    call getabsorberindex(AbsorberName=absorbername, AbsorberIndex=absorberindex)
end subroutine f90wrap_getabsorberindex

subroutine f90wrap_rrsw_ncpar__get__cpdair(f90wrap_cpdair)
    use rrsw_ncpar, only: rrsw_ncpar_cpdair => cpdair
    implicit none
    real(8), intent(out) :: f90wrap_cpdair
    
    f90wrap_cpdair = rrsw_ncpar_cpdair
end subroutine f90wrap_rrsw_ncpar__get__cpdair

subroutine f90wrap_rrsw_ncpar__array__status(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrsw_ncpar, only: rrsw_ncpar_status => status
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 5
    dshape(1:1) = shape(rrsw_ncpar_status)
    dloc = loc(rrsw_ncpar_status)
end subroutine f90wrap_rrsw_ncpar__array__status

subroutine f90wrap_rrsw_ncpar__get__i(f90wrap_i)
    use rrsw_ncpar, only: rrsw_ncpar_i => i
    implicit none
    integer(4), intent(out) :: f90wrap_i
    
    f90wrap_i = rrsw_ncpar_i
end subroutine f90wrap_rrsw_ncpar__get__i

subroutine f90wrap_rrsw_ncpar__set__i(f90wrap_i)
    use rrsw_ncpar, only: rrsw_ncpar_i => i
    implicit none
    integer(4), intent(in) :: f90wrap_i
    
    rrsw_ncpar_i = f90wrap_i
end subroutine f90wrap_rrsw_ncpar__set__i

subroutine f90wrap_rrsw_ncpar__get__keylower(f90wrap_keylower)
    use rrsw_ncpar, only: rrsw_ncpar_keylower => keylower
    implicit none
    integer(4), intent(out) :: f90wrap_keylower
    
    f90wrap_keylower = rrsw_ncpar_keylower
end subroutine f90wrap_rrsw_ncpar__get__keylower

subroutine f90wrap_rrsw_ncpar__get__keyupper(f90wrap_keyupper)
    use rrsw_ncpar, only: rrsw_ncpar_keyupper => keyupper
    implicit none
    integer(4), intent(out) :: f90wrap_keyupper
    
    f90wrap_keyupper = rrsw_ncpar_keyupper
end subroutine f90wrap_rrsw_ncpar__get__keyupper

subroutine f90wrap_rrsw_ncpar__get__tdiff(f90wrap_tdiff)
    use rrsw_ncpar, only: rrsw_ncpar_tdiff => tdiff
    implicit none
    integer(4), intent(out) :: f90wrap_tdiff
    
    f90wrap_tdiff = rrsw_ncpar_tdiff
end subroutine f90wrap_rrsw_ncpar__get__tdiff

subroutine f90wrap_rrsw_ncpar__get__ps(f90wrap_ps)
    use rrsw_ncpar, only: rrsw_ncpar_ps => ps
    implicit none
    integer(4), intent(out) :: f90wrap_ps
    
    f90wrap_ps = rrsw_ncpar_ps
end subroutine f90wrap_rrsw_ncpar__get__ps

subroutine f90wrap_rrsw_ncpar__get__plower(f90wrap_plower)
    use rrsw_ncpar, only: rrsw_ncpar_plower => plower
    implicit none
    integer(4), intent(out) :: f90wrap_plower
    
    f90wrap_plower = rrsw_ncpar_plower
end subroutine f90wrap_rrsw_ncpar__get__plower

subroutine f90wrap_rrsw_ncpar__get__pupper(f90wrap_pupper)
    use rrsw_ncpar, only: rrsw_ncpar_pupper => pupper
    implicit none
    integer(4), intent(out) :: f90wrap_pupper
    
    f90wrap_pupper = rrsw_ncpar_pupper
end subroutine f90wrap_rrsw_ncpar__get__pupper

subroutine f90wrap_rrsw_ncpar__get__tself(f90wrap_tself)
    use rrsw_ncpar, only: rrsw_ncpar_tself => tself
    implicit none
    integer(4), intent(out) :: f90wrap_tself
    
    f90wrap_tself = rrsw_ncpar_tself
end subroutine f90wrap_rrsw_ncpar__get__tself

subroutine f90wrap_rrsw_ncpar__get__tforeignlower(f90wrap_tforeignlower)
    use rrsw_ncpar, only: rrsw_ncpar_tforeignlower => tforeignlower
    implicit none
    integer(4), intent(out) :: f90wrap_tforeignlower
    
    f90wrap_tforeignlower = rrsw_ncpar_tforeignlower
end subroutine f90wrap_rrsw_ncpar__get__tforeignlower

subroutine f90wrap_rrsw_ncpar__get__tforeignupper(f90wrap_tforeignupper)
    use rrsw_ncpar, only: rrsw_ncpar_tforeignupper => tforeignupper
    implicit none
    integer(4), intent(out) :: f90wrap_tforeignupper
    
    f90wrap_tforeignupper = rrsw_ncpar_tforeignupper
end subroutine f90wrap_rrsw_ncpar__get__tforeignupper

subroutine f90wrap_rrsw_ncpar__get__pforeign(f90wrap_pforeign)
    use rrsw_ncpar, only: rrsw_ncpar_pforeign => pforeign
    implicit none
    integer(4), intent(out) :: f90wrap_pforeign
    
    f90wrap_pforeign = rrsw_ncpar_pforeign
end subroutine f90wrap_rrsw_ncpar__get__pforeign

subroutine f90wrap_rrsw_ncpar__get__t(f90wrap_t)
    use rrsw_ncpar, only: rrsw_ncpar_t => t
    implicit none
    integer(4), intent(out) :: f90wrap_t
    
    f90wrap_t = rrsw_ncpar_t
end subroutine f90wrap_rrsw_ncpar__get__t

subroutine f90wrap_rrsw_ncpar__get__band(f90wrap_band)
    use rrsw_ncpar, only: rrsw_ncpar_band => band
    implicit none
    integer(4), intent(out) :: f90wrap_band
    
    f90wrap_band = rrsw_ncpar_band
end subroutine f90wrap_rrsw_ncpar__get__band

subroutine f90wrap_rrsw_ncpar__get__gpoint(f90wrap_gpoint)
    use rrsw_ncpar, only: rrsw_ncpar_gpoint => gpoint
    implicit none
    integer(4), intent(out) :: f90wrap_gpoint
    
    f90wrap_gpoint = rrsw_ncpar_gpoint
end subroutine f90wrap_rrsw_ncpar__get__gpoint

subroutine f90wrap_rrsw_ncpar__get__gpointset(f90wrap_gpointset)
    use rrsw_ncpar, only: rrsw_ncpar_gpointset => gpointset
    implicit none
    integer(4), intent(out) :: f90wrap_gpointset
    
    f90wrap_gpointset = rrsw_ncpar_gpointset
end subroutine f90wrap_rrsw_ncpar__get__gpointset

subroutine &
    f90wrap_rrsw_ncpar__get__maxabsorbernamelength(f90wrap_maxabsorbernamelength)
    use rrsw_ncpar, only: rrsw_ncpar_maxabsorbernamelength => maxabsorbernamelength
    implicit none
    integer(4), intent(out) :: f90wrap_maxabsorbernamelength
    
    f90wrap_maxabsorbernamelength = rrsw_ncpar_maxabsorbernamelength
end subroutine f90wrap_rrsw_ncpar__get__maxabsorbernamelength

subroutine f90wrap_rrsw_ncpar__get__absorber(f90wrap_absorber)
    use rrsw_ncpar, only: rrsw_ncpar_absorber => absorber
    implicit none
    integer(4), intent(out) :: f90wrap_absorber
    
    f90wrap_absorber = rrsw_ncpar_absorber
end subroutine f90wrap_rrsw_ncpar__get__absorber

subroutine &
    f90wrap_rrsw_ncpar__get__maxkeyspeciesnamelength(f90wrap_maxkeyspeciesnamelength)
    use rrsw_ncpar, only: rrsw_ncpar_maxkeyspeciesnamelength => &
        maxkeyspeciesnamelength
    implicit none
    integer(4), intent(out) :: f90wrap_maxkeyspeciesnamelength
    
    f90wrap_maxkeyspeciesnamelength = rrsw_ncpar_maxkeyspeciesnamelength
end subroutine f90wrap_rrsw_ncpar__get__maxkeyspeciesnamelength

subroutine &
    f90wrap_rrsw_ncpar__get__maxkeyspeciesnames(f90wrap_maxkeyspeciesnames)
    use rrsw_ncpar, only: rrsw_ncpar_maxkeyspeciesnames => maxkeyspeciesnames
    implicit none
    integer(4), intent(out) :: f90wrap_maxkeyspeciesnames
    
    f90wrap_maxkeyspeciesnames = rrsw_ncpar_maxkeyspeciesnames
end subroutine f90wrap_rrsw_ncpar__get__maxkeyspeciesnames

subroutine f90wrap_rrsw_ncpar__array__bandnums(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrsw_ncpar, only: rrsw_ncpar_bandnums => bandnums
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 5
    dshape(1:1) = shape(rrsw_ncpar_bandnums)
    dloc = loc(rrsw_ncpar_bandnums)
end subroutine f90wrap_rrsw_ncpar__array__bandnums

subroutine f90wrap_rrsw_ncpar__array__keyspecieslower(dummy_this, nd, dtype, &
    dshape, dloc)
    use parkind
    use rrsw_ncpar, only: rrsw_ncpar_keyspecieslower => keyspecieslower
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_ncpar_keyspecieslower)
    dloc = loc(rrsw_ncpar_keyspecieslower)
end subroutine f90wrap_rrsw_ncpar__array__keyspecieslower

subroutine f90wrap_rrsw_ncpar__array__keyspeciesupper(dummy_this, nd, dtype, &
    dshape, dloc)
    use parkind
    use rrsw_ncpar, only: rrsw_ncpar_keyspeciesupper => keyspeciesupper
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_ncpar_keyspeciesupper)
    dloc = loc(rrsw_ncpar_keyspeciesupper)
end subroutine f90wrap_rrsw_ncpar__array__keyspeciesupper

subroutine f90wrap_rrsw_ncpar__array__tempdiffs(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrsw_ncpar, only: rrsw_ncpar_tempdiffs => tempdiffs
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_ncpar_tempdiffs)
    dloc = loc(rrsw_ncpar_tempdiffs)
end subroutine f90wrap_rrsw_ncpar__array__tempdiffs

subroutine f90wrap_rrsw_ncpar__array__tempself(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrsw_ncpar, only: rrsw_ncpar_tempself => tempself
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_ncpar_tempself)
    dloc = loc(rrsw_ncpar_tempself)
end subroutine f90wrap_rrsw_ncpar__array__tempself

subroutine f90wrap_rrsw_ncpar__array__tempforeignlower(dummy_this, nd, dtype, &
    dshape, dloc)
    use parkind
    use rrsw_ncpar, only: rrsw_ncpar_tempforeignlower => tempforeignlower
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_ncpar_tempforeignlower)
    dloc = loc(rrsw_ncpar_tempforeignlower)
end subroutine f90wrap_rrsw_ncpar__array__tempforeignlower

subroutine f90wrap_rrsw_ncpar__array__tempforeignupper(dummy_this, nd, dtype, &
    dshape, dloc)
    use parkind
    use rrsw_ncpar, only: rrsw_ncpar_tempforeignupper => tempforeignupper
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_ncpar_tempforeignupper)
    dloc = loc(rrsw_ncpar_tempforeignupper)
end subroutine f90wrap_rrsw_ncpar__array__tempforeignupper

subroutine f90wrap_rrsw_ncpar__array__pressforeign(dummy_this, nd, dtype, &
    dshape, dloc)
    use parkind
    use rrsw_ncpar, only: rrsw_ncpar_pressforeign => pressforeign
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_ncpar_pressforeign)
    dloc = loc(rrsw_ncpar_pressforeign)
end subroutine f90wrap_rrsw_ncpar__array__pressforeign

subroutine f90wrap_rrsw_ncpar__array__temp(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrsw_ncpar, only: rrsw_ncpar_temp => temp
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_ncpar_temp)
    dloc = loc(rrsw_ncpar_temp)
end subroutine f90wrap_rrsw_ncpar__array__temp

! End of module rrsw_ncpar defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_ncpar.f90

