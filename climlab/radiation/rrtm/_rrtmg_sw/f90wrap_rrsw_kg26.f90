! Module rrsw_kg26 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90

subroutine f90wrap_rrsw_kg26__get__no26(f90wrap_no26)
    use rrsw_kg26, only: rrsw_kg26_no26 => no26
    implicit none
    integer(4), intent(out) :: f90wrap_no26
    
    f90wrap_no26 = rrsw_kg26_no26
end subroutine f90wrap_rrsw_kg26__get__no26

subroutine f90wrap_rrsw_kg26__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg26, only: rrsw_kg26_sfluxrefo => sfluxrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg26_sfluxrefo)
    dloc = loc(rrsw_kg26_sfluxrefo)
end subroutine f90wrap_rrsw_kg26__array__sfluxrefo

subroutine f90wrap_rrsw_kg26__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg26, only: rrsw_kg26_irradnceo => irradnceo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg26_irradnceo)
    dloc = loc(rrsw_kg26_irradnceo)
end subroutine f90wrap_rrsw_kg26__array__irradnceo

subroutine f90wrap_rrsw_kg26__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg26, only: rrsw_kg26_facbrghto => facbrghto
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg26_facbrghto)
    dloc = loc(rrsw_kg26_facbrghto)
end subroutine f90wrap_rrsw_kg26__array__facbrghto

subroutine f90wrap_rrsw_kg26__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg26, only: rrsw_kg26_snsptdrko => snsptdrko
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg26_snsptdrko)
    dloc = loc(rrsw_kg26_snsptdrko)
end subroutine f90wrap_rrsw_kg26__array__snsptdrko

subroutine f90wrap_rrsw_kg26__array__raylo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg26, only: rrsw_kg26_raylo => raylo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg26_raylo)
    dloc = loc(rrsw_kg26_raylo)
end subroutine f90wrap_rrsw_kg26__array__raylo

subroutine f90wrap_rrsw_kg26__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg26, only: rrsw_kg26_sfluxref => sfluxref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg26_sfluxref)
    dloc = loc(rrsw_kg26_sfluxref)
end subroutine f90wrap_rrsw_kg26__array__sfluxref

subroutine f90wrap_rrsw_kg26__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg26, only: rrsw_kg26_irradnce => irradnce
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg26_irradnce)
    dloc = loc(rrsw_kg26_irradnce)
end subroutine f90wrap_rrsw_kg26__array__irradnce

subroutine f90wrap_rrsw_kg26__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg26, only: rrsw_kg26_facbrght => facbrght
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg26_facbrght)
    dloc = loc(rrsw_kg26_facbrght)
end subroutine f90wrap_rrsw_kg26__array__facbrght

subroutine f90wrap_rrsw_kg26__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg26, only: rrsw_kg26_snsptdrk => snsptdrk
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg26_snsptdrk)
    dloc = loc(rrsw_kg26_snsptdrk)
end subroutine f90wrap_rrsw_kg26__array__snsptdrk

subroutine f90wrap_rrsw_kg26__array__rayl(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg26, only: rrsw_kg26_rayl => rayl
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg26_rayl)
    dloc = loc(rrsw_kg26_rayl)
end subroutine f90wrap_rrsw_kg26__array__rayl

! End of module rrsw_kg26 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg26.f90

