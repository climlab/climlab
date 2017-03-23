! Module rrsw_kg17 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90

subroutine f90wrap_rrsw_kg17__get__no17(f90wrap_no17)
    use rrsw_kg17, only: rrsw_kg17_no17 => no17
    implicit none
    integer(4), intent(out) :: f90wrap_no17
    
    f90wrap_no17 = rrsw_kg17_no17
end subroutine f90wrap_rrsw_kg17__get__no17

subroutine f90wrap_rrsw_kg17__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg17_kao)
    dloc = loc(rrsw_kg17_kao)
end subroutine f90wrap_rrsw_kg17__array__kao

subroutine f90wrap_rrsw_kg17__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg17_kbo)
    dloc = loc(rrsw_kg17_kbo)
end subroutine f90wrap_rrsw_kg17__array__kbo

subroutine f90wrap_rrsw_kg17__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_selfrefo)
    dloc = loc(rrsw_kg17_selfrefo)
end subroutine f90wrap_rrsw_kg17__array__selfrefo

subroutine f90wrap_rrsw_kg17__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_forrefo)
    dloc = loc(rrsw_kg17_forrefo)
end subroutine f90wrap_rrsw_kg17__array__forrefo

subroutine f90wrap_rrsw_kg17__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_sfluxrefo => sfluxrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_sfluxrefo)
    dloc = loc(rrsw_kg17_sfluxrefo)
end subroutine f90wrap_rrsw_kg17__array__sfluxrefo

subroutine f90wrap_rrsw_kg17__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_irradnceo => irradnceo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_irradnceo)
    dloc = loc(rrsw_kg17_irradnceo)
end subroutine f90wrap_rrsw_kg17__array__irradnceo

subroutine f90wrap_rrsw_kg17__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_facbrghto => facbrghto
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_facbrghto)
    dloc = loc(rrsw_kg17_facbrghto)
end subroutine f90wrap_rrsw_kg17__array__facbrghto

subroutine f90wrap_rrsw_kg17__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_snsptdrko => snsptdrko
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_snsptdrko)
    dloc = loc(rrsw_kg17_snsptdrko)
end subroutine f90wrap_rrsw_kg17__array__snsptdrko

subroutine f90wrap_rrsw_kg17__get__rayl(f90wrap_rayl)
    use rrsw_kg17, only: rrsw_kg17_rayl => rayl
    implicit none
    real(8), intent(out) :: f90wrap_rayl
    
    f90wrap_rayl = rrsw_kg17_rayl
end subroutine f90wrap_rrsw_kg17__get__rayl

subroutine f90wrap_rrsw_kg17__set__rayl(f90wrap_rayl)
    use rrsw_kg17, only: rrsw_kg17_rayl => rayl
    implicit none
    real(8), intent(in) :: f90wrap_rayl
    
    rrsw_kg17_rayl = f90wrap_rayl
end subroutine f90wrap_rrsw_kg17__set__rayl

subroutine f90wrap_rrsw_kg17__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg17_ka)
    dloc = loc(rrsw_kg17_ka)
end subroutine f90wrap_rrsw_kg17__array__ka

subroutine f90wrap_rrsw_kg17__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_absa)
    dloc = loc(rrsw_kg17_absa)
end subroutine f90wrap_rrsw_kg17__array__absa

subroutine f90wrap_rrsw_kg17__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg17_kb)
    dloc = loc(rrsw_kg17_kb)
end subroutine f90wrap_rrsw_kg17__array__kb

subroutine f90wrap_rrsw_kg17__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_absb)
    dloc = loc(rrsw_kg17_absb)
end subroutine f90wrap_rrsw_kg17__array__absb

subroutine f90wrap_rrsw_kg17__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_selfref)
    dloc = loc(rrsw_kg17_selfref)
end subroutine f90wrap_rrsw_kg17__array__selfref

subroutine f90wrap_rrsw_kg17__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_forref)
    dloc = loc(rrsw_kg17_forref)
end subroutine f90wrap_rrsw_kg17__array__forref

subroutine f90wrap_rrsw_kg17__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_sfluxref => sfluxref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_sfluxref)
    dloc = loc(rrsw_kg17_sfluxref)
end subroutine f90wrap_rrsw_kg17__array__sfluxref

subroutine f90wrap_rrsw_kg17__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_irradnce => irradnce
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_irradnce)
    dloc = loc(rrsw_kg17_irradnce)
end subroutine f90wrap_rrsw_kg17__array__irradnce

subroutine f90wrap_rrsw_kg17__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_facbrght => facbrght
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_facbrght)
    dloc = loc(rrsw_kg17_facbrght)
end subroutine f90wrap_rrsw_kg17__array__facbrght

subroutine f90wrap_rrsw_kg17__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg17, only: rrsw_kg17_snsptdrk => snsptdrk
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg17_snsptdrk)
    dloc = loc(rrsw_kg17_snsptdrk)
end subroutine f90wrap_rrsw_kg17__array__snsptdrk

! End of module rrsw_kg17 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg17.f90

