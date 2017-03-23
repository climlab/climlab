! Module rrsw_kg16 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90

subroutine f90wrap_rrsw_kg16__get__no16(f90wrap_no16)
    use rrsw_kg16, only: rrsw_kg16_no16 => no16
    implicit none
    integer(4), intent(out) :: f90wrap_no16
    
    f90wrap_no16 = rrsw_kg16_no16
end subroutine f90wrap_rrsw_kg16__get__no16

subroutine f90wrap_rrsw_kg16__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg16_kao)
    dloc = loc(rrsw_kg16_kao)
end subroutine f90wrap_rrsw_kg16__array__kao

subroutine f90wrap_rrsw_kg16__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg16_kbo)
    dloc = loc(rrsw_kg16_kbo)
end subroutine f90wrap_rrsw_kg16__array__kbo

subroutine f90wrap_rrsw_kg16__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg16_selfrefo)
    dloc = loc(rrsw_kg16_selfrefo)
end subroutine f90wrap_rrsw_kg16__array__selfrefo

subroutine f90wrap_rrsw_kg16__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg16_forrefo)
    dloc = loc(rrsw_kg16_forrefo)
end subroutine f90wrap_rrsw_kg16__array__forrefo

subroutine f90wrap_rrsw_kg16__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_sfluxrefo => sfluxrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg16_sfluxrefo)
    dloc = loc(rrsw_kg16_sfluxrefo)
end subroutine f90wrap_rrsw_kg16__array__sfluxrefo

subroutine f90wrap_rrsw_kg16__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_irradnceo => irradnceo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg16_irradnceo)
    dloc = loc(rrsw_kg16_irradnceo)
end subroutine f90wrap_rrsw_kg16__array__irradnceo

subroutine f90wrap_rrsw_kg16__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_facbrghto => facbrghto
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg16_facbrghto)
    dloc = loc(rrsw_kg16_facbrghto)
end subroutine f90wrap_rrsw_kg16__array__facbrghto

subroutine f90wrap_rrsw_kg16__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_snsptdrko => snsptdrko
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg16_snsptdrko)
    dloc = loc(rrsw_kg16_snsptdrko)
end subroutine f90wrap_rrsw_kg16__array__snsptdrko

subroutine f90wrap_rrsw_kg16__get__rayl(f90wrap_rayl)
    use rrsw_kg16, only: rrsw_kg16_rayl => rayl
    implicit none
    real(8), intent(out) :: f90wrap_rayl
    
    f90wrap_rayl = rrsw_kg16_rayl
end subroutine f90wrap_rrsw_kg16__get__rayl

subroutine f90wrap_rrsw_kg16__set__rayl(f90wrap_rayl)
    use rrsw_kg16, only: rrsw_kg16_rayl => rayl
    implicit none
    real(8), intent(in) :: f90wrap_rayl
    
    rrsw_kg16_rayl = f90wrap_rayl
end subroutine f90wrap_rrsw_kg16__set__rayl

subroutine f90wrap_rrsw_kg16__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg16_ka)
    dloc = loc(rrsw_kg16_ka)
end subroutine f90wrap_rrsw_kg16__array__ka

subroutine f90wrap_rrsw_kg16__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg16_absa)
    dloc = loc(rrsw_kg16_absa)
end subroutine f90wrap_rrsw_kg16__array__absa

subroutine f90wrap_rrsw_kg16__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg16_kb)
    dloc = loc(rrsw_kg16_kb)
end subroutine f90wrap_rrsw_kg16__array__kb

subroutine f90wrap_rrsw_kg16__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg16_absb)
    dloc = loc(rrsw_kg16_absb)
end subroutine f90wrap_rrsw_kg16__array__absb

subroutine f90wrap_rrsw_kg16__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg16_selfref)
    dloc = loc(rrsw_kg16_selfref)
end subroutine f90wrap_rrsw_kg16__array__selfref

subroutine f90wrap_rrsw_kg16__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg16_forref)
    dloc = loc(rrsw_kg16_forref)
end subroutine f90wrap_rrsw_kg16__array__forref

subroutine f90wrap_rrsw_kg16__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_sfluxref => sfluxref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg16_sfluxref)
    dloc = loc(rrsw_kg16_sfluxref)
end subroutine f90wrap_rrsw_kg16__array__sfluxref

subroutine f90wrap_rrsw_kg16__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_irradnce => irradnce
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg16_irradnce)
    dloc = loc(rrsw_kg16_irradnce)
end subroutine f90wrap_rrsw_kg16__array__irradnce

subroutine f90wrap_rrsw_kg16__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_facbrght => facbrght
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg16_facbrght)
    dloc = loc(rrsw_kg16_facbrght)
end subroutine f90wrap_rrsw_kg16__array__facbrght

subroutine f90wrap_rrsw_kg16__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg16, only: rrsw_kg16_snsptdrk => snsptdrk
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg16_snsptdrk)
    dloc = loc(rrsw_kg16_snsptdrk)
end subroutine f90wrap_rrsw_kg16__array__snsptdrk

! End of module rrsw_kg16 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg16.f90

