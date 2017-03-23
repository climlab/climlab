! Module rrsw_kg22 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90

subroutine f90wrap_rrsw_kg22__get__no22(f90wrap_no22)
    use rrsw_kg22, only: rrsw_kg22_no22 => no22
    implicit none
    integer(4), intent(out) :: f90wrap_no22
    
    f90wrap_no22 = rrsw_kg22_no22
end subroutine f90wrap_rrsw_kg22__get__no22

subroutine f90wrap_rrsw_kg22__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_kao => kao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg22_kao)
    dloc = loc(rrsw_kg22_kao)
end subroutine f90wrap_rrsw_kg22__array__kao

subroutine f90wrap_rrsw_kg22__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_kbo => kbo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg22_kbo)
    dloc = loc(rrsw_kg22_kbo)
end subroutine f90wrap_rrsw_kg22__array__kbo

subroutine f90wrap_rrsw_kg22__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_selfrefo => selfrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_selfrefo)
    dloc = loc(rrsw_kg22_selfrefo)
end subroutine f90wrap_rrsw_kg22__array__selfrefo

subroutine f90wrap_rrsw_kg22__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_forrefo => forrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_forrefo)
    dloc = loc(rrsw_kg22_forrefo)
end subroutine f90wrap_rrsw_kg22__array__forrefo

subroutine f90wrap_rrsw_kg22__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_sfluxrefo => sfluxrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_sfluxrefo)
    dloc = loc(rrsw_kg22_sfluxrefo)
end subroutine f90wrap_rrsw_kg22__array__sfluxrefo

subroutine f90wrap_rrsw_kg22__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_irradnceo => irradnceo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_irradnceo)
    dloc = loc(rrsw_kg22_irradnceo)
end subroutine f90wrap_rrsw_kg22__array__irradnceo

subroutine f90wrap_rrsw_kg22__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_facbrghto => facbrghto
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_facbrghto)
    dloc = loc(rrsw_kg22_facbrghto)
end subroutine f90wrap_rrsw_kg22__array__facbrghto

subroutine f90wrap_rrsw_kg22__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_snsptdrko => snsptdrko
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_snsptdrko)
    dloc = loc(rrsw_kg22_snsptdrko)
end subroutine f90wrap_rrsw_kg22__array__snsptdrko

subroutine f90wrap_rrsw_kg22__get__rayl(f90wrap_rayl)
    use rrsw_kg22, only: rrsw_kg22_rayl => rayl
    implicit none
    real(8), intent(out) :: f90wrap_rayl
    
    f90wrap_rayl = rrsw_kg22_rayl
end subroutine f90wrap_rrsw_kg22__get__rayl

subroutine f90wrap_rrsw_kg22__set__rayl(f90wrap_rayl)
    use rrsw_kg22, only: rrsw_kg22_rayl => rayl
    implicit none
    real(8), intent(in) :: f90wrap_rayl
    
    rrsw_kg22_rayl = f90wrap_rayl
end subroutine f90wrap_rrsw_kg22__set__rayl

subroutine f90wrap_rrsw_kg22__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_ka => ka
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg22_ka)
    dloc = loc(rrsw_kg22_ka)
end subroutine f90wrap_rrsw_kg22__array__ka

subroutine f90wrap_rrsw_kg22__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_absa => absa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_absa)
    dloc = loc(rrsw_kg22_absa)
end subroutine f90wrap_rrsw_kg22__array__absa

subroutine f90wrap_rrsw_kg22__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_kb => kb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg22_kb)
    dloc = loc(rrsw_kg22_kb)
end subroutine f90wrap_rrsw_kg22__array__kb

subroutine f90wrap_rrsw_kg22__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_absb => absb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_absb)
    dloc = loc(rrsw_kg22_absb)
end subroutine f90wrap_rrsw_kg22__array__absb

subroutine f90wrap_rrsw_kg22__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_selfref => selfref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_selfref)
    dloc = loc(rrsw_kg22_selfref)
end subroutine f90wrap_rrsw_kg22__array__selfref

subroutine f90wrap_rrsw_kg22__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_forref => forref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_forref)
    dloc = loc(rrsw_kg22_forref)
end subroutine f90wrap_rrsw_kg22__array__forref

subroutine f90wrap_rrsw_kg22__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_sfluxref => sfluxref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_sfluxref)
    dloc = loc(rrsw_kg22_sfluxref)
end subroutine f90wrap_rrsw_kg22__array__sfluxref

subroutine f90wrap_rrsw_kg22__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_irradnce => irradnce
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_irradnce)
    dloc = loc(rrsw_kg22_irradnce)
end subroutine f90wrap_rrsw_kg22__array__irradnce

subroutine f90wrap_rrsw_kg22__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_facbrght => facbrght
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_facbrght)
    dloc = loc(rrsw_kg22_facbrght)
end subroutine f90wrap_rrsw_kg22__array__facbrght

subroutine f90wrap_rrsw_kg22__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg22, only: rrsw_kg22_snsptdrk => snsptdrk
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg22_snsptdrk)
    dloc = loc(rrsw_kg22_snsptdrk)
end subroutine f90wrap_rrsw_kg22__array__snsptdrk

! End of module rrsw_kg22 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg22.f90

