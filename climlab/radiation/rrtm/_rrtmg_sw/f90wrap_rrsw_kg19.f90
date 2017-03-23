! Module rrsw_kg19 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90

subroutine f90wrap_rrsw_kg19__get__no19(f90wrap_no19)
    use rrsw_kg19, only: rrsw_kg19_no19 => no19
    implicit none
    integer(4), intent(out) :: f90wrap_no19
    
    f90wrap_no19 = rrsw_kg19_no19
end subroutine f90wrap_rrsw_kg19__get__no19

subroutine f90wrap_rrsw_kg19__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg19_kao)
    dloc = loc(rrsw_kg19_kao)
end subroutine f90wrap_rrsw_kg19__array__kao

subroutine f90wrap_rrsw_kg19__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg19_kbo)
    dloc = loc(rrsw_kg19_kbo)
end subroutine f90wrap_rrsw_kg19__array__kbo

subroutine f90wrap_rrsw_kg19__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_selfrefo)
    dloc = loc(rrsw_kg19_selfrefo)
end subroutine f90wrap_rrsw_kg19__array__selfrefo

subroutine f90wrap_rrsw_kg19__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_forrefo)
    dloc = loc(rrsw_kg19_forrefo)
end subroutine f90wrap_rrsw_kg19__array__forrefo

subroutine f90wrap_rrsw_kg19__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_sfluxrefo => sfluxrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_sfluxrefo)
    dloc = loc(rrsw_kg19_sfluxrefo)
end subroutine f90wrap_rrsw_kg19__array__sfluxrefo

subroutine f90wrap_rrsw_kg19__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_irradnceo => irradnceo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_irradnceo)
    dloc = loc(rrsw_kg19_irradnceo)
end subroutine f90wrap_rrsw_kg19__array__irradnceo

subroutine f90wrap_rrsw_kg19__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_facbrghto => facbrghto
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_facbrghto)
    dloc = loc(rrsw_kg19_facbrghto)
end subroutine f90wrap_rrsw_kg19__array__facbrghto

subroutine f90wrap_rrsw_kg19__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_snsptdrko => snsptdrko
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_snsptdrko)
    dloc = loc(rrsw_kg19_snsptdrko)
end subroutine f90wrap_rrsw_kg19__array__snsptdrko

subroutine f90wrap_rrsw_kg19__get__rayl(f90wrap_rayl)
    use rrsw_kg19, only: rrsw_kg19_rayl => rayl
    implicit none
    real(8), intent(out) :: f90wrap_rayl
    
    f90wrap_rayl = rrsw_kg19_rayl
end subroutine f90wrap_rrsw_kg19__get__rayl

subroutine f90wrap_rrsw_kg19__set__rayl(f90wrap_rayl)
    use rrsw_kg19, only: rrsw_kg19_rayl => rayl
    implicit none
    real(8), intent(in) :: f90wrap_rayl
    
    rrsw_kg19_rayl = f90wrap_rayl
end subroutine f90wrap_rrsw_kg19__set__rayl

subroutine f90wrap_rrsw_kg19__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg19_ka)
    dloc = loc(rrsw_kg19_ka)
end subroutine f90wrap_rrsw_kg19__array__ka

subroutine f90wrap_rrsw_kg19__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_absa)
    dloc = loc(rrsw_kg19_absa)
end subroutine f90wrap_rrsw_kg19__array__absa

subroutine f90wrap_rrsw_kg19__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg19_kb)
    dloc = loc(rrsw_kg19_kb)
end subroutine f90wrap_rrsw_kg19__array__kb

subroutine f90wrap_rrsw_kg19__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_absb)
    dloc = loc(rrsw_kg19_absb)
end subroutine f90wrap_rrsw_kg19__array__absb

subroutine f90wrap_rrsw_kg19__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_selfref)
    dloc = loc(rrsw_kg19_selfref)
end subroutine f90wrap_rrsw_kg19__array__selfref

subroutine f90wrap_rrsw_kg19__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_forref)
    dloc = loc(rrsw_kg19_forref)
end subroutine f90wrap_rrsw_kg19__array__forref

subroutine f90wrap_rrsw_kg19__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_sfluxref => sfluxref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_sfluxref)
    dloc = loc(rrsw_kg19_sfluxref)
end subroutine f90wrap_rrsw_kg19__array__sfluxref

subroutine f90wrap_rrsw_kg19__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_irradnce => irradnce
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_irradnce)
    dloc = loc(rrsw_kg19_irradnce)
end subroutine f90wrap_rrsw_kg19__array__irradnce

subroutine f90wrap_rrsw_kg19__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_facbrght => facbrght
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_facbrght)
    dloc = loc(rrsw_kg19_facbrght)
end subroutine f90wrap_rrsw_kg19__array__facbrght

subroutine f90wrap_rrsw_kg19__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg19, only: rrsw_kg19_snsptdrk => snsptdrk
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg19_snsptdrk)
    dloc = loc(rrsw_kg19_snsptdrk)
end subroutine f90wrap_rrsw_kg19__array__snsptdrk

! End of module rrsw_kg19 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg19.f90

