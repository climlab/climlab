! Module rrsw_kg18 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90

subroutine f90wrap_rrsw_kg18__get__no18(f90wrap_no18)
    use rrsw_kg18, only: rrsw_kg18_no18 => no18
    implicit none
    integer(4), intent(out) :: f90wrap_no18
    
    f90wrap_no18 = rrsw_kg18_no18
end subroutine f90wrap_rrsw_kg18__get__no18

subroutine f90wrap_rrsw_kg18__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_kao => kao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg18_kao)
    dloc = loc(rrsw_kg18_kao)
end subroutine f90wrap_rrsw_kg18__array__kao

subroutine f90wrap_rrsw_kg18__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_kbo => kbo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg18_kbo)
    dloc = loc(rrsw_kg18_kbo)
end subroutine f90wrap_rrsw_kg18__array__kbo

subroutine f90wrap_rrsw_kg18__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_selfrefo => selfrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_selfrefo)
    dloc = loc(rrsw_kg18_selfrefo)
end subroutine f90wrap_rrsw_kg18__array__selfrefo

subroutine f90wrap_rrsw_kg18__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_forrefo => forrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_forrefo)
    dloc = loc(rrsw_kg18_forrefo)
end subroutine f90wrap_rrsw_kg18__array__forrefo

subroutine f90wrap_rrsw_kg18__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_sfluxrefo => sfluxrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_sfluxrefo)
    dloc = loc(rrsw_kg18_sfluxrefo)
end subroutine f90wrap_rrsw_kg18__array__sfluxrefo

subroutine f90wrap_rrsw_kg18__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_irradnceo => irradnceo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_irradnceo)
    dloc = loc(rrsw_kg18_irradnceo)
end subroutine f90wrap_rrsw_kg18__array__irradnceo

subroutine f90wrap_rrsw_kg18__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_facbrghto => facbrghto
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_facbrghto)
    dloc = loc(rrsw_kg18_facbrghto)
end subroutine f90wrap_rrsw_kg18__array__facbrghto

subroutine f90wrap_rrsw_kg18__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_snsptdrko => snsptdrko
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_snsptdrko)
    dloc = loc(rrsw_kg18_snsptdrko)
end subroutine f90wrap_rrsw_kg18__array__snsptdrko

subroutine f90wrap_rrsw_kg18__get__rayl(f90wrap_rayl)
    use rrsw_kg18, only: rrsw_kg18_rayl => rayl
    implicit none
    real(8), intent(out) :: f90wrap_rayl
    
    f90wrap_rayl = rrsw_kg18_rayl
end subroutine f90wrap_rrsw_kg18__get__rayl

subroutine f90wrap_rrsw_kg18__set__rayl(f90wrap_rayl)
    use rrsw_kg18, only: rrsw_kg18_rayl => rayl
    implicit none
    real(8), intent(in) :: f90wrap_rayl
    
    rrsw_kg18_rayl = f90wrap_rayl
end subroutine f90wrap_rrsw_kg18__set__rayl

subroutine f90wrap_rrsw_kg18__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_ka => ka
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg18_ka)
    dloc = loc(rrsw_kg18_ka)
end subroutine f90wrap_rrsw_kg18__array__ka

subroutine f90wrap_rrsw_kg18__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_absa => absa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_absa)
    dloc = loc(rrsw_kg18_absa)
end subroutine f90wrap_rrsw_kg18__array__absa

subroutine f90wrap_rrsw_kg18__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_kb => kb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg18_kb)
    dloc = loc(rrsw_kg18_kb)
end subroutine f90wrap_rrsw_kg18__array__kb

subroutine f90wrap_rrsw_kg18__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_absb => absb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_absb)
    dloc = loc(rrsw_kg18_absb)
end subroutine f90wrap_rrsw_kg18__array__absb

subroutine f90wrap_rrsw_kg18__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_selfref => selfref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_selfref)
    dloc = loc(rrsw_kg18_selfref)
end subroutine f90wrap_rrsw_kg18__array__selfref

subroutine f90wrap_rrsw_kg18__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_forref => forref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_forref)
    dloc = loc(rrsw_kg18_forref)
end subroutine f90wrap_rrsw_kg18__array__forref

subroutine f90wrap_rrsw_kg18__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_sfluxref => sfluxref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_sfluxref)
    dloc = loc(rrsw_kg18_sfluxref)
end subroutine f90wrap_rrsw_kg18__array__sfluxref

subroutine f90wrap_rrsw_kg18__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_irradnce => irradnce
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_irradnce)
    dloc = loc(rrsw_kg18_irradnce)
end subroutine f90wrap_rrsw_kg18__array__irradnce

subroutine f90wrap_rrsw_kg18__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_facbrght => facbrght
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_facbrght)
    dloc = loc(rrsw_kg18_facbrght)
end subroutine f90wrap_rrsw_kg18__array__facbrght

subroutine f90wrap_rrsw_kg18__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg18, only: rrsw_kg18_snsptdrk => snsptdrk
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg18_snsptdrk)
    dloc = loc(rrsw_kg18_snsptdrk)
end subroutine f90wrap_rrsw_kg18__array__snsptdrk

! End of module rrsw_kg18 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg18.f90

