! Module rrsw_kg20 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90

subroutine f90wrap_rrsw_kg20__get__no20(f90wrap_no20)
    use rrsw_kg20, only: rrsw_kg20_no20 => no20
    implicit none
    integer(4), intent(out) :: f90wrap_no20
    
    f90wrap_no20 = rrsw_kg20_no20
end subroutine f90wrap_rrsw_kg20__get__no20

subroutine f90wrap_rrsw_kg20__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg20_kao)
    dloc = loc(rrsw_kg20_kao)
end subroutine f90wrap_rrsw_kg20__array__kao

subroutine f90wrap_rrsw_kg20__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg20_kbo)
    dloc = loc(rrsw_kg20_kbo)
end subroutine f90wrap_rrsw_kg20__array__kbo

subroutine f90wrap_rrsw_kg20__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg20_selfrefo)
    dloc = loc(rrsw_kg20_selfrefo)
end subroutine f90wrap_rrsw_kg20__array__selfrefo

subroutine f90wrap_rrsw_kg20__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg20_forrefo)
    dloc = loc(rrsw_kg20_forrefo)
end subroutine f90wrap_rrsw_kg20__array__forrefo

subroutine f90wrap_rrsw_kg20__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_sfluxrefo => sfluxrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg20_sfluxrefo)
    dloc = loc(rrsw_kg20_sfluxrefo)
end subroutine f90wrap_rrsw_kg20__array__sfluxrefo

subroutine f90wrap_rrsw_kg20__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_irradnceo => irradnceo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg20_irradnceo)
    dloc = loc(rrsw_kg20_irradnceo)
end subroutine f90wrap_rrsw_kg20__array__irradnceo

subroutine f90wrap_rrsw_kg20__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_facbrghto => facbrghto
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg20_facbrghto)
    dloc = loc(rrsw_kg20_facbrghto)
end subroutine f90wrap_rrsw_kg20__array__facbrghto

subroutine f90wrap_rrsw_kg20__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_snsptdrko => snsptdrko
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg20_snsptdrko)
    dloc = loc(rrsw_kg20_snsptdrko)
end subroutine f90wrap_rrsw_kg20__array__snsptdrko

subroutine f90wrap_rrsw_kg20__array__absch4o(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_absch4o => absch4o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg20_absch4o)
    dloc = loc(rrsw_kg20_absch4o)
end subroutine f90wrap_rrsw_kg20__array__absch4o

subroutine f90wrap_rrsw_kg20__get__rayl(f90wrap_rayl)
    use rrsw_kg20, only: rrsw_kg20_rayl => rayl
    implicit none
    real(8), intent(out) :: f90wrap_rayl
    
    f90wrap_rayl = rrsw_kg20_rayl
end subroutine f90wrap_rrsw_kg20__get__rayl

subroutine f90wrap_rrsw_kg20__set__rayl(f90wrap_rayl)
    use rrsw_kg20, only: rrsw_kg20_rayl => rayl
    implicit none
    real(8), intent(in) :: f90wrap_rayl
    
    rrsw_kg20_rayl = f90wrap_rayl
end subroutine f90wrap_rrsw_kg20__set__rayl

subroutine f90wrap_rrsw_kg20__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg20_ka)
    dloc = loc(rrsw_kg20_ka)
end subroutine f90wrap_rrsw_kg20__array__ka

subroutine f90wrap_rrsw_kg20__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg20_absa)
    dloc = loc(rrsw_kg20_absa)
end subroutine f90wrap_rrsw_kg20__array__absa

subroutine f90wrap_rrsw_kg20__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg20_kb)
    dloc = loc(rrsw_kg20_kb)
end subroutine f90wrap_rrsw_kg20__array__kb

subroutine f90wrap_rrsw_kg20__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg20_absb)
    dloc = loc(rrsw_kg20_absb)
end subroutine f90wrap_rrsw_kg20__array__absb

subroutine f90wrap_rrsw_kg20__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg20_selfref)
    dloc = loc(rrsw_kg20_selfref)
end subroutine f90wrap_rrsw_kg20__array__selfref

subroutine f90wrap_rrsw_kg20__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg20_forref)
    dloc = loc(rrsw_kg20_forref)
end subroutine f90wrap_rrsw_kg20__array__forref

subroutine f90wrap_rrsw_kg20__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_sfluxref => sfluxref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg20_sfluxref)
    dloc = loc(rrsw_kg20_sfluxref)
end subroutine f90wrap_rrsw_kg20__array__sfluxref

subroutine f90wrap_rrsw_kg20__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_irradnce => irradnce
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg20_irradnce)
    dloc = loc(rrsw_kg20_irradnce)
end subroutine f90wrap_rrsw_kg20__array__irradnce

subroutine f90wrap_rrsw_kg20__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_facbrght => facbrght
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg20_facbrght)
    dloc = loc(rrsw_kg20_facbrght)
end subroutine f90wrap_rrsw_kg20__array__facbrght

subroutine f90wrap_rrsw_kg20__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_snsptdrk => snsptdrk
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg20_snsptdrk)
    dloc = loc(rrsw_kg20_snsptdrk)
end subroutine f90wrap_rrsw_kg20__array__snsptdrk

subroutine f90wrap_rrsw_kg20__array__absch4(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg20, only: rrsw_kg20_absch4 => absch4
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg20_absch4)
    dloc = loc(rrsw_kg20_absch4)
end subroutine f90wrap_rrsw_kg20__array__absch4

! End of module rrsw_kg20 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg20.f90

