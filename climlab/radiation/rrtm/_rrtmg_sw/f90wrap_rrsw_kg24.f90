! Module rrsw_kg24 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90

subroutine f90wrap_rrsw_kg24__get__no24(f90wrap_no24)
    use rrsw_kg24, only: rrsw_kg24_no24 => no24
    implicit none
    integer(4), intent(out) :: f90wrap_no24
    
    f90wrap_no24 = rrsw_kg24_no24
end subroutine f90wrap_rrsw_kg24__get__no24

subroutine f90wrap_rrsw_kg24__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg24_kao)
    dloc = loc(rrsw_kg24_kao)
end subroutine f90wrap_rrsw_kg24__array__kao

subroutine f90wrap_rrsw_kg24__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg24_kbo)
    dloc = loc(rrsw_kg24_kbo)
end subroutine f90wrap_rrsw_kg24__array__kbo

subroutine f90wrap_rrsw_kg24__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_selfrefo)
    dloc = loc(rrsw_kg24_selfrefo)
end subroutine f90wrap_rrsw_kg24__array__selfrefo

subroutine f90wrap_rrsw_kg24__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_forrefo)
    dloc = loc(rrsw_kg24_forrefo)
end subroutine f90wrap_rrsw_kg24__array__forrefo

subroutine f90wrap_rrsw_kg24__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_sfluxrefo => sfluxrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_sfluxrefo)
    dloc = loc(rrsw_kg24_sfluxrefo)
end subroutine f90wrap_rrsw_kg24__array__sfluxrefo

subroutine f90wrap_rrsw_kg24__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_irradnceo => irradnceo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_irradnceo)
    dloc = loc(rrsw_kg24_irradnceo)
end subroutine f90wrap_rrsw_kg24__array__irradnceo

subroutine f90wrap_rrsw_kg24__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_facbrghto => facbrghto
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_facbrghto)
    dloc = loc(rrsw_kg24_facbrghto)
end subroutine f90wrap_rrsw_kg24__array__facbrghto

subroutine f90wrap_rrsw_kg24__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_snsptdrko => snsptdrko
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_snsptdrko)
    dloc = loc(rrsw_kg24_snsptdrko)
end subroutine f90wrap_rrsw_kg24__array__snsptdrko

subroutine f90wrap_rrsw_kg24__array__abso3ao(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_abso3ao => abso3ao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg24_abso3ao)
    dloc = loc(rrsw_kg24_abso3ao)
end subroutine f90wrap_rrsw_kg24__array__abso3ao

subroutine f90wrap_rrsw_kg24__array__abso3bo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_abso3bo => abso3bo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg24_abso3bo)
    dloc = loc(rrsw_kg24_abso3bo)
end subroutine f90wrap_rrsw_kg24__array__abso3bo

subroutine f90wrap_rrsw_kg24__array__raylao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_raylao => raylao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_raylao)
    dloc = loc(rrsw_kg24_raylao)
end subroutine f90wrap_rrsw_kg24__array__raylao

subroutine f90wrap_rrsw_kg24__array__raylbo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_raylbo => raylbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg24_raylbo)
    dloc = loc(rrsw_kg24_raylbo)
end subroutine f90wrap_rrsw_kg24__array__raylbo

subroutine f90wrap_rrsw_kg24__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg24_ka)
    dloc = loc(rrsw_kg24_ka)
end subroutine f90wrap_rrsw_kg24__array__ka

subroutine f90wrap_rrsw_kg24__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_absa)
    dloc = loc(rrsw_kg24_absa)
end subroutine f90wrap_rrsw_kg24__array__absa

subroutine f90wrap_rrsw_kg24__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg24_kb)
    dloc = loc(rrsw_kg24_kb)
end subroutine f90wrap_rrsw_kg24__array__kb

subroutine f90wrap_rrsw_kg24__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_absb)
    dloc = loc(rrsw_kg24_absb)
end subroutine f90wrap_rrsw_kg24__array__absb

subroutine f90wrap_rrsw_kg24__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_selfref)
    dloc = loc(rrsw_kg24_selfref)
end subroutine f90wrap_rrsw_kg24__array__selfref

subroutine f90wrap_rrsw_kg24__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_forref)
    dloc = loc(rrsw_kg24_forref)
end subroutine f90wrap_rrsw_kg24__array__forref

subroutine f90wrap_rrsw_kg24__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_sfluxref => sfluxref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_sfluxref)
    dloc = loc(rrsw_kg24_sfluxref)
end subroutine f90wrap_rrsw_kg24__array__sfluxref

subroutine f90wrap_rrsw_kg24__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_irradnce => irradnce
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_irradnce)
    dloc = loc(rrsw_kg24_irradnce)
end subroutine f90wrap_rrsw_kg24__array__irradnce

subroutine f90wrap_rrsw_kg24__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_facbrght => facbrght
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_facbrght)
    dloc = loc(rrsw_kg24_facbrght)
end subroutine f90wrap_rrsw_kg24__array__facbrght

subroutine f90wrap_rrsw_kg24__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_snsptdrk => snsptdrk
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_snsptdrk)
    dloc = loc(rrsw_kg24_snsptdrk)
end subroutine f90wrap_rrsw_kg24__array__snsptdrk

subroutine f90wrap_rrsw_kg24__array__abso3a(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_abso3a => abso3a
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg24_abso3a)
    dloc = loc(rrsw_kg24_abso3a)
end subroutine f90wrap_rrsw_kg24__array__abso3a

subroutine f90wrap_rrsw_kg24__array__abso3b(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_abso3b => abso3b
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg24_abso3b)
    dloc = loc(rrsw_kg24_abso3b)
end subroutine f90wrap_rrsw_kg24__array__abso3b

subroutine f90wrap_rrsw_kg24__array__rayla(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_rayla => rayla
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg24_rayla)
    dloc = loc(rrsw_kg24_rayla)
end subroutine f90wrap_rrsw_kg24__array__rayla

subroutine f90wrap_rrsw_kg24__array__raylb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg24, only: rrsw_kg24_raylb => raylb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg24_raylb)
    dloc = loc(rrsw_kg24_raylb)
end subroutine f90wrap_rrsw_kg24__array__raylb

! End of module rrsw_kg24 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg24.f90

