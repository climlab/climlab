! Module rrsw_kg21 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90

subroutine f90wrap_rrsw_kg21__get__no21(f90wrap_no21)
    use rrsw_kg21, only: rrsw_kg21_no21 => no21
    implicit none
    integer(4), intent(out) :: f90wrap_no21
    
    f90wrap_no21 = rrsw_kg21_no21
end subroutine f90wrap_rrsw_kg21__get__no21

subroutine f90wrap_rrsw_kg21__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_kao => kao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg21_kao)
    dloc = loc(rrsw_kg21_kao)
end subroutine f90wrap_rrsw_kg21__array__kao

subroutine f90wrap_rrsw_kg21__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_kbo => kbo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg21_kbo)
    dloc = loc(rrsw_kg21_kbo)
end subroutine f90wrap_rrsw_kg21__array__kbo

subroutine f90wrap_rrsw_kg21__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_selfrefo => selfrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_selfrefo)
    dloc = loc(rrsw_kg21_selfrefo)
end subroutine f90wrap_rrsw_kg21__array__selfrefo

subroutine f90wrap_rrsw_kg21__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_forrefo => forrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_forrefo)
    dloc = loc(rrsw_kg21_forrefo)
end subroutine f90wrap_rrsw_kg21__array__forrefo

subroutine f90wrap_rrsw_kg21__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_sfluxrefo => sfluxrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_sfluxrefo)
    dloc = loc(rrsw_kg21_sfluxrefo)
end subroutine f90wrap_rrsw_kg21__array__sfluxrefo

subroutine f90wrap_rrsw_kg21__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_irradnceo => irradnceo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_irradnceo)
    dloc = loc(rrsw_kg21_irradnceo)
end subroutine f90wrap_rrsw_kg21__array__irradnceo

subroutine f90wrap_rrsw_kg21__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_facbrghto => facbrghto
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_facbrghto)
    dloc = loc(rrsw_kg21_facbrghto)
end subroutine f90wrap_rrsw_kg21__array__facbrghto

subroutine f90wrap_rrsw_kg21__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_snsptdrko => snsptdrko
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_snsptdrko)
    dloc = loc(rrsw_kg21_snsptdrko)
end subroutine f90wrap_rrsw_kg21__array__snsptdrko

subroutine f90wrap_rrsw_kg21__get__rayl(f90wrap_rayl)
    use rrsw_kg21, only: rrsw_kg21_rayl => rayl
    implicit none
    real(8), intent(out) :: f90wrap_rayl
    
    f90wrap_rayl = rrsw_kg21_rayl
end subroutine f90wrap_rrsw_kg21__get__rayl

subroutine f90wrap_rrsw_kg21__set__rayl(f90wrap_rayl)
    use rrsw_kg21, only: rrsw_kg21_rayl => rayl
    implicit none
    real(8), intent(in) :: f90wrap_rayl
    
    rrsw_kg21_rayl = f90wrap_rayl
end subroutine f90wrap_rrsw_kg21__set__rayl

subroutine f90wrap_rrsw_kg21__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_ka => ka
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg21_ka)
    dloc = loc(rrsw_kg21_ka)
end subroutine f90wrap_rrsw_kg21__array__ka

subroutine f90wrap_rrsw_kg21__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_absa => absa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_absa)
    dloc = loc(rrsw_kg21_absa)
end subroutine f90wrap_rrsw_kg21__array__absa

subroutine f90wrap_rrsw_kg21__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_kb => kb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg21_kb)
    dloc = loc(rrsw_kg21_kb)
end subroutine f90wrap_rrsw_kg21__array__kb

subroutine f90wrap_rrsw_kg21__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_absb => absb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_absb)
    dloc = loc(rrsw_kg21_absb)
end subroutine f90wrap_rrsw_kg21__array__absb

subroutine f90wrap_rrsw_kg21__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_selfref => selfref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_selfref)
    dloc = loc(rrsw_kg21_selfref)
end subroutine f90wrap_rrsw_kg21__array__selfref

subroutine f90wrap_rrsw_kg21__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_forref => forref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_forref)
    dloc = loc(rrsw_kg21_forref)
end subroutine f90wrap_rrsw_kg21__array__forref

subroutine f90wrap_rrsw_kg21__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_sfluxref => sfluxref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_sfluxref)
    dloc = loc(rrsw_kg21_sfluxref)
end subroutine f90wrap_rrsw_kg21__array__sfluxref

subroutine f90wrap_rrsw_kg21__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_irradnce => irradnce
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_irradnce)
    dloc = loc(rrsw_kg21_irradnce)
end subroutine f90wrap_rrsw_kg21__array__irradnce

subroutine f90wrap_rrsw_kg21__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_facbrght => facbrght
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_facbrght)
    dloc = loc(rrsw_kg21_facbrght)
end subroutine f90wrap_rrsw_kg21__array__facbrght

subroutine f90wrap_rrsw_kg21__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg21, only: rrsw_kg21_snsptdrk => snsptdrk
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg21_snsptdrk)
    dloc = loc(rrsw_kg21_snsptdrk)
end subroutine f90wrap_rrsw_kg21__array__snsptdrk

! End of module rrsw_kg21 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg21.f90

