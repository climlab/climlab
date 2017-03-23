! Module rrsw_kg28 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90

subroutine f90wrap_rrsw_kg28__get__no28(f90wrap_no28)
    use rrsw_kg28, only: rrsw_kg28_no28 => no28
    implicit none
    integer(4), intent(out) :: f90wrap_no28
    
    f90wrap_no28 = rrsw_kg28_no28
end subroutine f90wrap_rrsw_kg28__get__no28

subroutine f90wrap_rrsw_kg28__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg28_kao)
    dloc = loc(rrsw_kg28_kao)
end subroutine f90wrap_rrsw_kg28__array__kao

subroutine f90wrap_rrsw_kg28__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg28_kbo)
    dloc = loc(rrsw_kg28_kbo)
end subroutine f90wrap_rrsw_kg28__array__kbo

subroutine f90wrap_rrsw_kg28__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_sfluxrefo => sfluxrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg28_sfluxrefo)
    dloc = loc(rrsw_kg28_sfluxrefo)
end subroutine f90wrap_rrsw_kg28__array__sfluxrefo

subroutine f90wrap_rrsw_kg28__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_irradnceo => irradnceo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg28_irradnceo)
    dloc = loc(rrsw_kg28_irradnceo)
end subroutine f90wrap_rrsw_kg28__array__irradnceo

subroutine f90wrap_rrsw_kg28__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_facbrghto => facbrghto
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg28_facbrghto)
    dloc = loc(rrsw_kg28_facbrghto)
end subroutine f90wrap_rrsw_kg28__array__facbrghto

subroutine f90wrap_rrsw_kg28__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_snsptdrko => snsptdrko
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg28_snsptdrko)
    dloc = loc(rrsw_kg28_snsptdrko)
end subroutine f90wrap_rrsw_kg28__array__snsptdrko

subroutine f90wrap_rrsw_kg28__get__rayl(f90wrap_rayl)
    use rrsw_kg28, only: rrsw_kg28_rayl => rayl
    implicit none
    real(8), intent(out) :: f90wrap_rayl
    
    f90wrap_rayl = rrsw_kg28_rayl
end subroutine f90wrap_rrsw_kg28__get__rayl

subroutine f90wrap_rrsw_kg28__set__rayl(f90wrap_rayl)
    use rrsw_kg28, only: rrsw_kg28_rayl => rayl
    implicit none
    real(8), intent(in) :: f90wrap_rayl
    
    rrsw_kg28_rayl = f90wrap_rayl
end subroutine f90wrap_rrsw_kg28__set__rayl

subroutine f90wrap_rrsw_kg28__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg28_ka)
    dloc = loc(rrsw_kg28_ka)
end subroutine f90wrap_rrsw_kg28__array__ka

subroutine f90wrap_rrsw_kg28__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg28_absa)
    dloc = loc(rrsw_kg28_absa)
end subroutine f90wrap_rrsw_kg28__array__absa

subroutine f90wrap_rrsw_kg28__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrsw_kg28_kb)
    dloc = loc(rrsw_kg28_kb)
end subroutine f90wrap_rrsw_kg28__array__kb

subroutine f90wrap_rrsw_kg28__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg28_absb)
    dloc = loc(rrsw_kg28_absb)
end subroutine f90wrap_rrsw_kg28__array__absb

subroutine f90wrap_rrsw_kg28__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_sfluxref => sfluxref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg28_sfluxref)
    dloc = loc(rrsw_kg28_sfluxref)
end subroutine f90wrap_rrsw_kg28__array__sfluxref

subroutine f90wrap_rrsw_kg28__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_irradnce => irradnce
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg28_irradnce)
    dloc = loc(rrsw_kg28_irradnce)
end subroutine f90wrap_rrsw_kg28__array__irradnce

subroutine f90wrap_rrsw_kg28__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_facbrght => facbrght
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg28_facbrght)
    dloc = loc(rrsw_kg28_facbrght)
end subroutine f90wrap_rrsw_kg28__array__facbrght

subroutine f90wrap_rrsw_kg28__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg28, only: rrsw_kg28_snsptdrk => snsptdrk
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg28_snsptdrk)
    dloc = loc(rrsw_kg28_snsptdrk)
end subroutine f90wrap_rrsw_kg28__array__snsptdrk

! End of module rrsw_kg28 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg28.f90

