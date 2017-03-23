! Module rrsw_kg23 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90

subroutine f90wrap_rrsw_kg23__get__no23(f90wrap_no23)
    use rrsw_kg23, only: rrsw_kg23_no23 => no23
    implicit none
    integer(4), intent(out) :: f90wrap_no23
    
    f90wrap_no23 = rrsw_kg23_no23
end subroutine f90wrap_rrsw_kg23__get__no23

subroutine f90wrap_rrsw_kg23__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_kao => kao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg23_kao)
    dloc = loc(rrsw_kg23_kao)
end subroutine f90wrap_rrsw_kg23__array__kao

subroutine f90wrap_rrsw_kg23__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_selfrefo => selfrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg23_selfrefo)
    dloc = loc(rrsw_kg23_selfrefo)
end subroutine f90wrap_rrsw_kg23__array__selfrefo

subroutine f90wrap_rrsw_kg23__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_forrefo => forrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg23_forrefo)
    dloc = loc(rrsw_kg23_forrefo)
end subroutine f90wrap_rrsw_kg23__array__forrefo

subroutine f90wrap_rrsw_kg23__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_sfluxrefo => sfluxrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg23_sfluxrefo)
    dloc = loc(rrsw_kg23_sfluxrefo)
end subroutine f90wrap_rrsw_kg23__array__sfluxrefo

subroutine f90wrap_rrsw_kg23__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_irradnceo => irradnceo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg23_irradnceo)
    dloc = loc(rrsw_kg23_irradnceo)
end subroutine f90wrap_rrsw_kg23__array__irradnceo

subroutine f90wrap_rrsw_kg23__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_facbrghto => facbrghto
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg23_facbrghto)
    dloc = loc(rrsw_kg23_facbrghto)
end subroutine f90wrap_rrsw_kg23__array__facbrghto

subroutine f90wrap_rrsw_kg23__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_snsptdrko => snsptdrko
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg23_snsptdrko)
    dloc = loc(rrsw_kg23_snsptdrko)
end subroutine f90wrap_rrsw_kg23__array__snsptdrko

subroutine f90wrap_rrsw_kg23__array__raylo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_raylo => raylo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg23_raylo)
    dloc = loc(rrsw_kg23_raylo)
end subroutine f90wrap_rrsw_kg23__array__raylo

subroutine f90wrap_rrsw_kg23__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_ka => ka
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg23_ka)
    dloc = loc(rrsw_kg23_ka)
end subroutine f90wrap_rrsw_kg23__array__ka

subroutine f90wrap_rrsw_kg23__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_absa => absa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg23_absa)
    dloc = loc(rrsw_kg23_absa)
end subroutine f90wrap_rrsw_kg23__array__absa

subroutine f90wrap_rrsw_kg23__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_selfref => selfref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg23_selfref)
    dloc = loc(rrsw_kg23_selfref)
end subroutine f90wrap_rrsw_kg23__array__selfref

subroutine f90wrap_rrsw_kg23__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_forref => forref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg23_forref)
    dloc = loc(rrsw_kg23_forref)
end subroutine f90wrap_rrsw_kg23__array__forref

subroutine f90wrap_rrsw_kg23__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_sfluxref => sfluxref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg23_sfluxref)
    dloc = loc(rrsw_kg23_sfluxref)
end subroutine f90wrap_rrsw_kg23__array__sfluxref

subroutine f90wrap_rrsw_kg23__array__rayl(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_rayl => rayl
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg23_rayl)
    dloc = loc(rrsw_kg23_rayl)
end subroutine f90wrap_rrsw_kg23__array__rayl

subroutine f90wrap_rrsw_kg23__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_irradnce => irradnce
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg23_irradnce)
    dloc = loc(rrsw_kg23_irradnce)
end subroutine f90wrap_rrsw_kg23__array__irradnce

subroutine f90wrap_rrsw_kg23__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_facbrght => facbrght
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg23_facbrght)
    dloc = loc(rrsw_kg23_facbrght)
end subroutine f90wrap_rrsw_kg23__array__facbrght

subroutine f90wrap_rrsw_kg23__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use parkind
    use rrsw_kg23, only: rrsw_kg23_snsptdrk => snsptdrk
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg23_snsptdrk)
    dloc = loc(rrsw_kg23_snsptdrk)
end subroutine f90wrap_rrsw_kg23__array__snsptdrk

! End of module rrsw_kg23 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg23.f90

