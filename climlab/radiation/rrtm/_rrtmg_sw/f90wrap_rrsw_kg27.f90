! Module rrsw_kg27 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90

subroutine f90wrap_rrsw_kg27__get__no27(f90wrap_no27)
    use rrsw_kg27, only: rrsw_kg27_no27 => no27
    implicit none
    integer(4), intent(out) :: f90wrap_no27
    
    f90wrap_no27 = rrsw_kg27_no27
end subroutine f90wrap_rrsw_kg27__get__no27

subroutine f90wrap_rrsw_kg27__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg27_kao)
    dloc = loc(rrsw_kg27_kao)
end subroutine f90wrap_rrsw_kg27__array__kao

subroutine f90wrap_rrsw_kg27__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg27_kbo)
    dloc = loc(rrsw_kg27_kbo)
end subroutine f90wrap_rrsw_kg27__array__kbo

subroutine f90wrap_rrsw_kg27__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_sfluxrefo => sfluxrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg27_sfluxrefo)
    dloc = loc(rrsw_kg27_sfluxrefo)
end subroutine f90wrap_rrsw_kg27__array__sfluxrefo

subroutine f90wrap_rrsw_kg27__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_irradnceo => irradnceo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg27_irradnceo)
    dloc = loc(rrsw_kg27_irradnceo)
end subroutine f90wrap_rrsw_kg27__array__irradnceo

subroutine f90wrap_rrsw_kg27__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_facbrghto => facbrghto
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg27_facbrghto)
    dloc = loc(rrsw_kg27_facbrghto)
end subroutine f90wrap_rrsw_kg27__array__facbrghto

subroutine f90wrap_rrsw_kg27__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_snsptdrko => snsptdrko
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg27_snsptdrko)
    dloc = loc(rrsw_kg27_snsptdrko)
end subroutine f90wrap_rrsw_kg27__array__snsptdrko

subroutine f90wrap_rrsw_kg27__array__raylo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_raylo => raylo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg27_raylo)
    dloc = loc(rrsw_kg27_raylo)
end subroutine f90wrap_rrsw_kg27__array__raylo

subroutine f90wrap_rrsw_kg27__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg27_ka)
    dloc = loc(rrsw_kg27_ka)
end subroutine f90wrap_rrsw_kg27__array__ka

subroutine f90wrap_rrsw_kg27__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg27_absa)
    dloc = loc(rrsw_kg27_absa)
end subroutine f90wrap_rrsw_kg27__array__absa

subroutine f90wrap_rrsw_kg27__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg27_kb)
    dloc = loc(rrsw_kg27_kb)
end subroutine f90wrap_rrsw_kg27__array__kb

subroutine f90wrap_rrsw_kg27__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg27_absb)
    dloc = loc(rrsw_kg27_absb)
end subroutine f90wrap_rrsw_kg27__array__absb

subroutine f90wrap_rrsw_kg27__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_sfluxref => sfluxref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg27_sfluxref)
    dloc = loc(rrsw_kg27_sfluxref)
end subroutine f90wrap_rrsw_kg27__array__sfluxref

subroutine f90wrap_rrsw_kg27__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_irradnce => irradnce
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg27_irradnce)
    dloc = loc(rrsw_kg27_irradnce)
end subroutine f90wrap_rrsw_kg27__array__irradnce

subroutine f90wrap_rrsw_kg27__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_facbrght => facbrght
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg27_facbrght)
    dloc = loc(rrsw_kg27_facbrght)
end subroutine f90wrap_rrsw_kg27__array__facbrght

subroutine f90wrap_rrsw_kg27__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_snsptdrk => snsptdrk
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg27_snsptdrk)
    dloc = loc(rrsw_kg27_snsptdrk)
end subroutine f90wrap_rrsw_kg27__array__snsptdrk

subroutine f90wrap_rrsw_kg27__array__rayl(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg27, only: rrsw_kg27_rayl => rayl
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg27_rayl)
    dloc = loc(rrsw_kg27_rayl)
end subroutine f90wrap_rrsw_kg27__array__rayl

! End of module rrsw_kg27 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg27.f90

