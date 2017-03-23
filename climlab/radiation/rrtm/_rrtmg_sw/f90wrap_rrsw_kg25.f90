! Module rrsw_kg25 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90

subroutine f90wrap_rrsw_kg25__get__no25(f90wrap_no25)
    use rrsw_kg25, only: rrsw_kg25_no25 => no25
    implicit none
    integer(4), intent(out) :: f90wrap_no25
    
    f90wrap_no25 = rrsw_kg25_no25
end subroutine f90wrap_rrsw_kg25__get__no25

subroutine f90wrap_rrsw_kg25__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg25_kao)
    dloc = loc(rrsw_kg25_kao)
end subroutine f90wrap_rrsw_kg25__array__kao

subroutine f90wrap_rrsw_kg25__array__sfluxrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_sfluxrefo => sfluxrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_sfluxrefo)
    dloc = loc(rrsw_kg25_sfluxrefo)
end subroutine f90wrap_rrsw_kg25__array__sfluxrefo

subroutine f90wrap_rrsw_kg25__array__irradnceo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_irradnceo => irradnceo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_irradnceo)
    dloc = loc(rrsw_kg25_irradnceo)
end subroutine f90wrap_rrsw_kg25__array__irradnceo

subroutine f90wrap_rrsw_kg25__array__facbrghto(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_facbrghto => facbrghto
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_facbrghto)
    dloc = loc(rrsw_kg25_facbrghto)
end subroutine f90wrap_rrsw_kg25__array__facbrghto

subroutine f90wrap_rrsw_kg25__array__snsptdrko(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_snsptdrko => snsptdrko
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_snsptdrko)
    dloc = loc(rrsw_kg25_snsptdrko)
end subroutine f90wrap_rrsw_kg25__array__snsptdrko

subroutine f90wrap_rrsw_kg25__array__abso3ao(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_abso3ao => abso3ao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_abso3ao)
    dloc = loc(rrsw_kg25_abso3ao)
end subroutine f90wrap_rrsw_kg25__array__abso3ao

subroutine f90wrap_rrsw_kg25__array__abso3bo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_abso3bo => abso3bo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_abso3bo)
    dloc = loc(rrsw_kg25_abso3bo)
end subroutine f90wrap_rrsw_kg25__array__abso3bo

subroutine f90wrap_rrsw_kg25__array__raylo(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_raylo => raylo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_raylo)
    dloc = loc(rrsw_kg25_raylo)
end subroutine f90wrap_rrsw_kg25__array__raylo

subroutine f90wrap_rrsw_kg25__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrsw_kg25_ka)
    dloc = loc(rrsw_kg25_ka)
end subroutine f90wrap_rrsw_kg25__array__ka

subroutine f90wrap_rrsw_kg25__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrsw_kg25_absa)
    dloc = loc(rrsw_kg25_absa)
end subroutine f90wrap_rrsw_kg25__array__absa

subroutine f90wrap_rrsw_kg25__array__sfluxref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_sfluxref => sfluxref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_sfluxref)
    dloc = loc(rrsw_kg25_sfluxref)
end subroutine f90wrap_rrsw_kg25__array__sfluxref

subroutine f90wrap_rrsw_kg25__array__irradnce(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_irradnce => irradnce
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_irradnce)
    dloc = loc(rrsw_kg25_irradnce)
end subroutine f90wrap_rrsw_kg25__array__irradnce

subroutine f90wrap_rrsw_kg25__array__facbrght(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_facbrght => facbrght
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_facbrght)
    dloc = loc(rrsw_kg25_facbrght)
end subroutine f90wrap_rrsw_kg25__array__facbrght

subroutine f90wrap_rrsw_kg25__array__snsptdrk(dummy_this, nd, dtype, dshape, &
    dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_snsptdrk => snsptdrk
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_snsptdrk)
    dloc = loc(rrsw_kg25_snsptdrk)
end subroutine f90wrap_rrsw_kg25__array__snsptdrk

subroutine f90wrap_rrsw_kg25__array__abso3a(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_abso3a => abso3a
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_abso3a)
    dloc = loc(rrsw_kg25_abso3a)
end subroutine f90wrap_rrsw_kg25__array__abso3a

subroutine f90wrap_rrsw_kg25__array__abso3b(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_abso3b => abso3b
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_abso3b)
    dloc = loc(rrsw_kg25_abso3b)
end subroutine f90wrap_rrsw_kg25__array__abso3b

subroutine f90wrap_rrsw_kg25__array__rayl(dummy_this, nd, dtype, dshape, dloc)
    use parrrsw
    use rrsw_kg25, only: rrsw_kg25_rayl => rayl
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrsw_kg25_rayl)
    dloc = loc(rrsw_kg25_rayl)
end subroutine f90wrap_rrsw_kg25__array__rayl

! End of module rrsw_kg25 defined in file rrtmg_sw_v4.0/gcm_model/modules/rrsw_kg25.f90

