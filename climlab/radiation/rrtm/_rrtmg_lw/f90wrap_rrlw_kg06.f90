! Module rrlw_kg06 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90

subroutine f90wrap_rrlw_kg06__get__no6(f90wrap_no6)
    use rrlw_kg06, only: rrlw_kg06_no6 => no6
    implicit none
    integer(4), intent(out) :: f90wrap_no6
    
    f90wrap_no6 = rrlw_kg06_no6
end subroutine f90wrap_rrlw_kg06__get__no6

subroutine f90wrap_rrlw_kg06__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_fracrefao => fracrefao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg06_fracrefao)
    dloc = loc(rrlw_kg06_fracrefao)
end subroutine f90wrap_rrlw_kg06__array__fracrefao

subroutine f90wrap_rrlw_kg06__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_kao => kao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg06_kao)
    dloc = loc(rrlw_kg06_kao)
end subroutine f90wrap_rrlw_kg06__array__kao

subroutine f90wrap_rrlw_kg06__array__kao_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_kao_mco2 => kao_mco2
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg06_kao_mco2)
    dloc = loc(rrlw_kg06_kao_mco2)
end subroutine f90wrap_rrlw_kg06__array__kao_mco2

subroutine f90wrap_rrlw_kg06__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_selfrefo => selfrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg06_selfrefo)
    dloc = loc(rrlw_kg06_selfrefo)
end subroutine f90wrap_rrlw_kg06__array__selfrefo

subroutine f90wrap_rrlw_kg06__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_forrefo => forrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg06_forrefo)
    dloc = loc(rrlw_kg06_forrefo)
end subroutine f90wrap_rrlw_kg06__array__forrefo

subroutine f90wrap_rrlw_kg06__array__cfc11adjo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_cfc11adjo => cfc11adjo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg06_cfc11adjo)
    dloc = loc(rrlw_kg06_cfc11adjo)
end subroutine f90wrap_rrlw_kg06__array__cfc11adjo

subroutine f90wrap_rrlw_kg06__array__cfc12o(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_cfc12o => cfc12o
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg06_cfc12o)
    dloc = loc(rrlw_kg06_cfc12o)
end subroutine f90wrap_rrlw_kg06__array__cfc12o

subroutine f90wrap_rrlw_kg06__get__ng6(f90wrap_ng6)
    use rrlw_kg06, only: rrlw_kg06_ng6 => ng6
    implicit none
    integer(4), intent(out) :: f90wrap_ng6
    
    f90wrap_ng6 = rrlw_kg06_ng6
end subroutine f90wrap_rrlw_kg06__get__ng6

subroutine f90wrap_rrlw_kg06__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_fracrefa => fracrefa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg06_fracrefa)
    dloc = loc(rrlw_kg06_fracrefa)
end subroutine f90wrap_rrlw_kg06__array__fracrefa

subroutine f90wrap_rrlw_kg06__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_ka => ka
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg06_ka)
    dloc = loc(rrlw_kg06_ka)
end subroutine f90wrap_rrlw_kg06__array__ka

subroutine f90wrap_rrlw_kg06__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_absa => absa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg06_absa)
    dloc = loc(rrlw_kg06_absa)
end subroutine f90wrap_rrlw_kg06__array__absa

subroutine f90wrap_rrlw_kg06__array__ka_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_ka_mco2 => ka_mco2
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg06_ka_mco2)
    dloc = loc(rrlw_kg06_ka_mco2)
end subroutine f90wrap_rrlw_kg06__array__ka_mco2

subroutine f90wrap_rrlw_kg06__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_selfref => selfref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg06_selfref)
    dloc = loc(rrlw_kg06_selfref)
end subroutine f90wrap_rrlw_kg06__array__selfref

subroutine f90wrap_rrlw_kg06__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_forref => forref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg06_forref)
    dloc = loc(rrlw_kg06_forref)
end subroutine f90wrap_rrlw_kg06__array__forref

subroutine f90wrap_rrlw_kg06__array__cfc11adj(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_cfc11adj => cfc11adj
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg06_cfc11adj)
    dloc = loc(rrlw_kg06_cfc11adj)
end subroutine f90wrap_rrlw_kg06__array__cfc11adj

subroutine f90wrap_rrlw_kg06__array__cfc12(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg06, only: rrlw_kg06_cfc12 => cfc12
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg06_cfc12)
    dloc = loc(rrlw_kg06_cfc12)
end subroutine f90wrap_rrlw_kg06__array__cfc12

! End of module rrlw_kg06 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg06.f90

