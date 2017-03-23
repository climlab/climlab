! Module rrlw_kg08 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90

subroutine f90wrap_rrlw_kg08__get__no8(f90wrap_no8)
    use rrlw_kg08, only: rrlw_kg08_no8 => no8
    implicit none
    integer(4), intent(out) :: f90wrap_no8
    
    f90wrap_no8 = rrlw_kg08_no8
end subroutine f90wrap_rrlw_kg08__get__no8

subroutine f90wrap_rrlw_kg08__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_fracrefao => fracrefao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg08_fracrefao)
    dloc = loc(rrlw_kg08_fracrefao)
end subroutine f90wrap_rrlw_kg08__array__fracrefao

subroutine f90wrap_rrlw_kg08__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_fracrefbo => fracrefbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg08_fracrefbo)
    dloc = loc(rrlw_kg08_fracrefbo)
end subroutine f90wrap_rrlw_kg08__array__fracrefbo

subroutine f90wrap_rrlw_kg08__array__cfc12o(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg08, only: rrlw_kg08_cfc12o => cfc12o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg08_cfc12o)
    dloc = loc(rrlw_kg08_cfc12o)
end subroutine f90wrap_rrlw_kg08__array__cfc12o

subroutine f90wrap_rrlw_kg08__array__cfc22adjo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_cfc22adjo => cfc22adjo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg08_cfc22adjo)
    dloc = loc(rrlw_kg08_cfc22adjo)
end subroutine f90wrap_rrlw_kg08__array__cfc22adjo

subroutine f90wrap_rrlw_kg08__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg08, only: rrlw_kg08_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg08_kao)
    dloc = loc(rrlw_kg08_kao)
end subroutine f90wrap_rrlw_kg08__array__kao

subroutine f90wrap_rrlw_kg08__array__kao_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_kao_mco2 => kao_mco2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_kao_mco2)
    dloc = loc(rrlw_kg08_kao_mco2)
end subroutine f90wrap_rrlw_kg08__array__kao_mco2

subroutine f90wrap_rrlw_kg08__array__kao_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_kao_mn2o => kao_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_kao_mn2o)
    dloc = loc(rrlw_kg08_kao_mn2o)
end subroutine f90wrap_rrlw_kg08__array__kao_mn2o

subroutine f90wrap_rrlw_kg08__array__kao_mo3(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_kao_mo3 => kao_mo3
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_kao_mo3)
    dloc = loc(rrlw_kg08_kao_mo3)
end subroutine f90wrap_rrlw_kg08__array__kao_mo3

subroutine f90wrap_rrlw_kg08__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg08, only: rrlw_kg08_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg08_kbo)
    dloc = loc(rrlw_kg08_kbo)
end subroutine f90wrap_rrlw_kg08__array__kbo

subroutine f90wrap_rrlw_kg08__array__kbo_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_kbo_mco2 => kbo_mco2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_kbo_mco2)
    dloc = loc(rrlw_kg08_kbo_mco2)
end subroutine f90wrap_rrlw_kg08__array__kbo_mco2

subroutine f90wrap_rrlw_kg08__array__kbo_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_kbo_mn2o => kbo_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_kbo_mn2o)
    dloc = loc(rrlw_kg08_kbo_mn2o)
end subroutine f90wrap_rrlw_kg08__array__kbo_mn2o

subroutine f90wrap_rrlw_kg08__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_selfrefo)
    dloc = loc(rrlw_kg08_selfrefo)
end subroutine f90wrap_rrlw_kg08__array__selfrefo

subroutine f90wrap_rrlw_kg08__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_forrefo)
    dloc = loc(rrlw_kg08_forrefo)
end subroutine f90wrap_rrlw_kg08__array__forrefo

subroutine f90wrap_rrlw_kg08__get__ng8(f90wrap_ng8)
    use rrlw_kg08, only: rrlw_kg08_ng8 => ng8
    implicit none
    integer(4), intent(out) :: f90wrap_ng8
    
    f90wrap_ng8 = rrlw_kg08_ng8
end subroutine f90wrap_rrlw_kg08__get__ng8

subroutine f90wrap_rrlw_kg08__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_fracrefa => fracrefa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg08_fracrefa)
    dloc = loc(rrlw_kg08_fracrefa)
end subroutine f90wrap_rrlw_kg08__array__fracrefa

subroutine f90wrap_rrlw_kg08__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_fracrefb => fracrefb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg08_fracrefb)
    dloc = loc(rrlw_kg08_fracrefb)
end subroutine f90wrap_rrlw_kg08__array__fracrefb

subroutine f90wrap_rrlw_kg08__array__cfc12(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg08, only: rrlw_kg08_cfc12 => cfc12
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg08_cfc12)
    dloc = loc(rrlw_kg08_cfc12)
end subroutine f90wrap_rrlw_kg08__array__cfc12

subroutine f90wrap_rrlw_kg08__array__cfc22adj(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_cfc22adj => cfc22adj
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg08_cfc22adj)
    dloc = loc(rrlw_kg08_cfc22adj)
end subroutine f90wrap_rrlw_kg08__array__cfc22adj

subroutine f90wrap_rrlw_kg08__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg08, only: rrlw_kg08_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg08_ka)
    dloc = loc(rrlw_kg08_ka)
end subroutine f90wrap_rrlw_kg08__array__ka

subroutine f90wrap_rrlw_kg08__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg08, only: rrlw_kg08_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_absa)
    dloc = loc(rrlw_kg08_absa)
end subroutine f90wrap_rrlw_kg08__array__absa

subroutine f90wrap_rrlw_kg08__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg08, only: rrlw_kg08_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg08_kb)
    dloc = loc(rrlw_kg08_kb)
end subroutine f90wrap_rrlw_kg08__array__kb

subroutine f90wrap_rrlw_kg08__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg08, only: rrlw_kg08_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_absb)
    dloc = loc(rrlw_kg08_absb)
end subroutine f90wrap_rrlw_kg08__array__absb

subroutine f90wrap_rrlw_kg08__array__ka_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_ka_mco2 => ka_mco2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_ka_mco2)
    dloc = loc(rrlw_kg08_ka_mco2)
end subroutine f90wrap_rrlw_kg08__array__ka_mco2

subroutine f90wrap_rrlw_kg08__array__ka_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_ka_mn2o => ka_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_ka_mn2o)
    dloc = loc(rrlw_kg08_ka_mn2o)
end subroutine f90wrap_rrlw_kg08__array__ka_mn2o

subroutine f90wrap_rrlw_kg08__array__ka_mo3(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg08, only: rrlw_kg08_ka_mo3 => ka_mo3
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_ka_mo3)
    dloc = loc(rrlw_kg08_ka_mo3)
end subroutine f90wrap_rrlw_kg08__array__ka_mo3

subroutine f90wrap_rrlw_kg08__array__kb_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_kb_mco2 => kb_mco2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_kb_mco2)
    dloc = loc(rrlw_kg08_kb_mco2)
end subroutine f90wrap_rrlw_kg08__array__kb_mco2

subroutine f90wrap_rrlw_kg08__array__kb_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_kb_mn2o => kb_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_kb_mn2o)
    dloc = loc(rrlw_kg08_kb_mn2o)
end subroutine f90wrap_rrlw_kg08__array__kb_mn2o

subroutine f90wrap_rrlw_kg08__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg08, only: rrlw_kg08_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_selfref)
    dloc = loc(rrlw_kg08_selfref)
end subroutine f90wrap_rrlw_kg08__array__selfref

subroutine f90wrap_rrlw_kg08__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg08, only: rrlw_kg08_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg08_forref)
    dloc = loc(rrlw_kg08_forref)
end subroutine f90wrap_rrlw_kg08__array__forref

! End of module rrlw_kg08 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg08.f90

