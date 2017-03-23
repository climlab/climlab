! Module rrlw_kg02 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90

subroutine f90wrap_rrlw_kg02__get__no2(f90wrap_no2)
    use rrlw_kg02, only: rrlw_kg02_no2 => no2
    implicit none
    integer(4), intent(out) :: f90wrap_no2
    
    f90wrap_no2 = rrlw_kg02_no2
end subroutine f90wrap_rrlw_kg02__get__no2

subroutine f90wrap_rrlw_kg02__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg02, only: rrlw_kg02_fracrefao => fracrefao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg02_fracrefao)
    dloc = loc(rrlw_kg02_fracrefao)
end subroutine f90wrap_rrlw_kg02__array__fracrefao

subroutine f90wrap_rrlw_kg02__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg02, only: rrlw_kg02_fracrefbo => fracrefbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg02_fracrefbo)
    dloc = loc(rrlw_kg02_fracrefbo)
end subroutine f90wrap_rrlw_kg02__array__fracrefbo

subroutine f90wrap_rrlw_kg02__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg02, only: rrlw_kg02_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg02_kao)
    dloc = loc(rrlw_kg02_kao)
end subroutine f90wrap_rrlw_kg02__array__kao

subroutine f90wrap_rrlw_kg02__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg02, only: rrlw_kg02_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg02_kbo)
    dloc = loc(rrlw_kg02_kbo)
end subroutine f90wrap_rrlw_kg02__array__kbo

subroutine f90wrap_rrlw_kg02__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg02, only: rrlw_kg02_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg02_selfrefo)
    dloc = loc(rrlw_kg02_selfrefo)
end subroutine f90wrap_rrlw_kg02__array__selfrefo

subroutine f90wrap_rrlw_kg02__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg02, only: rrlw_kg02_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg02_forrefo)
    dloc = loc(rrlw_kg02_forrefo)
end subroutine f90wrap_rrlw_kg02__array__forrefo

subroutine f90wrap_rrlw_kg02__get__ng2(f90wrap_ng2)
    use rrlw_kg02, only: rrlw_kg02_ng2 => ng2
    implicit none
    integer(4), intent(out) :: f90wrap_ng2
    
    f90wrap_ng2 = rrlw_kg02_ng2
end subroutine f90wrap_rrlw_kg02__get__ng2

subroutine f90wrap_rrlw_kg02__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg02, only: rrlw_kg02_fracrefa => fracrefa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg02_fracrefa)
    dloc = loc(rrlw_kg02_fracrefa)
end subroutine f90wrap_rrlw_kg02__array__fracrefa

subroutine f90wrap_rrlw_kg02__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg02, only: rrlw_kg02_fracrefb => fracrefb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg02_fracrefb)
    dloc = loc(rrlw_kg02_fracrefb)
end subroutine f90wrap_rrlw_kg02__array__fracrefb

subroutine f90wrap_rrlw_kg02__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg02, only: rrlw_kg02_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg02_ka)
    dloc = loc(rrlw_kg02_ka)
end subroutine f90wrap_rrlw_kg02__array__ka

subroutine f90wrap_rrlw_kg02__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg02, only: rrlw_kg02_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg02_absa)
    dloc = loc(rrlw_kg02_absa)
end subroutine f90wrap_rrlw_kg02__array__absa

subroutine f90wrap_rrlw_kg02__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg02, only: rrlw_kg02_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg02_kb)
    dloc = loc(rrlw_kg02_kb)
end subroutine f90wrap_rrlw_kg02__array__kb

subroutine f90wrap_rrlw_kg02__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg02, only: rrlw_kg02_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg02_absb)
    dloc = loc(rrlw_kg02_absb)
end subroutine f90wrap_rrlw_kg02__array__absb

subroutine f90wrap_rrlw_kg02__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg02, only: rrlw_kg02_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg02_selfref)
    dloc = loc(rrlw_kg02_selfref)
end subroutine f90wrap_rrlw_kg02__array__selfref

subroutine f90wrap_rrlw_kg02__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg02, only: rrlw_kg02_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg02_forref)
    dloc = loc(rrlw_kg02_forref)
end subroutine f90wrap_rrlw_kg02__array__forref

subroutine f90wrap_rrlw_kg02__array__refparam(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg02, only: rrlw_kg02_refparam => refparam
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg02_refparam)
    dloc = loc(rrlw_kg02_refparam)
end subroutine f90wrap_rrlw_kg02__array__refparam

! End of module rrlw_kg02 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg02.f90

