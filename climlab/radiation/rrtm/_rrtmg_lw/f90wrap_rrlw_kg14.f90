! Module rrlw_kg14 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90

subroutine f90wrap_rrlw_kg14__get__no14(f90wrap_no14)
    use rrlw_kg14, only: rrlw_kg14_no14 => no14
    implicit none
    integer(4), intent(out) :: f90wrap_no14
    
    f90wrap_no14 = rrlw_kg14_no14
end subroutine f90wrap_rrlw_kg14__get__no14

subroutine f90wrap_rrlw_kg14__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_fracrefao => fracrefao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg14_fracrefao)
    dloc = loc(rrlw_kg14_fracrefao)
end subroutine f90wrap_rrlw_kg14__array__fracrefao

subroutine f90wrap_rrlw_kg14__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_fracrefbo => fracrefbo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg14_fracrefbo)
    dloc = loc(rrlw_kg14_fracrefbo)
end subroutine f90wrap_rrlw_kg14__array__fracrefbo

subroutine f90wrap_rrlw_kg14__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_kao => kao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg14_kao)
    dloc = loc(rrlw_kg14_kao)
end subroutine f90wrap_rrlw_kg14__array__kao

subroutine f90wrap_rrlw_kg14__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_kbo => kbo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg14_kbo)
    dloc = loc(rrlw_kg14_kbo)
end subroutine f90wrap_rrlw_kg14__array__kbo

subroutine f90wrap_rrlw_kg14__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_selfrefo => selfrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg14_selfrefo)
    dloc = loc(rrlw_kg14_selfrefo)
end subroutine f90wrap_rrlw_kg14__array__selfrefo

subroutine f90wrap_rrlw_kg14__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_forrefo => forrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg14_forrefo)
    dloc = loc(rrlw_kg14_forrefo)
end subroutine f90wrap_rrlw_kg14__array__forrefo

subroutine f90wrap_rrlw_kg14__get__ng14(f90wrap_ng14)
    use rrlw_kg14, only: rrlw_kg14_ng14 => ng14
    implicit none
    integer(4), intent(out) :: f90wrap_ng14
    
    f90wrap_ng14 = rrlw_kg14_ng14
end subroutine f90wrap_rrlw_kg14__get__ng14

subroutine f90wrap_rrlw_kg14__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_fracrefa => fracrefa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg14_fracrefa)
    dloc = loc(rrlw_kg14_fracrefa)
end subroutine f90wrap_rrlw_kg14__array__fracrefa

subroutine f90wrap_rrlw_kg14__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_fracrefb => fracrefb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg14_fracrefb)
    dloc = loc(rrlw_kg14_fracrefb)
end subroutine f90wrap_rrlw_kg14__array__fracrefb

subroutine f90wrap_rrlw_kg14__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_ka => ka
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg14_ka)
    dloc = loc(rrlw_kg14_ka)
end subroutine f90wrap_rrlw_kg14__array__ka

subroutine f90wrap_rrlw_kg14__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_absa => absa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg14_absa)
    dloc = loc(rrlw_kg14_absa)
end subroutine f90wrap_rrlw_kg14__array__absa

subroutine f90wrap_rrlw_kg14__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_kb => kb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg14_kb)
    dloc = loc(rrlw_kg14_kb)
end subroutine f90wrap_rrlw_kg14__array__kb

subroutine f90wrap_rrlw_kg14__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_absb => absb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg14_absb)
    dloc = loc(rrlw_kg14_absb)
end subroutine f90wrap_rrlw_kg14__array__absb

subroutine f90wrap_rrlw_kg14__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_selfref => selfref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg14_selfref)
    dloc = loc(rrlw_kg14_selfref)
end subroutine f90wrap_rrlw_kg14__array__selfref

subroutine f90wrap_rrlw_kg14__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg14, only: rrlw_kg14_forref => forref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg14_forref)
    dloc = loc(rrlw_kg14_forref)
end subroutine f90wrap_rrlw_kg14__array__forref

! End of module rrlw_kg14 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg14.f90

