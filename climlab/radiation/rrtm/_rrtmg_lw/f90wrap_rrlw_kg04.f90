! Module rrlw_kg04 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90

subroutine f90wrap_rrlw_kg04__get__no4(f90wrap_no4)
    use rrlw_kg04, only: rrlw_kg04_no4 => no4
    implicit none
    integer(4), intent(out) :: f90wrap_no4
    
    f90wrap_no4 = rrlw_kg04_no4
end subroutine f90wrap_rrlw_kg04__get__no4

subroutine f90wrap_rrlw_kg04__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_fracrefao => fracrefao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg04_fracrefao)
    dloc = loc(rrlw_kg04_fracrefao)
end subroutine f90wrap_rrlw_kg04__array__fracrefao

subroutine f90wrap_rrlw_kg04__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_fracrefbo => fracrefbo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg04_fracrefbo)
    dloc = loc(rrlw_kg04_fracrefbo)
end subroutine f90wrap_rrlw_kg04__array__fracrefbo

subroutine f90wrap_rrlw_kg04__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_kao => kao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg04_kao)
    dloc = loc(rrlw_kg04_kao)
end subroutine f90wrap_rrlw_kg04__array__kao

subroutine f90wrap_rrlw_kg04__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_kbo => kbo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg04_kbo)
    dloc = loc(rrlw_kg04_kbo)
end subroutine f90wrap_rrlw_kg04__array__kbo

subroutine f90wrap_rrlw_kg04__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_selfrefo => selfrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg04_selfrefo)
    dloc = loc(rrlw_kg04_selfrefo)
end subroutine f90wrap_rrlw_kg04__array__selfrefo

subroutine f90wrap_rrlw_kg04__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_forrefo => forrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg04_forrefo)
    dloc = loc(rrlw_kg04_forrefo)
end subroutine f90wrap_rrlw_kg04__array__forrefo

subroutine f90wrap_rrlw_kg04__get__ng4(f90wrap_ng4)
    use rrlw_kg04, only: rrlw_kg04_ng4 => ng4
    implicit none
    integer(4), intent(out) :: f90wrap_ng4
    
    f90wrap_ng4 = rrlw_kg04_ng4
end subroutine f90wrap_rrlw_kg04__get__ng4

subroutine f90wrap_rrlw_kg04__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_fracrefa => fracrefa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg04_fracrefa)
    dloc = loc(rrlw_kg04_fracrefa)
end subroutine f90wrap_rrlw_kg04__array__fracrefa

subroutine f90wrap_rrlw_kg04__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_fracrefb => fracrefb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg04_fracrefb)
    dloc = loc(rrlw_kg04_fracrefb)
end subroutine f90wrap_rrlw_kg04__array__fracrefb

subroutine f90wrap_rrlw_kg04__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_ka => ka
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg04_ka)
    dloc = loc(rrlw_kg04_ka)
end subroutine f90wrap_rrlw_kg04__array__ka

subroutine f90wrap_rrlw_kg04__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_absa => absa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg04_absa)
    dloc = loc(rrlw_kg04_absa)
end subroutine f90wrap_rrlw_kg04__array__absa

subroutine f90wrap_rrlw_kg04__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_kb => kb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg04_kb)
    dloc = loc(rrlw_kg04_kb)
end subroutine f90wrap_rrlw_kg04__array__kb

subroutine f90wrap_rrlw_kg04__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_absb => absb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg04_absb)
    dloc = loc(rrlw_kg04_absb)
end subroutine f90wrap_rrlw_kg04__array__absb

subroutine f90wrap_rrlw_kg04__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_selfref => selfref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg04_selfref)
    dloc = loc(rrlw_kg04_selfref)
end subroutine f90wrap_rrlw_kg04__array__selfref

subroutine f90wrap_rrlw_kg04__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg04, only: rrlw_kg04_forref => forref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg04_forref)
    dloc = loc(rrlw_kg04_forref)
end subroutine f90wrap_rrlw_kg04__array__forref

! End of module rrlw_kg04 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg04.f90

