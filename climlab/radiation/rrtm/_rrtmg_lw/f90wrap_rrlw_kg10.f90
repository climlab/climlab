! Module rrlw_kg10 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90

subroutine f90wrap_rrlw_kg10__get__no10(f90wrap_no10)
    use rrlw_kg10, only: rrlw_kg10_no10 => no10
    implicit none
    integer(4), intent(out) :: f90wrap_no10
    
    f90wrap_no10 = rrlw_kg10_no10
end subroutine f90wrap_rrlw_kg10__get__no10

subroutine f90wrap_rrlw_kg10__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg10, only: rrlw_kg10_fracrefao => fracrefao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg10_fracrefao)
    dloc = loc(rrlw_kg10_fracrefao)
end subroutine f90wrap_rrlw_kg10__array__fracrefao

subroutine f90wrap_rrlw_kg10__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg10, only: rrlw_kg10_fracrefbo => fracrefbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg10_fracrefbo)
    dloc = loc(rrlw_kg10_fracrefbo)
end subroutine f90wrap_rrlw_kg10__array__fracrefbo

subroutine f90wrap_rrlw_kg10__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg10, only: rrlw_kg10_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg10_kao)
    dloc = loc(rrlw_kg10_kao)
end subroutine f90wrap_rrlw_kg10__array__kao

subroutine f90wrap_rrlw_kg10__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg10, only: rrlw_kg10_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg10_kbo)
    dloc = loc(rrlw_kg10_kbo)
end subroutine f90wrap_rrlw_kg10__array__kbo

subroutine f90wrap_rrlw_kg10__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg10, only: rrlw_kg10_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg10_selfrefo)
    dloc = loc(rrlw_kg10_selfrefo)
end subroutine f90wrap_rrlw_kg10__array__selfrefo

subroutine f90wrap_rrlw_kg10__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg10, only: rrlw_kg10_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg10_forrefo)
    dloc = loc(rrlw_kg10_forrefo)
end subroutine f90wrap_rrlw_kg10__array__forrefo

subroutine f90wrap_rrlw_kg10__get__ng10(f90wrap_ng10)
    use rrlw_kg10, only: rrlw_kg10_ng10 => ng10
    implicit none
    integer(4), intent(out) :: f90wrap_ng10
    
    f90wrap_ng10 = rrlw_kg10_ng10
end subroutine f90wrap_rrlw_kg10__get__ng10

subroutine f90wrap_rrlw_kg10__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg10, only: rrlw_kg10_fracrefa => fracrefa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg10_fracrefa)
    dloc = loc(rrlw_kg10_fracrefa)
end subroutine f90wrap_rrlw_kg10__array__fracrefa

subroutine f90wrap_rrlw_kg10__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg10, only: rrlw_kg10_fracrefb => fracrefb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg10_fracrefb)
    dloc = loc(rrlw_kg10_fracrefb)
end subroutine f90wrap_rrlw_kg10__array__fracrefb

subroutine f90wrap_rrlw_kg10__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg10, only: rrlw_kg10_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg10_ka)
    dloc = loc(rrlw_kg10_ka)
end subroutine f90wrap_rrlw_kg10__array__ka

subroutine f90wrap_rrlw_kg10__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg10, only: rrlw_kg10_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg10_absa)
    dloc = loc(rrlw_kg10_absa)
end subroutine f90wrap_rrlw_kg10__array__absa

subroutine f90wrap_rrlw_kg10__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg10, only: rrlw_kg10_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg10_kb)
    dloc = loc(rrlw_kg10_kb)
end subroutine f90wrap_rrlw_kg10__array__kb

subroutine f90wrap_rrlw_kg10__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg10, only: rrlw_kg10_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg10_absb)
    dloc = loc(rrlw_kg10_absb)
end subroutine f90wrap_rrlw_kg10__array__absb

subroutine f90wrap_rrlw_kg10__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg10, only: rrlw_kg10_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg10_selfref)
    dloc = loc(rrlw_kg10_selfref)
end subroutine f90wrap_rrlw_kg10__array__selfref

subroutine f90wrap_rrlw_kg10__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg10, only: rrlw_kg10_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg10_forref)
    dloc = loc(rrlw_kg10_forref)
end subroutine f90wrap_rrlw_kg10__array__forref

! End of module rrlw_kg10 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg10.f90

