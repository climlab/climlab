! Module rrlw_kg05 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90

subroutine f90wrap_rrlw_kg05__get__no5(f90wrap_no5)
    use rrlw_kg05, only: rrlw_kg05_no5 => no5
    implicit none
    integer(4), intent(out) :: f90wrap_no5
    
    f90wrap_no5 = rrlw_kg05_no5
end subroutine f90wrap_rrlw_kg05__get__no5

subroutine f90wrap_rrlw_kg05__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg05, only: rrlw_kg05_fracrefao => fracrefao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg05_fracrefao)
    dloc = loc(rrlw_kg05_fracrefao)
end subroutine f90wrap_rrlw_kg05__array__fracrefao

subroutine f90wrap_rrlw_kg05__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg05, only: rrlw_kg05_fracrefbo => fracrefbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg05_fracrefbo)
    dloc = loc(rrlw_kg05_fracrefbo)
end subroutine f90wrap_rrlw_kg05__array__fracrefbo

subroutine f90wrap_rrlw_kg05__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg05, only: rrlw_kg05_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg05_kao)
    dloc = loc(rrlw_kg05_kao)
end subroutine f90wrap_rrlw_kg05__array__kao

subroutine f90wrap_rrlw_kg05__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg05, only: rrlw_kg05_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg05_kbo)
    dloc = loc(rrlw_kg05_kbo)
end subroutine f90wrap_rrlw_kg05__array__kbo

subroutine f90wrap_rrlw_kg05__array__kao_mo3(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg05, only: rrlw_kg05_kao_mo3 => kao_mo3
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg05_kao_mo3)
    dloc = loc(rrlw_kg05_kao_mo3)
end subroutine f90wrap_rrlw_kg05__array__kao_mo3

subroutine f90wrap_rrlw_kg05__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg05, only: rrlw_kg05_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg05_selfrefo)
    dloc = loc(rrlw_kg05_selfrefo)
end subroutine f90wrap_rrlw_kg05__array__selfrefo

subroutine f90wrap_rrlw_kg05__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg05, only: rrlw_kg05_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg05_forrefo)
    dloc = loc(rrlw_kg05_forrefo)
end subroutine f90wrap_rrlw_kg05__array__forrefo

subroutine f90wrap_rrlw_kg05__array__ccl4o(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg05, only: rrlw_kg05_ccl4o => ccl4o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg05_ccl4o)
    dloc = loc(rrlw_kg05_ccl4o)
end subroutine f90wrap_rrlw_kg05__array__ccl4o

subroutine f90wrap_rrlw_kg05__get__ng5(f90wrap_ng5)
    use rrlw_kg05, only: rrlw_kg05_ng5 => ng5
    implicit none
    integer(4), intent(out) :: f90wrap_ng5
    
    f90wrap_ng5 = rrlw_kg05_ng5
end subroutine f90wrap_rrlw_kg05__get__ng5

subroutine f90wrap_rrlw_kg05__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg05, only: rrlw_kg05_fracrefa => fracrefa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg05_fracrefa)
    dloc = loc(rrlw_kg05_fracrefa)
end subroutine f90wrap_rrlw_kg05__array__fracrefa

subroutine f90wrap_rrlw_kg05__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg05, only: rrlw_kg05_fracrefb => fracrefb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg05_fracrefb)
    dloc = loc(rrlw_kg05_fracrefb)
end subroutine f90wrap_rrlw_kg05__array__fracrefb

subroutine f90wrap_rrlw_kg05__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg05, only: rrlw_kg05_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg05_ka)
    dloc = loc(rrlw_kg05_ka)
end subroutine f90wrap_rrlw_kg05__array__ka

subroutine f90wrap_rrlw_kg05__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg05, only: rrlw_kg05_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg05_absa)
    dloc = loc(rrlw_kg05_absa)
end subroutine f90wrap_rrlw_kg05__array__absa

subroutine f90wrap_rrlw_kg05__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg05, only: rrlw_kg05_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg05_kb)
    dloc = loc(rrlw_kg05_kb)
end subroutine f90wrap_rrlw_kg05__array__kb

subroutine f90wrap_rrlw_kg05__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg05, only: rrlw_kg05_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg05_absb)
    dloc = loc(rrlw_kg05_absb)
end subroutine f90wrap_rrlw_kg05__array__absb

subroutine f90wrap_rrlw_kg05__array__ka_mo3(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg05, only: rrlw_kg05_ka_mo3 => ka_mo3
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg05_ka_mo3)
    dloc = loc(rrlw_kg05_ka_mo3)
end subroutine f90wrap_rrlw_kg05__array__ka_mo3

subroutine f90wrap_rrlw_kg05__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg05, only: rrlw_kg05_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg05_selfref)
    dloc = loc(rrlw_kg05_selfref)
end subroutine f90wrap_rrlw_kg05__array__selfref

subroutine f90wrap_rrlw_kg05__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg05, only: rrlw_kg05_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg05_forref)
    dloc = loc(rrlw_kg05_forref)
end subroutine f90wrap_rrlw_kg05__array__forref

subroutine f90wrap_rrlw_kg05__array__ccl4(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg05, only: rrlw_kg05_ccl4 => ccl4
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg05_ccl4)
    dloc = loc(rrlw_kg05_ccl4)
end subroutine f90wrap_rrlw_kg05__array__ccl4

! End of module rrlw_kg05 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg05.f90

