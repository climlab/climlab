! Module rrlw_kg11 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90

subroutine f90wrap_rrlw_kg11__get__no11(f90wrap_no11)
    use rrlw_kg11, only: rrlw_kg11_no11 => no11
    implicit none
    integer(4), intent(out) :: f90wrap_no11
    
    f90wrap_no11 = rrlw_kg11_no11
end subroutine f90wrap_rrlw_kg11__get__no11

subroutine f90wrap_rrlw_kg11__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg11, only: rrlw_kg11_fracrefao => fracrefao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg11_fracrefao)
    dloc = loc(rrlw_kg11_fracrefao)
end subroutine f90wrap_rrlw_kg11__array__fracrefao

subroutine f90wrap_rrlw_kg11__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg11, only: rrlw_kg11_fracrefbo => fracrefbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg11_fracrefbo)
    dloc = loc(rrlw_kg11_fracrefbo)
end subroutine f90wrap_rrlw_kg11__array__fracrefbo

subroutine f90wrap_rrlw_kg11__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg11, only: rrlw_kg11_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg11_kao)
    dloc = loc(rrlw_kg11_kao)
end subroutine f90wrap_rrlw_kg11__array__kao

subroutine f90wrap_rrlw_kg11__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg11, only: rrlw_kg11_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg11_kbo)
    dloc = loc(rrlw_kg11_kbo)
end subroutine f90wrap_rrlw_kg11__array__kbo

subroutine f90wrap_rrlw_kg11__array__kao_mo2(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg11, only: rrlw_kg11_kao_mo2 => kao_mo2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg11_kao_mo2)
    dloc = loc(rrlw_kg11_kao_mo2)
end subroutine f90wrap_rrlw_kg11__array__kao_mo2

subroutine f90wrap_rrlw_kg11__array__kbo_mo2(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg11, only: rrlw_kg11_kbo_mo2 => kbo_mo2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg11_kbo_mo2)
    dloc = loc(rrlw_kg11_kbo_mo2)
end subroutine f90wrap_rrlw_kg11__array__kbo_mo2

subroutine f90wrap_rrlw_kg11__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg11, only: rrlw_kg11_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg11_selfrefo)
    dloc = loc(rrlw_kg11_selfrefo)
end subroutine f90wrap_rrlw_kg11__array__selfrefo

subroutine f90wrap_rrlw_kg11__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg11, only: rrlw_kg11_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg11_forrefo)
    dloc = loc(rrlw_kg11_forrefo)
end subroutine f90wrap_rrlw_kg11__array__forrefo

subroutine f90wrap_rrlw_kg11__get__ng11(f90wrap_ng11)
    use rrlw_kg11, only: rrlw_kg11_ng11 => ng11
    implicit none
    integer(4), intent(out) :: f90wrap_ng11
    
    f90wrap_ng11 = rrlw_kg11_ng11
end subroutine f90wrap_rrlw_kg11__get__ng11

subroutine f90wrap_rrlw_kg11__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg11, only: rrlw_kg11_fracrefa => fracrefa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg11_fracrefa)
    dloc = loc(rrlw_kg11_fracrefa)
end subroutine f90wrap_rrlw_kg11__array__fracrefa

subroutine f90wrap_rrlw_kg11__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg11, only: rrlw_kg11_fracrefb => fracrefb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg11_fracrefb)
    dloc = loc(rrlw_kg11_fracrefb)
end subroutine f90wrap_rrlw_kg11__array__fracrefb

subroutine f90wrap_rrlw_kg11__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg11, only: rrlw_kg11_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg11_ka)
    dloc = loc(rrlw_kg11_ka)
end subroutine f90wrap_rrlw_kg11__array__ka

subroutine f90wrap_rrlw_kg11__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg11, only: rrlw_kg11_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg11_absa)
    dloc = loc(rrlw_kg11_absa)
end subroutine f90wrap_rrlw_kg11__array__absa

subroutine f90wrap_rrlw_kg11__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg11, only: rrlw_kg11_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg11_kb)
    dloc = loc(rrlw_kg11_kb)
end subroutine f90wrap_rrlw_kg11__array__kb

subroutine f90wrap_rrlw_kg11__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg11, only: rrlw_kg11_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg11_absb)
    dloc = loc(rrlw_kg11_absb)
end subroutine f90wrap_rrlw_kg11__array__absb

subroutine f90wrap_rrlw_kg11__array__ka_mo2(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg11, only: rrlw_kg11_ka_mo2 => ka_mo2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg11_ka_mo2)
    dloc = loc(rrlw_kg11_ka_mo2)
end subroutine f90wrap_rrlw_kg11__array__ka_mo2

subroutine f90wrap_rrlw_kg11__array__kb_mo2(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg11, only: rrlw_kg11_kb_mo2 => kb_mo2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg11_kb_mo2)
    dloc = loc(rrlw_kg11_kb_mo2)
end subroutine f90wrap_rrlw_kg11__array__kb_mo2

subroutine f90wrap_rrlw_kg11__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg11, only: rrlw_kg11_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg11_selfref)
    dloc = loc(rrlw_kg11_selfref)
end subroutine f90wrap_rrlw_kg11__array__selfref

subroutine f90wrap_rrlw_kg11__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg11, only: rrlw_kg11_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg11_forref)
    dloc = loc(rrlw_kg11_forref)
end subroutine f90wrap_rrlw_kg11__array__forref

! End of module rrlw_kg11 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg11.f90

