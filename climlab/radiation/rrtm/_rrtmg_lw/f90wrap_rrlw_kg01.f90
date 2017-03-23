! Module rrlw_kg01 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90

subroutine f90wrap_rrlw_kg01__get__no1(f90wrap_no1)
    use rrlw_kg01, only: rrlw_kg01_no1 => no1
    implicit none
    integer(4), intent(out) :: f90wrap_no1
    
    f90wrap_no1 = rrlw_kg01_no1
end subroutine f90wrap_rrlw_kg01__get__no1

subroutine f90wrap_rrlw_kg01__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg01, only: rrlw_kg01_fracrefao => fracrefao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg01_fracrefao)
    dloc = loc(rrlw_kg01_fracrefao)
end subroutine f90wrap_rrlw_kg01__array__fracrefao

subroutine f90wrap_rrlw_kg01__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg01, only: rrlw_kg01_fracrefbo => fracrefbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg01_fracrefbo)
    dloc = loc(rrlw_kg01_fracrefbo)
end subroutine f90wrap_rrlw_kg01__array__fracrefbo

subroutine f90wrap_rrlw_kg01__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg01, only: rrlw_kg01_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg01_kao)
    dloc = loc(rrlw_kg01_kao)
end subroutine f90wrap_rrlw_kg01__array__kao

subroutine f90wrap_rrlw_kg01__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg01, only: rrlw_kg01_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg01_kbo)
    dloc = loc(rrlw_kg01_kbo)
end subroutine f90wrap_rrlw_kg01__array__kbo

subroutine f90wrap_rrlw_kg01__array__kao_mn2(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg01, only: rrlw_kg01_kao_mn2 => kao_mn2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg01_kao_mn2)
    dloc = loc(rrlw_kg01_kao_mn2)
end subroutine f90wrap_rrlw_kg01__array__kao_mn2

subroutine f90wrap_rrlw_kg01__array__kbo_mn2(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg01, only: rrlw_kg01_kbo_mn2 => kbo_mn2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg01_kbo_mn2)
    dloc = loc(rrlw_kg01_kbo_mn2)
end subroutine f90wrap_rrlw_kg01__array__kbo_mn2

subroutine f90wrap_rrlw_kg01__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg01, only: rrlw_kg01_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg01_selfrefo)
    dloc = loc(rrlw_kg01_selfrefo)
end subroutine f90wrap_rrlw_kg01__array__selfrefo

subroutine f90wrap_rrlw_kg01__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg01, only: rrlw_kg01_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg01_forrefo)
    dloc = loc(rrlw_kg01_forrefo)
end subroutine f90wrap_rrlw_kg01__array__forrefo

subroutine f90wrap_rrlw_kg01__get__ng1(f90wrap_ng1)
    use rrlw_kg01, only: rrlw_kg01_ng1 => ng1
    implicit none
    integer(4), intent(out) :: f90wrap_ng1
    
    f90wrap_ng1 = rrlw_kg01_ng1
end subroutine f90wrap_rrlw_kg01__get__ng1

subroutine f90wrap_rrlw_kg01__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg01, only: rrlw_kg01_fracrefa => fracrefa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg01_fracrefa)
    dloc = loc(rrlw_kg01_fracrefa)
end subroutine f90wrap_rrlw_kg01__array__fracrefa

subroutine f90wrap_rrlw_kg01__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg01, only: rrlw_kg01_fracrefb => fracrefb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg01_fracrefb)
    dloc = loc(rrlw_kg01_fracrefb)
end subroutine f90wrap_rrlw_kg01__array__fracrefb

subroutine f90wrap_rrlw_kg01__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg01, only: rrlw_kg01_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg01_ka)
    dloc = loc(rrlw_kg01_ka)
end subroutine f90wrap_rrlw_kg01__array__ka

subroutine f90wrap_rrlw_kg01__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg01, only: rrlw_kg01_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg01_absa)
    dloc = loc(rrlw_kg01_absa)
end subroutine f90wrap_rrlw_kg01__array__absa

subroutine f90wrap_rrlw_kg01__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg01, only: rrlw_kg01_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg01_kb)
    dloc = loc(rrlw_kg01_kb)
end subroutine f90wrap_rrlw_kg01__array__kb

subroutine f90wrap_rrlw_kg01__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg01, only: rrlw_kg01_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg01_absb)
    dloc = loc(rrlw_kg01_absb)
end subroutine f90wrap_rrlw_kg01__array__absb

subroutine f90wrap_rrlw_kg01__array__ka_mn2(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg01, only: rrlw_kg01_ka_mn2 => ka_mn2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg01_ka_mn2)
    dloc = loc(rrlw_kg01_ka_mn2)
end subroutine f90wrap_rrlw_kg01__array__ka_mn2

subroutine f90wrap_rrlw_kg01__array__kb_mn2(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg01, only: rrlw_kg01_kb_mn2 => kb_mn2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg01_kb_mn2)
    dloc = loc(rrlw_kg01_kb_mn2)
end subroutine f90wrap_rrlw_kg01__array__kb_mn2

subroutine f90wrap_rrlw_kg01__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg01, only: rrlw_kg01_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg01_selfref)
    dloc = loc(rrlw_kg01_selfref)
end subroutine f90wrap_rrlw_kg01__array__selfref

subroutine f90wrap_rrlw_kg01__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg01, only: rrlw_kg01_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg01_forref)
    dloc = loc(rrlw_kg01_forref)
end subroutine f90wrap_rrlw_kg01__array__forref

! End of module rrlw_kg01 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg01.f90

