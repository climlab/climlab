! Module rrlw_kg09 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90

subroutine f90wrap_rrlw_kg09__get__no9(f90wrap_no9)
    use rrlw_kg09, only: rrlw_kg09_no9 => no9
    implicit none
    integer(4), intent(out) :: f90wrap_no9
    
    f90wrap_no9 = rrlw_kg09_no9
end subroutine f90wrap_rrlw_kg09__get__no9

subroutine f90wrap_rrlw_kg09__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg09, only: rrlw_kg09_fracrefbo => fracrefbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg09_fracrefbo)
    dloc = loc(rrlw_kg09_fracrefbo)
end subroutine f90wrap_rrlw_kg09__array__fracrefbo

subroutine f90wrap_rrlw_kg09__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg09, only: rrlw_kg09_fracrefao => fracrefao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg09_fracrefao)
    dloc = loc(rrlw_kg09_fracrefao)
end subroutine f90wrap_rrlw_kg09__array__fracrefao

subroutine f90wrap_rrlw_kg09__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg09, only: rrlw_kg09_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg09_kao)
    dloc = loc(rrlw_kg09_kao)
end subroutine f90wrap_rrlw_kg09__array__kao

subroutine f90wrap_rrlw_kg09__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg09, only: rrlw_kg09_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg09_kbo)
    dloc = loc(rrlw_kg09_kbo)
end subroutine f90wrap_rrlw_kg09__array__kbo

subroutine f90wrap_rrlw_kg09__array__kao_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg09, only: rrlw_kg09_kao_mn2o => kao_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg09_kao_mn2o)
    dloc = loc(rrlw_kg09_kao_mn2o)
end subroutine f90wrap_rrlw_kg09__array__kao_mn2o

subroutine f90wrap_rrlw_kg09__array__kbo_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg09, only: rrlw_kg09_kbo_mn2o => kbo_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg09_kbo_mn2o)
    dloc = loc(rrlw_kg09_kbo_mn2o)
end subroutine f90wrap_rrlw_kg09__array__kbo_mn2o

subroutine f90wrap_rrlw_kg09__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg09, only: rrlw_kg09_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg09_selfrefo)
    dloc = loc(rrlw_kg09_selfrefo)
end subroutine f90wrap_rrlw_kg09__array__selfrefo

subroutine f90wrap_rrlw_kg09__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg09, only: rrlw_kg09_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg09_forrefo)
    dloc = loc(rrlw_kg09_forrefo)
end subroutine f90wrap_rrlw_kg09__array__forrefo

subroutine f90wrap_rrlw_kg09__get__ng9(f90wrap_ng9)
    use rrlw_kg09, only: rrlw_kg09_ng9 => ng9
    implicit none
    integer(4), intent(out) :: f90wrap_ng9
    
    f90wrap_ng9 = rrlw_kg09_ng9
end subroutine f90wrap_rrlw_kg09__get__ng9

subroutine f90wrap_rrlw_kg09__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg09, only: rrlw_kg09_fracrefb => fracrefb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg09_fracrefb)
    dloc = loc(rrlw_kg09_fracrefb)
end subroutine f90wrap_rrlw_kg09__array__fracrefb

subroutine f90wrap_rrlw_kg09__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg09, only: rrlw_kg09_fracrefa => fracrefa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg09_fracrefa)
    dloc = loc(rrlw_kg09_fracrefa)
end subroutine f90wrap_rrlw_kg09__array__fracrefa

subroutine f90wrap_rrlw_kg09__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg09, only: rrlw_kg09_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg09_ka)
    dloc = loc(rrlw_kg09_ka)
end subroutine f90wrap_rrlw_kg09__array__ka

subroutine f90wrap_rrlw_kg09__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg09, only: rrlw_kg09_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg09_absa)
    dloc = loc(rrlw_kg09_absa)
end subroutine f90wrap_rrlw_kg09__array__absa

subroutine f90wrap_rrlw_kg09__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg09, only: rrlw_kg09_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg09_kb)
    dloc = loc(rrlw_kg09_kb)
end subroutine f90wrap_rrlw_kg09__array__kb

subroutine f90wrap_rrlw_kg09__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg09, only: rrlw_kg09_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg09_absb)
    dloc = loc(rrlw_kg09_absb)
end subroutine f90wrap_rrlw_kg09__array__absb

subroutine f90wrap_rrlw_kg09__array__ka_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg09, only: rrlw_kg09_ka_mn2o => ka_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg09_ka_mn2o)
    dloc = loc(rrlw_kg09_ka_mn2o)
end subroutine f90wrap_rrlw_kg09__array__ka_mn2o

subroutine f90wrap_rrlw_kg09__array__kb_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg09, only: rrlw_kg09_kb_mn2o => kb_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg09_kb_mn2o)
    dloc = loc(rrlw_kg09_kb_mn2o)
end subroutine f90wrap_rrlw_kg09__array__kb_mn2o

subroutine f90wrap_rrlw_kg09__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg09, only: rrlw_kg09_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg09_selfref)
    dloc = loc(rrlw_kg09_selfref)
end subroutine f90wrap_rrlw_kg09__array__selfref

subroutine f90wrap_rrlw_kg09__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg09, only: rrlw_kg09_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg09_forref)
    dloc = loc(rrlw_kg09_forref)
end subroutine f90wrap_rrlw_kg09__array__forref

! End of module rrlw_kg09 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg09.f90

