! Module rrlw_kg03 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90

subroutine f90wrap_rrlw_kg03__get__no3(f90wrap_no3)
    use rrlw_kg03, only: rrlw_kg03_no3 => no3
    implicit none
    integer(4), intent(out) :: f90wrap_no3
    
    f90wrap_no3 = rrlw_kg03_no3
end subroutine f90wrap_rrlw_kg03__get__no3

subroutine f90wrap_rrlw_kg03__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg03, only: rrlw_kg03_fracrefao => fracrefao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg03_fracrefao)
    dloc = loc(rrlw_kg03_fracrefao)
end subroutine f90wrap_rrlw_kg03__array__fracrefao

subroutine f90wrap_rrlw_kg03__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg03, only: rrlw_kg03_fracrefbo => fracrefbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg03_fracrefbo)
    dloc = loc(rrlw_kg03_fracrefbo)
end subroutine f90wrap_rrlw_kg03__array__fracrefbo

subroutine f90wrap_rrlw_kg03__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg03, only: rrlw_kg03_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg03_kao)
    dloc = loc(rrlw_kg03_kao)
end subroutine f90wrap_rrlw_kg03__array__kao

subroutine f90wrap_rrlw_kg03__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg03, only: rrlw_kg03_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg03_kbo)
    dloc = loc(rrlw_kg03_kbo)
end subroutine f90wrap_rrlw_kg03__array__kbo

subroutine f90wrap_rrlw_kg03__array__kao_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg03, only: rrlw_kg03_kao_mn2o => kao_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg03_kao_mn2o)
    dloc = loc(rrlw_kg03_kao_mn2o)
end subroutine f90wrap_rrlw_kg03__array__kao_mn2o

subroutine f90wrap_rrlw_kg03__array__kbo_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg03, only: rrlw_kg03_kbo_mn2o => kbo_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg03_kbo_mn2o)
    dloc = loc(rrlw_kg03_kbo_mn2o)
end subroutine f90wrap_rrlw_kg03__array__kbo_mn2o

subroutine f90wrap_rrlw_kg03__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg03, only: rrlw_kg03_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg03_selfrefo)
    dloc = loc(rrlw_kg03_selfrefo)
end subroutine f90wrap_rrlw_kg03__array__selfrefo

subroutine f90wrap_rrlw_kg03__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg03, only: rrlw_kg03_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg03_forrefo)
    dloc = loc(rrlw_kg03_forrefo)
end subroutine f90wrap_rrlw_kg03__array__forrefo

subroutine f90wrap_rrlw_kg03__get__ng3(f90wrap_ng3)
    use rrlw_kg03, only: rrlw_kg03_ng3 => ng3
    implicit none
    integer(4), intent(out) :: f90wrap_ng3
    
    f90wrap_ng3 = rrlw_kg03_ng3
end subroutine f90wrap_rrlw_kg03__get__ng3

subroutine f90wrap_rrlw_kg03__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg03, only: rrlw_kg03_fracrefa => fracrefa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg03_fracrefa)
    dloc = loc(rrlw_kg03_fracrefa)
end subroutine f90wrap_rrlw_kg03__array__fracrefa

subroutine f90wrap_rrlw_kg03__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg03, only: rrlw_kg03_fracrefb => fracrefb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg03_fracrefb)
    dloc = loc(rrlw_kg03_fracrefb)
end subroutine f90wrap_rrlw_kg03__array__fracrefb

subroutine f90wrap_rrlw_kg03__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg03, only: rrlw_kg03_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg03_ka)
    dloc = loc(rrlw_kg03_ka)
end subroutine f90wrap_rrlw_kg03__array__ka

subroutine f90wrap_rrlw_kg03__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg03, only: rrlw_kg03_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg03_absa)
    dloc = loc(rrlw_kg03_absa)
end subroutine f90wrap_rrlw_kg03__array__absa

subroutine f90wrap_rrlw_kg03__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg03, only: rrlw_kg03_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg03_kb)
    dloc = loc(rrlw_kg03_kb)
end subroutine f90wrap_rrlw_kg03__array__kb

subroutine f90wrap_rrlw_kg03__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg03, only: rrlw_kg03_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg03_absb)
    dloc = loc(rrlw_kg03_absb)
end subroutine f90wrap_rrlw_kg03__array__absb

subroutine f90wrap_rrlw_kg03__array__ka_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg03, only: rrlw_kg03_ka_mn2o => ka_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg03_ka_mn2o)
    dloc = loc(rrlw_kg03_ka_mn2o)
end subroutine f90wrap_rrlw_kg03__array__ka_mn2o

subroutine f90wrap_rrlw_kg03__array__kb_mn2o(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg03, only: rrlw_kg03_kb_mn2o => kb_mn2o
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg03_kb_mn2o)
    dloc = loc(rrlw_kg03_kb_mn2o)
end subroutine f90wrap_rrlw_kg03__array__kb_mn2o

subroutine f90wrap_rrlw_kg03__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg03, only: rrlw_kg03_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg03_selfref)
    dloc = loc(rrlw_kg03_selfref)
end subroutine f90wrap_rrlw_kg03__array__selfref

subroutine f90wrap_rrlw_kg03__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg03, only: rrlw_kg03_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg03_forref)
    dloc = loc(rrlw_kg03_forref)
end subroutine f90wrap_rrlw_kg03__array__forref

! End of module rrlw_kg03 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg03.f90

