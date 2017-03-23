! Module rrlw_kg15 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90

subroutine f90wrap_rrlw_kg15__get__no15(f90wrap_no15)
    use rrlw_kg15, only: rrlw_kg15_no15 => no15
    implicit none
    integer(4), intent(out) :: f90wrap_no15
    
    f90wrap_no15 = rrlw_kg15_no15
end subroutine f90wrap_rrlw_kg15__get__no15

subroutine f90wrap_rrlw_kg15__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg15, only: rrlw_kg15_fracrefao => fracrefao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg15_fracrefao)
    dloc = loc(rrlw_kg15_fracrefao)
end subroutine f90wrap_rrlw_kg15__array__fracrefao

subroutine f90wrap_rrlw_kg15__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg15, only: rrlw_kg15_kao => kao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg15_kao)
    dloc = loc(rrlw_kg15_kao)
end subroutine f90wrap_rrlw_kg15__array__kao

subroutine f90wrap_rrlw_kg15__array__kao_mn2(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg15, only: rrlw_kg15_kao_mn2 => kao_mn2
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg15_kao_mn2)
    dloc = loc(rrlw_kg15_kao_mn2)
end subroutine f90wrap_rrlw_kg15__array__kao_mn2

subroutine f90wrap_rrlw_kg15__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg15, only: rrlw_kg15_selfrefo => selfrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg15_selfrefo)
    dloc = loc(rrlw_kg15_selfrefo)
end subroutine f90wrap_rrlw_kg15__array__selfrefo

subroutine f90wrap_rrlw_kg15__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg15, only: rrlw_kg15_forrefo => forrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg15_forrefo)
    dloc = loc(rrlw_kg15_forrefo)
end subroutine f90wrap_rrlw_kg15__array__forrefo

subroutine f90wrap_rrlw_kg15__get__ng15(f90wrap_ng15)
    use rrlw_kg15, only: rrlw_kg15_ng15 => ng15
    implicit none
    integer(4), intent(out) :: f90wrap_ng15
    
    f90wrap_ng15 = rrlw_kg15_ng15
end subroutine f90wrap_rrlw_kg15__get__ng15

subroutine f90wrap_rrlw_kg15__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg15, only: rrlw_kg15_fracrefa => fracrefa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg15_fracrefa)
    dloc = loc(rrlw_kg15_fracrefa)
end subroutine f90wrap_rrlw_kg15__array__fracrefa

subroutine f90wrap_rrlw_kg15__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg15, only: rrlw_kg15_ka => ka
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg15_ka)
    dloc = loc(rrlw_kg15_ka)
end subroutine f90wrap_rrlw_kg15__array__ka

subroutine f90wrap_rrlw_kg15__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg15, only: rrlw_kg15_absa => absa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg15_absa)
    dloc = loc(rrlw_kg15_absa)
end subroutine f90wrap_rrlw_kg15__array__absa

subroutine f90wrap_rrlw_kg15__array__ka_mn2(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg15, only: rrlw_kg15_ka_mn2 => ka_mn2
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg15_ka_mn2)
    dloc = loc(rrlw_kg15_ka_mn2)
end subroutine f90wrap_rrlw_kg15__array__ka_mn2

subroutine f90wrap_rrlw_kg15__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg15, only: rrlw_kg15_selfref => selfref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg15_selfref)
    dloc = loc(rrlw_kg15_selfref)
end subroutine f90wrap_rrlw_kg15__array__selfref

subroutine f90wrap_rrlw_kg15__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg15, only: rrlw_kg15_forref => forref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg15_forref)
    dloc = loc(rrlw_kg15_forref)
end subroutine f90wrap_rrlw_kg15__array__forref

! End of module rrlw_kg15 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg15.f90

