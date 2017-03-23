! Module rrlw_kg12 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90

subroutine f90wrap_rrlw_kg12__get__no12(f90wrap_no12)
    use rrlw_kg12, only: rrlw_kg12_no12 => no12
    implicit none
    integer(4), intent(out) :: f90wrap_no12
    
    f90wrap_no12 = rrlw_kg12_no12
end subroutine f90wrap_rrlw_kg12__get__no12

subroutine f90wrap_rrlw_kg12__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg12, only: rrlw_kg12_fracrefao => fracrefao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg12_fracrefao)
    dloc = loc(rrlw_kg12_fracrefao)
end subroutine f90wrap_rrlw_kg12__array__fracrefao

subroutine f90wrap_rrlw_kg12__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg12, only: rrlw_kg12_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg12_kao)
    dloc = loc(rrlw_kg12_kao)
end subroutine f90wrap_rrlw_kg12__array__kao

subroutine f90wrap_rrlw_kg12__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg12, only: rrlw_kg12_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg12_selfrefo)
    dloc = loc(rrlw_kg12_selfrefo)
end subroutine f90wrap_rrlw_kg12__array__selfrefo

subroutine f90wrap_rrlw_kg12__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg12, only: rrlw_kg12_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg12_forrefo)
    dloc = loc(rrlw_kg12_forrefo)
end subroutine f90wrap_rrlw_kg12__array__forrefo

subroutine f90wrap_rrlw_kg12__get__ng12(f90wrap_ng12)
    use rrlw_kg12, only: rrlw_kg12_ng12 => ng12
    implicit none
    integer(4), intent(out) :: f90wrap_ng12
    
    f90wrap_ng12 = rrlw_kg12_ng12
end subroutine f90wrap_rrlw_kg12__get__ng12

subroutine f90wrap_rrlw_kg12__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg12, only: rrlw_kg12_fracrefa => fracrefa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg12_fracrefa)
    dloc = loc(rrlw_kg12_fracrefa)
end subroutine f90wrap_rrlw_kg12__array__fracrefa

subroutine f90wrap_rrlw_kg12__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg12, only: rrlw_kg12_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg12_ka)
    dloc = loc(rrlw_kg12_ka)
end subroutine f90wrap_rrlw_kg12__array__ka

subroutine f90wrap_rrlw_kg12__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg12, only: rrlw_kg12_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg12_absa)
    dloc = loc(rrlw_kg12_absa)
end subroutine f90wrap_rrlw_kg12__array__absa

subroutine f90wrap_rrlw_kg12__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg12, only: rrlw_kg12_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg12_selfref)
    dloc = loc(rrlw_kg12_selfref)
end subroutine f90wrap_rrlw_kg12__array__selfref

subroutine f90wrap_rrlw_kg12__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg12, only: rrlw_kg12_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg12_forref)
    dloc = loc(rrlw_kg12_forref)
end subroutine f90wrap_rrlw_kg12__array__forref

! End of module rrlw_kg12 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg12.f90

