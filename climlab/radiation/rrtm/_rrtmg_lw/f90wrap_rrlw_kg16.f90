! Module rrlw_kg16 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90

subroutine f90wrap_rrlw_kg16__get__no16(f90wrap_no16)
    use rrlw_kg16, only: rrlw_kg16_no16 => no16
    implicit none
    integer(4), intent(out) :: f90wrap_no16
    
    f90wrap_no16 = rrlw_kg16_no16
end subroutine f90wrap_rrlw_kg16__get__no16

subroutine f90wrap_rrlw_kg16__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg16, only: rrlw_kg16_fracrefbo => fracrefbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg16_fracrefbo)
    dloc = loc(rrlw_kg16_fracrefbo)
end subroutine f90wrap_rrlw_kg16__array__fracrefbo

subroutine f90wrap_rrlw_kg16__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg16, only: rrlw_kg16_fracrefao => fracrefao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg16_fracrefao)
    dloc = loc(rrlw_kg16_fracrefao)
end subroutine f90wrap_rrlw_kg16__array__fracrefao

subroutine f90wrap_rrlw_kg16__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg16, only: rrlw_kg16_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg16_kao)
    dloc = loc(rrlw_kg16_kao)
end subroutine f90wrap_rrlw_kg16__array__kao

subroutine f90wrap_rrlw_kg16__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg16, only: rrlw_kg16_kbo => kbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg16_kbo)
    dloc = loc(rrlw_kg16_kbo)
end subroutine f90wrap_rrlw_kg16__array__kbo

subroutine f90wrap_rrlw_kg16__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg16, only: rrlw_kg16_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg16_selfrefo)
    dloc = loc(rrlw_kg16_selfrefo)
end subroutine f90wrap_rrlw_kg16__array__selfrefo

subroutine f90wrap_rrlw_kg16__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg16, only: rrlw_kg16_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg16_forrefo)
    dloc = loc(rrlw_kg16_forrefo)
end subroutine f90wrap_rrlw_kg16__array__forrefo

subroutine f90wrap_rrlw_kg16__get__ng16(f90wrap_ng16)
    use rrlw_kg16, only: rrlw_kg16_ng16 => ng16
    implicit none
    integer(4), intent(out) :: f90wrap_ng16
    
    f90wrap_ng16 = rrlw_kg16_ng16
end subroutine f90wrap_rrlw_kg16__get__ng16

subroutine f90wrap_rrlw_kg16__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg16, only: rrlw_kg16_fracrefb => fracrefb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg16_fracrefb)
    dloc = loc(rrlw_kg16_fracrefb)
end subroutine f90wrap_rrlw_kg16__array__fracrefb

subroutine f90wrap_rrlw_kg16__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg16, only: rrlw_kg16_fracrefa => fracrefa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg16_fracrefa)
    dloc = loc(rrlw_kg16_fracrefa)
end subroutine f90wrap_rrlw_kg16__array__fracrefa

subroutine f90wrap_rrlw_kg16__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg16, only: rrlw_kg16_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg16_ka)
    dloc = loc(rrlw_kg16_ka)
end subroutine f90wrap_rrlw_kg16__array__ka

subroutine f90wrap_rrlw_kg16__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg16, only: rrlw_kg16_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg16_absa)
    dloc = loc(rrlw_kg16_absa)
end subroutine f90wrap_rrlw_kg16__array__absa

subroutine f90wrap_rrlw_kg16__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg16, only: rrlw_kg16_kb => kb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg16_kb)
    dloc = loc(rrlw_kg16_kb)
end subroutine f90wrap_rrlw_kg16__array__kb

subroutine f90wrap_rrlw_kg16__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg16, only: rrlw_kg16_absb => absb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg16_absb)
    dloc = loc(rrlw_kg16_absb)
end subroutine f90wrap_rrlw_kg16__array__absb

subroutine f90wrap_rrlw_kg16__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg16, only: rrlw_kg16_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg16_selfref)
    dloc = loc(rrlw_kg16_selfref)
end subroutine f90wrap_rrlw_kg16__array__selfref

subroutine f90wrap_rrlw_kg16__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg16, only: rrlw_kg16_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg16_forref)
    dloc = loc(rrlw_kg16_forref)
end subroutine f90wrap_rrlw_kg16__array__forref

! End of module rrlw_kg16 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg16.f90

