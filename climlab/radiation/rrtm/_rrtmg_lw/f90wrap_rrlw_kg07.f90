! Module rrlw_kg07 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90

subroutine f90wrap_rrlw_kg07__get__no7(f90wrap_no7)
    use rrlw_kg07, only: rrlw_kg07_no7 => no7
    implicit none
    integer(4), intent(out) :: f90wrap_no7
    
    f90wrap_no7 = rrlw_kg07_no7
end subroutine f90wrap_rrlw_kg07__get__no7

subroutine f90wrap_rrlw_kg07__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_fracrefbo => fracrefbo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg07_fracrefbo)
    dloc = loc(rrlw_kg07_fracrefbo)
end subroutine f90wrap_rrlw_kg07__array__fracrefbo

subroutine f90wrap_rrlw_kg07__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_fracrefao => fracrefao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg07_fracrefao)
    dloc = loc(rrlw_kg07_fracrefao)
end subroutine f90wrap_rrlw_kg07__array__fracrefao

subroutine f90wrap_rrlw_kg07__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_kao => kao
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg07_kao)
    dloc = loc(rrlw_kg07_kao)
end subroutine f90wrap_rrlw_kg07__array__kao

subroutine f90wrap_rrlw_kg07__array__kbo(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_kbo => kbo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg07_kbo)
    dloc = loc(rrlw_kg07_kbo)
end subroutine f90wrap_rrlw_kg07__array__kbo

subroutine f90wrap_rrlw_kg07__array__kao_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_kao_mco2 => kao_mco2
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg07_kao_mco2)
    dloc = loc(rrlw_kg07_kao_mco2)
end subroutine f90wrap_rrlw_kg07__array__kao_mco2

subroutine f90wrap_rrlw_kg07__array__kbo_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_kbo_mco2 => kbo_mco2
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg07_kbo_mco2)
    dloc = loc(rrlw_kg07_kbo_mco2)
end subroutine f90wrap_rrlw_kg07__array__kbo_mco2

subroutine f90wrap_rrlw_kg07__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_selfrefo => selfrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg07_selfrefo)
    dloc = loc(rrlw_kg07_selfrefo)
end subroutine f90wrap_rrlw_kg07__array__selfrefo

subroutine f90wrap_rrlw_kg07__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_forrefo => forrefo
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg07_forrefo)
    dloc = loc(rrlw_kg07_forrefo)
end subroutine f90wrap_rrlw_kg07__array__forrefo

subroutine f90wrap_rrlw_kg07__get__ng7(f90wrap_ng7)
    use rrlw_kg07, only: rrlw_kg07_ng7 => ng7
    implicit none
    integer(4), intent(out) :: f90wrap_ng7
    
    f90wrap_ng7 = rrlw_kg07_ng7
end subroutine f90wrap_rrlw_kg07__get__ng7

subroutine f90wrap_rrlw_kg07__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_fracrefb => fracrefb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg07_fracrefb)
    dloc = loc(rrlw_kg07_fracrefb)
end subroutine f90wrap_rrlw_kg07__array__fracrefb

subroutine f90wrap_rrlw_kg07__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_fracrefa => fracrefa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg07_fracrefa)
    dloc = loc(rrlw_kg07_fracrefa)
end subroutine f90wrap_rrlw_kg07__array__fracrefa

subroutine f90wrap_rrlw_kg07__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_ka => ka
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg07_ka)
    dloc = loc(rrlw_kg07_ka)
end subroutine f90wrap_rrlw_kg07__array__ka

subroutine f90wrap_rrlw_kg07__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_absa => absa
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg07_absa)
    dloc = loc(rrlw_kg07_absa)
end subroutine f90wrap_rrlw_kg07__array__absa

subroutine f90wrap_rrlw_kg07__array__kb(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_kb => kb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg07_kb)
    dloc = loc(rrlw_kg07_kb)
end subroutine f90wrap_rrlw_kg07__array__kb

subroutine f90wrap_rrlw_kg07__array__absb(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_absb => absb
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg07_absb)
    dloc = loc(rrlw_kg07_absb)
end subroutine f90wrap_rrlw_kg07__array__absb

subroutine f90wrap_rrlw_kg07__array__ka_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_ka_mco2 => ka_mco2
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg07_ka_mco2)
    dloc = loc(rrlw_kg07_ka_mco2)
end subroutine f90wrap_rrlw_kg07__array__ka_mco2

subroutine f90wrap_rrlw_kg07__array__kb_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_kb_mco2 => kb_mco2
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg07_kb_mco2)
    dloc = loc(rrlw_kg07_kb_mco2)
end subroutine f90wrap_rrlw_kg07__array__kb_mco2

subroutine f90wrap_rrlw_kg07__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_selfref => selfref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg07_selfref)
    dloc = loc(rrlw_kg07_selfref)
end subroutine f90wrap_rrlw_kg07__array__selfref

subroutine f90wrap_rrlw_kg07__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use parkind
    use rrlw_kg07, only: rrlw_kg07_forref => forref
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg07_forref)
    dloc = loc(rrlw_kg07_forref)
end subroutine f90wrap_rrlw_kg07__array__forref

! End of module rrlw_kg07 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg07.f90

