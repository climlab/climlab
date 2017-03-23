! Module rrlw_kg13 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90

subroutine f90wrap_rrlw_kg13__get__no13(f90wrap_no13)
    use rrlw_kg13, only: rrlw_kg13_no13 => no13
    implicit none
    integer(4), intent(out) :: f90wrap_no13
    
    f90wrap_no13 = rrlw_kg13_no13
end subroutine f90wrap_rrlw_kg13__get__no13

subroutine f90wrap_rrlw_kg13__array__fracrefbo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg13, only: rrlw_kg13_fracrefbo => fracrefbo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg13_fracrefbo)
    dloc = loc(rrlw_kg13_fracrefbo)
end subroutine f90wrap_rrlw_kg13__array__fracrefbo

subroutine f90wrap_rrlw_kg13__array__fracrefao(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg13, only: rrlw_kg13_fracrefao => fracrefao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg13_fracrefao)
    dloc = loc(rrlw_kg13_fracrefao)
end subroutine f90wrap_rrlw_kg13__array__fracrefao

subroutine f90wrap_rrlw_kg13__array__kao(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg13, only: rrlw_kg13_kao => kao
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg13_kao)
    dloc = loc(rrlw_kg13_kao)
end subroutine f90wrap_rrlw_kg13__array__kao

subroutine f90wrap_rrlw_kg13__array__kao_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg13, only: rrlw_kg13_kao_mco2 => kao_mco2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg13_kao_mco2)
    dloc = loc(rrlw_kg13_kao_mco2)
end subroutine f90wrap_rrlw_kg13__array__kao_mco2

subroutine f90wrap_rrlw_kg13__array__kao_mco(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg13, only: rrlw_kg13_kao_mco => kao_mco
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg13_kao_mco)
    dloc = loc(rrlw_kg13_kao_mco)
end subroutine f90wrap_rrlw_kg13__array__kao_mco

subroutine f90wrap_rrlw_kg13__array__kbo_mo3(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg13, only: rrlw_kg13_kbo_mo3 => kbo_mo3
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg13_kbo_mo3)
    dloc = loc(rrlw_kg13_kbo_mo3)
end subroutine f90wrap_rrlw_kg13__array__kbo_mo3

subroutine f90wrap_rrlw_kg13__array__selfrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg13, only: rrlw_kg13_selfrefo => selfrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg13_selfrefo)
    dloc = loc(rrlw_kg13_selfrefo)
end subroutine f90wrap_rrlw_kg13__array__selfrefo

subroutine f90wrap_rrlw_kg13__array__forrefo(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg13, only: rrlw_kg13_forrefo => forrefo
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg13_forrefo)
    dloc = loc(rrlw_kg13_forrefo)
end subroutine f90wrap_rrlw_kg13__array__forrefo

subroutine f90wrap_rrlw_kg13__get__ng13(f90wrap_ng13)
    use rrlw_kg13, only: rrlw_kg13_ng13 => ng13
    implicit none
    integer(4), intent(out) :: f90wrap_ng13
    
    f90wrap_ng13 = rrlw_kg13_ng13
end subroutine f90wrap_rrlw_kg13__get__ng13

subroutine f90wrap_rrlw_kg13__array__fracrefb(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg13, only: rrlw_kg13_fracrefb => fracrefb
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 12
    dshape(1:1) = shape(rrlw_kg13_fracrefb)
    dloc = loc(rrlw_kg13_fracrefb)
end subroutine f90wrap_rrlw_kg13__array__fracrefb

subroutine f90wrap_rrlw_kg13__array__fracrefa(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg13, only: rrlw_kg13_fracrefa => fracrefa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg13_fracrefa)
    dloc = loc(rrlw_kg13_fracrefa)
end subroutine f90wrap_rrlw_kg13__array__fracrefa

subroutine f90wrap_rrlw_kg13__array__ka(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg13, only: rrlw_kg13_ka => ka
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 4
    dtype = 12
    dshape(1:4) = shape(rrlw_kg13_ka)
    dloc = loc(rrlw_kg13_ka)
end subroutine f90wrap_rrlw_kg13__array__ka

subroutine f90wrap_rrlw_kg13__array__absa(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg13, only: rrlw_kg13_absa => absa
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg13_absa)
    dloc = loc(rrlw_kg13_absa)
end subroutine f90wrap_rrlw_kg13__array__absa

subroutine f90wrap_rrlw_kg13__array__ka_mco2(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg13, only: rrlw_kg13_ka_mco2 => ka_mco2
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg13_ka_mco2)
    dloc = loc(rrlw_kg13_ka_mco2)
end subroutine f90wrap_rrlw_kg13__array__ka_mco2

subroutine f90wrap_rrlw_kg13__array__ka_mco(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg13, only: rrlw_kg13_ka_mco => ka_mco
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 3
    dtype = 12
    dshape(1:3) = shape(rrlw_kg13_ka_mco)
    dloc = loc(rrlw_kg13_ka_mco)
end subroutine f90wrap_rrlw_kg13__array__ka_mco

subroutine f90wrap_rrlw_kg13__array__kb_mo3(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg13, only: rrlw_kg13_kb_mo3 => kb_mo3
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg13_kb_mo3)
    dloc = loc(rrlw_kg13_kb_mo3)
end subroutine f90wrap_rrlw_kg13__array__kb_mo3

subroutine f90wrap_rrlw_kg13__array__selfref(dummy_this, nd, dtype, dshape, &
    dloc)
    use rrlw_kg13, only: rrlw_kg13_selfref => selfref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg13_selfref)
    dloc = loc(rrlw_kg13_selfref)
end subroutine f90wrap_rrlw_kg13__array__selfref

subroutine f90wrap_rrlw_kg13__array__forref(dummy_this, nd, dtype, dshape, dloc)
    use rrlw_kg13, only: rrlw_kg13_forref => forref
    use parkind
    implicit none
    integer, intent(in) :: dummy_this(2)
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 12
    dshape(1:2) = shape(rrlw_kg13_forref)
    dloc = loc(rrlw_kg13_forref)
end subroutine f90wrap_rrlw_kg13__array__forref

! End of module rrlw_kg13 defined in file rrtmg_lw_v4.85/gcm_model/modules/rrlw_kg13.f90

