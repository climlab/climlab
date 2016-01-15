module param_cldoptics

!---------------------------------------------------------------------------------
! Purpose:
!
! Interface module for the calculation of cloud optical properties
!
! Author: Byron Boville  Sept 06, 2002
!
!---------------------------------------------------------------------------------

   use shr_kind_mod,  only: r8=>shr_kind_r8
   use ppgrid,        only: pcols, pver
   use constituents,  only: cnst_get_ind

   use physconst,     only: gravit, latvap, zvir

   private
   public :: param_cldoptics_init, param_cldoptics_calc

! Local variables
   integer :: &
        ixcldice,           & ! cloud ice water index
        ixcldliq              ! cloud liquid water index

contains

!===============================================================================
  subroutine param_cldoptics_init()
!-----------------------------------------------------------------------
    use history,       only: addfld, add_default, phys_decomp

    implicit none
!-----------------------------------------------------------------------

! get index of (liquid+ice) cloud water
  call cnst_get_ind('CLDICE', ixcldice)
  call cnst_get_ind('CLDLIQ', ixcldliq)

! register history variables
    call addfld ('GCLDLWP ','gram/m2 ',pver, 'A','Grid-box cloud water path'             ,phys_decomp, sampling_seq='rad_lwsw')
    call addfld ('ICLDLWP ','gram/m2 ',pver, 'A','In-cloud cloud water path (liquid and ice)',phys_decomp, sampling_seq='rad_lwsw')
    call addfld ('TGCLDCWP','gram/m2 ',1,    'A','Total grid-box cloud water path (liquid and ice)',phys_decomp, &
                                                                                                           sampling_seq='rad_lwsw')
    call addfld ('TGCLDLWP','gram/m2 ',1,    'A','Total grid-box cloud liquid water path',phys_decomp, sampling_seq='rad_lwsw')
    call addfld ('TGCLDIWP','gram/m2 ',1,    'A','Total grid-box cloud ice water path'   ,phys_decomp, sampling_seq='rad_lwsw')
    call addfld ('EFFCLD  ','fraction',pver, 'A','Effective cloud fraction'              ,phys_decomp, sampling_seq='rad_lwsw')
    call addfld ('SETLWP  ','gram/m2 ',pver, 'A','Prescribed liquid water path'          ,phys_decomp, sampling_seq='rad_lwsw')
    call addfld ('LWSH    ','m       ',1,    'A','Liquid water scale height'             ,phys_decomp, sampling_seq='rad_lwsw')

    call addfld ('ICLDIWP', 'gram/m2', pver, 'A','In-cloud ice water path'               ,phys_decomp, sampling_seq='rad_lwsw')
    call addfld ('EMIS',    '1',       pver, 'A','cloud emissivity'                      ,phys_decomp, sampling_seq='rad_lwsw')
    call addfld ('REL',     'micron',  pver, 'A','effective liquid drop radius'          ,phys_decomp, sampling_seq='rad_lwsw')
    call addfld ('REI',     'micron',  pver, 'A','effective ice particle radius'         ,phys_decomp, sampling_seq='rad_lwsw')

    call add_default ('GCLDLWP ', 1, ' ')
    call add_default ('ICLDLWP ', 1, ' ')
    call add_default ('ICLDIWP', 1, ' ')
    call add_default ('TGCLDLWP', 1, ' ')
    call add_default ('TGCLDIWP', 1, ' ')

    return
  end subroutine param_cldoptics_init

!===============================================================================
  subroutine param_cldoptics_calc(state, cldn, landfrac, landm,icefrac, &
        cicewp, cliqwp, emis, rel, rei, pmxrgn, nmxrgn, snowh )
!
! Compute (liquid+ice) water path and cloud water/ice diagnostics
! *** soon this code will compute liquid and ice paths from input liquid and ice mixing ratios
! 
! **** mixes interface and physics code temporarily
!-----------------------------------------------------------------------
    use physics_types, only: physics_state
    use history,       only: outfld
    use pkg_cldoptics, only: cldefr, cldems, cldovrlap, cldclw

    implicit none

! Arguments
    type(physics_state), intent(in)  :: state        ! state variables
    real(r8), intent(in)  :: cldn(pcols,pver)        ! new cloud fraction
    real(r8), intent(in)  :: landfrac(pcols)         ! Land fraction
    real(r8), intent(in)  :: icefrac(pcols)          ! Ice fraction
    real(r8), intent(in)  :: landm(pcols)            ! Land fraction ramped
    real(r8), intent(in) :: snowh(pcols)         ! Snow depth over land, water equivalent (m)

!!$    real(r8), intent(out) :: cwp   (pcols,pver)      ! in-cloud cloud (total) water path
    real(r8), intent(out) :: cicewp(pcols,pver)      ! in-cloud cloud ice water path
    real(r8), intent(out) :: cliqwp(pcols,pver)      ! in-cloud cloud liquid water path
    real(r8), intent(out) :: emis  (pcols,pver)      ! cloud emissivity
    real(r8), intent(in) :: rel   (pcols,pver)      ! effective drop radius (microns)
    real(r8), intent(in) :: rei   (pcols,pver)      ! ice effective drop size (microns)
    real(r8), intent(out) :: pmxrgn(pcols,pver+1)    ! Maximum values of pressure for each
    integer , intent(out) :: nmxrgn(pcols)           ! Number of maximally overlapped regions

! Local variables
    real(r8) :: cwp   (pcols,pver)      ! in-cloud cloud (total) water path
!!$    real(r8) :: cicewp(pcols,pver)      ! in-cloud cloud ice water path
!!$    real(r8) :: cliqwp(pcols,pver)      ! in-cloud cloud liquid water path
    real(r8) :: effcld(pcols,pver)                   ! effective cloud=cld*emis
    real(r8) :: gicewp(pcols,pver)                   ! grid-box cloud ice water path
    real(r8) :: gliqwp(pcols,pver)                   ! grid-box cloud liquid water path
    real(r8) :: gwp   (pcols,pver)                   ! grid-box cloud (total) water path
    real(r8) :: hl     (pcols)                       ! Liquid water scale height
    real(r8) :: tgicewp(pcols)                       ! Vertically integrated ice water path
    real(r8) :: tgliqwp(pcols)                       ! Vertically integrated liquid water path
    real(r8) :: tgwp   (pcols)                       ! Vertically integrated (total) cloud water path
    real(r8) :: tpw    (pcols)                       ! total precipitable water
    real(r8) :: clwpold(pcols,pver)                  ! Presribed cloud liq. h2o path
    real(r8) :: ficemr (pcols,pver)                  ! Ice fraction from ice and liquid mixing ratios

    real(r8) :: rgrav                ! inverse gravitational acceleration

    integer :: i,k                                   ! loop indexes
    integer :: ncol, lchnk


!-----------------------------------------------------------------------
    ncol  = state%ncol
    lchnk = state%lchnk

! Compute liquid and ice water paths
    tgicewp(:ncol) = 0.
    tgliqwp(:ncol) = 0.
    do k=1,pver
       do i = 1,ncol
          gicewp(i,k) = state%q(i,k,ixcldice)*state%pdel(i,k)/gravit*1000.0  ! Grid box ice water path.
          gliqwp(i,k) = state%q(i,k,ixcldliq)*state%pdel(i,k)/gravit*1000.0  ! Grid box liquid water path.
!!$          gwp   (i,k) = gicewp(i,k) + gliqwp(i,k)
          cicewp(i,k) = gicewp(i,k) / max(0.01_r8,cldn(i,k))                 ! In-cloud ice water path.
          cliqwp(i,k) = gliqwp(i,k) / max(0.01_r8,cldn(i,k))                 ! In-cloud liquid water path.
!!$          cwp   (i,k) = gwp   (i,k) / max(0.01_r8,cldn(i,k))
          ficemr(i,k) = state%q(i,k,ixcldice) /                 &
               max(1.e-10_r8,(state%q(i,k,ixcldice)+state%q(i,k,ixcldliq)))
          
          tgicewp(i)  = tgicewp(i) + gicewp(i,k)
          tgliqwp(i)  = tgliqwp(i) + gliqwp(i,k)
       end do
    end do
    tgwp(:ncol) = tgicewp(:ncol) + tgliqwp(:ncol)
    gwp(:ncol,:pver) = gicewp(:ncol,:pver) + gliqwp(:ncol,:pver) 
    cwp(:ncol,:pver) = cicewp(:ncol,:pver) + cliqwp(:ncol,:pver) 

! Compute total preciptable water in column (in mm)
    tpw(:ncol) = 0.0
    rgrav = 1.0/gravit
    do k=1,pver
       do i=1,ncol
          tpw(i) = tpw(i) + state%pdel(i,k)*state%q(i,k,1)*rgrav
       end do
    end do

! Diagnostic liquid water path (old specified form)
    call cldclw(lchnk, ncol, state%zi, clwpold, tpw, hl)


! Cloud emissivity.
    call cldems(lchnk, ncol, cwp, ficemr, rei, emis)

! Effective cloud cover
    do k=1,pver
       do i=1,ncol
          effcld(i,k) = cldn(i,k)*emis(i,k)
       end do
    end do

! Determine parameters for maximum/random overlap
    call cldovrlap(lchnk, ncol, state%pint, cldn, nmxrgn, pmxrgn)

    call outfld('GCLDLWP' ,gwp    , pcols,lchnk)
    call outfld('TGCLDCWP',tgwp   , pcols,lchnk)
    call outfld('TGCLDLWP',tgliqwp, pcols,lchnk)
    call outfld('TGCLDIWP',tgicewp, pcols,lchnk)
    call outfld('ICLDLWP' ,cwp    , pcols,lchnk)
    call outfld('ICLDIWP' ,cicewp , pcols,lchnk)
    call outfld('SETLWP'  ,clwpold, pcols,lchnk)
    call outfld('EFFCLD'  ,effcld , pcols,lchnk)
    call outfld('LWSH'    ,hl     , pcols,lchnk)
    call outfld('EMIS'    ,emis   , pcols,lchnk)
    call outfld('REL'     ,rel    , pcols,lchnk)
    call outfld('REI'     ,rei    , pcols,lchnk)

  end subroutine param_cldoptics_calc


end module param_cldoptics
