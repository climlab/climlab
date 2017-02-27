module pkg_cldoptics

!---------------------------------------------------------------------------------
! Purpose:
!
! Compute cloud optical properties: liquid and ice partical size; emissivity
!
! Author: Byron Boville  Sept 06, 2002, assembled from existing subroutines
!
!---------------------------------------------------------------------------------

  use shr_kind_mod,  only: r8=>shr_kind_r8
  !use ppgrid,        only: pcols, pver, pverp

  private
  public :: cldefr, cldems, cldovrlap, cldclw, reitab, reltab

contains

!===============================================================================
  subroutine cldefr(pcols, pver, pverp, lchnk   ,ncol    , &
       landfrac,t       ,rel     ,rei     ,ps      ,pmid    , landm, icefrac, snowh)
!-----------------------------------------------------------------------
!
! Purpose:
! Compute cloud water and ice particle size
!
! Method:
! use empirical formulas to construct effective radii
!
! Author: J.T. Kiehl, B. A. Boville, P. Rasch
!
!-----------------------------------------------------------------------

    implicit none
!------------------------------Arguments--------------------------------
!
!  CLIMLAB now passing grid dimensions as input
    integer, intent(in) ::   pcols
    integer, intent(in) ::   pver
    integer, intent(in) ::   pverp
! Input arguments
!
    integer, intent(in) :: lchnk                 ! chunk identifier
    integer, intent(in) :: ncol                  ! number of atmospheric columns

    real(r8), intent(in) :: landfrac(pcols)      ! Land fraction
    real(r8), intent(in) :: icefrac(pcols)       ! Ice fraction
    real(r8), intent(in) :: t(pcols,pver)        ! Temperature
    real(r8), intent(in) :: ps(pcols)            ! Surface pressure
    real(r8), intent(in) :: pmid(pcols,pver)     ! Midpoint pressures
    real(r8), intent(in) :: landm(pcols)
    real(r8), intent(in) :: snowh(pcols)         ! Snow depth over land, water equivalent (m)
!
! Output arguments
!
    real(r8), intent(out) :: rel(pcols,pver)      ! Liquid effective drop size (microns)
    real(r8), intent(out) :: rei(pcols,pver)      ! Ice effective drop size (microns)
!

!++pjr
! following Kiehl
         call reltab(pcols, pver, pverp, ncol, t, landfrac, landm, icefrac, rel, snowh)

! following Kristjansson and Mitchell
         call reitab(pcols, pver, pverp, ncol, t, rei)
!--pjr
!
!
    return
  end subroutine cldefr

!===============================================================================
  subroutine cldems(pcols, pver, pverp, lchnk   ,ncol    ,clwp    ,fice    , &
        rei     ,emis    )
!-----------------------------------------------------------------------
!
! Purpose:
! Compute cloud emissivity using cloud liquid water path (g/m**2)
!
! Method:
! <Describe the algorithm(s) used in the routine.>
! <Also include any applicable external references.>
!
! Author: J.T. Kiehl
!
!-----------------------------------------------------------------------

    implicit none
!------------------------------Parameters-------------------------------
!
    real(r8) kabsl                  ! longwave liquid absorption coeff (m**2/g)
    parameter (kabsl = 0.090361)
!
!------------------------------Arguments--------------------------------
!
!  CLIMLAB now passing grid dimensions as input
    integer, intent(in) ::   pcols
    integer, intent(in) ::   pver
    integer, intent(in) ::   pverp
! Input arguments
!
    integer, intent(in) :: lchnk                   ! chunk identifier
    integer, intent(in) :: ncol                    ! number of atmospheric columns

    real(r8), intent(in) :: clwp(pcols,pver)       ! cloud liquid water path (g/m**2)
    real(r8), intent(in) :: rei(pcols,pver)        ! ice effective drop size (microns)
    real(r8), intent(in) :: fice(pcols,pver)       ! fractional ice content within cloud
!
! Output arguments
!
    real(r8), intent(out) :: emis(pcols,pver)       ! cloud emissivity (fraction)
!
!---------------------------Local workspace-----------------------------
!
    integer i,k                 ! longitude, level indices
    real(r8) kabs                   ! longwave absorption coeff (m**2/g)
    real(r8) kabsi                  ! ice absorption coefficient
!
!-----------------------------------------------------------------------
!
    do k=1,pver
       do i=1,ncol
          kabsi = 0.005 + 1./rei(i,k)
          kabs = kabsl*(1.-fice(i,k)) + kabsi*fice(i,k)
          emis(i,k) = 1. - exp(-1.66*kabs*clwp(i,k))
       end do
    end do
!
    return
  end subroutine cldems

!===============================================================================
  subroutine cldovrlap(pcols, pver, pverp, lchnk   ,ncol    ,pint    , &
       cld     ,nmxrgn  ,pmxrgn  )
!-----------------------------------------------------------------------
!
! Purpose:
! Partitions each column into regions with clouds in neighboring layers.
! This information is used to implement maximum overlap in these regions
! with random overlap between them.
! On output,
!    nmxrgn contains the number of regions in each column
!    pmxrgn contains the interface pressures for the lower boundaries of
!           each region!
! Method:

!
! Author: W. Collins
!
!-----------------------------------------------------------------------

    implicit none
!
! Input arguments
!
!  CLIMLAB now passing grid dimensions as input
    integer, intent(in) ::   pcols
    integer, intent(in) ::   pver
    integer, intent(in) ::   pverp
!
    integer, intent(in) :: lchnk                ! chunk identifier
    integer, intent(in) :: ncol                 ! number of atmospheric columns

    real(r8), intent(in) :: pint(pcols,pverp)   ! Interface pressure
    real(r8), intent(in) :: cld(pcols,pver)     ! Fractional cloud cover
!
! Output arguments
!
    real(r8), intent(out) :: pmxrgn(pcols,pverp)! Maximum values of pressure for each
!    maximally overlapped region.
!    0->pmxrgn(i,1) is range of pressure for
!    1st region,pmxrgn(i,1)->pmxrgn(i,2) for
!    2nd region, etc
    integer nmxrgn(pcols)                    ! Number of maximally overlapped regions
!
!---------------------------Local variables-----------------------------
!
    integer i                    ! Longitude index
    integer k                    ! Level index
    integer n                    ! Max-overlap region counter

    real(r8) pnm(pcols,pverp)    ! Interface pressure

    logical cld_found            ! Flag for detection of cloud
    logical cld_layer(pver)      ! Flag for cloud in layer
!
!------------------------------------------------------------------------
!

    do i = 1, ncol
       cld_found = .false.
       cld_layer(:) = cld(i,:) > 0.0_r8
       pmxrgn(i,:) = 0.0
       pnm(i,:)=pint(i,:)*10.
       n = 1
       do k = 1, pver
          if (cld_layer(k) .and.  .not. cld_found) then
             cld_found = .true.
          else if ( .not. cld_layer(k) .and. cld_found) then
             cld_found = .false.
             if (count(cld_layer(k:pver)) == 0) then
                exit
             endif
             pmxrgn(i,n) = pnm(i,k)
             n = n + 1
          endif
       end do
       pmxrgn(i,n) = pnm(i,pverp)
       nmxrgn(i) = n
    end do

    return
  end subroutine cldovrlap

!===============================================================================
  subroutine cldclw(pcols, pver, pverp, lchnk   ,ncol    ,zi      , &
       clwp    ,tpw     ,hl      )
!-----------------------------------------------------------------------
!
! Purpose:
! Evaluate cloud liquid water path clwp (g/m**2)
!
! Method:
! <Describe the algorithm(s) used in the routine.>
! <Also include any applicable external references.>
!
! Author: J.T. Kiehl
!
!-----------------------------------------------------------------------

    implicit none

!
! Input arguments
!
!  CLIMLAB now passing grid dimensions as input
    integer, intent(in) ::   pcols
    integer, intent(in) ::   pver
    integer, intent(in) ::   pverp
!
    integer, intent(in) :: lchnk                 ! chunk identifier
    integer, intent(in) :: ncol                  ! number of atmospheric columns

    real(r8), intent(in) :: zi(pcols,pverp)      ! height at layer interfaces(m)
    real(r8), intent(in) :: tpw(pcols)           ! total precipitable water (mm)
!
! Output arguments
!
    real(r8) clwp(pcols,pver)     ! cloud liquid water path (g/m**2)
    real(r8) hl(pcols)            ! liquid water scale height
    real(r8) rhl(pcols)           ! 1/hl

!
!---------------------------Local workspace-----------------------------
!
    integer i,k               ! longitude, level indices
    real(r8) clwc0                ! reference liquid water concentration (g/m**3)
    real(r8) emziohl(pcols,pverp) ! exp(-zi/hl)
!
!-----------------------------------------------------------------------
!
! Set reference liquid water concentration
!
    clwc0 = 0.21
!
! Diagnose liquid water scale height from precipitable water
!
    do i=1,ncol
       hl(i)  = 700.0*log(max(tpw(i)+1.0_r8,1.0_r8))
       rhl(i) = 1.0/hl(i)
    end do
!
! Evaluate cloud liquid water path (vertical integral of exponential fn)
!
    do k=1,pverp
       do i=1,ncol
          emziohl(i,k) = exp(-zi(i,k)*rhl(i))
       end do
    end do
    do k=1,pver
       do i=1,ncol
          clwp(i,k) = clwc0*hl(i)*(emziohl(i,k+1) - emziohl(i,k))
       end do
    end do
!
    return
  end subroutine cldclw


!===============================================================================
  subroutine reltab(pcols, pver, pverp, ncol, t, landfrac, landm, icefrac, &
        rel, snowh)
!-----------------------------------------------------------------------
!
! Purpose:
! Compute cloud water size
!
! Method:
! analytic formula following the formulation originally developed by J. T. Kiehl
!
! Author: Phil Rasch
!
!-----------------------------------------------------------------------
    use physconst,          only: tmelt
    implicit none
!------------------------------Arguments--------------------------------
!
! Input arguments
!
!  CLIMLAB now passing grid dimensions as input
    integer, intent(in) ::   pcols
    integer, intent(in) ::   pver
    integer, intent(in) ::   pverp
!
    integer, intent(in) :: ncol
    real(r8), intent(in) :: landfrac(pcols)      ! Land fraction
    real(r8), intent(in) :: icefrac(pcols)       ! Ice fraction
    real(r8), intent(in) :: snowh(pcols)         ! Snow depth over land, water equivalent (m)
    real(r8), intent(in) :: landm(pcols)         ! Land fraction ramping to zero over ocean
    real(r8), intent(in) :: t(pcols,pver)        ! Temperature

!
! Output arguments
!
    real(r8), intent(out) :: rel(pcols,pver)      ! Liquid effective drop size (microns)
!
!---------------------------Local workspace-----------------------------
!
    integer i,k               ! Lon, lev indices
    real(r8) rliqland         ! liquid drop size if over land
    real(r8) rliqocean        ! liquid drop size if over ocean
    real(r8) rliqice          ! liquid drop size if over sea ice
!
!-----------------------------------------------------------------------
!
    rliqocean = 14.0_r8
    rliqice   = 14.0_r8
    rliqland  = 8.0_r8
    do k=1,pver
       do i=1,ncol
! jrm Reworked effective radius algorithm
          ! Start with temperature-dependent value appropriate for continental air
          ! Note: findmcnew has a pressure dependence here
          rel(i,k) = rliqland + (rliqocean-rliqland) * min(1.0_r8,max(0.0_r8,(tmelt-t(i,k))*0.05))
          ! Modify for snow depth over land
          rel(i,k) = rel(i,k) + (rliqocean-rel(i,k)) * min(1.0_r8,max(0.0_r8,snowh(i)*10.))
          ! Ramp between polluted value over land to clean value over ocean.
          rel(i,k) = rel(i,k) + (rliqocean-rel(i,k)) * min(1.0_r8,max(0.0_r8,1.0-landm(i)))
          ! Ramp between the resultant value and a sea ice value in the presence of ice.
          rel(i,k) = rel(i,k) + (rliqice-rel(i,k)) * min(1.0_r8,max(0.0_r8,icefrac(i)))
! end jrm
       end do
    end do
  end subroutine reltab

!===============================================================================
  subroutine reitab(pcols, pver, pverp, ncol, t, re)
    !

    !  CLIMLAB now passing grid dimensions as input
        integer, intent(in) ::   pcols
        integer, intent(in) ::   pver
        integer, intent(in) ::   pverp
    !
    integer, intent(in) :: ncol
    real(r8), intent(out) :: re(pcols,pver)
    real(r8), intent(in) :: t(pcols,pver)
    real(r8) retab(95)
    real(r8) corr
    integer i
    integer k
    integer index
    !
    !       Tabulated values of re(T) in the temperature interval
    !       180 K -- 274 K; hexagonal columns assumed:
    !
    data retab / 						&
         5.92779, 6.26422, 6.61973, 6.99539, 7.39234,	&
         7.81177, 8.25496, 8.72323, 9.21800, 9.74075, 10.2930,	&
         10.8765, 11.4929, 12.1440, 12.8317, 13.5581, 14.2319, 	&
         15.0351, 15.8799, 16.7674, 17.6986, 18.6744, 19.6955,	&
         20.7623, 21.8757, 23.0364, 24.2452, 25.5034, 26.8125,	&
         27.7895, 28.6450, 29.4167, 30.1088, 30.7306, 31.2943, 	&
         31.8151, 32.3077, 32.7870, 33.2657, 33.7540, 34.2601, 	&
         34.7892, 35.3442, 35.9255, 36.5316, 37.1602, 37.8078,	&
         38.4720, 39.1508, 39.8442, 40.5552, 41.2912, 42.0635,	&
         42.8876, 43.7863, 44.7853, 45.9170, 47.2165, 48.7221,	&
         50.4710, 52.4980, 54.8315, 57.4898, 60.4785, 63.7898,	&
         65.5604, 71.2885, 75.4113, 79.7368, 84.2351, 88.8833,	&
         93.6658, 98.5739, 103.603, 108.752, 114.025, 119.424, 	&
         124.954, 130.630, 136.457, 142.446, 148.608, 154.956,	&
         161.503, 168.262, 175.248, 182.473, 189.952, 197.699,	&
         205.728, 214.055, 222.694, 231.661, 240.971, 250.639/
    !
    save retab
    !
    do k=1,pver
       do i=1,ncol
          index = int(t(i,k)-179.)
          index = min(max(index,1),94)
          corr = t(i,k) - int(t(i,k))
          re(i,k) = retab(index)*(1.-corr)		&
               +retab(index+1)*corr
          !           re(i,k) = amax1(amin1(re(i,k),30.),10.)
       end do
    end do
    !
    return
  end subroutine reitab

end module pkg_cldoptics
