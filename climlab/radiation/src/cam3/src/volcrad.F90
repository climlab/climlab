#include <misc.h>
#include <params.h>

module volcrad
!------------------------------------------------------------------------------
!
! Description:
!
! Data and subroutines to calculate absorptivities and emissivity needed
! for the volcanics LW calculation.
!
! Public interfaces are:
!
! aer_trn -------------- Compute LW transmission factors
! aer_pth -------------- Computes strat. aerosol mass paths
!
! Author:  B. Collins, after original code by C. Zender and C. Ammann

!------------------------------------------------------------------------------
  use shr_kind_mod, only: r8 => shr_kind_r8
  !use ppgrid,       only: pcols, pver, pverp
!+++CliMT inf/nan disabled
!+++rca  use infnan

  implicit none

  save
!-----------------------------------------------------------------------------
! PUBLIC:: By default data and interfaces are private
!-----------------------------------------------------------------------------
  private
  public aer_trn, aer_pth

!
! Number of spectral bands used for LW volc. aerosol effects
!
      integer, public, parameter :: bnd_nbr_LW=7

! Index of volc. abs., H2O non-window
      integer, public, parameter :: idx_LW_H2O_NONWND=1
! Index of volc. abs., H2O window
      integer, public, parameter :: idx_LW_H2O_WINDOW=2
! Index of volc. cnt. abs. 0500--0650 cm-1
      integer, public, parameter :: idx_LW_0500_0650=3
! Index of volc. cnt. abs. 0650--0800 cm-1
      integer, public, parameter :: idx_LW_0650_0800=4
! Index of volc. cnt. abs. 0800--1000 cm-1
      integer, public, parameter :: idx_LW_0800_1000=5
! Index of volc. cnt. abs. 1000--1200 cm-1
      integer, public, parameter :: idx_LW_1000_1200=6
! Index of volc. cnt. abs. 1200--2000 cm-1
      integer, public, parameter :: idx_LW_1200_2000=7

! [m2 kg-1] Mass absorption coefficient
!     r-eff = 0.527 micron
!     ********************
!     H2SO4 at 300K (PaW75) aerosol mass extinction/absorption
!                           coefficients in m2 kg-1
!     Command line: ./mie -dbg --aer_typ=PaW75 --dist=lognormal
!                    --wvl_grd=CCM_SW --bnd_nbr=10 --sz_grd=log
!                    --sz_mnm=1.0e-3 --sz_mxm=5.0
!                    --sz_nbr=100 --dst_a=.35 --dst_b=0.405 --dst_c=.1e6
!
! First two values represent the overlap of volcanics with the non-window
! (0-800, 1200-2200 cm^-1) and window (800-1200 cm^-1) regions.� Coefficients
! were derived using crm_volc_minimize.pro with spectral flux optimization
! on first iteration, total heating rate on subsequent iterations (2-9).
! Five profiles for HLS, HLW, MLS, MLW, and TRO conditions were given equal
! weight.  RMS heating rate errors for a visible stratospheric optical
! depth of 1.0 are 0.02948 K/day.
!
      real(r8), public :: abs_cff_mss_aer(bnd_nbr_LW) = &
         (/ 70.257384, 285.282943, &
         1.0273851e+02, 6.3073303e+01, 1.2039569e+02, &
         3.6343643e+02, 2.7138528e+02 /)

!-----------------------------------------------------------------------------
! Public Interfaces
!-----------------------------------------------------------------------------
CONTAINS


      subroutine aer_trn(pcols, pver, pverp, aer_mpp, aer_trn_ttl)
!
!     Purpose: Compute strat. aerosol transmissions needed in absorptivity/
!              emissivity calculations
!              aer_trn() is called by radclw() when doabsems is .true.
!
      use prescribed_aerosols, only: strat_volcanic

!     Input arguments
!  CLIMLAB now passing grid dimensions as input
      integer, intent(in) ::   pcols
      integer, intent(in) ::   pver
      integer, intent(in) ::   pverp
!
!
!       [kg m-2] Volcanics path above kth interface level
!
      real(r8), intent(in) :: aer_mpp(pcols,pverp)

!     Output arguments
!
!       [fraction] Total volcanic transmission between interfaces k1 and k2
!
      real(r8), intent(out) ::  aer_trn_ttl(pcols,pverp,pverp,bnd_nbr_LW)

!-------------------------------------------------------------------------
!     Local variables
      integer bnd_idx           ! LW band index
      integer i                 ! lon index
      integer k1                ! lev index
      integer k2                ! lev index
      real(r8) aer_pth_dlt      ! [kg m-2] Volcanics path between interface
                                !          levels k1 and k2
      real(r8) odap_aer_ttl     ! [fraction] Total path absorption optical
                                !            depth

!-------------------------------------------------------------------------

      if (strat_volcanic) then
        do bnd_idx=1,bnd_nbr_LW
           do i=1,pcols
              aer_trn_ttl(i,1,1,bnd_idx)=1.0
           end do
           do k1=2,pverp
              do i=1,pcols
                 aer_trn_ttl(i,k1,k1,bnd_idx)=1.0

                 aer_pth_dlt  = abs(aer_mpp(i,k1) - aer_mpp(i,1))
                 odap_aer_ttl = abs_cff_mss_aer(bnd_idx) * aer_pth_dlt

                 aer_trn_ttl(i,1,k1,bnd_idx) = exp(-1.66 * odap_aer_ttl)
              end do
           end do

           do k1=2,pver
              do k2=k1+1,pverp
                 do i=1,pcols
                    aer_trn_ttl(i,k1,k2,bnd_idx) = &
                         aer_trn_ttl(i,1,k2,bnd_idx) / &
                         aer_trn_ttl(i,1,k1,bnd_idx)
                 end do
              end do
           end do

           do k1=2,pverp
              do k2=1,k1-1
                 do i=1,pcols
                    aer_trn_ttl(i,k1,k2,bnd_idx)=aer_trn_ttl(i,k2,k1,bnd_idx)
                 end do
              end do
           end do
        end do
      else
        aer_trn_ttl = 1.0
      endif

      return
      end subroutine aer_trn


      subroutine aer_pth(pcols, pver, pverp, aer_mass, aer_mpp, ncol)
!------------------------------------------------------
!     Purpose: convert mass per layer to cumulative mass from Top
!------------------------------------------------------
!     Input
!  CLIMLAB now passing grid dimensions as input
      integer, intent(in) ::   pcols
      integer, intent(in) ::   pver
      integer, intent(in) ::   pverp
!
      real(r8), intent(in):: aer_mass(pcols,pver)  ! Rad level aerosol mass mixing ratio
      integer,  intent(in):: ncol
!
!     Output
      real(r8), intent(out):: aer_mpp(pcols,pverp) ! [kg m-2] Volcanics path above kth interface
!
!     Local
      integer i      ! Column index
      integer k      ! Level index
!------------------------------------------------------
!------------------------------------------------------

      aer_mpp(1:ncol,1) =  0._r8
      do k=2,pverp
          aer_mpp(1:ncol,k) = aer_mpp(1:ncol,k-1) + aer_mass(1:ncol,k-1)
      enddo

      end subroutine aer_pth

end module volcrad
