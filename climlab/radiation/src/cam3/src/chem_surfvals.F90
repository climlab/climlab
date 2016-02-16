#include <misc.h>

module chem_surfvals

!-----------------------------------------------------------------------------------
! Purpose: Provides greenhouse gas (ghg) values at the Earth's surface.
! 
!!!!  Modified for CliMT !!!!!
!-----------------------------------------------------------------------------------

   use shr_kind_mod, only: r8=>shr_kind_r8

!-----------------------------------------------------------------------
!- module boilerplate --------------------------------------------------
!-----------------------------------------------------------------------
   implicit none
   private                   ! Make default access private
   save

! Public methods
   public ::&
      chem_surfvals_set_co2,         &
      chem_surfvals_get           ! return surface values for: CO2VMR, CO2MMR, CH4VMR
                                  ! N2OVMR, F11VMR, and F12VMR

! Private module data

   ! Default values for namelist variables
   real(r8) :: co2vmr = 3.550e-4               ! co2   volume mixing ratio 
   real(r8) :: n2ovmr = 0.311e-6               ! n2o   volume mixing ratio 
   real(r8) :: ch4vmr = 1.714e-6               ! ch4   volume mixing ratio 
   real(r8) :: f11vmr = 0.280e-9               ! cfc11 volume mixing ratio 
   real(r8) :: f12vmr = 0.503e-9               ! cfc12 volume mixing ratio 

!=========================================================================================

contains

!=========================================================================================
function chem_surfvals_get(name)
  use physconst,    only: mwdry, mwco2

  character(len=*), intent(in) :: name

  real(r8), parameter :: rmwco2 = mwco2/mwdry    ! ratio of molecular weights of co2 to dry air
  real(r8) :: chem_surfvals_get

  select case (name)
  case ('CO2VMR')
     chem_surfvals_get = co2vmr
  case ('CO2MMR')
     chem_surfvals_get = rmwco2 * co2vmr
  case ('N2OVMR')
     chem_surfvals_get = n2ovmr
  case ('CH4VMR')
     chem_surfvals_get = ch4vmr
  case ('F11VMR')
     chem_surfvals_get = f11vmr
  case ('F12VMR')
     chem_surfvals_get = f12vmr
  end select

end function chem_surfvals_get

!=========================================================================================

subroutine chem_surfvals_set_co2(co2)
!----------------------------------------------------------------------- 
! Dummy version for CliMT
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   real(r8) :: co2

   co2vmr = co2

end subroutine chem_surfvals_set_co2


!=========================================================================================
end module chem_surfvals
