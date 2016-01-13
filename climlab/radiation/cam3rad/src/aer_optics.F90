#include <misc.h>
#include <params.h>

module aer_optics

  ! Dummy version for CliMT -- aerosols disabled!

  use shr_kind_mod, only: r8 => shr_kind_r8

  implicit none

  integer, parameter, public :: idxVIS = 8     ! index to visible band
  integer, parameter, public :: nrh = 1000   ! number of relative humidity values for look-up-table
  integer, parameter, public :: nspint = 19   ! number of spectral intervals
  integer, parameter, public :: ndstsz = 4    ! number of dust size bins

  real(r8), public :: ksul(nrh, nspint) = 1.e-9    ! sulfate specific extinction  ( m^2 g-1 )
  real(r8), public :: wsul(nrh, nspint) = 1.e-9    ! sulfate single scattering albedo
  real(r8), public :: gsul(nrh, nspint)  = 1.e-9   ! sulfate asymmetry parameter
  real(r8), public :: kbg(nspint) = 1.e-9          ! background specific extinction  ( m^2 g-1 )
  real(r8), public :: wbg(nspint) = 1.e-9          ! background single scattering albedo
  real(r8), public :: gbg(nspint) = 1.e-9          ! background asymmetry parameter
  real(r8), public :: ksslt(nrh, nspint) = 1.e-9   ! sea-salt specific extinction  ( m^2 g-1 )
  real(r8), public :: wsslt(nrh, nspint) = 1.e-9   ! sea-salt single scattering albedo
  real(r8), public :: gsslt(nrh, nspint) = 1.e-9   ! sea-salt asymmetry parameter
  real(r8), public :: kcphil(nrh, nspint) = 1.e-9  ! hydrophilic carbon specific extinction  ( m^2 g-1 )
  real(r8), public :: wcphil(nrh, nspint) = 1.e-9  ! hydrophilic carbon single scattering albedo
  real(r8), public :: gcphil(nrh, nspint) = 1.e-9  ! hydrophilic carbon asymmetry parameter
  real(r8), public :: kcphob(nspint) = 1.e-9       ! hydrophobic carbon specific extinction  ( m^2 g-1 )
  real(r8), public :: wcphob(nspint) = 1.e-9       ! hydrophobic carbon single scattering albedo
  real(r8), public :: gcphob(nspint) = 1.e-9       ! hydrophobic carbon asymmetry parameter
  real(r8), public :: kcb(nspint) = 1.e-9          ! black carbon specific extinction  ( m^2 g-1 )
  real(r8), public :: wcb(nspint) = 1.e-9          ! black carbon single scattering albedo
  real(r8), public :: gcb(nspint) = 1.e-9          ! black carbon asymmetry parameter
  real(r8), public :: kvolc(nspint) = 1.e-9        ! volcanic specific extinction  ( m^2 g-1)
  real(r8), public :: wvolc(nspint) = 1.e-9        ! volcanic single scattering albedo
  real(r8), public :: gvolc(nspint) = 1.e-9        ! volcanic asymmetry parameter
  real(r8), public :: kdst(ndstsz, nspint) = 1.e-9 ! dust specific extinction  ( m^2 g-1 )
  real(r8), public :: wdst(ndstsz, nspint) = 1.e-9 ! dust single scattering albedo
  real(r8), public :: gdst(ndstsz, nspint) = 1.e-9 ! dust asymmetry parameter

  save

end module aer_optics

