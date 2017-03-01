module physconst
!
! Physical constants (replaces /comcon/)
!
   use shr_kind_mod, only: r8 => shr_kind_r8
!+++CliMT inf/nan disabled
!+++rca   use infnan,        only: inf
   use shr_const_mod, only: shr_const_g,      shr_const_stebol, shr_const_tkfrz,  &
                            shr_const_mwdair, shr_const_rdair,  shr_const_mwwv,   &
                            shr_const_latice, shr_const_latvap, shr_const_cpdair, &
                            shr_const_rhofw,  shr_const_cpwv,   shr_const_rgas,   &
                            shr_const_karman, shr_const_pstd,   shr_const_rhodair,&
                            shr_const_avogad, shr_const_boltz
   implicit none
!
! Make module data private by default (so that modules used don't become public)
!
   private

! Universal constants
   real(r8), public, parameter :: r_universal = shr_const_rgas ! Universal gas constant (J/K/kmol)
   real(r8), public, parameter :: stebol = shr_const_stebol    ! Stefan-Boltzmann's constant

! Constants for Earth
   real(r8), public, parameter :: gravit = shr_const_g      ! gravitational acceleration
   real(r8), public, parameter :: rga = 1./gravit           ! reciprocal of gravit

! Constants for air
   real(r8), public, parameter :: rair = shr_const_rdair    ! Gas constant for dry air (J/K/kg)
   real(r8), public, parameter :: cpair = shr_const_cpdair  ! specific heat of dry air (J/K/kg)
   real(r8), public, parameter :: cappa = rair/cpair        ! R/Cp
   real(r8), public, parameter :: pstd = shr_const_pstd     ! Standard pressure Pascals
   real(r8), public, parameter :: rhodair = shr_const_rhodair ! density of dry air at STP (kg/m3)

! Constants for water
   real(r8), public, parameter :: cph2o = shr_const_cpwv    ! specific heat of water vapor (J/K/kg)
   real(r8), public, parameter :: epsilo = shr_const_mwwv/shr_const_mwdair ! ratio of h2o to dry air molecular weights 
   real(r8), public, parameter :: latvap = shr_const_latvap ! Latent heat of vaporization
   real(r8), public, parameter :: latice = shr_const_latice ! Latent heat of fusion
   real(r8), public, parameter :: rhoh2o = shr_const_rhofw  ! Density of liquid water (STP)
   real(r8), public, parameter :: tmelt = shr_const_tkfrz   ! Freezing point of water

! Molecular weights
   real(r8), public, parameter :: mwdry =  shr_const_mwdair ! molecular weight dry air
   real(r8), public, parameter :: mwco2 =  44.              ! molecular weight co2
   real(r8), public, parameter :: mwh2o =  shr_const_mwwv   ! molecular weight h2o
   real(r8), public, parameter :: mwn2o =  44.              ! molecular weight n2o
   real(r8), public, parameter :: mwch4 =  16.              ! molecular weight ch4
   real(r8), public, parameter :: mwf11 = 136.              ! molecular weight cfc11
   real(r8), public, parameter :: mwf12 = 120.              ! molecular weight cfc12
   real(r8), public, parameter :: mwo3  =  48.              ! molecular weight O3
!  Other molecular constants
   real(r8), public, parameter :: avogad = shr_const_avogad ! Avogadro's number
   real(r8), public, parameter :: boltz  = shr_const_boltz  ! Boltzman's constant
! Turbulence
   real(r8), public, parameter :: karman = shr_const_karman ! VonKarman constant
!
! Defer setting the following variables to their actual values because their value depends on
! whether the run is adiabatic or not
!
!+++CliMT: inf and nan disabled for portability.
!+++rca    real(r8), public :: rh2o = inf          ! gas constant for water vapor
!+++rca    real(r8), public :: cpvir = inf         ! cpwv/cpair - 1
!+++rca    real(r8), public :: cpwv = inf          ! Specific heat of water vapor
!+++rca    real(r8), public :: zvir = inf          ! rh2o/rair - 1
!---CliMT
   real(r8), public :: rh2o = 0.          ! gas constant for water vapor
   real(r8), public :: cpvir = 0.         ! cpwv/cpair - 1
   real(r8), public :: cpwv = 0.          ! Specific heat of water vapor
   real(r8), public :: zvir = 0.          ! rh2o/rair - 1

end module physconst
