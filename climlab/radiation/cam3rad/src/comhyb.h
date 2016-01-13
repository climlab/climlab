!----------------------------------------------------------------------- 
! 
! Purpose: Hybrid level definitions: p = a*p0 + b*ps
!          interfaces   p(k) = hyai(k)*ps0 + hybi(k)*ps
!          midpoints    p(k) = hyam(k)*ps0 + hybm(k)*ps
! 
!-----------------------------------------------------------------------

      real(r8) hyai(plevp)        ! ps0 component of hybrid coordinate - interfaces
      real(r8) hyam(plev)        ! ps0 component of hybrid coordinate - midpoints
      real(r8) hybi(plevp)        ! ps component of hybrid coordinate - interfaces
      real(r8) hybm(plev)        ! ps component of hybrid coordinate - midpoints

      real(r8) hybd(plev)        ! difference  in b (hybi) across layers
      real(r8) hypi(plevp)        ! reference pressures at interfaces
      real(r8) hypm(plev)        ! reference pressures at midpoints
      real(r8) hypd(plev)        ! reference pressure layer thickness

      real(r8) ps0         ! base state sfc pressure for level definitions
      real(r8) psr         ! reference surface pressure for linearization

      integer nprlev       ! number of pure pressure levels at top

      common /comhyb/ hyai ,hyam  ,hybi ,hybm
      common /comhyb/ hybd ,hypi ,hypm  ,hypd
      common /comhyb/ ps0         ,psr         ,nprlev
 
