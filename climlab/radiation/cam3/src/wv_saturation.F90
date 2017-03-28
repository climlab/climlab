!
! Common block and statement functions for saturation vapor pressure
! look-up procedure, J. J. Hack, February 1990
!
! $Id: wv_saturation.F90,v 1.1 2009/02/12 16:41:44 rca Exp $
!
module wv_saturation
  use shr_kind_mod, only: r8 => shr_kind_r8
  use abortutils,   only: endrun

  private
  save
!
! Public interfaces
!
  public gestbl   ! Initialization subroutine
  public estblf   ! saturation pressure table lookup
  public aqsat    ! Returns saturation vapor pressure
  public aqsatd   ! Same as aqsat, but also returns a temperature derivitive
  public vqsatd   ! Vector version of aqsatd
  public fqsatd   ! Function version of vqsatd
!
! Data used by cldwat
!
  public hlatv, tmin, hlatf, rgasv, pcf, cp, epsqs, ttrice
!
! Data
!
  integer plenest  ! length of saturation vapor pressure table
  parameter (plenest=250)
!
! Table of saturation vapor pressure values es from tmin degrees
! to tmax+1 degrees k in one degree increments.  ttrice defines the
! transition region where es is a combination of ice & water values
!
  real(r8) estbl(plenest)      ! table values of saturation vapor pressure
  real(r8) tmin       ! min temperature (K) for table
  real(r8) tmax       ! max temperature (K) for table
  real(r8) ttrice     ! transition range from es over H2O to es over ice
  real(r8) pcf(6)     ! polynomial coeffs -> es transition water to ice
  real(r8) epsqs      ! Ratio of h2o to dry air molecular weights
  real(r8) rgasv      ! Gas constant for water vapor
  real(r8) hlatf      ! Latent heat of vaporization
  real(r8) hlatv      ! Latent heat of fusion
  real(r8) cp         ! specific heat of dry air
  real(r8) tmelt      ! Melting point of water (K)
  logical icephs  ! false => saturation vapor press over water only

contains

   real(r8) function estblf( td )
!
! Saturation vapor pressure table lookup
!
   real(r8), intent(in) :: td         ! Temperature for saturation lookup
!
   real(r8) :: e       ! intermediate variable for es look-up
   real(r8) :: ai
   integer  :: i
!
   e = max(min(td,tmax),tmin)   ! partial pressure
   i = int(e-tmin)+1
   ai = aint(e-tmin)
   estblf = (tmin+ai-e+1.)* &
            estbl(i)-(tmin+ai-e)* &
            estbl(i+1)
   end function estblf

subroutine gestbl(tmn     ,tmx     ,trice   ,ip      ,epsil   , &
                  latvap  ,latice  ,rh2o    ,cpair   ,tmeltx   )
!-----------------------------------------------------------------------
!
! Purpose:
! Builds saturation vapor pressure table for later lookup procedure.
!
! Method:
! Uses Goff & Gratch (1946) relationships to generate the table
! according to a set of free parameters defined below.  Auxiliary
! routines are also included for making rapid estimates (well with 1%)
! of both es and d(es)/dt for the particular table configuration.
!
! Author: J. Hack
!
!-----------------------------------------------------------------------
   use pmgrid, only: masterproc
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   real(r8), intent(in) :: tmn           ! Minimum temperature entry in es lookup table
   real(r8), intent(in) :: tmx           ! Maximum temperature entry in es lookup table
   real(r8), intent(in) :: epsil         ! Ratio of h2o to dry air molecular weights
   real(r8), intent(in) :: trice         ! Transition range from es over range to es over ice
   real(r8), intent(in) :: latvap        ! Latent heat of vaporization
   real(r8), intent(in) :: latice        ! Latent heat of fusion
   real(r8), intent(in) :: rh2o          ! Gas constant for water vapor
   real(r8), intent(in) :: cpair         ! Specific heat of dry air
   real(r8), intent(in) :: tmeltx        ! Melting point of water (K)
!
!---------------------------Local variables-----------------------------
!
   real(r8) t             ! Temperature
   integer n          ! Increment counter
   integer lentbl     ! Calculated length of lookup table
   integer itype      ! Ice phase: 0 -> no ice phase
!            1 -> ice phase, no transition
!           -x -> ice phase, x degree transition
   logical ip         ! Ice phase logical flag
!
!-----------------------------------------------------------------------
!
! Set es table parameters
!
   tmin   = tmn       ! Minimum temperature entry in table
   tmax   = tmx       ! Maximum temperature entry in table
   ttrice = trice     ! Trans. range from es over h2o to es over ice
   icephs = ip        ! Ice phase (true or false)
!
! Set physical constants required for es calculation
!
   epsqs  = epsil
   hlatv  = latvap
   hlatf  = latice
   rgasv  = rh2o
   cp     = cpair
   tmelt  = tmeltx
!
   lentbl = INT(tmax-tmin+2.000001)
   if (lentbl .gt. plenest) then
      write(6,9000) tmax, tmin, plenest
      call endrun ('GESTBL')    ! Abnormal termination
   end if
!
! Begin building es table.
! Check whether ice phase requested.
! If so, set appropriate transition range for temperature
!
   if (icephs) then
      if (ttrice /= 0.0) then
        ! CLIMLAB is this causing conversion problem?
         itype = -ttrice
      else
         itype = 1
      end if
   else
      itype = 0
   end if
!
   t = tmin - 1.0
   do n=1,lentbl
      t = t + 1.0
      call gffgch(t,estbl(n),itype)
   end do
!
   do n=lentbl+1,plenest
      estbl(n) = -99999.0
   end do
!
! Table complete -- Set coefficients for polynomial approximation of
! difference between saturation vapor press over water and saturation
! pressure over ice for -ttrice < t < 0 (degrees C). NOTE: polynomial
! is valid in the range -40 < t < 0 (degrees C).
!
!                  --- Degree 5 approximation ---
!
   pcf(1) =  5.04469588506e-01
   pcf(2) = -5.47288442819e+00
   pcf(3) = -3.67471858735e-01
   pcf(4) = -8.95963532403e-03
   pcf(5) = -7.78053686625e-05
!
!                  --- Degree 6 approximation ---
!
!-----pcf(1) =  7.63285250063e-02
!-----pcf(2) = -5.86048427932e+00
!-----pcf(3) = -4.38660831780e-01
!-----pcf(4) = -1.37898276415e-02
!-----pcf(5) = -2.14444472424e-04
!-----pcf(6) = -1.36639103771e-06
!
   if (masterproc) then
!++CliMT      write(6,*)' *** SATURATION VAPOR PRESSURE TABLE COMPLETED ***'
   end if

   return
!
9000 format('GESTBL: FATAL ERROR *********************************',/, &
            ' TMAX AND TMIN REQUIRE A LARGER DIMENSION ON THE LENGTH', &
            ' OF THE SATURATION VAPOR PRESSURE TABLE ESTBL(PLENEST)',/, &
            ' TMAX, TMIN, AND PLENEST => ', 2f7.2, i3)
!
end subroutine gestbl

subroutine aqsat(t       ,p       ,es      ,qs        ,ii      , &
                 ilen    ,kk      ,kstart  ,kend      )
!-----------------------------------------------------------------------
!
! Purpose:
! Utility procedure to look up and return saturation vapor pressure from
! precomputed table, calculate and return saturation specific humidity
! (g/g),for input arrays of temperature and pressure (dimensioned ii,kk)
! This routine is useful for evaluating only a selected region in the
! vertical.
!
! Method:
! <Describe the algorithm(s) used in the routine.>
! <Also include any applicable external references.>
!
! Author: J. Hack
!
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: ii             ! I dimension of arrays t, p, es, qs
   integer, intent(in) :: kk             ! K dimension of arrays t, p, es, qs
   integer, intent(in) :: ilen           ! Length of vectors in I direction which
   integer, intent(in) :: kstart         ! Starting location in K direction
   integer, intent(in) :: kend           ! Ending location in K direction
   real(r8), intent(in) :: t(ii,kk)          ! Temperature
   real(r8), intent(in) :: p(ii,kk)          ! Pressure
!
! Output arguments
!
   real(r8), intent(out) :: es(ii,kk)         ! Saturation vapor pressure
   real(r8), intent(out) :: qs(ii,kk)         ! Saturation specific humidity
!
!---------------------------Local workspace-----------------------------
!
   real(r8) omeps             ! 1 - 0.622
   integer i, k           ! Indices
!
!-----------------------------------------------------------------------
!
   omeps = 1.0 - epsqs
   do k=kstart,kend
      do i=1,ilen
         es(i,k) = estblf(t(i,k))
!
! Saturation specific humidity
!
         qs(i,k) = epsqs*es(i,k)/(p(i,k) - omeps*es(i,k))
!
! The following check is to avoid the generation of negative values
! that can occur in the upper stratosphere and mesosphere
!
         qs(i,k) = min(1.0_r8,qs(i,k))
!
         if (qs(i,k) < 0.0) then
            qs(i,k) = 1.0
            es(i,k) = p(i,k)
         end if
      end do
   end do
!
   return
end subroutine aqsat

subroutine aqsatd(t       ,p       ,es      ,qs      ,gam     , &
                  ii      ,ilen    ,kk      ,kstart  ,kend    )
!-----------------------------------------------------------------------
!
! Purpose:
! Utility procedure to look up and return saturation vapor pressure from
! precomputed table, calculate and return saturation specific humidity
! (g/g).
!
! Method:
! Differs from aqsat by also calculating and returning
! gamma (l/cp)*(d(qsat)/dT)
! Input arrays temperature and pressure (dimensioned ii,kk).
!
! Author: J. Hack
!
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: ii            ! I dimension of arrays t, p, es, qs
   integer, intent(in) :: ilen          ! Vector length in I direction
   integer, intent(in) :: kk            ! K dimension of arrays t, p, es, qs
   integer, intent(in) :: kstart        ! Starting location in K direction
   integer, intent(in) :: kend          ! Ending location in K direction

   real(r8), intent(in) :: t(ii,kk)         ! Temperature
   real(r8), intent(in) :: p(ii,kk)         ! Pressure

!
! Output arguments
!
   real(r8), intent(out) :: es(ii,kk)        ! Saturation vapor pressure
   real(r8), intent(out) :: qs(ii,kk)        ! Saturation specific humidity
   real(r8), intent(out) :: gam(ii,kk)       ! (l/cp)*(d(qs)/dt)
!
!---------------------------Local workspace-----------------------------
!
   logical lflg          ! True if in temperature transition region
   integer i             ! i index for vector calculations
   integer k             ! k index
   real(r8) omeps            ! 1. - 0.622
   real(r8) trinv            ! Reciprocal of ttrice (transition range)
   real(r8) tc               ! Temperature (in degrees C)
   real(r8) weight           ! Weight for es transition from water to ice
   real(r8) hltalt           ! Appropriately modified hlat for T derivatives
   real(r8) hlatsb           ! hlat weighted in transition region
   real(r8) hlatvp           ! hlat modified for t changes above freezing
   real(r8) tterm            ! Account for d(es)/dT in transition region
   real(r8) desdt            ! d(es)/dT
!
!-----------------------------------------------------------------------
!
   omeps = 1.0 - epsqs
   do k=kstart,kend
      do i=1,ilen
         es(i,k) = estblf(t(i,k))
!
! Saturation specific humidity
!
         qs(i,k) = epsqs*es(i,k)/(p(i,k) - omeps*es(i,k))
!
! The following check is to avoid the generation of negative qs
! values which can occur in the upper stratosphere and mesosphere
!
         qs(i,k) = min(1.0_r8,qs(i,k))
!
         if (qs(i,k) < 0.0) then
            qs(i,k) = 1.0
            es(i,k) = p(i,k)
         end if
      end do
   end do
!
! "generalized" analytic expression for t derivative of es
! accurate to within 1 percent for 173.16 < t < 373.16
!
   trinv = 0.0
   if ((.not. icephs) .or. (ttrice.eq.0.0)) go to 10
   trinv = 1.0/ttrice
!
   do k=kstart,kend
      do i=1,ilen
!
! Weighting of hlat accounts for transition from water to ice
! polynomial expression approximates difference between es over
! water and es over ice from 0 to -ttrice (C) (min of ttrice is
! -40): required for accurate estimate of es derivative in transition
! range from ice to water also accounting for change of hlatv with t
! above freezing where constant slope is given by -2369 j/(kg c) =cpv - cw
!
         tc     = t(i,k) - tmelt
         lflg   = (tc >= -ttrice .and. tc < 0.0)
         weight = min(-tc*trinv,1.0_r8)
         hlatsb = hlatv + weight*hlatf
         hlatvp = hlatv - 2369.0*tc
         if (t(i,k) < tmelt) then
            hltalt = hlatsb
         else
            hltalt = hlatvp
         end if
         if (lflg) then
            tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3) + tc*(pcf(4) + tc*pcf(5))))
         else
            tterm = 0.0
         end if
         desdt    = hltalt*es(i,k)/(rgasv*t(i,k)*t(i,k)) + tterm*trinv
         gam(i,k) = hltalt*qs(i,k)*p(i,k)*desdt/(cp*es(i,k)*(p(i,k) - omeps*es(i,k)))
         if (qs(i,k) == 1.0) gam(i,k) = 0.0
      end do
   end do
!
   go to 20
!
! No icephs or water to ice transition
!
10 do k=kstart,kend
      do i=1,ilen
!
! Account for change of hlatv with t above freezing where
! constant slope is given by -2369 j/(kg c) = cpv - cw
!
         hlatvp = hlatv - 2369.0*(t(i,k)-tmelt)
         if (icephs) then
            hlatsb = hlatv + hlatf
         else
            hlatsb = hlatv
         end if
         if (t(i,k) < tmelt) then
            hltalt = hlatsb
         else
            hltalt = hlatvp
         end if
         desdt    = hltalt*es(i,k)/(rgasv*t(i,k)*t(i,k))
         gam(i,k) = hltalt*qs(i,k)*p(i,k)*desdt/(cp*es(i,k)*(p(i,k) - omeps*es(i,k)))
         if (qs(i,k) == 1.0) gam(i,k) = 0.0
      end do
   end do
!
20 return
end subroutine aqsatd

subroutine vqsatd(t       ,p       ,es      ,qs      ,gam      , &
                  len     )
!-----------------------------------------------------------------------
!
! Purpose:
! Utility procedure to look up and return saturation vapor pressure from
! precomputed table, calculate and return saturation specific humidity
! (g/g), and calculate and return gamma (l/cp)*(d(qsat)/dT).  The same
! function as qsatd, but operates on vectors of temperature and pressure
!
! Method:
!
! Author: J. Hack
!
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: len       ! vector length
   real(r8), intent(in) :: t(len)       ! temperature
   real(r8), intent(in) :: p(len)       ! pressure
!
! Output arguments
!
   real(r8), intent(out) :: es(len)   ! saturation vapor pressure
   real(r8), intent(out) :: qs(len)   ! saturation specific humidity
   real(r8), intent(out) :: gam(len)  ! (l/cp)*(d(qs)/dt)
!
!--------------------------Local Variables------------------------------
!
   logical lflg   ! true if in temperature transition region
!
   integer i      ! index for vector calculations
!
   real(r8) omeps     ! 1. - 0.622
   real(r8) trinv     ! reciprocal of ttrice (transition range)
   real(r8) tc        ! temperature (in degrees C)
   real(r8) weight    ! weight for es transition from water to ice
   real(r8) hltalt    ! appropriately modified hlat for T derivatives
!
   real(r8) hlatsb    ! hlat weighted in transition region
   real(r8) hlatvp    ! hlat modified for t changes above freezing
   real(r8) tterm     ! account for d(es)/dT in transition region
   real(r8) desdt     ! d(es)/dT
!
!-----------------------------------------------------------------------
!
   omeps = 1.0 - epsqs
   do i=1,len
      es(i) = estblf(t(i))
!
! Saturation specific humidity
!
      qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))
!
! The following check is to avoid the generation of negative
! values that can occur in the upper stratosphere and mesosphere
!
      qs(i) = min(1.0_r8,qs(i))
!
      if (qs(i) < 0.0) then
         qs(i) = 1.0
         es(i) = p(i)
      end if
   end do
!
! "generalized" analytic expression for t derivative of es
! accurate to within 1 percent for 173.16 < t < 373.16
!
   trinv = 0.0
   if ((.not. icephs) .or. (ttrice.eq.0.0)) go to 10
   trinv = 1.0/ttrice
   do i=1,len
!
! Weighting of hlat accounts for transition from water to ice
! polynomial expression approximates difference between es over
! water and es over ice from 0 to -ttrice (C) (min of ttrice is
! -40): required for accurate estimate of es derivative in transition
! range from ice to water also accounting for change of hlatv with t
! above freezing where const slope is given by -2369 j/(kg c) = cpv - cw
!
      tc     = t(i) - tmelt
      lflg   = (tc >= -ttrice .and. tc < 0.0)
      weight = min(-tc*trinv,1.0_r8)
      hlatsb = hlatv + weight*hlatf
      hlatvp = hlatv - 2369.0*tc
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      if (lflg) then
         tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3) + tc*(pcf(4) + tc*pcf(5))))
      else
         tterm = 0.0
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i)) + tterm*trinv
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0) gam(i) = 0.0
   end do
   return
!
! No icephs or water to ice transition
!
10 do i=1,len
!
! Account for change of hlatv with t above freezing where
! constant slope is given by -2369 j/(kg c) = cpv - cw
!
      hlatvp = hlatv - 2369.0*(t(i)-tmelt)
      if (icephs) then
         hlatsb = hlatv + hlatf
      else
         hlatsb = hlatv
      end if
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i))
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0) gam(i) = 0.0
   end do
!
   return
!
end subroutine vqsatd

integer function fqsatd(t    ,p    ,es    ,qs   ,gam   , len     )
  !-----------------------------------------------------------------------
  ! Purpose:
  ! This is merely a function interface vqsatd.
  !------------------------------Arguments--------------------------------
  ! Input arguments
  integer, intent(in) :: len       ! vector length
  real(r8), intent(in) :: t(len)       ! temperature
  real(r8), intent(in) :: p(len)       ! pressure
  ! Output arguments
  real(r8), intent(out) :: es(len)   ! saturation vapor pressure
  real(r8), intent(out) :: qs(len)   ! saturation specific humidity
  real(r8), intent(out) :: gam(len)  ! (l/cp)*(d(qs)/dt)
  ! Call vqsatd
  call vqsatd(t       ,p       ,es      ,qs      ,gam  , len     )
  fqsatd = 1
  return
end function fqsatd
end module wv_saturation
