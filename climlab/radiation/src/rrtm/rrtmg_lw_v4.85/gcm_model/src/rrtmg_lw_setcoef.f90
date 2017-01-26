!     path:      $Source: /storm/rc1/cvsroot/rc/rrtmg_lw/src/rrtmg_lw_setcoef.f90,v $
!     author:    $Author: miacono $
!     revision:  $Revision: 1.6 $
!     created:   $Date: 2011/04/08 20:25:01 $
!
      module rrtmg_lw_setcoef

!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2009, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------

! ------- Modules -------

      use parkind, only : im => kind_im, rb => kind_rb
      use parrrtm, only : nbndlw, mg, maxxsec, mxmol
      use rrlw_wvn, only: totplnk, totplk16, totplnkderiv, totplk16deriv
      use rrlw_ref
      use rrlw_vsn, only: hvrset, hnamset

      implicit none

      contains

!----------------------------------------------------------------------------
      subroutine setcoef(nlayers, istart, pavel, tavel, tz, tbound, semiss, &
                         coldry, wkl, wbroad, &
                         laytrop, jp, jt, jt1, planklay, planklev, plankbnd, &
                         idrv, dplankbnd_dt, &
                         colh2o, colco2, colo3, coln2o, colco, colch4, colo2, &
                         colbrd, fac00, fac01, fac10, fac11, &
                         rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1, &
                         rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1, &
                         rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1, &
                         selffac, selffrac, indself, forfac, forfrac, indfor, &
                         minorfrac, scaleminor, scaleminorn2, indminor)
!----------------------------------------------------------------------------
!
!  Purpose:  For a given atmosphere, calculate the indices and
!  fractions related to the pressure and temperature interpolations.
!  Also calculate the values of the integrated Planck functions
!  for each band at the level and layer temperatures.

! ------- Declarations -------

! ----- Input -----
      integer(kind=im), intent(in) :: nlayers         ! total number of layers
      integer(kind=im), intent(in) :: istart          ! beginning band of calculation
      integer(kind=im), intent(in) :: idrv            ! Planck derivative option flag

      real(kind=rb), intent(in) :: pavel(:)           ! layer pressures (mb)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: tavel(:)           ! layer temperatures (K)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: tz(0:)             ! level (interface) temperatures (K)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(in) :: tbound             ! surface temperature (K)
      real(kind=rb), intent(in) :: coldry(:)          ! dry air column density (mol/cm2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: wbroad(:)          ! broadening gas column density (mol/cm2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: wkl(:,:)           ! molecular amounts (mol/cm-2)
                                                      !    Dimensions: (mxmol,nlayers)
      real(kind=rb), intent(in) :: semiss(:)          ! lw surface emissivity
                                                      !    Dimensions: (nbndlw)

! ----- Output -----
      integer(kind=im), intent(out) :: laytrop        ! tropopause layer index
      integer(kind=im), intent(out) :: jp(:)          !
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(out) :: jt(:)          !
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(out) :: jt1(:)         !
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: planklay(:,:)     !
                                                      !    Dimensions: (nlayers,nbndlw)
      real(kind=rb), intent(out) :: planklev(0:,:)    !
                                                      !    Dimensions: (0:nlayers,nbndlw)
      real(kind=rb), intent(out) :: plankbnd(:)       !
                                                      !    Dimensions: (nbndlw)
      real(kind=rb), intent(out) :: dplankbnd_dt(:)   !
                                                      !    Dimensions: (nbndlw)

      real(kind=rb), intent(out) :: colh2o(:)         ! column amount (h2o)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colco2(:)         ! column amount (co2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colo3(:)          ! column amount (o3)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: coln2o(:)         ! column amount (n2o)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colco(:)          ! column amount (co)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colch4(:)         ! column amount (ch4)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colo2(:)          ! column amount (o2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colbrd(:)         ! column amount (broadening gases)
                                                      !    Dimensions: (nlayers)

      integer(kind=im), intent(out) :: indself(:)
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(out) :: indfor(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: selffac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: selffrac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: forfac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: forfrac(:)
                                                      !    Dimensions: (nlayers)

      integer(kind=im), intent(out) :: indminor(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: minorfrac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: scaleminor(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: scaleminorn2(:)
                                                      !    Dimensions: (nlayers)

      real(kind=rb), intent(out) :: &                 !
                       fac00(:), fac01(:), &          !    Dimensions: (nlayers)
                       fac10(:), fac11(:)

      real(kind=rb), intent(out) :: &                 !
                       rat_h2oco2(:),rat_h2oco2_1(:), &
                       rat_h2oo3(:),rat_h2oo3_1(:), & !    Dimensions: (nlayers)
                       rat_h2on2o(:),rat_h2on2o_1(:), &
                       rat_h2och4(:),rat_h2och4_1(:), &
                       rat_n2oco2(:),rat_n2oco2_1(:), &
                       rat_o3co2(:),rat_o3co2_1(:)


! ----- Local -----
      integer(kind=im) :: indbound, indlev0
      integer(kind=im) :: lay, indlay, indlev, iband
      integer(kind=im) :: jp1
      real(kind=rb) :: stpfac, tbndfrac, t0frac, tlayfrac, tlevfrac
      real(kind=rb) :: dbdtlev, dbdtlay
      real(kind=rb) :: plog, fp, ft, ft1, water, scalefac, factor, compfp


      hvrset = '$Revision: 1.6 $'

      stpfac = 296._rb/1013._rb

      indbound = tbound - 159._rb
      if (indbound .lt. 1) then
         indbound = 1
      elseif (indbound .gt. 180) then
         indbound = 180
      endif
      tbndfrac = tbound - 159._rb - real(indbound)
      indlev0 = tz(0) - 159._rb
      if (indlev0 .lt. 1) then
         indlev0 = 1
      elseif (indlev0 .gt. 180) then
         indlev0 = 180
      endif
      t0frac = tz(0) - 159._rb - real(indlev0)
      laytrop = 0

! Begin layer loop
!  Calculate the integrated Planck functions for each band at the
!  surface, level, and layer temperatures.
      do lay = 1, nlayers
         indlay = tavel(lay) - 159._rb
         if (indlay .lt. 1) then
            indlay = 1
         elseif (indlay .gt. 180) then
            indlay = 180
         endif
         tlayfrac = tavel(lay) - 159._rb - real(indlay)
         indlev = tz(lay) - 159._rb
         if (indlev .lt. 1) then
            indlev = 1
         elseif (indlev .gt. 180) then
            indlev = 180
         endif
         tlevfrac = tz(lay) - 159._rb - real(indlev)

! Begin spectral band loop
         do iband = 1, 15
            if (lay.eq.1) then
               dbdtlev = totplnk(indbound+1,iband) - totplnk(indbound,iband)
               plankbnd(iband) = semiss(iband) * &
                   (totplnk(indbound,iband) + tbndfrac * dbdtlev)
               dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
               planklev(0,iband) = totplnk(indlev0,iband) + t0frac * dbdtlev
               if (idrv .eq. 1) then
                  dbdtlev = totplnkderiv(indbound+1,iband) - totplnkderiv(indbound,iband)
                  dplankbnd_dt(iband) = semiss(iband) * &
                      (totplnkderiv(indbound,iband) + tbndfrac * dbdtlev)
               endif
            endif
            dbdtlev = totplnk(indlev+1,iband) - totplnk(indlev,iband)
            dbdtlay = totplnk(indlay+1,iband) - totplnk(indlay,iband)
            planklay(lay,iband) = totplnk(indlay,iband) + tlayfrac * dbdtlay
            planklev(lay,iband) = totplnk(indlev,iband) + tlevfrac * dbdtlev
         enddo

!  For band 16, if radiative transfer will be performed on just
!  this band, use integrated Planck values up to 3250 cm-1.
!  If radiative transfer will be performed across all 16 bands,
!  then include in the integrated Planck values for this band
!  contributions from 2600 cm-1 to infinity.
         iband = 16
         if (istart .eq. 16) then
            if (lay.eq.1) then
               dbdtlev = totplk16(indbound+1) - totplk16(indbound)
               plankbnd(iband) = semiss(iband) * &
                    (totplk16(indbound) + tbndfrac * dbdtlev)
               if (idrv .eq. 1) then
                  dbdtlev = totplk16deriv(indbound+1) - totplk16deriv(indbound)
                  dplankbnd_dt(iband) = semiss(iband) * &
                       (totplk16deriv(indbound) + tbndfrac * dbdtlev)
               endif
               dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
               planklev(0,iband) = totplk16(indlev0) + &
                    t0frac * dbdtlev
            endif
            dbdtlev = totplk16(indlev+1) - totplk16(indlev)
            dbdtlay = totplk16(indlay+1) - totplk16(indlay)
            planklay(lay,iband) = totplk16(indlay) + tlayfrac * dbdtlay
            planklev(lay,iband) = totplk16(indlev) + tlevfrac * dbdtlev
         else
            if (lay.eq.1) then
               dbdtlev = totplnk(indbound+1,iband) - totplnk(indbound,iband)
               plankbnd(iband) = semiss(iband) * &
                    (totplnk(indbound,iband) + tbndfrac * dbdtlev)
               if (idrv .eq. 1) then
                  dbdtlev = totplnkderiv(indbound+1,iband) - totplnkderiv(indbound,iband)
                  dplankbnd_dt(iband) = semiss(iband) * &
                       (totplnkderiv(indbound,iband) + tbndfrac * dbdtlev)
               endif
               dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
               planklev(0,iband) = totplnk(indlev0,iband) + t0frac * dbdtlev
            endif
            dbdtlev = totplnk(indlev+1,iband) - totplnk(indlev,iband)
            dbdtlay = totplnk(indlay+1,iband) - totplnk(indlay,iband)
            planklay(lay,iband) = totplnk(indlay,iband) + tlayfrac * dbdtlay
            planklev(lay,iband) = totplnk(indlev,iband) + tlevfrac * dbdtlev
         endif

!  Find the two reference pressures on either side of the
!  layer pressure.  Store them in JP and JP1.  Store in FP the
!  fraction of the difference (in ln(pressure)) between these
!  two values that the layer pressure lies.
!         plog = alog(pavel(lay))
!  CLIMLAB change dlog to log here
         !plog = dlog(pavel(lay))
         plog = log(pavel(lay))
         jp(lay) = int(36._rb - 5*(plog+0.04_rb))
         if (jp(lay) .lt. 1) then
            jp(lay) = 1
         elseif (jp(lay) .gt. 58) then
            jp(lay) = 58
         endif
         jp1 = jp(lay) + 1
         fp = 5._rb *(preflog(jp(lay)) - plog)

!  Determine, for each reference pressure (JP and JP1), which
!  reference temperature (these are different for each
!  reference pressure) is nearest the layer temperature but does
!  not exceed it.  Store these indices in JT and JT1, resp.
!  Store in FT (resp. FT1) the fraction of the way between JT
!  (JT1) and the next highest reference temperature that the
!  layer temperature falls.
         jt(lay) = int(3._rb + (tavel(lay)-tref(jp(lay)))/15._rb)
         if (jt(lay) .lt. 1) then
            jt(lay) = 1
         elseif (jt(lay) .gt. 4) then
            jt(lay) = 4
         endif
         ft = ((tavel(lay)-tref(jp(lay)))/15._rb) - real(jt(lay)-3)
         jt1(lay) = int(3._rb + (tavel(lay)-tref(jp1))/15._rb)
         if (jt1(lay) .lt. 1) then
            jt1(lay) = 1
         elseif (jt1(lay) .gt. 4) then
            jt1(lay) = 4
         endif
         ft1 = ((tavel(lay)-tref(jp1))/15._rb) - real(jt1(lay)-3)
         water = wkl(1,lay)/coldry(lay)
         scalefac = pavel(lay) * stpfac / tavel(lay)

!  If the pressure is less than ~100mb, perform a different
!  set of species interpolations.
         if (plog .le. 4.56_rb) go to 5300
         laytrop =  laytrop + 1

         forfac(lay) = scalefac / (1.+water)
         factor = (332.0_rb-tavel(lay))/36.0_rb
         indfor(lay) = min(2, max(1, int(factor)))
         forfrac(lay) = factor - real(indfor(lay))

!  Set up factors needed to separately include the water vapor
!  self-continuum in the calculation of absorption coefficient.
         selffac(lay) = water * forfac(lay)
         factor = (tavel(lay)-188.0_rb)/7.2_rb
         indself(lay) = min(9, max(1, int(factor)-7))
         selffrac(lay) = factor - real(indself(lay) + 7)

!  Set up factors needed to separately include the minor gases
!  in the calculation of absorption coefficient
         scaleminor(lay) = pavel(lay)/tavel(lay)
         scaleminorn2(lay) = (pavel(lay)/tavel(lay)) &
             *(wbroad(lay)/(coldry(lay)+wkl(1,lay)))
         factor = (tavel(lay)-180.8_rb)/7.2_rb
         indminor(lay) = min(18, max(1, int(factor)))
         minorfrac(lay) = factor - real(indminor(lay))

!  Setup reference ratio to be used in calculation of binary
!  species parameter in lower atmosphere.
         rat_h2oco2(lay)=chi_mls(1,jp(lay))/chi_mls(2,jp(lay))
         rat_h2oco2_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(2,jp(lay)+1)

         rat_h2oo3(lay)=chi_mls(1,jp(lay))/chi_mls(3,jp(lay))
         rat_h2oo3_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(3,jp(lay)+1)

         rat_h2on2o(lay)=chi_mls(1,jp(lay))/chi_mls(4,jp(lay))
         rat_h2on2o_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(4,jp(lay)+1)

         rat_h2och4(lay)=chi_mls(1,jp(lay))/chi_mls(6,jp(lay))
         rat_h2och4_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(6,jp(lay)+1)

         rat_n2oco2(lay)=chi_mls(4,jp(lay))/chi_mls(2,jp(lay))
         rat_n2oco2_1(lay)=chi_mls(4,jp(lay)+1)/chi_mls(2,jp(lay)+1)

!  Calculate needed column amounts.
         colh2o(lay) = 1.e-20_rb * wkl(1,lay)
         colco2(lay) = 1.e-20_rb * wkl(2,lay)
         colo3(lay) = 1.e-20_rb * wkl(3,lay)
         coln2o(lay) = 1.e-20_rb * wkl(4,lay)
         colco(lay) = 1.e-20_rb * wkl(5,lay)
         colch4(lay) = 1.e-20_rb * wkl(6,lay)
         colo2(lay) = 1.e-20_rb * wkl(7,lay)
         if (colco2(lay) .eq. 0._rb) colco2(lay) = 1.e-32_rb * coldry(lay)
         if (colo3(lay) .eq. 0._rb) colo3(lay) = 1.e-32_rb * coldry(lay)
         if (coln2o(lay) .eq. 0._rb) coln2o(lay) = 1.e-32_rb * coldry(lay)
         if (colco(lay) .eq. 0._rb) colco(lay) = 1.e-32_rb * coldry(lay)
         if (colch4(lay) .eq. 0._rb) colch4(lay) = 1.e-32_rb * coldry(lay)
         colbrd(lay) = 1.e-20_rb * wbroad(lay)
         go to 5400

!  Above laytrop.
 5300    continue

         forfac(lay) = scalefac / (1.+water)
         factor = (tavel(lay)-188.0_rb)/36.0_rb
         indfor(lay) = 3
         forfrac(lay) = factor - 1.0_rb

!  Set up factors needed to separately include the water vapor
!  self-continuum in the calculation of absorption coefficient.
         selffac(lay) = water * forfac(lay)

!  Set up factors needed to separately include the minor gases
!  in the calculation of absorption coefficient
         scaleminor(lay) = pavel(lay)/tavel(lay)
         scaleminorn2(lay) = (pavel(lay)/tavel(lay)) &
             * (wbroad(lay)/(coldry(lay)+wkl(1,lay)))
         factor = (tavel(lay)-180.8_rb)/7.2_rb
         indminor(lay) = min(18, max(1, int(factor)))
         minorfrac(lay) = factor - real(indminor(lay))

!  Setup reference ratio to be used in calculation of binary
!  species parameter in upper atmosphere.
         rat_h2oco2(lay)=chi_mls(1,jp(lay))/chi_mls(2,jp(lay))
         rat_h2oco2_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(2,jp(lay)+1)

         rat_o3co2(lay)=chi_mls(3,jp(lay))/chi_mls(2,jp(lay))
         rat_o3co2_1(lay)=chi_mls(3,jp(lay)+1)/chi_mls(2,jp(lay)+1)

!  Calculate needed column amounts.
         colh2o(lay) = 1.e-20_rb * wkl(1,lay)
         colco2(lay) = 1.e-20_rb * wkl(2,lay)
         colo3(lay) = 1.e-20_rb * wkl(3,lay)
         coln2o(lay) = 1.e-20_rb * wkl(4,lay)
         colco(lay) = 1.e-20_rb * wkl(5,lay)
         colch4(lay) = 1.e-20_rb * wkl(6,lay)
         colo2(lay) = 1.e-20_rb * wkl(7,lay)
         if (colco2(lay) .eq. 0._rb) colco2(lay) = 1.e-32_rb * coldry(lay)
         if (colo3(lay) .eq. 0._rb) colo3(lay) = 1.e-32_rb * coldry(lay)
         if (coln2o(lay) .eq. 0._rb) coln2o(lay) = 1.e-32_rb * coldry(lay)
         if (colco(lay)  .eq. 0._rb) colco(lay) = 1.e-32_rb * coldry(lay)
         if (colch4(lay) .eq. 0._rb) colch4(lay) = 1.e-32_rb * coldry(lay)
         colbrd(lay) = 1.e-20_rb * wbroad(lay)
 5400    continue

!  We have now isolated the layer ln pressure and temperature,
!  between two reference pressures and two reference temperatures
!  (for each reference pressure).  We multiply the pressure
!  fraction FP with the appropriate temperature fractions to get
!  the factors that will be needed for the interpolation that yields
!  the optical depths (performed in routines TAUGBn for band n).`

         compfp = 1. - fp
         fac10(lay) = compfp * ft
         fac00(lay) = compfp * (1._rb - ft)
         fac11(lay) = fp * ft1
         fac01(lay) = fp * (1._rb - ft1)

!  Rescale selffac and forfac for use in taumol
         selffac(lay) = colh2o(lay)*selffac(lay)
         forfac(lay) = colh2o(lay)*forfac(lay)

! End layer loop
      enddo

      end subroutine setcoef

!***************************************************************************
      subroutine lwatmref
!***************************************************************************

      save

! These pressures are chosen such that the ln of the first pressure
! has only a few non-zero digits (i.e. ln(PREF(1)) = 6.96000) and
! each subsequent ln(pressure) differs from the previous one by 0.2.

      pref(:) = (/ &
          1.05363e+03_rb,8.62642e+02_rb,7.06272e+02_rb,5.78246e+02_rb,4.73428e+02_rb, &
          3.87610e+02_rb,3.17348e+02_rb,2.59823e+02_rb,2.12725e+02_rb,1.74164e+02_rb, &
          1.42594e+02_rb,1.16746e+02_rb,9.55835e+01_rb,7.82571e+01_rb,6.40715e+01_rb, &
          5.24573e+01_rb,4.29484e+01_rb,3.51632e+01_rb,2.87892e+01_rb,2.35706e+01_rb, &
          1.92980e+01_rb,1.57998e+01_rb,1.29358e+01_rb,1.05910e+01_rb,8.67114e+00_rb, &
          7.09933e+00_rb,5.81244e+00_rb,4.75882e+00_rb,3.89619e+00_rb,3.18993e+00_rb, &
          2.61170e+00_rb,2.13828e+00_rb,1.75067e+00_rb,1.43333e+00_rb,1.17351e+00_rb, &
          9.60789e-01_rb,7.86628e-01_rb,6.44036e-01_rb,5.27292e-01_rb,4.31710e-01_rb, &
          3.53455e-01_rb,2.89384e-01_rb,2.36928e-01_rb,1.93980e-01_rb,1.58817e-01_rb, &
          1.30029e-01_rb,1.06458e-01_rb,8.71608e-02_rb,7.13612e-02_rb,5.84256e-02_rb, &
          4.78349e-02_rb,3.91639e-02_rb,3.20647e-02_rb,2.62523e-02_rb,2.14936e-02_rb, &
          1.75975e-02_rb,1.44076e-02_rb,1.17959e-02_rb,9.65769e-03_rb/)

      preflog(:) = (/ &
           6.9600e+00_rb, 6.7600e+00_rb, 6.5600e+00_rb, 6.3600e+00_rb, 6.1600e+00_rb, &
           5.9600e+00_rb, 5.7600e+00_rb, 5.5600e+00_rb, 5.3600e+00_rb, 5.1600e+00_rb, &
           4.9600e+00_rb, 4.7600e+00_rb, 4.5600e+00_rb, 4.3600e+00_rb, 4.1600e+00_rb, &
           3.9600e+00_rb, 3.7600e+00_rb, 3.5600e+00_rb, 3.3600e+00_rb, 3.1600e+00_rb, &
           2.9600e+00_rb, 2.7600e+00_rb, 2.5600e+00_rb, 2.3600e+00_rb, 2.1600e+00_rb, &
           1.9600e+00_rb, 1.7600e+00_rb, 1.5600e+00_rb, 1.3600e+00_rb, 1.1600e+00_rb, &
           9.6000e-01_rb, 7.6000e-01_rb, 5.6000e-01_rb, 3.6000e-01_rb, 1.6000e-01_rb, &
          -4.0000e-02_rb,-2.4000e-01_rb,-4.4000e-01_rb,-6.4000e-01_rb,-8.4000e-01_rb, &
          -1.0400e+00_rb,-1.2400e+00_rb,-1.4400e+00_rb,-1.6400e+00_rb,-1.8400e+00_rb, &
          -2.0400e+00_rb,-2.2400e+00_rb,-2.4400e+00_rb,-2.6400e+00_rb,-2.8400e+00_rb, &
          -3.0400e+00_rb,-3.2400e+00_rb,-3.4400e+00_rb,-3.6400e+00_rb,-3.8400e+00_rb, &
          -4.0400e+00_rb,-4.2400e+00_rb,-4.4400e+00_rb,-4.6400e+00_rb/)

! These are the temperatures associated with the respective
! pressures for the mls standard atmosphere.

      tref(:) = (/ &
           2.9420e+02_rb, 2.8799e+02_rb, 2.7894e+02_rb, 2.6925e+02_rb, 2.5983e+02_rb, &
           2.5017e+02_rb, 2.4077e+02_rb, 2.3179e+02_rb, 2.2306e+02_rb, 2.1578e+02_rb, &
           2.1570e+02_rb, 2.1570e+02_rb, 2.1570e+02_rb, 2.1706e+02_rb, 2.1858e+02_rb, &
           2.2018e+02_rb, 2.2174e+02_rb, 2.2328e+02_rb, 2.2479e+02_rb, 2.2655e+02_rb, &
           2.2834e+02_rb, 2.3113e+02_rb, 2.3401e+02_rb, 2.3703e+02_rb, 2.4022e+02_rb, &
           2.4371e+02_rb, 2.4726e+02_rb, 2.5085e+02_rb, 2.5457e+02_rb, 2.5832e+02_rb, &
           2.6216e+02_rb, 2.6606e+02_rb, 2.6999e+02_rb, 2.7340e+02_rb, 2.7536e+02_rb, &
           2.7568e+02_rb, 2.7372e+02_rb, 2.7163e+02_rb, 2.6955e+02_rb, 2.6593e+02_rb, &
           2.6211e+02_rb, 2.5828e+02_rb, 2.5360e+02_rb, 2.4854e+02_rb, 2.4348e+02_rb, &
           2.3809e+02_rb, 2.3206e+02_rb, 2.2603e+02_rb, 2.2000e+02_rb, 2.1435e+02_rb, &
           2.0887e+02_rb, 2.0340e+02_rb, 1.9792e+02_rb, 1.9290e+02_rb, 1.8809e+02_rb, &
           1.8329e+02_rb, 1.7849e+02_rb, 1.7394e+02_rb, 1.7212e+02_rb/)

       chi_mls(1,1:12) = (/ &
        1.8760e-02_rb, 1.2223e-02_rb, 5.8909e-03_rb, 2.7675e-03_rb, 1.4065e-03_rb, &
        7.5970e-04_rb, 3.8876e-04_rb, 1.6542e-04_rb, 3.7190e-05_rb, 7.4765e-06_rb, &
        4.3082e-06_rb, 3.3319e-06_rb/)
       chi_mls(1,13:59) = (/ &
        3.2039e-06_rb,  3.1619e-06_rb,  3.2524e-06_rb,  3.4226e-06_rb,  3.6288e-06_rb, &
        3.9148e-06_rb,  4.1488e-06_rb,  4.3081e-06_rb,  4.4420e-06_rb,  4.5778e-06_rb, &
        4.7087e-06_rb,  4.7943e-06_rb,  4.8697e-06_rb,  4.9260e-06_rb,  4.9669e-06_rb, &
        4.9963e-06_rb,  5.0527e-06_rb,  5.1266e-06_rb,  5.2503e-06_rb,  5.3571e-06_rb, &
        5.4509e-06_rb,  5.4830e-06_rb,  5.5000e-06_rb,  5.5000e-06_rb,  5.4536e-06_rb, &
        5.4047e-06_rb,  5.3558e-06_rb,  5.2533e-06_rb,  5.1436e-06_rb,  5.0340e-06_rb, &
        4.8766e-06_rb,  4.6979e-06_rb,  4.5191e-06_rb,  4.3360e-06_rb,  4.1442e-06_rb, &
        3.9523e-06_rb,  3.7605e-06_rb,  3.5722e-06_rb,  3.3855e-06_rb,  3.1988e-06_rb, &
        3.0121e-06_rb,  2.8262e-06_rb,  2.6407e-06_rb,  2.4552e-06_rb,  2.2696e-06_rb, &
        4.3360e-06_rb,  4.1442e-06_rb/)
       chi_mls(2,1:12) = (/ &
        3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb, &
        3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb, &
        3.5500e-04_rb,  3.5500e-04_rb/)
       chi_mls(2,13:59) = (/ &
        3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb, &
        3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb, &
        3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb, &
        3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb, &
        3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb, &
        3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb, &
        3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb, &
        3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb,  3.5500e-04_rb, &
        3.5500e-04_rb,  3.5471e-04_rb,  3.5427e-04_rb,  3.5384e-04_rb,  3.5340e-04_rb, &
        3.5500e-04_rb,  3.5500e-04_rb/)
       chi_mls(3,1:12) = (/ &
        3.0170e-08_rb,  3.4725e-08_rb,  4.2477e-08_rb,  5.2759e-08_rb,  6.6944e-08_rb, &
        8.7130e-08_rb,  1.1391e-07_rb,  1.5677e-07_rb,  2.1788e-07_rb,  3.2443e-07_rb, &
        4.6594e-07_rb,  5.6806e-07_rb/)
       chi_mls(3,13:59) = (/ &
        6.9607e-07_rb,  1.1186e-06_rb,  1.7618e-06_rb,  2.3269e-06_rb,  2.9577e-06_rb, &
        3.6593e-06_rb,  4.5950e-06_rb,  5.3189e-06_rb,  5.9618e-06_rb,  6.5113e-06_rb, &
        7.0635e-06_rb,  7.6917e-06_rb,  8.2577e-06_rb,  8.7082e-06_rb,  8.8325e-06_rb, &
        8.7149e-06_rb,  8.0943e-06_rb,  7.3307e-06_rb,  6.3101e-06_rb,  5.3672e-06_rb, &
        4.4829e-06_rb,  3.8391e-06_rb,  3.2827e-06_rb,  2.8235e-06_rb,  2.4906e-06_rb, &
        2.1645e-06_rb,  1.8385e-06_rb,  1.6618e-06_rb,  1.5052e-06_rb,  1.3485e-06_rb, &
        1.1972e-06_rb,  1.0482e-06_rb,  8.9926e-07_rb,  7.6343e-07_rb,  6.5381e-07_rb, &
        5.4419e-07_rb,  4.3456e-07_rb,  3.6421e-07_rb,  3.1194e-07_rb,  2.5967e-07_rb, &
        2.0740e-07_rb,  1.9146e-07_rb,  1.9364e-07_rb,  1.9582e-07_rb,  1.9800e-07_rb, &
        7.6343e-07_rb,  6.5381e-07_rb/)
       chi_mls(4,1:12) = (/ &
        3.2000e-07_rb,  3.2000e-07_rb,  3.2000e-07_rb,  3.2000e-07_rb,  3.2000e-07_rb, &
        3.1965e-07_rb,  3.1532e-07_rb,  3.0383e-07_rb,  2.9422e-07_rb,  2.8495e-07_rb, &
        2.7671e-07_rb,  2.6471e-07_rb/)
       chi_mls(4,13:59) = (/ &
        2.4285e-07_rb,  2.0955e-07_rb,  1.7195e-07_rb,  1.3749e-07_rb,  1.1332e-07_rb, &
        1.0035e-07_rb,  9.1281e-08_rb,  8.5463e-08_rb,  8.0363e-08_rb,  7.3372e-08_rb, &
        6.5975e-08_rb,  5.6039e-08_rb,  4.7090e-08_rb,  3.9977e-08_rb,  3.2979e-08_rb, &
        2.6064e-08_rb,  2.1066e-08_rb,  1.6592e-08_rb,  1.3017e-08_rb,  1.0090e-08_rb, &
        7.6249e-09_rb,  6.1159e-09_rb,  4.6672e-09_rb,  3.2857e-09_rb,  2.8484e-09_rb, &
        2.4620e-09_rb,  2.0756e-09_rb,  1.8551e-09_rb,  1.6568e-09_rb,  1.4584e-09_rb, &
        1.3195e-09_rb,  1.2072e-09_rb,  1.0948e-09_rb,  9.9780e-10_rb,  9.3126e-10_rb, &
        8.6472e-10_rb,  7.9818e-10_rb,  7.5138e-10_rb,  7.1367e-10_rb,  6.7596e-10_rb, &
        6.3825e-10_rb,  6.0981e-10_rb,  5.8600e-10_rb,  5.6218e-10_rb,  5.3837e-10_rb, &
        9.9780e-10_rb,  9.3126e-10_rb/)
       chi_mls(5,1:12) = (/ &
        1.5000e-07_rb,  1.4306e-07_rb,  1.3474e-07_rb,  1.3061e-07_rb,  1.2793e-07_rb, &
        1.2038e-07_rb,  1.0798e-07_rb,  9.4238e-08_rb,  7.9488e-08_rb,  6.1386e-08_rb, &
        4.5563e-08_rb,  3.3475e-08_rb/)
       chi_mls(5,13:59) = (/ &
        2.5118e-08_rb,  1.8671e-08_rb,  1.4349e-08_rb,  1.2501e-08_rb,  1.2407e-08_rb, &
        1.3472e-08_rb,  1.4900e-08_rb,  1.6079e-08_rb,  1.7156e-08_rb,  1.8616e-08_rb, &
        2.0106e-08_rb,  2.1654e-08_rb,  2.3096e-08_rb,  2.4340e-08_rb,  2.5643e-08_rb, &
        2.6990e-08_rb,  2.8456e-08_rb,  2.9854e-08_rb,  3.0943e-08_rb,  3.2023e-08_rb, &
        3.3101e-08_rb,  3.4260e-08_rb,  3.5360e-08_rb,  3.6397e-08_rb,  3.7310e-08_rb, &
        3.8217e-08_rb,  3.9123e-08_rb,  4.1303e-08_rb,  4.3652e-08_rb,  4.6002e-08_rb, &
        5.0289e-08_rb,  5.5446e-08_rb,  6.0603e-08_rb,  6.8946e-08_rb,  8.3652e-08_rb, &
        9.8357e-08_rb,  1.1306e-07_rb,  1.4766e-07_rb,  1.9142e-07_rb,  2.3518e-07_rb, &
        2.7894e-07_rb,  3.5001e-07_rb,  4.3469e-07_rb,  5.1938e-07_rb,  6.0407e-07_rb, &
        6.8946e-08_rb,  8.3652e-08_rb/)
       chi_mls(6,1:12) = (/ &
        1.7000e-06_rb,  1.7000e-06_rb,  1.6999e-06_rb,  1.6904e-06_rb,  1.6671e-06_rb, &
        1.6351e-06_rb,  1.6098e-06_rb,  1.5590e-06_rb,  1.5120e-06_rb,  1.4741e-06_rb, &
        1.4385e-06_rb,  1.4002e-06_rb/)
       chi_mls(6,13:59) = (/ &
        1.3573e-06_rb,  1.3130e-06_rb,  1.2512e-06_rb,  1.1668e-06_rb,  1.0553e-06_rb, &
        9.3281e-07_rb,  8.1217e-07_rb,  7.5239e-07_rb,  7.0728e-07_rb,  6.6722e-07_rb, &
        6.2733e-07_rb,  5.8604e-07_rb,  5.4769e-07_rb,  5.1480e-07_rb,  4.8206e-07_rb, &
        4.4943e-07_rb,  4.1702e-07_rb,  3.8460e-07_rb,  3.5200e-07_rb,  3.1926e-07_rb, &
        2.8646e-07_rb,  2.5498e-07_rb,  2.2474e-07_rb,  1.9588e-07_rb,  1.8295e-07_rb, &
        1.7089e-07_rb,  1.5882e-07_rb,  1.5536e-07_rb,  1.5304e-07_rb,  1.5072e-07_rb, &
        1.5000e-07_rb,  1.5000e-07_rb,  1.5000e-07_rb,  1.5000e-07_rb,  1.5000e-07_rb, &
        1.5000e-07_rb,  1.5000e-07_rb,  1.5000e-07_rb,  1.5000e-07_rb,  1.5000e-07_rb, &
        1.5000e-07_rb,  1.5000e-07_rb,  1.5000e-07_rb,  1.5000e-07_rb,  1.5000e-07_rb, &
        1.5000e-07_rb,  1.5000e-07_rb/)
       chi_mls(7,1:12) = (/ &
        0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb, &
        0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb, &
        0.2090_rb,  0.2090_rb/)
       chi_mls(7,13:59) = (/ &
        0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb, &
        0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb, &
        0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb, &
        0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb, &
        0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb, &
        0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb, &
        0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb, &
        0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb, &
        0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb,  0.2090_rb, &
        0.2090_rb,  0.2090_rb/)

      end subroutine lwatmref

!***************************************************************************
      subroutine lwavplank
!***************************************************************************

      save

      totplnk(1:50,  1) = (/ &
      0.14783e-05_rb,0.15006e-05_rb,0.15230e-05_rb,0.15455e-05_rb,0.15681e-05_rb, &
      0.15908e-05_rb,0.16136e-05_rb,0.16365e-05_rb,0.16595e-05_rb,0.16826e-05_rb, &
      0.17059e-05_rb,0.17292e-05_rb,0.17526e-05_rb,0.17762e-05_rb,0.17998e-05_rb, &
      0.18235e-05_rb,0.18473e-05_rb,0.18712e-05_rb,0.18953e-05_rb,0.19194e-05_rb, &
      0.19435e-05_rb,0.19678e-05_rb,0.19922e-05_rb,0.20166e-05_rb,0.20412e-05_rb, &
      0.20658e-05_rb,0.20905e-05_rb,0.21153e-05_rb,0.21402e-05_rb,0.21652e-05_rb, &
      0.21902e-05_rb,0.22154e-05_rb,0.22406e-05_rb,0.22659e-05_rb,0.22912e-05_rb, &
      0.23167e-05_rb,0.23422e-05_rb,0.23678e-05_rb,0.23934e-05_rb,0.24192e-05_rb, &
      0.24450e-05_rb,0.24709e-05_rb,0.24968e-05_rb,0.25229e-05_rb,0.25490e-05_rb, &
      0.25751e-05_rb,0.26014e-05_rb,0.26277e-05_rb,0.26540e-05_rb,0.26805e-05_rb/)
      totplnk(51:100,  1) = (/ &
      0.27070e-05_rb,0.27335e-05_rb,0.27602e-05_rb,0.27869e-05_rb,0.28136e-05_rb, &
      0.28404e-05_rb,0.28673e-05_rb,0.28943e-05_rb,0.29213e-05_rb,0.29483e-05_rb, &
      0.29754e-05_rb,0.30026e-05_rb,0.30298e-05_rb,0.30571e-05_rb,0.30845e-05_rb, &
      0.31119e-05_rb,0.31393e-05_rb,0.31669e-05_rb,0.31944e-05_rb,0.32220e-05_rb, &
      0.32497e-05_rb,0.32774e-05_rb,0.33052e-05_rb,0.33330e-05_rb,0.33609e-05_rb, &
      0.33888e-05_rb,0.34168e-05_rb,0.34448e-05_rb,0.34729e-05_rb,0.35010e-05_rb, &
      0.35292e-05_rb,0.35574e-05_rb,0.35857e-05_rb,0.36140e-05_rb,0.36424e-05_rb, &
      0.36708e-05_rb,0.36992e-05_rb,0.37277e-05_rb,0.37563e-05_rb,0.37848e-05_rb, &
      0.38135e-05_rb,0.38421e-05_rb,0.38708e-05_rb,0.38996e-05_rb,0.39284e-05_rb, &
      0.39572e-05_rb,0.39861e-05_rb,0.40150e-05_rb,0.40440e-05_rb,0.40730e-05_rb/)
      totplnk(101:150,  1) = (/ &
      0.41020e-05_rb,0.41311e-05_rb,0.41602e-05_rb,0.41893e-05_rb,0.42185e-05_rb, &
      0.42477e-05_rb,0.42770e-05_rb,0.43063e-05_rb,0.43356e-05_rb,0.43650e-05_rb, &
      0.43944e-05_rb,0.44238e-05_rb,0.44533e-05_rb,0.44828e-05_rb,0.45124e-05_rb, &
      0.45419e-05_rb,0.45715e-05_rb,0.46012e-05_rb,0.46309e-05_rb,0.46606e-05_rb, &
      0.46903e-05_rb,0.47201e-05_rb,0.47499e-05_rb,0.47797e-05_rb,0.48096e-05_rb, &
      0.48395e-05_rb,0.48695e-05_rb,0.48994e-05_rb,0.49294e-05_rb,0.49594e-05_rb, &
      0.49895e-05_rb,0.50196e-05_rb,0.50497e-05_rb,0.50798e-05_rb,0.51100e-05_rb, &
      0.51402e-05_rb,0.51704e-05_rb,0.52007e-05_rb,0.52309e-05_rb,0.52612e-05_rb, &
      0.52916e-05_rb,0.53219e-05_rb,0.53523e-05_rb,0.53827e-05_rb,0.54132e-05_rb, &
      0.54436e-05_rb,0.54741e-05_rb,0.55047e-05_rb,0.55352e-05_rb,0.55658e-05_rb/)
      totplnk(151:181,  1) = (/ &
      0.55964e-05_rb,0.56270e-05_rb,0.56576e-05_rb,0.56883e-05_rb,0.57190e-05_rb, &
      0.57497e-05_rb,0.57804e-05_rb,0.58112e-05_rb,0.58420e-05_rb,0.58728e-05_rb, &
      0.59036e-05_rb,0.59345e-05_rb,0.59653e-05_rb,0.59962e-05_rb,0.60272e-05_rb, &
      0.60581e-05_rb,0.60891e-05_rb,0.61201e-05_rb,0.61511e-05_rb,0.61821e-05_rb, &
      0.62131e-05_rb,0.62442e-05_rb,0.62753e-05_rb,0.63064e-05_rb,0.63376e-05_rb, &
      0.63687e-05_rb,0.63998e-05_rb,0.64310e-05_rb,0.64622e-05_rb,0.64935e-05_rb, &
      0.65247e-05_rb/)
      totplnk(1:50,  2) = (/ &
      0.20262e-05_rb,0.20757e-05_rb,0.21257e-05_rb,0.21763e-05_rb,0.22276e-05_rb, &
      0.22794e-05_rb,0.23319e-05_rb,0.23849e-05_rb,0.24386e-05_rb,0.24928e-05_rb, &
      0.25477e-05_rb,0.26031e-05_rb,0.26591e-05_rb,0.27157e-05_rb,0.27728e-05_rb, &
      0.28306e-05_rb,0.28889e-05_rb,0.29478e-05_rb,0.30073e-05_rb,0.30673e-05_rb, &
      0.31279e-05_rb,0.31890e-05_rb,0.32507e-05_rb,0.33129e-05_rb,0.33757e-05_rb, &
      0.34391e-05_rb,0.35029e-05_rb,0.35674e-05_rb,0.36323e-05_rb,0.36978e-05_rb, &
      0.37638e-05_rb,0.38304e-05_rb,0.38974e-05_rb,0.39650e-05_rb,0.40331e-05_rb, &
      0.41017e-05_rb,0.41708e-05_rb,0.42405e-05_rb,0.43106e-05_rb,0.43812e-05_rb, &
      0.44524e-05_rb,0.45240e-05_rb,0.45961e-05_rb,0.46687e-05_rb,0.47418e-05_rb, &
      0.48153e-05_rb,0.48894e-05_rb,0.49639e-05_rb,0.50389e-05_rb,0.51143e-05_rb/)
      totplnk(51:100,  2) = (/ &
      0.51902e-05_rb,0.52666e-05_rb,0.53434e-05_rb,0.54207e-05_rb,0.54985e-05_rb, &
      0.55767e-05_rb,0.56553e-05_rb,0.57343e-05_rb,0.58139e-05_rb,0.58938e-05_rb, &
      0.59742e-05_rb,0.60550e-05_rb,0.61362e-05_rb,0.62179e-05_rb,0.63000e-05_rb, &
      0.63825e-05_rb,0.64654e-05_rb,0.65487e-05_rb,0.66324e-05_rb,0.67166e-05_rb, &
      0.68011e-05_rb,0.68860e-05_rb,0.69714e-05_rb,0.70571e-05_rb,0.71432e-05_rb, &
      0.72297e-05_rb,0.73166e-05_rb,0.74039e-05_rb,0.74915e-05_rb,0.75796e-05_rb, &
      0.76680e-05_rb,0.77567e-05_rb,0.78459e-05_rb,0.79354e-05_rb,0.80252e-05_rb, &
      0.81155e-05_rb,0.82061e-05_rb,0.82970e-05_rb,0.83883e-05_rb,0.84799e-05_rb, &
      0.85719e-05_rb,0.86643e-05_rb,0.87569e-05_rb,0.88499e-05_rb,0.89433e-05_rb, &
      0.90370e-05_rb,0.91310e-05_rb,0.92254e-05_rb,0.93200e-05_rb,0.94150e-05_rb/)
      totplnk(101:150,  2) = (/ &
      0.95104e-05_rb,0.96060e-05_rb,0.97020e-05_rb,0.97982e-05_rb,0.98948e-05_rb, &
      0.99917e-05_rb,0.10089e-04_rb,0.10186e-04_rb,0.10284e-04_rb,0.10382e-04_rb, &
      0.10481e-04_rb,0.10580e-04_rb,0.10679e-04_rb,0.10778e-04_rb,0.10877e-04_rb, &
      0.10977e-04_rb,0.11077e-04_rb,0.11178e-04_rb,0.11279e-04_rb,0.11380e-04_rb, &
      0.11481e-04_rb,0.11583e-04_rb,0.11684e-04_rb,0.11786e-04_rb,0.11889e-04_rb, &
      0.11992e-04_rb,0.12094e-04_rb,0.12198e-04_rb,0.12301e-04_rb,0.12405e-04_rb, &
      0.12509e-04_rb,0.12613e-04_rb,0.12717e-04_rb,0.12822e-04_rb,0.12927e-04_rb, &
      0.13032e-04_rb,0.13138e-04_rb,0.13244e-04_rb,0.13349e-04_rb,0.13456e-04_rb, &
      0.13562e-04_rb,0.13669e-04_rb,0.13776e-04_rb,0.13883e-04_rb,0.13990e-04_rb, &
      0.14098e-04_rb,0.14206e-04_rb,0.14314e-04_rb,0.14422e-04_rb,0.14531e-04_rb/)
      totplnk(151:181,  2) = (/ &
      0.14639e-04_rb,0.14748e-04_rb,0.14857e-04_rb,0.14967e-04_rb,0.15076e-04_rb, &
      0.15186e-04_rb,0.15296e-04_rb,0.15407e-04_rb,0.15517e-04_rb,0.15628e-04_rb, &
      0.15739e-04_rb,0.15850e-04_rb,0.15961e-04_rb,0.16072e-04_rb,0.16184e-04_rb, &
      0.16296e-04_rb,0.16408e-04_rb,0.16521e-04_rb,0.16633e-04_rb,0.16746e-04_rb, &
      0.16859e-04_rb,0.16972e-04_rb,0.17085e-04_rb,0.17198e-04_rb,0.17312e-04_rb, &
      0.17426e-04_rb,0.17540e-04_rb,0.17654e-04_rb,0.17769e-04_rb,0.17883e-04_rb, &
      0.17998e-04_rb/)
      totplnk(1:50, 3) = (/ &
      1.34822e-06_rb,1.39134e-06_rb,1.43530e-06_rb,1.48010e-06_rb,1.52574e-06_rb, &
      1.57222e-06_rb,1.61956e-06_rb,1.66774e-06_rb,1.71678e-06_rb,1.76666e-06_rb, &
      1.81741e-06_rb,1.86901e-06_rb,1.92147e-06_rb,1.97479e-06_rb,2.02898e-06_rb, &
      2.08402e-06_rb,2.13993e-06_rb,2.19671e-06_rb,2.25435e-06_rb,2.31285e-06_rb, &
      2.37222e-06_rb,2.43246e-06_rb,2.49356e-06_rb,2.55553e-06_rb,2.61837e-06_rb, &
      2.68207e-06_rb,2.74664e-06_rb,2.81207e-06_rb,2.87837e-06_rb,2.94554e-06_rb, &
      3.01356e-06_rb,3.08245e-06_rb,3.15221e-06_rb,3.22282e-06_rb,3.29429e-06_rb, &
      3.36662e-06_rb,3.43982e-06_rb,3.51386e-06_rb,3.58876e-06_rb,3.66451e-06_rb, &
      3.74112e-06_rb,3.81857e-06_rb,3.89688e-06_rb,3.97602e-06_rb,4.05601e-06_rb, &
      4.13685e-06_rb,4.21852e-06_rb,4.30104e-06_rb,4.38438e-06_rb,4.46857e-06_rb/)
      totplnk(51:100, 3) = (/ &
      4.55358e-06_rb,4.63943e-06_rb,4.72610e-06_rb,4.81359e-06_rb,4.90191e-06_rb, &
      4.99105e-06_rb,5.08100e-06_rb,5.17176e-06_rb,5.26335e-06_rb,5.35573e-06_rb, &
      5.44892e-06_rb,5.54292e-06_rb,5.63772e-06_rb,5.73331e-06_rb,5.82970e-06_rb, &
      5.92688e-06_rb,6.02485e-06_rb,6.12360e-06_rb,6.22314e-06_rb,6.32346e-06_rb, &
      6.42455e-06_rb,6.52641e-06_rb,6.62906e-06_rb,6.73247e-06_rb,6.83664e-06_rb, &
      6.94156e-06_rb,7.04725e-06_rb,7.15370e-06_rb,7.26089e-06_rb,7.36883e-06_rb, &
      7.47752e-06_rb,7.58695e-06_rb,7.69712e-06_rb,7.80801e-06_rb,7.91965e-06_rb, &
      8.03201e-06_rb,8.14510e-06_rb,8.25891e-06_rb,8.37343e-06_rb,8.48867e-06_rb, &
      8.60463e-06_rb,8.72128e-06_rb,8.83865e-06_rb,8.95672e-06_rb,9.07548e-06_rb, &
      9.19495e-06_rb,9.31510e-06_rb,9.43594e-06_rb,9.55745e-06_rb,9.67966e-06_rb/)
      totplnk(101:150, 3) = (/ &
      9.80254e-06_rb,9.92609e-06_rb,1.00503e-05_rb,1.01752e-05_rb,1.03008e-05_rb, &
      1.04270e-05_rb,1.05539e-05_rb,1.06814e-05_rb,1.08096e-05_rb,1.09384e-05_rb, &
      1.10679e-05_rb,1.11980e-05_rb,1.13288e-05_rb,1.14601e-05_rb,1.15922e-05_rb, &
      1.17248e-05_rb,1.18581e-05_rb,1.19920e-05_rb,1.21265e-05_rb,1.22616e-05_rb, &
      1.23973e-05_rb,1.25337e-05_rb,1.26706e-05_rb,1.28081e-05_rb,1.29463e-05_rb, &
      1.30850e-05_rb,1.32243e-05_rb,1.33642e-05_rb,1.35047e-05_rb,1.36458e-05_rb, &
      1.37875e-05_rb,1.39297e-05_rb,1.40725e-05_rb,1.42159e-05_rb,1.43598e-05_rb, &
      1.45044e-05_rb,1.46494e-05_rb,1.47950e-05_rb,1.49412e-05_rb,1.50879e-05_rb, &
      1.52352e-05_rb,1.53830e-05_rb,1.55314e-05_rb,1.56803e-05_rb,1.58297e-05_rb, &
      1.59797e-05_rb,1.61302e-05_rb,1.62812e-05_rb,1.64327e-05_rb,1.65848e-05_rb/)
      totplnk(151:181, 3) = (/ &
      1.67374e-05_rb,1.68904e-05_rb,1.70441e-05_rb,1.71982e-05_rb,1.73528e-05_rb, &
      1.75079e-05_rb,1.76635e-05_rb,1.78197e-05_rb,1.79763e-05_rb,1.81334e-05_rb, &
      1.82910e-05_rb,1.84491e-05_rb,1.86076e-05_rb,1.87667e-05_rb,1.89262e-05_rb, &
      1.90862e-05_rb,1.92467e-05_rb,1.94076e-05_rb,1.95690e-05_rb,1.97309e-05_rb, &
      1.98932e-05_rb,2.00560e-05_rb,2.02193e-05_rb,2.03830e-05_rb,2.05472e-05_rb, &
      2.07118e-05_rb,2.08768e-05_rb,2.10423e-05_rb,2.12083e-05_rb,2.13747e-05_rb, &
      2.15414e-05_rb/)
      totplnk(1:50, 4) = (/ &
      8.90528e-07_rb,9.24222e-07_rb,9.58757e-07_rb,9.94141e-07_rb,1.03038e-06_rb, &
      1.06748e-06_rb,1.10545e-06_rb,1.14430e-06_rb,1.18403e-06_rb,1.22465e-06_rb, &
      1.26618e-06_rb,1.30860e-06_rb,1.35193e-06_rb,1.39619e-06_rb,1.44136e-06_rb, &
      1.48746e-06_rb,1.53449e-06_rb,1.58246e-06_rb,1.63138e-06_rb,1.68124e-06_rb, &
      1.73206e-06_rb,1.78383e-06_rb,1.83657e-06_rb,1.89028e-06_rb,1.94495e-06_rb, &
      2.00060e-06_rb,2.05724e-06_rb,2.11485e-06_rb,2.17344e-06_rb,2.23303e-06_rb, &
      2.29361e-06_rb,2.35519e-06_rb,2.41777e-06_rb,2.48134e-06_rb,2.54592e-06_rb, &
      2.61151e-06_rb,2.67810e-06_rb,2.74571e-06_rb,2.81433e-06_rb,2.88396e-06_rb, &
      2.95461e-06_rb,3.02628e-06_rb,3.09896e-06_rb,3.17267e-06_rb,3.24741e-06_rb, &
      3.32316e-06_rb,3.39994e-06_rb,3.47774e-06_rb,3.55657e-06_rb,3.63642e-06_rb/)
      totplnk(51:100, 4) = (/ &
      3.71731e-06_rb,3.79922e-06_rb,3.88216e-06_rb,3.96612e-06_rb,4.05112e-06_rb, &
      4.13714e-06_rb,4.22419e-06_rb,4.31227e-06_rb,4.40137e-06_rb,4.49151e-06_rb, &
      4.58266e-06_rb,4.67485e-06_rb,4.76806e-06_rb,4.86229e-06_rb,4.95754e-06_rb, &
      5.05383e-06_rb,5.15113e-06_rb,5.24946e-06_rb,5.34879e-06_rb,5.44916e-06_rb, &
      5.55053e-06_rb,5.65292e-06_rb,5.75632e-06_rb,5.86073e-06_rb,5.96616e-06_rb, &
      6.07260e-06_rb,6.18003e-06_rb,6.28848e-06_rb,6.39794e-06_rb,6.50838e-06_rb, &
      6.61983e-06_rb,6.73229e-06_rb,6.84573e-06_rb,6.96016e-06_rb,7.07559e-06_rb, &
      7.19200e-06_rb,7.30940e-06_rb,7.42779e-06_rb,7.54715e-06_rb,7.66749e-06_rb, &
      7.78882e-06_rb,7.91110e-06_rb,8.03436e-06_rb,8.15859e-06_rb,8.28379e-06_rb, &
      8.40994e-06_rb,8.53706e-06_rb,8.66515e-06_rb,8.79418e-06_rb,8.92416e-06_rb/)
      totplnk(101:150, 4) = (/ &
      9.05510e-06_rb,9.18697e-06_rb,9.31979e-06_rb,9.45356e-06_rb,9.58826e-06_rb, &
      9.72389e-06_rb,9.86046e-06_rb,9.99793e-06_rb,1.01364e-05_rb,1.02757e-05_rb, &
      1.04159e-05_rb,1.05571e-05_rb,1.06992e-05_rb,1.08422e-05_rb,1.09861e-05_rb, &
      1.11309e-05_rb,1.12766e-05_rb,1.14232e-05_rb,1.15707e-05_rb,1.17190e-05_rb, &
      1.18683e-05_rb,1.20184e-05_rb,1.21695e-05_rb,1.23214e-05_rb,1.24741e-05_rb, &
      1.26277e-05_rb,1.27822e-05_rb,1.29376e-05_rb,1.30939e-05_rb,1.32509e-05_rb, &
      1.34088e-05_rb,1.35676e-05_rb,1.37273e-05_rb,1.38877e-05_rb,1.40490e-05_rb, &
      1.42112e-05_rb,1.43742e-05_rb,1.45380e-05_rb,1.47026e-05_rb,1.48680e-05_rb, &
      1.50343e-05_rb,1.52014e-05_rb,1.53692e-05_rb,1.55379e-05_rb,1.57074e-05_rb, &
      1.58778e-05_rb,1.60488e-05_rb,1.62207e-05_rb,1.63934e-05_rb,1.65669e-05_rb/)
      totplnk(151:181, 4) = (/ &
      1.67411e-05_rb,1.69162e-05_rb,1.70920e-05_rb,1.72685e-05_rb,1.74459e-05_rb, &
      1.76240e-05_rb,1.78029e-05_rb,1.79825e-05_rb,1.81629e-05_rb,1.83440e-05_rb, &
      1.85259e-05_rb,1.87086e-05_rb,1.88919e-05_rb,1.90760e-05_rb,1.92609e-05_rb, &
      1.94465e-05_rb,1.96327e-05_rb,1.98199e-05_rb,2.00076e-05_rb,2.01961e-05_rb, &
      2.03853e-05_rb,2.05752e-05_rb,2.07658e-05_rb,2.09571e-05_rb,2.11491e-05_rb, &
      2.13418e-05_rb,2.15352e-05_rb,2.17294e-05_rb,2.19241e-05_rb,2.21196e-05_rb, &
      2.23158e-05_rb/)
      totplnk(1:50, 5) = (/ &
      5.70230e-07_rb,5.94788e-07_rb,6.20085e-07_rb,6.46130e-07_rb,6.72936e-07_rb, &
      7.00512e-07_rb,7.28869e-07_rb,7.58019e-07_rb,7.87971e-07_rb,8.18734e-07_rb, &
      8.50320e-07_rb,8.82738e-07_rb,9.15999e-07_rb,9.50110e-07_rb,9.85084e-07_rb, &
      1.02093e-06_rb,1.05765e-06_rb,1.09527e-06_rb,1.13378e-06_rb,1.17320e-06_rb, &
      1.21353e-06_rb,1.25479e-06_rb,1.29698e-06_rb,1.34011e-06_rb,1.38419e-06_rb, &
      1.42923e-06_rb,1.47523e-06_rb,1.52221e-06_rb,1.57016e-06_rb,1.61910e-06_rb, &
      1.66904e-06_rb,1.71997e-06_rb,1.77192e-06_rb,1.82488e-06_rb,1.87886e-06_rb, &
      1.93387e-06_rb,1.98991e-06_rb,2.04699e-06_rb,2.10512e-06_rb,2.16430e-06_rb, &
      2.22454e-06_rb,2.28584e-06_rb,2.34821e-06_rb,2.41166e-06_rb,2.47618e-06_rb, &
      2.54178e-06_rb,2.60847e-06_rb,2.67626e-06_rb,2.74514e-06_rb,2.81512e-06_rb/)
      totplnk(51:100, 5) = (/ &
      2.88621e-06_rb,2.95841e-06_rb,3.03172e-06_rb,3.10615e-06_rb,3.18170e-06_rb, &
      3.25838e-06_rb,3.33618e-06_rb,3.41511e-06_rb,3.49518e-06_rb,3.57639e-06_rb, &
      3.65873e-06_rb,3.74221e-06_rb,3.82684e-06_rb,3.91262e-06_rb,3.99955e-06_rb, &
      4.08763e-06_rb,4.17686e-06_rb,4.26725e-06_rb,4.35880e-06_rb,4.45150e-06_rb, &
      4.54537e-06_rb,4.64039e-06_rb,4.73659e-06_rb,4.83394e-06_rb,4.93246e-06_rb, &
      5.03215e-06_rb,5.13301e-06_rb,5.23504e-06_rb,5.33823e-06_rb,5.44260e-06_rb, &
      5.54814e-06_rb,5.65484e-06_rb,5.76272e-06_rb,5.87177e-06_rb,5.98199e-06_rb, &
      6.09339e-06_rb,6.20596e-06_rb,6.31969e-06_rb,6.43460e-06_rb,6.55068e-06_rb, &
      6.66793e-06_rb,6.78636e-06_rb,6.90595e-06_rb,7.02670e-06_rb,7.14863e-06_rb, &
      7.27173e-06_rb,7.39599e-06_rb,7.52142e-06_rb,7.64802e-06_rb,7.77577e-06_rb/)
      totplnk(101:150, 5) = (/ &
      7.90469e-06_rb,8.03477e-06_rb,8.16601e-06_rb,8.29841e-06_rb,8.43198e-06_rb, &
      8.56669e-06_rb,8.70256e-06_rb,8.83957e-06_rb,8.97775e-06_rb,9.11706e-06_rb, &
      9.25753e-06_rb,9.39915e-06_rb,9.54190e-06_rb,9.68580e-06_rb,9.83085e-06_rb, &
      9.97704e-06_rb,1.01243e-05_rb,1.02728e-05_rb,1.04224e-05_rb,1.05731e-05_rb, &
      1.07249e-05_rb,1.08779e-05_rb,1.10320e-05_rb,1.11872e-05_rb,1.13435e-05_rb, &
      1.15009e-05_rb,1.16595e-05_rb,1.18191e-05_rb,1.19799e-05_rb,1.21418e-05_rb, &
      1.23048e-05_rb,1.24688e-05_rb,1.26340e-05_rb,1.28003e-05_rb,1.29676e-05_rb, &
      1.31361e-05_rb,1.33056e-05_rb,1.34762e-05_rb,1.36479e-05_rb,1.38207e-05_rb, &
      1.39945e-05_rb,1.41694e-05_rb,1.43454e-05_rb,1.45225e-05_rb,1.47006e-05_rb, &
      1.48797e-05_rb,1.50600e-05_rb,1.52413e-05_rb,1.54236e-05_rb,1.56070e-05_rb/)
      totplnk(151:181, 5) = (/ &
      1.57914e-05_rb,1.59768e-05_rb,1.61633e-05_rb,1.63509e-05_rb,1.65394e-05_rb, &
      1.67290e-05_rb,1.69197e-05_rb,1.71113e-05_rb,1.73040e-05_rb,1.74976e-05_rb, &
      1.76923e-05_rb,1.78880e-05_rb,1.80847e-05_rb,1.82824e-05_rb,1.84811e-05_rb, &
      1.86808e-05_rb,1.88814e-05_rb,1.90831e-05_rb,1.92857e-05_rb,1.94894e-05_rb, &
      1.96940e-05_rb,1.98996e-05_rb,2.01061e-05_rb,2.03136e-05_rb,2.05221e-05_rb, &
      2.07316e-05_rb,2.09420e-05_rb,2.11533e-05_rb,2.13657e-05_rb,2.15789e-05_rb, &
      2.17931e-05_rb/)
      totplnk(1:50, 6) = (/ &
      2.73493e-07_rb,2.87408e-07_rb,3.01848e-07_rb,3.16825e-07_rb,3.32352e-07_rb, &
      3.48439e-07_rb,3.65100e-07_rb,3.82346e-07_rb,4.00189e-07_rb,4.18641e-07_rb, &
      4.37715e-07_rb,4.57422e-07_rb,4.77774e-07_rb,4.98784e-07_rb,5.20464e-07_rb, &
      5.42824e-07_rb,5.65879e-07_rb,5.89638e-07_rb,6.14115e-07_rb,6.39320e-07_rb, &
      6.65266e-07_rb,6.91965e-07_rb,7.19427e-07_rb,7.47666e-07_rb,7.76691e-07_rb, &
      8.06516e-07_rb,8.37151e-07_rb,8.68607e-07_rb,9.00896e-07_rb,9.34029e-07_rb, &
      9.68018e-07_rb,1.00287e-06_rb,1.03860e-06_rb,1.07522e-06_rb,1.11274e-06_rb, &
      1.15117e-06_rb,1.19052e-06_rb,1.23079e-06_rb,1.27201e-06_rb,1.31418e-06_rb, &
      1.35731e-06_rb,1.40141e-06_rb,1.44650e-06_rb,1.49257e-06_rb,1.53965e-06_rb, &
      1.58773e-06_rb,1.63684e-06_rb,1.68697e-06_rb,1.73815e-06_rb,1.79037e-06_rb/)
      totplnk(51:100, 6) = (/ &
      1.84365e-06_rb,1.89799e-06_rb,1.95341e-06_rb,2.00991e-06_rb,2.06750e-06_rb, &
      2.12619e-06_rb,2.18599e-06_rb,2.24691e-06_rb,2.30895e-06_rb,2.37212e-06_rb, &
      2.43643e-06_rb,2.50189e-06_rb,2.56851e-06_rb,2.63628e-06_rb,2.70523e-06_rb, &
      2.77536e-06_rb,2.84666e-06_rb,2.91916e-06_rb,2.99286e-06_rb,3.06776e-06_rb, &
      3.14387e-06_rb,3.22120e-06_rb,3.29975e-06_rb,3.37953e-06_rb,3.46054e-06_rb, &
      3.54280e-06_rb,3.62630e-06_rb,3.71105e-06_rb,3.79707e-06_rb,3.88434e-06_rb, &
      3.97288e-06_rb,4.06270e-06_rb,4.15380e-06_rb,4.24617e-06_rb,4.33984e-06_rb, &
      4.43479e-06_rb,4.53104e-06_rb,4.62860e-06_rb,4.72746e-06_rb,4.82763e-06_rb, &
      4.92911e-06_rb,5.03191e-06_rb,5.13603e-06_rb,5.24147e-06_rb,5.34824e-06_rb, &
      5.45634e-06_rb,5.56578e-06_rb,5.67656e-06_rb,5.78867e-06_rb,5.90213e-06_rb/)
      totplnk(101:150, 6) = (/ &
      6.01694e-06_rb,6.13309e-06_rb,6.25060e-06_rb,6.36947e-06_rb,6.48968e-06_rb, &
      6.61126e-06_rb,6.73420e-06_rb,6.85850e-06_rb,6.98417e-06_rb,7.11120e-06_rb, &
      7.23961e-06_rb,7.36938e-06_rb,7.50053e-06_rb,7.63305e-06_rb,7.76694e-06_rb, &
      7.90221e-06_rb,8.03887e-06_rb,8.17690e-06_rb,8.31632e-06_rb,8.45710e-06_rb, &
      8.59928e-06_rb,8.74282e-06_rb,8.88776e-06_rb,9.03409e-06_rb,9.18179e-06_rb, &
      9.33088e-06_rb,9.48136e-06_rb,9.63323e-06_rb,9.78648e-06_rb,9.94111e-06_rb, &
      1.00971e-05_rb,1.02545e-05_rb,1.04133e-05_rb,1.05735e-05_rb,1.07351e-05_rb, &
      1.08980e-05_rb,1.10624e-05_rb,1.12281e-05_rb,1.13952e-05_rb,1.15637e-05_rb, &
      1.17335e-05_rb,1.19048e-05_rb,1.20774e-05_rb,1.22514e-05_rb,1.24268e-05_rb, &
      1.26036e-05_rb,1.27817e-05_rb,1.29612e-05_rb,1.31421e-05_rb,1.33244e-05_rb/)
      totplnk(151:181, 6) = (/ &
      1.35080e-05_rb,1.36930e-05_rb,1.38794e-05_rb,1.40672e-05_rb,1.42563e-05_rb, &
      1.44468e-05_rb,1.46386e-05_rb,1.48318e-05_rb,1.50264e-05_rb,1.52223e-05_rb, &
      1.54196e-05_rb,1.56182e-05_rb,1.58182e-05_rb,1.60196e-05_rb,1.62223e-05_rb, &
      1.64263e-05_rb,1.66317e-05_rb,1.68384e-05_rb,1.70465e-05_rb,1.72559e-05_rb, &
      1.74666e-05_rb,1.76787e-05_rb,1.78921e-05_rb,1.81069e-05_rb,1.83230e-05_rb, &
      1.85404e-05_rb,1.87591e-05_rb,1.89791e-05_rb,1.92005e-05_rb,1.94232e-05_rb, &
      1.96471e-05_rb/)
      totplnk(1:50, 7) = (/ &
      1.25349e-07_rb,1.32735e-07_rb,1.40458e-07_rb,1.48527e-07_rb,1.56954e-07_rb, &
      1.65748e-07_rb,1.74920e-07_rb,1.84481e-07_rb,1.94443e-07_rb,2.04814e-07_rb, &
      2.15608e-07_rb,2.26835e-07_rb,2.38507e-07_rb,2.50634e-07_rb,2.63229e-07_rb, &
      2.76301e-07_rb,2.89864e-07_rb,3.03930e-07_rb,3.18508e-07_rb,3.33612e-07_rb, &
      3.49253e-07_rb,3.65443e-07_rb,3.82195e-07_rb,3.99519e-07_rb,4.17428e-07_rb, &
      4.35934e-07_rb,4.55050e-07_rb,4.74785e-07_rb,4.95155e-07_rb,5.16170e-07_rb, &
      5.37844e-07_rb,5.60186e-07_rb,5.83211e-07_rb,6.06929e-07_rb,6.31355e-07_rb, &
      6.56498e-07_rb,6.82373e-07_rb,7.08990e-07_rb,7.36362e-07_rb,7.64501e-07_rb, &
      7.93420e-07_rb,8.23130e-07_rb,8.53643e-07_rb,8.84971e-07_rb,9.17128e-07_rb, &
      9.50123e-07_rb,9.83969e-07_rb,1.01868e-06_rb,1.05426e-06_rb,1.09073e-06_rb/)
      totplnk(51:100, 7) = (/ &
      1.12810e-06_rb,1.16638e-06_rb,1.20558e-06_rb,1.24572e-06_rb,1.28680e-06_rb, &
      1.32883e-06_rb,1.37183e-06_rb,1.41581e-06_rb,1.46078e-06_rb,1.50675e-06_rb, &
      1.55374e-06_rb,1.60174e-06_rb,1.65078e-06_rb,1.70087e-06_rb,1.75200e-06_rb, &
      1.80421e-06_rb,1.85749e-06_rb,1.91186e-06_rb,1.96732e-06_rb,2.02389e-06_rb, &
      2.08159e-06_rb,2.14040e-06_rb,2.20035e-06_rb,2.26146e-06_rb,2.32372e-06_rb, &
      2.38714e-06_rb,2.45174e-06_rb,2.51753e-06_rb,2.58451e-06_rb,2.65270e-06_rb, &
      2.72210e-06_rb,2.79272e-06_rb,2.86457e-06_rb,2.93767e-06_rb,3.01201e-06_rb, &
      3.08761e-06_rb,3.16448e-06_rb,3.24261e-06_rb,3.32204e-06_rb,3.40275e-06_rb, &
      3.48476e-06_rb,3.56808e-06_rb,3.65271e-06_rb,3.73866e-06_rb,3.82595e-06_rb, &
      3.91456e-06_rb,4.00453e-06_rb,4.09584e-06_rb,4.18851e-06_rb,4.28254e-06_rb/)
      totplnk(101:150, 7) = (/ &
      4.37796e-06_rb,4.47475e-06_rb,4.57293e-06_rb,4.67249e-06_rb,4.77346e-06_rb, &
      4.87583e-06_rb,4.97961e-06_rb,5.08481e-06_rb,5.19143e-06_rb,5.29948e-06_rb, &
      5.40896e-06_rb,5.51989e-06_rb,5.63226e-06_rb,5.74608e-06_rb,5.86136e-06_rb, &
      5.97810e-06_rb,6.09631e-06_rb,6.21597e-06_rb,6.33713e-06_rb,6.45976e-06_rb, &
      6.58388e-06_rb,6.70950e-06_rb,6.83661e-06_rb,6.96521e-06_rb,7.09531e-06_rb, &
      7.22692e-06_rb,7.36005e-06_rb,7.49468e-06_rb,7.63084e-06_rb,7.76851e-06_rb, &
      7.90773e-06_rb,8.04846e-06_rb,8.19072e-06_rb,8.33452e-06_rb,8.47985e-06_rb, &
      8.62674e-06_rb,8.77517e-06_rb,8.92514e-06_rb,9.07666e-06_rb,9.22975e-06_rb, &
      9.38437e-06_rb,9.54057e-06_rb,9.69832e-06_rb,9.85762e-06_rb,1.00185e-05_rb, &
      1.01810e-05_rb,1.03450e-05_rb,1.05106e-05_rb,1.06777e-05_rb,1.08465e-05_rb/)
      totplnk(151:181, 7) = (/ &
      1.10168e-05_rb,1.11887e-05_rb,1.13621e-05_rb,1.15372e-05_rb,1.17138e-05_rb, &
      1.18920e-05_rb,1.20718e-05_rb,1.22532e-05_rb,1.24362e-05_rb,1.26207e-05_rb, &
      1.28069e-05_rb,1.29946e-05_rb,1.31839e-05_rb,1.33749e-05_rb,1.35674e-05_rb, &
      1.37615e-05_rb,1.39572e-05_rb,1.41544e-05_rb,1.43533e-05_rb,1.45538e-05_rb, &
      1.47558e-05_rb,1.49595e-05_rb,1.51647e-05_rb,1.53716e-05_rb,1.55800e-05_rb, &
      1.57900e-05_rb,1.60017e-05_rb,1.62149e-05_rb,1.64296e-05_rb,1.66460e-05_rb, &
      1.68640e-05_rb/)
      totplnk(1:50, 8) = (/ &
      6.74445e-08_rb,7.18176e-08_rb,7.64153e-08_rb,8.12456e-08_rb,8.63170e-08_rb, &
      9.16378e-08_rb,9.72168e-08_rb,1.03063e-07_rb,1.09184e-07_rb,1.15591e-07_rb, &
      1.22292e-07_rb,1.29296e-07_rb,1.36613e-07_rb,1.44253e-07_rb,1.52226e-07_rb, &
      1.60540e-07_rb,1.69207e-07_rb,1.78236e-07_rb,1.87637e-07_rb,1.97421e-07_rb, &
      2.07599e-07_rb,2.18181e-07_rb,2.29177e-07_rb,2.40598e-07_rb,2.52456e-07_rb, &
      2.64761e-07_rb,2.77523e-07_rb,2.90755e-07_rb,3.04468e-07_rb,3.18673e-07_rb, &
      3.33381e-07_rb,3.48603e-07_rb,3.64352e-07_rb,3.80638e-07_rb,3.97474e-07_rb, &
      4.14871e-07_rb,4.32841e-07_rb,4.51395e-07_rb,4.70547e-07_rb,4.90306e-07_rb, &
      5.10687e-07_rb,5.31699e-07_rb,5.53357e-07_rb,5.75670e-07_rb,5.98652e-07_rb, &
      6.22315e-07_rb,6.46672e-07_rb,6.71731e-07_rb,6.97511e-07_rb,7.24018e-07_rb/)
      totplnk(51:100, 8) = (/ &
      7.51266e-07_rb,7.79269e-07_rb,8.08038e-07_rb,8.37584e-07_rb,8.67922e-07_rb, &
      8.99061e-07_rb,9.31016e-07_rb,9.63797e-07_rb,9.97417e-07_rb,1.03189e-06_rb, &
      1.06722e-06_rb,1.10343e-06_rb,1.14053e-06_rb,1.17853e-06_rb,1.21743e-06_rb, &
      1.25726e-06_rb,1.29803e-06_rb,1.33974e-06_rb,1.38241e-06_rb,1.42606e-06_rb, &
      1.47068e-06_rb,1.51630e-06_rb,1.56293e-06_rb,1.61056e-06_rb,1.65924e-06_rb, &
      1.70894e-06_rb,1.75971e-06_rb,1.81153e-06_rb,1.86443e-06_rb,1.91841e-06_rb, &
      1.97350e-06_rb,2.02968e-06_rb,2.08699e-06_rb,2.14543e-06_rb,2.20500e-06_rb, &
      2.26573e-06_rb,2.32762e-06_rb,2.39068e-06_rb,2.45492e-06_rb,2.52036e-06_rb, &
      2.58700e-06_rb,2.65485e-06_rb,2.72393e-06_rb,2.79424e-06_rb,2.86580e-06_rb, &
      2.93861e-06_rb,3.01269e-06_rb,3.08803e-06_rb,3.16467e-06_rb,3.24259e-06_rb/)
      totplnk(101:150, 8) = (/ &
      3.32181e-06_rb,3.40235e-06_rb,3.48420e-06_rb,3.56739e-06_rb,3.65192e-06_rb, &
      3.73779e-06_rb,3.82502e-06_rb,3.91362e-06_rb,4.00359e-06_rb,4.09494e-06_rb, &
      4.18768e-06_rb,4.28182e-06_rb,4.37737e-06_rb,4.47434e-06_rb,4.57273e-06_rb, &
      4.67254e-06_rb,4.77380e-06_rb,4.87651e-06_rb,4.98067e-06_rb,5.08630e-06_rb, &
      5.19339e-06_rb,5.30196e-06_rb,5.41201e-06_rb,5.52356e-06_rb,5.63660e-06_rb, &
      5.75116e-06_rb,5.86722e-06_rb,5.98479e-06_rb,6.10390e-06_rb,6.22453e-06_rb, &
      6.34669e-06_rb,6.47042e-06_rb,6.59569e-06_rb,6.72252e-06_rb,6.85090e-06_rb, &
      6.98085e-06_rb,7.11238e-06_rb,7.24549e-06_rb,7.38019e-06_rb,7.51646e-06_rb, &
      7.65434e-06_rb,7.79382e-06_rb,7.93490e-06_rb,8.07760e-06_rb,8.22192e-06_rb, &
      8.36784e-06_rb,8.51540e-06_rb,8.66459e-06_rb,8.81542e-06_rb,8.96786e-06_rb/)
      totplnk(151:181, 8) = (/ &
      9.12197e-06_rb,9.27772e-06_rb,9.43513e-06_rb,9.59419e-06_rb,9.75490e-06_rb, &
      9.91728e-06_rb,1.00813e-05_rb,1.02471e-05_rb,1.04144e-05_rb,1.05835e-05_rb, &
      1.07543e-05_rb,1.09267e-05_rb,1.11008e-05_rb,1.12766e-05_rb,1.14541e-05_rb, &
      1.16333e-05_rb,1.18142e-05_rb,1.19969e-05_rb,1.21812e-05_rb,1.23672e-05_rb, &
      1.25549e-05_rb,1.27443e-05_rb,1.29355e-05_rb,1.31284e-05_rb,1.33229e-05_rb, &
      1.35193e-05_rb,1.37173e-05_rb,1.39170e-05_rb,1.41185e-05_rb,1.43217e-05_rb, &
      1.45267e-05_rb/)
      totplnk(1:50, 9) = (/ &
      2.61522e-08_rb,2.80613e-08_rb,3.00838e-08_rb,3.22250e-08_rb,3.44899e-08_rb, &
      3.68841e-08_rb,3.94129e-08_rb,4.20820e-08_rb,4.48973e-08_rb,4.78646e-08_rb, &
      5.09901e-08_rb,5.42799e-08_rb,5.77405e-08_rb,6.13784e-08_rb,6.52001e-08_rb, &
      6.92126e-08_rb,7.34227e-08_rb,7.78375e-08_rb,8.24643e-08_rb,8.73103e-08_rb, &
      9.23832e-08_rb,9.76905e-08_rb,1.03240e-07_rb,1.09039e-07_rb,1.15097e-07_rb, &
      1.21421e-07_rb,1.28020e-07_rb,1.34902e-07_rb,1.42075e-07_rb,1.49548e-07_rb, &
      1.57331e-07_rb,1.65432e-07_rb,1.73860e-07_rb,1.82624e-07_rb,1.91734e-07_rb, &
      2.01198e-07_rb,2.11028e-07_rb,2.21231e-07_rb,2.31818e-07_rb,2.42799e-07_rb, &
      2.54184e-07_rb,2.65983e-07_rb,2.78205e-07_rb,2.90862e-07_rb,3.03963e-07_rb, &
      3.17519e-07_rb,3.31541e-07_rb,3.46039e-07_rb,3.61024e-07_rb,3.76507e-07_rb/)
      totplnk(51:100, 9) = (/ &
      3.92498e-07_rb,4.09008e-07_rb,4.26050e-07_rb,4.43633e-07_rb,4.61769e-07_rb, &
      4.80469e-07_rb,4.99744e-07_rb,5.19606e-07_rb,5.40067e-07_rb,5.61136e-07_rb, &
      5.82828e-07_rb,6.05152e-07_rb,6.28120e-07_rb,6.51745e-07_rb,6.76038e-07_rb, &
      7.01010e-07_rb,7.26674e-07_rb,7.53041e-07_rb,7.80124e-07_rb,8.07933e-07_rb, &
      8.36482e-07_rb,8.65781e-07_rb,8.95845e-07_rb,9.26683e-07_rb,9.58308e-07_rb, &
      9.90732e-07_rb,1.02397e-06_rb,1.05803e-06_rb,1.09292e-06_rb,1.12866e-06_rb, &
      1.16526e-06_rb,1.20274e-06_rb,1.24109e-06_rb,1.28034e-06_rb,1.32050e-06_rb, &
      1.36158e-06_rb,1.40359e-06_rb,1.44655e-06_rb,1.49046e-06_rb,1.53534e-06_rb, &
      1.58120e-06_rb,1.62805e-06_rb,1.67591e-06_rb,1.72478e-06_rb,1.77468e-06_rb, &
      1.82561e-06_rb,1.87760e-06_rb,1.93066e-06_rb,1.98479e-06_rb,2.04000e-06_rb/)
      totplnk(101:150, 9) = (/ &
      2.09631e-06_rb,2.15373e-06_rb,2.21228e-06_rb,2.27196e-06_rb,2.33278e-06_rb, &
      2.39475e-06_rb,2.45790e-06_rb,2.52222e-06_rb,2.58773e-06_rb,2.65445e-06_rb, &
      2.72238e-06_rb,2.79152e-06_rb,2.86191e-06_rb,2.93354e-06_rb,3.00643e-06_rb, &
      3.08058e-06_rb,3.15601e-06_rb,3.23273e-06_rb,3.31075e-06_rb,3.39009e-06_rb, &
      3.47074e-06_rb,3.55272e-06_rb,3.63605e-06_rb,3.72072e-06_rb,3.80676e-06_rb, &
      3.89417e-06_rb,3.98297e-06_rb,4.07315e-06_rb,4.16474e-06_rb,4.25774e-06_rb, &
      4.35217e-06_rb,4.44802e-06_rb,4.54532e-06_rb,4.64406e-06_rb,4.74428e-06_rb, &
      4.84595e-06_rb,4.94911e-06_rb,5.05376e-06_rb,5.15990e-06_rb,5.26755e-06_rb, &
      5.37671e-06_rb,5.48741e-06_rb,5.59963e-06_rb,5.71340e-06_rb,5.82871e-06_rb, &
      5.94559e-06_rb,6.06403e-06_rb,6.18404e-06_rb,6.30565e-06_rb,6.42885e-06_rb/)
      totplnk(151:181, 9) = (/ &
      6.55364e-06_rb,6.68004e-06_rb,6.80806e-06_rb,6.93771e-06_rb,7.06898e-06_rb, &
      7.20190e-06_rb,7.33646e-06_rb,7.47267e-06_rb,7.61056e-06_rb,7.75010e-06_rb, &
      7.89133e-06_rb,8.03423e-06_rb,8.17884e-06_rb,8.32514e-06_rb,8.47314e-06_rb, &
      8.62284e-06_rb,8.77427e-06_rb,8.92743e-06_rb,9.08231e-06_rb,9.23893e-06_rb, &
      9.39729e-06_rb,9.55741e-06_rb,9.71927e-06_rb,9.88291e-06_rb,1.00483e-05_rb, &
      1.02155e-05_rb,1.03844e-05_rb,1.05552e-05_rb,1.07277e-05_rb,1.09020e-05_rb, &
      1.10781e-05_rb/)
      totplnk(1:50,10) = (/ &
      8.89300e-09_rb,9.63263e-09_rb,1.04235e-08_rb,1.12685e-08_rb,1.21703e-08_rb, &
      1.31321e-08_rb,1.41570e-08_rb,1.52482e-08_rb,1.64090e-08_rb,1.76428e-08_rb, &
      1.89533e-08_rb,2.03441e-08_rb,2.18190e-08_rb,2.33820e-08_rb,2.50370e-08_rb, &
      2.67884e-08_rb,2.86402e-08_rb,3.05969e-08_rb,3.26632e-08_rb,3.48436e-08_rb, &
      3.71429e-08_rb,3.95660e-08_rb,4.21179e-08_rb,4.48040e-08_rb,4.76294e-08_rb, &
      5.05996e-08_rb,5.37201e-08_rb,5.69966e-08_rb,6.04349e-08_rb,6.40411e-08_rb, &
      6.78211e-08_rb,7.17812e-08_rb,7.59276e-08_rb,8.02670e-08_rb,8.48059e-08_rb, &
      8.95508e-08_rb,9.45090e-08_rb,9.96873e-08_rb,1.05093e-07_rb,1.10733e-07_rb, &
      1.16614e-07_rb,1.22745e-07_rb,1.29133e-07_rb,1.35786e-07_rb,1.42711e-07_rb, &
      1.49916e-07_rb,1.57410e-07_rb,1.65202e-07_rb,1.73298e-07_rb,1.81709e-07_rb/)
      totplnk(51:100,10) = (/ &
      1.90441e-07_rb,1.99505e-07_rb,2.08908e-07_rb,2.18660e-07_rb,2.28770e-07_rb, &
      2.39247e-07_rb,2.50101e-07_rb,2.61340e-07_rb,2.72974e-07_rb,2.85013e-07_rb, &
      2.97467e-07_rb,3.10345e-07_rb,3.23657e-07_rb,3.37413e-07_rb,3.51623e-07_rb, &
      3.66298e-07_rb,3.81448e-07_rb,3.97082e-07_rb,4.13212e-07_rb,4.29848e-07_rb, &
      4.47000e-07_rb,4.64680e-07_rb,4.82898e-07_rb,5.01664e-07_rb,5.20991e-07_rb, &
      5.40888e-07_rb,5.61369e-07_rb,5.82440e-07_rb,6.04118e-07_rb,6.26410e-07_rb, &
      6.49329e-07_rb,6.72887e-07_rb,6.97095e-07_rb,7.21964e-07_rb,7.47506e-07_rb, &
      7.73732e-07_rb,8.00655e-07_rb,8.28287e-07_rb,8.56635e-07_rb,8.85717e-07_rb, &
      9.15542e-07_rb,9.46122e-07_rb,9.77469e-07_rb,1.00960e-06_rb,1.04251e-06_rb, &
      1.07623e-06_rb,1.11077e-06_rb,1.14613e-06_rb,1.18233e-06_rb,1.21939e-06_rb/)
      totplnk(101:150,10) = (/ &
      1.25730e-06_rb,1.29610e-06_rb,1.33578e-06_rb,1.37636e-06_rb,1.41785e-06_rb, &
      1.46027e-06_rb,1.50362e-06_rb,1.54792e-06_rb,1.59319e-06_rb,1.63942e-06_rb, &
      1.68665e-06_rb,1.73487e-06_rb,1.78410e-06_rb,1.83435e-06_rb,1.88564e-06_rb, &
      1.93797e-06_rb,1.99136e-06_rb,2.04582e-06_rb,2.10137e-06_rb,2.15801e-06_rb, &
      2.21576e-06_rb,2.27463e-06_rb,2.33462e-06_rb,2.39577e-06_rb,2.45806e-06_rb, &
      2.52153e-06_rb,2.58617e-06_rb,2.65201e-06_rb,2.71905e-06_rb,2.78730e-06_rb, &
      2.85678e-06_rb,2.92749e-06_rb,2.99946e-06_rb,3.07269e-06_rb,3.14720e-06_rb, &
      3.22299e-06_rb,3.30007e-06_rb,3.37847e-06_rb,3.45818e-06_rb,3.53923e-06_rb, &
      3.62161e-06_rb,3.70535e-06_rb,3.79046e-06_rb,3.87695e-06_rb,3.96481e-06_rb, &
      4.05409e-06_rb,4.14477e-06_rb,4.23687e-06_rb,4.33040e-06_rb,4.42538e-06_rb/)
      totplnk(151:181,10) = (/ &
      4.52180e-06_rb,4.61969e-06_rb,4.71905e-06_rb,4.81991e-06_rb,4.92226e-06_rb, &
      5.02611e-06_rb,5.13148e-06_rb,5.23839e-06_rb,5.34681e-06_rb,5.45681e-06_rb, &
      5.56835e-06_rb,5.68146e-06_rb,5.79614e-06_rb,5.91242e-06_rb,6.03030e-06_rb, &
      6.14978e-06_rb,6.27088e-06_rb,6.39360e-06_rb,6.51798e-06_rb,6.64398e-06_rb, &
      6.77165e-06_rb,6.90099e-06_rb,7.03198e-06_rb,7.16468e-06_rb,7.29906e-06_rb, &
      7.43514e-06_rb,7.57294e-06_rb,7.71244e-06_rb,7.85369e-06_rb,7.99666e-06_rb, &
      8.14138e-06_rb/)
      totplnk(1:50,11) = (/ &
      2.53767e-09_rb,2.77242e-09_rb,3.02564e-09_rb,3.29851e-09_rb,3.59228e-09_rb, &
      3.90825e-09_rb,4.24777e-09_rb,4.61227e-09_rb,5.00322e-09_rb,5.42219e-09_rb, &
      5.87080e-09_rb,6.35072e-09_rb,6.86370e-09_rb,7.41159e-09_rb,7.99628e-09_rb, &
      8.61974e-09_rb,9.28404e-09_rb,9.99130e-09_rb,1.07437e-08_rb,1.15436e-08_rb, &
      1.23933e-08_rb,1.32953e-08_rb,1.42522e-08_rb,1.52665e-08_rb,1.63410e-08_rb, &
      1.74786e-08_rb,1.86820e-08_rb,1.99542e-08_rb,2.12985e-08_rb,2.27179e-08_rb, &
      2.42158e-08_rb,2.57954e-08_rb,2.74604e-08_rb,2.92141e-08_rb,3.10604e-08_rb, &
      3.30029e-08_rb,3.50457e-08_rb,3.71925e-08_rb,3.94476e-08_rb,4.18149e-08_rb, &
      4.42991e-08_rb,4.69043e-08_rb,4.96352e-08_rb,5.24961e-08_rb,5.54921e-08_rb, &
      5.86277e-08_rb,6.19081e-08_rb,6.53381e-08_rb,6.89231e-08_rb,7.26681e-08_rb/)
      totplnk(51:100,11) = (/ &
      7.65788e-08_rb,8.06604e-08_rb,8.49187e-08_rb,8.93591e-08_rb,9.39879e-08_rb, &
      9.88106e-08_rb,1.03834e-07_rb,1.09063e-07_rb,1.14504e-07_rb,1.20165e-07_rb, &
      1.26051e-07_rb,1.32169e-07_rb,1.38525e-07_rb,1.45128e-07_rb,1.51982e-07_rb, &
      1.59096e-07_rb,1.66477e-07_rb,1.74132e-07_rb,1.82068e-07_rb,1.90292e-07_rb, &
      1.98813e-07_rb,2.07638e-07_rb,2.16775e-07_rb,2.26231e-07_rb,2.36015e-07_rb, &
      2.46135e-07_rb,2.56599e-07_rb,2.67415e-07_rb,2.78592e-07_rb,2.90137e-07_rb, &
      3.02061e-07_rb,3.14371e-07_rb,3.27077e-07_rb,3.40186e-07_rb,3.53710e-07_rb, &
      3.67655e-07_rb,3.82031e-07_rb,3.96848e-07_rb,4.12116e-07_rb,4.27842e-07_rb, &
      4.44039e-07_rb,4.60713e-07_rb,4.77876e-07_rb,4.95537e-07_rb,5.13706e-07_rb, &
      5.32392e-07_rb,5.51608e-07_rb,5.71360e-07_rb,5.91662e-07_rb,6.12521e-07_rb/)
      totplnk(101:150,11) = (/ &
      6.33950e-07_rb,6.55958e-07_rb,6.78556e-07_rb,7.01753e-07_rb,7.25562e-07_rb, &
      7.49992e-07_rb,7.75055e-07_rb,8.00760e-07_rb,8.27120e-07_rb,8.54145e-07_rb, &
      8.81845e-07_rb,9.10233e-07_rb,9.39318e-07_rb,9.69113e-07_rb,9.99627e-07_rb, &
      1.03087e-06_rb,1.06286e-06_rb,1.09561e-06_rb,1.12912e-06_rb,1.16340e-06_rb, &
      1.19848e-06_rb,1.23435e-06_rb,1.27104e-06_rb,1.30855e-06_rb,1.34690e-06_rb, &
      1.38609e-06_rb,1.42614e-06_rb,1.46706e-06_rb,1.50886e-06_rb,1.55155e-06_rb, &
      1.59515e-06_rb,1.63967e-06_rb,1.68512e-06_rb,1.73150e-06_rb,1.77884e-06_rb, &
      1.82715e-06_rb,1.87643e-06_rb,1.92670e-06_rb,1.97797e-06_rb,2.03026e-06_rb, &
      2.08356e-06_rb,2.13791e-06_rb,2.19330e-06_rb,2.24975e-06_rb,2.30728e-06_rb, &
      2.36589e-06_rb,2.42560e-06_rb,2.48641e-06_rb,2.54835e-06_rb,2.61142e-06_rb/)
      totplnk(151:181,11) = (/ &
      2.67563e-06_rb,2.74100e-06_rb,2.80754e-06_rb,2.87526e-06_rb,2.94417e-06_rb, &
      3.01429e-06_rb,3.08562e-06_rb,3.15819e-06_rb,3.23199e-06_rb,3.30704e-06_rb, &
      3.38336e-06_rb,3.46096e-06_rb,3.53984e-06_rb,3.62002e-06_rb,3.70151e-06_rb, &
      3.78433e-06_rb,3.86848e-06_rb,3.95399e-06_rb,4.04084e-06_rb,4.12907e-06_rb, &
      4.21868e-06_rb,4.30968e-06_rb,4.40209e-06_rb,4.49592e-06_rb,4.59117e-06_rb, &
      4.68786e-06_rb,4.78600e-06_rb,4.88561e-06_rb,4.98669e-06_rb,5.08926e-06_rb, &
      5.19332e-06_rb/)
      totplnk(1:50,12) = (/ &
      2.73921e-10_rb,3.04500e-10_rb,3.38056e-10_rb,3.74835e-10_rb,4.15099e-10_rb, &
      4.59126e-10_rb,5.07214e-10_rb,5.59679e-10_rb,6.16857e-10_rb,6.79103e-10_rb, &
      7.46796e-10_rb,8.20335e-10_rb,9.00144e-10_rb,9.86671e-10_rb,1.08039e-09_rb, &
      1.18180e-09_rb,1.29142e-09_rb,1.40982e-09_rb,1.53757e-09_rb,1.67529e-09_rb, &
      1.82363e-09_rb,1.98327e-09_rb,2.15492e-09_rb,2.33932e-09_rb,2.53726e-09_rb, &
      2.74957e-09_rb,2.97710e-09_rb,3.22075e-09_rb,3.48145e-09_rb,3.76020e-09_rb, &
      4.05801e-09_rb,4.37595e-09_rb,4.71513e-09_rb,5.07672e-09_rb,5.46193e-09_rb, &
      5.87201e-09_rb,6.30827e-09_rb,6.77205e-09_rb,7.26480e-09_rb,7.78794e-09_rb, &
      8.34304e-09_rb,8.93163e-09_rb,9.55537e-09_rb,1.02159e-08_rb,1.09151e-08_rb, &
      1.16547e-08_rb,1.24365e-08_rb,1.32625e-08_rb,1.41348e-08_rb,1.50554e-08_rb/)
      totplnk(51:100,12) = (/ &
      1.60264e-08_rb,1.70500e-08_rb,1.81285e-08_rb,1.92642e-08_rb,2.04596e-08_rb, &
      2.17171e-08_rb,2.30394e-08_rb,2.44289e-08_rb,2.58885e-08_rb,2.74209e-08_rb, &
      2.90290e-08_rb,3.07157e-08_rb,3.24841e-08_rb,3.43371e-08_rb,3.62782e-08_rb, &
      3.83103e-08_rb,4.04371e-08_rb,4.26617e-08_rb,4.49878e-08_rb,4.74190e-08_rb, &
      4.99589e-08_rb,5.26113e-08_rb,5.53801e-08_rb,5.82692e-08_rb,6.12826e-08_rb, &
      6.44245e-08_rb,6.76991e-08_rb,7.11105e-08_rb,7.46634e-08_rb,7.83621e-08_rb, &
      8.22112e-08_rb,8.62154e-08_rb,9.03795e-08_rb,9.47081e-08_rb,9.92066e-08_rb, &
      1.03879e-07_rb,1.08732e-07_rb,1.13770e-07_rb,1.18998e-07_rb,1.24422e-07_rb, &
      1.30048e-07_rb,1.35880e-07_rb,1.41924e-07_rb,1.48187e-07_rb,1.54675e-07_rb, &
      1.61392e-07_rb,1.68346e-07_rb,1.75543e-07_rb,1.82988e-07_rb,1.90688e-07_rb/)
      totplnk(101:150,12) = (/ &
      1.98650e-07_rb,2.06880e-07_rb,2.15385e-07_rb,2.24172e-07_rb,2.33247e-07_rb, &
      2.42617e-07_rb,2.52289e-07_rb,2.62272e-07_rb,2.72571e-07_rb,2.83193e-07_rb, &
      2.94147e-07_rb,3.05440e-07_rb,3.17080e-07_rb,3.29074e-07_rb,3.41430e-07_rb, &
      3.54155e-07_rb,3.67259e-07_rb,3.80747e-07_rb,3.94631e-07_rb,4.08916e-07_rb, &
      4.23611e-07_rb,4.38725e-07_rb,4.54267e-07_rb,4.70245e-07_rb,4.86666e-07_rb, &
      5.03541e-07_rb,5.20879e-07_rb,5.38687e-07_rb,5.56975e-07_rb,5.75751e-07_rb, &
      5.95026e-07_rb,6.14808e-07_rb,6.35107e-07_rb,6.55932e-07_rb,6.77293e-07_rb, &
      6.99197e-07_rb,7.21656e-07_rb,7.44681e-07_rb,7.68278e-07_rb,7.92460e-07_rb, &
      8.17235e-07_rb,8.42614e-07_rb,8.68606e-07_rb,8.95223e-07_rb,9.22473e-07_rb, &
      9.50366e-07_rb,9.78915e-07_rb,1.00813e-06_rb,1.03802e-06_rb,1.06859e-06_rb/)
      totplnk(151:181,12) = (/ &
      1.09986e-06_rb,1.13184e-06_rb,1.16453e-06_rb,1.19796e-06_rb,1.23212e-06_rb, &
      1.26703e-06_rb,1.30270e-06_rb,1.33915e-06_rb,1.37637e-06_rb,1.41440e-06_rb, &
      1.45322e-06_rb,1.49286e-06_rb,1.53333e-06_rb,1.57464e-06_rb,1.61679e-06_rb, &
      1.65981e-06_rb,1.70370e-06_rb,1.74847e-06_rb,1.79414e-06_rb,1.84071e-06_rb, &
      1.88821e-06_rb,1.93663e-06_rb,1.98599e-06_rb,2.03631e-06_rb,2.08759e-06_rb, &
      2.13985e-06_rb,2.19310e-06_rb,2.24734e-06_rb,2.30260e-06_rb,2.35888e-06_rb, &
      2.41619e-06_rb/)
      totplnk(1:50,13) = (/ &
      4.53634e-11_rb,5.11435e-11_rb,5.75754e-11_rb,6.47222e-11_rb,7.26531e-11_rb, &
      8.14420e-11_rb,9.11690e-11_rb,1.01921e-10_rb,1.13790e-10_rb,1.26877e-10_rb, &
      1.41288e-10_rb,1.57140e-10_rb,1.74555e-10_rb,1.93665e-10_rb,2.14613e-10_rb, &
      2.37548e-10_rb,2.62633e-10_rb,2.90039e-10_rb,3.19948e-10_rb,3.52558e-10_rb, &
      3.88073e-10_rb,4.26716e-10_rb,4.68719e-10_rb,5.14331e-10_rb,5.63815e-10_rb, &
      6.17448e-10_rb,6.75526e-10_rb,7.38358e-10_rb,8.06277e-10_rb,8.79625e-10_rb, &
      9.58770e-10_rb,1.04410e-09_rb,1.13602e-09_rb,1.23495e-09_rb,1.34135e-09_rb, &
      1.45568e-09_rb,1.57845e-09_rb,1.71017e-09_rb,1.85139e-09_rb,2.00268e-09_rb, &
      2.16464e-09_rb,2.33789e-09_rb,2.52309e-09_rb,2.72093e-09_rb,2.93212e-09_rb, &
      3.15740e-09_rb,3.39757e-09_rb,3.65341e-09_rb,3.92579e-09_rb,4.21559e-09_rb/)
      totplnk(51:100,13) = (/ &
      4.52372e-09_rb,4.85115e-09_rb,5.19886e-09_rb,5.56788e-09_rb,5.95928e-09_rb, &
      6.37419e-09_rb,6.81375e-09_rb,7.27917e-09_rb,7.77168e-09_rb,8.29256e-09_rb, &
      8.84317e-09_rb,9.42487e-09_rb,1.00391e-08_rb,1.06873e-08_rb,1.13710e-08_rb, &
      1.20919e-08_rb,1.28515e-08_rb,1.36514e-08_rb,1.44935e-08_rb,1.53796e-08_rb, &
      1.63114e-08_rb,1.72909e-08_rb,1.83201e-08_rb,1.94008e-08_rb,2.05354e-08_rb, &
      2.17258e-08_rb,2.29742e-08_rb,2.42830e-08_rb,2.56545e-08_rb,2.70910e-08_rb, &
      2.85950e-08_rb,3.01689e-08_rb,3.18155e-08_rb,3.35373e-08_rb,3.53372e-08_rb, &
      3.72177e-08_rb,3.91818e-08_rb,4.12325e-08_rb,4.33727e-08_rb,4.56056e-08_rb, &
      4.79342e-08_rb,5.03617e-08_rb,5.28915e-08_rb,5.55270e-08_rb,5.82715e-08_rb, &
      6.11286e-08_rb,6.41019e-08_rb,6.71951e-08_rb,7.04119e-08_rb,7.37560e-08_rb/)
      totplnk(101:150,13) = (/ &
      7.72315e-08_rb,8.08424e-08_rb,8.45927e-08_rb,8.84866e-08_rb,9.25281e-08_rb, &
      9.67218e-08_rb,1.01072e-07_rb,1.05583e-07_rb,1.10260e-07_rb,1.15107e-07_rb, &
      1.20128e-07_rb,1.25330e-07_rb,1.30716e-07_rb,1.36291e-07_rb,1.42061e-07_rb, &
      1.48031e-07_rb,1.54206e-07_rb,1.60592e-07_rb,1.67192e-07_rb,1.74015e-07_rb, &
      1.81064e-07_rb,1.88345e-07_rb,1.95865e-07_rb,2.03628e-07_rb,2.11643e-07_rb, &
      2.19912e-07_rb,2.28443e-07_rb,2.37244e-07_rb,2.46318e-07_rb,2.55673e-07_rb, &
      2.65316e-07_rb,2.75252e-07_rb,2.85489e-07_rb,2.96033e-07_rb,3.06891e-07_rb, &
      3.18070e-07_rb,3.29576e-07_rb,3.41417e-07_rb,3.53600e-07_rb,3.66133e-07_rb, &
      3.79021e-07_rb,3.92274e-07_rb,4.05897e-07_rb,4.19899e-07_rb,4.34288e-07_rb, &
      4.49071e-07_rb,4.64255e-07_rb,4.79850e-07_rb,4.95863e-07_rb,5.12300e-07_rb/)
      totplnk(151:181,13) = (/ &
      5.29172e-07_rb,5.46486e-07_rb,5.64250e-07_rb,5.82473e-07_rb,6.01164e-07_rb, &
      6.20329e-07_rb,6.39979e-07_rb,6.60122e-07_rb,6.80767e-07_rb,7.01922e-07_rb, &
      7.23596e-07_rb,7.45800e-07_rb,7.68539e-07_rb,7.91826e-07_rb,8.15669e-07_rb, &
      8.40076e-07_rb,8.65058e-07_rb,8.90623e-07_rb,9.16783e-07_rb,9.43544e-07_rb, &
      9.70917e-07_rb,9.98912e-07_rb,1.02754e-06_rb,1.05681e-06_rb,1.08673e-06_rb, &
      1.11731e-06_rb,1.14856e-06_rb,1.18050e-06_rb,1.21312e-06_rb,1.24645e-06_rb, &
      1.28049e-06_rb/)
      totplnk(1:50,14) = (/ &
      1.40113e-11_rb,1.59358e-11_rb,1.80960e-11_rb,2.05171e-11_rb,2.32266e-11_rb, &
      2.62546e-11_rb,2.96335e-11_rb,3.33990e-11_rb,3.75896e-11_rb,4.22469e-11_rb, &
      4.74164e-11_rb,5.31466e-11_rb,5.94905e-11_rb,6.65054e-11_rb,7.42522e-11_rb, &
      8.27975e-11_rb,9.22122e-11_rb,1.02573e-10_rb,1.13961e-10_rb,1.26466e-10_rb, &
      1.40181e-10_rb,1.55206e-10_rb,1.71651e-10_rb,1.89630e-10_rb,2.09265e-10_rb, &
      2.30689e-10_rb,2.54040e-10_rb,2.79467e-10_rb,3.07128e-10_rb,3.37190e-10_rb, &
      3.69833e-10_rb,4.05243e-10_rb,4.43623e-10_rb,4.85183e-10_rb,5.30149e-10_rb, &
      5.78755e-10_rb,6.31255e-10_rb,6.87910e-10_rb,7.49002e-10_rb,8.14824e-10_rb, &
      8.85687e-10_rb,9.61914e-10_rb,1.04385e-09_rb,1.13186e-09_rb,1.22631e-09_rb, &
      1.32761e-09_rb,1.43617e-09_rb,1.55243e-09_rb,1.67686e-09_rb,1.80992e-09_rb/)
      totplnk(51:100,14) = (/ &
      1.95212e-09_rb,2.10399e-09_rb,2.26607e-09_rb,2.43895e-09_rb,2.62321e-09_rb, &
      2.81949e-09_rb,3.02844e-09_rb,3.25073e-09_rb,3.48707e-09_rb,3.73820e-09_rb, &
      4.00490e-09_rb,4.28794e-09_rb,4.58819e-09_rb,4.90647e-09_rb,5.24371e-09_rb, &
      5.60081e-09_rb,5.97875e-09_rb,6.37854e-09_rb,6.80120e-09_rb,7.24782e-09_rb, &
      7.71950e-09_rb,8.21740e-09_rb,8.74271e-09_rb,9.29666e-09_rb,9.88054e-09_rb, &
      1.04956e-08_rb,1.11434e-08_rb,1.18251e-08_rb,1.25422e-08_rb,1.32964e-08_rb, &
      1.40890e-08_rb,1.49217e-08_rb,1.57961e-08_rb,1.67140e-08_rb,1.76771e-08_rb, &
      1.86870e-08_rb,1.97458e-08_rb,2.08553e-08_rb,2.20175e-08_rb,2.32342e-08_rb, &
      2.45077e-08_rb,2.58401e-08_rb,2.72334e-08_rb,2.86900e-08_rb,3.02122e-08_rb, &
      3.18021e-08_rb,3.34624e-08_rb,3.51954e-08_rb,3.70037e-08_rb,3.88899e-08_rb/)
      totplnk(101:150,14) = (/ &
      4.08568e-08_rb,4.29068e-08_rb,4.50429e-08_rb,4.72678e-08_rb,4.95847e-08_rb, &
      5.19963e-08_rb,5.45058e-08_rb,5.71161e-08_rb,5.98309e-08_rb,6.26529e-08_rb, &
      6.55857e-08_rb,6.86327e-08_rb,7.17971e-08_rb,7.50829e-08_rb,7.84933e-08_rb, &
      8.20323e-08_rb,8.57035e-08_rb,8.95105e-08_rb,9.34579e-08_rb,9.75488e-08_rb, &
      1.01788e-07_rb,1.06179e-07_rb,1.10727e-07_rb,1.15434e-07_rb,1.20307e-07_rb, &
      1.25350e-07_rb,1.30566e-07_rb,1.35961e-07_rb,1.41539e-07_rb,1.47304e-07_rb, &
      1.53263e-07_rb,1.59419e-07_rb,1.65778e-07_rb,1.72345e-07_rb,1.79124e-07_rb, &
      1.86122e-07_rb,1.93343e-07_rb,2.00792e-07_rb,2.08476e-07_rb,2.16400e-07_rb, &
      2.24568e-07_rb,2.32988e-07_rb,2.41666e-07_rb,2.50605e-07_rb,2.59813e-07_rb, &
      2.69297e-07_rb,2.79060e-07_rb,2.89111e-07_rb,2.99455e-07_rb,3.10099e-07_rb/)
      totplnk(151:181,14) = (/ &
      3.21049e-07_rb,3.32311e-07_rb,3.43893e-07_rb,3.55801e-07_rb,3.68041e-07_rb, &
      3.80621e-07_rb,3.93547e-07_rb,4.06826e-07_rb,4.20465e-07_rb,4.34473e-07_rb, &
      4.48856e-07_rb,4.63620e-07_rb,4.78774e-07_rb,4.94325e-07_rb,5.10280e-07_rb, &
      5.26648e-07_rb,5.43436e-07_rb,5.60652e-07_rb,5.78302e-07_rb,5.96397e-07_rb, &
      6.14943e-07_rb,6.33949e-07_rb,6.53421e-07_rb,6.73370e-07_rb,6.93803e-07_rb, &
      7.14731e-07_rb,7.36157e-07_rb,7.58095e-07_rb,7.80549e-07_rb,8.03533e-07_rb, &
      8.27050e-07_rb/)
      totplnk(1:50,15) = (/ &
      3.90483e-12_rb,4.47999e-12_rb,5.13122e-12_rb,5.86739e-12_rb,6.69829e-12_rb, &
      7.63467e-12_rb,8.68833e-12_rb,9.87221e-12_rb,1.12005e-11_rb,1.26885e-11_rb, &
      1.43534e-11_rb,1.62134e-11_rb,1.82888e-11_rb,2.06012e-11_rb,2.31745e-11_rb, &
      2.60343e-11_rb,2.92087e-11_rb,3.27277e-11_rb,3.66242e-11_rb,4.09334e-11_rb, &
      4.56935e-11_rb,5.09455e-11_rb,5.67338e-11_rb,6.31057e-11_rb,7.01127e-11_rb, &
      7.78096e-11_rb,8.62554e-11_rb,9.55130e-11_rb,1.05651e-10_rb,1.16740e-10_rb, &
      1.28858e-10_rb,1.42089e-10_rb,1.56519e-10_rb,1.72243e-10_rb,1.89361e-10_rb, &
      2.07978e-10_rb,2.28209e-10_rb,2.50173e-10_rb,2.73999e-10_rb,2.99820e-10_rb, &
      3.27782e-10_rb,3.58034e-10_rb,3.90739e-10_rb,4.26067e-10_rb,4.64196e-10_rb, &
      5.05317e-10_rb,5.49631e-10_rb,5.97347e-10_rb,6.48689e-10_rb,7.03891e-10_rb/)
      totplnk(51:100,15) = (/ &
      7.63201e-10_rb,8.26876e-10_rb,8.95192e-10_rb,9.68430e-10_rb,1.04690e-09_rb, &
      1.13091e-09_rb,1.22079e-09_rb,1.31689e-09_rb,1.41957e-09_rb,1.52922e-09_rb, &
      1.64623e-09_rb,1.77101e-09_rb,1.90401e-09_rb,2.04567e-09_rb,2.19647e-09_rb, &
      2.35690e-09_rb,2.52749e-09_rb,2.70875e-09_rb,2.90127e-09_rb,3.10560e-09_rb, &
      3.32238e-09_rb,3.55222e-09_rb,3.79578e-09_rb,4.05375e-09_rb,4.32682e-09_rb, &
      4.61574e-09_rb,4.92128e-09_rb,5.24420e-09_rb,5.58536e-09_rb,5.94558e-09_rb, &
      6.32575e-09_rb,6.72678e-09_rb,7.14964e-09_rb,7.59526e-09_rb,8.06470e-09_rb, &
      8.55897e-09_rb,9.07916e-09_rb,9.62638e-09_rb,1.02018e-08_rb,1.08066e-08_rb, &
      1.14420e-08_rb,1.21092e-08_rb,1.28097e-08_rb,1.35446e-08_rb,1.43155e-08_rb, &
      1.51237e-08_rb,1.59708e-08_rb,1.68581e-08_rb,1.77873e-08_rb,1.87599e-08_rb/)
      totplnk(101:150,15) = (/ &
      1.97777e-08_rb,2.08423e-08_rb,2.19555e-08_rb,2.31190e-08_rb,2.43348e-08_rb, &
      2.56045e-08_rb,2.69302e-08_rb,2.83140e-08_rb,2.97578e-08_rb,3.12636e-08_rb, &
      3.28337e-08_rb,3.44702e-08_rb,3.61755e-08_rb,3.79516e-08_rb,3.98012e-08_rb, &
      4.17265e-08_rb,4.37300e-08_rb,4.58143e-08_rb,4.79819e-08_rb,5.02355e-08_rb, &
      5.25777e-08_rb,5.50114e-08_rb,5.75393e-08_rb,6.01644e-08_rb,6.28896e-08_rb, &
      6.57177e-08_rb,6.86521e-08_rb,7.16959e-08_rb,7.48520e-08_rb,7.81239e-08_rb, &
      8.15148e-08_rb,8.50282e-08_rb,8.86675e-08_rb,9.24362e-08_rb,9.63380e-08_rb, &
      1.00376e-07_rb,1.04555e-07_rb,1.08878e-07_rb,1.13349e-07_rb,1.17972e-07_rb, &
      1.22751e-07_rb,1.27690e-07_rb,1.32793e-07_rb,1.38064e-07_rb,1.43508e-07_rb, &
      1.49129e-07_rb,1.54931e-07_rb,1.60920e-07_rb,1.67099e-07_rb,1.73473e-07_rb/)
      totplnk(151:181,15) = (/ &
      1.80046e-07_rb,1.86825e-07_rb,1.93812e-07_rb,2.01014e-07_rb,2.08436e-07_rb, &
      2.16082e-07_rb,2.23957e-07_rb,2.32067e-07_rb,2.40418e-07_rb,2.49013e-07_rb, &
      2.57860e-07_rb,2.66963e-07_rb,2.76328e-07_rb,2.85961e-07_rb,2.95868e-07_rb, &
      3.06053e-07_rb,3.16524e-07_rb,3.27286e-07_rb,3.38345e-07_rb,3.49707e-07_rb, &
      3.61379e-07_rb,3.73367e-07_rb,3.85676e-07_rb,3.98315e-07_rb,4.11287e-07_rb, &
      4.24602e-07_rb,4.38265e-07_rb,4.52283e-07_rb,4.66662e-07_rb,4.81410e-07_rb, &
      4.96535e-07_rb/)
      totplnk(1:50,16) = (/ &
      0.28639e-12_rb,0.33349e-12_rb,0.38764e-12_rb,0.44977e-12_rb,0.52093e-12_rb, &
      0.60231e-12_rb,0.69522e-12_rb,0.80111e-12_rb,0.92163e-12_rb,0.10586e-11_rb, &
      0.12139e-11_rb,0.13899e-11_rb,0.15890e-11_rb,0.18138e-11_rb,0.20674e-11_rb, &
      0.23531e-11_rb,0.26744e-11_rb,0.30352e-11_rb,0.34401e-11_rb,0.38936e-11_rb, &
      0.44011e-11_rb,0.49681e-11_rb,0.56010e-11_rb,0.63065e-11_rb,0.70919e-11_rb, &
      0.79654e-11_rb,0.89357e-11_rb,0.10012e-10_rb,0.11205e-10_rb,0.12526e-10_rb, &
      0.13986e-10_rb,0.15600e-10_rb,0.17380e-10_rb,0.19342e-10_rb,0.21503e-10_rb, &
      0.23881e-10_rb,0.26494e-10_rb,0.29362e-10_rb,0.32509e-10_rb,0.35958e-10_rb, &
      0.39733e-10_rb,0.43863e-10_rb,0.48376e-10_rb,0.53303e-10_rb,0.58679e-10_rb, &
      0.64539e-10_rb,0.70920e-10_rb,0.77864e-10_rb,0.85413e-10_rb,0.93615e-10_rb/)
      totplnk(51:100,16) = (/ &
      0.10252e-09_rb,0.11217e-09_rb,0.12264e-09_rb,0.13397e-09_rb,0.14624e-09_rb, &
      0.15950e-09_rb,0.17383e-09_rb,0.18930e-09_rb,0.20599e-09_rb,0.22399e-09_rb, &
      0.24339e-09_rb,0.26427e-09_rb,0.28674e-09_rb,0.31090e-09_rb,0.33686e-09_rb, &
      0.36474e-09_rb,0.39466e-09_rb,0.42676e-09_rb,0.46115e-09_rb,0.49800e-09_rb, &
      0.53744e-09_rb,0.57964e-09_rb,0.62476e-09_rb,0.67298e-09_rb,0.72448e-09_rb, &
      0.77945e-09_rb,0.83809e-09_rb,0.90062e-09_rb,0.96725e-09_rb,0.10382e-08_rb, &
      0.11138e-08_rb,0.11941e-08_rb,0.12796e-08_rb,0.13704e-08_rb,0.14669e-08_rb, &
      0.15694e-08_rb,0.16781e-08_rb,0.17934e-08_rb,0.19157e-08_rb,0.20453e-08_rb, &
      0.21825e-08_rb,0.23278e-08_rb,0.24815e-08_rb,0.26442e-08_rb,0.28161e-08_rb, &
      0.29978e-08_rb,0.31898e-08_rb,0.33925e-08_rb,0.36064e-08_rb,0.38321e-08_rb/)
      totplnk(101:150,16) = (/ &
      0.40700e-08_rb,0.43209e-08_rb,0.45852e-08_rb,0.48636e-08_rb,0.51567e-08_rb, &
      0.54652e-08_rb,0.57897e-08_rb,0.61310e-08_rb,0.64897e-08_rb,0.68667e-08_rb, &
      0.72626e-08_rb,0.76784e-08_rb,0.81148e-08_rb,0.85727e-08_rb,0.90530e-08_rb, &
      0.95566e-08_rb,0.10084e-07_rb,0.10638e-07_rb,0.11217e-07_rb,0.11824e-07_rb, &
      0.12458e-07_rb,0.13123e-07_rb,0.13818e-07_rb,0.14545e-07_rb,0.15305e-07_rb, &
      0.16099e-07_rb,0.16928e-07_rb,0.17795e-07_rb,0.18699e-07_rb,0.19643e-07_rb, &
      0.20629e-07_rb,0.21656e-07_rb,0.22728e-07_rb,0.23845e-07_rb,0.25010e-07_rb, &
      0.26223e-07_rb,0.27487e-07_rb,0.28804e-07_rb,0.30174e-07_rb,0.31600e-07_rb, &
      0.33084e-07_rb,0.34628e-07_rb,0.36233e-07_rb,0.37902e-07_rb,0.39637e-07_rb, &
      0.41440e-07_rb,0.43313e-07_rb,0.45259e-07_rb,0.47279e-07_rb,0.49376e-07_rb/)
      totplnk(151:181,16) = (/ &
      0.51552e-07_rb,0.53810e-07_rb,0.56153e-07_rb,0.58583e-07_rb,0.61102e-07_rb, &
      0.63713e-07_rb,0.66420e-07_rb,0.69224e-07_rb,0.72129e-07_rb,0.75138e-07_rb, &
      0.78254e-07_rb,0.81479e-07_rb,0.84818e-07_rb,0.88272e-07_rb,0.91846e-07_rb, &
      0.95543e-07_rb,0.99366e-07_rb,0.10332e-06_rb,0.10740e-06_rb,0.11163e-06_rb, &
      0.11599e-06_rb,0.12050e-06_rb,0.12515e-06_rb,0.12996e-06_rb,0.13493e-06_rb, &
      0.14005e-06_rb,0.14534e-06_rb,0.15080e-06_rb,0.15643e-06_rb,0.16224e-06_rb, &
      0.16823e-06_rb/)
      totplk16(1:50) = (/ &
      0.28481e-12_rb,0.33159e-12_rb,0.38535e-12_rb,0.44701e-12_rb,0.51763e-12_rb, &
      0.59836e-12_rb,0.69049e-12_rb,0.79549e-12_rb,0.91493e-12_rb,0.10506e-11_rb, &
      0.12045e-11_rb,0.13788e-11_rb,0.15758e-11_rb,0.17984e-11_rb,0.20493e-11_rb, &
      0.23317e-11_rb,0.26494e-11_rb,0.30060e-11_rb,0.34060e-11_rb,0.38539e-11_rb, &
      0.43548e-11_rb,0.49144e-11_rb,0.55387e-11_rb,0.62344e-11_rb,0.70086e-11_rb, &
      0.78692e-11_rb,0.88248e-11_rb,0.98846e-11_rb,0.11059e-10_rb,0.12358e-10_rb, &
      0.13794e-10_rb,0.15379e-10_rb,0.17128e-10_rb,0.19055e-10_rb,0.21176e-10_rb, &
      0.23508e-10_rb,0.26070e-10_rb,0.28881e-10_rb,0.31963e-10_rb,0.35339e-10_rb, &
      0.39034e-10_rb,0.43073e-10_rb,0.47484e-10_rb,0.52299e-10_rb,0.57548e-10_rb, &
      0.63267e-10_rb,0.69491e-10_rb,0.76261e-10_rb,0.83616e-10_rb,0.91603e-10_rb/)
      totplk16(51:100) = (/ &
      0.10027e-09_rb,0.10966e-09_rb,0.11983e-09_rb,0.13084e-09_rb,0.14275e-09_rb, &
      0.15562e-09_rb,0.16951e-09_rb,0.18451e-09_rb,0.20068e-09_rb,0.21810e-09_rb, &
      0.23686e-09_rb,0.25704e-09_rb,0.27875e-09_rb,0.30207e-09_rb,0.32712e-09_rb, &
      0.35400e-09_rb,0.38282e-09_rb,0.41372e-09_rb,0.44681e-09_rb,0.48223e-09_rb, &
      0.52013e-09_rb,0.56064e-09_rb,0.60392e-09_rb,0.65015e-09_rb,0.69948e-09_rb, &
      0.75209e-09_rb,0.80818e-09_rb,0.86794e-09_rb,0.93157e-09_rb,0.99929e-09_rb, &
      0.10713e-08_rb,0.11479e-08_rb,0.12293e-08_rb,0.13157e-08_rb,0.14074e-08_rb, &
      0.15047e-08_rb,0.16079e-08_rb,0.17172e-08_rb,0.18330e-08_rb,0.19557e-08_rb, &
      0.20855e-08_rb,0.22228e-08_rb,0.23680e-08_rb,0.25214e-08_rb,0.26835e-08_rb, &
      0.28546e-08_rb,0.30352e-08_rb,0.32257e-08_rb,0.34266e-08_rb,0.36384e-08_rb/)
      totplk16(101:150) = (/ &
      0.38615e-08_rb,0.40965e-08_rb,0.43438e-08_rb,0.46041e-08_rb,0.48779e-08_rb, &
      0.51658e-08_rb,0.54683e-08_rb,0.57862e-08_rb,0.61200e-08_rb,0.64705e-08_rb, &
      0.68382e-08_rb,0.72240e-08_rb,0.76285e-08_rb,0.80526e-08_rb,0.84969e-08_rb, &
      0.89624e-08_rb,0.94498e-08_rb,0.99599e-08_rb,0.10494e-07_rb,0.11052e-07_rb, &
      0.11636e-07_rb,0.12246e-07_rb,0.12884e-07_rb,0.13551e-07_rb,0.14246e-07_rb, &
      0.14973e-07_rb,0.15731e-07_rb,0.16522e-07_rb,0.17347e-07_rb,0.18207e-07_rb, &
      0.19103e-07_rb,0.20037e-07_rb,0.21011e-07_rb,0.22024e-07_rb,0.23079e-07_rb, &
      0.24177e-07_rb,0.25320e-07_rb,0.26508e-07_rb,0.27744e-07_rb,0.29029e-07_rb, &
      0.30365e-07_rb,0.31753e-07_rb,0.33194e-07_rb,0.34691e-07_rb,0.36246e-07_rb, &
      0.37859e-07_rb,0.39533e-07_rb,0.41270e-07_rb,0.43071e-07_rb,0.44939e-07_rb/)
      totplk16(151:181) = (/ &
      0.46875e-07_rb,0.48882e-07_rb,0.50961e-07_rb,0.53115e-07_rb,0.55345e-07_rb, &
      0.57655e-07_rb,0.60046e-07_rb,0.62520e-07_rb,0.65080e-07_rb,0.67728e-07_rb, &
      0.70466e-07_rb,0.73298e-07_rb,0.76225e-07_rb,0.79251e-07_rb,0.82377e-07_rb, &
      0.85606e-07_rb,0.88942e-07_rb,0.92386e-07_rb,0.95942e-07_rb,0.99612e-07_rb, &
      0.10340e-06_rb,0.10731e-06_rb,0.11134e-06_rb,0.11550e-06_rb,0.11979e-06_rb, &
      0.12421e-06_rb,0.12876e-06_rb,0.13346e-06_rb,0.13830e-06_rb,0.14328e-06_rb, &
      0.14841e-06_rb/)

      end subroutine lwavplank

!***************************************************************************
      subroutine lwavplankderiv
!***************************************************************************

      save

      totplnkderiv(1:50,  1) = (/ &
      2.22125e-08_rb,2.23245e-08_rb,2.24355e-08_rb,2.25435e-08_rb,2.26560e-08_rb, &
      2.27620e-08_rb,2.28690e-08_rb,2.29760e-08_rb,2.30775e-08_rb,2.31800e-08_rb, &
      2.32825e-08_rb,2.33825e-08_rb,2.34820e-08_rb,2.35795e-08_rb,2.36760e-08_rb, &
      2.37710e-08_rb,2.38655e-08_rb,2.39595e-08_rb,2.40530e-08_rb,2.41485e-08_rb, &
      2.42395e-08_rb,2.43300e-08_rb,2.44155e-08_rb,2.45085e-08_rb,2.45905e-08_rb, &
      2.46735e-08_rb,2.47565e-08_rb,2.48465e-08_rb,2.49315e-08_rb,2.50100e-08_rb, &
      2.50905e-08_rb,2.51705e-08_rb,2.52490e-08_rb,2.53260e-08_rb,2.54075e-08_rb, &
      2.54785e-08_rb,2.55555e-08_rb,2.56340e-08_rb,2.57050e-08_rb,2.57820e-08_rb, &
      2.58525e-08_rb,2.59205e-08_rb,2.59945e-08_rb,2.60680e-08_rb,2.61375e-08_rb, &
      2.61980e-08_rb,2.62745e-08_rb,2.63335e-08_rb,2.63995e-08_rb,2.64710e-08_rb/)
      totplnkderiv(51:100,  1) = (/ &
      2.65300e-08_rb,2.66005e-08_rb,2.66685e-08_rb,2.67310e-08_rb,2.67915e-08_rb, &
      2.68540e-08_rb,2.69065e-08_rb,2.69730e-08_rb,2.70270e-08_rb,2.70690e-08_rb, &
      2.71420e-08_rb,2.71985e-08_rb,2.72560e-08_rb,2.73180e-08_rb,2.73760e-08_rb, &
      2.74285e-08_rb,2.74840e-08_rb,2.75290e-08_rb,2.75950e-08_rb,2.76360e-08_rb, &
      2.76975e-08_rb,2.77475e-08_rb,2.78080e-08_rb,2.78375e-08_rb,2.79120e-08_rb, &
      2.79510e-08_rb,2.79955e-08_rb,2.80625e-08_rb,2.80920e-08_rb,2.81570e-08_rb, &
      2.81990e-08_rb,2.82330e-08_rb,2.82830e-08_rb,2.83365e-08_rb,2.83740e-08_rb, &
      2.84295e-08_rb,2.84910e-08_rb,2.85275e-08_rb,2.85525e-08_rb,2.86085e-08_rb, &
      2.86535e-08_rb,2.86945e-08_rb,2.87355e-08_rb,2.87695e-08_rb,2.88105e-08_rb, &
      2.88585e-08_rb,2.88945e-08_rb,2.89425e-08_rb,2.89580e-08_rb,2.90265e-08_rb/)
      totplnkderiv(101:150,  1) = (/ &
      2.90445e-08_rb,2.90905e-08_rb,2.91425e-08_rb,2.91560e-08_rb,2.91970e-08_rb, &
      2.91905e-08_rb,2.92880e-08_rb,2.92950e-08_rb,2.93630e-08_rb,2.93995e-08_rb, &
      2.94425e-08_rb,2.94635e-08_rb,2.94770e-08_rb,2.95290e-08_rb,2.95585e-08_rb, &
      2.95815e-08_rb,2.95995e-08_rb,2.96745e-08_rb,2.96725e-08_rb,2.97040e-08_rb, &
      2.97750e-08_rb,2.97905e-08_rb,2.98175e-08_rb,2.98355e-08_rb,2.98705e-08_rb, &
      2.99040e-08_rb,2.99680e-08_rb,2.99860e-08_rb,3.00270e-08_rb,3.00200e-08_rb, &
      3.00770e-08_rb,3.00795e-08_rb,3.01065e-08_rb,3.01795e-08_rb,3.01815e-08_rb, &
      3.02025e-08_rb,3.02360e-08_rb,3.02360e-08_rb,3.03090e-08_rb,3.03155e-08_rb, &
      3.03725e-08_rb,3.03635e-08_rb,3.04270e-08_rb,3.04610e-08_rb,3.04635e-08_rb, &
      3.04610e-08_rb,3.05180e-08_rb,3.05430e-08_rb,3.05290e-08_rb,3.05885e-08_rb/)
      totplnkderiv(151:181,  1) = (/ &
      3.05750e-08_rb,3.05775e-08_rb,3.06795e-08_rb,3.07025e-08_rb,3.07365e-08_rb, &
      3.07435e-08_rb,3.07525e-08_rb,3.07680e-08_rb,3.08115e-08_rb,3.07930e-08_rb, &
      3.08155e-08_rb,3.08660e-08_rb,3.08865e-08_rb,3.08390e-08_rb,3.09340e-08_rb, &
      3.09685e-08_rb,3.09340e-08_rb,3.09820e-08_rb,3.10365e-08_rb,3.10705e-08_rb, &
      3.10750e-08_rb,3.10475e-08_rb,3.11685e-08_rb,3.11455e-08_rb,3.11500e-08_rb, &
      3.11775e-08_rb,3.11890e-08_rb,3.12045e-08_rb,3.12185e-08_rb,3.12415e-08_rb, &
      3.12590e-08_rb/)
      totplnkderiv(1:50,  2) = (/ &
      4.91150e-08_rb,4.97290e-08_rb,5.03415e-08_rb,5.09460e-08_rb,5.15550e-08_rb, &
      5.21540e-08_rb,5.27575e-08_rb,5.33500e-08_rb,5.39500e-08_rb,5.45445e-08_rb, &
      5.51290e-08_rb,5.57235e-08_rb,5.62955e-08_rb,5.68800e-08_rb,5.74620e-08_rb, &
      5.80425e-08_rb,5.86145e-08_rb,5.91810e-08_rb,5.97435e-08_rb,6.03075e-08_rb, &
      6.08625e-08_rb,6.14135e-08_rb,6.19775e-08_rb,6.25185e-08_rb,6.30675e-08_rb, &
      6.36145e-08_rb,6.41535e-08_rb,6.46920e-08_rb,6.52265e-08_rb,6.57470e-08_rb, &
      6.62815e-08_rb,6.68000e-08_rb,6.73320e-08_rb,6.78550e-08_rb,6.83530e-08_rb, &
      6.88760e-08_rb,6.93735e-08_rb,6.98790e-08_rb,7.03950e-08_rb,7.08810e-08_rb, &
      7.13815e-08_rb,7.18795e-08_rb,7.23415e-08_rb,7.28505e-08_rb,7.33285e-08_rb, &
      7.38075e-08_rb,7.42675e-08_rb,7.47605e-08_rb,7.52380e-08_rb,7.57020e-08_rb/)
      totplnkderiv(51:100,  2) = (/ &
      7.61495e-08_rb,7.65955e-08_rb,7.70565e-08_rb,7.75185e-08_rb,7.79735e-08_rb, &
      7.83915e-08_rb,7.88625e-08_rb,7.93215e-08_rb,7.97425e-08_rb,8.02195e-08_rb, &
      8.05905e-08_rb,8.10335e-08_rb,8.14770e-08_rb,8.19025e-08_rb,8.22955e-08_rb, &
      8.27115e-08_rb,8.31165e-08_rb,8.35645e-08_rb,8.39440e-08_rb,8.43785e-08_rb, &
      8.47380e-08_rb,8.51495e-08_rb,8.55405e-08_rb,8.59720e-08_rb,8.63135e-08_rb, &
      8.67065e-08_rb,8.70930e-08_rb,8.74545e-08_rb,8.78780e-08_rb,8.82160e-08_rb, &
      8.85625e-08_rb,8.89850e-08_rb,8.93395e-08_rb,8.97080e-08_rb,9.00675e-08_rb, &
      9.04085e-08_rb,9.07360e-08_rb,9.11315e-08_rb,9.13815e-08_rb,9.18320e-08_rb, &
      9.21500e-08_rb,9.24725e-08_rb,9.28640e-08_rb,9.31955e-08_rb,9.35185e-08_rb, &
      9.38645e-08_rb,9.41780e-08_rb,9.45465e-08_rb,9.48470e-08_rb,9.51375e-08_rb/)
      totplnkderiv(101:150,  2) = (/ &
      9.55245e-08_rb,9.57925e-08_rb,9.61195e-08_rb,9.64750e-08_rb,9.68110e-08_rb, &
      9.71715e-08_rb,9.74150e-08_rb,9.77250e-08_rb,9.79600e-08_rb,9.82600e-08_rb, &
      9.85300e-08_rb,9.88400e-08_rb,9.91600e-08_rb,9.95350e-08_rb,9.97500e-08_rb, &
      1.00090e-07_rb,1.00370e-07_rb,1.00555e-07_rb,1.00935e-07_rb,1.01275e-07_rb, &
      1.01400e-07_rb,1.01790e-07_rb,1.01945e-07_rb,1.02225e-07_rb,1.02585e-07_rb, &
      1.02895e-07_rb,1.03010e-07_rb,1.03285e-07_rb,1.03540e-07_rb,1.03890e-07_rb, &
      1.04015e-07_rb,1.04420e-07_rb,1.04640e-07_rb,1.04810e-07_rb,1.05090e-07_rb, &
      1.05385e-07_rb,1.05600e-07_rb,1.05965e-07_rb,1.06050e-07_rb,1.06385e-07_rb, &
      1.06390e-07_rb,1.06795e-07_rb,1.06975e-07_rb,1.07240e-07_rb,1.07435e-07_rb, &
      1.07815e-07_rb,1.07960e-07_rb,1.08010e-07_rb,1.08535e-07_rb,1.08670e-07_rb/)
      totplnkderiv(151:181,  2) = (/ &
      1.08855e-07_rb,1.09210e-07_rb,1.09195e-07_rb,1.09510e-07_rb,1.09665e-07_rb, &
      1.09885e-07_rb,1.10130e-07_rb,1.10440e-07_rb,1.10640e-07_rb,1.10760e-07_rb, &
      1.11125e-07_rb,1.11195e-07_rb,1.11345e-07_rb,1.11710e-07_rb,1.11765e-07_rb, &
      1.11960e-07_rb,1.12225e-07_rb,1.12460e-07_rb,1.12595e-07_rb,1.12730e-07_rb, &
      1.12880e-07_rb,1.13295e-07_rb,1.13215e-07_rb,1.13505e-07_rb,1.13665e-07_rb, &
      1.13870e-07_rb,1.14025e-07_rb,1.14325e-07_rb,1.14495e-07_rb,1.14605e-07_rb, &
      1.14905e-07_rb/)
      totplnkderiv(1:50, 3) = (/ &
      4.27040e-08_rb,4.35430e-08_rb,4.43810e-08_rb,4.52210e-08_rb,4.60630e-08_rb, &
      4.69135e-08_rb,4.77585e-08_rb,4.86135e-08_rb,4.94585e-08_rb,5.03230e-08_rb, &
      5.11740e-08_rb,5.20250e-08_rb,5.28940e-08_rb,5.37465e-08_rb,5.46175e-08_rb, &
      5.54700e-08_rb,5.63430e-08_rb,5.72085e-08_rb,5.80735e-08_rb,5.89430e-08_rb, &
      5.98015e-08_rb,6.06680e-08_rb,6.15380e-08_rb,6.24130e-08_rb,6.32755e-08_rb, &
      6.41340e-08_rb,6.50060e-08_rb,6.58690e-08_rb,6.67315e-08_rb,6.76025e-08_rb, &
      6.84585e-08_rb,6.93205e-08_rb,7.01845e-08_rb,7.10485e-08_rb,7.19160e-08_rb, &
      7.27695e-08_rb,7.36145e-08_rb,7.44840e-08_rb,7.53405e-08_rb,7.61770e-08_rb, &
      7.70295e-08_rb,7.78745e-08_rb,7.87350e-08_rb,7.95740e-08_rb,8.04150e-08_rb, &
      8.12565e-08_rb,8.20885e-08_rb,8.29455e-08_rb,8.37830e-08_rb,8.46035e-08_rb/)
      totplnkderiv(51:100, 3) = (/ &
      8.54315e-08_rb,8.62770e-08_rb,8.70975e-08_rb,8.79140e-08_rb,8.87190e-08_rb, &
      8.95625e-08_rb,9.03625e-08_rb,9.11795e-08_rb,9.19930e-08_rb,9.27685e-08_rb, &
      9.36095e-08_rb,9.43785e-08_rb,9.52375e-08_rb,9.59905e-08_rb,9.67680e-08_rb, &
      9.75840e-08_rb,9.83755e-08_rb,9.91710e-08_rb,9.99445e-08_rb,1.00706e-07_rb, &
      1.01477e-07_rb,1.02255e-07_rb,1.03021e-07_rb,1.03776e-07_rb,1.04544e-07_rb, &
      1.05338e-07_rb,1.06082e-07_rb,1.06843e-07_rb,1.07543e-07_rb,1.08298e-07_rb, &
      1.09103e-07_rb,1.09812e-07_rb,1.10536e-07_rb,1.11268e-07_rb,1.12027e-07_rb, &
      1.12727e-07_rb,1.13464e-07_rb,1.14183e-07_rb,1.15037e-07_rb,1.15615e-07_rb, &
      1.16329e-07_rb,1.17057e-07_rb,1.17734e-07_rb,1.18448e-07_rb,1.19149e-07_rb, &
      1.19835e-07_rb,1.20512e-07_rb,1.21127e-07_rb,1.21895e-07_rb,1.22581e-07_rb/)
      totplnkderiv(101:150, 3) = (/ &
      1.23227e-07_rb,1.23928e-07_rb,1.24560e-07_rb,1.25220e-07_rb,1.25895e-07_rb, &
      1.26565e-07_rb,1.27125e-07_rb,1.27855e-07_rb,1.28490e-07_rb,1.29195e-07_rb, &
      1.29790e-07_rb,1.30470e-07_rb,1.31070e-07_rb,1.31690e-07_rb,1.32375e-07_rb, &
      1.32960e-07_rb,1.33570e-07_rb,1.34230e-07_rb,1.34840e-07_rb,1.35315e-07_rb, &
      1.35990e-07_rb,1.36555e-07_rb,1.37265e-07_rb,1.37945e-07_rb,1.38425e-07_rb, &
      1.38950e-07_rb,1.39640e-07_rb,1.40220e-07_rb,1.40775e-07_rb,1.41400e-07_rb, &
      1.42020e-07_rb,1.42500e-07_rb,1.43085e-07_rb,1.43680e-07_rb,1.44255e-07_rb, &
      1.44855e-07_rb,1.45385e-07_rb,1.45890e-07_rb,1.46430e-07_rb,1.46920e-07_rb, &
      1.47715e-07_rb,1.48090e-07_rb,1.48695e-07_rb,1.49165e-07_rb,1.49715e-07_rb, &
      1.50130e-07_rb,1.50720e-07_rb,1.51330e-07_rb,1.51725e-07_rb,1.52350e-07_rb/)
      totplnkderiv(151:181, 3) = (/ &
      1.52965e-07_rb,1.53305e-07_rb,1.53915e-07_rb,1.54280e-07_rb,1.54950e-07_rb, &
      1.55370e-07_rb,1.55850e-07_rb,1.56260e-07_rb,1.56825e-07_rb,1.57470e-07_rb, &
      1.57760e-07_rb,1.58295e-07_rb,1.58780e-07_rb,1.59470e-07_rb,1.59940e-07_rb, &
      1.60325e-07_rb,1.60825e-07_rb,1.61100e-07_rb,1.61605e-07_rb,1.62045e-07_rb, &
      1.62670e-07_rb,1.63020e-07_rb,1.63625e-07_rb,1.63900e-07_rb,1.64420e-07_rb, &
      1.64705e-07_rb,1.65430e-07_rb,1.65610e-07_rb,1.66220e-07_rb,1.66585e-07_rb, &
      1.66965e-07_rb/)
      totplnkderiv(1:50, 4) = (/ &
      3.32829e-08_rb,3.41160e-08_rb,3.49626e-08_rb,3.58068e-08_rb,3.66765e-08_rb, &
      3.75320e-08_rb,3.84095e-08_rb,3.92920e-08_rb,4.01830e-08_rb,4.10715e-08_rb, &
      4.19735e-08_rb,4.28835e-08_rb,4.37915e-08_rb,4.47205e-08_rb,4.56410e-08_rb, &
      4.65770e-08_rb,4.75090e-08_rb,4.84530e-08_rb,4.93975e-08_rb,5.03470e-08_rb, &
      5.13000e-08_rb,5.22560e-08_rb,5.32310e-08_rb,5.41865e-08_rb,5.51655e-08_rb, &
      5.61590e-08_rb,5.71120e-08_rb,5.81075e-08_rb,5.91060e-08_rb,6.00895e-08_rb, &
      6.10750e-08_rb,6.20740e-08_rb,6.30790e-08_rb,6.40765e-08_rb,6.50940e-08_rb, &
      6.60895e-08_rb,6.71230e-08_rb,6.81200e-08_rb,6.91260e-08_rb,7.01485e-08_rb, &
      7.11625e-08_rb,7.21870e-08_rb,7.32010e-08_rb,7.42080e-08_rb,7.52285e-08_rb, &
      7.62930e-08_rb,7.73040e-08_rb,7.83185e-08_rb,7.93410e-08_rb,8.03560e-08_rb/)
      totplnkderiv(51:100, 4) = (/ &
      8.14115e-08_rb,8.24200e-08_rb,8.34555e-08_rb,8.45100e-08_rb,8.55265e-08_rb, &
      8.65205e-08_rb,8.75615e-08_rb,8.85870e-08_rb,8.96175e-08_rb,9.07015e-08_rb, &
      9.16475e-08_rb,9.27525e-08_rb,9.37055e-08_rb,9.47375e-08_rb,9.57995e-08_rb, &
      9.67635e-08_rb,9.77980e-08_rb,9.87735e-08_rb,9.98485e-08_rb,1.00904e-07_rb, &
      1.01900e-07_rb,1.02876e-07_rb,1.03905e-07_rb,1.04964e-07_rb,1.05956e-07_rb, &
      1.06870e-07_rb,1.07952e-07_rb,1.08944e-07_rb,1.10003e-07_rb,1.10965e-07_rb, &
      1.11952e-07_rb,1.12927e-07_rb,1.13951e-07_rb,1.14942e-07_rb,1.15920e-07_rb, &
      1.16968e-07_rb,1.17877e-07_rb,1.18930e-07_rb,1.19862e-07_rb,1.20817e-07_rb, &
      1.21817e-07_rb,1.22791e-07_rb,1.23727e-07_rb,1.24751e-07_rb,1.25697e-07_rb, &
      1.26634e-07_rb,1.27593e-07_rb,1.28585e-07_rb,1.29484e-07_rb,1.30485e-07_rb/)
      totplnkderiv(101:150, 4) = (/ &
      1.31363e-07_rb,1.32391e-07_rb,1.33228e-07_rb,1.34155e-07_rb,1.35160e-07_rb, &
      1.36092e-07_rb,1.37070e-07_rb,1.37966e-07_rb,1.38865e-07_rb,1.39740e-07_rb, &
      1.40770e-07_rb,1.41620e-07_rb,1.42605e-07_rb,1.43465e-07_rb,1.44240e-07_rb, &
      1.45305e-07_rb,1.46220e-07_rb,1.47070e-07_rb,1.47935e-07_rb,1.48890e-07_rb, &
      1.49905e-07_rb,1.50640e-07_rb,1.51435e-07_rb,1.52335e-07_rb,1.53235e-07_rb, &
      1.54045e-07_rb,1.54895e-07_rb,1.55785e-07_rb,1.56870e-07_rb,1.57360e-07_rb, &
      1.58395e-07_rb,1.59185e-07_rb,1.60060e-07_rb,1.60955e-07_rb,1.61770e-07_rb, &
      1.62445e-07_rb,1.63415e-07_rb,1.64170e-07_rb,1.65125e-07_rb,1.65995e-07_rb, &
      1.66545e-07_rb,1.67580e-07_rb,1.68295e-07_rb,1.69130e-07_rb,1.69935e-07_rb, &
      1.70800e-07_rb,1.71610e-07_rb,1.72365e-07_rb,1.73215e-07_rb,1.73770e-07_rb/)
      totplnkderiv(151:181, 4) = (/ &
      1.74590e-07_rb,1.75525e-07_rb,1.76095e-07_rb,1.77125e-07_rb,1.77745e-07_rb, &
      1.78580e-07_rb,1.79315e-07_rb,1.80045e-07_rb,1.80695e-07_rb,1.81580e-07_rb, &
      1.82360e-07_rb,1.83205e-07_rb,1.84055e-07_rb,1.84315e-07_rb,1.85225e-07_rb, &
      1.85865e-07_rb,1.86660e-07_rb,1.87445e-07_rb,1.88350e-07_rb,1.88930e-07_rb, &
      1.89420e-07_rb,1.90275e-07_rb,1.90630e-07_rb,1.91650e-07_rb,1.92485e-07_rb, &
      1.93285e-07_rb,1.93695e-07_rb,1.94595e-07_rb,1.94895e-07_rb,1.95960e-07_rb, &
      1.96525e-07_rb/)
      totplnkderiv(1:50, 5) = (/ &
      2.41948e-08_rb,2.49273e-08_rb,2.56705e-08_rb,2.64263e-08_rb,2.71899e-08_rb, &
      2.79687e-08_rb,2.87531e-08_rb,2.95520e-08_rb,3.03567e-08_rb,3.11763e-08_rb, &
      3.20014e-08_rb,3.28390e-08_rb,3.36865e-08_rb,3.45395e-08_rb,3.54083e-08_rb, &
      3.62810e-08_rb,3.71705e-08_rb,3.80585e-08_rb,3.89650e-08_rb,3.98750e-08_rb, &
      4.07955e-08_rb,4.17255e-08_rb,4.26635e-08_rb,4.36095e-08_rb,4.45605e-08_rb, &
      4.55190e-08_rb,4.64910e-08_rb,4.74670e-08_rb,4.84480e-08_rb,4.94430e-08_rb, &
      5.04460e-08_rb,5.14440e-08_rb,5.24500e-08_rb,5.34835e-08_rb,5.44965e-08_rb, &
      5.55325e-08_rb,5.65650e-08_rb,5.76050e-08_rb,5.86615e-08_rb,5.97175e-08_rb, &
      6.07750e-08_rb,6.18400e-08_rb,6.29095e-08_rb,6.39950e-08_rb,6.50665e-08_rb, &
      6.61405e-08_rb,6.72290e-08_rb,6.82800e-08_rb,6.94445e-08_rb,7.05460e-08_rb/)
      totplnkderiv(51:100, 5) = (/ &
      7.16400e-08_rb,7.27475e-08_rb,7.38790e-08_rb,7.49845e-08_rb,7.61270e-08_rb, &
      7.72375e-08_rb,7.83770e-08_rb,7.95045e-08_rb,8.06315e-08_rb,8.17715e-08_rb, &
      8.29275e-08_rb,8.40555e-08_rb,8.52110e-08_rb,8.63565e-08_rb,8.75045e-08_rb, &
      8.86735e-08_rb,8.98150e-08_rb,9.09970e-08_rb,9.21295e-08_rb,9.32730e-08_rb, &
      9.44605e-08_rb,9.56170e-08_rb,9.67885e-08_rb,9.79275e-08_rb,9.91190e-08_rb, &
      1.00278e-07_rb,1.01436e-07_rb,1.02625e-07_rb,1.03792e-07_rb,1.04989e-07_rb, &
      1.06111e-07_rb,1.07320e-07_rb,1.08505e-07_rb,1.09626e-07_rb,1.10812e-07_rb, &
      1.11948e-07_rb,1.13162e-07_rb,1.14289e-07_rb,1.15474e-07_rb,1.16661e-07_rb, &
      1.17827e-07_rb,1.19023e-07_rb,1.20167e-07_rb,1.21356e-07_rb,1.22499e-07_rb, &
      1.23653e-07_rb,1.24876e-07_rb,1.25983e-07_rb,1.27175e-07_rb,1.28325e-07_rb/)
      totplnkderiv(101:150, 5) = (/ &
      1.29517e-07_rb,1.30685e-07_rb,1.31840e-07_rb,1.33013e-07_rb,1.34160e-07_rb, &
      1.35297e-07_rb,1.36461e-07_rb,1.37630e-07_rb,1.38771e-07_rb,1.39913e-07_rb, &
      1.41053e-07_rb,1.42218e-07_rb,1.43345e-07_rb,1.44460e-07_rb,1.45692e-07_rb, &
      1.46697e-07_rb,1.47905e-07_rb,1.49010e-07_rb,1.50210e-07_rb,1.51285e-07_rb, &
      1.52380e-07_rb,1.53555e-07_rb,1.54655e-07_rb,1.55805e-07_rb,1.56850e-07_rb, &
      1.58055e-07_rb,1.59115e-07_rb,1.60185e-07_rb,1.61255e-07_rb,1.62465e-07_rb, &
      1.63575e-07_rb,1.64675e-07_rb,1.65760e-07_rb,1.66765e-07_rb,1.67945e-07_rb, &
      1.69070e-07_rb,1.70045e-07_rb,1.71145e-07_rb,1.72260e-07_rb,1.73290e-07_rb, &
      1.74470e-07_rb,1.75490e-07_rb,1.76515e-07_rb,1.77555e-07_rb,1.78660e-07_rb, &
      1.79670e-07_rb,1.80705e-07_rb,1.81895e-07_rb,1.82745e-07_rb,1.83950e-07_rb/)
      totplnkderiv(151:181, 5) = (/ &
      1.84955e-07_rb,1.85940e-07_rb,1.87080e-07_rb,1.88010e-07_rb,1.89145e-07_rb, &
      1.90130e-07_rb,1.91110e-07_rb,1.92130e-07_rb,1.93205e-07_rb,1.94230e-07_rb, &
      1.95045e-07_rb,1.96070e-07_rb,1.97155e-07_rb,1.98210e-07_rb,1.99080e-07_rb, &
      2.00280e-07_rb,2.01135e-07_rb,2.02150e-07_rb,2.03110e-07_rb,2.04135e-07_rb, &
      2.05110e-07_rb,2.06055e-07_rb,2.07120e-07_rb,2.08075e-07_rb,2.08975e-07_rb, &
      2.09950e-07_rb,2.10870e-07_rb,2.11830e-07_rb,2.12960e-07_rb,2.13725e-07_rb, &
      2.14765e-07_rb/)
      totplnkderiv(1:50, 6) = (/ &
      1.36567e-08_rb,1.41766e-08_rb,1.47079e-08_rb,1.52499e-08_rb,1.58075e-08_rb, &
      1.63727e-08_rb,1.69528e-08_rb,1.75429e-08_rb,1.81477e-08_rb,1.87631e-08_rb, &
      1.93907e-08_rb,2.00297e-08_rb,2.06808e-08_rb,2.13432e-08_rb,2.20183e-08_rb, &
      2.27076e-08_rb,2.34064e-08_rb,2.41181e-08_rb,2.48400e-08_rb,2.55750e-08_rb, &
      2.63231e-08_rb,2.70790e-08_rb,2.78502e-08_rb,2.86326e-08_rb,2.94259e-08_rb, &
      3.02287e-08_rb,3.10451e-08_rb,3.18752e-08_rb,3.27108e-08_rb,3.35612e-08_rb, &
      3.44198e-08_rb,3.52930e-08_rb,3.61785e-08_rb,3.70690e-08_rb,3.79725e-08_rb, &
      3.88845e-08_rb,3.98120e-08_rb,4.07505e-08_rb,4.16965e-08_rb,4.26515e-08_rb, &
      4.36190e-08_rb,4.45925e-08_rb,4.55760e-08_rb,4.65735e-08_rb,4.75835e-08_rb, &
      4.85970e-08_rb,4.96255e-08_rb,5.06975e-08_rb,5.16950e-08_rb,5.27530e-08_rb/)
      totplnkderiv(51:100, 6) = (/ &
      5.38130e-08_rb,5.48860e-08_rb,5.59715e-08_rb,5.70465e-08_rb,5.81385e-08_rb, &
      5.92525e-08_rb,6.03565e-08_rb,6.14815e-08_rb,6.26175e-08_rb,6.37475e-08_rb, &
      6.48855e-08_rb,6.60340e-08_rb,6.71980e-08_rb,6.83645e-08_rb,6.95430e-08_rb, &
      7.07145e-08_rb,7.19015e-08_rb,7.30995e-08_rb,7.43140e-08_rb,7.55095e-08_rb, &
      7.67115e-08_rb,7.79485e-08_rb,7.91735e-08_rb,8.03925e-08_rb,8.16385e-08_rb, &
      8.28775e-08_rb,8.41235e-08_rb,8.53775e-08_rb,8.66405e-08_rb,8.78940e-08_rb, &
      8.91805e-08_rb,9.04515e-08_rb,9.17290e-08_rb,9.30230e-08_rb,9.43145e-08_rb, &
      9.56200e-08_rb,9.69160e-08_rb,9.82140e-08_rb,9.95285e-08_rb,1.00829e-07_rb, &
      1.02145e-07_rb,1.03478e-07_rb,1.04787e-07_rb,1.06095e-07_rb,1.07439e-07_rb, &
      1.08785e-07_rb,1.10078e-07_rb,1.11466e-07_rb,1.12795e-07_rb,1.14133e-07_rb/)
      totplnkderiv(101:150, 6) = (/ &
      1.15479e-07_rb,1.16825e-07_rb,1.18191e-07_rb,1.19540e-07_rb,1.20908e-07_rb, &
      1.22257e-07_rb,1.23634e-07_rb,1.24992e-07_rb,1.26345e-07_rb,1.27740e-07_rb, &
      1.29098e-07_rb,1.30447e-07_rb,1.31831e-07_rb,1.33250e-07_rb,1.34591e-07_rb, &
      1.36011e-07_rb,1.37315e-07_rb,1.38721e-07_rb,1.40103e-07_rb,1.41504e-07_rb, &
      1.42882e-07_rb,1.44259e-07_rb,1.45674e-07_rb,1.46997e-07_rb,1.48412e-07_rb, &
      1.49794e-07_rb,1.51167e-07_rb,1.52577e-07_rb,1.53941e-07_rb,1.55369e-07_rb, &
      1.56725e-07_rb,1.58125e-07_rb,1.59460e-07_rb,1.60895e-07_rb,1.62260e-07_rb, &
      1.63610e-07_rb,1.65085e-07_rb,1.66410e-07_rb,1.67805e-07_rb,1.69185e-07_rb, &
      1.70570e-07_rb,1.71915e-07_rb,1.73375e-07_rb,1.74775e-07_rb,1.76090e-07_rb, &
      1.77485e-07_rb,1.78905e-07_rb,1.80190e-07_rb,1.81610e-07_rb,1.82960e-07_rb/)
      totplnkderiv(151:181, 6) = (/ &
      1.84330e-07_rb,1.85750e-07_rb,1.87060e-07_rb,1.88470e-07_rb,1.89835e-07_rb, &
      1.91250e-07_rb,1.92565e-07_rb,1.93925e-07_rb,1.95220e-07_rb,1.96620e-07_rb, &
      1.98095e-07_rb,1.99330e-07_rb,2.00680e-07_rb,2.02090e-07_rb,2.03360e-07_rb, &
      2.04775e-07_rb,2.06080e-07_rb,2.07440e-07_rb,2.08820e-07_rb,2.10095e-07_rb, &
      2.11445e-07_rb,2.12785e-07_rb,2.14050e-07_rb,2.15375e-07_rb,2.16825e-07_rb, &
      2.18080e-07_rb,2.19345e-07_rb,2.20710e-07_rb,2.21980e-07_rb,2.23425e-07_rb, &
      2.24645e-07_rb/)
      totplnkderiv(1:50, 7) = (/ &
      7.22270e-09_rb,7.55350e-09_rb,7.89480e-09_rb,8.24725e-09_rb,8.60780e-09_rb, &
      8.98215e-09_rb,9.36430e-09_rb,9.76035e-09_rb,1.01652e-08_rb,1.05816e-08_rb, &
      1.10081e-08_rb,1.14480e-08_rb,1.18981e-08_rb,1.23600e-08_rb,1.28337e-08_rb, &
      1.33172e-08_rb,1.38139e-08_rb,1.43208e-08_rb,1.48413e-08_rb,1.53702e-08_rb, &
      1.59142e-08_rb,1.64704e-08_rb,1.70354e-08_rb,1.76178e-08_rb,1.82065e-08_rb, &
      1.88083e-08_rb,1.94237e-08_rb,2.00528e-08_rb,2.06913e-08_rb,2.13413e-08_rb, &
      2.20058e-08_rb,2.26814e-08_rb,2.33686e-08_rb,2.40729e-08_rb,2.47812e-08_rb, &
      2.55099e-08_rb,2.62449e-08_rb,2.69966e-08_rb,2.77569e-08_rb,2.85269e-08_rb, &
      2.93144e-08_rb,3.01108e-08_rb,3.09243e-08_rb,3.17433e-08_rb,3.25756e-08_rb, &
      3.34262e-08_rb,3.42738e-08_rb,3.51480e-08_rb,3.60285e-08_rb,3.69160e-08_rb/)
      totplnkderiv(51:100, 7) = (/ &
      3.78235e-08_rb,3.87390e-08_rb,3.96635e-08_rb,4.06095e-08_rb,4.15600e-08_rb, &
      4.25180e-08_rb,4.34895e-08_rb,4.44800e-08_rb,4.54715e-08_rb,4.64750e-08_rb, &
      4.74905e-08_rb,4.85210e-08_rb,4.95685e-08_rb,5.06135e-08_rb,5.16725e-08_rb, &
      5.27480e-08_rb,5.38265e-08_rb,5.49170e-08_rb,5.60120e-08_rb,5.71275e-08_rb, &
      5.82610e-08_rb,5.93775e-08_rb,6.05245e-08_rb,6.17025e-08_rb,6.28355e-08_rb, &
      6.40135e-08_rb,6.52015e-08_rb,6.63865e-08_rb,6.75790e-08_rb,6.88120e-08_rb, &
      7.00070e-08_rb,7.12335e-08_rb,7.24720e-08_rb,7.37340e-08_rb,7.49775e-08_rb, &
      7.62415e-08_rb,7.75185e-08_rb,7.87915e-08_rb,8.00875e-08_rb,8.13630e-08_rb, &
      8.26710e-08_rb,8.39645e-08_rb,8.53060e-08_rb,8.66305e-08_rb,8.79915e-08_rb, &
      8.93080e-08_rb,9.06560e-08_rb,9.19860e-08_rb,9.33550e-08_rb,9.47305e-08_rb/)
      totplnkderiv(101:150, 7) = (/ &
      9.61180e-08_rb,9.74500e-08_rb,9.88850e-08_rb,1.00263e-07_rb,1.01688e-07_rb, &
      1.03105e-07_rb,1.04489e-07_rb,1.05906e-07_rb,1.07345e-07_rb,1.08771e-07_rb, &
      1.10220e-07_rb,1.11713e-07_rb,1.13098e-07_rb,1.14515e-07_rb,1.16019e-07_rb, &
      1.17479e-07_rb,1.18969e-07_rb,1.20412e-07_rb,1.21852e-07_rb,1.23387e-07_rb, &
      1.24851e-07_rb,1.26319e-07_rb,1.27811e-07_rb,1.29396e-07_rb,1.30901e-07_rb, &
      1.32358e-07_rb,1.33900e-07_rb,1.35405e-07_rb,1.36931e-07_rb,1.38443e-07_rb, &
      1.39985e-07_rb,1.41481e-07_rb,1.43072e-07_rb,1.44587e-07_rb,1.46133e-07_rb, &
      1.47698e-07_rb,1.49203e-07_rb,1.50712e-07_rb,1.52363e-07_rb,1.53795e-07_rb, &
      1.55383e-07_rb,1.56961e-07_rb,1.58498e-07_rb,1.60117e-07_rb,1.61745e-07_rb, &
      1.63190e-07_rb,1.64790e-07_rb,1.66370e-07_rb,1.67975e-07_rb,1.69555e-07_rb/)
      totplnkderiv(151:181, 7) = (/ &
      1.71060e-07_rb,1.72635e-07_rb,1.74345e-07_rb,1.75925e-07_rb,1.77395e-07_rb, &
      1.78960e-07_rb,1.80620e-07_rb,1.82180e-07_rb,1.83840e-07_rb,1.85340e-07_rb, &
      1.86940e-07_rb,1.88550e-07_rb,1.90095e-07_rb,1.91670e-07_rb,1.93385e-07_rb, &
      1.94895e-07_rb,1.96500e-07_rb,1.98090e-07_rb,1.99585e-07_rb,2.01280e-07_rb, &
      2.02950e-07_rb,2.04455e-07_rb,2.06075e-07_rb,2.07635e-07_rb,2.09095e-07_rb, &
      2.10865e-07_rb,2.12575e-07_rb,2.14050e-07_rb,2.15630e-07_rb,2.17060e-07_rb, &
      2.18715e-07_rb/)
      totplnkderiv(1:50, 8) = (/ &
      4.26397e-09_rb,4.48470e-09_rb,4.71299e-09_rb,4.94968e-09_rb,5.19542e-09_rb, &
      5.44847e-09_rb,5.71195e-09_rb,5.98305e-09_rb,6.26215e-09_rb,6.55290e-09_rb, &
      6.85190e-09_rb,7.15950e-09_rb,7.47745e-09_rb,7.80525e-09_rb,8.14190e-09_rb, &
      8.48915e-09_rb,8.84680e-09_rb,9.21305e-09_rb,9.59105e-09_rb,9.98130e-09_rb, &
      1.03781e-08_rb,1.07863e-08_rb,1.12094e-08_rb,1.16371e-08_rb,1.20802e-08_rb, &
      1.25327e-08_rb,1.29958e-08_rb,1.34709e-08_rb,1.39592e-08_rb,1.44568e-08_rb, &
      1.49662e-08_rb,1.54828e-08_rb,1.60186e-08_rb,1.65612e-08_rb,1.71181e-08_rb, &
      1.76822e-08_rb,1.82591e-08_rb,1.88487e-08_rb,1.94520e-08_rb,2.00691e-08_rb, &
      2.06955e-08_rb,2.13353e-08_rb,2.19819e-08_rb,2.26479e-08_rb,2.33234e-08_rb, &
      2.40058e-08_rb,2.47135e-08_rb,2.54203e-08_rb,2.61414e-08_rb,2.68778e-08_rb/)
      totplnkderiv(51:100, 8) = (/ &
      2.76265e-08_rb,2.83825e-08_rb,2.91632e-08_rb,2.99398e-08_rb,3.07389e-08_rb, &
      3.15444e-08_rb,3.23686e-08_rb,3.31994e-08_rb,3.40487e-08_rb,3.49020e-08_rb, &
      3.57715e-08_rb,3.66515e-08_rb,3.75465e-08_rb,3.84520e-08_rb,3.93675e-08_rb, &
      4.02985e-08_rb,4.12415e-08_rb,4.21965e-08_rb,4.31630e-08_rb,4.41360e-08_rb, &
      4.51220e-08_rb,4.61235e-08_rb,4.71440e-08_rb,4.81515e-08_rb,4.91905e-08_rb, &
      5.02395e-08_rb,5.12885e-08_rb,5.23735e-08_rb,5.34460e-08_rb,5.45245e-08_rb, &
      5.56375e-08_rb,5.67540e-08_rb,5.78780e-08_rb,5.90065e-08_rb,6.01520e-08_rb, &
      6.13000e-08_rb,6.24720e-08_rb,6.36530e-08_rb,6.48500e-08_rb,6.60500e-08_rb, &
      6.72435e-08_rb,6.84735e-08_rb,6.97025e-08_rb,7.09530e-08_rb,7.21695e-08_rb, &
      7.34270e-08_rb,7.47295e-08_rb,7.59915e-08_rb,7.72685e-08_rb,7.85925e-08_rb/)
      totplnkderiv(101:150, 8) = (/ &
      7.98855e-08_rb,8.12205e-08_rb,8.25120e-08_rb,8.38565e-08_rb,8.52005e-08_rb, &
      8.65570e-08_rb,8.79075e-08_rb,8.92920e-08_rb,9.06535e-08_rb,9.20455e-08_rb, &
      9.34230e-08_rb,9.48355e-08_rb,9.62720e-08_rb,9.76890e-08_rb,9.90755e-08_rb, &
      1.00528e-07_rb,1.01982e-07_rb,1.03436e-07_rb,1.04919e-07_rb,1.06368e-07_rb, &
      1.07811e-07_rb,1.09326e-07_rb,1.10836e-07_rb,1.12286e-07_rb,1.13803e-07_rb, &
      1.15326e-07_rb,1.16809e-07_rb,1.18348e-07_rb,1.19876e-07_rb,1.21413e-07_rb, &
      1.22922e-07_rb,1.24524e-07_rb,1.26049e-07_rb,1.27573e-07_rb,1.29155e-07_rb, &
      1.30708e-07_rb,1.32327e-07_rb,1.33958e-07_rb,1.35480e-07_rb,1.37081e-07_rb, &
      1.38716e-07_rb,1.40326e-07_rb,1.41872e-07_rb,1.43468e-07_rb,1.45092e-07_rb, &
      1.46806e-07_rb,1.48329e-07_rb,1.49922e-07_rb,1.51668e-07_rb,1.53241e-07_rb/)
      totplnkderiv(151:181, 8) = (/ &
      1.54996e-07_rb,1.56561e-07_rb,1.58197e-07_rb,1.59884e-07_rb,1.61576e-07_rb, &
      1.63200e-07_rb,1.64885e-07_rb,1.66630e-07_rb,1.68275e-07_rb,1.69935e-07_rb, &
      1.71650e-07_rb,1.73245e-07_rb,1.75045e-07_rb,1.76710e-07_rb,1.78330e-07_rb, &
      1.79995e-07_rb,1.81735e-07_rb,1.83470e-07_rb,1.85200e-07_rb,1.86890e-07_rb, &
      1.88595e-07_rb,1.90300e-07_rb,1.91995e-07_rb,1.93715e-07_rb,1.95495e-07_rb, &
      1.97130e-07_rb,1.98795e-07_rb,2.00680e-07_rb,2.02365e-07_rb,2.04090e-07_rb, &
      2.05830e-07_rb/)
      totplnkderiv(1:50, 9) = (/ &
      1.85410e-09_rb,1.96515e-09_rb,2.08117e-09_rb,2.20227e-09_rb,2.32861e-09_rb, &
      2.46066e-09_rb,2.59812e-09_rb,2.74153e-09_rb,2.89058e-09_rb,3.04567e-09_rb, &
      3.20674e-09_rb,3.37442e-09_rb,3.54854e-09_rb,3.72892e-09_rb,3.91630e-09_rb, &
      4.11013e-09_rb,4.31150e-09_rb,4.52011e-09_rb,4.73541e-09_rb,4.95870e-09_rb, &
      5.18913e-09_rb,5.42752e-09_rb,5.67340e-09_rb,5.92810e-09_rb,6.18995e-09_rb, &
      6.46055e-09_rb,6.73905e-09_rb,7.02620e-09_rb,7.32260e-09_rb,7.62700e-09_rb, &
      7.94050e-09_rb,8.26370e-09_rb,8.59515e-09_rb,8.93570e-09_rb,9.28535e-09_rb, &
      9.64575e-09_rb,1.00154e-08_rb,1.03944e-08_rb,1.07839e-08_rb,1.11832e-08_rb, &
      1.15909e-08_rb,1.20085e-08_rb,1.24399e-08_rb,1.28792e-08_rb,1.33280e-08_rb, &
      1.37892e-08_rb,1.42573e-08_rb,1.47408e-08_rb,1.52345e-08_rb,1.57371e-08_rb/)
      totplnkderiv(51:100, 9) = (/ &
      1.62496e-08_rb,1.67756e-08_rb,1.73101e-08_rb,1.78596e-08_rb,1.84161e-08_rb, &
      1.89869e-08_rb,1.95681e-08_rb,2.01632e-08_rb,2.07626e-08_rb,2.13800e-08_rb, &
      2.20064e-08_rb,2.26453e-08_rb,2.32970e-08_rb,2.39595e-08_rb,2.46340e-08_rb, &
      2.53152e-08_rb,2.60158e-08_rb,2.67235e-08_rb,2.74471e-08_rb,2.81776e-08_rb, &
      2.89233e-08_rb,2.96822e-08_rb,3.04488e-08_rb,3.12298e-08_rb,3.20273e-08_rb, &
      3.28304e-08_rb,3.36455e-08_rb,3.44765e-08_rb,3.53195e-08_rb,3.61705e-08_rb, &
      3.70385e-08_rb,3.79155e-08_rb,3.88065e-08_rb,3.97055e-08_rb,4.06210e-08_rb, &
      4.15490e-08_rb,4.24825e-08_rb,4.34355e-08_rb,4.43920e-08_rb,4.53705e-08_rb, &
      4.63560e-08_rb,4.73565e-08_rb,4.83655e-08_rb,4.93815e-08_rb,5.04180e-08_rb, &
      5.14655e-08_rb,5.25175e-08_rb,5.35865e-08_rb,5.46720e-08_rb,5.57670e-08_rb/)
      totplnkderiv(101:150, 9) = (/ &
      5.68640e-08_rb,5.79825e-08_rb,5.91140e-08_rb,6.02515e-08_rb,6.13985e-08_rb, &
      6.25525e-08_rb,6.37420e-08_rb,6.49220e-08_rb,6.61145e-08_rb,6.73185e-08_rb, &
      6.85520e-08_rb,6.97760e-08_rb,7.10050e-08_rb,7.22650e-08_rb,7.35315e-08_rb, &
      7.48035e-08_rb,7.60745e-08_rb,7.73740e-08_rb,7.86870e-08_rb,7.99845e-08_rb, &
      8.13325e-08_rb,8.26615e-08_rb,8.40010e-08_rb,8.53640e-08_rb,8.67235e-08_rb, &
      8.80960e-08_rb,8.95055e-08_rb,9.08945e-08_rb,9.23045e-08_rb,9.37100e-08_rb, &
      9.51555e-08_rb,9.65630e-08_rb,9.80235e-08_rb,9.94920e-08_rb,1.00966e-07_rb, &
      1.02434e-07_rb,1.03898e-07_rb,1.05386e-07_rb,1.06905e-07_rb,1.08418e-07_rb, &
      1.09926e-07_rb,1.11454e-07_rb,1.13010e-07_rb,1.14546e-07_rb,1.16106e-07_rb, &
      1.17652e-07_rb,1.19264e-07_rb,1.20817e-07_rb,1.22395e-07_rb,1.24024e-07_rb/)
      totplnkderiv(151:181, 9) = (/ &
      1.25585e-07_rb,1.27213e-07_rb,1.28817e-07_rb,1.30472e-07_rb,1.32088e-07_rb, &
      1.33752e-07_rb,1.35367e-07_rb,1.37018e-07_rb,1.38698e-07_rb,1.40394e-07_rb, &
      1.42026e-07_rb,1.43796e-07_rb,1.45438e-07_rb,1.47175e-07_rb,1.48866e-07_rb, &
      1.50576e-07_rb,1.52281e-07_rb,1.54018e-07_rb,1.55796e-07_rb,1.57515e-07_rb, &
      1.59225e-07_rb,1.60989e-07_rb,1.62754e-07_rb,1.64532e-07_rb,1.66285e-07_rb, &
      1.68070e-07_rb,1.69870e-07_rb,1.71625e-07_rb,1.73440e-07_rb,1.75275e-07_rb, &
      1.77040e-07_rb/)
      totplnkderiv(1:50,10) = (/ &
      7.14917e-10_rb,7.64833e-10_rb,8.17460e-10_rb,8.72980e-10_rb,9.31380e-10_rb, &
      9.92940e-10_rb,1.05746e-09_rb,1.12555e-09_rb,1.19684e-09_rb,1.27162e-09_rb, &
      1.35001e-09_rb,1.43229e-09_rb,1.51815e-09_rb,1.60831e-09_rb,1.70271e-09_rb, &
      1.80088e-09_rb,1.90365e-09_rb,2.01075e-09_rb,2.12261e-09_rb,2.23924e-09_rb, &
      2.36057e-09_rb,2.48681e-09_rb,2.61814e-09_rb,2.75506e-09_rb,2.89692e-09_rb, &
      3.04423e-09_rb,3.19758e-09_rb,3.35681e-09_rb,3.52113e-09_rb,3.69280e-09_rb, &
      3.86919e-09_rb,4.05205e-09_rb,4.24184e-09_rb,4.43877e-09_rb,4.64134e-09_rb, &
      4.85088e-09_rb,5.06670e-09_rb,5.29143e-09_rb,5.52205e-09_rb,5.75980e-09_rb, &
      6.00550e-09_rb,6.25840e-09_rb,6.51855e-09_rb,6.78800e-09_rb,7.06435e-09_rb, &
      7.34935e-09_rb,7.64220e-09_rb,7.94470e-09_rb,8.25340e-09_rb,8.57030e-09_rb/)
      totplnkderiv(51:100,10) = (/ &
      8.89680e-09_rb,9.23255e-09_rb,9.57770e-09_rb,9.93045e-09_rb,1.02932e-08_rb, &
      1.06649e-08_rb,1.10443e-08_rb,1.14348e-08_rb,1.18350e-08_rb,1.22463e-08_rb, &
      1.26679e-08_rb,1.30949e-08_rb,1.35358e-08_rb,1.39824e-08_rb,1.44425e-08_rb, &
      1.49126e-08_rb,1.53884e-08_rb,1.58826e-08_rb,1.63808e-08_rb,1.68974e-08_rb, &
      1.74159e-08_rb,1.79447e-08_rb,1.84886e-08_rb,1.90456e-08_rb,1.96124e-08_rb, &
      2.01863e-08_rb,2.07737e-08_rb,2.13720e-08_rb,2.19837e-08_rb,2.26044e-08_rb, &
      2.32396e-08_rb,2.38856e-08_rb,2.45344e-08_rb,2.52055e-08_rb,2.58791e-08_rb, &
      2.65706e-08_rb,2.72758e-08_rb,2.79852e-08_rb,2.87201e-08_rb,2.94518e-08_rb, &
      3.02063e-08_rb,3.09651e-08_rb,3.17357e-08_rb,3.25235e-08_rb,3.33215e-08_rb, &
      3.41285e-08_rb,3.49485e-08_rb,3.57925e-08_rb,3.66330e-08_rb,3.74765e-08_rb/)
      totplnkderiv(101:150,10) = (/ &
      3.83675e-08_rb,3.92390e-08_rb,4.01330e-08_rb,4.10340e-08_rb,4.19585e-08_rb, &
      4.28815e-08_rb,4.38210e-08_rb,4.47770e-08_rb,4.57575e-08_rb,4.67325e-08_rb, &
      4.77170e-08_rb,4.87205e-08_rb,4.97410e-08_rb,5.07620e-08_rb,5.18180e-08_rb, &
      5.28540e-08_rb,5.39260e-08_rb,5.50035e-08_rb,5.60885e-08_rb,5.71900e-08_rb, &
      5.82940e-08_rb,5.94380e-08_rb,6.05690e-08_rb,6.17185e-08_rb,6.28860e-08_rb, &
      6.40670e-08_rb,6.52300e-08_rb,6.64225e-08_rb,6.76485e-08_rb,6.88715e-08_rb, &
      7.00750e-08_rb,7.13760e-08_rb,7.25910e-08_rb,7.38860e-08_rb,7.51290e-08_rb, &
      7.64420e-08_rb,7.77550e-08_rb,7.90725e-08_rb,8.03825e-08_rb,8.17330e-08_rb, &
      8.30810e-08_rb,8.44330e-08_rb,8.57720e-08_rb,8.72115e-08_rb,8.85800e-08_rb, &
      8.99945e-08_rb,9.13905e-08_rb,9.28345e-08_rb,9.42665e-08_rb,9.56765e-08_rb/)
      totplnkderiv(151:181,10) = (/ &
      9.72000e-08_rb,9.86780e-08_rb,1.00105e-07_rb,1.01616e-07_rb,1.03078e-07_rb, &
      1.04610e-07_rb,1.06154e-07_rb,1.07639e-07_rb,1.09242e-07_rb,1.10804e-07_rb, &
      1.12384e-07_rb,1.13871e-07_rb,1.15478e-07_rb,1.17066e-07_rb,1.18703e-07_rb, &
      1.20294e-07_rb,1.21930e-07_rb,1.23543e-07_rb,1.25169e-07_rb,1.26806e-07_rb, &
      1.28503e-07_rb,1.30233e-07_rb,1.31834e-07_rb,1.33596e-07_rb,1.35283e-07_rb, &
      1.36947e-07_rb,1.38594e-07_rb,1.40362e-07_rb,1.42131e-07_rb,1.43823e-07_rb, &
      1.45592e-07_rb/)
      totplnkderiv(1:50,11) = (/ &
      2.25919e-10_rb,2.43810e-10_rb,2.62866e-10_rb,2.83125e-10_rb,3.04676e-10_rb, &
      3.27536e-10_rb,3.51796e-10_rb,3.77498e-10_rb,4.04714e-10_rb,4.33528e-10_rb, &
      4.64000e-10_rb,4.96185e-10_rb,5.30165e-10_rb,5.65999e-10_rb,6.03749e-10_rb, &
      6.43579e-10_rb,6.85479e-10_rb,7.29517e-10_rb,7.75810e-10_rb,8.24440e-10_rb, &
      8.75520e-10_rb,9.29065e-10_rb,9.85175e-10_rb,1.04405e-09_rb,1.10562e-09_rb, &
      1.17005e-09_rb,1.23742e-09_rb,1.30780e-09_rb,1.38141e-09_rb,1.45809e-09_rb, &
      1.53825e-09_rb,1.62177e-09_rb,1.70884e-09_rb,1.79942e-09_rb,1.89390e-09_rb, &
      1.99205e-09_rb,2.09429e-09_rb,2.20030e-09_rb,2.31077e-09_rb,2.42510e-09_rb, &
      2.54410e-09_rb,2.66754e-09_rb,2.79529e-09_rb,2.92777e-09_rb,3.06498e-09_rb, &
      3.20691e-09_rb,3.35450e-09_rb,3.50653e-09_rb,3.66427e-09_rb,3.82723e-09_rb/)
      totplnkderiv(51:100,11) = (/ &
      3.99549e-09_rb,4.16911e-09_rb,4.34892e-09_rb,4.53415e-09_rb,4.72504e-09_rb, &
      4.92197e-09_rb,5.12525e-09_rb,5.33485e-09_rb,5.55085e-09_rb,5.77275e-09_rb, &
      6.00105e-09_rb,6.23650e-09_rb,6.47855e-09_rb,6.72735e-09_rb,6.98325e-09_rb, &
      7.24695e-09_rb,7.51730e-09_rb,7.79480e-09_rb,8.07975e-09_rb,8.37170e-09_rb, &
      8.67195e-09_rb,8.98050e-09_rb,9.29575e-09_rb,9.61950e-09_rb,9.95150e-09_rb, &
      1.02912e-08_rb,1.06397e-08_rb,1.09964e-08_rb,1.13611e-08_rb,1.17348e-08_rb, &
      1.21158e-08_rb,1.25072e-08_rb,1.29079e-08_rb,1.33159e-08_rb,1.37342e-08_rb, &
      1.41599e-08_rb,1.45966e-08_rb,1.50438e-08_rb,1.54964e-08_rb,1.59605e-08_rb, &
      1.64337e-08_rb,1.69189e-08_rb,1.74134e-08_rb,1.79136e-08_rb,1.84272e-08_rb, &
      1.89502e-08_rb,1.94845e-08_rb,2.00248e-08_rb,2.05788e-08_rb,2.11455e-08_rb/)
      totplnkderiv(101:150,11) = (/ &
      2.17159e-08_rb,2.23036e-08_rb,2.28983e-08_rb,2.35033e-08_rb,2.41204e-08_rb, &
      2.47485e-08_rb,2.53860e-08_rb,2.60331e-08_rb,2.66891e-08_rb,2.73644e-08_rb, &
      2.80440e-08_rb,2.87361e-08_rb,2.94412e-08_rb,3.01560e-08_rb,3.08805e-08_rb, &
      3.16195e-08_rb,3.23690e-08_rb,3.31285e-08_rb,3.39015e-08_rb,3.46820e-08_rb, &
      3.54770e-08_rb,3.62805e-08_rb,3.70960e-08_rb,3.79295e-08_rb,3.87715e-08_rb, &
      3.96185e-08_rb,4.04860e-08_rb,4.13600e-08_rb,4.22500e-08_rb,4.31490e-08_rb, &
      4.40610e-08_rb,4.49810e-08_rb,4.59205e-08_rb,4.68650e-08_rb,4.78260e-08_rb, &
      4.87970e-08_rb,4.97790e-08_rb,5.07645e-08_rb,5.17730e-08_rb,5.27960e-08_rb, &
      5.38285e-08_rb,5.48650e-08_rb,5.59205e-08_rb,5.69960e-08_rb,5.80690e-08_rb, &
      5.91570e-08_rb,6.02640e-08_rb,6.13750e-08_rb,6.25015e-08_rb,6.36475e-08_rb/)
      totplnkderiv(151:181,11) = (/ &
      6.47950e-08_rb,6.59510e-08_rb,6.71345e-08_rb,6.83175e-08_rb,6.95250e-08_rb, &
      7.07325e-08_rb,7.19490e-08_rb,7.31880e-08_rb,7.44315e-08_rb,7.56880e-08_rb, &
      7.69500e-08_rb,7.82495e-08_rb,7.95330e-08_rb,8.08450e-08_rb,8.21535e-08_rb, &
      8.34860e-08_rb,8.48330e-08_rb,8.61795e-08_rb,8.75480e-08_rb,8.89235e-08_rb, &
      9.03060e-08_rb,9.17045e-08_rb,9.31140e-08_rb,9.45240e-08_rb,9.59720e-08_rb, &
      9.74140e-08_rb,9.88825e-08_rb,1.00347e-07_rb,1.01825e-07_rb,1.03305e-07_rb, &
      1.04826e-07_rb/)
      totplnkderiv(1:50,12) = (/ &
      2.91689e-11_rb,3.20300e-11_rb,3.51272e-11_rb,3.84803e-11_rb,4.21014e-11_rb, &
      4.60107e-11_rb,5.02265e-11_rb,5.47685e-11_rb,5.96564e-11_rb,6.49111e-11_rb, &
      7.05522e-11_rb,7.66060e-11_rb,8.30974e-11_rb,9.00441e-11_rb,9.74820e-11_rb, &
      1.05435e-10_rb,1.13925e-10_rb,1.22981e-10_rb,1.32640e-10_rb,1.42933e-10_rb, &
      1.53882e-10_rb,1.65527e-10_rb,1.77903e-10_rb,1.91054e-10_rb,2.05001e-10_rb, &
      2.19779e-10_rb,2.35448e-10_rb,2.52042e-10_rb,2.69565e-10_rb,2.88128e-10_rb, &
      3.07714e-10_rb,3.28370e-10_rb,3.50238e-10_rb,3.73235e-10_rb,3.97433e-10_rb, &
      4.22964e-10_rb,4.49822e-10_rb,4.78042e-10_rb,5.07721e-10_rb,5.38915e-10_rb, &
      5.71610e-10_rb,6.05916e-10_rb,6.41896e-10_rb,6.79600e-10_rb,7.19110e-10_rb, &
      7.60455e-10_rb,8.03625e-10_rb,8.48870e-10_rb,8.96080e-10_rb,9.45490e-10_rb/)
      totplnkderiv(51:100,12) = (/ &
      9.96930e-10_rb,1.05071e-09_rb,1.10679e-09_rb,1.16521e-09_rb,1.22617e-09_rb, &
      1.28945e-09_rb,1.35554e-09_rb,1.42427e-09_rb,1.49574e-09_rb,1.56984e-09_rb, &
      1.64695e-09_rb,1.72715e-09_rb,1.81034e-09_rb,1.89656e-09_rb,1.98613e-09_rb, &
      2.07898e-09_rb,2.17515e-09_rb,2.27498e-09_rb,2.37826e-09_rb,2.48517e-09_rb, &
      2.59566e-09_rb,2.71004e-09_rb,2.82834e-09_rb,2.95078e-09_rb,3.07686e-09_rb, &
      3.20739e-09_rb,3.34232e-09_rb,3.48162e-09_rb,3.62515e-09_rb,3.77337e-09_rb, &
      3.92614e-09_rb,4.08317e-09_rb,4.24567e-09_rb,4.41272e-09_rb,4.58524e-09_rb, &
      4.76245e-09_rb,4.94450e-09_rb,5.13235e-09_rb,5.32535e-09_rb,5.52415e-09_rb, &
      5.72770e-09_rb,5.93815e-09_rb,6.15315e-09_rb,6.37525e-09_rb,6.60175e-09_rb, &
      6.83485e-09_rb,7.07490e-09_rb,7.32060e-09_rb,7.57225e-09_rb,7.83035e-09_rb/)
      totplnkderiv(101:150,12) = (/ &
      8.09580e-09_rb,8.36620e-09_rb,8.64410e-09_rb,8.93110e-09_rb,9.22170e-09_rb, &
      9.52055e-09_rb,9.82595e-09_rb,1.01399e-08_rb,1.04613e-08_rb,1.07878e-08_rb, &
      1.11223e-08_rb,1.14667e-08_rb,1.18152e-08_rb,1.21748e-08_rb,1.25410e-08_rb, &
      1.29147e-08_rb,1.32948e-08_rb,1.36858e-08_rb,1.40827e-08_rb,1.44908e-08_rb, &
      1.49040e-08_rb,1.53284e-08_rb,1.57610e-08_rb,1.61995e-08_rb,1.66483e-08_rb, &
      1.71068e-08_rb,1.75714e-08_rb,1.80464e-08_rb,1.85337e-08_rb,1.90249e-08_rb, &
      1.95309e-08_rb,2.00407e-08_rb,2.05333e-08_rb,2.10929e-08_rb,2.16346e-08_rb, &
      2.21829e-08_rb,2.27402e-08_rb,2.33112e-08_rb,2.38922e-08_rb,2.44802e-08_rb, &
      2.50762e-08_rb,2.56896e-08_rb,2.63057e-08_rb,2.69318e-08_rb,2.75705e-08_rb, &
      2.82216e-08_rb,2.88787e-08_rb,2.95505e-08_rb,3.02335e-08_rb,3.09215e-08_rb/)
      totplnkderiv(151:181,12) = (/ &
      3.16235e-08_rb,3.23350e-08_rb,3.30590e-08_rb,3.37960e-08_rb,3.45395e-08_rb, &
      3.52955e-08_rb,3.60615e-08_rb,3.68350e-08_rb,3.76265e-08_rb,3.84255e-08_rb, &
      3.92400e-08_rb,4.00485e-08_rb,4.08940e-08_rb,4.17310e-08_rb,4.25860e-08_rb, &
      4.34585e-08_rb,4.43270e-08_rb,4.52220e-08_rb,4.61225e-08_rb,4.70345e-08_rb, &
      4.79560e-08_rb,4.89000e-08_rb,4.98445e-08_rb,5.07985e-08_rb,5.17705e-08_rb, &
      5.27575e-08_rb,5.37420e-08_rb,5.47495e-08_rb,5.57725e-08_rb,5.68105e-08_rb, &
      5.78395e-08_rb/)
      totplnkderiv(1:50,13) = (/ &
      5.47482e-12_rb,6.09637e-12_rb,6.77874e-12_rb,7.52703e-12_rb,8.34784e-12_rb, &
      9.24486e-12_rb,1.02246e-11_rb,1.12956e-11_rb,1.24615e-11_rb,1.37321e-11_rb, &
      1.51131e-11_rb,1.66129e-11_rb,1.82416e-11_rb,2.00072e-11_rb,2.19187e-11_rb, &
      2.39828e-11_rb,2.62171e-11_rb,2.86290e-11_rb,3.12283e-11_rb,3.40276e-11_rb, &
      3.70433e-11_rb,4.02847e-11_rb,4.37738e-11_rb,4.75070e-11_rb,5.15119e-11_rb, &
      5.58120e-11_rb,6.04059e-11_rb,6.53208e-11_rb,7.05774e-11_rb,7.61935e-11_rb, &
      8.21832e-11_rb,8.85570e-11_rb,9.53575e-11_rb,1.02592e-10_rb,1.10298e-10_rb, &
      1.18470e-10_rb,1.27161e-10_rb,1.36381e-10_rb,1.46161e-10_rb,1.56529e-10_rb, &
      1.67521e-10_rb,1.79142e-10_rb,1.91423e-10_rb,2.04405e-10_rb,2.18123e-10_rb, &
      2.32608e-10_rb,2.47889e-10_rb,2.63994e-10_rb,2.80978e-10_rb,2.98843e-10_rb/)
      totplnkderiv(51:100,13) = (/ &
      3.17659e-10_rb,3.37423e-10_rb,3.58206e-10_rb,3.80090e-10_rb,4.02996e-10_rb, &
      4.27065e-10_rb,4.52298e-10_rb,4.78781e-10_rb,5.06493e-10_rb,5.35576e-10_rb, &
      5.65942e-10_rb,5.97761e-10_rb,6.31007e-10_rb,6.65740e-10_rb,7.02095e-10_rb, &
      7.39945e-10_rb,7.79575e-10_rb,8.20845e-10_rb,8.63870e-10_rb,9.08680e-10_rb, &
      9.55385e-10_rb,1.00416e-09_rb,1.05464e-09_rb,1.10737e-09_rb,1.16225e-09_rb, &
      1.21918e-09_rb,1.27827e-09_rb,1.33988e-09_rb,1.40370e-09_rb,1.46994e-09_rb, &
      1.53850e-09_rb,1.60993e-09_rb,1.68382e-09_rb,1.76039e-09_rb,1.83997e-09_rb, &
      1.92182e-09_rb,2.00686e-09_rb,2.09511e-09_rb,2.18620e-09_rb,2.28034e-09_rb, &
      2.37753e-09_rb,2.47805e-09_rb,2.58193e-09_rb,2.68935e-09_rb,2.80064e-09_rb, &
      2.91493e-09_rb,3.03271e-09_rb,3.15474e-09_rb,3.27987e-09_rb,3.40936e-09_rb/)
      totplnkderiv(101:150,13) = (/ &
      3.54277e-09_rb,3.68019e-09_rb,3.82173e-09_rb,3.96703e-09_rb,4.11746e-09_rb, &
      4.27104e-09_rb,4.43020e-09_rb,4.59395e-09_rb,4.76060e-09_rb,4.93430e-09_rb, &
      5.11085e-09_rb,5.29280e-09_rb,5.48055e-09_rb,5.67300e-09_rb,5.86950e-09_rb, &
      6.07160e-09_rb,6.28015e-09_rb,6.49295e-09_rb,6.71195e-09_rb,6.93455e-09_rb, &
      7.16470e-09_rb,7.39985e-09_rb,7.64120e-09_rb,7.88885e-09_rb,8.13910e-09_rb, &
      8.39930e-09_rb,8.66535e-09_rb,8.93600e-09_rb,9.21445e-09_rb,9.49865e-09_rb, &
      9.78845e-09_rb,1.00856e-08_rb,1.04361e-08_rb,1.07018e-08_rb,1.10164e-08_rb, &
      1.13438e-08_rb,1.16748e-08_rb,1.20133e-08_rb,1.23575e-08_rb,1.27117e-08_rb, &
      1.30708e-08_rb,1.34383e-08_rb,1.38138e-08_rb,1.41985e-08_rb,1.45859e-08_rb, &
      1.49846e-08_rb,1.53879e-08_rb,1.58042e-08_rb,1.62239e-08_rb,1.66529e-08_rb/)
      totplnkderiv(151:181,13) = (/ &
      1.70954e-08_rb,1.75422e-08_rb,1.79943e-08_rb,1.84537e-08_rb,1.89280e-08_rb, &
      1.94078e-08_rb,1.98997e-08_rb,2.03948e-08_rb,2.08956e-08_rb,2.14169e-08_rb, &
      2.19330e-08_rb,2.24773e-08_rb,2.30085e-08_rb,2.35676e-08_rb,2.41237e-08_rb, &
      2.46919e-08_rb,2.52720e-08_rb,2.58575e-08_rb,2.64578e-08_rb,2.70675e-08_rb, &
      2.76878e-08_rb,2.83034e-08_rb,2.89430e-08_rb,2.95980e-08_rb,3.02480e-08_rb, &
      3.09105e-08_rb,3.15980e-08_rb,3.22865e-08_rb,3.29755e-08_rb,3.36775e-08_rb, &
      3.43990e-08_rb/)
      totplnkderiv(1:50,14) = (/ &
      1.81489e-12_rb,2.03846e-12_rb,2.28659e-12_rb,2.56071e-12_rb,2.86352e-12_rb, &
      3.19789e-12_rb,3.56668e-12_rb,3.97211e-12_rb,4.41711e-12_rb,4.90616e-12_rb, &
      5.44153e-12_rb,6.02790e-12_rb,6.67001e-12_rb,7.37018e-12_rb,8.13433e-12_rb, &
      8.96872e-12_rb,9.87526e-12_rb,1.08601e-11_rb,1.19328e-11_rb,1.30938e-11_rb, &
      1.43548e-11_rb,1.57182e-11_rb,1.71916e-11_rb,1.87875e-11_rb,2.05091e-11_rb, &
      2.23652e-11_rb,2.43627e-11_rb,2.65190e-11_rb,2.88354e-11_rb,3.13224e-11_rb, &
      3.39926e-11_rb,3.68664e-11_rb,3.99372e-11_rb,4.32309e-11_rb,4.67496e-11_rb, &
      5.05182e-11_rb,5.45350e-11_rb,5.88268e-11_rb,6.34126e-11_rb,6.82878e-11_rb, &
      7.34973e-11_rb,7.90201e-11_rb,8.49075e-11_rb,9.11725e-11_rb,9.78235e-11_rb, &
      1.04856e-10_rb,1.12342e-10_rb,1.20278e-10_rb,1.28680e-10_rb,1.37560e-10_rb/)
      totplnkderiv(51:100,14) = (/ &
      1.46953e-10_rb,1.56900e-10_rb,1.67401e-10_rb,1.78498e-10_rb,1.90161e-10_rb, &
      2.02523e-10_rb,2.15535e-10_rb,2.29239e-10_rb,2.43665e-10_rb,2.58799e-10_rb, &
      2.74767e-10_rb,2.91522e-10_rb,3.09141e-10_rb,3.27625e-10_rb,3.47011e-10_rb, &
      3.67419e-10_rb,3.88720e-10_rb,4.11066e-10_rb,4.34522e-10_rb,4.59002e-10_rb, &
      4.84657e-10_rb,5.11391e-10_rb,5.39524e-10_rb,5.68709e-10_rb,5.99240e-10_rb, &
      6.31295e-10_rb,6.64520e-10_rb,6.99200e-10_rb,7.35525e-10_rb,7.73135e-10_rb, &
      8.12440e-10_rb,8.53275e-10_rb,8.95930e-10_rb,9.40165e-10_rb,9.86260e-10_rb, &
      1.03423e-09_rb,1.08385e-09_rb,1.13567e-09_rb,1.18916e-09_rb,1.24469e-09_rb, &
      1.30262e-09_rb,1.36268e-09_rb,1.42479e-09_rb,1.48904e-09_rb,1.55557e-09_rb, &
      1.62478e-09_rb,1.69642e-09_rb,1.77023e-09_rb,1.84696e-09_rb,1.92646e-09_rb/)
      totplnkderiv(101:150,14) = (/ &
      2.00831e-09_rb,2.09299e-09_rb,2.18007e-09_rb,2.27093e-09_rb,2.36398e-09_rb, &
      2.46020e-09_rb,2.55985e-09_rb,2.66230e-09_rb,2.76795e-09_rb,2.87667e-09_rb, &
      2.98971e-09_rb,3.10539e-09_rb,3.22462e-09_rb,3.34779e-09_rb,3.47403e-09_rb, &
      3.60419e-09_rb,3.73905e-09_rb,3.87658e-09_rb,4.01844e-09_rb,4.16535e-09_rb, &
      4.31470e-09_rb,4.46880e-09_rb,4.62765e-09_rb,4.78970e-09_rb,4.95735e-09_rb, &
      5.12890e-09_rb,5.30430e-09_rb,5.48595e-09_rb,5.67010e-09_rb,5.86145e-09_rb, &
      6.05740e-09_rb,6.25725e-09_rb,6.46205e-09_rb,6.67130e-09_rb,6.88885e-09_rb, &
      7.10845e-09_rb,7.33450e-09_rb,7.56700e-09_rb,7.80440e-09_rb,8.04465e-09_rb, &
      8.29340e-09_rb,8.54820e-09_rb,8.80790e-09_rb,9.07195e-09_rb,9.34605e-09_rb, &
      9.62005e-09_rb,9.90685e-09_rb,1.01939e-08_rb,1.04938e-08_rb,1.07957e-08_rb/)
      totplnkderiv(151:181,14) = (/ &
      1.11059e-08_rb,1.14208e-08_rb,1.17447e-08_rb,1.20717e-08_rb,1.24088e-08_rb, &
      1.27490e-08_rb,1.31020e-08_rb,1.34601e-08_rb,1.38231e-08_rb,1.41966e-08_rb, &
      1.45767e-08_rb,1.49570e-08_rb,1.53503e-08_rb,1.57496e-08_rb,1.61663e-08_rb, &
      1.65784e-08_rb,1.70027e-08_rb,1.74290e-08_rb,1.78730e-08_rb,1.83235e-08_rb, &
      1.87810e-08_rb,1.92418e-08_rb,1.97121e-08_rb,2.01899e-08_rb,2.05787e-08_rb, &
      2.11784e-08_rb,2.16824e-08_rb,2.21931e-08_rb,2.27235e-08_rb,2.32526e-08_rb, &
      2.37850e-08_rb/)
      totplnkderiv(1:50,15) = (/ &
      5.39905e-13_rb,6.11835e-13_rb,6.92224e-13_rb,7.81886e-13_rb,8.81851e-13_rb, &
      9.93072e-13_rb,1.11659e-12_rb,1.25364e-12_rb,1.40562e-12_rb,1.57359e-12_rb, &
      1.75937e-12_rb,1.96449e-12_rb,2.19026e-12_rb,2.43892e-12_rb,2.71249e-12_rb, &
      3.01233e-12_rb,3.34163e-12_rb,3.70251e-12_rb,4.09728e-12_rb,4.52885e-12_rb, &
      4.99939e-12_rb,5.51242e-12_rb,6.07256e-12_rb,6.68167e-12_rb,7.34274e-12_rb, &
      8.06178e-12_rb,8.84185e-12_rb,9.68684e-12_rb,1.06020e-11_rb,1.15909e-11_rb, &
      1.26610e-11_rb,1.38158e-11_rb,1.50620e-11_rb,1.64047e-11_rb,1.78508e-11_rb, &
      1.94055e-11_rb,2.10805e-11_rb,2.28753e-11_rb,2.48000e-11_rb,2.68699e-11_rb, &
      2.90824e-11_rb,3.14526e-11_rb,3.39882e-11_rb,3.67020e-11_rb,3.95914e-11_rb, &
      4.26870e-11_rb,4.59824e-11_rb,4.94926e-11_rb,5.32302e-11_rb,5.72117e-11_rb/)
      totplnkderiv(51:100,15) = (/ &
      6.14475e-11_rb,6.59483e-11_rb,7.07393e-11_rb,7.57999e-11_rb,8.11980e-11_rb, &
      8.68920e-11_rb,9.29390e-11_rb,9.93335e-11_rb,1.06101e-10_rb,1.13263e-10_rb, &
      1.20827e-10_rb,1.28819e-10_rb,1.37255e-10_rb,1.46163e-10_rb,1.55547e-10_rb, &
      1.65428e-10_rb,1.75837e-10_rb,1.86816e-10_rb,1.98337e-10_rb,2.10476e-10_rb, &
      2.23218e-10_rb,2.36600e-10_rb,2.50651e-10_rb,2.65425e-10_rb,2.80895e-10_rb, &
      2.97102e-10_rb,3.14100e-10_rb,3.31919e-10_rb,3.50568e-10_rb,3.70064e-10_rb, &
      3.90464e-10_rb,4.11813e-10_rb,4.34111e-10_rb,4.57421e-10_rb,4.81717e-10_rb, &
      5.07039e-10_rb,5.33569e-10_rb,5.61137e-10_rb,5.89975e-10_rb,6.19980e-10_rb, &
      6.51170e-10_rb,6.83650e-10_rb,7.17520e-10_rb,7.52735e-10_rb,7.89390e-10_rb, &
      8.27355e-10_rb,8.66945e-10_rb,9.08020e-10_rb,9.50665e-10_rb,9.95055e-10_rb/)
      totplnkderiv(101:150,15) = (/ &
      1.04101e-09_rb,1.08864e-09_rb,1.13823e-09_rb,1.18923e-09_rb,1.24257e-09_rb, &
      1.29741e-09_rb,1.35442e-09_rb,1.41347e-09_rb,1.47447e-09_rb,1.53767e-09_rb, &
      1.60322e-09_rb,1.67063e-09_rb,1.74033e-09_rb,1.81256e-09_rb,1.88704e-09_rb, &
      1.96404e-09_rb,2.04329e-09_rb,2.12531e-09_rb,2.21032e-09_rb,2.29757e-09_rb, &
      2.38739e-09_rb,2.48075e-09_rb,2.57628e-09_rb,2.67481e-09_rb,2.77627e-09_rb, &
      2.88100e-09_rb,2.98862e-09_rb,3.09946e-09_rb,3.21390e-09_rb,3.33105e-09_rb, &
      3.45185e-09_rb,3.57599e-09_rb,3.70370e-09_rb,3.83512e-09_rb,3.96909e-09_rb, &
      4.10872e-09_rb,4.25070e-09_rb,4.39605e-09_rb,4.54670e-09_rb,4.70015e-09_rb, &
      4.85850e-09_rb,5.02050e-09_rb,5.18655e-09_rb,5.35815e-09_rb,5.53180e-09_rb, &
      5.71225e-09_rb,5.89495e-09_rb,6.08260e-09_rb,6.27485e-09_rb,6.47345e-09_rb/)
      totplnkderiv(151:181,15) = (/ &
      6.67520e-09_rb,6.88310e-09_rb,7.09400e-09_rb,7.31140e-09_rb,7.53350e-09_rb, &
      7.76040e-09_rb,7.99215e-09_rb,8.22850e-09_rb,8.47235e-09_rb,8.71975e-09_rb, &
      8.97360e-09_rb,9.23365e-09_rb,9.49950e-09_rb,9.76965e-09_rb,1.00441e-08_rb, &
      1.03270e-08_rb,1.06158e-08_rb,1.09112e-08_rb,1.12111e-08_rb,1.15172e-08_rb, &
      1.18263e-08_rb,1.21475e-08_rb,1.24735e-08_rb,1.28027e-08_rb,1.32023e-08_rb, &
      1.34877e-08_rb,1.38399e-08_rb,1.42000e-08_rb,1.45625e-08_rb,1.49339e-08_rb, &
      1.53156e-08_rb/)
      totplnkderiv(1:50,16) = (/ &
      4.38799e-14_rb,5.04835e-14_rb,5.79773e-14_rb,6.64627e-14_rb,7.60706e-14_rb, &
      8.69213e-14_rb,9.91554e-14_rb,1.12932e-13_rb,1.28419e-13_rb,1.45809e-13_rb, &
      1.65298e-13_rb,1.87109e-13_rb,2.11503e-13_rb,2.38724e-13_rb,2.69058e-13_rb, &
      3.02878e-13_rb,3.40423e-13_rb,3.82128e-13_rb,4.28390e-13_rb,4.79625e-13_rb, &
      5.36292e-13_rb,5.98933e-13_rb,6.68066e-13_rb,7.44216e-13_rb,8.28159e-13_rb, &
      9.20431e-13_rb,1.02180e-12_rb,1.13307e-12_rb,1.25504e-12_rb,1.38863e-12_rb, &
      1.53481e-12_rb,1.69447e-12_rb,1.86896e-12_rb,2.05903e-12_rb,2.26637e-12_rb, &
      2.49193e-12_rb,2.73736e-12_rb,3.00416e-12_rb,3.29393e-12_rb,3.60781e-12_rb, &
      3.94805e-12_rb,4.31675e-12_rb,4.71543e-12_rb,5.14627e-12_rb,5.61226e-12_rb, &
      6.11456e-12_rb,6.65585e-12_rb,7.23969e-12_rb,7.86811e-12_rb,8.54456e-12_rb/)
      totplnkderiv(51:100,16) = (/ &
      9.27075e-12_rb,1.00516e-11_rb,1.08898e-11_rb,1.17884e-11_rb,1.27514e-11_rb, &
      1.37839e-11_rb,1.48893e-11_rb,1.60716e-11_rb,1.73333e-11_rb,1.86849e-11_rb, &
      2.01237e-11_rb,2.16610e-11_rb,2.33001e-11_rb,2.50440e-11_rb,2.69035e-11_rb, &
      2.88827e-11_rb,3.09881e-11_rb,3.32234e-11_rb,3.55981e-11_rb,3.81193e-11_rb, &
      4.07946e-11_rb,4.36376e-11_rb,4.66485e-11_rb,4.98318e-11_rb,5.32080e-11_rb, &
      5.67754e-11_rb,6.05524e-11_rb,6.45450e-11_rb,6.87639e-11_rb,7.32160e-11_rb, &
      7.79170e-11_rb,8.28780e-11_rb,8.81045e-11_rb,9.36200e-11_rb,9.94280e-11_rb, &
      1.05545e-10_rb,1.11982e-10_rb,1.18752e-10_rb,1.25866e-10_rb,1.33350e-10_rb, &
      1.41210e-10_rb,1.49469e-10_rb,1.58143e-10_rb,1.67233e-10_rb,1.76760e-10_rb, &
      1.86758e-10_rb,1.97236e-10_rb,2.08227e-10_rb,2.19723e-10_rb,2.31737e-10_rb/)
      totplnkderiv(101:150,16) = (/ &
      2.44329e-10_rb,2.57503e-10_rb,2.71267e-10_rb,2.85647e-10_rb,3.00706e-10_rb, &
      3.16391e-10_rb,3.32807e-10_rb,3.49887e-10_rb,3.67748e-10_rb,3.86369e-10_rb, &
      4.05746e-10_rb,4.25984e-10_rb,4.47060e-10_rb,4.68993e-10_rb,4.91860e-10_rb, &
      5.15601e-10_rb,5.40365e-10_rb,5.66085e-10_rb,5.92855e-10_rb,6.20640e-10_rb, &
      6.49605e-10_rb,6.79585e-10_rb,7.10710e-10_rb,7.43145e-10_rb,7.76805e-10_rb, &
      8.11625e-10_rb,8.47800e-10_rb,8.85300e-10_rb,9.24220e-10_rb,9.64550e-10_rb, &
      1.00623e-09_rb,1.04957e-09_rb,1.09429e-09_rb,1.14079e-09_rb,1.18882e-09_rb, &
      1.23848e-09_rb,1.28986e-09_rb,1.34301e-09_rb,1.39796e-09_rb,1.45493e-09_rb, &
      1.51372e-09_rb,1.57440e-09_rb,1.63702e-09_rb,1.70173e-09_rb,1.76874e-09_rb, &
      1.83753e-09_rb,1.90898e-09_rb,1.98250e-09_rb,2.05836e-09_rb,2.13646e-09_rb/)
      totplnkderiv(151:181,16) = (/ &
      2.21710e-09_rb,2.30027e-09_rb,2.38591e-09_rb,2.47432e-09_rb,2.56503e-09_rb, &
      2.65878e-09_rb,2.75516e-09_rb,2.85432e-09_rb,2.95688e-09_rb,3.06201e-09_rb, &
      3.17023e-09_rb,3.28153e-09_rb,3.39604e-09_rb,3.51391e-09_rb,3.63517e-09_rb, &
      3.75955e-09_rb,3.88756e-09_rb,4.01880e-09_rb,4.15405e-09_rb,4.29255e-09_rb, &
      4.43535e-09_rb,4.58145e-09_rb,4.73165e-09_rb,4.88560e-09_rb,5.04390e-09_rb, &
      5.20630e-09_rb,5.37255e-09_rb,5.54355e-09_rb,5.71915e-09_rb,5.89855e-09_rb, &
      6.08280e-09_rb/)
      totplk16deriv(1:50) = (/ &
      4.35811e-14_rb,5.01270e-14_rb,5.75531e-14_rb,6.59588e-14_rb,7.54735e-14_rb, &
      8.62147e-14_rb,9.83225e-14_rb,1.11951e-13_rb,1.27266e-13_rb,1.44456e-13_rb, &
      1.63715e-13_rb,1.85257e-13_rb,2.09343e-13_rb,2.36209e-13_rb,2.66136e-13_rb, &
      2.99486e-13_rb,3.36493e-13_rb,3.77582e-13_rb,4.23146e-13_rb,4.73578e-13_rb, &
      5.29332e-13_rb,5.90936e-13_rb,6.58891e-13_rb,7.33710e-13_rb,8.16135e-13_rb, &
      9.06705e-13_rb,1.00614e-12_rb,1.11524e-12_rb,1.23477e-12_rb,1.36561e-12_rb, &
      1.50871e-12_rb,1.66488e-12_rb,1.83552e-12_rb,2.02123e-12_rb,2.22375e-12_rb, &
      2.44389e-12_rb,2.68329e-12_rb,2.94338e-12_rb,3.22570e-12_rb,3.53129e-12_rb, &
      3.86236e-12_rb,4.22086e-12_rb,4.60827e-12_rb,5.02666e-12_rb,5.47890e-12_rb, &
      5.96595e-12_rb,6.49057e-12_rb,7.05592e-12_rb,7.66401e-12_rb,8.31821e-12_rb/)
      totplk16deriv(51:100) = (/ &
      9.01998e-12_rb,9.77390e-12_rb,1.05826e-11_rb,1.14491e-11_rb,1.23769e-11_rb, &
      1.33709e-11_rb,1.44341e-11_rb,1.55706e-11_rb,1.67821e-11_rb,1.80793e-11_rb, &
      1.94586e-11_rb,2.09316e-11_rb,2.25007e-11_rb,2.41685e-11_rb,2.59454e-11_rb, &
      2.78356e-11_rb,2.98440e-11_rb,3.19744e-11_rb,3.42355e-11_rb,3.66340e-11_rb, &
      3.91772e-11_rb,4.18773e-11_rb,4.47339e-11_rb,4.77509e-11_rb,5.09490e-11_rb, &
      5.43240e-11_rb,5.78943e-11_rb,6.16648e-11_rb,6.56445e-11_rb,6.98412e-11_rb, &
      7.42680e-11_rb,7.89335e-11_rb,8.38450e-11_rb,8.90220e-11_rb,9.44695e-11_rb, &
      1.00197e-10_rb,1.06221e-10_rb,1.12550e-10_rb,1.19193e-10_rb,1.26175e-10_rb, &
      1.33498e-10_rb,1.41188e-10_rb,1.49251e-10_rb,1.57693e-10_rb,1.66530e-10_rb, &
      1.75798e-10_rb,1.85495e-10_rb,1.95661e-10_rb,2.06275e-10_rb,2.17357e-10_rb/)
      totplk16deriv(101:150) = (/ &
      2.28959e-10_rb,2.41085e-10_rb,2.53739e-10_rb,2.66944e-10_rb,2.80755e-10_rb, &
      2.95121e-10_rb,3.10141e-10_rb,3.25748e-10_rb,3.42057e-10_rb,3.59026e-10_rb, &
      3.76668e-10_rb,3.95066e-10_rb,4.14211e-10_rb,4.34111e-10_rb,4.54818e-10_rb, &
      4.76295e-10_rb,4.98681e-10_rb,5.21884e-10_rb,5.46000e-10_rb,5.71015e-10_rb, &
      5.97065e-10_rb,6.23965e-10_rb,6.51865e-10_rb,6.80905e-10_rb,7.11005e-10_rb, &
      7.42100e-10_rb,7.74350e-10_rb,8.07745e-10_rb,8.42355e-10_rb,8.78185e-10_rb, &
      9.15130e-10_rb,9.53520e-10_rb,9.93075e-10_rb,1.03415e-09_rb,1.07649e-09_rb, &
      1.12021e-09_rb,1.16539e-09_rb,1.21207e-09_rb,1.26025e-09_rb,1.31014e-09_rb, &
      1.36156e-09_rb,1.41453e-09_rb,1.46909e-09_rb,1.52540e-09_rb,1.58368e-09_rb, &
      1.64334e-09_rb,1.70527e-09_rb,1.76888e-09_rb,1.83442e-09_rb,1.90182e-09_rb/)
      totplk16deriv(151:181) = (/ &
      1.97128e-09_rb,2.04281e-09_rb,2.11635e-09_rb,2.19219e-09_rb,2.26979e-09_rb, &
      2.34989e-09_rb,2.43219e-09_rb,2.51660e-09_rb,2.60396e-09_rb,2.69317e-09_rb, &
      2.78501e-09_rb,2.87927e-09_rb,2.97600e-09_rb,3.07548e-09_rb,3.17772e-09_rb, &
      3.28235e-09_rb,3.38982e-09_rb,3.49985e-09_rb,3.61307e-09_rb,3.72883e-09_rb, &
      3.84805e-09_rb,3.96975e-09_rb,4.09465e-09_rb,4.22240e-09_rb,4.35370e-09_rb, &
      4.48800e-09_rb,4.62535e-09_rb,4.76640e-09_rb,4.91110e-09_rb,5.05850e-09_rb, &
      5.20965e-09_rb/)

      end subroutine lwavplankderiv

      end module rrtmg_lw_setcoef
