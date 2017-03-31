!     path:      $Source$
!     author:    $Author: miacono $
!     revision:  $Revision: 30822 $
!     created:   $Date: 2016-12-29 15:53:24 -0500 (Thu, 29 Dec 2016) $
!

      program rrtmg_sw

!----------------------------------------------------------------------------
! Copyright (c) 2002-2016, Atmospheric & Environmental Research, Inc. (AER)
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!  * Redistributions of source code must retain the above copyright
!    notice, this list of conditions and the following disclaimer.
!  * Redistributions in binary form must reproduce the above copyright
!    notice, this list of conditions and the following disclaimer in the
!    documentation and/or other materials provided with the distribution.
!  * Neither the name of Atmospheric & Environmental Research, Inc., nor
!    the names of its contributors may be used to endorse or promote products
!    derived from this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
! ARE DISCLAIMED. IN NO EVENT SHALL ATMOSPHERIC & ENVIRONMENTAL RESEARCH, INC., 
! BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
! THE POSSIBILITY OF SUCH DAMAGE.
!                        (http://www.rtweb.aer.com/)                        
!----------------------------------------------------------------------------
!
! ****************************************************************************
! *                                                                          *
! *                             RRTMG_SW                                     *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                 a rapid radiative transfer model                         *
! *                  for the solar spectral region                           *
! *           for application to general circulation models                  *
! *                                                                          *
! *                                                                          *
! *           Atmospheric and Environmental Research, Inc.                   *
! *                       131 Hartwell Avenue                                *
! *                       Lexington, MA 02421                                *
! *                                                                          *
! *                                                                          *
! *                          Eli J. Mlawer                                   *
! *                       Jennifer S. Delamere                               *
! *                        Michael J. Iacono                                 *
! *                        Shepard A. Clough                                 *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                      email:  miacono@aer.com                             *
! *                      email:  emlawer@aer.com                             *
! *                      email:  jdelamer@aer.com                            *
! *                                                                          *
! *       The authors wish to acknowledge the contributions of the           *
! *       following people: Steven J. Taubman, Patrick D. Brown,             *
! *       Ronald E. Farren, Luke Chen, Robert Bergstrom.                     *
! *                                                                          *
! ****************************************************************************

! ------- Description -------

! This program is the driver for RRTMG_SW, the AER SW radiation model for 
!  application to GCMs, that has been adapted from RRTM_SW for improved
!  efficiency and to provide fractional cloudiness and cloud overlap
!  capability using McICA.

! This routine
!    a) calls RRTMG_SW_INI to initialize data and to perform
!       the g-point interval reduction from 224 to 112
!    b) calls READPROF to read in the atmospheric profile;
!       all layering in RRTMG is ordered from surface to toa. 
!    c) calls CLDPROP to set cloud optical depth based on input
!       cloud properties, or CLDPRMC to set cloud optical depth
!       for McICA
!    d) calls SETCOEF to calculate various quantities needed for 
!       the radiative transfer algorithm
!    e) calls SPCVRT to call the two-stream model that in turn 
!       calls TAUMOL to calculate gaseous optical depths for each 
!       of the 16 spectral bands and to perform the radiative transfer
!       with or without McICA, the Monte-Carlo Independent Column
!       Approximation to represent sub-grid scale cloud variability
!    f) writes out the calculated fluxes and cooling rates
!
! Two modes of operation are possible:
!     The mode is chosen by setting flag imca below.  
!
!    1) Standard, single forward model calculation (imca = 0); this is 
!       valid only for clear sky or fully overcast clouds
!    2) Monte Carlo Independent Column Approximation (McICA, Pincus et al., 
!       JC, 2003) method is applied to the forward model calculation (imca = 1)
!       For single column calculations, this method also requires setting flag
!       nmca below to the sample size of the Monte Carlo calculation; 
!       (nmca = 200 is recommended). This is method is valid for clear sky
!       or partial cloud conditions
!
! Two random number generators are available for use when imca = 1
!     This is chosen by setting flag irng below.
!
!    1) KISSVEC (irng = 0)
!    2) Mersenne Twister (irng = 1); the default setting
!
! Two methods of cloud property input are possible:
!     Cloud properties can be input in one of two ways (controlled by input 
!     flags inflag, iceflag and liqflag; see text file rrtmg_sw_instructions
!     and subroutine rrtmg_sw_cldprop.f90 for further details):
!
!    1) Input cloud fraction and cloud optical depth directly (inflgsw = 0)
!    2) Input cloud fraction and cloud physical properties (inflgsw = 1 or 2);  
!       cloud optical properties are calculated by cldprop or cldprmc based
!       on input settings of iceflgsw and liqflgsw. Ice particle size provided
!       must be appropriately defined for the ice parameterization selected. 
!
! Two methods of aerosol property input are possible:
!     Aerosol properties can be input in one of two ways (controlled by input 
!     flag iaer, see text file rrtmg_sw_instructions for further details):
!
!    1) Input aerosol optical depth, single scattering albedo and asymmetry
!       parameter directly by layer and spectral band (iaer=10)
!    2) Input aerosol optical depth and 0.55 micron directly by layer and use
!       one or more of six ECMWF aerosol types (iaer=6)
!
!
! ------- Modifications -------
!
! This version of RRTMG_SW has been modified from RRTM_SW to use a reduced
! set of g-point intervals and a two-stream model for application to GCMs. 
!
!-- Original version (derived from RRTM_SW)
!     2002: AER. Inc.
!-- Conversion to F90 formatting; addition of 2-stream radiative transfer
!     Feb 2003: J.-J. Morcrette, ECMWF
!-- Additional modifications for GCM application
!     Aug 2003: M. J. Iacono, AER Inc.
!-- Total number of g-points reduced from 224 to 112.  Original
!   set of 224 can be restored by exchanging code in module parrrsw.f90 
!   and in file rrtmg_sw_init.f90.
!     Apr 2004: M. J. Iacono, AER, Inc.
!-- Modifications to include output for direct and diffuse 
!   downward fluxes.  There are output as "true" fluxes without
!   any delta scaling applied.  Code can be commented to exclude
!   this calculation in source file rrtmg_sw_spcvrt.f90.
!     Jan 2005: E. J. Mlawer, M. J. Iacono, AER, Inc.
!-- Revised to add McICA capability.
!     Nov 2005: M. J. Iacono, AER, Inc.
!-- Reformatted for consistency with rrtmg_lw.
!     Feb 2007: M. J. Iacono, AER, Inc.
!-- Modified to output direct and diffuse fluxes either with or without
!   delta scaling based on setting of idelm flag. 
!     Dec 2008: M. J. Iacono, AER, Inc.
!-- Revised to add new solar variability options based on the
!   NRLSSI2 solar model
!     Dec 2016: M. J. Iacono, AER

! --------- Modules ---------

      use parkind, only : im => kind_im, rb => kind_rb
      use parrrsw, only : mxlay, nbndsw, ngptsw, naerec, nstr, nmol, mxmol, &
                          jpband, jpb1, jpb2
      use rrsw_aer, only : rsrtaua, rsrpiza, rsrasya
      use rrsw_con, only : heatfac, oneminus, pi
      use rrsw_wvn, only : wavenum1, wavenum2
      use rrsw_vsn
      use mcica_subcol_gen_sw, only: mcica_subcol_sw
      use rrtmg_sw_cldprop, only: cldprop_sw
      use rrtmg_sw_cldprmc, only: cldprmc_sw
      use rrtmg_sw_init, only: rrtmg_sw_ini
      use rrtmg_sw_setcoef, only: setcoef_sw
      use rrtmg_sw_spcvrt, only: spcvrt_sw
      use rrtmg_sw_spcvmc, only: spcvmc_sw

      implicit none

! ------- Declarations

! ----- Local -----

! Control
      integer(kind=im) :: nlayers             ! total number of layers
      integer(kind=im) :: istart              ! beginning band of calculation
      integer(kind=im) :: iend                ! ending band of calculation
      integer(kind=im) :: icld                ! clear/cloud and cloud overlap flag
      integer(kind=im) :: icpr                ! cldprop/cldprmc use flag
      integer(kind=im) :: iflag               ! control flag
      integer(kind=im) :: iout                ! output option flag
      integer(kind=im) :: iaer                ! aerosol option flag
      integer(kind=im) :: idelm               ! delta-m scaling flag
                                              ! [0 = direct and diffuse fluxes are unscaled]
                                              ! [1 = direct and diffuse fluxes are scaled]
      integer(kind=im) :: isccos              ! instrumental cosine response flag
      integer(kind=im) :: ird                 ! input unit
      integer(kind=im) :: iwr                 ! output unit
      integer(kind=im) :: i                   ! layer loop index                      ! jk
      integer(kind=im) :: ib                  ! band loop index                       ! jsw
      integer(kind=im) :: ia, ig              ! indices
      integer(kind=im) :: iplon               ! column loop index                     ! jl
      integer(kind=im) :: ims                 ! mcica statistical loop index
      integer(kind=im) :: imca                ! flag for mcica [0=off, 1=on]
      integer(kind=im) :: nmca                ! number of mcica samples (mcica mode)
      integer(kind=im) :: irng                ! flag for random number generator
                                              ! [0=kissvec, 1=mersenne twister (default)]
      integer(kind=im), parameter :: ncol = 1 ! total number of columns

      integer(kind=im) :: iout1, iout2        ! output control flags
      integer(kind=im) :: indform             ! output control flag
      character page 
      character*50 outform(7)

      real(kind=rb) :: zepsec, zepzen         ! epsilon
      real(kind=rb) :: zdpgcp                 ! flux to heating conversion ratio


! Atmosphere
      real(kind=rb) :: pavel(mxlay)           ! layer pressures (mb) 
      real(kind=rb) :: tavel(mxlay)           ! layer temperatures (K)
      real(kind=rb) :: pz(0:mxlay)            ! level (interface) pressures (hPa, mb)
      real(kind=rb) :: tz(0:mxlay)            ! level (interface) temperatures (K)
      real(kind=rb) :: tbound                 ! surface temperature (K)
      real(kind=rb) :: pdp(mxlay)             ! layer pressure thickness (hPa, mb)
      real(kind=rb) :: coldry(mxlay)          ! 
      real(kind=rb) :: wbrodl(mxlay)          !
      real(kind=rb) :: wkl(mxmol,mxlay)       ! molecular amounts (mol/cm-2)

      real(kind=rb) :: cossza, zenith         ! cosine of solar zenith angle 
!      real(kind=rb) :: earth_sun              ! function for Earth/Sun distance factor
      real(kind=rb) :: adjflux(jpband)        ! adjustment for current Earth/Sun distance
      real(kind=rb) :: solvar(jpband)         ! solar constant scaling factor from rrtmg_sw
                                              !  default value of 1368.22 Wm-2 at 1 AU
      real(kind=rb) :: semiss(jpband)         ! surface emissivity
      real(kind=rb) :: albdir(nbndsw)         ! surface albedo, direct          ! zalbp
      real(kind=rb) :: albdif(nbndsw)         ! surface albedo, diffuse         ! zalbd

      real(kind=rb) :: tauaer(mxlay,jpband)   ! aerosol optical depth (iaer=10 only)
                                              ! (non-delta scaled)      
      real(kind=rb) :: ssaaer(mxlay,jpband)   ! aerosol single scattering albedo (iaer=10 only)
                                              ! (non-delta scaled)      
      real(kind=rb) :: asmaer(mxlay,jpband)   ! aerosol asymmetry parameter (iaer=10 only)
                                              ! (non-delta scaled)      
                                              !   first moment of input phase function
      real(kind=rb) :: ecaer(mxlay,naerec)    ! aerosol optical thickness at 0.55 micron (iaer=6 only)
                                              ! (non-delta scaled)      

! Atmosphere - setcoef
      integer(kind=im) :: laytrop             ! tropopause layer index
      integer(kind=im) :: layswtch            ! tropopause layer index
      integer(kind=im) :: laylow              ! tropopause layer index
      integer(kind=im) :: jp(mxlay)           ! 
      integer(kind=im) :: jt(mxlay)           !
      integer(kind=im) :: jt1(mxlay)          !

      real(kind=rb) :: colh2o(mxlay)          ! column amount (h2o)
      real(kind=rb) :: colco2(mxlay)          ! column amount (co2)
      real(kind=rb) :: colo3(mxlay)           ! column amount (o3)
      real(kind=rb) :: coln2o(mxlay)          ! column amount (n2o)
      real(kind=rb) :: colch4(mxlay)          ! column amount (ch4)
      real(kind=rb) :: colo2(mxlay)           ! column amount (o2)
      real(kind=rb) :: colmol(mxlay)          ! column amount
      real(kind=rb) :: co2mult(mxlay)         ! column amount 

      integer(kind=im) :: indself(mxlay)
      integer(kind=im) :: indfor(mxlay)
      real(kind=rb) :: selffac(mxlay)
      real(kind=rb) :: selffrac(mxlay)
      real(kind=rb) :: forfac(mxlay)
      real(kind=rb) :: forfrac(mxlay)

      real(kind=rb) :: &                      !
                         fac00(mxlay), fac01(mxlay), &
                         fac10(mxlay), fac11(mxlay) 

! Atmosphere/clouds - cldprop
      integer(kind=im) :: ncbands             ! number of cloud spectral bands
      integer(kind=im) :: inflag              ! flag for cloud property method
      integer(kind=im) :: iceflag             ! flag for ice cloud properties
      integer(kind=im) :: liqflag             ! flag for liquid cloud properties

      real(kind=rb) :: cldfrac(mxlay)         ! layer cloud fraction
      real(kind=rb) :: tauc(nbndsw,mxlay)     ! in-cloud optical depth (non-delta scaled)
      real(kind=rb) :: ssac(nbndsw,mxlay)     ! in-cloud single scattering albedo (non-delta scaled)
      real(kind=rb) :: asmc(nbndsw,mxlay)     ! in-cloud asymmetry parameter (non-delta scaled)
      real(kind=rb) :: fsfc(nbndsw,mxlay)     ! in-cloud forward scattering fraction (non-delta scaled)
      real(kind=rb) :: ciwp(mxlay)            ! in-cloud ice water path
      real(kind=rb) :: clwp(mxlay)            ! in-cloud liquid water path
      real(kind=rb) :: rei(mxlay)             ! cloud ice particle effective size (microns)
                                              ! specific definition of rei depends on setting of iceflag:
                                              ! iceflag = 0: ice effective radius, r_ec, (Ebert and Curry, 1992),
                                              !              r_ec must be >= 10.0 microns
                                              ! iceflag = 1: ice effective radius, r_ec, (Ebert and Curry, 1992),
                                              !              r_ec range is limited to 13.0 to 130.0 microns
                                              ! iceflag = 2: ice effective radius, r_k, (Key, Streamer Ref. Manual, 1996)
                                              !              r_k range is limited to 5.0 to 131.0 microns
                                              ! iceflag = 3: generalized effective size, dge, (Fu, 1996),
                                              !              dge range is limited to 5.0 to 140.0 microns
                                              !              [dge = 1.0315 * r_ec]
      real(kind=rb) :: rel(mxlay)             ! cloud liquid particle effective radius (microns)

      real(kind=rb) :: taucloud(mxlay,jpband) ! in-cloud optical depth
      real(kind=rb) :: taucldorig(mxlay,jpband)! in-cloud optical depth (non-delta scaled)
      real(kind=rb) :: ssacloud(mxlay,jpband) ! in-cloud single scattering albedo
      real(kind=rb) :: asmcloud(mxlay,jpband) ! in-cloud asymmetry parameter

! Atmosphere/clouds - cldprmc [mcica]
      real(kind=rb) :: cldfmc(ngptsw,mxlay)   ! cloud fraction [mcica]
      real(kind=rb) :: ciwpmc(ngptsw,mxlay)   ! in-cloud ice water path [mcica]
      real(kind=rb) :: clwpmc(ngptsw,mxlay)   ! in-cloud liquid water path [mcica]
      real(kind=rb) :: relqmc(mxlay)          ! liquid particle effective radius (microns)
      real(kind=rb) :: reicmc(mxlay)          ! ice particle effective radius (microns)
      real(kind=rb) :: taucmc(ngptsw,mxlay)   ! in-cloud optical depth [mcica]
      real(kind=rb) :: taormc(ngptsw,mxlay)   ! unscaled in-cloud optical depth [mcica]
      real(kind=rb) :: ssacmc(ngptsw,mxlay)   ! in-cloud single scattering albedo [mcica]
      real(kind=rb) :: asmcmc(ngptsw,mxlay)   ! in-cloud asymmetry parameter [mcica]
      real(kind=rb) :: fsfcmc(ngptsw,mxlay)   ! in-cloud forward scattering fraction [mcica]

! Atmosphere/clouds/aerosol - spcvrt,spcvmc
      real(kind=rb) :: ztauc(mxlay,nbndsw)    ! cloud optical depth
      real(kind=rb) :: ztaucorig(mxlay,nbndsw)! unscaled cloud optical depth
      real(kind=rb) :: zasyc(mxlay,nbndsw)    ! cloud asymmetry parameter 
                                              !  (first moment of phase function)
      real(kind=rb) :: zomgc(mxlay,nbndsw)    ! cloud single scattering albedo
      real(kind=rb) :: ztaua(mxlay,nbndsw)    ! total aerosol optical depth
      real(kind=rb) :: zasya(mxlay,nbndsw)    ! total aerosol asymmetry parameter 
      real(kind=rb) :: zomga(mxlay,nbndsw)    ! total aerosol single scattering albedo

      real(kind=rb) :: zcldfmc(mxlay,ngptsw)  ! cloud fraction [mcica]
      real(kind=rb) :: ztaucmc(mxlay,ngptsw)  ! cloud optical depth [mcica]
      real(kind=rb) :: ztaormc(mxlay,ngptsw)  ! unscaled cloud optical depth [mcica]
      real(kind=rb) :: zasycmc(mxlay,ngptsw)  ! cloud asymmetry parameter [mcica] 
      real(kind=rb) :: zomgcmc(mxlay,ngptsw)  ! cloud single scattering albedo [mcica]

      real(kind=rb) :: zbbfu(mxlay+1)         ! temporary upward shortwave flux (w/m2)
      real(kind=rb) :: zbbfd(mxlay+1)         ! temporary downward shortwave flux (w/m2)
      real(kind=rb) :: zbbcu(mxlay+1)         ! temporary clear sky upward shortwave flux (w/m2)
      real(kind=rb) :: zbbcd(mxlay+1)         ! temporary clear sky downward shortwave flux (w/m2)
      real(kind=rb) :: zbbfddir(mxlay+1)      ! temporary downward direct shortwave flux (w/m2)
      real(kind=rb) :: zbbcddir(mxlay+1)      ! temporary clear sky downward direct shortwave flux (w/m2)
      real(kind=rb) :: zuvfd(mxlay+1)         ! temporary UV downward shortwave flux (w/m2)
      real(kind=rb) :: zuvcd(mxlay+1)         ! temporary clear sky UV downward shortwave flux (w/m2)
      real(kind=rb) :: zuvfddir(mxlay+1)      ! temporary UV downward direct shortwave flux (w/m2)
      real(kind=rb) :: zuvcddir(mxlay+1)      ! temporary clear sky UV downward direct shortwave flux (w/m2)
      real(kind=rb) :: znifd(mxlay+1)         ! temporary near-IR downward shortwave flux (w/m2)
      real(kind=rb) :: znicd(mxlay+1)         ! temporary clear sky near-IR downward shortwave flux (w/m2)
      real(kind=rb) :: znifddir(mxlay+1)      ! temporary near-IR downward direct shortwave flux (w/m2)
      real(kind=rb) :: znicddir(mxlay+1)      ! temporary clear sky near-IR downward direct shortwave flux (w/m2)

! Parameters
      real(kind=rb), parameter :: cpdair = 1.004e3_rb  ! Specific heat capacity of dry air
                                                       ! at constant pressure at 273 K (J kg-1 K-1)
! Output
      real(kind=rb) :: totuflux(0:mxlay)      ! upward shortwave flux (w/m2)                  ! pfup
      real(kind=rb) :: totdflux(0:mxlay)      ! downward shortwave flux (w/m2)                ! pfdown
      real(kind=rb) :: fnet(0:mxlay)          ! net shortwave flux (w/m2)                     ! pfls
      real(kind=rb) :: htr(0:mxlay)           ! shortwave heating rate (k/day)                ! pheat
      real(kind=rb) :: totuclfl(0:mxlay)      ! clear sky upward shortwave flux (w/m2)        ! pcup 
      real(kind=rb) :: totdclfl(0:mxlay)      ! clear sky downward shortwave flux (w/m2)      ! pcdown 
      real(kind=rb) :: fnetc(0:mxlay)         ! clear sky net shortwave flux (w/m2)           ! pfcs
      real(kind=rb) :: htrc(0:mxlay)          ! clear sky shortwave heating rate (k/day)      ! pheac

      real(kind=rb) :: dirdflux(0:mxlay)      ! direct downward shortwave flux (w/m2)         ! dirdownflux
      real(kind=rb) :: difdflux(0:mxlay)      ! diffuse downward shortwave flux (w/m2)        ! difdownflux
      real(kind=rb) :: dflxuv(0:mxlay)        ! Total sky downward shortwave flux, UV/vis     ! pfdnuv
      real(kind=rb) :: dflxir(0:mxlay)        ! Total sky downward shortwave flux, near-IR    ! pfdnir 
      real(kind=rb) :: dirdnuv(0:mxlay)       ! Direct downward shortwave surface flux, UV/vis
      real(kind=rb) :: difdnuv(0:mxlay)       ! Diffuse downward shortwave surface flux, UV/vis
      real(kind=rb) :: dirdnir(0:mxlay)       ! Direct downward shortwave surface flux, near-IR
      real(kind=rb) :: difdnir(0:mxlay)       ! Diffuse downward shortwave surface flux, near-IR

! Output - inactive
!      real(kind=rb) :: zuvfu(mxlay+1)         ! temporary upward UV shortwave flux (w/m2)
!      real(kind=rb) :: zuvfd(mxlay+1)         ! temporary downward UV shortwave flux (w/m2)
!      real(kind=rb) :: zuvcu(mxlay+1)         ! temporary clear sky upward UV shortwave flux (w/m2)
!      real(kind=rb) :: zuvcd(mxlay+1)         ! temporary clear sky downward UV shortwave flux (w/m2)
!      real(kind=rb) :: zvsfu(mxlay+1)         ! temporary upward visible shortwave flux (w/m2)
!      real(kind=rb) :: zvsfd(mxlay+1)         ! temporary downward visible shortwave flux (w/m2)
!      real(kind=rb) :: zvscu(mxlay+1)         ! temporary clear sky upward visible shortwave flux (w/m2)
!      real(kind=rb) :: zvscd(mxlay+1)         ! temporary clear sky downward visible shortwave flux (w/m2)
!      real(kind=rb) :: znifu(mxlay+1)         ! temporary upward near-IR shortwave flux (w/m2)
!      real(kind=rb) :: znifd(mxlay+1)         ! temporary downward near-IR shortwave flux (w/m2)
!      real(kind=rb) :: znicu(mxlay+1)         ! temporary clear sky upward near-IR shortwave flux (w/m2)
!      real(kind=rb) :: znicd(mxlay+1)         ! temporary clear sky downward near-IR shortwave flux (w/m2)

! Output (mean output for McICA calculation)
      real(kind=rb) :: uflxsum(0:mxlay)       ! upward shortwave flux (w/m2)
      real(kind=rb) :: dflxsum(0:mxlay)       ! downward shortwave flux (w/m2)
      real(kind=rb) :: dirdsum(0:mxlay)       ! direct downward shortwave flux (w/m2)
      real(kind=rb) :: difdsum(0:mxlay)       ! diffuse downward shortwave flux (w/m2)
      real(kind=rb) :: fnetsum(0:mxlay)       ! net shortwave flux (w/m2)
      real(kind=rb) :: htrsum(0:mxlay)        ! shortwave heating rate (k/day)

! Solar variability
      integer(kind=im) :: isolvar             ! Flag for solar variability method
                                              !   -1 = (when SCON .eq. 0.0): No solar variability
                                              !        and no solar cycle (Kurucz solar irradiance
                                              !        of 1368.22 Wm-2 only);
                                              !        (when SCON .ne. 0.0): Kurucz solar irradiance
                                              !        scaled to SCON and solar variability defined
                                              !        (optional) by setting non-zero scale factors
                                              !        for each band in SOLVAR
                                              !    0 = (when SCON .eq. 0.0): No solar variability 
                                              !        and no solar cycle (NRLSSI2 solar constant of 
                                              !        1360.85 Wm-2 for the 100-50000 cm-1 spectral 
                                              !        range only), with facular and sunspot effects 
                                              !        fixed to the mean of Solar Cycles 13-24;
                                              !        (when SCON .ne. 0.0): No solar variability 
                                              !        and no solar cycle (NRLSSI2 solar constant of 
                                              !        1360.85 Wm-2 for the 100-50000 cm-1 spectral 
                                              !        range only), is scaled to SCON
                                              !    1 = Solar variability (using NRLSSI2  solar
                                              !        model) with solar cycle contribution
                                              !        determined by fraction of solar cycle
                                              !        with facular and sunspot variations
                                              !        fixed to their mean variations over the
                                              !        average of Solar Cycles 13-24;
                                              !        two amplitude scale factors allow
                                              !        facular and sunspot adjustments from
                                              !        mean solar cycle as defined by solvar 
                                              !    2 = Solar variability (using NRLSSI2 solar
                                              !        model) over solar cycle determined by 
                                              !        direct specification of Mg (facular)
                                              !        and SB (sunspot) indices provided
                                              !        in solvar (scon = 0.0 only)
                                              !    3 = (when SCON .eq. 0.0): No solar variability
                                              !        and no solar cycle (NRLSSI2 solar irradiance
                                              !        of 1360.85 Wm-2 only);
                                              !        (when SCON .ne. 0.0): NRLSSI2 solar irradiance
                                              !        scaled to SCON and solar variability defined
                                              !        (optional) by setting non-zero scale factors
                                              !        for each band in SOLVAR
      real(kind=rb) :: svar_f                 ! Solar variability facular multiplier
      real(kind=rb) :: svar_s                 ! Solar variability sunspot multiplier
      real(kind=rb) :: svar_i                 ! Solar variability baseline irradiance multiplier
      real(kind=rb) :: svar_f_bnd(jpband)     ! Solar variability facular multiplier (by band)
      real(kind=rb) :: svar_s_bnd(jpband)     ! Solar variability sunspot multiplier (by band)
      real(kind=rb) :: svar_i_bnd(jpband)     ! Solar variability baseline irradiance multiplier (by band)


! Initializations

      hvrrtm = '$Revision: 30822 $'
      hvrini = 'NOT USED'
      hvrcld = 'NOT USED'
      hvrclc = 'NOT USED'
      hvrrft = 'NOT USED'
      hvrspv = 'NOT USED'
      hvrspc = 'NOT USED'
      hvrset = 'NOT USED'
      hvrtau = 'NOT USED'
      hvrvqd = 'NOT USED'
      hvrkg  = '$Revision: 30822 $'
      hvratm = 'NOT USED'
      hvrutl = 'NOT USED'
      hvrext = 'NOT USED'

      hnamrtm = '  rrtmg_sw.1col.f90:'
      hnamini = '  rrtmg_sw_init.f90:'
      hnamcld = 'rrtmgsw_cldprop.f90:'
      hnamclc = 'rrtmgsw_cldprmc.f90:'
      hnamrft = 'rrtmg_sw_reftra.f90:'
      hnamspv = 'rrtmg_sw_spcvrt.f90:'
      hnamspc = 'rrtmg_sw_spcvmc.f90:'
      hnamset = 'rrtmgsw_setcoef.f90:'
      hnamtau = 'rrtmg_sw_taumol.f90:'
      hnamvqd = 'rrtmg_sw_vrtqdr.f90:'
      hnamkg  = '   rrtmg_sw_k_g.f90:'
      hnamatm = '           rrtatm.f:'
      hnamutl = '         util_xxx.f:'
      hnamext = '            extra.f:'

      zepsec = 1.e-06_rb
      zepzen = 1.e-10_rb
      oneminus = 1.0_rb - zepsec
      pi = 2._rb * asin(1._rb)

      ird = 9
      iwr = 10
      icpr = 0
      page = char(12)

      data outform &
         /'(1x,i3,3x,f7.6,4x,4(f10.4,4x),f11.6,4x,f10.5)',&
          '(1x,i3,4x,f6.5,4x,4(f10.4,4x),f11.6,4x,f10.5)',&
          '(1x,i3,4x,f6.4,4x,4(f10.4,4x),f11.6,4x,f10.5)',&
          '(1x,i3,4x,f6.3,4x,4(f10.4,4x),f11.6,4x,f10.5)',&
          '(1x,i3,4x,f6.2,4x,4(f10.4,4x),f11.6,4x,f10.5)',&
          '(1x,i3,4x,f6.1,4x,4(f10.4,4x),f11.6,4x,f10.5)',&
          '(1x,i3,4x,f6.1,4x,4(f10.4,4x),f11.6,4x,f10.5)'/

      uflxsum(0:) = 0._rb
      dflxsum(0:) = 0._rb
      dirdsum(0:) = 0._rb
      difdsum(0:) = 0._rb
      fnetsum(0:) = 0._rb
      htrsum(0:) = 0._rb

! Set imca to select calculation type:
!  (read by subroutine readprof from input file INPUT_RRTM):  
! imca = 0, use standard forward model calculation (clear and overcast only)
! imca = 1, use McICA for Monte Carlo treatment of sub-grid cloud variability
!           (clear, overcast or partial cloud conditions)

! Set irng to select random number generator for McICA (use when imca = 1)
! irng = 0, KISSVEC
! irng = 1, Mersenne Twister
!      irng = 0
      irng = 1

! Set icld to select of clear or cloud calculation and cloud overlap method
!  (read by subroutine readprof from input file INPUT_RRTM):  
! icld = 0, clear only
! icld = 1, with clouds using random cloud overlap (McICA only)
! icld = 2, with clouds using maximum/random cloud overlap (McICA only)
! icld = 3, with clouds using maximum cloud overlap (McICA only)

! Call model and data initialization, compute lookup tables, perform
! reduction of g-points from 224 to 112 for input absorption
! coefficient data and other arrays.
!
! In a GCM this call should be placed in the model initialization
! area, since this has to be called only once.  

      call rrtmg_sw_ini(cpdair)

! Open the input set of atmospheres
      open (ird,file='INPUT_RRTM',form='formatted')
! Open the output file
      open (iwr,file='OUTPUT_RRTM',form='formatted')
      

! This is the main longitude/column loop within rrtmg.

      do iplon = 1, ncol

! Input atmospheric profile from INPUT_RRTM. 
        call readprof(ird, nlayers, iout, imca, icld, iaer, isccos, idelm, pdp, &
                       pavel, tavel, pz, tz, tbound, semiss, zenith, adjflux, &
                       coldry, wkl, inflag, iceflag, liqflag, &
                       cldfrac, tauc, ssac, asmc, fsfc, ciwp, clwp, rei, rel, &
                       tauaer, ssaaer, asmaer, &
! solar variability I/O
                       isolvar,svar_f,svar_s,svar_i, &
                       svar_f_bnd, svar_s_bnd, svar_i_bnd)

         istart = jpb1                 ! jpb1 = 16
         iend = jpb2                   ! jpb2 = 29
         iflag = iout

! Set nmca to sample size for Monte Carlo calculation
         if (imca.eq.0) nmca = 1
         if (imca.eq.1) nmca = 200

! Return here for multiple band output
 1000    continue
         if (iflag .gt. 0 .and. iflag .le. jpb2) then
            istart = iflag
            iend = iflag
         endif

! This is the statistical sampling loop for McICA

         do ims = 1, nmca

! Call sub-colum cloud generator for McICA calculations.
! Output will be averaged over all nmca samples.  The code can be modified to
! write output for each individual sample (this will be excessive if output
! is also requested for each spectral band).  

            if (imca.eq.1) then
               call mcica_subcol_sw(iplon, nlayers, icld, ims, irng, pavel, &
                          cldfrac, ciwp, clwp, rei, rel, tauc, ssac, asmc, fsfc, &
                          cldfmc, ciwpmc, clwpmc, reicmc, relqmc, taucmc, &
                          ssacmc, asmcmc, fsfcmc)
            endif

!  For cloudy atmosphere, use cldprop to set cloud optical properties based on
!  input cloud physical properties.  Select method based on choices described
!  in cldprop.  Cloud fraction, water path, liquid droplet and ice particle
!  effective radius must be passed in cldprop.  Cloud fraction and cloud
!  optical properties are transferred to rrtmg_sw arrays in cldprop.  
!  Note: Model will be stopped if partial cloud present without McICA.

!  If McICA is requested use cloud fraction and cloud physical properties 
!  generated by sub-column cloud generator above. 

            if (imca.eq.0) then
               do i = 1, nlayers
                  if (cldfrac(i).ne.0._rb .and. cldfrac(i).ne.1._rb) then
                     stop 'PARTIAL CLOUD NOT ALLOWED FOR IMCA=0'
                  endif
               enddo
               call cldprop_sw(nlayers, inflag, iceflag, liqflag, cldfrac, &
                               tauc, ssac, asmc, fsfc, ciwp, clwp, rei, rel, &
                               taucldorig, taucloud, ssacloud, asmcloud)
               icpr = 1
            else
               call cldprmc_sw(nlayers, inflag, iceflag, liqflag, cldfmc, &
                               ciwpmc, clwpmc, reicmc, relqmc, &
                               taormc, taucmc, ssacmc, asmcmc, fsfcmc)
               icpr = 1
            endif


! Calculate coefficients for the temperature and pressure dependence of the 
! molecular absorption coefficients by interpolating data from stored
! reference atmospheres.

            call setcoef_sw(nlayers, pavel, tavel, pz, tz, tbound, coldry, wkl, &
                            laytrop, layswtch, laylow, jp, jt, jt1, &
                            co2mult, colch4, colco2, colh2o, colmol, coln2o, &
                            colo2, colo3, fac00, fac01, fac10, fac11, &
                            selffac, selffrac, indself, forfac, forfrac, indfor)
  

! Cosine of the solar zenith angle 
!  Prevent using value of zero;

            cossza = zenith
            if (cossza.eq.0._rb) cossza = zepzen

! Transfer albedo, cloud and aerosol properties into arrays for 2-stream radiative transfer 
  
! Surface albedo
            do ib=1,nbndsw
               albdif(ib) = 1._rb - semiss(jpb1-1+ib)
               albdir(ib) = 1._rb - semiss(jpb1-1+ib)
            enddo

! Clouds
            if (icld.eq.0) then

               ztauc(:,:) = 0._rb
               ztaucorig(:,:) = 0._rb
               zasyc(:,:) = 0._rb
               zomgc(:,:) = 1._rb
               zcldfmc(:,:) = 0._rb
               ztaucmc(:,:) = 0._rb
               ztaormc(:,:) = 0._rb
               zasycmc(:,:) = 0._rb
               zomgcmc(:,:) = 1._rb

            elseif (icld.ge.1) then
               if (imca.eq.0) then
                  do i=1,nlayers
                     do ib=1,nbndsw
                        if (cldfrac(i) .ge. zepsec) then
                           ztauc(i,ib) = taucloud(i,jpb1-1+ib)
                           ztaucorig(i,ib) = taucldorig(i,jpb1-1+ib)
                           zasyc(i,ib) = asmcloud(i,jpb1-1+ib)
                           zomgc(i,ib) = ssacloud(i,jpb1-1+ib)
                        endif
                     enddo
                  enddo
               else
                  do i=1,nlayers
                     do ig=1,ngptsw
                        zcldfmc(i,ig) = cldfmc(ig,i)
                        ztaucmc(i,ig) = taucmc(ig,i)
                        ztaormc(i,ig) = taormc(ig,i)
                        zasycmc(i,ig) = asmcmc(ig,i)
                        zomgcmc(i,ig) = ssacmc(ig,i)
                     enddo
                  enddo
               endif   
            endif   

! Aerosol
! IAER = 0: no aerosols
            if (iaer.eq.0) then

               ztaua(:,:) = 0._rb
               zasya(:,:) = 0._rb
               zomga(:,:) = 1._rb

! IAER = 6: Use ECMWF six aerosol types. See rrsw_aer.f90 for details.
! Input aerosol optical thickness at 0.55 micron for each aerosol type (ecaer)
! or set manually here for each aerosol and layer. 
            elseif (iaer.eq.6) then

               do i = 1, nlayers
                  do ia = 1, naerec
                     ecaer(i,ia) = 1.0e-15_rb
!                      ecaer(i,ia) = 1.0e-04_rb
                  enddo
               enddo

               do i = 1, nlayers
                  do ib = 1, nbndsw
                     ztaua(i,ib) = 0._rb
                     zasya(i,ib) = 0._rb
                     zomga(i,ib) = 1._rb
                     do ia = 1, naerec
                        ztaua(i,ib) = ztaua(i,ib) + rsrtaua(ib,ia) * ecaer(i,ia)
                        zomga(i,ib) = zomga(i,ib) + rsrtaua(ib,ia) * ecaer(i,ia) * &
                                      rsrpiza(ib,ia)
                        zasya(i,ib) = zasya(i,ib) + rsrtaua(ib,ia) * ecaer(i,ia) * &
                                      rsrpiza(ib,ia) * rsrasya(ib,ia)
                     enddo
                     if (zomga(i,ib) /= 0._rb) then
                        zasya(i,ib) = zasya(i,ib) / zomga(i,ib)
                     endif
                     if (ztaua(i,ib) /= 0._rb) then
                        zomga(i,ib) = zomga(i,ib) / ztaua(i,ib)
                     endif
                  enddo
               enddo

! IAER=10: Direct specification of aerosol properties from IN_AER_RRTM.
            elseif (iaer.eq.10) then

               do i = 1 ,nlayers
                  do ib = 1 ,nbndsw
                     ztaua(i,ib) = tauaer(i,jpb1-1+ib)
                     zasya(i,ib) = asmaer(i,jpb1-1+ib)
                     zomga(i,ib) = ssaaer(i,jpb1-1+ib)
                  enddo
               enddo

            endif

! Call the 2-stream radiation transfer model

            do i=1,nlayers+1
               zbbcu(i) = 0._rb
               zbbcd(i) = 0._rb
               zbbfu(i) = 0._rb
               zbbfd(i) = 0._rb
               zbbcddir(i) = 0._rb
               zbbfddir(i) = 0._rb
               zuvcd(i) = 0._rb
               zuvfd(i) = 0._rb
               zuvcddir(i) = 0._rb
               zuvfddir(i) = 0._rb
               znicd(i) = 0._rb
               znifd(i) = 0._rb
               znicddir(i) = 0._rb
               znifddir(i) = 0._rb
            enddo

            if (imca.eq.0) then
               call spcvrt_sw &
                   (nlayers, istart, iend, icpr, idelm, iout, &
                    pavel, tavel, pz, tz, tbound, albdif, albdir, &
                    cldfrac, ztauc, zasyc, zomgc, ztaucorig, &
                    ztaua, zasya, zomga, cossza, coldry, wkl, adjflux, &	 
                    isolvar, svar_f, svar_s, svar_i, &
                    svar_f_bnd, svar_s_bnd, svar_i_bnd, &
                    laytrop, layswtch, laylow, jp, jt, jt1, &
                    co2mult, colch4, colco2, colh2o, colmol, coln2o, colo2, colo3, &
                    fac00, fac01, fac10, fac11, &
                    selffac, selffrac, indself, forfac, forfrac, indfor, &
                    zbbfd, zbbfu, zbbcd, zbbcu, zuvfd, zuvcd, znifd, znicd, &
                    zbbfddir, zbbcddir, zuvfddir, zuvcddir, znifddir, znicddir)
            else
               call spcvmc_sw &
                   (nlayers, istart, iend, icpr, idelm, iout, &
                    pavel, tavel, pz, tz, tbound, albdif, albdir, &
                    zcldfmc, ztaucmc, zasycmc, zomgcmc, ztaormc, &
                    ztaua, zasya, zomga, cossza, coldry, wkl, adjflux, &	 
                    isolvar, svar_f, svar_s, svar_i, &
                    svar_f_bnd, svar_s_bnd, svar_i_bnd, &
                    laytrop, layswtch, laylow, jp, jt, jt1, &
                    co2mult, colch4, colco2, colh2o, colmol, coln2o, colo2, colo3, &
                    fac00, fac01, fac10, fac11, &
                    selffac, selffrac, indself, forfac, forfrac, indfor, &
                    zbbfd, zbbfu, zbbcd, zbbcu, zuvfd, zuvcd, znifd, znicd, &
                    zbbfddir, zbbcddir, zuvfddir, zuvcddir, znifddir, znicddir)
            endif


! Prepare output up and down, clear and total flux output
            do i = 1, nlayers+1
               totuclfl(i-1) = zbbcu(i)
               totdclfl(i-1) = zbbcd(i)
               totuflux(i-1) = zbbfu(i)
               totdflux(i-1) = zbbfd(i)
! Prepare direct/diffuse flux output
               dirdflux(i-1) = zbbfddir(i)
               difdflux(i-1) = totdflux(i-1) - dirdflux(i-1)
            enddo

! Prepare net clear and total flux output
            do i = 1, nlayers+1
               fnetc(i-1) = totdclfl(i-1) - totuclfl(i-1)
               fnet(i-1) = totdflux(i-1) - totuflux(i-1)
            enddo

! Output clear and total heating rates
            do i = 1, nlayers
               zdpgcp = heatfac / pdp(i)
               htrc(i-1) = (fnetc(i) - fnetc(i-1)) * zdpgcp
               htr(i-1) = (fnet(i) - fnet(i-1)) * zdpgcp
            enddo
            htr(nlayers) = 0._rb
            htrc(nlayers) = 0._rb

! Process output.
            if (iout .lt. 0) goto 2000

            if (imca .eq. 0) then

               if (istart .ne. iend) then
                  iout1 = jpb2
                  iout2 = iend-1
               else
                  iout1 = istart
                  iout2 = iend
               endif

               if (isccos .eq. 1) then 
                  write(iwr,9880) 
               elseif (isccos .eq. 2) then
                  write(iwr,9881)
               else
                  write(iwr,9879)
               endif

               if (idelm .eq. 0) then
                  write(iwr,9883)
               else
                  write(iwr,9882)
               endif

               write(iwr,9899) wavenum1(iout1),wavenum2(iout2),iplon
               write(iwr,9900)
               write(iwr,9901)

               do i = nlayers, 0, -1
                  if (pz(i) .lt. 1.e-2_rb) then
                     indform = 1
                  elseif (pz(i) .lt. 1.e-1_rb) then
                     indform = 2
                  elseif (pz(i) .lt. 1._rb) then
                     indform = 3
                  elseif (pz(i) .lt. 10._rb) then
                     indform = 4
                  elseif (pz(i) .lt. 100._rb) then
                     indform = 5
                  elseif (pz(i) .lt. 1000._rb) then
                     indform = 6
                  else
                     indform = 7
                  endif
                  write(iwr,outform(indform)) i, pz(i), totuflux(i), &
                       difdflux(i), dirdflux(i), totdflux(i), fnet(i), htr(i)
               enddo
               write(iwr,9903)page

            elseif (imca .eq. 1) then

               do i = nlayers, 0, -1
                  uflxsum(i) = uflxsum(i) + totuflux(i)
                  dflxsum(i) = dflxsum(i) + totdflux(i)
                  dirdsum(i) = dirdsum(i) + dirdflux(i)
                  difdsum(i) = difdsum(i) + difdflux(i)
                  fnetsum(i) = fnetsum(i) + fnet(i)
                  htrsum(i) = htrsum(i) + htr(i)
               enddo

! Output average over samples when last sample reached. Comment this if-check 
! and swap flux output write statement below to output all McICA samples. 
               if (ims .eq. nmca) then

                  do i = nlayers, 0, -1
                     uflxsum(i) = uflxsum(i)/nmca
                     dflxsum(i) = dflxsum(i)/nmca
                     dirdsum(i) = dirdsum(i)/nmca
                     difdsum(i) = difdsum(i)/nmca
                     fnetsum(i) = fnetsum(i)/nmca
                     htrsum(i) = htrsum(i)/nmca
                  enddo

                  if (istart .ne. iend) then
                     iout1 = jpb2
                     iout2 = iend-1
                  else
                     iout1 = istart
                     iout2 = iend
                  endif

                  if (isccos .eq. 1) then 
                     write(iwr,9880) 
                  elseif (isccos .eq. 2) then
                     write(iwr,9881)
                  else
                     write(iwr,9879)
                  endif

                  if (idelm .eq. 0) then
                     write(iwr,9883)
                  else
                     write(iwr,9882)
                  endif

                  write(iwr,9899) wavenum1(iout1),wavenum2(iout2),iplon
                  write(iwr,9900)
                  write(iwr,9901)

                  do i = nlayers, 0, -1
                     if (pz(i) .lt. 1.e-2_rb) then
                        indform = 1
                     elseif (pz(i) .lt. 1.e-1_rb) then
                        indform = 2
                     elseif (pz(i) .lt. 1._rb) then
                        indform = 3
                     elseif (pz(i) .lt. 10._rb) then
                        indform = 4
                     elseif (pz(i) .lt. 100._rb) then
                        indform = 5
                     elseif (pz(i) .lt. 1000._rb) then
                        indform = 6
                     else
                        indform = 7
                     endif
                     write(iwr,outform(indform)) i, pz(i), uflxsum(i), &
                          difdsum(i), dirdsum(i), dflxsum(i), fnetsum(i), htrsum(i)
!                     write(iwr,outform(indform)) i, pz(i), totuflux(i), &
!                          difdflux(i), dirdflux(i), totdflux(i), fnet(i), htr(i)
                  enddo
                  write(iwr,9903)page

               endif

            endif

 2000       continue
! End statistical loop for McICA
         enddo

         if (iout .ge. 0 .and. iout .le. jpb2) goto 3500
         if (iflag .eq. 98) then
            iflag = jpb1
         elseif (iflag .gt. 0 .and. iflag .lt. jpb2) then
            iflag = iflag + 1
         else
            goto 3500
         endif
            goto 1000
 3500    continue

! Output module version numbers

         write(iwr,9910) hnamrtm,hvrrtm,hnamini,hvrini,hnamcld,hvrcld, &
           hnamclc,hvrclc,hnamrft,hvrrft,hnamspv,hvrspv,hnamspc,hvrspc, &
           hnamset,hvrset,hnamtau,hvrtau,hnamvqd,hvrvqd,hnamatm,hvratm, &
           hnamutl,hvrutl,hnamext,hvrext,hnamkg,hvrkg

 4000    continue

! End longitude/column loop
      enddo

      close(ird)
      close(iwr)

 9879 format(1x)
 9880 format(1x,'All output fluxes have been adjusted to account for ins&
     &trumental cosine response.') 
 9881 format(1x,'The output diffuse fluxes have been adjusted to account&
     & for instrumental cosine response.') 

 9882 format(1x,'The downwelling direct and diffuse fluxes have been com&
     &puted using the delta-M scaling approximation.') 
 9883 format(1x)

 9899 format(1x,'Wavenumbers: ',f6.0,' - ',f6.0,' cm-1, ATM ',i6)
 9900 format(1x,'LEVEL PRESSURE   UPWARD FLUX   DIFDOWN FLUX  DIRDOWN FL&
     &UX  DOWNWARD FLUX   NET FLUX    HEATING RATE')
 9901 format(1x,'         mb          W/m2          W/m2          W/m2&
     &        W/m2          W/m2       degree/day')
 9902 format(1x,i3,3x,f11.6,4x,1p,2(g12.6,2x),g13.6,3x,g16.9,0p)
 9903 format(a)
 9910 format('  Modules and versions used in this calculation:',/,/, &
              7(5x,a20,2x,a18,10x,a20,2x,a18,/))

      contains

!*************************************************************************
      real(kind=rb) function earth_sun(idn)
!*************************************************************************
!
!  Purpose: Function to calculate the correction factor of Earth's orbit
!  for current day of the year

!  idn        : Day of the year
!  earth_sun  : square of the ratio of mean to actual Earth-Sun distance

! ------- Modules -------

      use parkind, only : im => kind_im, rb => kind_rb
      use rrsw_con, only : pi

      implicit none

      integer(kind=im), intent(in) :: idn

      real(kind=rb) :: gamma

      gamma = 2._rb*pi*(idn-1)/365._rb

! Use Iqbal's equation 1.2.1

      earth_sun = 1.000110_rb + .034221_rb * cos(gamma) + .001289_rb * sin(gamma) + &
                   .000719_rb * cos(2._rb*gamma) + .000077_rb * sin(2._rb*gamma)

      end function earth_sun

!************************************************************************
      subroutine readprof(ird_in, nlayers_out, iout_out, imca, icld_out, &
           iaer_out, isccos_out, idelm_out, pdp, &
           pavel_out, tavel_out, pz_out, tz_out, tbound_out, semiss_out, &
           zenith_out, adjflux_out, &
           coldry_out, wkl_out, inflag_out, iceflag_out,liqflag_out, &
           cldfrac_out, tauc, ssac, asmc, fsfc, ciwp, clwp, rei, rel, &
           tauaer_out, ssaaer_out, asmaer_out, &
! solar variability I/O
           isolvar,svar_f_out,svar_s_out,svar_i_out, &
           svar_f_bnd_out, svar_s_bnd_out, svar_i_bnd_out)
!************************************************************************

! -------- Modules --------

      use parkind, only : im => kind_im, rb => kind_rb
      use parrrsw, only: rrsw_scon
      use rrsw_con, only: pi,planck,boltz,clight,avogad,alosmt,gascon, &
                          radcn1,radcn2
      use rrsw_wvn, only: wavenum1, wavenum2, delwave

! Note: COMMON blocks are left in this routine for array passing with
!       rrtatm.f for reading input profiles in single-column mode.
!       Scalars and arrays in subroutine call are renamed to avoid conflict
!       with COMMON blocks.
!                                                                         
! Purpose: Read in atmospheric profile.

      implicit integer(i-n), real(a-h,o-z)

! ------- Parameters -------
      parameter (mxlay = 203)
      parameter (nbndsw = 14)
      parameter (jpbands = 29)
      parameter (ib1 = 16, ib2 = 29)
      parameter (mg = 16)
      parameter (mxstr = 16)
      parameter (mcmu = 32)
      parameter (mxmol = 38)
      parameter (maxinpx = 35)
      parameter (maxxsec = 4)

      dimension altz(0:mxlay),ixtrans(14)
      dimension solvar(jpbands)

      common /control/  iaer, nstr, iout, istart, iend, icld, idelm, isccos
      common /constants/fluxfac,heatfac
      common /consts/   pic,planckc,boltzc,clightc,avogadc,alosmtc,gasconc, &
                        radcn1c,radcn2c
      common /swprop/   zenith, albedo, adjflux(jpbands)
      common /surface/  ireflect,semiss(jpbands)
      common /profile/  nlayers,pavel(mxlay),tavel(mxlay),pz(0:mxlay),tz(0:mxlay),tbound
      common /species/  coldry(mxlay),wkl(mxmol,mxlay),wbrodl(mxlay),colmol(mxlay),nmol
      common /ifil/     ird,ipr,ipu,idum(15)
      common /xsecctrl/ nxmol,ixindx(maxinpx)
      common /xsec/     wx(maxxsec,mxlay)
      common /pathx/    ixmax,nxmol0,ixindx0(maxinpx),wx0(maxinpx,mxlay)    
      common /xrrtatm/  ixsect

      common /cloudin/   inflag,clddat1(mxlay),clddat2(mxlay), &
                         iceflag,liqflag,clddat3(mxlay),clddat4(mxlay), &
                         clddatmom(0:16,mxlay)
      common /clouddat/  ncbands,cldfrac(mxlay), &
                         taucloud(mxlay,jpbands),ssacloud(mxlay,jpbands), &
                         xmom(0:16,mxlay,jpbands)
      common /aerdat/    ssaaer(mxlay,jpbands), phase(mcmu,mxlay,jpbands), &
                         tauaer(mxlay,jpbands)

      character*80 form1(0:1),form2(0:1),form3(0:1)
      character*1 ctest, cdollar, cdum
      character*3 cinp

! Dimensions for transfer to rrtmg
      integer(kind=im), intent(in) :: ird_in                 ! input file unit
      integer(kind=im), intent(out) :: nlayers_out           ! total number of layers
      integer(kind=im), intent(out) :: imca                  ! McICA on/off flag (1 = use McICA)
      integer(kind=im), intent(out) :: icld_out              ! clear/cloud/overlap flag
      integer(kind=im), intent(out) :: iout_out              ! output option flag
      integer(kind=im), intent(out) :: iaer_out              ! aerosol flag
      integer(kind=im), intent(out) :: isccos_out            ! aerosol flag
      integer(kind=im), intent(out) :: idelm_out             ! aerosol flag

      real(kind=rb), intent(out) :: pavel_out(mxlay)         ! layer pressures (mb) 
      real(kind=rb), intent(out) :: tavel_out(mxlay)         ! layer temperatures (K)
      real(kind=rb), intent(out) :: pz_out(0:mxlay)          ! level (interface) pressures (hPa, mb)
      real(kind=rb), intent(out) :: tz_out(0:mxlay)          ! level (interface) temperatures (K)
      real(kind=rb), intent(out) :: tbound_out               ! surface temperature (K)
      real(kind=rb), intent(out) :: pdp(mxlay)               ! layer pressure thickness (hPa, mb)
      real(kind=rb), intent(out) :: coldry_out(mxlay)        ! dry air molecular amount
      real(kind=rb), intent(out) :: wkl_out(mxmol,mxlay)     ! molecular amounts (mol/cm-2)
      real(kind=rb), intent(out) :: semiss_out(jpbands)       ! surface emissivity
      real(kind=rb), intent(out) :: zenith_out               ! cos solar zenith angle
      real(kind=rb), intent(out) :: adjflux_out(jpbands)      ! adjustment for current Earth/Sun distance

      integer(kind=im), intent(out) :: inflag_out              ! cloud property option flag
      integer(kind=im), intent(out) :: iceflag_out             ! ice cloud property flag
      integer(kind=im), intent(out) :: liqflag_out             ! liquid cloud property flag

      real(kind=rb), intent(out) :: cldfrac_out(mxlay)         ! cloud fraction
      real(kind=rb), intent(out) :: tauc(nbndsw,mxlay)         ! in-cloud optical depth (non-delta scaled)
      real(kind=rb), intent(out) :: ssac(nbndsw,mxlay)         ! in-cloud single scattering albedo (non-delta scaled)
      real(kind=rb), intent(out) :: asmc(nbndsw,mxlay)         ! in-cloud asymmetry parameter (non-delta scaled)
      real(kind=rb), intent(out) :: fsfc(nbndsw,mxlay)         ! in-cloud forward scattering fraction (non-delta scaled)
      real(kind=rb), intent(out) :: ciwp(mxlay)                ! in-cloud ice water path
      real(kind=rb), intent(out) :: clwp(mxlay)                ! in-cloud liquid water path
      real(kind=rb), intent(out) :: rei(mxlay)                 ! cloud ice particle size
      real(kind=rb), intent(out) :: rel(mxlay)                 ! cloud liquid particle size
      real(kind=rb), intent(out) :: tauaer_out(mxlay,jpbands)   ! aerosol optical depth
      real(kind=rb), intent(out) :: ssaaer_out(mxlay,jpbands)   ! aerosol single scattering albedo
      real(kind=rb), intent(out) :: asmaer_out(mxlay,jpbands)   ! aerosol asymmetry parameter
                                                                !   first momemnt of input phase function
! Solar variability
      integer(kind=im) :: isolvar                     ! Flag for solar variability method
                                                      !   -1 = (when SCON .eq. 0.0): No solar variability
                                                      !        and no solar cycle (Kurucz solar irradiance
                                                      !        of 1368.22 Wm-2 only);
                                                      !        (when SCON .ne. 0.0): Kurucz solar irradiance
                                                      !        scaled to SCON and solar variability defined
                                                      !        (optional) by setting non-zero scale factors
                                                      !        for each band in SOLVAR
                                                      !    0 = (when SCON .eq. 0.0): No solar variability 
                                                      !        and no solar cycle (NRLSSI2 solar constant of 
                                                      !        1360.85 Wm-2 for the 100-50000 cm-1 spectral 
                                                      !        range only), with facular and sunspot effects 
                                                      !        fixed to the mean of Solar Cycles 13-24;
                                                      !        (when SCON .ne. 0.0): No solar variability 
                                                      !        and no solar cycle (NRLSSI2 solar constant of 
                                                      !        1360.85 Wm-2 for the 100-50000 cm-1 spectral 
                                                      !        range only), is scaled to SCON
                                                      !    1 = Solar variability (using NRLSSI2  solar
                                                      !        model) with solar cycle contribution
                                                      !        determined by fraction of solar cycle
                                                      !        with facular and sunspot variations
                                                      !        fixed to their mean variations over the
                                                      !        average of Solar Cycles 13-24;
                                                      !        two amplitude scale factors allow
                                                      !        facular and sunspot adjustments from
                                                      !        mean solar cycle as defined by solvar 
                                                      !    2 = Solar variability (using NRLSSI2 solar
                                                      !        model) over solar cycle determined by 
                                                      !        direct specification of Mg (facular)
                                                      !        and SB (sunspot) indices provided
                                                      !        in solvar (scon = 0.0 only)
                                                      !    3 = (when SCON .eq. 0.0): No solar variability
                                                      !        and no solar cycle (NRLSSI2 solar irradiance
                                                      !        of 1360.85 Wm-2 only);
                                                      !        (when SCON .ne. 0.0): NRLSSI2 solar irradiance
                                                      !        scaled to SCON and solar variability defined
                                                      !        (optional) by setting non-zero scale factors
                                                      !        for each band in SOLVAR
      real(kind=rb), intent(out) :: svar_f_out                 ! Solar variability facular multiplier
      real(kind=rb), intent(out) :: svar_s_out                 ! Solar variability sunspot multiplier
      real(kind=rb), intent(out) :: svar_i_out                 ! Solar variability baseline irradiance multiplier
      real(kind=rb), intent(out) :: svar_f_bnd_out(jpbands)    ! Solar variability facular multiplier (by band)
      real(kind=rb), intent(out) :: svar_s_bnd_out(jpbands)    ! Solar variability sunspot multiplier (by band)
      real(kind=rb), intent(out) :: svar_i_bnd_out(jpbands)    ! Solar variability baseline irradiance multiplier (by band)

!
! Local
      integer(kind=im) :: juldat                               ! day of year
      real(kind=rb) :: fice(mxlay)                             ! cloud ice fraction

! Local - solar variability
      real(kind=rb) :: scon                             ! solar constant (Wm-2)
      real(kind=rb) :: svar_f                           ! Solar variability facular multiplier
      real(kind=rb) :: svar_s                           ! Solar variability sunspot multiplier
      real(kind=rb) :: svar_i                           ! Solar variability baseline irradiance multiplier
      real(kind=rb) :: svar_f_bnd(jpbands)              ! Solar variability facular multiplier (by band)
      real(kind=rb) :: svar_s_bnd(jpbands)              ! Solar variability sunspot multiplier (by band)
      real(kind=rb) :: svar_i_bnd(jpbands)              ! Solar variability baseline irradiance multiplier (by band)
      real(kind=rb) :: bndsolvar(nbndsw)                ! Band scale factors for modeling spectral 
                                                        ! variation of solar cycle for each shortwave band;
                                                        ! for Kurucz solar constant (isolvar=-1), or
                                                        ! averaged NRLSSI2 model solar cycle (isolvar=3)
                                                        !    Dimensions: (nbndsw=14)
      real(kind=rb) :: indsolvar(2)                     ! Mg and SB index amplitude scale factors (isolvar=1), or
                                                        ! Mg and SB indices as defined in the NRLSSI2 model (isolvar=2)
                                                        !    Dimensions: (2)
      real(kind=rb) :: solcycfrac                       ! Fraction of averaged solar cycle (0-1)
                                                        !    at current time (isolvar=1)
      real(kind=rb) :: wgt                              ! Weighting factor for amplitude scale factor adjustment
      real(kind=rb) :: svar_f_0, svar_s_0               ! Solar variability indices for current fractional
                                                        !  position in typical solar cycle, interpolated
                                                        !  from lookup table of values over solar cycle
      real(kind=rb) :: svar_cprim                       ! Solar variability intermediate value
      real(kind=rb) :: svar_r                           ! Solar variability intermediate value
      integer(kind=im) :: sfid                          ! Solar variability solar cycle fraction index
      real(kind=rb) :: tmp_f_0, tmp_s_0                 ! Solar variability temporary quantities
      real(kind=rb) :: fraclo, frachi, intfrac          ! Solar variability interpolation factors

! Mean quiet sun, facular brightening, and sunspot dimming coefficient terms (NRLSSI2, 100-50000 cm-1), 
! spectrally integrated (from hi-res values after mapping to g-point space)
      real(kind=rb), parameter :: Iint = 1360.37_rb     ! Solar quiet sun irradiance term, integrated
      real(kind=rb), parameter :: Fint = 0.996047_rb    ! Solar facular brightening term (index-offset), integrated
      real(kind=rb), parameter :: Sint = -0.511590_rb   ! Solar sunspot dimming term (index-offset), integrated
      real(kind=rb), parameter :: Foffset = 0.14959542_rb    ! Solar facular offset
      real(kind=rb), parameter :: Soffset = 0.00066696_rb    ! Solar sunspot offset

! Mg and SB indices for average solar cycle integrated over solar cycle
      real(kind=rb), parameter :: svar_f_avg = 0.1568113_rb  ! Solar variability NRLSSI2 Mg "Bremen" index 
                                                             !  time-averaged over Solar Cycles 13-24
                                                             !  and averaged over solar cycle
      real(kind=rb), parameter :: svar_s_avg = 909.21910_rb  ! Solar variability NRLSSI2 SB "SPOT67" index 
                                                             !  time-averaged over Solar Cycles 13-24
                                                             !  and averaged over solar cycle
      integer(kind=im), parameter :: nsolfrac = 132     ! Number of elements in solar arrays (12 months
                                                             !  per year over 11-year solar cycle)
      real(kind=rb) :: nsfm1_inv                        ! Inverse of (nsolfrac-1)

! Mg and SB index look-up tables for average solar cycle as a function of solar cycle
      real(kind=rb) :: mgavgcyc(nsolfrac)               ! Facular index from NRLSSI2 Mg "Bremen" index 
                                                        !  time-averaged over Solar Cycles 13-24
      real(kind=rb) :: sbavgcyc(nsolfrac)               ! Sunspot index from NRLSSI2 SB "SPOT67" index 
                                                        !  time-averaged over Solar Cycles 13-24
      mgavgcyc(:) = (/ &
        &   0.150737_rb,  0.150733_rb,  0.150718_rb,  0.150725_rb,  0.150762_rb,  0.150828_rb, &
        &   0.150918_rb,  0.151017_rb,  0.151113_rb,  0.151201_rb,  0.151292_rb,  0.151403_rb, &
        &   0.151557_rb,  0.151766_rb,  0.152023_rb,  0.152322_rb,  0.152646_rb,  0.152969_rb, &
        &   0.153277_rb,  0.153579_rb,  0.153899_rb,  0.154252_rb,  0.154651_rb,  0.155104_rb, &
        &   0.155608_rb,  0.156144_rb,  0.156681_rb,  0.157178_rb,  0.157605_rb,  0.157971_rb, &
        &   0.158320_rb,  0.158702_rb,  0.159133_rb,  0.159583_rb,  0.160018_rb,  0.160408_rb, &
        &   0.160725_rb,  0.160960_rb,  0.161131_rb,  0.161280_rb,  0.161454_rb,  0.161701_rb, &
        &   0.162034_rb,  0.162411_rb,  0.162801_rb,  0.163186_rb,  0.163545_rb,  0.163844_rb, &
        &   0.164029_rb,  0.164054_rb,  0.163910_rb,  0.163621_rb,  0.163239_rb,  0.162842_rb, &
        &   0.162525_rb,  0.162344_rb,  0.162275_rb,  0.162288_rb,  0.162369_rb,  0.162500_rb, &
        &   0.162671_rb,  0.162878_rb,  0.163091_rb,  0.163251_rb,  0.163320_rb,  0.163287_rb, &
        &   0.163153_rb,  0.162927_rb,  0.162630_rb,  0.162328_rb,  0.162083_rb,  0.161906_rb, &
        &   0.161766_rb,  0.161622_rb,  0.161458_rb,  0.161266_rb,  0.161014_rb,  0.160666_rb, &
        &   0.160213_rb,  0.159690_rb,  0.159190_rb,  0.158831_rb,  0.158664_rb,  0.158634_rb, &
        &   0.158605_rb,  0.158460_rb,  0.158152_rb,  0.157691_rb,  0.157152_rb,  0.156631_rb, &
        &   0.156180_rb,  0.155827_rb,  0.155575_rb,  0.155406_rb,  0.155280_rb,  0.155145_rb, &
        &   0.154972_rb,  0.154762_rb,  0.154554_rb,  0.154388_rb,  0.154267_rb,  0.154152_rb, &
        &   0.154002_rb,  0.153800_rb,  0.153567_rb,  0.153348_rb,  0.153175_rb,  0.153044_rb, &
        &   0.152923_rb,  0.152793_rb,  0.152652_rb,  0.152510_rb,  0.152384_rb,  0.152282_rb, &
        &   0.152194_rb,  0.152099_rb,  0.151980_rb,  0.151844_rb,  0.151706_rb,  0.151585_rb, &
        &   0.151496_rb,  0.151437_rb,  0.151390_rb,  0.151347_rb,  0.151295_rb,  0.151220_rb, &
        &   0.151115_rb,  0.150993_rb,  0.150883_rb,  0.150802_rb,  0.150752_rb,  0.150737_rb/)
      sbavgcyc(:) = (/ &
        &    50.3550_rb,   52.0179_rb,   59.2231_rb,   66.3702_rb,   71.7545_rb,   76.8671_rb, &
        &    83.4723_rb,   91.1574_rb,   98.4915_rb,  105.3173_rb,  115.1791_rb,  130.9432_rb, &
        &   155.0483_rb,  186.5379_rb,  221.5456_rb,  256.9212_rb,  291.5276_rb,  325.2953_rb, &
        &   356.4789_rb,  387.2470_rb,  422.8557_rb,  466.1698_rb,  521.5139_rb,  593.2833_rb, &
        &   676.6234_rb,  763.6930_rb,  849.1200_rb,  928.4259_rb,  994.9705_rb, 1044.2605_rb, &
        &  1087.5703_rb, 1145.0623_rb, 1224.3491_rb, 1320.6497_rb, 1413.0979_rb, 1472.1591_rb, &
        &  1485.7531_rb, 1464.1610_rb, 1439.1617_rb, 1446.2449_rb, 1496.4323_rb, 1577.8394_rb, &
        &  1669.5933_rb, 1753.0408_rb, 1821.9296_rb, 1873.2789_rb, 1906.5240_rb, 1920.4482_rb, &
        &  1904.6881_rb, 1861.8397_rb, 1802.7661_rb, 1734.0215_rb, 1665.0562_rb, 1608.8999_rb, &
        &  1584.8208_rb, 1594.0162_rb, 1616.1486_rb, 1646.6031_rb, 1687.1962_rb, 1736.4778_rb, &
        &  1787.2419_rb, 1824.9084_rb, 1835.5236_rb, 1810.2161_rb, 1768.6124_rb, 1745.1085_rb, &
        &  1748.7762_rb, 1756.1239_rb, 1738.9929_rb, 1700.0656_rb, 1658.2209_rb, 1629.2925_rb, &
        &  1620.9709_rb, 1622.5157_rb, 1623.4703_rb, 1612.3083_rb, 1577.3031_rb, 1516.7953_rb, &
        &  1430.0403_rb, 1331.5112_rb, 1255.5171_rb, 1226.7653_rb, 1241.4419_rb, 1264.6549_rb, &
        &  1255.5559_rb, 1203.0286_rb, 1120.2747_rb, 1025.5101_rb,  935.4602_rb,  855.0434_rb, &
        &   781.0189_rb,  718.0328_rb,  678.5850_rb,  670.4219_rb,  684.1906_rb,  697.0376_rb, &
        &   694.8083_rb,  674.1456_rb,  638.8199_rb,  602.3454_rb,  577.6292_rb,  565.6213_rb, &
        &   553.7846_rb,  531.7452_rb,  503.9732_rb,  476.9708_rb,  452.4296_rb,  426.2826_rb, &
        &   394.6636_rb,  360.1086_rb,  324.9731_rb,  297.2957_rb,  286.1536_rb,  287.4195_rb, &
        &   288.9029_rb,  282.7594_rb,  267.7211_rb,  246.6594_rb,  224.7318_rb,  209.2318_rb, &
        &   204.5217_rb,  204.1653_rb,  200.0440_rb,  191.0689_rb,  175.7699_rb,  153.9869_rb, &
        &   128.4389_rb,  103.8445_rb,   85.6083_rb,   73.6264_rb,   64.4393_rb,   50.3550_rb/)

! Initializations

      data cdollar /'$'/
      data ixtrans /0,0,0,1,2,3,0,0,0,0,0,4,0,0/

      form1(0) = '(3f10.4,a3,i2,1x,2(f7.2,f8.3,f7.2))'
      form2(0) = '(3f10.4,a3,i2,23x,(f7.2,f8.3,f7.2))'
      form3(0) = '(8e10.3)'
      form1(1) = '(g15.7,g10.4,g10.4,a3,i2,1x,2(g7.2,g8.3,g7.2))'
      form2(1) = '(g15.7,g10.4,g10.4,a3,i2,23x,(g7.2,g8.3,g7.2))'
      form3(1) = '(8g15.7)'

! Pass constants to common block names for rrtatm
      pic = pi
      planckc = planck
      boltzc = boltz
      clightc = clight
      avogadc = avogad
      alosmtc = alosmt
      gasconc = gascon
      radcn1c = radcn1
      radcn2c = radcn2
 
      ixmax = maxinpx

      ird = ird_in

      do ilay = 1,mxlay
         do isp = 1,mxmol
            wkl(isp,ilay) = 0.0_rb
         enddo
         do isp = 1,maxxsec
            wx(isp,ilay) = 0.0_rb
         enddo
      enddo

! Top of read input loop
 1000 continue
      read (ird,9009,end=8800) ctest
      if (ctest .ne. cdollar) goto 1000

      read (ird,9011) iaer, iatm, iscat, istrm, iout, imca, icld, idelm, icos

      if (idelm.gt.1 .or. idelm.lt.0 .or. icos.gt.0 .or. icos.lt.0) then
         print *,'INVALID MEASUREMENT COMPARISON FLAG'
         stop
      endif
      isccos = icos

! No cross-sections implemented in shortwave.
      ixsect = 0

! Only 2-stream scattering implemented in rrtmg
      if (iscat .ne. 1) then
         print *,'INVALID SCATTERING OPTION CHOSEN'
         stop
      endif

      if (istrm .eq. 0) then 
         nstr = 2
      else 
         print *, 'INVALID VALUE FOR ISTRM'
         stop
      endif

      read (ird,9020) juldat, sza, isolvar, scon, solcycfrac, solvar(ib1:ib2)

      indsolvar(:) = 1.0_rb
      bndsolvar(:) = 1.0_rb

      if (isolvar.eq.1) then 
         indsolvar(1) = solvar(16)
         indsolvar(2) = solvar(17)
! Adjust amplitude scaling to be 1.0 at solar min (solcycfrac=0.0229),
! to be the requested indsolvar at solar max (solcycfrac=0.3817), and
! to vary between those values at other solcycfrac. 
         if (indsolvar(1).ne.1.0_rb.or.indsolvar(2).ne.1.0_rb) then 
            if (solcycfrac.ge.0.0_rb.and.solcycfrac.lt.0.0229_rb) then
               wgt = (solcycfrac+1.0_rb-0.3817_rb)/(1.0229_rb-0.3817_rb)
               indsolvar(1) = indsolvar(1) + wgt * (1.0_rb-indsolvar(1))
               indsolvar(2) = indsolvar(2) + wgt * (1.0_rb-indsolvar(2))
            endif
            if (solcycfrac.ge.0.0229_rb.and.solcycfrac.le.0.3817_rb) then
               wgt = (solcycfrac-0.0229_rb)/(0.3817_rb-0.0229_rb)
               indsolvar(1) = 1.0_rb + wgt * (indsolvar(1)-1.0_rb)
               indsolvar(2) = 1.0_rb + wgt * (indsolvar(2)-1.0_rb)
            endif
            if (solcycfrac.gt.0.3817_rb.and.solcycfrac.le.1.0_rb) then
               wgt = (solcycfrac-0.3817_rb)/(1.0229_rb-0.3817_rb)
               indsolvar(1) = indsolvar(1) + wgt * (1.0_rb-indsolvar(1))
               indsolvar(2) = indsolvar(2) + wgt * (1.0_rb-indsolvar(2))
            endif
         endif
      endif
         
      if (isolvar.eq.2) then 
         indsolvar(1) = solvar(17)
         indsolvar(2) = solvar(18)
      endif

      if (isolvar.eq.-1 .or. isolvar.eq.3) then 
         bndsolvar(:) = solvar(ib1:ib2)
      endif
         
      zenith = cos(sza * pi / 180._rb)
      if (juldat .eq. 0) then
         adjflux_jd = 1._rb
      else
         adjflux_jd = earth_sun (juldat)
      endif

! If clouds are present, read in appropriate input file, IN_CLD_RRTM.
      if (icld .ge. 1) call readcld

! If aerosols are present, read in appropriate input from file, IN_AER_RRTM. 
      if (iaer .eq. 10) call readaer


! Apply selected solar variability option based on ISOLVAR and input 
! solar constant.
! For scon = 0, use internally defined solar constant, which is
! 1368.22 Wm-2 (for ISOLVAR=-1) and 1360.85 Wm-2 (for ISOLVAR=0,3;
! options ISOLVAR=1,2 model solar cycle variations from 1360.85 Wm-2)
!
! SCON = 0 
! Use internal TSI value 
      if (scon .eq. 0.0_rb) then 

!   No solar cycle and no solar variability (Kurucz solar irradiance)
!   Apply constant scaling by band if first element of bndsolvar specified
         if (isolvar .eq. -1) then
            solvar(ib1:ib2) = 1.0_rb
            if (bndsolvar(1).gt.0.0_rb) solvar(ib1:ib2) = bndsolvar(:)
         endif 

!   Mean solar cycle with no solar variability (NRLSSI2 model solar irradiance)
!   Quiet sun, facular, and sunspot terms averaged over the mean solar cycle 
!   (defined as average of Solar Cycles 13-24).
         if (isolvar .eq. 0) then
            svar_f = 1.0_rb
            svar_s = 1.0_rb
            svar_i = 1.0_rb
         endif 

!   Mean solar cycle with solar variability (NRLSSI2 model)
!   Facular and sunspot terms interpolated from LUTs to input solar cycle 
!   fraction for mean solar cycle. Scalings defined below to convert from 
!   averaged Mg and SB terms to Mg and SB terms interpolated here.
!   (Includes optional facular and sunspot amplitude scale factors)
         if (isolvar .eq. 1) then
!   Interpolate svar_f_0 and svar_s_0 from lookup tables using provided solar cycle fraction
            if (solcycfrac .le. 0.0_rb) then
               tmp_f_0 = mgavgcyc(1)
               tmp_s_0 = sbavgcyc(1)
            elseif (solcycfrac .ge. 1.0_rb) then
               tmp_f_0 = mgavgcyc(nsolfrac)
               tmp_s_0 = sbavgcyc(nsolfrac)
            else
               sfid = floor(solcycfrac * (nsolfrac-1)) + 1
               nsfm1_inv = 1.0_rb / (nsolfrac-1)
               fraclo = (sfid-1) * nsfm1_inv
               frachi = sfid * nsfm1_inv
               intfrac = (solcycfrac - fraclo) / (frachi - fraclo)
               tmp_f_0 = mgavgcyc(sfid) + intfrac * (mgavgcyc(sfid+1) - mgavgcyc(sfid))
               tmp_s_0 = sbavgcyc(sfid) + intfrac * (sbavgcyc(sfid+1) - sbavgcyc(sfid))
            endif
            svar_f_0 = tmp_f_0
            svar_s_0 = tmp_s_0
            svar_f = indsolvar(1) * (svar_f_0 - Foffset) / (svar_f_avg - Foffset)
            svar_s = indsolvar(2) * (svar_s_0 - Soffset) / (svar_s_avg - Soffset)
            svar_i = 1.0_rb
         endif 

!   Specific solar cycle with solar variability (NRLSSI2 model)
!   Facular and sunspot index terms input directly to model specific 
!   solar cycle.  Scalings defined below to convert from averaged
!   Mg and SB terms to specified Mg and SB terms. 
         if (isolvar .eq. 2) then
            svar_f = (indsolvar(1) - Foffset) / (svar_f_avg - Foffset)
            svar_s = (indsolvar(2) - Soffset) / (svar_s_avg - Soffset)
            svar_i = 1.0_rb
         endif 

!   Mean solar cycle with no solar variability (NRLSSI2 model)
!   Averaged facular, sunspot and quiet sun terms from mean solar cycle 
!   (derived as average of Solar Cycles 13-24). This information is built
!   into coefficient terms specified by g-point elsewhere. Separate
!   scaling by spectral band is applied as defined by bndsolvar. 
         if (isolvar .eq. 3) then
            solvar(ib1:ib2) = bndsolvar(:)
            do ib = ib1,ib2
               svar_f_bnd(ib) = solvar(ib)
               svar_s_bnd(ib) = solvar(ib)
               svar_i_bnd(ib) = solvar(ib)
            enddo
         endif 

      endif

! SCON > 0 
! Scale from internal TSI to externally specified TSI value (scon)
      if (scon .gt. 0.0_rb) then 

!   No solar cycle and no solar variability (Kurucz solar irradiance)
!   Scale from internal solar constant to requested solar constant.
!   Apply optional constant scaling by band if first element of bndsolvar > 0.0
         if (isolvar .eq. -1) then
            if (bndsolvar(1).eq.0.0_rb) solvar(ib1:ib2) = scon / rrsw_scon 
            if (bndsolvar(1).gt.0.0_rb) solvar(ib1:ib2) = bndsolvar(:) * scon / rrsw_scon 
         endif 

!   Mean solar cycle with no solar variability (NRLSSI2 model solar irradiance)
!   Quiet sun, facular, and sunspot terms averaged over the mean solar cycle 
!   (defined as average of Solar Cycles 13-24).
!   Scale internal solar constant to requested solar constant (scon). 
!!   Fint is provided as the product of (svar_f_avg-Foffset) and Fint, 
!!   Sint is provided as the product of (svar_s_avg-Soffset) and Sint
         if (isolvar .eq. 0) then
            svar_cprim = Fint + Sint + Iint
            svar_r = scon / svar_cprim
            svar_f = svar_r
            svar_s = svar_r
            svar_i = svar_r
         endif 

!   Mean solar cycle with solar variability (NRLSSI2 model)
!   Facular and sunspot terms interpolated from LUTs to input solar cycle 
!   fraction for mean solar cycle. Scalings defined below to convert from 
!   averaged Mg and SB terms to Mg and SB terms interpolated here.
!   Scale internal solar constant to requested solar constant. 
!   (Includes optional facular and sunspot amplitude scale factors)
         if (isolvar .eq. 1) then
!   Interpolate svar_f_0 and svar_s_0 from lookup tables using provided solar cycle fraction
            if (solcycfrac .le. 0.0_rb) then
               tmp_f_0 = mgavgcyc(1)
               tmp_s_0 = sbavgcyc(1)
            elseif (solcycfrac .ge. 1.0_rb) then
               tmp_f_0 = mgavgcyc(nsolfrac)
               tmp_s_0 = sbavgcyc(nsolfrac)
            else
               sfid = floor(solcycfrac * (nsolfrac-1)) + 1
               nsfm1_inv = 1.0_rb / (nsolfrac-1)
               fraclo = (sfid-1) * nsfm1_inv
               frachi = sfid * nsfm1_inv
               intfrac = (solcycfrac - fraclo) / (frachi - fraclo)
               tmp_f_0 = mgavgcyc(sfid) + intfrac * (mgavgcyc(sfid+1) - mgavgcyc(sfid))
               tmp_s_0 = sbavgcyc(sfid) + intfrac * (sbavgcyc(sfid+1) - sbavgcyc(sfid))
            endif
            svar_f_0 = tmp_f_0
            svar_s_0 = tmp_s_0
!   Define Cprime 
!            svar_cprim = indsolvar(1) * Fint + indsolvar(2) * Sint + Iint
!   Fint is provided as the product of (svar_f_avg-Foffset) and Fint, 
!   Sint is provided as the product of (svar_s_avg-Soffset) and Sint
            svar_i = (scon - (indsolvar(1) * Fint + indsolvar(2) * Sint)) / Iint
            svar_f = indsolvar(1) * (svar_f_0 - Foffset) / (svar_f_avg - Foffset)
            svar_s = indsolvar(2) * (svar_s_0 - Soffset) / (svar_s_avg - Soffset)
         endif 

!   Specific solar cycle with solar variability (NRLSSI2 model)
!   (Not available for SCON > 0)
!         if (isolvar .eq. 2) then
!            scon = 0.0_rb
!            svar_f = (indsolvar(1) - Foffset) / (svar_f_avg - Foffset)
!            svar_s = (indsolvar(2) - Soffset) / (svar_s_avg - Soffset)
!            svar_i = 1.0_rb
!         endif 

!   Mean solar cycle with no solar variability (NRLSSI2 model)
!   Averaged facular, sunspot and quiet sun terms from mean solar cycle 
!   (derived as average of Solar Cycles 13-24). This information is built
!   into coefficient terms specified by g-point elsewhere. Separate
!   scaling by spectral band is applied as defined by bndsolvar. 
!   Scale internal solar constant (svar_cprim) to requested solar constant (scon)
!   Fint is provided as the product of (svar_f_avg-Foffset) and Fint, 
!   Sint is provided as the product of (svar_s_avg-Soffset) and Sint
         if (isolvar .eq. 3) then
            svar_cprim = Fint + Sint + Iint
            if (bndsolvar(1).eq.0.0_rb) solvar(ib1:ib2) = scon / svar_cprim
            if (bndsolvar(1).gt.0.0_rb) solvar(ib1:ib2) = bndsolvar(:) * scon / svar_cprim
            do ib = ib1,ib2
               svar_f_bnd(ib) = solvar(ib)
               svar_s_bnd(ib) = solvar(ib)
               svar_i_bnd(ib) = solvar(ib)
            enddo
         endif 

      endif

! Combine Earth-Sun adjustment (adjflux_jd) and solar constant scaling (solvar)
! by band when no solar variability and no solar cycle requested
      if (isolvar .lt. 0) then
         do ib = ib1,ib2
            adjflux(ib) = adjflux_jd * solvar(ib)
         enddo
      endif
! Define Earth-Sun adjustment (adjflux_jd) by band for other solar options
      if (isolvar .ge. 0) then
         do ib = ib1,ib2
            adjflux(ib) = adjflux_jd
         enddo
      endif

      read (ird,9012) iemis, ireflect, semiss(ib1:ib2)
      if (iemis .eq. 0) then
         do ib = ib1, ib2
            semiss(ib) = 1._rb
         enddo
      elseif (iemis .eq. 1) then
         do ib = ib1, ib2
            semiss(ib) = semiss(ib1)
         enddo
      elseif (iemis .eq. 2) then
!          print *, 'THESE ARE THE INPUT EMISSIVITY VALUES'
!          print *, semiss(ib1:ib2)
      else
          print *, 'IEMIS = ', iemis, ' NOT A VALID INPUT VALUE'
          stop
      endif
     
      if (iatm .eq. 0) then
         read (ird,9013) iform,nlayers,nmol
         if (nmol.eq.0) nmol = 7                                    
         read (ird,form1(iform)) pavel(1),tavel(1),secntk,cinp, &
              ipthak,altz(0),pz(0),tz(0),altz(1),pz(1),tz(1)
         read (ird,form3(iform)) (wkl(m,1),m=1,7), wbrodl(1)
         if(nmol .gt. 7) read (ird,form3(iform)) (wkl(m,1),m=8,nmol)

         do l = 2, nlayers
            read (ird,form2(iform)) pavel(l),tavel(l),secntk,cinp, &
                 ipthrk,altz(l),pz(l),tz(l)
            read (ird,form3(iform)) (wkl(m,l),m=1,7), wbrodl(l)
            if(nmol .gt. 7) read (ird,form3(iform)) (wkl(m,l),m=8,nmol)
         enddo
           
         if (ixsect .eq. 1) then                                 
            read (ird,9300) nxmol0
            nxmol = nxmol0
            call xsident(ird)
            read (ird,9301) iformx
     
            do l = 1, nlayers       
               read (ird,9010) cdum
               read (ird, form3(iformx)) (wx0(m,l),m=1,7),wbrodx    
               if (nxmol0 .gt. 7) read (ird,form3(iformx)) &
                  (wx0(m,l),m=8,nxmol0)
            enddo
         endif
      else
         ipu = 7
         ipr = 66
         open(unit=ipr,file='tape6',status='unknown')
         call rrtatm
         if (ixsect .eq. 1) then
            do mx = 1, nxmol0
               ixindx(mx) = ixtrans(ixindx0(mx))
            enddo
         endif
      endif

! Test for mixing ratio input.
      imix = 1
      do m = 1, nmol
         if (wkl(m,1) .gt. 1.0_rb) then
            imix = 0
            goto 3600
         endif
      enddo
 3600 continue

      if (ixsect .eq. 1) then
         imixx = 0
         if (wx0(1,1) .le. 1.0_rb) imixx = 1
      endif
      do l = 1, nlayers
         summol = 0.0_rb
         do imol = 2, nmol
            summol = summol + wkl(imol,l)
         enddo
         if (imix .eq. 1) then
            coldry(l) = wbrodl(l) / (1._rb - summol)
            do imol = 1, nmol
               wkl(imol,l) = coldry(l) * wkl(imol,l)
            enddo
         else
            coldry(l) = wbrodl(l) + summol
         endif
         if (ixsect .eq. 1) then
            do ix = 1, nxmol0
               if (ixindx(ix) .ne. 0) then
                  if (imixx .eq. 1) then
                     wx(ixindx(ix),l) = coldry(l) * wx0(ix,l) * 1.e-20_rb
                  else
                     wx(ixindx(ix),l) = wx0(ix,l) * 1.e-20_rb
                  endif
               endif
            enddo 
         endif
      enddo

! Pass output arrays to new variables for transfer to rrtmg thorugh subroutine call.
      nlayers_out = nlayers
      iout_out = iout
      icld_out = icld
      iaer_out = iaer
      isccos_out = isccos
      idelm_out = idelm
      inflag_out = inflag
      iceflag_out = iceflag
      liqflag_out = liqflag

      pz_out(0) = pz(0)
      tz_out(0) = tz(0)
      tbound_out = tz(0)
      do l = 1, mxlay
         pavel_out(l) = pavel(l)
         tavel_out(l) = tavel(l)
         pz_out(l) = pz(l)
         tz_out(l) = tz(l)
         pdp(l) = (pz(l-1) - pz(l))
         coldry_out(l) = coldry(l)
         cldfrac_out(l) = cldfrac(l)
         do imol = 1,nmol
            wkl_out(imol,l) = wkl(imol,l)
         enddo
      enddo

      do l = 1, mxlay
         if (inflag.eq.0) then
            do n = 1, nbndsw
               tauc(n,l) = clddat1(l)
               ssac(n,l) = clddat2(l)
               asmc(n,l) = clddatmom(1,l)
               fsfc(n,l) = asmc(n,l)**2
            enddo
            ciwp(l) = 0._rb
            clwp(l) = 0._rb
            fice(l) = 0._rb
            rei(l) = 0._rb
            rel(l) = 0._rb
         else
            do n = 1, nbndsw
               tauc(n,l) = 0._rb
               ssac(n,l) = 1._rb
               asmc(n,l) = 0._rb
               fsfc(n,l) = 0._rb
            enddo
            cwp = clddat1(l)
            fice(l) = clddat2(l)
            ciwp(l) = cwp * fice(l)
            clwp(l) = cwp * (1._rb - fice(l))
            rei(l) = clddat3(l)
            rel(l) = clddat4(l)
         endif 
      enddo

      do l = 1, mxlay
         do nb = ib1,ib2
            tauaer_out(l,nb) = tauaer(l,nb)
            ssaaer_out(l,nb) = ssaaer(l,nb)
            asmaer_out(l,nb) = phase(1,l,nb)
         enddo
      enddo
      zenith_out = zenith
      do nb = ib1,ib2
        semiss_out(nb) = semiss(nb)
        adjflux_out(nb) = adjflux(nb)
      enddo

! Solar variability
      svar_f_out = svar_f
      svar_s_out = svar_s
      svar_i_out = svar_i
      do nb = ib1,ib2
        svar_f_bnd_out(nb) = svar_f_bnd(nb)
        svar_s_bnd_out(nb) = svar_s_bnd(nb)
        svar_i_bnd_out(nb) = svar_i_bnd(nb)
      enddo
      
      goto 9000

 8800 continue
      stop ' INVALID INPUT_RRTM '

 9000 continue

 9009 format (a1,1x,i2,i2,i2)
 9010 format (a1)
 9011 format (18x,i2,29x,i1,32x,i1,1x,i1,2x,i3,3x,i1,i1,3x,i1,i1)
 9012 format (11x,i1,2x,i1,14f5.3)
 9013 format (1x,i1,i3,i5)                                     
 9020 format (12x, i3, 3x, f7.4, 3x, i2, f10.4, f10.5, 14f10.5)
 9300 format (i5)
 9301 format (1x,i1)

      end subroutine readprof

!***************************************************************************
      subroutine readcld
!***************************************************************************

      use parkind, only : im => kind_im, rb => kind_rb

! Purpose:  To read in IN_CLD_RRTM_SW, the file that contains input 
!           cloud properties.

      implicit integer(i-n), real(a-h,o-z)

! ------- Parameters ------- 
      parameter (mxlay = 203, jpbands = 29)
      parameter (ib1 = 16, ib2 = 29)
      parameter (mg = 16)
      parameter (mxstr = 16)

      common /control/   iaer, nstr, iout, istart, iend, icld, idelm, isccos
      common /profile/   nlayers,pavel(mxlay),tavel(mxlay),pz(0:mxlay),tz(0:mxlay),tbound
      common /cloudin/   inflag,clddat1(mxlay),clddat2(mxlay), &
                         iceflag,liqflag,clddat3(mxlay),clddat4(mxlay), &
                         clddatmom(0:16,mxlay)
      common /clouddat/  ncbands,cldfrac(mxlay), &
                         taucloud(mxlay,jpbands),ssacloud(mxlay,jpbands), &
                         xmom(0:16,mxlay,jpbands)

      character*1 ctest, cpercent

      data cpercent /'%'/
      irdcld = 11

      open(irdcld,file='IN_CLD_RRTM',form='formatted')

! Read in cloud input option.  
      read(irdcld,9050) inflag, iceflag, liqflag

      do lay = 1, nlayers
         cldfrac(lay) = 0._rb
      enddo

      if (inflag .eq. 0) then
 950     continue
!  For INFLAG = 0 or 1, for each cloudy layer only LAY, FRAC, and
!  DAT1 are pertinent.  If CTEST = '%', then there are no more 
!  cloudy layers to process.
         read (irdcld,9099,end=8950) ctest,lay,frac,dat1,dat2,clddatmom(0:nstr,lay)
         if (ctest .eq. cpercent) goto 8950
         cldfrac(lay) = frac
         clddat1(lay) = dat1
         clddat2(lay) = dat2
         goto 950
 8950    continue

      else
 1000    continue
! For INFLAG = 0 or 1, for each cloudy layer only LAY, FRAC, and
! DAT1 are pertinent.  If CTEST = '%', then there are no more 
! cloudy layers to process.
         read (irdcld,9100,end=9000) ctest,lay,frac,dat1,dat2,dat3,dat4
         if (ctest .eq. cpercent) goto 9000
         cldfrac(lay) = frac
         clddat1(lay) = dat1
         clddat2(lay) = dat2
         clddat3(lay) = dat3
         clddat4(lay) = dat4
         goto 1000
 9000    continue
      endif

      close(irdcld)

 9050 format (3x,i2,4x,i1,4x,i1)
 9099 format (a1,1x,i3,19e10.5)
 9100 format (a1,1x,i3,5e10.5)

      end subroutine readcld

!***************************************************************************
      subroutine readaer
!***************************************************************************

      use parkind, only : im => kind_im, rb => kind_rb

! Purpose:  To read in IN_AER_RRTM, the file that contains input
!           aerosol properties.

! -------- Modules --------

      use rrsw_wvn, only : wavenum1, wavenum2

      implicit integer(i-n), real(a-h,o-z)

! ------- Parameters -------
      parameter (mxlay = 203, jpbands = 29)
      parameter (ib1 = 16, ib2 = 29)
      parameter (mg = 16)
      parameter (mxstr = 16)
      parameter (mcmu = 32)

      real aerpar(3), ssa(jpbands), asym(jpbands), aod(mxlay),aod1(jpbands)
      real rlambda(jpbands), specfac(jpbands)
      real rnu0(16:29),rnu1(23:26)
      real f1(23:26),od0(23:26),od1(23:26)
      integer lay(mxlay),ivec(mxlay)

      common /control/ iaer, nstr, iout, istart, iend, icld, idelm, isccos
      common /profile/ nlayers,pavel(mxlay),tavel(mxlay), pz(0:mxlay),tz(0:mxlay),tbound
      common /swprop/  zenith, albedo, adjflux(jpbands)
      common /aerdat/  ssaaer(mxlay,jpbands), phase(mcmu,mxlay,jpbands), tauaer(mxlay,jpbands)

      character*1 ctest, cpercent

      data cpercent /'%'/

      data rnu0 /2903._rb,3601._rb,4310._rb,4892._rb,5623._rb,6872._rb, &
                 7872._rb,10590._rb,14420._rb,18970._rb,25015._rb,30390._rb, & 
                 43507._rb,1412._rb/
      data rnu1 /10530.7_rb,14293.3_rb,18678.0_rb,24475.1_rb/
 
      data f1  /0.9929_rb,0.9883_rb,0.978_rb,0.9696_rb/
      data od0 /0.1084_rb,0.167_rb,0.245_rb,0.3611_rb/
      data od1 /0.3144_rb,0.4822_rb,0.7013_rb,1.0239_rb/

      eps = 1.e-10_rb
      irdaer = 12
      open(irdaer,file='IN_AER_RRTM',form='formatted')

      aod(:) = 0.0_rb
      tauaer(:,ib1:ib2) = 0.0_rb

! Read in number of different aerosol models option.
      read (irdaer, 9010) naer
!       if (naer .gt. 4) then
!          print *, 'NAER (= ', naer, ') IS GREATER THAN 4'
!          stop
!       endif
        
! For each aerosol read in optical properties and layer aerosol 
! optical depths.
      do ia = 1, naer
	 read (irdaer, 9011) nlay, iaod, issa, iasym, aerpar(1:3)

         if (iaod .eq. 0) then
! Set defaults to get standard Angstrom relation.
            if (aerpar(2) .lt. eps) aerpar(2) = 1._rb

            do ib = ib1, ib2
  	       rlambda(ib) = 10000._rb/rnu0(ib)
               specfac(ib) = (aerpar(2) + aerpar(3) * rlambda(ib)) / &
                            ((aerpar(2) + aerpar(3) - 1._rb) + &
                              rlambda(ib)**aerpar(1))
            enddo
         endif

! For this aerosol, read in layers and optical depth information.
! Store a nonzero optical depth in aod to check for double specification.
         do il = 1, nlay
            read(irdaer, 9012) lay(il), aod1(ib1:ib2)
            if (aod(lay(il)) .lt. eps) then
               if (iaod .eq. 0) then
                  aod(lay(il)) = aod1(ib1)
                  do ib = ib1, ib2
                     tauaer(lay(il),ib) = aod(lay(il)) * specfac(ib)
                  enddo
               else
                  do ib = ib1, ib2
                     aod(lay(il)) = max(aod(lay(il)),aod1(ib))
                     tauaer(lay(il),ib) = aod1(ib)
                  enddo
               endif
            else
               print *,'LAYER ',lay(il),' HAS MORE THAN ONE AEROSOL TYPE'
               stop
            endif
         enddo

! Build vector of aerosol layer indices 

         do il=1,nlay
            ivec(il) = lay(il) 
         end do

! Correct bands 23 through 26 for sza effect (negligible for others)
         do ib=23,26
            if (iaod.eq.0) then
                od = sum(tauaer(ivec(1:nlay),ib))/zenith
                rnu = rnu0(ib) + &
                     (rnu1(ib)-rnu0(ib))*(od-od0(ib))/(od1(ib)-od0(ib))
               rlambda_new = 10000._rb/rnu
               specfac_new = (aerpar(2)+aerpar(3)*rlambda_new) / &
                  ((aerpar(2)+aerpar(3)- 1.)+rlambda_new**aerpar(1))
               do il=1,nlay
                  tauaer(lay(il),ib) = tauaer(lay(il),ib) * &
                         specfac_new/specfac(ib)
               end do
            endif
         end do

! For this aerosol, read and store optical properties
         read (irdaer, 9013) ssa(ib1:ib2)

         do ib = ib1, ib2
            do il = 1, nlay
               if (issa .eq. 0) then 
                  ssaaer(lay(il),ib) = ssa(ib1)
               else
                  ssaaer(lay(il),ib) = ssa(ib)
               endif
            enddo
         enddo

         if (iasym .lt. 2) then
            read (irdaer, 9013) asym(ib1:ib2)

            do ib = ib1, ib2
               do il = 1, nlay
                  do istr = 1,  nstr
                     if (iasym .eq. 0) then 
                        phase(istr,lay(il),ib) = asym(ib1)**istr
                     elseif (iasym .eq. 1) then
                        phase(istr,lay(il),ib) = asym(ib)**istr
                     endif
                  enddo
               enddo
            enddo
         else
            do il = 1, nlay
               do istr = 1, nstr
                  read (irdaer, 9013) phase(istr,lay(il),ib1:ib2)
               enddo
            enddo
         endif

! End of naer loop
      enddo

 9000 continue
      close(irdaer)

 9010 format (3x, i2)
 9011 format (2x, i3, 4x, i1, 4x, i1, 4x, i1, 3f8.3)
 9012 format (2x, i3, 14f7.4)
 9013 format (14f5.3)

      end subroutine readaer

!**********************************************************************
      subroutine xsident(ird)
!**********************************************************************

! Purpose:  This subroutine identifies which cross-sections are to be used.

      implicit integer(i-n), real(a-h,o-z)

! ------- Parameters -------
      parameter (maxinpx=35)
      parameter (maxxsec=4)

      common /xsecctrl/ nxmol,ixindx(maxinpx)
                                                                         
!     nxmol     - number of cross-sections input by user
!     ixindx(i) - index of cross-section molecule corresponding to Ith
!                 cross-section specified by user
!                 = 0 -- not allowed in rrtm
!                 = 1 -- ccl4
!                 = 2 -- cfc11
!                 = 3 -- cfc12
!                 = 4 -- cfc22
!                                                                         
!     xsname=names, alias=aliases of the cross-section molecules          
!                                                                         
      character*10 xsname(maxinpx),alias(maxxsec,4),blank               

      data (alias(1,i),i=1,4)/ &
         'CCL4      ', 'CCL3F     ', 'CCL2F2    ', 'CHCLF2    '/ 
      data (alias(2,i),i=1,4)/ &
         ' ZZZZZZZZ ', 'CFCL3     ', 'CF2CL2    ', 'CHF2CL    '/         
      data (alias(3,i),i=1,4)/ &
         ' ZZZZZZZZ ', 'CFC11     ', 'CFC12     ', 'CFC22     '/         
      data (alias(4,i),i=1,4)/ &
         ' ZZZZZZZZ ', 'F11       ', 'F12       ', 'F22       '/        

      data blank / '          '/

      do i = 1, nxmol
         xsname(i) = blank
      enddo

! Read in the names of the molecules

      if (nxmol.gt.7) then                                               
         read (ird,'(7a10)') (xsname(i),i=1,7)                            
         read (ird,'(8a10)') (xsname(i),i=8,nxmol)                       
      else                                                                
         read (ird,'(7a10)') (xsname(i),i=1,nxmol)                       
      endif                                                               
                                                                         
!  Match the names read in against the names stored in alias           
!  and determine the index value.  
      ixmax = 4
      do i = 1, nxmol
!  Left-justify all inputed names.                                      
         call cljust (xsname(i),10)
         ixindx(i) = 0
         do j = 1, ixmax
            if ((xsname(i).eq.alias(1,j)) .or. &
                (xsname(i).eq.alias(2,j)) .or. &
                (xsname(i).eq.alias(3,j)) .or. &                           
                (xsname(i).eq.alias(4,j))) then                           
               ixindx(i) = j                                              
            endif                                                         
         enddo
      enddo   

      end subroutine xsident

      end program rrtmg_sw


