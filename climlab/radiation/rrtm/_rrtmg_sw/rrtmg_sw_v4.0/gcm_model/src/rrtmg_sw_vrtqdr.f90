!     path:      $Source$
!     author:    $Author: miacono $
!     revision:  $Revision: 29812 $
!     created:   $Date: 2016-09-02 17:01:49 -0400 (Fri, 02 Sep 2016) $
!
      module rrtmg_sw_vrtqdr

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

! ------- Modules -------

      use parkind, only: im => kind_im, rb => kind_rb
!      use parrrsw, only: ngptsw

      implicit none

      contains

! --------------------------------------------------------------------------
      subroutine vrtqdr_sw(klev, kw, &
                           pref, prefd, ptra, ptrad, &
                           pdbt, prdnd, prup, prupd, ptdbt, &
                           pfd, pfu)
! --------------------------------------------------------------------------
 
! Purpose: This routine performs the vertical quadrature integration
!
! Interface:  *vrtqdr_sw* is called from *spcvrt_sw* and *spcvmc_sw*
!
! Modifications.
! 
! Original: H. Barker
! Revision: Integrated with rrtmg_sw, J.-J. Morcrette, ECMWF, Oct 2002
! Revision: Reformatted for consistency with rrtmg_lw: MJIacono, AER, Jul 2006
!
!-----------------------------------------------------------------------

! ------- Declarations -------

! Input

      integer(kind=im), intent (in) :: klev                   ! number of model layers
      integer(kind=im), intent (in) :: kw                     ! g-point index

      real(kind=rb), intent(in) :: pref(:)                    ! direct beam reflectivity
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(in) :: prefd(:)                   ! diffuse beam reflectivity
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(in) :: ptra(:)                    ! direct beam transmissivity
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(in) :: ptrad(:)                   ! diffuse beam transmissivity
                                                              !   Dimensions: (nlayers+1)

      real(kind=rb), intent(in) :: pdbt(:)
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(in) :: ptdbt(:)
                                                              !   Dimensions: (nlayers+1)

      real(kind=rb), intent(inout) :: prdnd(:)
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(inout) :: prup(:)
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(inout) :: prupd(:)
                                                              !   Dimensions: (nlayers+1)

! Output
      real(kind=rb), intent(out) :: pfd(:,:)                  ! downwelling flux (W/m2)
                                                              !   Dimensions: (nlayers+1,ngptsw)
                                                              ! unadjusted for earth/sun distance or zenith angle
      real(kind=rb), intent(out) :: pfu(:,:)                  ! upwelling flux (W/m2)
                                                              !   Dimensions: (nlayers+1,ngptsw)
                                                              ! unadjusted for earth/sun distance or zenith angle

! Local

      integer(kind=im) :: ikp, ikx, jk

      real(kind=rb) :: zreflect
      real(kind=rb) :: ztdn(klev+1)  

! Definitions
!
! pref(jk)   direct reflectance
! prefd(jk)  diffuse reflectance
! ptra(jk)   direct transmittance
! ptrad(jk)  diffuse transmittance
!
! pdbt(jk)   layer mean direct beam transmittance
! ptdbt(jk)  total direct beam transmittance at levels
!
!-----------------------------------------------------------------------------
                   
! Link lowest layer with surface
             
      zreflect = 1._rb / (1._rb - prefd(klev+1) * prefd(klev))
      prup(klev) = pref(klev) + (ptrad(klev) * &
                 ((ptra(klev) - pdbt(klev)) * prefd(klev+1) + &
                   pdbt(klev) * pref(klev+1))) * zreflect
      prupd(klev) = prefd(klev) + ptrad(klev) * ptrad(klev) * &
                    prefd(klev+1) * zreflect

! Pass from bottom to top 

      do jk = 1,klev-1
         ikp = klev+1-jk                       
         ikx = ikp-1
         zreflect = 1._rb / (1._rb -prupd(ikp) * prefd(ikx))
         prup(ikx) = pref(ikx) + (ptrad(ikx) * &
                   ((ptra(ikx) - pdbt(ikx)) * prupd(ikp) + &
                     pdbt(ikx) * prup(ikp))) * zreflect
         prupd(ikx) = prefd(ikx) + ptrad(ikx) * ptrad(ikx) * &
                      prupd(ikp) * zreflect
      enddo
    
! Upper boundary conditions

      ztdn(1) = 1._rb
      prdnd(1) = 0._rb
      ztdn(2) = ptra(1)
      prdnd(2) = prefd(1)

! Pass from top to bottom

      do jk = 2,klev
         ikp = jk+1
         zreflect = 1._rb / (1._rb - prefd(jk) * prdnd(jk))
         ztdn(ikp) = ptdbt(jk) * ptra(jk) + &
                    (ptrad(jk) * ((ztdn(jk) - ptdbt(jk)) + &
                     ptdbt(jk) * pref(jk) * prdnd(jk))) * zreflect
         prdnd(ikp) = prefd(jk) + ptrad(jk) * ptrad(jk) * &
                      prdnd(jk) * zreflect
      enddo
    
! Up and down-welling fluxes at levels

      do jk = 1,klev+1
         zreflect = 1._rb / (1._rb - prdnd(jk) * prupd(jk))
         pfu(jk,kw) = (ptdbt(jk) * prup(jk) + &
                      (ztdn(jk) - ptdbt(jk)) * prupd(jk)) * zreflect
         pfd(jk,kw) = ptdbt(jk) + (ztdn(jk) - ptdbt(jk)+ &
                      ptdbt(jk) * prup(jk) * prdnd(jk)) * zreflect
      enddo

      end subroutine vrtqdr_sw

      end module rrtmg_sw_vrtqdr
