      module rrsw_kg22

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng22

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 22
! band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
! Revised: MJIacono, AER, nov2015, solar variability
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!irradnceo: real     
!facbrghto: real     
!snsptdrko: real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no22 = 16

      real(kind=rb) :: kao(9,5,13,no22)
      real(kind=rb) :: kbo(5,13:59,no22)
      real(kind=rb) :: selfrefo(10,no22), forrefo(3,no22)
      real(kind=rb) :: sfluxrefo(no22,9)
      real(kind=rb) :: irradnceo(no22,9)
      real(kind=rb) :: facbrghto(no22,9),snsptdrko(no22,9)

      real(kind=rb) :: rayl

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 22
! band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
! Revised: MJIacono, AER, nov2015, solar variability
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! selfref : real     
! forref  : real
! sfluxref: real     
! irradnce: real     
! facbrght: real     
! snsptdrk: real     
!-----------------------------------------------------------------

      real(kind=rb) :: ka(9,5,13,ng22), absa(585,ng22)
      real(kind=rb) :: kb(5,13:59,ng22), absb(235,ng22)
      real(kind=rb) :: selfref(10,ng22), forref(3,ng22)
      real(kind=rb) :: sfluxref(ng22,9)
      real(kind=rb) :: irradnce(ng22,9)
      real(kind=rb) :: facbrght(ng22,9),snsptdrk(ng22,9)

      equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrsw_kg22

