      module rrsw_kg29

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng29

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 29
! band 29:  820-2600 cm-1 (low - h2o; high - co2)
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
! absh2oo : real     
! absco2o : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no29 = 16

      real(kind=rb) :: kao(5,13,no29)
      real(kind=rb) :: kbo(5,13:59,no29)
      real(kind=rb) :: selfrefo(10,no29), forrefo(4,no29)
      real(kind=rb) :: sfluxrefo(no29)
      real(kind=rb) :: irradnceo(no29)
      real(kind=rb) :: facbrghto(no29),snsptdrko(no29)
      real(kind=rb) :: absh2oo(no29), absco2o(no29)

      real(kind=rb) :: rayl

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 29
! band 29:  820-2600 cm-1 (low - h2o; high - co2)
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
! selfref : real     
! forref  : real     
! sfluxref: real     
! irradnce: real     
! facbrght: real     
! snsptdrk: real     
! absh2o  : real     
! absco2  : real     
!-----------------------------------------------------------------

      real(kind=rb) :: ka(5,13,ng29), absa(65,ng29)
      real(kind=rb) :: kb(5,13:59,ng29), absb(235,ng29)
      real(kind=rb) :: selfref(10,ng29), forref(4,ng29)
      real(kind=rb) :: sfluxref(ng29)
      real(kind=rb) :: irradnce(ng29)
      real(kind=rb) :: facbrght(ng29),snsptdrk(ng29)
      real(kind=rb) :: absh2o(ng29), absco2(ng29)

      equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrsw_kg29

