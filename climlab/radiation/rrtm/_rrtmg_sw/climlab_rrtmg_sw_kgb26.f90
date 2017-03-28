! **************************************************************************
      subroutine sw_kgb26
! **************************************************************************

      use parkind, only : im => kind_im, rb => kind_rb
      use rrsw_kg26, only : sfluxrefo, raylo, &
                            irradnceo, facbrghto, snsptdrko

      implicit none
      save

! Kurucz solar source function
      sfluxrefo(:) = (/ &
!         &     129.462_rb, 15*0._rb /)
        &   29.0079_rb,  28.4088_rb,     20.3099_rb,  13.0283_rb &
        &,  11.8619_rb,  9.95840_rb,     6.68696_rb,  5.38987_rb &
        &,  3.49829_rb, 0.407693_rb,    0.299027_rb, 0.236827_rb &
        &, 0.188502_rb, 0.163489_rb, 4.64335e-02_rb, 2.72662e-03_rb /)

! Solar variability components: time-invariant baseline quiet sun irradiance
      irradnceo(:) = (/ &
        & 3.04845E+01_rb, 2.82835E+01_rb, 2.06520E+01_rb, 1.33606E+01_rb,&
        & 1.19253E+01_rb, 9.46266E+00_rb, 6.49295E+00_rb, 4.95470E+00_rb,&
        & 3.04469E+00_rb, 3.68830E-01_rb, 2.69394E-01_rb, 2.19130E-01_rb,&
        & 1.67527E-01_rb, 1.51647E-01_rb, 3.99119E-02_rb, 0.00000E+00_rb/)
! Solar variability components: facular brightening
      facbrghto(:) = (/ &
        & 3.63487E-02_rb, 3.07171E-02_rb, 2.93798E-02_rb, 3.97712E-02_rb,&
        & 2.31774E-02_rb, 1.35487E-02_rb, 1.22683E-02_rb, 8.39258E-03_rb,&
        & 5.82190E-03_rb, 6.43794E-04_rb, 5.41994E-04_rb, 4.30282E-04_rb,&
        & 3.14737E-04_rb, 1.99894E-04_rb, 7.52298E-05_rb, 1.04596E-05_rb/)
! Solar variability components: sunspot darkening
      snsptdrko(:) = (/ &
        &-1.85717E-02_rb,-1.87356E-02_rb,-1.39340E-02_rb,-8.79268E-03_rb,&
        &-8.22724E-03_rb,-6.20947E-03_rb,-4.30472E-03_rb,-3.59906E-03_rb,&
        &-2.11540E-03_rb,-2.34184E-04_rb,-1.94365E-04_rb,-1.52493E-04_rb,&
        &-1.10488E-04_rb,-6.94776E-05_rb,-2.59857E-05_rb,-3.60660E-06_rb/)

! Rayleigh extinction coefficient at all v
      raylo(:) = (/ &
        &  1.21263e-06_rb,1.43428e-06_rb,1.67677e-06_rb,1.93255e-06_rb &
        &, 2.19177e-06_rb,2.44195e-06_rb,2.66926e-06_rb,2.85990e-06_rb &
        &, 3.00380e-06_rb,3.06996e-06_rb,3.08184e-06_rb,3.09172e-06_rb &
        &, 3.09938e-06_rb,3.10456e-06_rb,3.10727e-06_rb,3.10818e-06_rb /)

      end subroutine sw_kgb26
