!  CLIMLAB driver for Emanual convection scheme
!
!   using Kerry Emanuel's convect43.f
!
!  This is a lightweight driver designed for wrapping with f2py
!  and calling from Python
!  Refer to convect43.f source code for more documentation
!  As well as the complete scientific documentation from Kerry Emanuel.
!
!  One subroutine is exposed here:
!   emanuel_convection (wrapper for CONVECT)
!   The call signature is *nearly* identical to the Emanuel code
!  Except for the CBMF argument (Cloud Base Mass Flux)
!  Which is both input and output in the original Fortran code.
!
!   See the python module climlab/convection/emanuel_convection.py
!    to see how these are called from Python
!
!  Brian Rose
!  brose@albany.edu
!  January 2018


subroutine emanuel_convection(T, Q, QS, U, V, TRA, P, PH, &
                NCOL,  ND,  NL,   NTRA,   DELT, CBMFold, &
                CPD, CPV, CL, RV, RD, LV0, G, ROWL, MINORIG, &
                ELCRIT, TLCRIT, ENTP, SIGD, SIGS, OMTRAIN, OMTSNOW, &
                COEFFR, COEFFS, CU, BETA, DTMAX, ALPHA, DAMP, &
                IFLAG,  FT,     FQ,   FU,     FV,  FTRA, &
                PRECIP, WD,   TPRIME, QPRIME, CBMFnew    )

! INPUT
    integer, intent(in) :: ND, NTRA ! number of layers, number of tracers
    integer, intent(in) :: NL  ! max number of levels to which convection can penetrate
    integer, intent(in) :: NCOL  ! number of independent columns
    real, intent(in) :: T(NCOL,ND),Q(NCOL,ND),QS(NCOL,ND),U(NCOL,ND),V(NCOL,ND)
    real, intent(in) :: TRA(NCOL,ND,NTRA),P(ND),PH(ND+1)
    real, intent(in) :: DELT ! The model time step (sec) between calls to CONVECT
    real, intent(in) :: CBMFold(NCOL) ! The cloud base mass flux ((kg/m**2)/s)
    real, intent(in) :: CPD, CPV, CL, RV, RD, LV0, G, ROWL  ! thermodynamic constants
    integer, intent(in) :: MINORIG  ! index of lowest level from which convection may originate
    real, intent(in) :: ELCRIT, TLCRIT, ENTP, SIGD, SIGS, OMTRAIN, OMTSNOW ! model parameters
    real, intent(in) :: COEFFR, COEFFS, CU, BETA, DTMAX, ALPHA, DAMP ! more model parameters
! OUTPUT
    integer, intent(out) :: IFLAG(NCOL)
    real, intent(out) :: FT(NCOL,ND),FQ(NCOL,ND),FU(NCOL,ND),FV(NCOL,ND)
    real, intent(out) :: FTRA(NCOL,ND,NTRA)
    real, intent(out) :: PRECIP(NCOL), WD(NCOL), TPRIME(NCOL), QPRIME(NCOL)
    real, intent(out) :: CBMFnew(NCOL)
!  These are not comments! Necessary directives to f2py to handle array dimensions
!f2py depend(ND) P,PH
!f2py depend(NCOL,ND) T,Q,QS,U,V
!f2py depend(NCOL,ND,NTRA) TRA
!f2py depend(NCOL,ND) FT,FQ,FU,FV
!f2py depend(NCOL,ND,NTRA) FTRA
!f2py depend(NCOL) IFLAG, CBMFold, CBMFnew, PRECIP, WD, TPRIME, QPRIME

    do j = 1, NCOL
      CBMFnew(j) = 0. + CBMFold(j)  ! will be updated during call to CONVECT
      call CONVECT(T(j,:),Q(j,:),QS(j,:),U(j,:),V(j,:),TRA(j,:,:),P,PH, &
             ND,NL,NTRA,DELT,MINORIG, &
             CPD, CPV, CL, RV, RD, LV0, G, ROWL, &
             ELCRIT, TLCRIT, ENTP, SIGD, SIGS, OMTRAIN, OMTSNOW, &
             COEFFR, COEFFS, CU, BETA, DTMAX, ALPHA, DAMP, &
             IFLAG(j),FT(j,:),FQ(j,:),FU(j,:),FV(j,:),FTRA(j,:,:), PRECIP(j), &
             WD(j), TPRIME(j),QPRIME(j),CBMFnew(j))
    end do

end subroutine emanuel_convection
