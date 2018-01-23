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
                ND,  NL,   NTRA,   DELT, CBMFold, IFLAG,  FT,     FQ,   FU, &
                FV,  FTRA, PRECIP, WD,   TPRIME, QPRIME, CBMFnew    )

! INPUT
    integer, intent(in) :: ND, NTRA ! number of layers, number of tracers
    integer, intent(in) :: NL  ! max number of levels to which convection can penetrate
    real, intent(in) :: T(ND),Q(ND),QS(ND),U(ND),V(ND),TRA(ND,NTRA),P(ND),PH(ND+1)
    real, intent(in) :: DELT ! The model time step (sec) between calls to CONVECT
    real, intent(in) :: CBMFold ! The cloud base mass flux ((kg/m**2)/s)
! OUTPUT
    integer, intent(out) :: IFLAG
    real, intent(out) :: FT(ND),FQ(ND),FU(ND),FV(ND),FTRA(ND,NTRA)
    real, intent(out) :: PRECIP, WD, TPRIME, QPRIME
    real, intent(out) :: CBMFnew
!  These are not comments! Necessary directives to f2py to handle array dimensions
!f2py depend(ND) T,Q,QS,U,V,P,PH
!f2py depend(ND,NTRA) TRA
!f2py depend(ND) FT,FQ,FU,FV
!f2py depend(ND,NTRA) FTRA

    CBMFnew = 0. + CBMFold  ! will be updated during call to CONVECT

    call CONVECT(T,Q,QS,U,V,TRA,P,PH,ND,NL,NTRA,DELT, &
             IFLAG,FT,FQ,FU,FV,FTRA,PRECIP,WD,TPRIME,QPRIME,CBMFnew)

end subroutine emanuel_convection
