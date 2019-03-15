*   Modified form of convect43.f
*     for use in climlab
*   Here we have taken the dry convective adjustment code
*   (everything wrapped within the block IF(IPBL.NE.0)THEN )
*   and wrapped it in its own subroutine to be called by the parent program
*
*   Brian E. J. Rose
*   January 2018


***************************************************************************
*****                       SUBROUTINE CONVECT                        *****
*****                          VERSION 4.3c                           *****
*****                          20 May, 2002                           *****
*****                          Kerry Emanuel                          *****
***************************************************************************
C
        SUBROUTINE DRYADJUSTMENT
     *    (T,   Q,    QS,     U,    V,      TRA,    P,    PH,
     *     ND,  NL,   NTRA,   DELT,
     *     CPD, CPV, CL, RV, RD, LV0, G, ROWL, PRECIP
     *     )
C    ***  THE PARAMETER NA SHOULD IN GENERAL BE GREATER THAN   ***
C    ***                OR EQUAL TO  ND + 1                    ***
C
      PARAMETER (NA=70)
C
      REAL T(ND),Q(ND),QS(ND),U(ND),V(ND),TRA(ND,NTRA),P(ND),PH(ND)
      REAL TRATM(NA)
      REAL TH(NA),TOLD(NA)
      REAL LV(NA),LV0,H(NA),HP(NA),GZ(NA),HM(NA)

C  Compute potential temperature
        DO 7 I=1,NL+1
         RDCP=(RD*(1.-Q(I))+Q(I)*RV)/
     1    (CPD*(1.-Q(I))+Q(I)*CPV)
         TH(I)=T(I)*(1000.0/P(I))**RDCP
    7   CONTINUE

C
C     ***            PERFORM DRY ADIABATIC ADJUSTMENT            ***
C
        JC=0
        DO 30 I=NL-1,1,-1
         JN=0
          SUM=TH(I)*(1.+Q(I)*EPSI-Q(I))
         DO 10 J=I+1,NL
          SUM=SUM+TH(J)*(1.+Q(J)*EPSI-Q(J))
          THBAR=SUM/FLOAT(J+1-I)
          IF((TH(J)*(1.+Q(J)*EPSI-Q(J))).LT.THBAR)JN=J
   10    CONTINUE
         IF(I.EQ.1)JN=MAX(JN,2)
         IF(JN.EQ.0)GOTO 30
   12    CONTINUE
         AHM=0.0
         RM=0.0
         UM=0.0
         VM=0.0
         DO K=1,NTRA
          TRATM(K)=0.0
         END DO
         DO 15 J=I,JN
          AHM=AHM+(CPD*(1.-Q(J))+Q(J)*CPV)*T(J)*(PH(J)-PH(J+1))
          RM=RM+Q(J)*(PH(J)-PH(J+1))
          UM=UM+U(J)*(PH(J)-PH(J+1))
          VM=VM+V(J)*(PH(J)-PH(J+1))
          DO K=1,NTRA
           TRATM(K)=TRATM(K)+TRA(J,K)*(PH(J)-PH(J+1))
          END DO
   15    CONTINUE
         DPHINV=1./(PH(I)-PH(JN+1))
         RM=RM*DPHINV
         UM=UM*DPHINV
         VM=VM*DPHINV
         DO K=1,NTRA
          TRATM(K)=TRATM(K)*DPHINV
         END DO
         A2=0.0
         DO 20 J=I,JN
          Q(J)=RM
          U(J)=UM
          V(J)=VM
          DO K=1,NTRA
           TRA(J,K)=TRATM(K)
          END DO
          RDCP=(RD*(1.-Q(J))+Q(J)*RV)/
     1     (CPD*(1.-Q(J))+Q(J)*CPV)
          X=(0.001*P(J))**RDCP
          TOLD(J)=T(J)
          T(J)=X
          A2=A2+(CPD*(1.-Q(J))+Q(J)*CPV)*X*(PH(J)-PH(J+1))
   20    CONTINUE
         DO 25 J=I,JN
          TH(J)=AHM/A2
          T(J)=T(J)*TH(J)
          TC=TOLD(J)-273.15
          ALV=LV0-CPVMCL*TC
          QS(J)=QS(J)+QS(J)*(1.+QS(J)*(EPSI-1.))*ALV*(T(J)-
     1     TOLD(J))/(RV*TOLD(J)*TOLD(J))
   25    CONTINUE
         IF((TH(JN+1)*(1.+Q(JN+1)*EPSI-Q(JN+1))).LT.
     1    (TH(JN)*(1.+Q(JN)*EPSI-Q(JN))))THEN
          JN=JN+1
          GOTO 12
         END IF
         IF(I.EQ.1)JC=JN
   30   CONTINUE
C
C   ***   Remove any supersaturation that results from adjustment ***
C
      IF(JC.GT.1)THEN
       DO 38 J=1,JC
          IF(QS(J).LT.Q(J))THEN
           ALV=LV0-CPVMCL*(T(J)-273.15)
           TNEW=T(J)+ALV*(Q(J)-QS(J))/(CPD*(1.-Q(J))+
     1      CL*Q(J)+QS(J)*(CPV-CL+ALV*ALV/(RV*T(J)*T(J))))
           ALVNEW=LV0-CPVMCL*(TNEW-273.15)
           QNEW=(ALV*Q(J)-(TNEW-T(J))*(CPD*(1.-Q(J))+CL*Q(J)))/ALVNEW
           PRECIP=PRECIP+24.*3600.*1.0E5*(PH(J)-PH(J+1))*
     1      (Q(J)-QNEW)/(G*DELT*ROWL)
           T(J)=TNEW
           Q(J)=QNEW
           QS(J)=QNEW
          END IF
   38  CONTINUE
      END IF
C
      RETURN
      END
