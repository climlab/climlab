***************************************************************************
*****                       SUBROUTINE CONVECT                        *****
*****                          VERSION 4.1d                           *****
*****                         1 October, 1997                         *****
***************************************************************************
C
	SUBROUTINE CONVECT
     *    (T,       Q,   QS,     P,       PH,      TG,   QG,
     *     PG,      ND,  NL,     DELT,    IFLAG,   FT,   FQ,
     *     PRECIP,  CWDTRAIN,    CBMF)
C
C-----------------------------------------------------------------------------
C    *** On input:      ***
C
C     T:   Array of absolute temperature (K) of dimension ND, with first
C           index corresponding to lowest model level. Note that this array
C           will be altered by the subroutine if dry convective adjustment
C           occurs
C
C     Q:   Array of specific humidity (gm/gm) of dimension ND, with first
C            index corresponding to lowest model level. Must be defined
C            at same grid levels as T. Note that this array will be altered
C            if dry convective adjustment occurs
C
C     QS:  Array of saturation specific humidity of dimension ND, with first
C            index corresponding to lowest model level. Must be defined
C            at same grid levels as T. Note that this array will be altered
C            if dry convective adjustment occurs
C
C     P:   Array of pressure (mb) of dimension ND, with first
C            index corresponding to lowest model level. Must be defined
C            at same grid levels as T
C
C     PH:  Array of pressure (mb) of dimension ND+1, with first index
C            corresponding to lowest level. These pressures are defined at
C            levels intermediate between those of P, T, Q and QS. The first
C             value of PH should be greater than (i.e. at a lower level than)
C             the first value of the array P
C
C     TG:  Scalar ground or sea surface temperature (K)
C
C     QG:  Scalar ground or sea surface value of saturation specific humidity(gm/gm)
C
C     PG:  Scalar ground or sea surface value of pressure (mb). Can be the same
C           value as PH(1)
C
C     ND: The dimension of the arrays T,Q,QS,P,PH,FT and FQ
C
C     NL:  The maximum number of levels to which convection can penetrate, plus 1
C              NL MUST be less than or equal to ND-1
C
C     DELT: The model time step (sec) between calls to CONVECT
C
C----------------------------------------------------------------------------------
C    ***   On Output:         ***
C
C     IFLAG: An output integer whose value denotes the following:
C
C                VALUE                        INTERPRETATION
C                -----                        --------------
C                  0               No moist convection; atmosphere is not
C                                  unstable, or surface temperature is less
C                                  than 250 K or surface specific humidity
C                                  is non-positive.
C
C                  1               Moist convection occurs.
C
C                  2               No moist convection: lifted condensation
C                                  level is above the 200 mb level.
C
C                  3               No moist convection: cloud base is higher
C                                  then the level NL-1.
C
C                  4               Moist convection occurs, but a CFL condition
C                                  on the subsidence warming is violated. This
C                                  does not cause the scheme to terminate.
C
C     FT:   Array of temperature tendency (K/s) of dimension ND, defined at same
C             grid levels as T, Q, QS and P
C
C     FQ:   Array of specific humidity tendencies ((gm/gm)/s) of dimension ND,
C             defined at same grid levels as T, Q, QS and P
C
C     PRECIP: Scalar convective precipitation rate (mm/day)
C
C     CWDTRAIN: Array of condensed water detrainment rate ((Kg/m^2)/s) of
C                dimension ND defined at same grid levels as T,Q, etc.
C
C     CBMF:   The cloud base mass flux ((kg/m**2)/s). THIS SCALAR VALUE MUST
C              BE STORED BY THE CALLING PROGRAM AND RETURNED TO CONVECT AT
C              ITS NEXT CALL. That is, the value of CBMF must be "remembered"
C              by the calling program between calls to CONVECT
C
C------------------------------------------------------------------------------
C
C    ***  THE PARAMETER NA SHOULD IN GENERAL BE GREATER THAN   ***
C    ***                OR EQUAL TO  ND + 1                    ***
C
      PARAMETER (NA=50)           
C
      INTEGER NENT(NA)
      REAL T(ND),Q(ND),QS(ND),P(ND),PH(ND+1)
      REAL FT(ND),FQ(ND),CWDTRAIN(ND)
      REAL M(NA),MENT(NA,NA),QENT(NA,NA),ELIJ(NA,NA)
      REAL SIJ(NA,NA),TVP(NA),TV(NA),WATER(NA)
      REAL QP(NA),EP(NA),TH(NA),WT(NA),CLW(NA)
      REAL SIGP(NA),TP(NA),TOLD(NA),CPN(NA)
      REAL LV(NA),LVCP(NA),LV0,H(NA),HP(NA),GZ(NA)
C
C------------------------------------------------------------------------------
C
C   ***                    SPECIFY PARAMETERS                        ***
C
C   *** ELCRIT IS THE AUTOCONVERSION THERSHOLD WATER CONTENT (gm/gm) ***
C   ***  TLCRIT IS CRITICAL TEMPERATURE BELOW WHICH THE AUTO-        ***
C   ***       CONVERSION THRESHOLD IS ASSUMED TO BE ZERO             ***
C   ***     (THE AUTOCONVERSION THRESHOLD VARIES LINEARLY            ***
C   ***               BETWEEN 0 C AND TLCRIT)                        ***
C   ***   ENTP IS THE COEFFICIENT OF MIXING IN THE ENTRAINMENT       ***
C   ***                       FORMULATION                            ***
C   ***  SIGD IS THE FRACTIONAL AREA COVERED BY UNSATURATED DNDRAFT  ***
C   ***  SIGS IS THE FRACTION OF PRECIPITATION FALLING OUTSIDE       ***
C   ***                        OF CLOUD                              ***
C   ***        OMTRAIN IS THE ASSUMED FALL SPEED (P/s) OF RAIN       ***
C   ***     OMTSNOW IS THE ASSUMED FALL SPEED (P/s) OF SNOW          ***
C   ***  COEFFR IS A COEFFICIENT GOVERNING THE RATE OF EVAPORATION   ***
C   ***                          OF RAIN                             ***
C   ***  COEFFS IS A COEFFICIENT GOVERNING THE RATE OF EVAPORATION   ***
c   ***                          OF SNOW                             ***
C   ***    DTMAX IS THE MAXIMUM NEGATIVE TEMPERATURE PERTURBATION    ***
C   ***        A LIFTED PARCEL IS ALLOWED TO HAVE BELOW ITS LFC      ***
C   ***    SLFRAC GOVERNS THE MAGNITUDE OF THE TEMPERATURE AND       ***
C   ***          SPECIFIC HUMIDITY GRADIENTS IN AN ASSUMED           ***
C   ***                         SURFACE LAYER                        ***
C   ***    ALPHA AND DAMP ARE PARAMETERS THAT CONTROL THE RATE OF    ***
C   ***                 APPROACH TO QUASI-EQUILIBRIUM                ***
C   ***   (THEIR STANDARD VALUES ARE  0.20 AND 0.01, RESPECTIVELY)   ***
C   ***                   (DAMP MUST BE LESS THAN 1)                 ***
C
        ELCRIT=.0011
        TLCRIT=-55.0
        ENTP=1.5
        SIGD=0.05
        SIGS=0.12
        OMTRAIN=50.0
        OMTSNOW=5.5 
        DTMAX=0.65
        SLFRAC=0.0
        ALPHA=0.2 
        DAMP=0.01
C
C   ***  PPMIN IS THE MINIMUM PRESSURE FROM WHICH CONVECTING         ***
C   ***             PARCELS ARE ASSUMED TO ORIGINATE                 ***
C
        PPMIN=600.0
        PPMIN=PPMIN-1.0

C
C   ***        ASSIGN VALUES OF THERMODYNAMIC CONSTANTS,        ***
C   ***            GRAVITY, AND LIQUID WATER DENSITY.           ***
C   ***             THESE SHOULD BE CONSISTENT WITH             ***
C   ***              THOSE USED IN CALLING PROGRAM              ***
C   ***     NOTE: THESE ARE ALSO SPECIFIED IN SUBROUTINE TLIFT  ***
C
      CPD=1005.7
      CPV=1870.0
      CL=4190.0 
      RV=461.5
      RD=287.04
      LV0=2.501E6
      G=9.8  
      ROWL=1000.0
C
      CPVMCL=CL-CPV 
      EPS=RD/RV
      EPSI=1./EPS
      GINV=1.0/G
      DELTI=1.0/DELT
C
C           ***  INITIALIZE OUTPUT ARRAYS AND PARAMETERS  ***
C
	DO 5 I=1,ND
	 FT(I)=0.0
	 FQ(I)=0.0
         CWDTRAIN(I)=0.0
    5   CONTINUE
	DO 7 I=1,NL+1
	 RDCP=(RD*(1.-Q(I))+Q(I)*RV)/
     1    (CPD*(1.-Q(I))+Q(I)*CPV)
	 TH(I)=T(I)*(1000.0/P(I))**RDCP
    7   CONTINUE
	PRECIP=0.0
	IFLAG=0
C
C     ***            PERFORM DRY ADIABATIC ADJUSTMENT            ***
C
C     ***  DO NOT BYPASS THIS EVEN IF THE CALLING PROGRAM HAS A  ***
C     ***                BOUNDARY LAYER SCHEME !                 ***
C                        
      JC=0
	DO 30 I=NL-1,1,-1
	 JN=0
	   SUM=TH(I)
	 DO 10 J=I+1,NL
	    SUM=SUM+TH(J)
	    THBAR=SUM/FLOAT(J+1-I)
	  IF(TH(J).LT.THBAR)JN=J
   10    CONTINUE
	 IF(JN.EQ.0)GOTO 30
   12    CONTINUE
	 AHM=0.0
	 RM=0.0
	 DO 15 J=I,JN
	  AHM=AHM+(CPD*(1.-Q(J))+Q(J)*CPV)*T(J)*(PH(J)-PH(J+1))
	  RM=RM+Q(J)*(PH(J)-PH(J+1))
   15    CONTINUE
	 RM=RM/(PH(I)-PH(JN+1))
	 A2=0.0
	 DO 20 J=I,JN
	  Q(J)=RM
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
	  QS(J)=QS(J)+QS(J)*(1.+QS(J)*0.608)*ALV*(T(J)-
     1     TOLD(J))/(RV*TOLD(J)*TOLD(J))
   25    CONTINUE
	 IF(TH(JN+1).LT.TH(JN))THEN
	  JN=JN+1
	  GOTO 12
	 END IF
	 IF(I.EQ.1)JC=JN 
   30   CONTINUE
C
C  ***  Add surface layer and re-adjust mixed layer to conserve enthalpy   ***
C
      IF(JC.GT.1)THEN
       TG1=TG*(P(1)/PG)**(RD/CPD)
       QOLD=Q(1)
       Q(1)=Q(1)*(1.-SLFRAC)+SLFRAC*QG
       RDCP1=(RD*(1.-Q(1))+RV*Q(1))/(CPD*(1.-Q(1))+CPV*Q(1))
       T1OLD=T(1)
       T(1)=T(1)*(1.-SLFRAC)+SLFRAC*TG1
	 TH(1)=T(1)*(1000.0/P(1))**RDCP1
	 QN=(QOLD*(PH(1)-PH(JC+1))-Q(1)*(PH(1)-PH(2)))/(PH(2)-PH(JC+1))
	 RDCPN=(RD*(1.-QN)+RV*QN)/(CPD*(1.-QN)+CPV*QN) 
       AHOLD=(CPD*(1.-QOLD)+CPV*QOLD)*T1OLD*(PH(1)-PH(2))
       PTHINT=0.0
       DO 32 I=2,JC
        AHOLD=AHOLD+(CPD*(1.-QOLD)+CPV*QOLD)*T(I)*(PH(I)-PH(I+1))
         TH(I)=(0.001*P(I))**RDCPN
         PTHINT=PTHINT+TH(I)*(PH(I)-PH(I+1))
   32  CONTINUE  
       THNEW=(AHOLD-(CPD*(1.-Q(1))+CPV*Q(1))*T(1)*(PH(1)-PH(2)))/
     1  ((CPD*(1.-QN)+CPV*QN)*PTHINT)
       DO 35 I=2,JC
        T(I)=THNEW*TH(I)
        TH(I)=THNEW
        Q(I)=QN
   35  CONTINUE
C
C   ***   Remove any supersaturation that results from adjustment ***
C
       DO 38 J=1,JC
	  IF(QS(J).LT.Q(J))THEN 
	   ALV=LV0-CPVMCL*(T(J)-273.15)  
	   TNEW=T(J)+ALV*(Q(J)-QS(J))/(CPD*(1.-Q(J))+
     1      CL*Q(J)+QS(J)*(CPV-CL+ALV*ALV/(RV*T(J)*T(J))))
	   QNEW=Q(J)-(CPD*(1.-Q(J))+CPV*Q(J))*(TNEW-T(J))/ALV
	   PRECIP=PRECIP+24.*3600.*1.0E5*(PH(J)-PH(J+1))*
     1      (Q(J)-QNEW)/(G*DELT*ROWL)
	   T(J)=TNEW
	   Q(J)=QNEW
	  END IF     
   38  CONTINUE  
      END IF
C
C  *** CALCULATE ARRAYS OF GEOPOTENTIAL, HEAT CAPACITY AND STATIC ENERGY
C  
	GZ(1)=0.0
	CPN(1)=CPD*(1.-Q(1))+Q(1)*CPV
	H(1)=T(1)*CPN(1)
	DO 40 I=2,NL
	  TVX=T(I)*(1.+Q(I)*EPSI-Q(I))
	  TVY=T(I-1)*(1.+Q(I-1)*EPSI-Q(I-1))
	  GZ(I)=GZ(I-1)+0.5*RD*(TVX+TVY)*(P(I-1)-P(I))/PH(I)
	  CPN(I)=CPD*(1.-Q(I))+CPV*Q(I)
	  H(I)=T(I)*CPN(I)+GZ(I)
   40   CONTINUE
C
C  ***  CHECK WHETHER LOWEST LEVEL TEMPERATURE AND SPECIFIC HUMIDITY   ***
C  ***                          ARE REASONABLE                         ***
C
	IF(T(1).LT.250.0.OR.Q(1).LE.0.0)THEN
	 IFLAG=0
	 RETURN
	END IF
C
C   ***  START LOOP THROUGH PARCEL ORIGIN LEVELS    ***
C
	NKP=0
	DO 800 NK=1,NL-1      
C
C   ***   CONVECT ONLY IF PRESSURE IS GREATER THAN PPMIN     ***
C
	IF(P(NK).LT.PPMIN)GOTO 800
C
C   ***  CALCULATE LIFTED CONDENSATION LEVEL OF AIR AT PARCEL ORIGIN LEVEL ***
C   ***       (WITHIN 0.2% OF FORMULA OF BOLTON, MON. WEA. REV.,1980)      ***
C
	IF(T(NK).LT.250.0.OR.Q(NK).LE.0.0)GOTO 800
	RH=Q(NK)/QS(NK)
	CHI=T(NK)/(1669.0-122.0*RH-T(NK))
	PLCL=P(NK)*(RH**CHI)
	IF(PLCL.LT.200.0.OR.PLCL.GE.2000.0)THEN
	 IFLAG=2
	 GOTO 800
	END IF
C
C   ***  CALCULATE FIRST LEVEL ABOVE LCL (=ICB)  ***
C
	ICB=NL-1
	DO 50 I=NK+1,NL
	 IF(P(I).LT.PLCL)THEN
	  ICB=MIN(ICB,I)
	 END IF
   50   CONTINUE
	IF(ICB.GE.(NL-1))THEN
	 IFLAG=3
	 GOTO 800
	END IF
C
C   ***  CALCULATE MAXIMUM ALLOWED VALUE OF THE CLOUD BASE MASS FLUX  ***
C
	CBMFMAX=10.*(PH(ICB)-PH(ICB+1))*GINV*DELTI
C
C   *** FIND TEMPERATURE UP THROUGH ICB AND TEST FOR INSTABILITY           ***
C
C   *** SUBROUTINE TLIFT CALCULATES PART OF THE LIFTED PARCEL VIRTUAL      ***
C   ***  TEMPERATURE, THE ACTUAL TEMPERATURE AND THE ADIABATIC             ***
C   ***                   LIQUID WATER CONTENT                             ***
C
	CALL TLIFT(P,T,Q,QS,GZ,ICB,NK,TVP,TP,CLW,ND,NL,1)
	DO 54 I=NK,ICB
	 TV(I)=T(I)*(1.+Q(I)*EPSI-Q(I))
	 TVP(I)=TVP(I)-TP(I)*Q(NK)
   54   CONTINUE
C
C   ***  If there was no convection at last time step and parcel    ***
C   ***       is stable at ICB then skip rest of calculation        ***
C
        IF(CBMF.EQ.0.0.AND.TVP(ICB).LE.(TV(ICB)-DTMAX))THEN
	 IFLAG=0
	 RETURN
	END IF
C
C   ***  IF THIS POINT IS REACHED, MOIST CONVECTIVE ADJUSTMENT IS NECESSARY ***
C
	NKP=1
	IF(IFLAG.NE.4)IFLAG=1
C
C   ***  FIND THE REST OF THE LIFTED PARCEL TEMPERATURES          ***
C
	CALL TLIFT(P,T,Q,QS,GZ,ICB,NK,TVP,TP,CLW,ND,NL,2)
C
C   ***  SET THE PRECIPITATION EFFICIENCIES AND THE FRACTION OF   ***
C   ***          PRECIPITATION FALLING OUTSIDE OF CLOUD           ***
C   ***      THESE MAY BE FUNCTIONS OF TP(I), P(I) AND CLW(I)     ***
C                 
	DO 60 I=1,NL
	 TCA=TP(I)-273.15
	 IF(TCA.GE.0.0)THEN
	  ELACRIT=ELCRIT
	 ELSE
	  ELACRIT=ELCRIT*(1.0-TCA/TLCRIT)
	 END IF
         ELACRIT=MAX(ELACRIT,0.0)
	 EP(I)=1.0-ELACRIT/MAX(CLW(I),1.0E-8)
	 EP(I)=MAX(EP(I),0.0)
	 EP(I)=MIN(EP(I),1.0)
	 SIGP(I)=SIGS
   60   CONTINUE
C
C   ***       CALCULATE VIRTUAL TEMPERATURE AND LIFTED PARCEL     ***
C   ***                    VIRTUAL TEMPERATURE                    ***
C
	DO 64 I=ICB+1,NL
	 TV(I)=T(I)*(1.+Q(I)*EPSI-Q(I))
         TVP(I)=TVP(I)*(1.-Q(NK)+EP(I)*CLW(I))
   64   CONTINUE
C
C   ***        NOW INITIALIZE VARIOUS ARRAYS USED IN THE COMPUTATIONS       ***
C
	DO 70 I=1,NL
	 HP(I)=H(I)
	 QP(I)=Q(I)
	 NENT(I)=0
	 WATER(I)=0.0
	 M(I)=0.0
	 LV(I)=LV0-CPVMCL*(T(I)-273.15)
	 LVCP(I)=LV(I)/CPN(I)
	 DO 70 J=1,NL
	  QENT(I,J)=Q(J)
	  ELIJ(I,J)=0.0
	  MENT(I,J)=0.0
	  SIJ(I,J)=0.0
   70   CONTINUE
C
C  ***  FIND THE FIRST MODEL LEVEL (INB) ABOVE THE PARCEL'S       ***
C  ***          HIGHEST LEVEL OF NEUTRAL BUOYANCY                 ***
C
	INB=ICB+1
	DO 80 I=NL-1,ICB+1,-1 
	 IF((TVP(I)-TV(I)).GT.0.0)THEN
	  INB=MAX(INB,I)
	 END IF
   80   CONTINUE     
	INB=INB+1
C
C  ***  Redefine INB if there is an intervening stable layer   ***
C
	INBOLD=INB
 	ANA=0.0
	CAPE=0.0
 	DO 82 I=ICB+1,INBOLD-1
       	 BY=(TVP(I)-TV(I))*(PH(I)-PH(I+1))/P(I)
  	 BYP=(TVP(I+1)-TV(I+1))*(PH(I+1)-PH(I+2))/P(I+1)
  	 CAPE=CAPE+BY
  	 IF(CAPE.GT.0.001)THEN
   	  INB=I+1
   	  ANA=ANA-MIN(BYP,0.0)
  	  IF(ANA.GT.0.001)THEN
   	   GOTO 81
	  END IF
         END IF
   82   CONTINUE
   81   CONTINUE
        INB=MIN(INB,(NL-1))
C
C   ***   CALCULATE LIQUID WATER STATIC ENERGY OF LIFTED PARCEL   ***
C
	DO 95 I=ICB,INB
	 HP(I)=H(NK)+(LV(I)+(CPD-CPV)*T(I))*EP(I)*CLW(I)
   95   CONTINUE                  
C
C   ***  CALCULATE CLOUD BASE MASS FLUX AND RATES OF MIXING, M(I),  ***
c   ***                   AT EACH MODEL LEVEL                       ***
C
	CBMFSUM=0.0
	DBOSUM=0.0
C   
C   ***     INTERPOLATE DIFFERENCE BETWEEN LIFTED PARCEL AND      ***
C   ***  ENVIRONMENTAL TEMPERATURES TO LIFTED CONDENSATION LEVEL  ***
C	
        TVPPLCL=TVP(ICB-1)-RD*TVP(ICB-1)*(P(ICB-1)-PLCL)/
     1    (CPN(ICB-1)*P(ICB-1))
        TVAPLCL=TV(ICB)+(TVP(ICB)-TVP(ICB+1))*(PLCL-P(ICB))/
     1    (P(ICB)-P(ICB+1))
        DTPBL=0.0
        DO 96 I=NK,ICB-1
         DTPBL=DTPBL+(TVP(I)-TV(I))*(PH(I)-PH(I+1))
   96   CONTINUE
        DTPBL=DTPBL/(PH(NK)-PH(ICB))
        DTMIN=TVPPLCL-TVAPLCL+DTMAX+DTPBL
        DTMA=DTMIN
C
C   ***  ADJUST CLOUD BASE MASS FLUX   ***
C

      CBMFOLD=CBMF
      CBMF=(1.-DAMP)*CBMF+0.1*ALPHA*DTMA
      CBMF=MAX(CBMF,0.0)
C
C   *** If cloud base mass flux is zero, skip rest of calculation  ***
C
      IF(CBMF.EQ.0.0.AND.CBMFOLD.EQ.0.0)THEN
       RETURN
      END IF
      CBMFSUM=0.5*(CBMF+CBMFOLD)
C
C   ***  LIMIT CLOUD BASE MASS FLUX TO AVOID VIOLATING CFL  ***
C
      IF(CBMFSUM.GT.CBMFMAX)THEN
       CBMF=CBMF*(CBMFMAX/CBMFSUM)
       CBMFSUM=CBMFMAX
      END IF        
C
C   ***   CALCULATE RATES OF MIXING,  M(I)   ***
C
      M(ICB)=0.0
      DO 103 I=ICB+1,INB
       DBO=ABS(TV(I+1)-TVP(I+1)-TV(I-1)+TVP(I-1))+
     1  ENTP*0.04*(PH(I)-PH(I+1))
       DBOSUM=DBOSUM+DBO
       M(I)=CBMF*DBO
  103 CONTINUE
      DO 110 I=ICB+1,INB
       M(I)=M(I)/DBOSUM  
  110 CONTINUE     
C
C   ***  CALCULATE ENTRAINED AIR MASS FLUX (MENT), TOTAL WATER MIXING  ***
C   ***     RATIO (QENT), TOTAL CONDENSED WATER (ELIJ), AND MIXING     ***
C   ***                        FRACTION (SIJ)                          ***
C
	DO 170 I=ICB+1,INB
	 QTI=Q(NK)-EP(I)*CLW(I)
	 DO 160 J=ICB,INB
	  BF2=1.+LV(J)*LV(J)*QS(J)/(RV*T(J)*T(J)*CPD)
	  ANUM=H(J)-HP(I)+(CPV-CPD)*T(J)*(QTI-Q(J))
	  DENOM=H(I)-HP(I)+(CPD-CPV)*(Q(I)-QTI)*T(J)
	  DEI=DENOM
	  IF(ABS(DEI).LT.0.01)DEI=0.01
	  SIJ(I,J)=ANUM/DEI
	  SIJ(I,I)=1.0
	  ALTEM=SIJ(I,J)*Q(I)+(1.-SIJ(I,J))*QTI-QS(J)
	  ALTEM=ALTEM/BF2
	  CWAT=CLW(J)*(1.-EP(J))
	  STEMP=SIJ(I,J)
	  IF((STEMP.LT.0.0.OR.STEMP.GT.1.0.OR.
     1      ALTEM.GT.CWAT).AND.J.GT.I)THEN
	   ANUM=ANUM-LV(J)*(QTI-QS(J)-CWAT*BF2)
	   DENOM=DENOM+LV(J)*(Q(I)-QTI)
	   IF(ABS(DENOM).LT.0.01)DENOM=0.01
	   SIJ(I,J)=ANUM/DENOM
	   ALTEM=SIJ(I,J)*Q(I)+(1.-SIJ(I,J))*QTI-QS(J)
	   ALTEM=ALTEM-(BF2-1.)*CWAT
	  END IF
	  IF(SIJ(I,J).GT.0.0.AND.SIJ(I,J).LT.0.90)THEN
	   QENT(I,J)=SIJ(I,J)*Q(I)+(1.-SIJ(I,J))*QTI
	   ELIJ(I,J)=ALTEM
	   ELIJ(I,J)=MAX(0.0,ELIJ(I,J))
	   MENT(I,J)=M(I)/(1.-SIJ(I,J))
	   NENT(I)=NENT(I)+1
	  END IF
	  SIJ(I,J)=MAX(0.0,SIJ(I,J))
	  SIJ(I,J)=MIN(1.0,SIJ(I,J))
  160    CONTINUE
C
C   ***   IF NO AIR CAN ENTRAIN AT LEVEL I ASSUME THAT UPDRAFT DETRAINS  ***
C   ***   AT THAT LEVEL AND CALCULATE DETRAINED AIR FLUX AND PROPEQTIES  ***
C
	 IF(NENT(I).EQ.0)THEN
	  MENT(I,I)=M(I)
	  QENT(I,I)=Q(NK)-EP(I)*CLW(I)
	  ELIJ(I,I)=CLW(I)
	  SIJ(I,I)=1.0
	 END IF 
  170   CONTINUE
	SIJ(INB,INB)=1.0
C
C   ***  NORMALIZE ENTRAINED AIR MASS FLUXES TO REPRESENT EQUAL  ***
C   ***              PROBABILITIES OF MIXING                     ***
C
	DO 200 I=ICB+1,INB
	IF(NENT(I).NE.0)THEN
	 QP1=Q(NK)-EP(I)*CLW(I)
	 ANUM=H(I)-HP(I)-LV(I)*(QP1-QS(I))
	 DENOM=H(I)-HP(I)+LV(I)*(Q(I)-QP1)
	 IF(ABS(DENOM).LT.0.01)DENOM=0.01
	 SCRIT=ANUM/DENOM
	 ALT=QP1-QS(I)+SCRIT*(Q(I)-QP1)
	 IF(SCRIT.LT.0.0.OR.ALT.LT.0.0)SCRIT=1.0
	 ASIJ=0.0
	 SMIN=1.0
	 DO 175 J=ICB,INB    
	  IF(SIJ(I,J).GT.0.0.AND.SIJ(I,J).LT.0.90)THEN
	   IF(J.GT.I)THEN
	    SMID=MIN(SIJ(I,J),SCRIT)
	    SJMAX=SMID
	    SJMIN=SMID
	    IF(SMID.LT.SMIN.AND.SIJ(I,J+1).LT.SMID)THEN
	     SMIN=SMID
	     SJMAX=MIN(SIJ(I,J+1),SIJ(I,J),SCRIT)
	     SJMIN=MAX(SIJ(I,J-1),SIJ(I,J))
	     SJMIN=MIN(SJMIN,SCRIT)
	    END IF
	   ELSE
	    SJMAX=MAX(SIJ(I,J+1),SCRIT)
	    SMID=MAX(SIJ(I,J),SCRIT)
	    SJMIN=0.0
	    IF(J.GT.1)SJMIN=SIJ(I,J-1)
	    SJMIN=MAX(SJMIN,SCRIT)
	   END IF
	   DELP=ABS(SJMAX-SMID)
	   DELM=ABS(SJMIN-SMID)
           ASIJ=ASIJ+(DELP+DELM)*(PH(J)-PH(J+1))
           MENT(I,J)=MENT(I,J)*(DELP+DELM)*(PH(J)-PH(J+1))
	  END IF
  175    CONTINUE
	 ASIJ=MAX(1.0E-21,ASIJ)
	 ASIJ=1.0/ASIJ
	 DO 180 J=ICB,INB
	  MENT(I,J)=MENT(I,J)*ASIJ
  180    CONTINUE
	 BSUM=0.0
	 DO 190 J=ICB,INB
	  BSUM=BSUM+MENT(I,J)
  190    CONTINUE
	 IF(BSUM.LT.1.0E-18)THEN
	  NENT(I)=0
	  MENT(I,I)=M(I)
	  QENT(I,I)=Q(NK)-EP(I)*CLW(I)
	  ELIJ(I,I)=CLW(I)
	  SIJ(I,I)=1.0
	 END IF
	END IF
  200   CONTINUE
C
C   ***  CHECK WHETHER EP(INB)=0, IF SO, SKIP PRECIPITATING    ***
C   ***             DOWNDRAFT CALCULATION                      ***
C
	IF(EP(INB).LT.0.0001)GOTO 405
C
C   ***  INTEGRATE LIQUID WATER EQUATION TO FIND CONDENSED WATER   ***
C   ***                AND CONDENSED WATER FLUX                    ***
C
	QP(INB)=Q(INB-1)
	QP(INB+1)=Q(INB)
	WT(INB+1)=0.0
	JTT=2
C
C    ***                    BEGIN DOWNDRAFT LOOP                    ***
C
	DO 400 I=INB,1,-1
C
C    ***              CALCULATE DETRAINED PRECIPITATION             ***
C
	WDTRAIN=G*EP(I)*M(I)*CLW(I)
	IF(I.GT.1)THEN
	 DO 320 J=1,I-1
	 AWAT=ELIJ(J,I)-(1.-EP(I))*CLW(I)
	 AWAT=MAX(0.0,AWAT)
  320    WDTRAIN=WDTRAIN+G*AWAT*MENT(J,I)
	END IF
	CWDTRAIN(I)=SIGP(I)*WDTRAIN/G
C
C    ***    FIND RAIN WATER AND PRECIPITATION   ***
C     
c
c   ***  Value of terminal velocity for snow   ***
c 
        WT(I)=OMTSNOW
c      
c   ***  Value of terminal velocity for rain   ***
c
        IF(T(I).GT.273.0)THEN
         WT(I)=OMTRAIN
        END IF
c
        WATER(I)=(WATER(I+1)*WT(I+1)+(1.-SIGP(I))*WDTRAIN/SIGD)/WT(I)
C
C   ***  CALCULATE SURFACE CONVECTIVE PRECIPITATION IN MM/DAY     ***
C
	PRECIP=PRECIP+WT(1)*SIGD*WATER(1)*3600.*24000./(ROWL*G)
C
  405   CONTINUE
C
C   ***  CALCULATE TENDENCIES OF LOWEST LEVEL POTENTIAL TEMPERATURE  ***
C   ***                      AND MIXING RATIO                        ***
C
	DPINV=0.01/(PH(1)-PH(2))
	AM=0.0
	IF(NK.EQ.1)THEN
	 DO 410 K=2,INB
  410    AM=AM+M(K)
	END IF
	IF((2.*G*DPINV*AM).GE.DELTI)IFLAG=4
	FT(1)=FT(1)+G*DPINV*AM*(T(2)-T(1)+(GZ(2)-GZ(1))/CPN(1))
	FT(1)=FT(1)+SIGD*WT(2)*(CL-CPD)*WATER(2)*(T(2)-
     1   T(1))*DPINV/CPN(1)
	FQ(1)=FQ(1)+G*AM*(Q(2)-Q(1))*DPINV
	AMDE=0.0
	DO 415 J=2,INB
	 FQ(1)=FQ(1)+G*DPINV*MENT(J,1)*(QENT(J,1)-Q(1))
  415   CONTINUE
C
C   ***  CALCULATE TENDENCIES OF POTENTIAL TEMPERATURE AND MIXING RATIO  ***
C   ***               AT LEVELS ABOVE THE LOWEST LEVEL                   ***
C
C   ***  FIRST FIND THE NET SATURATED UPDRAFT AND DOWNDRAFT MASS FLUXES  ***
C   ***                      THROUGH EACH LEVEL                          ***
C
	DO 500 I=2,INB
	DPINV=0.01/(PH(I)-PH(I+1))
	CPINV=1.0/CPN(I)
	AMP1=0.0
	AD=0.0
	IF(I.GE.NK)THEN
	 DO 440 K=I+1,INB+1
  440    AMP1=AMP1+M(K)
	END IF
	DO 450 K=1,I
	DO 450 J=I+1,INB+1
	 AMP1=AMP1+MENT(K,J)
  450   CONTINUE
	IF((2.*G*DPINV*AMP1).GE.DELTI)IFLAG=4
	DO 470 K=1,I-1
	DO 470 J=I,INB
	AD=AD+MENT(J,K)
  470   CONTINUE
	FT(I)=FT(I)+G*DPINV*(AMP1*(T(I+1)-T(I)+(GZ(I+1)-GZ(I))*
     1   CPINV)-AD*(T(I)-T(I-1)+(GZ(I)-GZ(I-1))*CPINV))
	FT(I)=FT(I)+G*DPINV*MENT(I,I)*(HP(I)-H(I)+
     1    T(I)*(CPV-CPD)*(Q(I)-QENT(I,I)))*CPINV
	FT(I)=FT(I)+SIGD*WT(I+1)*(CL-CPD)*WATER(I+1)*
     1    (T(I+1)-T(I))*DPINV*CPINV
	FQ(I)=FQ(I)+G*DPINV*(AMP1*(Q(I+1)-Q(I))-
     1    AD*(Q(I)-Q(I-1)))
	DO 480 K=1,I-1
	 AWAT=ELIJ(K,I)-(1.-EP(I))*CLW(I)
	 AWAT=MAX(AWAT,0.0)
	 FQ(I)=FQ(I)+G*DPINV*MENT(K,I)*(QENT(K,I)-AWAT-Q(I))
  480   CONTINUE
	DO 490 K=I,INB
	 FQ(I)=FQ(I)+G*DPINV*MENT(K,I)*(QENT(K,I)-Q(I))
  490   CONTINUE
  500   CONTINUE
C
C   ***   Very slightly adjust temperature tendencies to force exact   ***
C   ***                     enthalpy conservation                      ***
C
	ENTS=0.0
	DO 680 I=1,INB
	 ENTS=ENTS+(CPN(I)*FT(I)+LV(I)*FQ(I))*(PH(I)-PH(I+1))	
  680	CONTINUE
	ENTS=ENTS/(PH(1)-PH(INB+1))
	DO 640 I=1,INB
	 FT(I)=FT(I)-ENTS/CPN(I)
  640	CONTINUE
C
C   ***           RESET COUNTER AND RETURN           ***
C
	RETURN
  800   CONTINUE
	RETURN
	END
C
C ---------------------------------------------------------------------------
C
	SUBROUTINE TLIFT(P,T,Q,QS,GZ,ICB,NK,TVP,TPK,CLW,ND,NL,KK)
	REAL GZ(ND),TPK(ND),CLW(ND),P(ND)
	REAL T(ND),Q(ND),QS(ND),TVP(ND),LV0
C
C   ***   ASSIGN VALUES OF THERMODYNAMIC CONSTANTS     ***
C
	CPD=1005.7
	CPV=1870.0
	CL=4190.0
        RV=461.5
	RD=287.04
	LV0=2.501E6
C
	CPVMCL=CL-CPV
	EPS=RD/RV
	EPSI=1./EPS
C
C   ***  CALCULATE CERTAIN PARCEL QUANTITIES, INCLUDING STATIC ENERGY   ***
C
	AH0=(CPD*(1.-Q(NK))+CL*Q(NK))*T(NK)+Q(NK)*(LV0-CPVMCL*(
     1   T(NK)-273.15))+GZ(NK)
	CPP=CPD*(1.-Q(NK))+Q(NK)*CPV
	CPINV=1./CPP
C
	IF(KK.EQ.1)THEN
C
C   ***   CALCULATE LIFTED PARCEL QUANTITIES BELOW CLOUD BASE   ***
C
	DO 50 I=1,ICB-1
	 CLW(I)=0.0
   50   CONTINUE
	DO 100 I=NK,ICB-1
	 TPK(I)=T(NK)-(GZ(I)-GZ(NK))*CPINV
	 TVP(I)=TPK(I)*(1.+Q(NK)*EPSI)
  100   CONTINUE
	END IF
C
C    ***  FIND LIFTED PARCEL QUANTITIES ABOVE CLOUD BASE    ***
C
	NST=ICB
	NSB=ICB
	IF(KK.EQ.2)THEN  
	 NST=NL
	 NSB=ICB+1
	END IF
	DO 300 I=NSB,NST
	 TG=T(I)
	 QG=QS(I)
	 ALV=LV0-CPVMCL*(T(I)-273.15)
	 DO 200 J=1,2
	  S=CPD+ALV*ALV*QG/(RV*T(I)*T(I))
	  S=1./S
	  AHG=CPD*TG+(CL-CPD)*Q(NK)*T(I)+ALV*QG+GZ(I)
	  TG=TG+S*(AH0-AHG)
	  TC=TG-273.15
	  DENOM=243.5+TC
	  DENOM=MAX(DENOM,1.0)
          IF(TC.GE.0.0)THEN  
	   ES=6.112*EXP(17.67*TC/DENOM)
          ELSE  
           ES=EXP(23.33086-6111.72784/TG+0.15215*LOG(TG))
          END IF  
	  QG=EPS*ES/(P(I)-ES*(1.-EPS))
  200    CONTINUE
	 ALV=LV0-CPVMCL*(T(I)-273.15)
         TPK(I)=(AH0-(CL-CPD)*Q(NK)*T(I)-GZ(I)-ALV*QG)/CPD
	 CLW(I)=Q(NK)-QG
	 CLW(I)=MAX(0.0,CLW(I))
	 RG=QG/(1.-Q(NK))
	 TVP(I)=TPK(I)*(1.+RG*EPSI)
  300   CONTINUE
	RETURN
	END
